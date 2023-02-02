library(data.table)
library(kableExtra)
library(knitr)
library(tidyverse)

# function to generate data from simulation process
gendata <- function(
    N_train = 1000,  # number of training observations
    N_test = 1000,   # number of observations in test set
    N_truth = 1000,  # number of observations for calculating true/oracle MSE
    sims = 1         # number of simulations
  ) {
  
  N <- N_train + N_test + N_truth
  X <- runif(N * sims, 0, 10)
  A <- rbinom(N * sims, 1, plogis(-1.5 + 0.3 * X))
  
  split <- factor(x = rep(c(
    rep("train", N_train),
    rep("test", N_test),
    rep("truth", N_truth)
  ),
  sims))
    
  Y <- 1 + X + 0.5 * X^2 - 3 * A * I(split != "truth") + rnorm(N * sims)
  
  dt <- data.table(
    sim = rep(1:sims, each = N), 
    split = split,
    X = X,
    A = A,
    Y = Y
  )
  
  setkey(dt, sim)
  
  return(dt)
}

# fit models to each sim
runsim <- function(df) {
  
  # weight models
  pA <-
    lapply(c("train", "test"), function(x) {
      fit <- glm(
        A ~ X,
        family = binomial(link = "logit"),
        data = subset(df, split == x)
      )
      
      predict(fit, 
              newdata = subset(df, split == x & A == 0), 
              type = "response")
    })
  
  names(pA) <- c("train", "test")

  # prediction models
  models <-
    list(
      naive_miss = 
        glm(
          Y ~ X,
          family = gaussian(link = "identity"),
          data = subset(df, split == "train")
        ),
      naive_correct = 
        glm(
          Y ~ X + I(X^2),
          family = gaussian(link = "identity"),
          data = subset(df, split == "train")
        ),
      ipw_miss =
        glm(
          Y ~ X,
          family = gaussian(link = "identity"),
          weights = 1 / (1 - pA[['train']]),
          data = subset(df, split == "train" & A == 0)
        ),
      ipw_correct =
        glm(
          Y ~ X + I(X^2),
          family = gaussian(link = "identity"),
          weights = 1 / (1 - pA[['train']]),
          data = subset(df, split == "train" & A == 0)
        )
    )
  
  
  # MSE 
  unweighted <- lapply(models, function(fit) {
    err <- df$Y[df$split == "test"] - 
      predict(fit, newdata = subset(df, split == "test"))
    
    mean(err^2)
  })
  
  weighted <- lapply(models, function(fit, pA) {
    err <- df$Y[df$split == "test" & df$A == 0] - 
      predict(fit, newdata = subset(df, split == "test" & A == 0))
    
    weighted.mean(err^2, 1 / (1 - pA[['test']]))
  }, pA = pA)
  
  truth <- lapply(models, function(fit) {
    err <- df$Y[df$split == "truth"] - 
      predict(fit, newdata = subset(df, split == "truth"))
    
    mean(err^2)
  })
  
  
  return(c(unweighted, weighted, truth))
}

set.seed(8761276)

dt <- gendata(sims = 10000) 

dt <- dt[, .(
  mse = unlist(runsim(.SD)),
  type = rep(c("unweighted", "weighted", "truth"), each = 4),
  fit = rep(c("OLS, miss", "OLS, cor", "WLS, miss", "WLS, cor"), 3)
), by = sim]

final <- dt[, .(mse = mean(mse)), by = list(type, fit)]

final_output <-
  as_tibble(final) %>%
  pivot_wider(names_from = type, values_from = mse) %>%
  mutate(
    fit = str_replace(fit, "miss", "misspecified"),
    fit = str_replace(fit, "cor", "correct")
  )

kable(
  x = final_output,
  format = "latex",
  col.names = c(
    "Estimator", 
    "Unweighted MSE", 
    "Weighted MSE", 
    "True MSE"
  ),
  digits = 1,
  booktabs = TRUE
) %>%
  footnote(
    threeparttable = TRUE,
    general_title = "",
    general = "Correctly specified and incorrectly specified refers to the specification of the posited prediction model. OLS = model estimation using ordinary least squares regression (unweighted); WLS = model estimation using weighted least squares regression with weights equal to the inverse probability of being untreated. Results were averaged over 10, 000 simulations. The true counterfactual MSE was obtained using numerical methods. "
  )


