library(data.table)
library(kableExtra)
library(knitr)
library(tidyverse)
library(MASS)
library(mgcv)

# function to generate data from simulation process
gendata <- function(
    N_train = 1000,  # number of training observations
    N_test = 1000,   # number of observations in test set
    N_truth = 1000,  # number of observations for calculating true/oracle MSE
    sims = 1         # number of simulations
) {
  
  N <- N_train + N_test + N_truth
  X <- mvrnorm(
    N * sims, 
    rep(0, 3),  
    sapply(1:3, function(x) 0.5^abs(1:3 - x))
  )
  fX <- -0.3 + 0.2 * rowSums(X[, 1:3]) + 0.3 * rowSums(X[, 1:3]^2)
  A <- rbinom(
    N * sims, 
    1, 
    plogis(fX)
  )
  
  split <- factor(rep(c(
    rep("train", N_train),
    rep("test", N_test),
    rep("truth", N_truth)
  ),
  sims))
  
  Y <- rbinom(
    N * sims, 
    1, 
    plogis(fX - 0.5 * A * I(split != "truth"))
  )
  
  dt <- data.table(
    sim = rep(1:sims, each = N), 
    split = split,
    X = as.data.table(X),
    A = A,
    Y = Y
  )
  
  setkey(dt, sim)
  
  return(dt)
}

# fit models to each sim
runsim <- function(df) {

  # train prediction model
  model <- glm(
    Y ~ X.V1 + X.V2 + X.V3,
    family = binomial(link = "logit"),
    data = subset(df, split == "train")
  )
  
  # fitted predictions in the test set
  gX <- predict(model, 
                newdata = subset(df, split == "test"), 
                type = "response")
  
  # weight models for MSE
  Afits <- list(
    correct = glm(
      A ~ X.V1 + I(X.V1^2) + X.V2 + I(X.V2^2) + X.V3 + I(X.V3^2),
      family = binomial(link = "logit"),
      data = subset(df, split == "test")
    ),
    main_effects = glm(
      A ~ X.V1 + X.V2 + X.V3,
      family = binomial(link = "logit"),
      data = subset(df, split == "test")
    ),
    gam = gam(
      A ~ s(X.V1) + s(X.V2) + s(X.V3),
      family = binomial(link = "logit"),
      data = subset(df, split == "test")
    )
  )
  
  # conditional loss models for MSE
  hfits <- list(
    correct = glm(
      Y ~ X.V1 + I(X.V1^2) + X.V2 + I(X.V2^2) + X.V3 + I(X.V3^2),
      family = binomial(link = "logit"),
      data = subset(df, split == "test" & A == 0)
    ),
    main_effects = glm(
      Y ~ X.V1 + X.V2 + X.V3,
      family = binomial(link = "logit"),
      data = subset(df, split == "test" & A == 0)
    ),
    gam = gam(
      Y ~ s(X.V1) + s(X.V2) + s(X.V3),
      family = binomial(link = "logit"),
      data = subset(df, split == "test" & A == 0)
    )
  )
  
  # propensity scores
  pA <- lapply(Afits, function(fit) {
      predict(fit, 
              newdata = subset(df, split == "test" & A == 0), 
              type = "response")
    })
  
  names(pA) <- names(Afits)
  
  
  # conditional loss estimates  
  hL <- lapply(hfits, function(fit, gX) {
    pY <- predict(fit, 
                  newdata = subset(df, split == "test"), 
                  type = "response")
    
    pY - 2 * gX * pY + gX^2
  }, gX = gX)
  
  names(hL) <- names(hfits)
  
  # MSE 
  naive <- list(
    mean((df$Y[df$split == "test"] - gX)^2)
  )
  
  condlost <- lapply(hL, function(x) mean(x))
  
  weighted <- lapply(pA, function(pA, gX) {
    obs <- which(df$split == 'test' & df$A == 0)
    test_obs <- with(subset(df, split == 'test'), which(A == 0))
    
    err <- df$Y[obs] - gX[test_obs]
    
    weighted.mean(err^2, 1 / (1 - pA))
  }, gX = gX)
  
  specs <- list(
    list("correct", "correct"),
    list("correct", "main_effects"),
    list("main_effects", "correct"),
    list("main_effects", "main_effects"),
    list("gam", "gam")
  )
  
  # fix this
  dr <- lapply(specs, function(spec, pA, hL, gX) {
    obs <- which(df$split == 'test' & df$A == 0)
    test_obs <- with(subset(df, split == 'test'), which(A == 0))
    
    eif <- rep(0, nrow(subset(df, split == 'test')))
    
    eif[test_obs] <- 1 / (1 - pA[[spec[[2]]]]) *
      ((df$Y[obs] - gX[test_obs])^2 - hL[[spec[[1]]]][test_obs])
    
    eif <- hL[[spec[[1]]]] + eif
    
    mean(eif)
  }, pA = pA, hL = hL, gX = gX)
  
  truth <- list(mean((
    df$Y[df$split == "truth"] -
      predict(
        model,
        newdata = subset(df, split == "truth"),
        type = "response"
      )
  ) ^ 2))
  
  return(c(naive, condlost, weighted, dr, truth))
}

set.seed(8761276)

dt <- gendata(sims = 1000) 

dt <- dt[, .(
  mse = unlist(runsim(.SD)),
  type = c(
    'naive',
    'CL, correct',
    'CL, misspecified',
    'CL, gam',
    'IPW, correct',
    'IPW, misspecified',
    'IPW, gam',
    'DR, correct',
    'DR, e(X) misspecified',
    'DR, h(X) misspecified',
    'DR, both misspecified',
    'DR, gam',
    'truth'
  )
), by = sim]

final <- dt[, .(mse = mean(mse)), by = list(type)]

final_output <-
  as_tibble(final) %>%
  mutate(
    bias = (mse - mse[type == 'truth']) * 10^2,
    perc = bias / mse[type == 'truth']
  )

kable(
  x = final_output,
  format = "latex",
  col.names = c(
    "Estimator", 
    "MSE",
    "Bias ($\\times 10^2$)", 
    "Percent"
  ),
  digits = 3,
  booktabs = TRUE
) %>%
  footnote(
    threeparttable = TRUE,
    general_title = "",
    general = "Correctly specified and incorrectly specified refers to the specification of the posited prediction model. OLS = model estimation using ordinary least squares regression (unweighted); WLS = model estimation using weighted least squares regression with weights equal to the inverse probability of being untreated. Results were averaged over 10, 000 simulations. The true counterfactual MSE was obtained using numerical methods. "
  )


