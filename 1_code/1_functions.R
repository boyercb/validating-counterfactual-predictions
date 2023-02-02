bootfun <- function(data) {
  
  # bootstrap replicates 
  data <- data[sample(1:nrow(mesa), nrow(mesa), replace = TRUE), ]
  
  # split into training and test
  split <- sample(1:nrow(data), floor(length(1:nrow(data)) / 2))
  
  train <- mesa[split, ]
  test <- mesa[-split, ]
  
  # fit naive model 
  naive_model <- glm(
    formula = cvda ~ bl_age + gender1 + bl_cursmk + bl_dm03 + bl_sbp +
      bl_hdl + bl_chol + bl_htnmed + bl_sbp:bl_htnmed,
    family = binomial(link = "logit"),
    data = train
  )
  
  # fit ipcw model 
  # models for numerator and denominator of stabilized weights
  treat_model <- list(
    "num" = glm(
      formula = reformulate(
        termlabels = model_vars,
        response = "A"
      ),
      family = binomial(link = "logit"),
      data = train
    ),
    "den" = glm(
      formula = reformulate(
        termlabels = c(baseline_vars, baseline_vars_long),
        response = "A"
      ),
      family = binomial(link = "logit"),
      data = train
    )
  )
  
  # get probs for numerator and denominator of weights
  p <- lapply(treat_model, function(x) predict(x, type = "response"))
  
  # add weights to training data
  train <- 
    train |>
    mutate(
      w = (A * p[['num']] + (1 - A) * (1 - p[['num']])) / 
        (A * p[['den']] + (1 - A) * (1 - p[['den']]))
    )
  
  # fit model
  ipw_model <- glm(
    formula = cvda ~ bl_age + gender1 + bl_cursmk + bl_dm03 + bl_sbp +
      bl_hdl + bl_chol + bl_htnmed + bl_sbp:bl_htnmed,
    family = binomial(link = "logit"),
    data = train[train$A == 0, ],
    weights = train$w[train$A == 0]
  )
  
  # make predictions in test set 
  p_naive <- predict(naive_model, newdata = test, type = "response")
  p_ipw <- predict(ipw_model, newdata = test, type = "response")
  
  
  # fit models for counterfactual mse 
  e_fit <- list(
    "num" = glm(
      formula = A ~ 1,
      family = binomial(link = "logit"),
      data = test
    ), 
    "den" = glm(
      formula = reformulate(
        termlabels = c(baseline_vars, baseline_vars_long), 
        response = "A"
      ),
      family = binomial(link = "logit"),
      data = test
    )
  )
  
  h_fit <- glm(
    formula = reformulate(
      termlabels = c(baseline_vars, baseline_vars_long), 
      response = "cvda"
    ),
    family = binomial(link = "logit"),
    data = subset(test, A == 0)
  )
  
  gam_fits <- list(
    "num" = glm(
      formula = A ~ 1,
      family = binomial(link = "logit"),
      data = test
    ), 
    "den" = gam(
      formula = reformulate(
        termlabels = c(baseline_vars, baseline_vars_long), 
        response = "A"
      ),
      family = binomial(link = "logit"),
      data = test
    ),
    "h" = gam(
      formula = reformulate(
        termlabels = c(baseline_vars, baseline_vars_long), 
        response = "cvda"
      ),
      family = binomial(link = "logit"),
      data = subset(test, A == 0)
    )
  )
  
  # calculate mse in test set 
  e <- lapply(e_fit, function(x) predict(x, type = "response"))
  pY <- predict(h_fit, newdata = test, type = "response")
  h_naive <- pY - 2 * p_naive * pY + p_naive^2
  h_ipw <- pY - 2 * p_ipw * pY + p_ipw^2
  
  test <- 
    test |>
    mutate(
      w = (A * e[['num']] + (1 - A) * (1 - e[['num']])) / 
        (A * e[['den']] + (1 - A) * (1 - e[['den']]))
    )
  
  test |>
    summarise(
      mse_naive_naive = mean((p_naive - cvda)^2),
      mse_naive_ipw = mean((p_ipw - cvda)^2),
      mse_ipw_naive = sum(I(A == 0) * w * (p_naive - cvda)^2) / sum(I(A == 0)),
      mse_ipw_ipw = sum(I(A == 0) * w * (p_ipw - cvda)^2) / sum(I(A == 0)),
      mse_cl_naive = mean(h_naive),
      mse_cl_ipw = mean(h_ipw),
      mse_dr_naive = mean(I(A == 0) * w * ((p_naive - cvda)^2 - h_naive) + h_naive),
      mse_dr_ipw = mean(I(A == 0) * w * ((p_ipw - cvda)^2 - h_ipw) + h_ipw)
    ) |>
    pivot_longer(
      everything(),
      names_pattern = "(.*)_(.*)",
      names_to = c(".value", "estimator")
    ) 
}

specd <- function(x, k) trimws(format(round(x, k), nsmall=k))

