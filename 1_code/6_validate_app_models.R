
# make predictions in test set --------------------------------------------

p_naive <- predict(naive_model, newdata = test, type = "response")
p_ipw <- predict(ipw_model, newdata = test, type = "response")


# fit models for counterfactual mse ---------------------------------------

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

# calculate mse in test set -----------------------------------------------

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

mse <- 
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


# bootstrap uncertainty ---------------------------------------------------

replicates <- 1000

boots <- rerun(replicates, bootfun(data = mesa))

se <- 
  boots |>
  bind_rows(.id = "sim") |>
  group_by(estimator) |>
  summarise(across(starts_with("mse"), sd)) 

bind_rows(mse, se)

