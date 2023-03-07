
set.seed(8761276)

dt1 <- gendata1(sims = 10000) 

dt1 <- dt1[, .(
  mse = unlist(runsim1(.SD)),
  type = rep(c("unweighted", "weighted", "truth"), each = 4),
  fit = rep(c("OLS, miss", "OLS, cor", "WLS, miss", "WLS, cor"), 3)
), by = sim]

final1 <- dt1[, .(mse = mean(mse)), by = list(type, fit)]

final_output1 <-
  as_tibble(final1) %>%
  pivot_wider(names_from = type, values_from = mse) %>%
  mutate(
    fit = str_replace(fit, "miss", "misspecified"),
    fit = str_replace(fit, "cor", "correct")
  )

kable(
  x = final_output1,
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


