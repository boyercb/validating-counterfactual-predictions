
predict_plogit <- function(fit, newdata, id, time) {
  t <- time
  obs <- nrow(newdata)
  
  newdata <- slice(newdata, rep(1:n(), each = t))
  newdata[['time']] <- rep(1:t, obs)
  
  newdata[['haz']] <- stats::predict(fit, newdata = newdata, type = "response")
  
  newdata <- dplyr::group_by(newdata, .data[[id]]) 
  
  newdata <- dplyr::summarise(
    newdata, 
    cuminc = sum(haz * cumprod(1 - dplyr::lag(haz, 1, default = 0)))
  )
  
  pull(newdata, cuminc)
}


# get test predictions
p_cox <- compute_risk_score(
  fit = fit_cox,
  newdata = cox_df[!cox_df$mesaid %in% train_ids, ],
  time = 120
)

p_ipcw <- predict_plogit(
  fit = fit_ipcw,
  newdata = trial_pt_uncens[!trial_pt_uncens$mesaid %in% train_ids &
                              trial_pt_uncens$time == 1, ],
  id = "mesaid",
  time = 120
) 

p_plogit <- predict_plogit(
  fit = fit_plogit,
  newdata = trial_pt_uncens[!trial_pt_uncens$mesaid %in% train_ids &
                              trial_pt_uncens$time == 1, ],
  id = "mesaid",
  time = 120
) 

# fit loss estimator (h) in test

# fit propensity score estimator (e) in test
test_weights_fit <- ipcw(
  formula = reformulate(
    termlabels = c("bl_age",
                   "gender1",
                   "bl_cursmk",
                   "bl_dm03",
                   "bl_sbp",
                   "bl_hdl",
                   "bl_chol",
                   "bl_htnmed",
                   "ns(time, 5)"),
    response = "cvda"
  ),
  treatment = "tx",
  # compevent = "dth",
  numerator = reformulate(
    termlabels = c(
      "ns(time, 5)",
      baseline_vars,
      baseline_vars_long
    ),
    response = "censor"
  ),
  denominator = reformulate(
    termlabels = c(
      "ns(time, 5)",
      tv_adj_vars,
      baseline_vars,
      baseline_vars_long
    ),
    response = "censor"
  ),
  id = "id",
  time = "time",
  data = trial_pt_cens[!trial_pt_cens$mesaid %in% train_ids, ]
)

# calculate MSE

test <- trial_pt_cens %>% 
  filter(!trial_pt_cens$mesaid %in% train_ids) %>%
  mutate(ipcw = test_weights_fit$ipcw) %>%
  group_by(mesaid) %>%
  filter(row_number() == n()) %>%
  ungroup() 
  
w <- pull(test, ipcw)

sum(I(test$tx == 0) * w * (p_cox - test$cvda)^2) / sum(I(test$tx == 0))
sum(I(test$tx == 0) * w * (p_ipcw - test$cvda)^2)  / sum(I(test$tx == 0))
sum(I(test$tx == 0) * w * (p_plogit - test$cvda)^2)  / sum(I(test$tx == 0))

mean((p_cox - test$cvda)^2) 
mean((p_ipcw - test$cvda)^2) 

plot(p_cox[test$tx == 0], p_ipcw[test$tx == 0])
plot(p_plogit[test$tx == 0], p_ipcw[test$tx == 0])

