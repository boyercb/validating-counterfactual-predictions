
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
    Y = cvda,
    w = (A * e[['num']] + (1 - A) * (1 - e[['num']])) / 
      (A * e[['den']] + (1 - A) * (1 - e[['den']]))
  )

auc_naive_naive <- cal_auc(test$cvda, p_naive)$auc
auc_naive_ipw <- cal_auc(test$cvda, p_ipw)$auc
aucs_naive <- cal_counter_auc(test, p_naive, e_fit$den, h_fit, se.fit = FALSE)
aucs_ipw <- cal_counter_auc(test, p_ipw, e_fit$den, h_fit, se.fit = FALSE)

results <- 
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
  mutate(
    auc_naive_naive = auc_naive_naive,
    auc_naive_ipw = auc_naive_ipw,
    auc_ipw_naive = aucs_naive$ipw,
    auc_ipw_ipw = aucs_ipw$ipw,
    auc_cl_naive = aucs_naive$om,
    auc_cl_ipw = aucs_ipw$om,
    auc_dr_naive = aucs_naive$dr,
    auc_dr_ipw = aucs_ipw$dr
  ) |>
  pivot_longer(
    everything(),
    names_pattern = "(.*)_(.*)_(.*)",
    names_to = c(".value", "estimator", "model")
  ) |>
  pivot_wider(
    names_from = model,
    values_from = c(mse, auc)
  )


# bootstrap uncertainty ---------------------------------------------------

replicates <- 1000

with_progress({
  p <- progressor(steps = replicates)
  
  boots <- rerun(replicates, bootfun(data = mesa, p = p))
})

se <- 
  boots |>
  bind_rows(.id = "sim") |>
  group_by(estimator) |>
  summarise(across(-sim, sd)) 

application_results <- 
  bind_rows(results, se) |>
  arrange(match(estimator, c("naive", "cl", "ipw", "dr")), desc(mse_naive)) |>
  group_by(estimator) |>
  mutate(
    estimator = case_when(
      estimator == "naive" & row_number() == 1 ~ "Naive",
      estimator == "cl" & row_number() == 1 ~ "CL",
      estimator == "ipw" & row_number() == 1 ~ "IPW",
      estimator == "dr" & row_number() == 1 ~ "DR",
      TRUE ~ ""
      ),
    across(c(starts_with("mse"), starts_with("auc")), specd, k = 3),
    across(c(starts_with("mse"), starts_with("auc")), as.character),
    across(c(starts_with("mse"), starts_with("auc")), function(x)
      if_else(row_number() == 2, paste0("(", x, ")"), x))
  )

kable(
  x = application_results,
  format = "latex",
  col.names = c(
    "Estimator",
    "Logit", 
    "Weighted Logit", 
    "Logit", 
    "Weighted Logit"
  ),
  digits = 3,
  align = "lcccc",
  escape = FALSE,
  booktabs = TRUE
) %>%
  add_header_above(c(" " = 1, "MSE" = 2, "AUC" = 2)) |>
  footnote(
    threeparttable = TRUE,
    general_title = "",
    general = "The first column refers to the posited prediction model: the first model is an (unweighted) logistic regression model and the second is a logistic regression model with inverse probability weights for remaining statin-free. $\\\\widehat{\\\\psi}_{Naive}$ is the empirical estimator of the MSE using factual outcomes, $\\\\widehat{\\\\psi}_{CL}$ is the conditional loss estimator, $\\\\widehat{\\\\psi}_{IPW}$ is the inverse probability weighting estimator, $\\\\widehat{\\\\psi}_{DR}$ is the doubly-robust estimator. Standard error estimates are shown in parentheses obtained via 1000 bootstrap replicates.",
    escape = FALSE
  )



# calibration
test$p_naive <- p_naive
test$p_ipw <- p_ipw

test <- 
  test |>
  mutate(
    Y = cvda,
    w2 = (1 - A) * (1 - e[['num']])/(1 - e[['den']]) + 0
  )

loess.gcv <- function(x, y, w) {
  nobs <- length(y)
  tune.loess <- function(s) {
    lo <- loess(y ~ x, weights = w, span = s)
    mean((lo$fitted - y) ^ 2) / (1 - lo$trace.hat / nobs) ^ 2
  }
  os <- optimize(tune.loess, interval = c(.01, 99))$minimum
  
  loess(y ~ x, weights = w, span = os)
}

loess_naive <-
  loess(cvda ~ p_naive, data = subset(test, A == 0), weights = w2)
  # locfit(cvda ~ lp(p_naive, deg = 2), data = subset(test, A == 0), weights = w2, family = "huber")
#   loess.gcv(
#     x = test$p_naive[test$A == 0],
#     y = test$cvda[test$A == 0],
#     w = test$w2[test$A == 0]
# )
loess_ipw <-
  loess(cvda ~ p_ipw, data = subset(test, A == 0), weights = w2)
# locfit(cvda ~ lp(p_ipw, deg = 2), data = subset(test, A == 0), weights = w2, family = "huber")
  # loess.gcv(
  #   x = test$p_ipw[test$A == 0],
  #   y = test$cvda[test$A == 0],
  #   w = test$w2[test$A == 0]
  # )

calib <- subset(test, A == 0)
# calib$curve_naive <- predict(loess_naive, newdata = calib, type = "response")
# calib$curve_ipw <- predict(loess_ipw, newdata = calib, type = "response")

calib$curve_naive <- predict(loess_naive, newdata = calib$p_naive, type = "response")
calib$curve_ipw <- predict(loess_ipw, newdata = calib$p_ipw, type = "response")

plt1 <- calib |>
  select(p_naive, p_ipw, curve_naive, curve_ipw, cvda) |>
  pivot_longer(
    cols = c(p_naive, p_ipw, curve_naive, curve_ipw),
    names_pattern = "(.*)_(.*)",
    names_to = c('.value', 'model')
  ) |>
  filter(model == "naive")

p1 <- 
  ggplot(plt1, aes(x = p, y = curve),) + 
  geom_line(linewidth = 1.25) +
  geom_rug(sides = "b", alpha = 0.3, data = filter(plt1, cvda == 0), position = "jitter") +
  geom_rug(sides = "t", alpha = 0.3, data = filter(plt1, cvda == 1), position = "jitter") +
  geom_abline(slope = 1, linetype = "dashed") +
  theme_bw(base_size = 10) +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 0.75, 0.25), limits = c(-0.05, 0.85)) +
  scale_x_continuous(breaks = seq(0, 0.75, 0.25), limits = c(-0.05, 0.85)) +
  labs(
    title = "Logit",
    x = "Predicted risk",
    y = "Observed risk (weighted)"
  )
    

plt2 <- calib |>
  select(p_naive, p_ipw, curve_naive, curve_ipw, cvda) |>
  pivot_longer(
    cols = c(p_naive, p_ipw, curve_naive, curve_ipw),
    names_pattern = "(.*)_(.*)",
    names_to = c('.value', 'model')
  ) |>
  filter(model == "ipw")

p2 <- 
  ggplot(plt2, aes(x = p, y = curve)) + 
  geom_line(linewidth = 1.25) +
  geom_rug(sides = "b", alpha = 0.3, data = filter(plt2, cvda == 0)) +
  geom_rug(sides = "t", alpha = 0.3, data = filter(plt2, cvda == 1)) +
  geom_abline(slope = 1, linetype = "dashed") +
  theme_bw(base_size = 10) +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 0.75, 0.25), limits = c(-0.05, 0.85)) +
  scale_x_continuous(breaks = seq(0, 0.75, 0.25), limits = c(-0.05, 0.85)) +
  labs(
    title = "Weighted Logit",
    x = "Predicted risk",
    y = "Observed risk (weighted)"
  )

plt <- calib |>
  select(p_naive, p_ipw, cvda) |>
  pivot_longer(
    cols = c(p_naive, p_ipw),
    names_pattern = "(.*)_(.*)",
    names_to = c('.value', 'model')
  )

plt3 <- filter(plt, model == "naive")

p3 <- 
  ggplot(plt3, aes(x = p)) + 
  geom_histogram(aes(y = -1 * after_stat(count)), bins = 40, data = subset(plt3)) +
  geom_histogram(bins = 40, data = subset(plt3, cvda == 1)) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(0, 0.75, 0.25), limits = c(-0.05, 0.85)) +
  theme_bw()  +
  labs(
    x = NULL,
    y = ""
  )
  
plt4 <- filter(plt, model == "ipw")

p4 <- 
  ggplot(plt3, aes(x = p)) + 
  geom_histogram(aes(y = -1 * after_stat(count)), bins = 40, data = subset(plt4)) +
  geom_histogram(bins = 40, data = subset(plt4, cvda == 1)) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(0, 0.75, 0.25), limits = c(-0.05, 0.85)) +
  theme_bw() +
  labs(
    x = NULL,
    y = ""
  ) 
  
ggsave(filename = "3_figures/calib.pdf",
       plot = (p1 + p2),
       device = "pdf",
       width = 6.5,
       height = 3.5
)
       