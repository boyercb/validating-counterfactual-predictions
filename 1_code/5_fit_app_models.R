
# check ITT/adherence-adjusted estimates  ---------------------------------

trial_estimates <- 
  list(
    "ITT" = NULL,
    "adherence" = NULL
  )

# tb <- tbl_merge(
#   map(trial_estimates, function (x) {
#     tbl_regression(
#       x = x,
#       label = list(tx ~ "statins"),
#       include = "tx",
#       exponentiate = TRUE
#     ) %>%
#       modify_footnote(c(estimate, ci) ~ NA)
#   }),
#   tab_spanner = FALSE
# ) %>%
#   modify_table_styling(
#     column = c(p.value_1, p.value_2),
#     hide = TRUE
#   ) %>%
#   modify_table_styling(
#     column = c(ci_1),
#     row = !is.na(ci_1),
#     cols_merge_pattern = "({ci_1})"
#   ) %>%
#   modify_table_styling(
#     column = c(ci_2),
#     row = !is.na(ci_2),
#     cols_merge_pattern = "({ci_2})"
#   ) 
# 
# tb


# fit naive model ---------------------------------------------------------

naive_model <- glm(
  formula = cvda ~ bl_age + gender1 + bl_cursmk + bl_dm03 + bl_sbp +
    bl_hdl + bl_chol + bl_htnmed + bl_sbp:bl_htnmed,
  family = binomial(link = "logit"),
  data = train
)


# fit ipcw model ----------------------------------------------------------

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

modelsummary(list(naive_model, ipw_model))

