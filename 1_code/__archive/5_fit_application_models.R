# Cox model

# fit model in training dataset
fit_cox <- coxph(
  Surv(cvdatt, cvda) ~ bl_age + gender1 + bl_cursmk + bl_dm03 + 
    bl_sbp + bl_hdl + bl_chol + bl_htnmed + bl_sbp:bl_htnmed,
  data = cox_df[cox_df$mesaid %in% train_ids, ],
  model = TRUE,
  x = TRUE,
  y = TRUE
)


# check the ITT/adherence-adjusted effect estimates
trial_estimates <- 
  list(
    "ITT" = glm(
      formula = reformulate(
        termlabels = c("ns(time, 5)",
                       tv_vars,
                       baseline_vars,
                       "tx"),
        response = "cvda"
      ),
      family = binomial(link = "logit"),
      data = trial_itt,
    ), 
    "adherence" = ipcw(
      formula = reformulate(
        termlabels = c("ns(time, 5)",
                       baseline_vars,
                       baseline_vars_long,
                       "tx"),
        response = "cvda"
      ),
      treatment = "tx",
      compevent = "dth",
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
          "trial2",
          "trial3",
          "trial4",
          "ns(time, 5)",
          tv_adj_vars,
          baseline_vars,
          baseline_vars_long
        ),
        response = "censor"
      ),
      id = "id",
      time = "time",
      data = trial_pt_cens
    )$fit
  )
  
tb <- tbl_merge(
  map(trial_estimates, function (x) {
    tbl_regression(
      x = x,
      label = list(tx ~ "statins"),
      include = "tx",
      exponentiate = TRUE,
      tidy_fun = partial(
          tidy_robust,
          vcov = "HC",
          ci_method = "wald"
        )
    ) %>%
      modify_header(label = "", estimate = "**HR**") %>%
      modify_footnote(c(estimate, ci) ~ NA)
  }),
  tab_spanner = FALSE
) %>%
  modify_table_styling(
    column = c(p.value_1, p.value_2),
    hide = TRUE
  ) %>%
  modify_table_styling(
    column = c(ci_1),
    row = !is.na(ci_1),
    cols_merge_pattern = "({ci_1})"
  ) %>%
  modify_table_styling(
    column = c(ci_2),
    row = !is.na(ci_2),
    cols_merge_pattern = "({ci_2})"
  ) 
  
tb

# IPCW model

# fit model for inverse-probability of censoring weights in training data
train_weights_fit <- ipcw(
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
      "bl_age",
      "gender1",
      "bl_cursmk",
      "bl_dm03",
      "bl_sbp",
      "bl_hdl",
      "bl_chol",
      "bl_htnmed",
      "ns(time, 5)"
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
  data = trial_pt_cens[trial_pt_cens$mesaid %in% train_ids, ] #trial_pt_cens
)

# fit model in training dataset
fit_ipcw <- glm(
  formula = cvda ~ bl_age + gender1 + bl_cursmk + bl_dm03 + bl_sbp +
    bl_hdl + bl_chol + bl_htnmed + bl_sbp:bl_htnmed + ns(time, 5),
  family = binomial(link = "logit"),
  data = ipcw_df[ipcw_df$mesaid %in% train_ids, ], # ipcw_df
  weights = train_weights_fit$ipcw[trial_pt_cens$mesaid %in% train_ids &
                                     trial_pt_cens$tx == 0] # train_weights_fit$ipcw[trial_pt_cens$tx == 0]
)

fit_plogit <- glm(
  formula = cvda ~ bl_age + gender1 + bl_cursmk + bl_dm03 + bl_sbp +
    bl_hdl + bl_chol + bl_htnmed + bl_sbp:bl_htnmed + ns(time, 8),
  family = binomial(link = "logit"),
  data = trial_pt_uncens[trial_pt_uncens$mesaid %in% train_ids, ],
)

t1 <- tbl_regression(
  fit_cox,
  exponentiate = TRUE
) %>% 
  modify_header(label = "", estimate = "**HR**") %>%
  tbl_butcher() 

t2 <- tbl_regression(
  fit_ipcw,
  include = c("bl_age",
              "gender1",
              "bl_cursmk",
              "bl_dm03",
              "bl_sbp",
              "bl_hdl",
              "bl_chol",
              "bl_htnmed",
              "bl_sbp:bl_htnmed"),
  exponentiate = TRUE
) %>% 
  modify_header(label = "", estimate = "**HR**") %>%
  tbl_butcher()

t3 <- tbl_regression(
  fit_plogit,
  include = c("bl_age",
              "gender1",
              "bl_cursmk",
              "bl_dm03",
              "bl_sbp",
              "bl_hdl",
              "bl_chol",
              "bl_htnmed",
              "bl_sbp:bl_htnmed"),
  exponentiate = TRUE
) %>% 
  modify_header(label = "", estimate = "**HR**") %>%
  tbl_butcher()

for (stat in c("modify_stat_N", "modify_stat_n", "modify_stat_N_event")) {
  t1$table_styling$header[[stat]] <- as.numeric(t1$table_styling$header[[stat]])
}

tbl_merge(
  list(t1, t2, t3)
) %>%
  modify_table_styling(
    column = c(ci_1),
    row = !is.na(ci_1),
    cols_merge_pattern = "({ci_1})"
  ) %>%
  modify_table_styling(
    column = c(ci_2),
    row = !is.na(ci_2),
    cols_merge_pattern = "({ci_2})"
  ) %>% 
  modify_table_styling(
    column = c(ci_3),
    row = !is.na(ci_3),
    cols_merge_pattern = "({ci_3})"
  ) #%>%
    # as_gt() %>%
    # as_latex() %>%
    # as.character() %>%
    # write_file("2_tables/models2.tex")
