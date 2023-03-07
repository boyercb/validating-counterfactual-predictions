
set.seed(8761276)

dt2 <- gendata2(sims = 10000) 

with_progress({
  p <- progressor(steps = 10000)
  
  dt2 <- dt2[, .(
    est = unlist(runsim2(.SD, p)),
    psi = c(
      rep("mse", 13),
      rep("auc", 13)
    ),
    type = c(
      'Naive',
      'CL, correct',
      'CL, $h_a(X)$ misspecified',
      'CL gam',
      'IPW, correct',
      'IPW, $e_a(X)$ misspecified',
      'IPW gam',
      'DR, correct',
      'DR, $e_a(X)$ misspecified',
      'DR, $h_a(X)$ misspecified',
      'DR, both misspecified',
      'DR gam',
      'truth',
      'naive',
      'CL, correct',
      'CL, $h_a(X)$ misspecified',
      'CL gam',
      'IPW, correct',
      'IPW, $e_a(X)$ misspecified',
      'IPW gam',
      'DR, correct',
      'DR, $e_a(X)$ misspecified',
      'DR, $h_a(X)$ misspecified',
      'DR, both misspecified',
      'DR gam',
      'Truth'
    )
  ), by = sim]
})

final2 <- dt2[, .(est = mean(est), sd = sd(est)), by = list(psi, type)]
  
final_output2 <-
  as_tibble(final2) |>
  group_by(psi) |>
  mutate(
    bias = (est - est[type == 'truth']) * 10^2,
    perc = bias / est[type == 'truth'],
    est = specd(est, 3),
    sd = specd(sd, 3),
    bias = specd(bias, 3),
    perc = specd(perc, 1)
  ) |>
  pivot_wider(
    names_from = psi, 
    values_from = c(est, sd, bias, perc)
  ) |>
  slice(1:2, 5, 8, 2, 6, 9, 3, 5, 10, 4, 7, 12, 13) |>
  separate(type, c("estimator", "delete"), sep = ",") |>
  dplyr::select(estimator, ends_with("mse"), ends_with("auc")) 

kable(
  x = final_output2,
  format = "latex",
  col.names = c(
    "Estimator", 
    "Mean",
    "SD",
    "Bias ($\\times 10^2$)", 
    "Percent",
    "Mean",
    "SD",
    "Bias ($\\times 10^2$)", 
    "Percent"
  ),
  digits = 2,
  align = 'lcccccccc',
  linesep = "",
  booktabs = TRUE,
  escape = FALSE
) |>
  add_header_above(c(" " = 1, "MSE" = 4, "AUC" = 4)) |>
  kableExtra::group_rows("Correct", 2, 4, bold = FALSE) |>
  kableExtra::group_rows("$e_a(X)$ misspecified", 5, 7, escape = FALSE, bold = FALSE) |>
  kableExtra::group_rows("$h_a(X)$ misspecified", 8, 10, escape = FALSE, bold = FALSE) |>
  kableExtra::group_rows("both misspecified", 11, 13, bold = FALSE) |>
  footnote(
    threeparttable = TRUE,
    general_title = "",
    general = "Correct and misspecified refers to the specification of the
    nuisance models, $e_a(X)$ or $h_a(X)$. Results were averaged over
    10,000 simulations.",
    escape = FALSE
  )


