
# load data ---------------------------------------------------------------

# path to clean MESA data 
rds_path <- "../../3_data_encrypted/rds/"

# load nested trial data
mesa <- read_rds(paste0(rds_path, "trials_long.rds"))


# clean application data --------------------------------------------------

# let's drop nesting for now (i.e. limit to first trial)
mesa <- filter(mesa, trial == 1)

# create adherence variable
mesa <- 
  mesa |> 
  group_by(mesaid) |> 
  mutate(
    adhere = 1 - max(drop),
    A = as.numeric(tx == 1 | (tx == 0 & adhere == 0))
  ) |>
  ungroup()

# because we're collapsing to (0/1) we only need baseline
mesa <- filter(mesa, exam == 2)

# get counts of eventtypes
mesa |>
  count(tx, adhere, cvda)
# A tibble: 8 × 4
#    tx adhere  cvda     n
# <dbl>  <dbl> <dbl> <int>
#     0      0     0   924
#     0      0     1    51
#     0      1     0  2573
#     0      1     1   226
#     1      0     0   181
#     1      0     1     8
#     1      1     0   183
#     1      1     1    23


# create training/test splits ---------------------------------------------

# check that we have unique obs
stopifnot(!any(duplicated(mesa$mesaid)))

# split into training and test datasets
set.seed(23457123)
split <- sample(1:nrow(mesa), floor(length(1:nrow(mesa)) / 2))

train <- mesa[split, ]
test <- mesa[-split, ]


# variable lists ----------------------------------------------------------

baseline_vars <- c(
  "gender1",
  "married1",
  "educ_sec1",
  "educ_col1",
  "race_black1",
  "race_hisp1",
  "race_asia1",
  "employed1",
  "retired1",
  "evsmk1",
  "cesd1c",
  "chrbu61c",
  "discry1c",
  "emot1c",
  "hassl1c",
  "splang1c",
  "splanx1c",
  "pregn1",
  "bpillyr1",
  "menoage1",
  "nprob1c",
  "famhist1",
  "agatpm1c",
  "ecglvh1c",
  "crp1",
  "il61"
)

baseline_vars_long <- c(
  "bl_age",
  "bl_dm03",
  "bl_htn",
  "bl_cursmk",
  "bl_waistcm",
  "bl_htnmed",
  "bl_asacat",
  "bl_diur",
  "bl_diabins",
  "bl_anydep",
  "bl_vasoda",
  "bl_anara",
  "bl_hinone",
  "bl_sbp",
  "bl_dbp",
  "bl_ldl",
  "bl_hdl",
  "bl_trig",
  "bl_dpw",
  "bl_exercise"
)

# create list of time-varying covariates
tv_vars <- c(
  "age",
  "dm03",
  "htn",
  "cursmk",
  "waistcm",
  "htnmed",
  "asacat",
  "diur",
  "diabins",
  "anydep",
  "vasoda",
  "anara",
  "hinone",
  "sbp",
  "dbp",
  "ldl",
  "hdl",
  "trig", 
  "dpw",
  "exercise"
)

model_vars <- c(
  "bl_age",
  "gender1",
  "bl_cursmk",
  "bl_dm03",
  "bl_sbp",
  "bl_hdl",
  "bl_chol",
  "bl_htnmed"
)



