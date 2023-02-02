
# path to rds 
rds_path <- "../../3_data_encrypted/rds/"

# load nested trial emulation data from MESA
trials <- read_rds(paste0(rds_path, "trials.rds"))
trials_pt <- read_rds(paste0(rds_path, "trials_pt.rds"))

trials_long <- read_rds(paste0(rds_path, "trials_long.rds"))
trials_long_pt <- read_rds(paste0(rds_path, "trials_long_pt.rds"))

# let's drop nesting for now
trial <- filter(trials_long, trial == 1 & exam == 2)
trial_pt_cens <- filter(trials_long_pt, trial == 1)

trial_pt_uncens <- 
  trials_long %>%
  group_by(trial, mesaid) %>%
  mutate(censor = max(as.numeric(drop)))  %>%
  ungroup() %>%
  uncount(counts, .id = "time") %>%
  group_by(trial, mesaid) %>%
  mutate(
    time = row_number(),
    cvda = replace(cvda, time != max(time), 0),
    censor = replace(censor, time != max(time), 0),
    dth = replace(dth, time != max(time), 0)
  ) %>%
  ungroup() %>%
  filter(trial == 1)
  
trial_itt <- filter(trials_pt, trial == 1)

rm(trials, trials_pt, trials_long, trials_long_pt)

# drop loss to follow up and competing event observations

# prepare datasets for 
cox_df <- 
  trial %>%
  mutate(
    # convert continuous risk factors to larger units to be more readily interpretable
    bl_age = bl_age / 5,    # age in 5 year increments
    bl_sbp = bl_sbp / 10,   # SBP in 10 mmHg increments
    bl_hdl = bl_hdl / 10,   # HDL in 10 mg/dL increments
    bl_chol = bl_chol / 10,  # total cholesterol in 10 mg/dL increments
  ) 

# record risk factor means prior to censoring
risk_factor_means <- 
  cox_df %>%
  summarise(across(c(bl_age, bl_sbp, bl_chol, bl_hdl), mean))

# center risk factors prior to fitting
cox_df <- 
  cox_df %>%
  mutate(
    across(
      .cols = c(bl_age, bl_sbp, bl_chol, bl_hdl),
      .fns = ~ as.numeric(scale(.x, scale = FALSE))
    ))

trial_pt_cens <- trial_pt_cens %>%
  mutate(
    bl_age = bl_age / 5,    # age in 5 year increments
    bl_sbp = bl_sbp / 10,   # SBP in 10 mmHg increments
    bl_hdl = bl_hdl / 10,   # HDL in 10 mg/dL increments
    bl_chol = bl_chol / 10, # total cholesterol in 10 mg/dL increments
    
    # center
    bl_age = bl_age - risk_factor_means$bl_age,    
    bl_sbp = bl_sbp - risk_factor_means$bl_sbp,   
    bl_hdl = bl_hdl - risk_factor_means$bl_hdl,   
    bl_chol = bl_chol - risk_factor_means$bl_chol, 
  ) 

trial_pt_uncens <- trial_pt_uncens %>%
  mutate(
    bl_age = bl_age / 5,    # age in 5 year increments
    bl_sbp = bl_sbp / 10,   # SBP in 10 mmHg increments
    bl_hdl = bl_hdl / 10,   # HDL in 10 mg/dL increments
    bl_chol = bl_chol / 10, # total cholesterol in 10 mg/dL increments
    
    # center
    bl_age = bl_age - risk_factor_means$bl_age,    
    bl_sbp = bl_sbp - risk_factor_means$bl_sbp,   
    bl_hdl = bl_hdl - risk_factor_means$bl_hdl,   
    bl_chol = bl_chol - risk_factor_means$bl_chol, 
  ) 


ipcw_df <- trial_pt_cens %>% filter(tx == 0)

# split into training and test datasets
set.seed(23457123)
unique_ids <- unique(trial$mesaid)
train_ids <- sample(unique_ids, floor(length(unique_ids) / 2))


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

log_transform <- c(
  "agatpm1c",
  "crp1",
  "il61",
  "exercise"
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

tv_vars_wide <- c(
  "age1c",
  "dm031c",
  "htn1c",
  "bmi1c",
  "htcm1",
  "wtlb1",
  "hipcm1",
  "waistcm1",
  "asacat1c",
  "lipid1c",
  "htnmed1c",
  "diabins1",
  "anydep1c",
  "anara1c",
  "vasoda1c",
  "income1",
  "hinone1",
  "cursmk1",
  "sbp1c",
  "dbp1c",
  "chol1",
  "hdl1",
  "ldl1",
  "trig1",
  "age2c",
  "dm032c",
  "htn2c",
  "bmi2c",
  "htcm2",
  "wtlb2",
  "hipcm2",
  "waistcm2",
  "asacat2c",
  "lipid2c",
  "htnmed2c",
  "diur2c",
  "diabins2",
  "anydep2c",
  "anara2c",
  "vasoda2c",
  "income2",
  "hinone2",
  "cursmk2",
  "sbp2c",
  "dbp2c",
  "chol2",
  "hdl2",
  "ldl2",
  "trig2",
  "age3c",
  "dm033c",
  "htn3c",
  "bmi3c",
  "htcm3",
  "wtlb3",
  "hipcm3",
  "waistcm3",
  "asacat3c",
  "lipid3c",
  "htnmed3c",
  "diur3c",
  "diabins3",
  "anydep3c",
  "anara3c",
  "vasoda3c",
  "income3",
  "hinone3",
  "cursmk3",
  "sbp3c",
  "dbp3c",
  "chol3",
  "hdl3",
  "ldl3",
  "trig3",
  "age4c",
  "dm034c",
  "htn4c",
  "bmi4c",
  "htcm4",
  "wtlb4",
  "hipcm4",
  "waistcm4",
  "asacat4c",
  "lipid4c",
  "htnmed4c",
  "diur4c",
  "diabins4",
  "anydep4c",
  "anara4c",
  "vasoda4c",
  "hinone4",
  "cursmk4",
  "sbp4c",
  "dbp4c",
  "chol4",
  "hdl4",
  "ldl4",
  "trig4",
  "age5c",
  "dm035c",
  "htn5c",
  "bmi5c",
  "htcm5",
  "wtlb5",
  "hipcm5",
  "waistcm5",
  "asacat5c",
  "lipid5c",
  "htnmed5c",
  "diur5c",
  "diabins5",
  "anydep5c",
  "anara5c",
  "vasoda5c",
  "income5",
  "hinone5",
  "cursmk5",
  "sbp5c",
  "dbp5c",
  "chol5",
  "hdl5",
  "ldl5",
  "trig5",
  "exercise1",
  "exercise2",
  "exercise3",
  "exercise4",
  "exercise5",
  "dpw1",
  "dpw2",
  "dpw3",
  "dpw4",
  "dpw5",
  "employed2",
  "employed3",
  "employed4",
  "employed5",
  "retired2",
  "retired3",
  "retired4",
  "retired5"
)

outcome_vars <- c(
  "cvdatt",
  "cvda",
  "dth",
  "dthtt"
)

time_vars_wide <- c(
  "time2",
  "time3",
  "time4",
  "time5"
)

ref_vars <- c(
  "race_white1",
  "educ_pri1"
)

cont_vars <- c(
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
  "agatpm1c",
  "crp1",
  "il61",
  "age",
  "waistcm",
  "sbp", 
  "dbp",
  "ldl",
  "hdl",
  "trig",
  "dpw",
  "exercise"
)

tv_adj_vars <- c(
  "lag1_dm03",
  "lag1_htn",
  "lag1_cursmk",
  "lag1_waistcm",
  "htnmed",
  "asacat",
  "diur",
  "diabins",
  "anydep",
  "vasoda",
  "anara",
  "lag1_hinone",
  "lag1_sbp",
  "lag1_dbp",
  "lag1_ldl",
  "lag1_hdl",
  "lag1_trig", 
  "lag1_dpw",
  "lag1_exercise"
)


