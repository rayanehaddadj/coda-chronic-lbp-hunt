# title: 04a_stratified_analysis_models.R
# author: Rayane Haddadj
# year: 2026

# INITIALIZATION ----
library(data.table)

source("code/00_functions_analysis.R")

hunt_df <- readRDS("data/cleaned_data/data_for_analysis_coda_lbp.rds")
hunt_valid <- hunt_df[analysis == 1]

partition <- c(
  "sleep_lrem",
  "sed_time_lrem",
  "standing_lrem",
  "walking_lrem",
  "run_cycl_lrem"
)
ilr <- paste0("ilr_", 1:(length(partition) - 1))
hunt_valid[
  , 
  (ilr) := data.table(create.ilr(.SD)), 
  .SDcols = partition
]

# AGE-STRATIFIED ANALYSIS ----
adj_age <- c(
  "sex",
  "education",
  "income",
  "work_status",
  "smoking",
  "depression"
)

# younger participants
hunt_young <- hunt_valid[age < 65]
poisson.ilr(
  data = hunt_young,
  outcome = "lbp_t1",
  exposure = ilr,
  covariates = adj_age,
  output_dir = "stratified_analysis/age_stratified",
  model_name = "younger"
)

# older participants
hunt_old <- hunt_valid[age >= 65]
poisson.ilr(
  data = hunt_old,
  outcome = "lbp_t1",
  exposure = ilr,
  covariates = adj_age,
  output_dir = "stratified_analysis/age_stratified",
  model_name = "older"
)

# SEX-STRATIFIED ANALYSIS ----
adj_sex <- c(
  "age",
  "education",
  "income",
  "work_status",
  "smoking",
  "depression"
)

# female
hunt_female <- hunt_valid[sex == "Female"]
poisson.ilr(
  data = hunt_female,
  outcome = "lbp_t1",
  exposure = ilr,
  covariates = adj_sex,
  output_dir = "stratified_analysis/sex_stratified",
  model_name = "female"
)

# male
hunt_male <- hunt_valid[sex == "Male"]
poisson.ilr(
  data = hunt_male,
  outcome = "lbp_t1",
  exposure = ilr,
  covariates = adj_sex,
  output_dir = "stratified_analysis/sex_stratified",
  model_name = "male"
)

# SLEEP-STRATIFIED ANALYSIS ----
adj_sleep <- c(
  "age",
  "sex",
  "education",
  "income",
  "work_status",
  "smoking",
  "depression"
)

# short sleeper
hunt_short <- hunt_valid[sleep < 420]
poisson.ilr(
  data = hunt_short,
  outcome = "lbp_t1",
  exposure = ilr,
  covariates = adj_sleep,
  output_dir = "stratified_analysis/sleep_stratified",
  model_name = "short_sleeper"
)

# older participants
hunt_long <- hunt_valid[sleep >= 420]
poisson.ilr(
  data = hunt_long,
  outcome = "lbp_t1",
  exposure = ilr,
  covariates = adj_sleep,
  output_dir = "stratified_analysis/sleep_stratified",
  model_name = "long_sleeper"
)