# title: 05a_sensitivity_analysis_models.R
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

full_adj <- c(
  "age",
  "sex",
  "education",
  "income",
  "work_status",
  "smoking",
  "depression"
)

# NO CHRONIC PAIN ----
hunt_no_msp <- hunt_valid[msp_t0 == 0]
poisson.ilr(
  data = hunt_no_msp,
  outcome = "lbp_t1",
  exposure = ilr,
  covariates = full_adj,
  output_dir = "sensitivity_analysis/no_chronic_pain",
  model_name = NULL
)

# NO PAIN LAST 4 WEEKS ----
hunt_pain_l4w <- hunt_valid[!pain_l4w %in% c(NA, "Moderate", "Strong", "Very strong")]
poisson.ilr(
  data = hunt_pain_l4w,
  outcome = "lbp_t1",
  exposure = ilr,
  covariates = full_adj,
  output_dir = "sensitivity_analysis/no_pain_last_4_weeks",
  model_name = NULL
)

# NO POOR HEALTH STATUS ----
hunt_health_status <- hunt_valid[!health_status %in% c(NA, "Poor", "Not so good")]
poisson.ilr(
  data = hunt_health_status,
  outcome = "lbp_t1",
  exposure = ilr,
  covariates = full_adj,
  output_dir = "sensitivity_analysis/health_status",
  model_name = NULL
)

# NO HISTORY OF DIABETES/CANCER/CVD ----
hunt_chr_dis <- hunt_valid[valid_chr_dis == 1 & chronic_disease == 0]
poisson.ilr(
  data = hunt_chr_dis,
  outcome = "lbp_t1",
  exposure = ilr,
  covariates = full_adj,
  output_dir = "sensitivity_analysis/no_chronic_disease",
  model_name = NULL
)

# NO HOSPITALIZATON LAST 12 MONTHS ----
hunt_hosp <- hunt_valid[hosp_12m == 0]
poisson.ilr(
  data = hunt_hosp,
  outcome = "lbp_t1",
  exposure = ilr,
  covariates = full_adj,
  output_dir = "sensitivity_analysis/no_hospitalization_last_12m",
  model_name = NULL
)

# >= 4 VALID DAYS OF ACCELEROMETRY ----
hunt_valid_days <- hunt_valid[n_valid_d >= 4]
poisson.ilr(
  data = hunt_valid_days,
  outcome = "lbp_t1",
  exposure = ilr,
  covariates = full_adj,
  output_dir = "sensitivity_analysis/4_valid_days",
  model_name = NULL
)