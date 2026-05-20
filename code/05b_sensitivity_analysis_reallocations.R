# title: 05b_sensitivity_analysis_reallocations.R
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

path <- file.path("outputs", "models", "sensitivity_analysis")
rds <- "model.RDS"
model_no_msp <- readRDS(file.path(path, "no_chronic_pain", rds))
model_pain_l4w <- readRDS(file.path(path, "no_pain_last_4_weeks", rds))
model_health_status <- readRDS(file.path(path, "health_status", rds))
model_chr_dis <- readRDS(file.path(path, "no_chronic_disease", rds))
model_hosp <- readRDS(file.path(path, "no_hospitalization_last_12m", rds))
model_valid_day <- readRDS(file.path(path, "4_valid_days", rds))

# PAIRWISE REALLOCATIONS ----
count <- 1
for (i in seq_along(partition)){
  if (i < length(partition)) {
    for (j in (i+1):length(partition)) {
      # no chronic pain
      pairwise.reallocation(
        data = hunt_valid,
        model = model_no_msp,
        composition = partition,
        reference = "compositional_mean",
        behaviour1 = partition[[i]],
        behaviour2 = partition[[j]],
        time = 60,
        output_dir = "sensitivity_analysis/no_chronic_pain",
        file_name = count
      )
      
      # no pain last 4 weeks
      pairwise.reallocation(
        data = hunt_valid,
        model = model_pain_l4w,
        composition = partition,
        reference = "compositional_mean",
        behaviour1 = partition[[i]],
        behaviour2 = partition[[j]],
        time = 60,
        output_dir = "sensitivity_analysis/no_pain_last_4_weeks",
        file_name = count
      )
      
      # no poor health_status
      pairwise.reallocation(
        data = hunt_valid,
        model = model_health_status,
        composition = partition,
        reference = "compositional_mean",
        behaviour1 = partition[[i]],
        behaviour2 = partition[[j]],
        time = 60,
        output_dir = "sensitivity_analysis/health_status",
        file_name = count
      )
      
      # no history of diabetes/cancer/cvd
      pairwise.reallocation(
        data = hunt_valid,
        model = model_chr_dis,
        composition = partition,
        reference = "compositional_mean",
        behaviour1 = partition[[i]],
        behaviour2 = partition[[j]],
        time = 60,
        output_dir = "sensitivity_analysis/no_chronic_disease",
        file_name = count
      )
      
      # no hospitalization last 12 months
      pairwise.reallocation(
        data = hunt_valid,
        model = model_hosp,
        composition = partition,
        reference = "compositional_mean",
        behaviour1 = partition[[i]],
        behaviour2 = partition[[j]],
        time = 60,
        output_dir = "sensitivity_analysis/no_hospitalization_last_12m",
        file_name = count
      )
      
      # >= 4 valid days of accelerometry
      pairwise.reallocation(
        data = hunt_valid,
        model = model_valid_day,
        composition = partition,
        reference = "compositional_mean",
        behaviour1 = partition[[i]],
        behaviour2 = partition[[j]],
        time = 60,
        output_dir = "sensitivity_analysis/4_valid_days",
        file_name = count
      )
      
      count <- count + 1
    }
  }
}