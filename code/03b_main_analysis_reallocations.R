# title: 03b_main_analysis_reallocations.R
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

model_age_adj <- readRDS("outputs/models/main_analysis/age_adjusted/sleep/model.RDS")
model_full_adj <- readRDS("outputs/models/main_analysis/fully_adjusted/sleep/model.RDS")

# PAIRWISE REALLOCATIONS ----
count <- 1
for (i in seq_along(partition)){
  if (i < length(partition)) {
    for (j in (i+1):length(partition)) {
      pairwise.reallocation(
        data = hunt_valid,
        model = model_age_adj,
        composition = partition,
        reference = "compositional_mean",
        behaviour1 = partition[[i]],
        behaviour2 = partition[[j]],
        time = 60,
        output_dir = "main_analysis/age_adjusted",
        file_name = count
      )
      pairwise.reallocation(
        data = hunt_valid,
        model = model_full_adj,
        composition = partition,
        reference = "compositional_mean",
        behaviour1 = partition[[i]],
        behaviour2 = partition[[j]],
        time = 60,
        output_dir = "main_analysis/fully_adjusted",
        file_name = count,
        estimates_table = TRUE
      )
      count <- count + 1
    }
  }
}