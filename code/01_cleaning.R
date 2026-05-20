# title: 01_cleaning.R
# author: Rayane Haddadj
# year: 2026

# LOAD LIBRARY ----
library(data.table)
library(zCompositions)

# LOAD DATA ----
baseline_df <- fread("data/raw_data/2024-04-16_114165_Data.csv")
names(baseline_df) <- sub("@", "_", names(baseline_df))
sleep_df <- fread("data/raw_data/2024-09-05_114165_Data.csv")
names(sleep_df) <- sub("@", "_", names(sleep_df))

hunt_df <- sleep_df[baseline_df, on = "PID_114165"]

# RUN CLEANING CODE ----
source("code/01a_cleaning_covariates.R")
source("code/01b_cleaning_accelerometer.R")
source("code/01c_inclusion_criteria.R")

# DEAL WITH ROUNDED ZEROS ----
partition <- c(
  "walking",
  "run_cycl",
  "sed_time",
  "standing",
  "sleep"
)

if (sum(hunt_df[analysis == 1, partition, with = FALSE] == 0) > 0) {
  hunt_df[
    analysis == 1,
    paste0(partition, "_lrem") := 
      lrEM(
        .SD,
        label = 0, 
        dl = rep(5/60/7, length(partition)),
        ini.cov = "complete.obs"
      ),
    .SDcols = partition
  ]
}

# REMOVE RAW DATA ----
hunt_df[, grep("[A-Z]", names(hunt_df)) := NULL]

# SAVE DATA ----
saveRDS(hunt_df, "data/cleaned_data/data_for_analysis_coda_lbp.rds")