# title: 03a_main_analysis_models.R
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

full_adj <- c(
  "age", 
  "sex",
  "education",
  "income",
  "work_status",
  "smoking",
  "depression"
)

# RUN ANALYSIS ----
for (i in partition){
  index <- which(partition == i)
  partition_ordered <- c(partition[index:length(partition)], partition[1:index - 1])
  ilr <- paste0("ilr_", 1:(length(partition) - 1))
  hunt_valid[
    , 
    (ilr) := data.table(create.ilr(.SD)), 
    .SDcols = partition_ordered
  ]
  
  # age-adjusted analysis
  poisson.ilr(
    data = hunt_valid,
    outcome = "lbp_t1",
    exposure = ilr,
    covariates = "age",
    output_dir = "main_analysis/age_adjusted",
    model_name = sub("_lrem", "", i)
  )
  
  # fully adjusted analysis
  poisson.ilr(
    data = hunt_valid,
    outcome = "lbp_t1",
    exposure = ilr,
    covariates = full_adj,
    output_dir = "main_analysis/fully_adjusted",
    model_name = sub("_lrem", "", i)
  )
}