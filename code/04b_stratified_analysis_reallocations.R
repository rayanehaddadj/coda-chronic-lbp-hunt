# title: 04b_stratified_analysis_reallocations.R
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

path <- file.path("outputs", "models", "stratified_analysis")
rds <- "model.RDS"
model_young <- readRDS(file.path(path, "age_stratified", "younger", rds))
model_old <- readRDS(file.path(path, "age_stratified", "older", rds))
model_female <- readRDS(file.path(path, "sex_stratified", "female", rds))
model_male <- readRDS(file.path(path, "sex_stratified", "male", rds))
model_short <- readRDS(file.path(path, "sleep_stratified", "short_sleeper", rds))
model_long <- readRDS(file.path(path, "sleep_stratified", "long_sleeper", rds))

# PAIRWISE REALLOCATIONS ----
count <- 1
for (i in seq_along(partition)){
  if (i < length(partition)) {
    for (j in (i+1):length(partition)) {
      # age_stratified models
      pairwise.reallocation(
        data = hunt_valid,
        model = model_young,
        composition = partition,
        reference = "compositional_mean",
        behaviour1 = partition[[i]],
        behaviour2 = partition[[j]],
        time = 60,
        output_dir = "stratified_analysis/age_stratified/younger",
        file_name = count
      )
      pairwise.reallocation(
        data = hunt_valid,
        model = model_old,
        composition = partition,
        reference = "compositional_mean",
        behaviour1 = partition[[i]],
        behaviour2 = partition[[j]],
        time = 60,
        output_dir = "stratified_analysis/age_stratified/older",
        file_name = count
      )
      
      # sex-stratified models
      pairwise.reallocation(
        data = hunt_valid,
        model = model_female,
        composition = partition,
        reference = "compositional_mean",
        behaviour1 = partition[[i]],
        behaviour2 = partition[[j]],
        time = 60,
        output_dir = "stratified_analysis/sex_stratified/female",
        file_name = count
      )
      pairwise.reallocation(
        data = hunt_valid,
        model = model_male,
        composition = partition,
        reference = "compositional_mean",
        behaviour1 = partition[[i]],
        behaviour2 = partition[[j]],
        time = 60,
        output_dir = "stratified_analysis/sex_stratified/male",
        file_name = count
      )
      
      # sleep-stratified models
      pairwise.reallocation(
        data = hunt_valid,
        model = model_short,
        composition = partition,
        reference = "compositional_mean",
        behaviour1 = partition[[i]],
        behaviour2 = partition[[j]],
        time = 60,
        output_dir = "stratified_analysis/sleep_stratified/short_sleeper",
        file_name = count
      )
      pairwise.reallocation(
        data = hunt_valid,
        model = model_long,
        composition = partition,
        reference = "compositional_mean",
        behaviour1 = partition[[i]],
        behaviour2 = partition[[j]],
        time = 60,
        output_dir = "stratified_analysis/sleep_stratified/long_sleeper",
        file_name = count
      )
      
      count <- count + 1
    }
  }
}