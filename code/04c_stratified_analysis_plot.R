# title: 04c_stratified_analysis_plot.R
# author: Rayane Haddadj
# year: 2026

# INITIALIZATION ----
library(data.table)
library(gridExtra)

source("code/00_functions_analysis.R")

partition <- c(
  "sleep_lrem" = "Sleep",
  "sed_time_lrem" = "Sedentary time",
  "standing_lrem" = "Standing",
  "walking_lrem" = "Walking",
  "run_cycl_lrem" = "Running/cycling"
)

plot_layout <- create.layout(length(partition) - 1)

input_path <- file.path("outputs", "estimates", "stratified_analysis")
output_path <- file.path("outputs", "plots", "stratified_analysis")

# AGE-STRATIFIED ANALYSIS ----
# younger participants
plot_list <- list()
directory <- file.path(input_path, "age_stratified", "younger")
for (i in dir(directory)) {
  estimates <- readRDS(file.path(directory, i))
  p <- plot.reallocation(estimates, partition, "Adjusted risk ratio")
  plot_list <- append(plot_list, list(p))
}
final_plot <- grid.arrange(grobs = plot_list, layout_matrix = plot_layout)
ggsave(
  file.path(output_path, "age_stratified", "younger.png"),
  create.dir = TRUE,
  plot = final_plot, 
  width = 4961, 
  height = 3307,
  units = "px"
)

# older participants
plot_list <- list()
directory <- file.path(input_path, "age_stratified", "older")
for (i in dir(directory)) {
  estimates <- readRDS(file.path(directory, i))
  p <- plot.reallocation(estimates, partition, "Adjusted risk ratio")
  plot_list <- append(plot_list, list(p))
}
final_plot <- grid.arrange(grobs = plot_list, layout_matrix = plot_layout)
ggsave(
  file.path(output_path, "age_stratified", "older.png"),
  create.dir = TRUE,
  plot = final_plot, 
  width = 4961, 
  height = 3307,
  units = "px"
)

# SEX-STRATIFIED ANALYSIS ----
# female
plot_list <- list()
directory <- file.path(input_path, "sex_stratified", "female")
for (i in dir(directory)) {
  estimates <- readRDS(file.path(directory, i))
  p <- plot.reallocation(estimates, partition, "Adjusted risk ratio")
  plot_list <- append(plot_list, list(p))
}
final_plot <- grid.arrange(grobs = plot_list, layout_matrix = plot_layout)
ggsave(
  file.path(output_path, "sex_stratified", "female.png"), 
  create.dir = TRUE,
  plot = final_plot, 
  width = 4961, 
  height = 3307,
  units = "px"
)

# male
plot_list <- list()
directory <- file.path(input_path, "sex_stratified", "male")
for (i in dir(directory)) {
  estimates <- readRDS(file.path(directory, i))
  p <- plot.reallocation(estimates, partition, "Adjusted risk ratio")
  plot_list <- append(plot_list, list(p))
}
final_plot <- grid.arrange(grobs = plot_list, layout_matrix = plot_layout)
ggsave(
  file.path(output_path, "sex_stratified", "male.png"),
  create.dir = TRUE,
  plot = final_plot, 
  width = 4961, 
  height = 3307,
  units = "px"
)

# SLEEP-STRATIFIED ANALYSIS ----
# short sleeper
plot_list <- list()
directory <- file.path(input_path, "sleep_stratified", "short_sleeper")
for (i in dir(directory)) {
  estimates <- readRDS(file.path(directory, i))
  p <- plot.reallocation(estimates, partition, "Adjusted risk ratio")
  plot_list <- append(plot_list, list(p))
}
final_plot <- grid.arrange(grobs = plot_list, layout_matrix = plot_layout)
ggsave(
  file.path(output_path, "sleep_stratified", "short_sleeper.png"),
  create.dir = TRUE,
  plot = final_plot, 
  width = 4961, 
  height = 3307,
  units = "px"
)

# long sleeper
plot_list <- list()
directory <- file.path(input_path, "age_stratified", "older")
for (i in dir(directory)) {
  estimates <- readRDS(file.path(directory, i))
  p <- plot.reallocation(estimates, partition, "Adjusted risk ratio")
  plot_list <- append(plot_list, list(p))
}
final_plot <- grid.arrange(grobs = plot_list, layout_matrix = plot_layout)
ggsave(
  file.path(output_path, "sleep_stratified", "long_sleeper.png"),
  create.dir = TRUE,
  plot = final_plot, 
  width = 4961, 
  height = 3307,
  units = "px",
)