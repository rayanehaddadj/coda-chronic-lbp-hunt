# title: 05c_sensitivity_analysis_plot.R
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

input_path <- file.path("outputs", "estimates", "sensitivity_analysis")
output_path <- file.path("outputs", "plots", "sensitivity_analysis")

# NO CHRONIC PAIN ----
plot_list <- list()
directory <- file.path(input_path, "no_chronic_pain")
for (i in dir(directory)) {
  estimates <- readRDS(file.path(directory, i))
  p <- plot.reallocation(estimates, partition, "Adjusted risk ratio")
  plot_list <- append(plot_list, list(p))
}
final_plot <- grid.arrange(grobs = plot_list, layout_matrix = plot_layout)
ggsave(
  file.path(output_path, "no_chronic_pain.png"),
  create.dir = TRUE,
  plot = final_plot, 
  width = 4961, 
  height = 3307,
  units = "px"
)

# NO PAIN LAST 4 WEEKS ----
plot_list <- list()
directory <- file.path(input_path, "no_pain_last_4_weeks")
for (i in dir(directory)) {
  estimates <- readRDS(file.path(directory, i))
  p <- plot.reallocation(estimates, partition, "Adjusted risk ratio")
  plot_list <- append(plot_list, list(p))
}
final_plot <- grid.arrange(grobs = plot_list, layout_matrix = plot_layout)
ggsave(
  file.path(output_path, "pain_last_4_weeks.png"),
  create.dir = TRUE,
  plot = final_plot, 
  width = 4961, 
  height = 3307,
  units = "px"
)

# NO POOR HEALTH STATUS ----
plot_list <- list()
directory <- file.path(input_path, "health_status")
for (i in dir(directory)) {
  estimates <- readRDS(file.path(directory, i))
  p <- plot.reallocation(estimates, partition, "Adjusted risk ratio")
  plot_list <- append(plot_list, list(p))
}
final_plot <- grid.arrange(grobs = plot_list, layout_matrix = plot_layout)
ggsave(
  file.path(output_path, "health_status.png"),
  create.dir = TRUE,
  plot = final_plot, 
  width = 4961, 
  height = 3307,
  units = "px"
)

# NO DIABETES/CANCER/CVD ----
plot_list <- list()
directory <- file.path(input_path, "no_chronic_disease")
for (i in dir(directory)) {
  estimates <- readRDS(file.path(directory, i))
  p <- plot.reallocation(estimates, partition, "Adjusted risk ratio")
  plot_list <- append(plot_list, list(p))
}
final_plot <- grid.arrange(grobs = plot_list, layout_matrix = plot_layout)
ggsave(
  file.path(output_path, "no_chronic_disease.png"),
  create.dir = TRUE,
  plot = final_plot, 
  width = 4961, 
  height = 3307,
  units = "px"
)

# NO HOSPITALIZATION LAST 12 MONTHS ----
plot_list <- list()
directory <- file.path(input_path, "no_hospitalization_last_12m")
for (i in dir(directory)) {
  estimates <- readRDS(file.path(directory, i))
  p <- plot.reallocation(estimates, partition, "Adjusted risk ratio")
  plot_list <- append(plot_list, list(p))
}
final_plot <- grid.arrange(grobs = plot_list, layout_matrix = plot_layout)
ggsave(
  file.path(output_path, "no_hospitalization_last_12m.png"),
  create.dir = TRUE,
  plot = final_plot, 
  width = 4961, 
  height = 3307,
  units = "px"
)

# >= 4 VALID DAYS OF ACCELEROMETRY ----
plot_list <- list()
directory <- file.path(input_path, "4_valid_days")
for (i in dir(directory)) {
  estimates <- readRDS(file.path(directory, i))
  p <- plot.reallocation(estimates, partition, "Adjusted risk ratio")
  plot_list <- append(plot_list, list(p))
}
final_plot <- grid.arrange(grobs = plot_list, layout_matrix = plot_layout)
ggsave(
  file.path(output_path, "4_valid_days.png"),
  create.dir = TRUE,
  plot = final_plot, 
  width = 4961, 
  height = 3307,
  units = "px"
)