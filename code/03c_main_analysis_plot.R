# title: 03c_main_analysis_plot.R
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

# AGE-ADJUSTED ANALYSIS ----
plot_list <- list()
directory <- file.path("outputs", "estimates", "main_analysis", "age_adjusted")
for (i in dir(directory)) {
  estimates <- readRDS(file.path(directory, i))
  p <- plot.reallocation(estimates, partition, "Age-adjusted risk ratio")
  plot_list <- append(plot_list, list(p))
}

final_plot <- grid.arrange(grobs = plot_list, layout_matrix = plot_layout)
ggsave(
  file.path("outputs", "plots", "main_analysis", "age_adjusted.pdf"), 
  plot = final_plot, 
  width = 42, 
  height = 28, 
  units = "cm",
  device = cairo_pdf,
  create.dir = TRUE
)
ggsave(
  file.path("outputs", "plots", "main_analysis", "age_adjusted.png"), 
  plot = final_plot, 
  width = 4961, 
  height = 3307,
  units = "px",
)

# FULLY ADJUTED ANALYSIS ----
plot_list <- list()
directory <- file.path("outputs", "estimates", "main_analysis", "fully_adjusted")
for (i in grep(".RDS", dir(directory), value = TRUE)) {
  estimates <- readRDS(file.path(directory, i))
  p <- plot.reallocation(estimates, partition, "Adjusted risk ratio")
  plot_list <- append(plot_list, list(p))
}

final_plot <- grid.arrange(grobs = plot_list, layout_matrix = plot_layout)
ggsave(
  file.path("outputs", "plots", "main_analysis", "fully_adjusted.pdf"), 
  plot = final_plot, 
  width = 42, 
  height = 28,
  device = cairo_pdf,
  units = "cm",
)
ggsave(
  file.path("outputs", "plots", "main_analysis", "fully_adjusted.png"), 
  plot = final_plot, 
  width = 4961, 
  height = 3307,
  units = "px",
)