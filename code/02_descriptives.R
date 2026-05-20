# title: 02_descriptives.R
# author: Rayane Haddadj
# year: 2026

# INITIALIZATION ----
library(data.table)
library(compositions)
library(table1) # For label()
library(gtsummary) # For tbl_summary()
library(gt) # For gtsave()

hunt_df <- readRDS("data/cleaned_data/data_for_analysis_coda_lbp.rds")
hunt_valid <- hunt_df[analysis == 1]

label(hunt_valid$id) <- "Participants"
label(hunt_valid$age) <- "Age"
label(hunt_valid$education) <- "Education"
label(hunt_valid$income) <- "Yearly household income"
label(hunt_valid$work_status) <- "Employment status"
label(hunt_valid$smoking) <- "Smoking status"
label(hunt_valid$depression) <- "Depression"
label(hunt_valid$sleep) <- "Sleep"
label(hunt_valid$sed_time) <- "Sedentary time"
label(hunt_valid$standing) <- "Standing"
label(hunt_valid$walking) <- "Walking"
label(hunt_valid$run_cycl) <- "Running/cycling"
label(hunt_valid$n_valid_d) <- "Days of accelerometry"

# COMPOSITIONAL MEAN ----
partition <- c(
  "sleep_lrem",
  "sed_time_lrem",
  "standing_lrem",
  "walking_lrem",
  "run_cycl_lrem"
)
data.table(t(geometricmeanCol(acomp(hunt_valid[, ..partition])))) * 1440

# TABLE 1 ----
# set language
theme_gtsummary_language(language = "en", big.mark = "")

# create table 1
table1 <- 
  tbl_summary(
    hunt_valid, 
    include = c(
      id,
      sex,
      age,
      education,
      income,
      work_status,
      smoking,
      depression,
      sleep,
      sed_time,
      standing,
      walking,
      run_cycl,
      n_valid_d
    ),
    value = c(sex ~ "Female"),
    label = c(sex ~ "Female"),
    type = c(c(id, n_valid_d) ~ "continuous", depression ~ "categorical"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", 
      all_categorical() ~ "{n} ({p})",
      id ~ "{N_obs}" # keep last, overwrite otherwise
    ),
    digits = c(
      all_continuous() ~ 1, 
      all_categorical() ~ c(0,1), 
      c(id) ~ 0 # keep last, overwrite otherwise
    ),
    by = lbp_t1
  ) %>%
  add_overall() %>%
  modify_header(all_stat_cols() ~ "{level}") %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**Chronic LBP at follow-up**") %>%
  add_stat_label(
    label = c(
      all_continuous() ~ "mean (SD)", 
      all_categorical() ~ "n (%)",
      id ~ "n" # keep last, overwrite otherwise
    )
  ) %>% 
  modify_column_alignment(columns = everything(), align = "left")

# save table1
if (file.exists("outputs/descriptives/table1_raw.docx")) {
  file.remove("outputs/descriptives/table1_raw.docx")
} 
table1 <- as_gt(table1)
gtsave(table1, filename = "outputs/descriptives/table1_raw.docx")

# INCLUDED VS EXCLUDED PARTICIPANTS ----
table_suppl <- 
  tbl_summary(
    hunt_df, 
    include = c(
      id,
      sex,
      age,
      education,
      income,
      work_status,
      smoking,
      depression
    ),
    value = c(sex ~ "Female"),
    label = c(sex ~ "Female"),
    type = c(id ~ "continuous", depression ~ "categorical"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", 
      all_categorical() ~ "{n} ({p})",
      id ~ "{N_obs}" # keep last, overwrite otherwise
    ),
    digits = c(
      all_continuous() ~ 1, 
      all_categorical() ~ c(0,1), 
      c(id) ~ 0 # keep last, overwrite otherwise
    ),
    by = analysis,
    missing = "no"
  ) %>%
  add_overall() %>%
  modify_header(all_stat_cols() ~ "{level}") %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**Included in study**") %>%
  add_stat_label(
    label = c(
      all_continuous() ~ "mean (SD)", 
      all_categorical() ~ "n (%)",
      id ~ "n" # keep last, overwrite otherwise
    )
  ) %>% 
  modify_column_alignment(columns = everything(), align = "left")

# save table
if (file.exists("outputs/descriptives/table_supplementary_raw.docx")) {
  file.remove("outputs/descriptives/table_supplementary_raw.docx")
} 
table_suppl <- as_gt(table_suppl)
gtsave(table_suppl, filename = "outputs/descriptives/table_supplementary_raw.docx")
