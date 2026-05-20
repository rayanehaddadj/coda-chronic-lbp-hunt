# title: 01b_cleaning_accelerometer.R
# author: Rayane Haddadj
# year: 2026

#-------------------------------------------------------------------------------
# CLEAN DATA -------------------------------------------------------------------
#-------------------------------------------------------------------------------

# WEAR TIME ----
# convert seconds to minutes
hunt_df[
  , 
  sub(
    "NT4ActMX", 
    "min", 
    grep("^(?!.*Part|Sle|Wak).*NT4ActMX", names(hunt_df), perl = TRUE, value = TRUE)
  ) := 
    lapply(.SD, function(x) x / 60),
  .SDcols = grep("^(?!.*Part|Sle|Wak).*NT4ActMX", names(hunt_df), perl = TRUE)
]

# sleep time and lying awake
for (i in 1:7) {
  sleep_var <- paste0("SleTmD", i, "_NT4ActMX")
  hunt_df[, paste0("SleTmD", i, "_min") := get(sleep_var)]
  hunt_df[
    ,
    paste0("LyAwTmD", i, "_min") :=
      fifelse(
        is.na(get(sleep_var)),
        get(paste0("LyTmD", i, "_min")),
        get(paste0("LyTmD", i, "_min")) - get(paste0("SleTmD", i, "_min"))
      )
  ]
}

# VALID DAYS WITHOUT SLEEP MODEL ----
# raw wear time and number of valid days
for (i in 1:7){
  hunt_df[
    ,
    paste0("wt_D", i, "_raw_ns") := rowSums(.SD), 
    .SDcols = grep(
      paste0("(?<!(Sle|LyAw|Walk))TmD", i, "_min"), 
      names(hunt_df), 
      perl = TRUE
    )
  ]
}
hunt_df[
  , 
  valid_d_raw_ns := 
    rowSums(hunt_df[, paste0("wt_D", 1:7, "_raw_ns")] == 1440, na.rm = TRUE)
]

# remove days with 0 min of lying down/sitting/standing/walking
for (i in 1:7){
  col_to_check <- grep(
    paste0("(Ly|Sit|Stand|Walk)TmD", i, "_min"), 
    names(hunt_df),
    perl = TRUE,
    value = TRUE
  )
  col_baseline <- grep(
    paste0("(?<!(Sle|LyAw))TmD", i, "_min"),
    names(hunt_df),
    perl = TRUE,
    value = TRUE
  )
  hunt_df[
    , 
    cond0 := rowSums(
      .SD <= 0 | is.na(.SD), na.rm = TRUE),
    .SDcols = col_to_check
  ]
  for (j in col_baseline){
    hunt_df[cond0 == 0, sub("min", "ns", j) := get(j)]
  }
  hunt_df[, cond0 := NULL]  
}

# updated wear time and number of valid days
for (i in 1:7){
  hunt_df[
    ,
    paste0("wt_D", i, "_clean_ns") := rowSums(.SD), 
    .SDcols = grep(
      paste0("(?<!Walk)TmD", i, "_ns"), 
      names(hunt_df), 
      perl = TRUE
    )
  ]
}
hunt_df[
  , 
  valid_d_clean_ns := 
    rowSums(hunt_df[, paste0("wt_D", 1:7, "_clean_ns")] == 1440, na.rm = TRUE)
]

# VALID DAYS WITH SLEEP MODEL ----
# raw wear time and number of valid days
for (i in 1:7){
  hunt_df[
    ,
    paste0("wt_D", i, "_raw_sle") := rowSums(.SD), 
    .SDcols = grep(
      paste0("(?<!(Ly|Walk))TmD", i, "_min"), 
      names(hunt_df), 
      perl = TRUE
    )
  ]
}
hunt_df[
  , 
  valid_d_raw_sle := 
    rowSums(hunt_df[, paste0("wt_D", 1:7, "_raw_sle")] == 1440, na.rm = TRUE)
]

# remove days with 0 min of sleep/lying awake/sitting/standing/walking
for (i in 1:7){
  col_to_check <- grep(
    paste0("(Sle|LyAw|Sit|Stand|Walk)TmD", i, "_min"), 
    names(hunt_df),
    perl = TRUE,
    value = TRUE
  )
  col_baseline <- grep(
    paste0("(?<!Ly)TmD", i, "_min"),
    names(hunt_df),
    perl = TRUE,
    value = TRUE
  )
  hunt_df[
    , 
    cond0 := rowSums(
      .SD <= 0 | is.na(.SD), na.rm = TRUE),
    .SDcols = col_to_check
  ]
  for (j in col_baseline){
    hunt_df[cond0 == 0, sub("min", "sle", j) := get(j)]
  }
  hunt_df[, cond0 := NULL]  
}

# updated wear time and number of valid days
for (i in 1:7){
  hunt_df[
    ,
    paste0("wt_D", i, "_clean_sle") := rowSums(.SD), 
    .SDcols = grep(
      paste0("(?<!Walk)TmD", i, "_sle"), 
      names(hunt_df), 
      perl = TRUE
    )
  ]
}
hunt_df[
  , 
  n_valid_d := 
    rowSums(hunt_df[, paste0("wt_D", 1:7, "_clean_sle")] == 1440, na.rm = TRUE)
]

#-------------------------------------------------------------------------------
# COMPUTE DAILY AVERAGE --------------------------------------------------------
#------------------------------------------------------------------------------

# SLEEP ----
hunt_df[
  , 
  sleep := rowMeans(.SD, na.rm = TRUE),
    .SDcols = grep("SleTmD[1-7]_sle", names(hunt_df))
]

# SEDENTARY BEHAVIOURS ----
# lying Down
hunt_df[
  , 
  lying := rowMeans(.SD, na.rm = TRUE),
  .SDcols = grep("LyAwTmD[1-7]_sle", names(hunt_df))
]

# sitting
hunt_df[
  , 
  sitting := rowMeans(.SD, na.rm = TRUE),
  .SDcols = grep("SitTmD[1-7]_sle", names(hunt_df))
]

# standing
hunt_df[
  , 
  standing := rowMeans(.SD, na.rm = TRUE),
  .SDcols = grep("StandTmD[1-7]_sle", names(hunt_df))
]

# sedentary time
hunt_df[, sed_time := lying + sitting]

# total sedentary time
hunt_df[, sed_tot := lying + sitting + standing]

# PHYSICAL ACTIVITY ----
# slow Walking
hunt_df[
  , 
  walk1 := rowMeans(.SD, na.rm = TRUE),
  .SDcols = grep("Walk1TmD[1-7]_sle", names(hunt_df))
]

# moderate Walking
hunt_df[
  , 
  walk2 := rowMeans(.SD, na.rm = TRUE),
  .SDcols = grep("Walk2TmD[1-7]_sle", names(hunt_df))
]

# brisk Walking
hunt_df[
  , 
  walk3 := rowMeans(.SD, na.rm = TRUE),
  .SDcols = grep("Walk3TmD[1-7]_sle", names(hunt_df))
]

# total Walking
hunt_df[
  , 
  walking := rowMeans(.SD, na.rm = TRUE),
  .SDcols = grep("WalkTmD[1-7]_sle", names(hunt_df))
]

# running
hunt_df[
  , 
  running := rowMeans(.SD, na.rm = TRUE),
  .SDcols = grep("RunTmD[1-7]_sle", names(hunt_df))
]

# cycling
hunt_df[
  , 
  cycling := rowMeans(.SD, na.rm = TRUE),
  .SDcols = grep("CyclTmD[1-7]_sle", names(hunt_df))
]

# mvpa walking
hunt_df[, walk_mvpa := walk2 + walk3]

# running/cycling
hunt_df[, run_cycl := running + cycling]

# lpa
hunt_df[, lpa := walk1]

# mvpa
hunt_df[, mvpa := walk2 + walk3 + running + cycling]

# total pa
hunt_df[, pa := walking + running + cycling]