# title: 01a_cleaning_covariates.R
# author: Rayane Haddadj
# year: 2026

# CLEAN VARIABLES ----
#id
hunt_df[, id := PID_114165]

# sex
hunt_df[
  , 
  sex := factor(
    Sex, 
    levels = 0:1,
    labels = c("Female", "Male")
  )
]

# age
hunt_df[, age := PartAg_NT4BLM]

# education
hunt_df[
  , education := factor(
    Educ_NT4BLQ1,
    levels = 1:6,
    labels = c(
      "Primary school", 
      "1-2y of academic/vocational school",
      "3y of academic/vocational school",
      "3-4y of academic/vocational school",
      "University <4y",
      "University ≥4 y"
    )
  )
]

# education 3-level
hunt_df[
  , 
  education3 := fcase(
    as.numeric(education) == 1                             , "Primary school",
    as.numeric(education) >= 2 & as.numeric(education) <= 4, "Secondary school",
    as.numeric(education) >= 5                             , "University"
  )
]

# income 
hunt_df[
  ,
  income := factor(
    IncoTot_NT4BLQ1,
    levels = 1:5,
    labels = c(
      "<25 000 USD",
      "25 000-45 000 USD",
      "45 100-75 000 USD",
      "75 100-100 000 USD",
      ">100 000 USD"
    )
  )
]

# employment status
hunt_df[
  ,
  work_status := factor(
    WorCu_NT4BLI, 
    levels = 0:1, 
    labels = c("Employed", "Non-employed")
  )
]

# smoking status
hunt_df[
  , 
  smoking := factor(
    fcase(
      SmoStat_NT4BLQ1 == 0,         "Non-smoker",
      SmoStat_NT4BLQ1 %in% c(1, 4), "Former smoker",
      SmoStat_NT4BLQ1 %in% c(2, 3), "Current smoker"), 
    levels = c("Non-smoker", "Former smoker", "Current smoker")
  )
]

# depression
hunt_df[
  ,
  depression := cut(
    HADSDepr_NT4BLQ2, 
    breaks = c(0, 7, 21),
    right = FALSE,
    include.lowest = TRUE, 
    labels = c("No", "Yes")
  )
]

# MISSING COVARIATES ----
hunt_df[
  ,
  missing_cov := rowSums(is.na(.SD)), 
  .SDcols = c(
    "sex", 
    "age",
    "education",
    "income",
    "work_status",
    "smoking", 
    "depression"
  )
]

# RECODE PAIN VARIABLES ----
# chronic msk pain at baseline
hunt_df[, msp_t0 := MSPaLY_NT4BLQ2]

# chronic lbp at baseline
hunt_df[
  ,
  lbp_t0 := fcase(
    !is.na(msp_t0) & is.na(MSPaLum_NT4BLQ2), 0,
    MSPaLum_NT4BLQ2 == 1,        1
  )
]

# chronic msk pain at follow-up
hunt_df[, msp_t1 := MSPaLY_NT4CovQ]

# chronic lbp at follow-up
hunt_df[
  , 
  lbp_t1 := fcase(
    !is.na(msp_t1) & is.na(MSPaLum_NT4CovQ), 0,
    MSPaLum_NT4CovQ == 1,        1
  )
]

# pain intensity last 4 weeks
hunt_df[
  ,
  pain_l4w := fcase(
    MSPaChrL4W_NT4BLQ1 == 1, "No pain",
    MSPaChrL4W_NT4BLQ1 == 2, "Very mild",
    MSPaChrL4W_NT4BLQ1 == 3, "Mild",
    MSPaChrL4W_NT4BLQ1 == 4, "Moderate",
    MSPaChrL4W_NT4BLQ1 == 5, "Strong",
    MSPaChrL4W_NT4BLQ1 == 6, "Very strong"
  )
]

# OTHER VARIABLES ----
# follow-up time
hunt_df[
  , 
  ":=" (
    part_date_t0 = as.IDate(PartDat_NT4BLM, "%m/%d/%Y"),
    part_date_t1 = as.IDate(PartDat_NT4CovQ, "%m/%d/%Y")
  )
][, followup := part_date_t1 - part_date_t0]

# health status
hunt_df[
  ,
  health_status := fcase(
    Healt_NT4BLQ1 == 1, "Poor",
    Healt_NT4BLQ1 == 2, "Not so good",
    Healt_NT4BLQ1 == 3, "Good",
    Healt_NT4BLQ1 == 4, "Very good"
  )
]

# history of diabetes/cancer/cvd
disease_list <- c(
  "DiaEv_NT4BLQ1",
  "CaEv_NT4BLQ1",
  "CarAngEv_NT4BLQ1",
  "CarInfEv_NT4BLQ1",
  "CarFaiEv_NT4BLQ1",
  "CarAtrFibrEv_NT4BLQ1",
  "ApoplEv_NT4BLQ1"
)
hunt_df[
  ,
  chronic_disease := fcase(
  rowSums(.SD == 1) >= 1, 1,
  default = 0
  ),
  .SDcols = disease_list
]
hunt_df[
  ,
  valid_chr_dis := fcase(
    rowSums(is.na(.SD)) == 0, 1,
    default = 0
  ),
  .SDcols = disease_list
]

# hospitalization last 12 months
hunt_df[, hosp_12m := HospLY_NT4BLQ1]