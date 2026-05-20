# title: 01c_inclusion_criteria.R
# author: Rayane Haddadj
# year: 2026

hunt_n <- 56042
hunt_acc <- 31295
excl <- hunt_n - hunt_acc
hunt_val <- nrow(hunt_df[n_valid_d > 0])

hunt_df[
  ,
  valid_cov := fcase(
    missing_cov == 0, 1,
    default = 0
  )
]

hunt_df[
  ,
  valid_acc := fcase(
    n_valid_d > 0, 1,
    default = 0
  )
]

hunt_df[
  ,
  valid_t0 := fcase(
    lbp_t0 == 0, 1,
    default = 0
  )
]
hunt_df[
  ,
  valid_t1 := fcase(
    !is.na(msp_t1), 1,
    default = 0
  )
]
condition <- c("valid_cov", "valid_acc", "valid_t0", "valid_t1")
hunt_df[
  , 
  analysis := fcase(
    rowSums(.SD == 1) == length(condition), 1,
    default = 0
  ),
  .SDcols = condition
]