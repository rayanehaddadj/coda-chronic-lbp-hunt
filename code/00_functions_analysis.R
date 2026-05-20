# LOAD LIBRARIES ----
library(data.table)
library(compositions)
library(sandwich)
library(gt)
library(plyr)
library(ggplot2)
library(showtext)

# ILR CREATION ----
create.ilr <- function(composition){
  sbp.matrix <- function(n){
    mtx <- matrix(data = 0, nrow = n - 1, ncol = n)
    for (i in 1:(n - 1)){
      mtx[i, i] <- 1
      mtx[i, (i + 1):n] <- -1
    }
    return(mtx)
  }
  sbp <- sbp.matrix(n = length(composition))
  psi <- gsi.buildilrBase(t(sbp))
  ilr <- ilr(composition, V = psi)
  return(ilr)
}

# POISSON REGRESSION ----
poisson.ilr <- function(data, outcome, exposure, covariates, output_dir, model_name, 
                        return_output = FALSE){
  if (!is.null(model_name)){
    output_path <- file.path("outputs", "models", output_dir, model_name)
  } else {
    output_path <- file.path("outputs", "models", output_dir)
  }
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  predictors <- paste0(c(exposure, covariates), collapse = "+")
  formula <- as.formula(paste0(c(outcome, predictors), collapse = "~"))
  model <- glm(formula, family = poisson(link = "log"), data = data)
  saveRDS(model, file.path(output_path, "model.RDS"))
  coeff_df <- data.table(
    ilr = exposure,
    RR = exp(coef(model)[exposure]),
    SE = sqrt(diag(vcovHC(model, type = "HC0")[exposure, exposure]))
  )
  coeff_df[
    , 
    ":=" (
      LL = exp(log(RR) - qnorm(0.975) * SE),
      UL = exp(log(RR) + qnorm(0.975) * SE)
    )
  ][sign(LL - 1) == sign(UL - 1), Significant := "Yes"]
  coeff_df[, Estimates := sprintf("%.2f (%.2f-%.2f)", RR, LL, UL)]
  if (file.exists(file.path(output_path, "regression_coefficients.docx"))){
    file.remove(file.path(output_path, "regression_coefficients.docx"))
  }
  coeff_df |>
    gt(rowname_col = "ilr") |>
    gtsave(file.path(output_path, "regression_coefficients.docx"))
  cat(
    "\n",
    "Nb of participants:",         data[, .N],                  "\n\n",
    "Nb of cases:",                data[get(outcome) == 1, .N], "\n\n",
    "Exposure:",                   exposure,                    "\n\n",
    "Covariates:",                 covariates,                  "\n\n",
    "Outcome:",                    outcome,                     "\n\n",
    "Formula:",                    deparse(formula),            "\n\n",
    file = file.path(output_path, "model_log.txt")
  )
  if (return_output) return(model)
}

# PAIRWISE REALLOCATION ----
pairwise.reallocation <- function(data, model, composition, reference, behaviour1, 
                                  behaviour2, time, output_dir, file_name,
                                  estimates_table = FALSE){
  output_path <- file.path("outputs", "estimates", output_dir)
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  if (reference == "compositional_mean") {
    ref <- data.table(t(geometricmeanCol(acomp(data[, ..composition])))) * 1440
  } else {
    ref <- reference
  }
  ref_ilr <- c(create.ilr(ref))
  time1 <- min(time, ref[, get(behaviour1)])
  time2 <- min(time, ref[, get(behaviour2)])
  step <- 0.1
  reallocation_df <- data.table()
  for (i in composition) {
    if (i == behaviour1){
      reallocation_df[, (i) := seq(ref[, get(i)] - time1, ref[, get(i)] + time2, step)]
    } else if (i == behaviour2){
      reallocation_df[, (i) := seq(ref[, get(i)] + time1, ref[, get(i)] - time2, -step)]
    } else {
      reallocation_df[, (i) := rep(ref[, get(i)], (time1 + time2) / step + 1)]
    }
  }
  reallocation_df[
    , 
    ":=" (
      behaviour1 = (behaviour1),
      behaviour2 = (behaviour2),
      time = get(behaviour1) - ref[, get(behaviour1)]
    )
  ]
  ilr <- paste0("ilr_", 1:(length(composition) - 1))
  reallocation_df[, (ilr) := data.table(create.ilr(.SD)), .SDcols = composition]
  reallocation_df[, (ilr) := sweep(.SD, 2, ref_ilr, "-"), .SDcols = ilr]
  vcov_matrix <- vcovHC(model, type = "HC0")[ilr, ilr]
  ilr_matrix <- data.matrix(reallocation_df[, ..ilr])
  reallocation_df[
    ,
    ":=" (
      log_risk = rowSums(sapply(ilr, function(x) coef(model)[[x]] * get(x))),
      se = sqrt(diag(ilr_matrix %*% vcov_matrix %*% t(ilr_matrix)))
    )
  ][
    ,
    ":=" (
      rr = exp(log_risk),
      ll = exp(log_risk - qnorm(0.975) * se),
      ul = exp(log_risk + qnorm(0.975) * se)
    )
  ]
  if (estimates_table) {
    table_df <- reallocation_df[time %in% c(min(time), max(time)), .(time, rr, ll, ul)]
    behaviour1_str <- sub("_lrem", "", behaviour1)
    behaviour2_str <- sub("_lrem", "", behaviour2)
    suppressWarnings(table_df[
      , 
      ":=" (
        rr_str = round(rr, 2),
        ll_str = round(ll, 2),
        ul_str = round(ul, 2),
        evalue = fcase(
          ll > 1 & ul > 1, rr + sqrt(rr * (rr - 1)),
          ll < 1 & ul < 1, 1/rr + sqrt((1/rr) * ((1/rr) - 1))
        ),
        evalue_ci = fcase(
          ll > 1 & ul > 1, ll + sqrt(ll * (ll - 1)),
          ll < 1 & ul < 1, 1/ul + sqrt((1/ul) * ((1/ul) - 1))
        ),
        behaviour1 = behaviour1_str,
        behaviour2 = behaviour2_str
      )
    ])
    table_dir <- file.path(output_path, "estimates_tables")
    if (!dir.exists(table_dir)) dir.create(table_dir, recursive = TRUE)
    table_name <- paste0(sprintf("%02d", file_name), "_", behaviour1_str, "_", behaviour2_str, ".docx")
    table_path <- file.path(table_dir, table_name)
    if (file.exists(table_path)) file.remove(table_path)
    table_df |> gt() |> gtsave(filename = table_path)
  }
  saveRDS(reallocation_df, file.path(output_path, paste0(sprintf("%02d", file_name), ".RDS")))
}

# PLOT LAYOUT ----
create.layout <- function(n) {
  layout <- matrix(NA, nrow = n, ncol = n)
  num <- 1
  for (col in 1:n) {
    for (row in col:n) {
      layout[row, col] <- num
      num <- num + 1
    }
  }
  return(layout)
}

# PLOT PAIRWISE REALLOCATION ----
plot.reallocation <- function(data, dictionary, y_title = "Adjusted risk ratio"){
  font_add("Arial", regular = "arial.ttf")
  showtext_auto()
  
  # y-axis values
  if (data[, max(ul)] >= 2) {
    y_max <- 2
  }
  else if (data[, max(ul)] > 1.5 & data[, max(ul)] < 2) {
    y_max <- round_any(data[, max(ul)], 0.2, ceiling)
  } 
  else {
    y_max <- 1.5
  }
  y_min <- ifelse(data[, min(ll)] < 0.7, round_any(data[, min(ll)], 0.1, floor), 0.7)
  y_step <- ifelse(y_max < 1.6, 0.1, 0.2)
  
  # arrow
  make.arrow <- function(x0, x1){
    y <- y_min * (y_max / y_min) ^ 0.9
    annotate(
      geom = "segment",
      x = x0, 
      y = y, 
      xend = x1,
      arrow = arrow(length = unit(0.25, "cm"), type = "closed")
    )
  }
  
  # arrow label
  arrow.label <- function(x, behaviour1, behaviour2){
    y1 <- y_min * (y_max / y_min) ^ 0.8375
    y2 <- y_min * (y_max / y_min) ^ 0.775
    annotate(
      geom = "text",
      x = x,
      y = c(y1, y2),
      label = c(paste("Replacing", behaviour1), paste("with", behaviour2)),
      size = 2.5,
      family = "Arial"
    )
  }
  
  # labels
  behaviour1 <- dictionary[[data[1, behaviour1]]]
  behaviour2 <- dictionary[[data[1, behaviour2]]]
  
  # plot data
  plot <- ggplot(data) +
    aes(
      x = time, 
      y = rr
    ) +
    geom_hline(yintercept = 1, linewidth = 0.33) +
    geom_vline(xintercept = 0, linetype = 2, linewidth = 0.33) +
    geom_ribbon(
      aes(ymin = ll, ymax = ul), 
      fill = "skyblue4",
      alpha = 0.33,
      linetype = "blank"
    ) +
    geom_line(color = "skyblue4", linewidth = 0.33) +
    make.arrow(x0 = -22.5, x1 = -37.5) +
    make.arrow(x0 = 22.5, x1 = 37.5) +
    arrow.label(-30, tolower(behaviour1), tolower(behaviour2)) +
    arrow.label(30, tolower(behaviour2), tolower(behaviour1)) +
    scale_x_continuous(
      name = ifelse(
        which(partition == behaviour2) == length(partition),
        paste(behaviour1, "(min/day)"),
        ""
      ),
      breaks = seq(-60, 60, by = 15),
      expand = c(0, 0)
    ) +
    scale_y_log10(
      name = ifelse(which(partition == behaviour1) == 1, y_title, ""),
      breaks = c(seq(0, 0.9, 0.1), seq(1, y_max, y_step)),
      labels = function(x) ifelse(x == 1, "1", x),
      expand = c(0, 0)
    ) +
    coord_cartesian(xlim = c(-60, 60), ylim = c(y_min, y_max)) +
    theme(
      text = element_text(family = "Arial"),
      axis.line = element_line(linewidth = 0.33),
      axis.ticks = element_line(color = "black", linewidth = 0.33),
      axis.title = element_text(size = 9, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 7.5)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(color = "black", size = 8),
      panel.background = element_blank()
    )
  return(plot)
}