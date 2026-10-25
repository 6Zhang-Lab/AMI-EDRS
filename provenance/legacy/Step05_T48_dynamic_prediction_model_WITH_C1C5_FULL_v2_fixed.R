# HISTORICAL PROVENANCE ONLY. Not the current AMI-EDRS model. See docs/REPRODUCIBILITY.md.
############################################################
## Step05: T48 dynamic prediction model integrating C1-C5 trajectories
## Project: MIMIC_AMI_DynamicRisk_Project
##
## Main idea:
## Strict AMI cohort
##   -> T0/T24/T48 landmark organ-domain features
##   -> C1-C5 trajectory phenotype from Step03-final
##   -> Dynamic prediction of 30-day mortality after T48
##   -> AMI-EDRS risk score / risk groups / SHAP interpretation
##
## This version intentionally fuses Step03/Step04 trajectory phenotype
## into Step05 prediction models, so the paper has one coherent chain:
## trajectory discovery -> clinical meaning -> predictive utility.
##
## Primary target:
## death_30d_after_landmark at T48
##
## Main models:
## 1. GLM: host vulnerability
## 2. GLM: conventional clinical severity
## 3. GLM: organ-domain static T0
## 4. GLM: organ-domain dynamic T0/T24/T48
## 5. GLM: trajectory-enhanced dynamic model
## 6. Elastic net: dynamic + C1-C5 trajectory
## 7. Random forest: dynamic + C1-C5 trajectory
## 8. XGBoost: dynamic + C1-C5 trajectory
##
## Required upstream files:
## Preferred:
## <AMI_MIMIC_PROJECT>/
##   08_step04_v2_trajectory_clinical_meaning/
##     analysis_datasets/Step04_v2_T48_dataset_with_k5_trajectory_and_modules.csv
##
## Fallback:
##   03_analysis_dataset/Step02_AMI_T48_dataset.csv
##   07_step03_final_k5_trajectory_phenotyping/trajectory_datasets/
##     Step03_final_k5_T48_trajectory_labels.csv
##     Step03_final_k5_organ_domain_scores_with_trajectory.csv
##
## Output:
## <AMI_MIMIC_PROJECT>/
##   09_step05_T48_dynamic_prediction_model_with_C1C5/
############################################################

rm(list = ls())
gc()

options(stringsAsFactors = FALSE)
options(warn = 1)

## =========================================================
## 0. Packages
## =========================================================

pkg_needed <- c(
  "dplyr", "tidyr", "stringr", "purrr", "tibble", "data.table",
  "ggplot2", "scales", "openxlsx", "pROC", "PRROC",
  "glmnet", "ranger", "xgboost", "Matrix", "broom", "forcats"
)

for (p in pkg_needed) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

invisible(lapply(pkg_needed, library, character.only = TRUE))

## =========================================================
## 1. Paths
## =========================================================

project_root <- Sys.getenv("AMI_MIMIC_PROJECT", unset=file.path(getwd(), "work/MIMIC"))

## Preferred Step04-v2 analysis dataset
input_step04_v2 <- file.path(
  project_root,
  "08_step04_v2_trajectory_clinical_meaning",
  "analysis_datasets",
  "Step04_v2_T48_dataset_with_k5_trajectory_and_modules.csv"
)

## Fallback files
input_step02_t48 <- file.path(
  project_root,
  "03_analysis_dataset",
  "Step02_AMI_T48_dataset.csv"
)

input_step03_labels <- file.path(
  project_root,
  "07_step03_final_k5_trajectory_phenotyping",
  "trajectory_datasets",
  "Step03_final_k5_T48_trajectory_labels.csv"
)

input_step03_modules <- file.path(
  project_root,
  "07_step03_final_k5_trajectory_phenotyping",
  "trajectory_datasets",
  "Step03_final_k5_organ_domain_scores_with_trajectory.csv"
)

dir_step05 <- file.path(project_root, "09_step05_T48_dynamic_prediction_model_with_C1C5")
dir_fig <- file.path(dir_step05, "figures")
dir_tbl <- file.path(dir_step05, "tables")
dir_data <- file.path(dir_step05, "analysis_datasets")
dir_obj <- file.path(dir_step05, "objects")
dir_log <- file.path(dir_step05, "logs")

dir.create(dir_step05, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tbl, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_data, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_obj, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_log, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(dir_log, paste0("Step05_T48_dynamic_prediction_with_C1C5_", timestamp, ".log"))

out_xlsx <- file.path(dir_tbl, "Step05_T48_dynamic_prediction_with_C1C5_summary.xlsx")
out_predictions <- file.path(dir_data, "Step05_T48_model_predictions_train_test.csv")
out_score_test <- file.path(dir_data, "Step05_T48_AMI_EDRS_scores_test_set.csv")
out_model_objects <- file.path(dir_obj, "Step05_T48_model_objects_with_C1C5.rds")

## =========================================================
## 2. Colors and theme
## =========================================================

pal_blue <- "#4A7494"
pal_gold <- "#F2B85B"
pal_gray <- "#E3E8EC"
pal_red <- "#DA1F2E"
pal_dark <- "#1F2933"
pal_purple <- "#7A5D98"
pal_green <- "#13974A"
pal_orange <- "#EC7725"
pal_pink <- "#CE72A4"

pal_models <- c(
  "Host vulnerability" = pal_blue,
  "Conventional clinical severity" = "#89B3D0",
  "Static T0 organ-domain severity" = "#9D80BC",
  "Organ-domain dynamic model" = pal_gold,
  "Trajectory-enhanced dynamic model" = pal_orange,
  "Elastic net: dynamic + trajectory" = pal_green,
  "Random forest: dynamic + trajectory" = pal_purple,
  "XGBoost: dynamic + trajectory" = pal_red
)

pal_risk <- c(
  "Low risk (<5%)" = "#4A7494",
  "Intermediate risk (5-15%)" = "#89B3D0",
  "High risk (15-30%)" = "#F2B85B",
  "Very high risk (>=30%)" = "#DA1F2E"
)

theme_ami <- function(base_size = 16) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = pal_gray, linewidth = 0.55),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold", color = "black", size = base_size + 1),
      axis.text = element_text(color = "black", size = base_size - 1),
      legend.title = element_text(face = "bold", size = base_size),
      legend.text = element_text(size = base_size - 1),
      strip.background = element_rect(fill = pal_gray, color = "grey70", linewidth = 0.35),
      strip.text = element_text(face = "bold", size = base_size - 1, color = "black"),
      plot.title = element_text(face = "bold", size = base_size + 5, color = "black", hjust = 0),
      plot.subtitle = element_text(size = base_size + 1, color = "grey30", hjust = 0),
      plot.caption = element_text(size = base_size - 3, color = "grey40"),
      axis.line = element_line(color = "black", linewidth = 0.35),
      plot.margin = margin(12, 18, 12, 18)
    )
}

save_plot <- function(plot_obj, filename_stub, width = 10, height = 6.5) {
  ggplot2::ggsave(
    filename = file.path(dir_fig, paste0(filename_stub, ".pdf")),
    plot = plot_obj,
    width = width,
    height = height,
    device = cairo_pdf
  )
  ggplot2::ggsave(
    filename = file.path(dir_fig, paste0(filename_stub, ".png")),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = 600
  )
}

## =========================================================
## 3. Logging
## =========================================================

sink(log_file, split = TRUE)

cat("\n============================================================\n")
cat("Step05: T48 dynamic prediction model integrating C1-C5 trajectories\n")
cat("Time:", as.character(Sys.time()), "\n")
cat("Project root:", project_root, "\n")
cat("Output folder:", dir_step05, "\n")
cat("============================================================\n\n")

## =========================================================
## 4. Helper functions
## =========================================================

safe_fwrite <- function(x, path) {
  data.table::fwrite(x, path)
  cat("Saved:", path, "\n")
}

add_sheet <- function(wb, sheet_name, df) {
  sheet_name <- substr(sheet_name, 1, 31)
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeDataTable(
    wb,
    sheet = sheet_name,
    x = df,
    tableStyle = "TableStyleMedium2",
    withFilter = TRUE
  )
  openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:ncol(df), widths = "auto")
}

as_binary01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x)) return(ifelse(is.na(x), NA_integer_, as.integer(x != 0)))
  xx <- toupper(as.character(x))
  dplyr::case_when(
    is.na(xx) | xx == "" ~ NA_integer_,
    xx %in% c("1", "YES", "Y", "TRUE", "T") ~ 1L,
    xx %in% c("0", "NO", "N", "FALSE", "F") ~ 0L,
    TRUE ~ NA_integer_
  )
}

to_numeric_safe <- function(x) {
  suppressWarnings(as.numeric(x))
}

select_existing <- function(df, vars) {
  unique(vars[vars %in% names(df)])
}

clean_name <- function(x) {
  x <- make.names(x, unique = TRUE)
  x <- stringr::str_replace_all(x, "\\.+", "_")
  x <- stringr::str_replace_all(x, "_$", "")
  x
}

pretty_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

format_metric <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f"), x))
}

safe_auc <- function(y, pred) {
  y <- as.integer(y)
  pred <- as.numeric(pred)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]
  pred <- pred[ok]
  if (length(unique(y)) < 2) return(NA_real_)
  tryCatch(
    as.numeric(pROC::auc(pROC::roc(y, pred, quiet = TRUE, direction = "<"))),
    error = function(e) NA_real_
  )
}

safe_auprc <- function(y, pred) {
  y <- as.integer(y)
  pred <- as.numeric(pred)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]
  pred <- pred[ok]
  if (length(unique(y)) < 2) return(NA_real_)

  pos <- pred[y == 1]
  neg <- pred[y == 0]
  if (length(pos) == 0 || length(neg) == 0) return(NA_real_)

  tryCatch(
    PRROC::pr.curve(scores.class0 = pos, scores.class1 = neg, curve = FALSE)$auc.integral,
    error = function(e) NA_real_
  )
}

safe_brier <- function(y, pred) {
  y <- as.integer(y)
  pred <- pmin(pmax(as.numeric(pred), 1e-6), 1 - 1e-6)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  mean((y[ok] - pred[ok])^2)
}

calibration_intercept_slope <- function(y, pred) {
  y <- as.integer(y)
  pred <- pmin(pmax(as.numeric(pred), 1e-6), 1 - 1e-6)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]
  pred <- pred[ok]
  if (length(unique(y)) < 2) {
    return(tibble::tibble(calibration_intercept = NA_real_, calibration_slope = NA_real_))
  }
  lp <- qlogis(pred)
  fit <- tryCatch(glm(y ~ lp, family = binomial()), error = function(e) NULL)
  if (is.null(fit)) {
    return(tibble::tibble(calibration_intercept = NA_real_, calibration_slope = NA_real_))
  }
  tibble::tibble(
    calibration_intercept = unname(coef(fit)[1]),
    calibration_slope = unname(coef(fit)[2])
  )
}

expected_calibration_error <- function(y, pred, n_bins = 10) {
  y <- as.integer(y)
  pred <- pmin(pmax(as.numeric(pred), 1e-6), 1 - 1e-6)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]
  pred <- pred[ok]
  if (length(y) < n_bins) return(NA_real_)

  q <- unique(stats::quantile(pred, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE))
  if (length(q) < 3) {
    br <- cut(pred, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)
  } else {
    br <- cut(pred, breaks = q, include.lowest = TRUE)
  }

  df <- tibble::tibble(y = y, pred = pred, bin = br) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      n = dplyr::n(),
      obs = mean(y),
      exp = mean(pred),
      .groups = "drop"
    )

  sum(df$n / sum(df$n) * abs(df$obs - df$exp))
}

best_youden_threshold <- function(y, pred) {
  y <- as.integer(y)
  pred <- as.numeric(pred)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]
  pred <- pred[ok]
  if (length(unique(y)) < 2) return(0.5)

  roc_obj <- tryCatch(pROC::roc(y, pred, quiet = TRUE, direction = "<"), error = function(e) NULL)
  if (is.null(roc_obj)) return(0.5)

  thr <- tryCatch(
    as.numeric(pROC::coords(roc_obj, "best", ret = "threshold", best.method = "youden")[[1]]),
    error = function(e) 0.5
  )
  ifelse(is.na(thr) || !is.finite(thr), 0.5, thr)
}

classification_metrics <- function(y, pred, threshold = NULL) {
  y <- as.integer(y)
  pred <- pmin(pmax(as.numeric(pred), 1e-6), 1 - 1e-6)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]
  pred <- pred[ok]
  if (is.null(threshold)) threshold <- best_youden_threshold(y, pred)

  cls <- as.integer(pred >= threshold)
  TP <- sum(cls == 1 & y == 1)
  TN <- sum(cls == 0 & y == 0)
  FP <- sum(cls == 1 & y == 0)
  FN <- sum(cls == 0 & y == 1)

  tibble::tibble(
    threshold = threshold,
    sensitivity = ifelse(TP + FN > 0, TP / (TP + FN), NA_real_),
    specificity = ifelse(TN + FP > 0, TN / (TN + FP), NA_real_),
    PPV = ifelse(TP + FP > 0, TP / (TP + FP), NA_real_),
    NPV = ifelse(TN + FN > 0, TN / (TN + FN), NA_real_),
    accuracy = (TP + TN) / (TP + TN + FP + FN)
  )
}

evaluate_predictions <- function(y, pred, model_name, dataset_name, threshold = NULL) {
  calib <- calibration_intercept_slope(y, pred)
  class_m <- classification_metrics(y, pred, threshold = threshold)

  tibble::tibble(
    model = model_name,
    dataset = dataset_name,
    n = sum(!is.na(y) & !is.na(pred)),
    events = sum(y == 1, na.rm = TRUE),
    event_rate = mean(y == 1, na.rm = TRUE),
    AUROC = safe_auc(y, pred),
    AUPRC = safe_auprc(y, pred),
    Brier = safe_brier(y, pred),
    ECE_10 = expected_calibration_error(y, pred, n_bins = 10)
  ) %>%
    dplyr::bind_cols(calib) %>%
    dplyr::bind_cols(class_m)
}

make_roc_df <- function(y, pred, model_name) {
  y <- as.integer(y)
  pred <- as.numeric(pred)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]
  pred <- pred[ok]
  if (length(unique(y)) < 2) return(tibble())

  roc_obj <- tryCatch(pROC::roc(y, pred, quiet = TRUE, direction = "<"), error = function(e) NULL)
  if (is.null(roc_obj)) return(tibble())

  tibble::tibble(
    specificity = as.numeric(roc_obj$specificities),
    sensitivity = as.numeric(roc_obj$sensitivities),
    fpr = 1 - specificity,
    model = model_name,
    AUROC = as.numeric(pROC::auc(roc_obj))
  )
}

make_pr_df <- function(y, pred, model_name) {
  y <- as.integer(y)
  pred <- as.numeric(pred)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]
  pred <- pred[ok]
  if (length(unique(y)) < 2) return(tibble())

  ord <- order(pred, decreasing = TRUE)
  y_ord <- y[ord]
  pred_ord <- pred[ord]

  tp <- cumsum(y_ord == 1)
  fp <- cumsum(y_ord == 0)
  precision <- tp / pmax(tp + fp, 1)
  recall <- tp / sum(y_ord == 1)

  tibble::tibble(
    recall = recall,
    precision = precision,
    threshold = pred_ord,
    model = model_name,
    AUPRC = safe_auprc(y, pred)
  )
}

make_calibration_df <- function(y, pred, model_name, n_bins = 10) {
  y <- as.integer(y)
  pred <- pmin(pmax(as.numeric(pred), 1e-6), 1 - 1e-6)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]
  pred <- pred[ok]

  if (length(y) < n_bins) return(tibble())

  q <- unique(stats::quantile(pred, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE))
  if (length(q) < 3) {
    bin <- cut(pred, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)
  } else {
    bin <- cut(pred, breaks = q, include.lowest = TRUE)
  }

  tibble::tibble(y = y, pred = pred, bin = bin) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      n = dplyr::n(),
      observed = mean(y),
      predicted = mean(pred),
      lower = pmax(observed - 1.96 * sqrt(observed * (1 - observed) / n), 0),
      upper = pmin(observed + 1.96 * sqrt(observed * (1 - observed) / n), 1),
      .groups = "drop"
    ) %>%
    dplyr::mutate(model = model_name)
}

decision_curve_df <- function(y, pred, model_name, thresholds = seq(0.01, 0.50, by = 0.01)) {
  y <- as.integer(y)
  pred <- pmin(pmax(as.numeric(pred), 1e-6), 1 - 1e-6)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]
  pred <- pred[ok]
  n <- length(y)
  prevalence <- mean(y == 1)

  purrr::map_dfr(thresholds, function(pt) {
    treat <- pred >= pt
    TP <- sum(treat & y == 1)
    FP <- sum(treat & y == 0)
    nb <- TP / n - FP / n * pt / (1 - pt)
    nb_all <- prevalence - (1 - prevalence) * pt / (1 - pt)
    tibble::tibble(
      model = model_name,
      threshold = pt,
      net_benefit = nb,
      net_benefit_all = nb_all,
      net_benefit_none = 0
    )
  })
}

## =========================================================
## 5. Load analysis dataset and rebuild fallback if needed
## =========================================================

if (file.exists(input_step04_v2)) {
  cat("Using Step04-v2 analysis dataset:\n", input_step04_v2, "\n\n")
  df_raw <- data.table::fread(input_step04_v2) %>% as.data.frame()
} else {
  cat("Step04-v2 analysis dataset not found. Rebuilding from Step02 + Step03 files.\n\n")

  required_fallback <- c(input_step02_t48, input_step03_labels, input_step03_modules)
  missing_fallback <- required_fallback[!file.exists(required_fallback)]
  if (length(missing_fallback) > 0) {
    stop("Missing required fallback input files:\n", paste(missing_fallback, collapse = "\n"))
  }

  t48 <- data.table::fread(input_step02_t48) %>% as.data.frame()
  labels <- data.table::fread(input_step03_labels) %>% as.data.frame()
  modules <- data.table::fread(input_step03_modules) %>% as.data.frame()

  module_wide <- modules %>%
    dplyr::filter(landmark %in% c("T0", "T24", "T48")) %>%
    dplyr::mutate(name = paste0(module, "_", landmark, "_score")) %>%
    dplyr::select(stay_id, name, module_score) %>%
    tidyr::pivot_wider(id_cols = stay_id, names_from = name, values_from = module_score)

  df_raw <- t48 %>%
    dplyr::left_join(
      labels %>% dplyr::select(stay_id, trajectory, dplyr::any_of(c("death_30d_after_landmark", "days_from_landmark_to_death"))),
      by = "stay_id"
    ) %>%
    dplyr::left_join(module_wide, by = "stay_id")
}

df <- df_raw %>% as.data.frame()

if (!"stay_id" %in% names(df)) stop("stay_id is missing.")
if (!"trajectory" %in% names(df)) stop("trajectory is missing. Please run Step03-final k=5 first.")

primary_outcome <- "death_30d_after_landmark"

if (!primary_outcome %in% names(df)) {
  stop("Primary outcome death_30d_after_landmark is missing.")
}

trajectory_levels <- paste0("C", 1:5)

df <- df %>%
  dplyr::mutate(
    trajectory = factor(as.character(trajectory), levels = trajectory_levels),
    death_30d_after_landmark = as_binary01(.data[[primary_outcome]]),
    trajectory_ord = as.integer(trajectory)
  ) %>%
  dplyr::filter(!is.na(trajectory), !is.na(death_30d_after_landmark))

## Add common dynamic features if missing
module_bases <- c(
  "hemodynamic",
  "respiratory_neurologic",
  "renal_metabolic",
  "inflammatory_coagulopathic",
  "cardiac_hypoperfusion"
)

for (m in module_bases) {
  c0 <- paste0(m, "_T0_score")
  c24 <- paste0(m, "_T24_score")
  c48 <- paste0(m, "_T48_score")

  if (all(c(c0, c24, c48) %in% names(df))) {
    if (!paste0(m, "_delta_T48_T0") %in% names(df)) {
      df[[paste0(m, "_delta_T48_T0")]] <- df[[c48]] - df[[c0]]
    }
    if (!paste0(m, "_delta_T48_T24") %in% names(df)) {
      df[[paste0(m, "_delta_T48_T24")]] <- df[[c48]] - df[[c24]]
    }
    if (!paste0(m, "_slope_T0_T48") %in% names(df)) {
      df[[paste0(m, "_slope_T0_T48")]] <- (df[[c48]] - df[[c0]]) / 48
    }
  }
}

## Optional survival time, not used for model training, but saved for clinical interpretation
if (!"time_to_death_or_censor_days" %in% names(df)) {
  if ("days_from_landmark_to_death" %in% names(df)) {
    df$time_to_death_or_censor_days <- ifelse(
      df$death_30d_after_landmark == 1 & !is.na(df$days_from_landmark_to_death),
      pmax(as.numeric(df$days_from_landmark_to_death), 0.05),
      30
    )
  } else {
    df$time_to_death_or_censor_days <- 30
  }
}

cat("Analysis dataset:\n")
cat("Rows:", nrow(df), "\n")
cat("Events:", sum(df$death_30d_after_landmark == 1, na.rm = TRUE), "\n")
cat("Event rate:", sprintf("%.1f%%", 100 * mean(df$death_30d_after_landmark == 1, na.rm = TRUE)), "\n\n")

## Save assembled dataset
safe_fwrite(df, file.path(dir_data, "Step05_T48_analysis_dataset_with_C1C5.csv"))

## =========================================================
## 6. Candidate predictors
## =========================================================

## Demographics and host vulnerability
demographic_candidates <- c(
  "age", "gender", "sex", "race_collapsed", "race", "admission_type",
  "first_careunit", "insurance"
)

comorbidity_candidates <- c(
  "hypertension", "diabetes", "diabetes_without_cc", "diabetes_with_cc",
  "heart_failure", "congestive_heart_failure",
  "atrial_fibrillation", "ckd", "chronic_kidney_disease", "renal_disease",
  "copd", "chronic_pulmonary_disease",
  "stroke", "cerebrovascular_disease", "peripheral_vascular_disease",
  "liver_disease", "mild_liver_disease", "severe_liver_disease",
  "malignancy", "metastatic_solid_tumor",
  "charlson_comorbidity_index"
)

## Conventional clinical severity
conventional_candidates <- c(
  "age", "gender", "sex", "race_collapsed", "race",
  "charlson_comorbidity_index",
  "sofa", "apsiii", "sapsii", "oasis", "lods",
  "heart_rate_first_value", "heart_rate_mean_value", "heart_rate_last_value",
  "sbp_first_value", "sbp_mean_value", "sbp_last_value",
  "map_first_value", "map_mean_value", "map_last_value",
  "resp_rate_first_value", "resp_rate_mean_value", "resp_rate_last_value",
  "spo2_first_value", "spo2_mean_value", "spo2_last_value",
  "temperature_first_value", "temperature_mean_value", "temperature_last_value"
)

## T0 organ-domain static scores
t0_module_candidates <- paste0(module_bases, "_T0_score")

## Dynamic organ-domain features
dynamic_module_candidates <- c(
  unlist(lapply(module_bases, function(m) paste0(m, "_", c("T0", "T24", "T48"), "_score"))),
  unlist(lapply(module_bases, function(m) paste0(m, "_delta_T48_T0"))),
  unlist(lapply(module_bases, function(m) paste0(m, "_delta_T48_T24"))),
  unlist(lapply(module_bases, function(m) paste0(m, "_slope_T0_T48")))
)

## Lab/vital dynamic features from Step02 if available.
## These are candidate predictors, filtered later by availability and missingness.
dynamic_raw_patterns <- c(
  "_first_value$", "_last_value$", "_mean_value$", "_min_value$", "_max_value$",
  "_sd_value$", "_delta_value$", "_slope_per_hour$", "_abnormal_ratio$"
)

dynamic_raw_candidates <- names(df)[
  Reduce(`|`, lapply(dynamic_raw_patterns, function(pat) stringr::str_detect(names(df), pat)))
]

## Remove obvious IDs/time/outcome leakage
leakage_patterns <- c(
  "death", "mortality", "expire", "survival", "deathtime", "dod",
  "los", "dischtime", "outtime", "time_to", "days_from_landmark_to_death",
  "label", "split", "prediction", "risk_group"
)

is_leakage <- function(x) {
  any(stringr::str_detect(tolower(x), leakage_patterns))
}

dynamic_raw_candidates <- dynamic_raw_candidates[!purrr::map_lgl(dynamic_raw_candidates, is_leakage)]

host_predictors <- select_existing(df, unique(c(demographic_candidates, comorbidity_candidates)))
conventional_predictors <- select_existing(df, unique(c(conventional_candidates, comorbidity_candidates)))
t0_predictors <- select_existing(df, unique(c(host_predictors, t0_module_candidates)))
dynamic_predictors <- select_existing(df, unique(c(host_predictors, dynamic_module_candidates)))
dynamic_extended_predictors <- select_existing(df, unique(c(host_predictors, dynamic_module_candidates, dynamic_raw_candidates)))

## Trajectory variable is deliberately added only after dynamic features, to test added value.
trajectory_predictors <- c("trajectory")
trajectory_ord_predictors <- c("trajectory_ord")

model_predictor_sets <- list(
  "Host vulnerability" = host_predictors,
  "Conventional clinical severity" = conventional_predictors,
  "Static T0 organ-domain severity" = t0_predictors,
  "Organ-domain dynamic model" = dynamic_predictors,
  "Trajectory-enhanced dynamic model" = select_existing(df, unique(c(dynamic_predictors, trajectory_predictors))),
  "Elastic net: dynamic + trajectory" = select_existing(df, unique(c(dynamic_extended_predictors, trajectory_predictors))),
  "Random forest: dynamic + trajectory" = select_existing(df, unique(c(dynamic_extended_predictors, trajectory_predictors))),
  "XGBoost: dynamic + trajectory" = select_existing(df, unique(c(dynamic_extended_predictors, trajectory_predictors)))
)

## Filter high-missing predictors for model stability
missing_df <- purrr::map_dfr(names(df), function(v) {
  tibble::tibble(
    variable = v,
    missing_pct = mean(is.na(df[[v]])) * 100,
    n_nonmissing = sum(!is.na(df[[v]])),
    n_unique = dplyr::n_distinct(df[[v]], na.rm = TRUE)
  )
})

safe_fwrite(missing_df, file.path(dir_tbl, "Step05_variable_missingness_all.csv"))

max_missing_for_model <- 80

filter_predictors <- function(predictors, max_missing = max_missing_for_model) {
  predictors <- unique(predictors)
  predictors <- predictors[predictors %in% names(df)]
  predictors <- predictors[!purrr::map_lgl(predictors, is_leakage)]
  predictors <- predictors[predictors != primary_outcome]

  predictors[
    purrr::map_lgl(predictors, function(v) {
      miss <- mean(is.na(df[[v]])) * 100
      nunq <- dplyr::n_distinct(df[[v]], na.rm = TRUE)
      is.finite(miss) && miss <= max_missing && nunq >= 2
    })
  ]
}

model_predictor_sets <- purrr::map(model_predictor_sets, filter_predictors)

predictor_map <- purrr::imap_dfr(model_predictor_sets, function(vars, nm) {
  tibble::tibble(model = nm, predictor = vars)
})

safe_fwrite(predictor_map, file.path(dir_tbl, "Step05_model_predictor_map.csv"))

cat("Predictor counts:\n")
print(predictor_map %>% dplyr::count(model, name = "n_predictors"))

## =========================================================
## 7. Train/test split
## =========================================================

set.seed(20260601)

df_model <- df %>%
  dplyr::filter(!is.na(death_30d_after_landmark)) %>%
  dplyr::mutate(row_id_step05 = dplyr::row_number())

## Stratified 70/30 split by outcome
idx_event <- which(df_model$death_30d_after_landmark == 1)
idx_nonevent <- which(df_model$death_30d_after_landmark == 0)

train_event <- sample(idx_event, size = floor(0.70 * length(idx_event)))
train_nonevent <- sample(idx_nonevent, size = floor(0.70 * length(idx_nonevent)))

train_idx <- sort(c(train_event, train_nonevent))
test_idx <- setdiff(seq_len(nrow(df_model)), train_idx)

train_df <- df_model[train_idx, , drop = FALSE] %>% as.data.frame()
test_df <- df_model[test_idx, , drop = FALSE] %>% as.data.frame()

split_df <- dplyr::bind_rows(
  train_df %>% dplyr::transmute(stay_id, row_id_step05, split = "Train", death_30d_after_landmark),
  test_df %>% dplyr::transmute(stay_id, row_id_step05, split = "Test", death_30d_after_landmark)
)

safe_fwrite(split_df, file.path(dir_data, "Step05_train_test_split.csv"))

cat("\nTrain/test split:\n")
cat("Train n:", nrow(train_df), "events:", sum(train_df$death_30d_after_landmark == 1), "\n")
cat("Test n:", nrow(test_df), "events:", sum(test_df$death_30d_after_landmark == 1), "\n\n")

## =========================================================
## 8. Preprocessor
## =========================================================

make_preprocessor <- function(train_data, predictors, outcome) {
  predictors <- unique(predictors[predictors %in% names(train_data)])
  predictors <- predictors[predictors != outcome]

  x_train <- train_data[, predictors, drop = FALSE] %>% as.data.frame()

  ## Identify numeric vs categorical
  is_num <- purrr::map_lgl(x_train, function(x) {
    is.numeric(x) || is.integer(x)
  })

  numeric_vars <- names(x_train)[is_num]
  categorical_vars <- names(x_train)[!is_num]

  ## Convert categorical to factor and preserve levels.
  ## Important fix:
  ## Always keep "Missing" as an allowed level. Otherwise, if the test set
  ## contains missing values while the training set has none, model.matrix()
  ## silently drops those rows and cbind(mm, miss_mat) fails.
  cat_levels <- list()
  for (v in categorical_vars) {
    x_train[[v]] <- as.character(x_train[[v]])
    x_train[[v]][is.na(x_train[[v]]) | x_train[[v]] == ""] <- "Missing"
    lv <- sort(unique(x_train[[v]]))
    lv <- unique(c(lv, "Missing"))
    x_train[[v]] <- factor(x_train[[v]], levels = lv)
    cat_levels[[v]] <- lv
  }

  ## Numeric medians and missing indicators
  num_medians <- list()
  num_missing_indicator_vars <- c()
  for (v in numeric_vars) {
    x <- to_numeric_safe(x_train[[v]])
    x[!is.finite(x)] <- NA_real_
    med <- suppressWarnings(stats::median(x, na.rm = TRUE))
    if (is.na(med) || !is.finite(med)) med <- 0
    num_medians[[v]] <- med

    miss_pct <- mean(is.na(x)) * 100
    if (miss_pct >= 5) {
      num_missing_indicator_vars <- c(num_missing_indicator_vars, v)
    }
  }

  ## Formula for model.matrix after imputation
  formula_terms <- predictors
  mm_formula <- stats::as.formula(paste("~", paste(formula_terms, collapse = " + "), "- 1"))

  prep <- list(
    predictors = predictors,
    numeric_vars = numeric_vars,
    categorical_vars = categorical_vars,
    num_medians = num_medians,
    num_missing_indicator_vars = num_missing_indicator_vars,
    cat_levels = cat_levels,
    mm_formula = mm_formula
  )

  class(prep) <- "ami_preprocessor"
  prep
}

apply_preprocessor <- function(prep, new_data) {
  x <- new_data[, prep$predictors, drop = FALSE] %>% as.data.frame()

  ## Numeric imputation and missing indicators
  for (v in prep$numeric_vars) {
    if (!v %in% names(x)) x[[v]] <- NA_real_
    xx <- to_numeric_safe(x[[v]])
    xx[!is.finite(xx)] <- NA_real_
    if (v %in% prep$num_missing_indicator_vars) {
      x[[paste0(v, "_missing")]] <- as.integer(is.na(xx))
    }
    xx[is.na(xx)] <- prep$num_medians[[v]]
    x[[v]] <- xx
  }

  ## Categorical handling
  for (v in prep$categorical_vars) {
    if (!v %in% names(x)) x[[v]] <- "Missing"
    xx <- as.character(x[[v]])
    xx[is.na(xx) | xx == ""] <- "Missing"
    known <- prep$cat_levels[[v]]
    if (!"Missing" %in% known) known <- unique(c(known, "Missing"))
    xx[!xx %in% known] <- "Missing"
    x[[v]] <- factor(xx, levels = known)
  }

  ## Keep order and include generated missing indicators
  extra_missing <- paste0(prep$num_missing_indicator_vars, "_missing")
  all_cols <- unique(c(prep$predictors, extra_missing))

  for (v in all_cols) {
    if (!v %in% names(x)) x[[v]] <- 0
  }

  ## Safety check: no NA should remain in formula predictors.
  ## If NA remains, replace categorical NA with Missing and numeric NA with median.
  for (v in prep$predictors) {
    if (!v %in% names(x)) next
    if (v %in% prep$numeric_vars) {
      xx <- to_numeric_safe(x[[v]])
      xx[!is.finite(xx) | is.na(xx)] <- prep$num_medians[[v]]
      x[[v]] <- xx
    } else if (v %in% prep$categorical_vars) {
      xx <- as.character(x[[v]])
      xx[is.na(xx) | xx == ""] <- "Missing"
      known <- prep$cat_levels[[v]]
      if (!"Missing" %in% known) known <- unique(c(known, "Missing"))
      xx[!xx %in% known] <- "Missing"
      x[[v]] <- factor(xx, levels = known)
    }
  }

  mm <- stats::model.matrix(prep$mm_formula, data = x, na.action = stats::na.pass)

  ## Add missing indicators to matrix
  if (length(extra_missing) > 0) {
    miss_mat <- as.matrix(x[, extra_missing, drop = FALSE])
    colnames(miss_mat) <- clean_name(colnames(miss_mat))

    if (nrow(mm) != nrow(miss_mat)) {
      stop(
        "Preprocessor row mismatch: model.matrix produced ", nrow(mm),
        " rows but missing-indicator matrix has ", nrow(miss_mat),
        " rows. This usually means some categorical NA remained before model.matrix."
      )
    }

    mm <- cbind(mm, miss_mat)
  }

  colnames(mm) <- clean_name(colnames(mm))
  mm <- mm[, colSums(is.na(mm)) == 0, drop = FALSE]
  storage.mode(mm) <- "numeric"

  if (nrow(mm) != nrow(new_data)) {
    stop(
      "Preprocessor row mismatch after model.matrix: expected ", nrow(new_data),
      " rows but got ", nrow(mm), "."
    )
  }

  list(
    matrix = mm,
    data = x
  )
}

align_matrices <- function(train_mat, test_mat) {
  all_cols <- union(colnames(train_mat), colnames(test_mat))

  add_missing_cols <- function(mat, all_cols) {
    missing_cols <- setdiff(all_cols, colnames(mat))
    if (length(missing_cols) > 0) {
      add <- matrix(0, nrow = nrow(mat), ncol = length(missing_cols))
      colnames(add) <- missing_cols
      mat <- cbind(mat, add)
    }
    mat[, all_cols, drop = FALSE]
  }

  list(
    train = add_missing_cols(train_mat, all_cols),
    test = add_missing_cols(test_mat, all_cols)
  )
}

fit_glm_model <- function(model_name, predictors) {
  predictors <- unique(predictors)
  if (length(predictors) == 0) return(NULL)

  cat("\nFitting GLM:", model_name, "\n")

  prep <- make_preprocessor(train_df, predictors, primary_outcome)
  tr <- apply_preprocessor(prep, train_df)
  te <- apply_preprocessor(prep, test_df)
  mats <- align_matrices(tr$matrix, te$matrix)

  train_mat <- mats$train
  test_mat <- mats$test

  glm_train_df <- as.data.frame(train_mat, check.names = FALSE)
  colnames(glm_train_df) <- clean_name(colnames(glm_train_df))
  glm_test_df <- as.data.frame(test_mat, check.names = FALSE)
  colnames(glm_test_df) <- clean_name(colnames(glm_test_df))

  ## Remove zero-variance columns
  nzv <- purrr::map_lgl(glm_train_df, ~ stats::sd(.x, na.rm = TRUE) > 0)
  glm_train_df <- glm_train_df[, nzv, drop = FALSE]
  glm_test_df <- glm_test_df[, colnames(glm_train_df), drop = FALSE]

  glm_train_df[[primary_outcome]] <- train_df[[primary_outcome]]

  if (ncol(glm_train_df) <= 1) return(NULL)

  f <- as.formula(paste(primary_outcome, "~", paste(setdiff(names(glm_train_df), primary_outcome), collapse = " + ")))

  fit <- tryCatch(
    stats::glm(f, data = glm_train_df, family = binomial()),
    error = function(e) {
      message("GLM failed for ", model_name, ": ", e$message)
      NULL
    }
  )

  if (is.null(fit)) return(NULL)

  p_train <- tryCatch(
    stats::predict(fit, newdata = glm_train_df, type = "response"),
    error = function(e) rep(mean(train_df[[primary_outcome]]), nrow(train_df))
  )

  p_test <- tryCatch(
    stats::predict(fit, newdata = glm_test_df, type = "response"),
    error = function(e) rep(mean(train_df[[primary_outcome]]), nrow(test_df))
  )

  p_train <- pmin(pmax(p_train, 1e-6), 1 - 1e-6)
  p_test <- pmin(pmax(p_test, 1e-6), 1 - 1e-6)

  list(
    object = fit,
    prep = prep,
    train_pred = p_train,
    test_pred = p_test,
    train_matrix = train_mat,
    test_matrix = test_mat
  )
}

## =========================================================
## 9. Fit models
## =========================================================

model_objects <- list()
prediction_list <- list()
performance_list <- list()

## GLM models
glm_model_names <- c(
  "Host vulnerability",
  "Conventional clinical severity",
  "Static T0 organ-domain severity",
  "Organ-domain dynamic model",
  "Trajectory-enhanced dynamic model"
)

for (mn in glm_model_names) {
  res <- fit_glm_model(mn, model_predictor_sets[[mn]])

  if (!is.null(res)) {
    model_objects[[mn]] <- res

    prediction_list[[paste0(mn, "_Train")]] <- tibble::tibble(
      model = mn,
      split = "Train",
      stay_id = train_df$stay_id,
      y = train_df[[primary_outcome]],
      pred = as.numeric(res$train_pred)
    )

    prediction_list[[paste0(mn, "_Test")]] <- tibble::tibble(
      model = mn,
      split = "Test",
      stay_id = test_df$stay_id,
      y = test_df[[primary_outcome]],
      pred = as.numeric(res$test_pred)
    )

    thr <- best_youden_threshold(train_df[[primary_outcome]], res$train_pred)

    performance_list[[paste0(mn, "_Train")]] <- evaluate_predictions(
      train_df[[primary_outcome]], res$train_pred, mn, "Train", threshold = thr
    )
    performance_list[[paste0(mn, "_Test")]] <- evaluate_predictions(
      test_df[[primary_outcome]], res$test_pred, mn, "Test", threshold = thr
    )
  }
}

## Elastic net
elastic_name <- "Elastic net: dynamic + trajectory"
cat("\nFitting Elastic Net:", elastic_name, "\n")

prep_enet <- make_preprocessor(train_df, model_predictor_sets[[elastic_name]], primary_outcome)
tr_enet <- apply_preprocessor(prep_enet, train_df)
te_enet <- apply_preprocessor(prep_enet, test_df)
mats_enet <- align_matrices(tr_enet$matrix, te_enet$matrix)

x_train_enet <- mats_enet$train
x_test_enet <- mats_enet$test
y_train <- train_df[[primary_outcome]]
y_test <- test_df[[primary_outcome]]

## Remove zero variance columns
nzv_enet <- apply(x_train_enet, 2, sd, na.rm = TRUE) > 0
x_train_enet <- x_train_enet[, nzv_enet, drop = FALSE]
x_test_enet <- x_test_enet[, colnames(x_train_enet), drop = FALSE]

enet_fit <- tryCatch({
  glmnet::cv.glmnet(
    x = x_train_enet,
    y = y_train,
    family = "binomial",
    alpha = 0.5,
    nfolds = 5,
    type.measure = "deviance",
    standardize = TRUE,
    maxit = 1e5
  )
}, error = function(e) {
  message("Elastic net failed: ", e$message)
  NULL
})

if (!is.null(enet_fit)) {
  pred_train_enet <- as.numeric(predict(enet_fit, newx = x_train_enet, s = "lambda.min", type = "response"))
  pred_test_enet <- as.numeric(predict(enet_fit, newx = x_test_enet, s = "lambda.min", type = "response"))
  pred_train_enet <- pmin(pmax(pred_train_enet, 1e-6), 1 - 1e-6)
  pred_test_enet <- pmin(pmax(pred_test_enet, 1e-6), 1 - 1e-6)

  model_objects[[elastic_name]] <- list(object = enet_fit, prep = prep_enet)

  prediction_list[[paste0(elastic_name, "_Train")]] <- tibble::tibble(
    model = elastic_name, split = "Train", stay_id = train_df$stay_id, y = y_train, pred = pred_train_enet
  )
  prediction_list[[paste0(elastic_name, "_Test")]] <- tibble::tibble(
    model = elastic_name, split = "Test", stay_id = test_df$stay_id, y = y_test, pred = pred_test_enet
  )

  thr <- best_youden_threshold(y_train, pred_train_enet)
  performance_list[[paste0(elastic_name, "_Train")]] <- evaluate_predictions(y_train, pred_train_enet, elastic_name, "Train", threshold = thr)
  performance_list[[paste0(elastic_name, "_Test")]] <- evaluate_predictions(y_test, pred_test_enet, elastic_name, "Test", threshold = thr)

  enet_coef <- as.matrix(coef(enet_fit, s = "lambda.min"))
  enet_importance <- tibble::tibble(
    feature = rownames(enet_coef),
    coefficient = as.numeric(enet_coef[, 1]),
    abs_coefficient = abs(coefficient)
  ) %>%
    dplyr::filter(feature != "(Intercept)", coefficient != 0) %>%
    dplyr::arrange(desc(abs_coefficient))

  safe_fwrite(enet_importance, file.path(dir_tbl, "Step05_elastic_net_nonzero_coefficients.csv"))
}

## Random forest
rf_name <- "Random forest: dynamic + trajectory"
cat("\nFitting Random Forest:", rf_name, "\n")

prep_rf <- make_preprocessor(train_df, model_predictor_sets[[rf_name]], primary_outcome)
tr_rf <- apply_preprocessor(prep_rf, train_df)
te_rf <- apply_preprocessor(prep_rf, test_df)
mats_rf <- align_matrices(tr_rf$matrix, te_rf$matrix)

x_train_rf <- mats_rf$train
x_test_rf <- mats_rf$test

nzv_rf <- apply(x_train_rf, 2, sd, na.rm = TRUE) > 0
x_train_rf <- x_train_rf[, nzv_rf, drop = FALSE]
x_test_rf <- x_test_rf[, colnames(x_train_rf), drop = FALSE]

rf_fit <- tryCatch({
  ranger::ranger(
    x = as.data.frame(x_train_rf, check.names = TRUE),
    y = factor(y_train, levels = c(0, 1)),
    probability = TRUE,
    num.trees = 800,
    mtry = max(2, floor(sqrt(ncol(x_train_rf)))),
    min.node.size = 25,
    importance = "permutation",
    seed = 20260601
  )
}, error = function(e) {
  message("Random forest failed: ", e$message)
  NULL
})

if (!is.null(rf_fit)) {
  pred_train_rf <- predict(rf_fit, data = as.data.frame(x_train_rf, check.names = TRUE))$predictions[, "1"]
  pred_test_rf <- predict(rf_fit, data = as.data.frame(x_test_rf, check.names = TRUE))$predictions[, "1"]
  pred_train_rf <- pmin(pmax(pred_train_rf, 1e-6), 1 - 1e-6)
  pred_test_rf <- pmin(pmax(pred_test_rf, 1e-6), 1 - 1e-6)

  model_objects[[rf_name]] <- list(object = rf_fit, prep = prep_rf)

  prediction_list[[paste0(rf_name, "_Train")]] <- tibble::tibble(
    model = rf_name, split = "Train", stay_id = train_df$stay_id, y = y_train, pred = pred_train_rf
  )
  prediction_list[[paste0(rf_name, "_Test")]] <- tibble::tibble(
    model = rf_name, split = "Test", stay_id = test_df$stay_id, y = y_test, pred = pred_test_rf
  )

  thr <- best_youden_threshold(y_train, pred_train_rf)
  performance_list[[paste0(rf_name, "_Train")]] <- evaluate_predictions(y_train, pred_train_rf, rf_name, "Train", threshold = thr)
  performance_list[[paste0(rf_name, "_Test")]] <- evaluate_predictions(y_test, pred_test_rf, rf_name, "Test", threshold = thr)

  rf_importance <- tibble::tibble(
    feature = names(rf_fit$variable.importance),
    importance = as.numeric(rf_fit$variable.importance)
  ) %>%
    dplyr::arrange(desc(importance))

  safe_fwrite(rf_importance, file.path(dir_tbl, "Step05_random_forest_importance.csv"))
}

## XGBoost
xgb_name <- "XGBoost: dynamic + trajectory"
cat("\nFitting XGBoost:", xgb_name, "\n")

prep_xgb <- make_preprocessor(train_df, model_predictor_sets[[xgb_name]], primary_outcome)
tr_xgb <- apply_preprocessor(prep_xgb, train_df)
te_xgb <- apply_preprocessor(prep_xgb, test_df)
mats_xgb <- align_matrices(tr_xgb$matrix, te_xgb$matrix)

x_train_xgb <- mats_xgb$train
x_test_xgb <- mats_xgb$test

nzv_xgb <- apply(x_train_xgb, 2, sd, na.rm = TRUE) > 0
x_train_xgb <- x_train_xgb[, nzv_xgb, drop = FALSE]
x_test_xgb <- x_test_xgb[, colnames(x_train_xgb), drop = FALSE]

dtrain <- xgboost::xgb.DMatrix(data = x_train_xgb, label = y_train)
dtest <- xgboost::xgb.DMatrix(data = x_test_xgb, label = y_test)

pos_rate <- mean(y_train == 1)
scale_pos_weight <- ifelse(pos_rate > 0, (1 - pos_rate) / pos_rate, 1)

xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 3,
  eta = 0.03,
  subsample = 0.80,
  colsample_bytree = 0.80,
  min_child_weight = 15,
  gamma = 0.5,
  lambda = 2,
  alpha = 0.1,
  scale_pos_weight = scale_pos_weight
)

xgb_fit <- tryCatch({
  set.seed(20260601)
  xgboost::xgb.train(
    params = xgb_params,
    data = dtrain,
    nrounds = 1200,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = 60,
    verbose = 0
  )
}, error = function(e) {
  message("XGBoost failed: ", e$message)
  NULL
})

if (!is.null(xgb_fit)) {
  pred_train_xgb <- predict(xgb_fit, dtrain)
  pred_test_xgb <- predict(xgb_fit, dtest)
  pred_train_xgb <- pmin(pmax(pred_train_xgb, 1e-6), 1 - 1e-6)
  pred_test_xgb <- pmin(pmax(pred_test_xgb, 1e-6), 1 - 1e-6)

  model_objects[[xgb_name]] <- list(object = xgb_fit, prep = prep_xgb)

  prediction_list[[paste0(xgb_name, "_Train")]] <- tibble::tibble(
    model = xgb_name, split = "Train", stay_id = train_df$stay_id, y = y_train, pred = pred_train_xgb
  )
  prediction_list[[paste0(xgb_name, "_Test")]] <- tibble::tibble(
    model = xgb_name, split = "Test", stay_id = test_df$stay_id, y = y_test, pred = pred_test_xgb
  )

  thr <- best_youden_threshold(y_train, pred_train_xgb)
  performance_list[[paste0(xgb_name, "_Train")]] <- evaluate_predictions(y_train, pred_train_xgb, xgb_name, "Train", threshold = thr)
  performance_list[[paste0(xgb_name, "_Test")]] <- evaluate_predictions(y_test, pred_test_xgb, xgb_name, "Test", threshold = thr)

  xgb_importance <- tryCatch({
    xgboost::xgb.importance(feature_names = colnames(x_train_xgb), model = xgb_fit) %>%
      tibble::as_tibble()
  }, error = function(e) tibble())

  if (nrow(xgb_importance) > 0) {
    safe_fwrite(xgb_importance, file.path(dir_tbl, "Step05_xgboost_importance_gain.csv"))
  }

  ## SHAP-like contribution values by predcontrib
  shap_mat <- tryCatch({
    predict(xgb_fit, dtest, predcontrib = TRUE)
  }, error = function(e) NULL)

  if (!is.null(shap_mat)) {
    shap_df <- as.data.frame(shap_mat, check.names = FALSE)
    shap_cols <- setdiff(colnames(shap_df), "BIAS")
    shap_importance <- tibble::tibble(
      feature = shap_cols,
      mean_abs_shap = purrr::map_dbl(shap_cols, ~ mean(abs(shap_df[[.x]]), na.rm = TRUE))
    ) %>%
      dplyr::arrange(desc(mean_abs_shap))

    safe_fwrite(shap_importance, file.path(dir_tbl, "Step05_xgboost_SHAP_importance.csv"))
  } else {
    shap_importance <- tibble()
  }
} else {
  xgb_importance <- tibble()
  shap_importance <- tibble()
}

## =========================================================
## 10. Collect predictions and performance
## =========================================================

predictions_all <- dplyr::bind_rows(prediction_list)
performance_df <- dplyr::bind_rows(performance_list) %>%
  dplyr::arrange(dataset, desc(AUROC))

safe_fwrite(predictions_all, out_predictions)
safe_fwrite(performance_df, file.path(dir_tbl, "Step05_model_performance_train_test.csv"))

test_perf <- performance_df %>%
  dplyr::filter(dataset == "Test") %>%
  dplyr::arrange(desc(AUROC), desc(AUPRC), Brier)

n_top_models <- min(5L, nrow(test_perf))
top_models <- test_perf %>%
  dplyr::slice_head(n = n_top_models) %>%
  dplyr::pull(model)

cat("\nTop models by Test AUROC:\n")
print(test_perf %>% dplyr::select(model, AUROC, AUPRC, Brier, ECE_10))

## =========================================================
## 11. Incremental value of adding trajectory to dynamic GLM
## =========================================================

incremental_compare <- performance_df %>%
  dplyr::filter(
    dataset == "Test",
    model %in% c("Organ-domain dynamic model", "Trajectory-enhanced dynamic model")
  ) %>%
  dplyr::select(model, AUROC, AUPRC, Brier, ECE_10, calibration_intercept, calibration_slope) %>%
  tidyr::pivot_longer(cols = -model, names_to = "metric", values_to = "value") %>%
  tidyr::pivot_wider(names_from = model, values_from = value) %>%
  dplyr::mutate(
    delta_trajectory_minus_dynamic = `Trajectory-enhanced dynamic model` - `Organ-domain dynamic model`
  )

safe_fwrite(incremental_compare, file.path(dir_tbl, "Step05_incremental_value_C1C5_vs_dynamic_GLM.csv"))

## =========================================================
## 12. Calibration, ROC, PR, DCA
## =========================================================

predictions_test <- predictions_all %>% dplyr::filter(split == "Test")

roc_df <- purrr::map_dfr(top_models, function(mn) {
  dat <- predictions_test %>% dplyr::filter(model == mn)
  make_roc_df(dat$y, dat$pred, mn)
})

pr_df <- purrr::map_dfr(top_models, function(mn) {
  dat <- predictions_test %>% dplyr::filter(model == mn)
  make_pr_df(dat$y, dat$pred, mn)
})

calibration_df <- purrr::map_dfr(top_models, function(mn) {
  dat <- predictions_test %>% dplyr::filter(model == mn)
  make_calibration_df(dat$y, dat$pred, mn, n_bins = 10)
})

dca_df <- purrr::map_dfr(top_models, function(mn) {
  dat <- predictions_test %>% dplyr::filter(model == mn)
  decision_curve_df(dat$y, dat$pred, mn)
})

dca_all_none <- dca_df %>%
  dplyr::group_by(threshold) %>%
  dplyr::summarise(
    net_benefit_all = dplyr::first(net_benefit_all),
    net_benefit_none = 0,
    .groups = "drop"
  )

safe_fwrite(roc_df, file.path(dir_tbl, "Step05_T48_ROC_curve_data.csv"))
safe_fwrite(pr_df, file.path(dir_tbl, "Step05_T48_PR_curve_data.csv"))
safe_fwrite(calibration_df, file.path(dir_tbl, "Step05_T48_calibration_deciles.csv"))
safe_fwrite(dca_df, file.path(dir_tbl, "Step05_T48_decision_curve.csv"))

## =========================================================
## 13. Final model and AMI-EDRS risk score
## =========================================================

## Prefer XGBoost if fitted and its test AUROC is not clearly worse.
## Otherwise use the best test AUROC model.
if (xgb_name %in% test_perf$model) {
  xgb_auc <- test_perf$AUROC[test_perf$model == xgb_name][1]
  best_auc <- max(test_perf$AUROC, na.rm = TRUE)
  final_model_name <- ifelse(!is.na(xgb_auc) && xgb_auc >= best_auc - 0.01, xgb_name, test_perf$model[1])
} else {
  final_model_name <- test_perf$model[1]
}

cat("\nFinal selected model for AMI-EDRS:", final_model_name, "\n\n")

final_pred_test <- predictions_test %>%
  dplyr::filter(model == final_model_name) %>%
  dplyr::transmute(
    stay_id,
    y,
    AMI_EDRS_probability = pred,
    AMI_EDRS_0_100 = round(pred * 100, 1),
    AMI_EDRS_risk_group = dplyr::case_when(
      AMI_EDRS_probability < 0.05 ~ "Low risk (<5%)",
      AMI_EDRS_probability < 0.15 ~ "Intermediate risk (5-15%)",
      AMI_EDRS_probability < 0.30 ~ "High risk (15-30%)",
      TRUE ~ "Very high risk (>=30%)"
    ),
    AMI_EDRS_risk_group = factor(
      AMI_EDRS_risk_group,
      levels = c("Low risk (<5%)", "Intermediate risk (5-15%)", "High risk (15-30%)", "Very high risk (>=30%)")
    )
  ) %>%
  dplyr::left_join(
    test_df %>%
      dplyr::select(
        stay_id,
        dplyr::any_of(c(
          "subject_id", "hadm_id", "trajectory",
          "hemodynamic_T0_score", "hemodynamic_T24_score", "hemodynamic_T48_score",
          "respiratory_neurologic_T0_score", "respiratory_neurologic_T24_score", "respiratory_neurologic_T48_score",
          "renal_metabolic_T0_score", "renal_metabolic_T24_score", "renal_metabolic_T48_score",
          "inflammatory_coagulopathic_T0_score", "inflammatory_coagulopathic_T24_score", "inflammatory_coagulopathic_T48_score",
          "cardiac_hypoperfusion_T0_score", "cardiac_hypoperfusion_T24_score", "cardiac_hypoperfusion_T48_score"
        ))
      ),
    by = "stay_id"
  )

safe_fwrite(final_pred_test, out_score_test)

risk_group_summary <- final_pred_test %>%
  dplyr::group_by(AMI_EDRS_risk_group) %>%
  dplyr::summarise(
    n = dplyr::n(),
    deaths = sum(y == 1, na.rm = TRUE),
    observed_mortality_pct = deaths / n * 100,
    mean_predicted_risk_pct = mean(AMI_EDRS_probability, na.rm = TRUE) * 100,
    median_score = median(AMI_EDRS_0_100, na.rm = TRUE),
    q25_score = quantile(AMI_EDRS_0_100, 0.25, na.rm = TRUE),
    q75_score = quantile(AMI_EDRS_0_100, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

safe_fwrite(risk_group_summary, file.path(dir_tbl, "Step05_AMI_EDRS_risk_group_summary_test.csv"))

trajectory_risk_summary <- final_pred_test %>%
  dplyr::filter(!is.na(trajectory)) %>%
  dplyr::group_by(trajectory) %>%
  dplyr::summarise(
    n = dplyr::n(),
    deaths = sum(y == 1, na.rm = TRUE),
    observed_mortality_pct = deaths / n * 100,
    mean_predicted_risk_pct = mean(AMI_EDRS_probability, na.rm = TRUE) * 100,
    median_score = median(AMI_EDRS_0_100, na.rm = TRUE),
    .groups = "drop"
  )

safe_fwrite(trajectory_risk_summary, file.path(dir_tbl, "Step05_AMI_EDRS_by_C1C5_trajectory_test.csv"))

## =========================================================
## 14. Figures
## =========================================================

## Figure 5A: model architecture / feature layer availability
feature_layer_df <- tibble::tibble(
  layer = c(
    "Host vulnerability",
    "Conventional clinical severity",
    "Static T0 organ-domain severity",
    "Organ-domain dynamic model",
    "C1-C5 trajectory phenotype",
    "ML-enhanced dynamic prediction"
  ),
  order = 1:6,
  description = c(
    "Demographics and comorbidities",
    "Host factors plus conventional severity signals",
    "Baseline organ-domain burden at T0",
    "T0/T24/T48 organ-domain evolution",
    "Data-supported trajectory phenotype from Step03",
    "Elastic net, random forest, and XGBoost"
  )
)

fig5a <- ggplot(feature_layer_df, aes(x = order, y = 1, fill = layer)) +
  geom_tile(width = 0.92, height = 0.62, color = "white", linewidth = 1.2) +
  geom_text(aes(label = layer), fontface = "bold", size = 4.3, color = "black") +
  geom_text(aes(label = description), y = 0.70, size = 3.4, color = "grey25") +
  scale_fill_manual(values = c(
    "Host vulnerability" = "#89B3D0",
    "Conventional clinical severity" = "#4A7494",
    "Static T0 organ-domain severity" = "#9D80BC",
    "Organ-domain dynamic model" = "#F2B85B",
    "C1-C5 trajectory phenotype" = "#EC7725",
    "ML-enhanced dynamic prediction" = "#DA1F2E"
  )) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  labs(
    title = "Figure 5A. Step05 dynamic prediction framework integrating C1-C5 trajectories",
    subtitle = "Trajectory phenotype is explicitly fused with T48 dynamic predictors to form AMI-EDRS",
    x = NULL,
    y = NULL
  ) +
  theme_ami(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

save_plot(fig5a, "Figure5A_dynamic_prediction_framework_with_C1C5", width = 14, height = 4.3)

## Figure 5B: performance bars
perf_plot_df <- performance_df %>%
  dplyr::filter(dataset == "Test") %>%
  dplyr::select(model, AUROC, AUPRC, Brier) %>%
  tidyr::pivot_longer(cols = c(AUROC, AUPRC, Brier), names_to = "metric", values_to = "value") %>%
  dplyr::mutate(
    model = factor(model, levels = rev(test_perf$model)),
    model_class = ifelse(stringr::str_detect(as.character(model), "trajectory|Elastic|Random|XGBoost|dynamic", ignore_case = TRUE),
                         "Enhanced/dynamic", "Baseline/clinical")
  )

fig5b <- ggplot(perf_plot_df, aes(x = value, y = model, fill = model_class)) +
  geom_col(width = 0.70) +
  geom_text(aes(label = sprintf("%.3f", value)), hjust = -0.08, size = 3.8, fontface = "bold") +
  facet_wrap(~ metric, scales = "free_x", ncol = 3) +
  scale_fill_manual(values = c("Baseline/clinical" = pal_blue, "Enhanced/dynamic" = pal_gold)) +
  labs(
    title = "Figure 5B. Discrimination and overall accuracy of T48 prediction models",
    subtitle = "Gold highlights dynamic or trajectory-enhanced models; gray-blue indicates baseline or conventional clinical models",
    x = "Metric value",
    y = NULL,
    fill = NULL
  ) +
  theme_ami(base_size = 13) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 14)
  ) +
  coord_cartesian(clip = "off")

save_plot(fig5b, "Figure5B_T48_model_performance_bars", width = 13.5, height = 8.0)

## Figure 5C: ROC curves
roc_label_df <- roc_df %>%
  dplyr::distinct(model, AUROC) %>%
  dplyr::mutate(model_label = paste0(model, " (AUROC=", sprintf("%.3f", AUROC), ")"))

roc_df_plot <- roc_df %>%
  dplyr::left_join(roc_label_df, by = c("model", "AUROC"))

fig5c <- ggplot(roc_df_plot, aes(x = fpr, y = sensitivity, color = model_label)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey55") +
  geom_line(linewidth = 1.15) +
  scale_color_manual(values = rep(c(pal_gold, pal_blue, pal_orange, pal_green, pal_purple, pal_red, "#89B3D0"), length.out = length(unique(roc_df_plot$model_label)))) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  labs(
    title = "Figure 5C. ROC curves for top-performing T48 models",
    subtitle = "Prediction target: 30-day mortality after the T48 landmark",
    x = "1 - Specificity",
    y = "Sensitivity",
    color = NULL
  ) +
  theme_ami(base_size = 13) +
  theme(legend.position = "bottom")

save_plot(fig5c, "Figure5C_T48_ROC_curves", width = 10.5, height = 8.2)

## Figure 5D: PR curves
pr_label_df <- pr_df %>%
  dplyr::distinct(model, AUPRC) %>%
  dplyr::mutate(model_label = paste0(model, " (AUPRC=", sprintf("%.3f", AUPRC), ")"))

pr_df_plot <- pr_df %>%
  dplyr::left_join(pr_label_df, by = c("model", "AUPRC"))

fig5d <- ggplot(pr_df_plot, aes(x = recall, y = precision, color = model_label)) +
  geom_line(linewidth = 1.15) +
  scale_color_manual(values = rep(c(pal_gold, pal_blue, pal_orange, pal_green, pal_purple, pal_red, "#89B3D0"), length.out = length(unique(pr_df_plot$model_label)))) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = "Figure 5D. Precision-recall curves for top-performing T48 models",
    subtitle = "AUPRC is emphasized because 30-day mortality is an imbalanced outcome",
    x = "Recall",
    y = "Precision",
    color = NULL
  ) +
  theme_ami(base_size = 13) +
  theme(legend.position = "bottom")

save_plot(fig5d, "Figure5D_T48_precision_recall_curves", width = 10.5, height = 8.2)

## Figure 5E: calibration
fig5e <- ggplot(calibration_df, aes(x = predicted, y = observed, color = model, fill = model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey55") +
  geom_line(linewidth = 1.1) +
  geom_point(aes(size = n), alpha = 0.90) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.01, alpha = 0.50) +
  scale_color_manual(values = rep(c(pal_gold, pal_blue, pal_orange, pal_green, pal_purple, pal_red, "#89B3D0"), length.out = length(unique(calibration_df$model)))) +
  scale_fill_manual(values = rep(c(pal_gold, pal_blue, pal_orange, pal_green, pal_purple, pal_red, "#89B3D0"), length.out = length(unique(calibration_df$model)))) +
  scale_size_continuous(range = c(2, 6)) +
  coord_cartesian(xlim = c(0, max(calibration_df$predicted, na.rm = TRUE) * 1.05), ylim = c(0, max(calibration_df$observed, na.rm = TRUE) * 1.10)) +
  labs(
    title = "Figure 5E. Calibration of top-performing T48 models",
    subtitle = "Observed versus predicted event rates across prediction deciles",
    x = "Mean predicted risk",
    y = "Observed 30-day mortality",
    color = NULL,
    fill = NULL,
    size = "n"
  ) +
  theme_ami(base_size = 13) +
  theme(legend.position = "bottom")

save_plot(fig5e, "Figure5E_T48_calibration_curves", width = 10.5, height = 7.6)

## Figure 5F: DCA
fig5f <- ggplot() +
  geom_line(
    data = dca_all_none,
    aes(x = threshold, y = net_benefit_all),
    color = pal_blue,
    linewidth = 1.0,
    linetype = "dashed"
  ) +
  geom_line(
    data = dca_all_none,
    aes(x = threshold, y = net_benefit_none),
    color = "grey45",
    linewidth = 1.0,
    linetype = "dotted"
  ) +
  geom_line(
    data = dca_df,
    aes(x = threshold, y = net_benefit, color = model),
    linewidth = 1.05
  ) +
  scale_color_manual(values = rep(c(pal_gold, pal_blue, pal_orange, pal_green, pal_purple, pal_red, "#89B3D0"), length.out = length(unique(dca_df$model)))) +
  coord_cartesian(xlim = c(0.01, 0.50)) +
  labs(
    title = "Figure 5F. Decision curve analysis for top-performing models",
    subtitle = "Dashed gray-blue line represents treat-all; dotted line represents treat-none",
    x = "Risk threshold",
    y = "Net benefit",
    color = NULL
  ) +
  theme_ami(base_size = 13) +
  theme(legend.position = "bottom")

save_plot(fig5f, "Figure5F_T48_decision_curve_analysis", width = 11.5, height = 7.4)

## Figure 5G: SHAP importance
if (exists("shap_importance") && nrow(shap_importance) > 0) {
  shap_plot_df <- shap_importance %>%
    dplyr::slice_head(n = 25) %>%
    dplyr::mutate(
      feature_label = stringr::str_replace_all(feature, "_", " "),
      feature_label = forcats::fct_reorder(feature_label, mean_abs_shap)
    )

  fig5g <- ggplot(shap_plot_df, aes(x = mean_abs_shap, y = feature_label)) +
    geom_col(fill = pal_gold, width = 0.72) +
    labs(
      title = "Figure 5G. XGBoost feature importance based on SHAP values",
      subtitle = "Top predictors ranked by mean absolute SHAP contribution in the test set",
      x = "Mean absolute SHAP value",
      y = NULL
    ) +
    theme_ami(base_size = 13)

  save_plot(fig5g, "Figure5G_T48_XGBoost_SHAP_importance", width = 10.0, height = 8.5)
}

## Figure 5H: risk groups
fig5h <- ggplot(risk_group_summary, aes(x = AMI_EDRS_risk_group, y = observed_mortality_pct, fill = AMI_EDRS_risk_group)) +
  geom_col(width = 0.70) +
  geom_text(
    aes(label = paste0(sprintf("%.1f%%", observed_mortality_pct), "\n(n=", n, ")")),
    vjust = -0.25,
    fontface = "bold",
    size = 4.1
  ) +
  scale_fill_manual(values = pal_risk) +
  labs(
    title = "Figure 5H. Observed mortality across AMI-EDRS risk groups",
    subtitle = paste0("Risk groups were defined using predicted probability from: ", final_model_name),
    x = NULL,
    y = "Observed 30-day mortality after T48 (%)",
    fill = NULL
  ) +
  theme_ami(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 15, hjust = 1, face = "bold")
  ) +
  coord_cartesian(ylim = c(0, max(risk_group_summary$observed_mortality_pct, na.rm = TRUE) * 1.25))

save_plot(fig5h, "Figure5H_T48_AMI_EDRS_risk_groups", width = 10.5, height = 6.8)

## Figure 5I: AMI-EDRS across C1-C5 trajectories
if (nrow(trajectory_risk_summary) > 0) {
  fig5i <- ggplot(trajectory_risk_summary, aes(x = trajectory, y = mean_predicted_risk_pct, fill = trajectory)) +
    geom_col(width = 0.70) +
    geom_text(
      aes(label = paste0("Pred ", sprintf("%.1f%%", mean_predicted_risk_pct), "\nObs ", sprintf("%.1f%%", observed_mortality_pct))),
      vjust = -0.25,
      fontface = "bold",
      size = 4.0
    ) +
    scale_fill_manual(values = c(
      "C1" = pal_blue,
      "C2" = pal_gold,
      "C3" = pal_purple,
      "C4" = pal_green,
      "C5" = pal_orange
    )) +
    labs(
      title = "Figure 5I. AMI-EDRS risk burden across C1-C5 trajectory phenotypes",
      subtitle = "Predicted risk and observed mortality are aligned to connect trajectory phenotype with dynamic prediction",
      x = "Trajectory phenotype",
      y = "Mean predicted 30-day risk after T48 (%)",
      fill = NULL
    ) +
    theme_ami(base_size = 13) +
    theme(legend.position = "none") +
    coord_cartesian(ylim = c(0, max(trajectory_risk_summary$mean_predicted_risk_pct, na.rm = TRUE) * 1.25))

  save_plot(fig5i, "Figure5I_AMI_EDRS_by_C1C5_trajectory", width = 9.5, height = 6.8)
}

## =========================================================
## 15. Export Excel workbook
## =========================================================

method_note <- tibble::tribble(
  ~item, ~description,
  "Step", "Step05: T48 dynamic prediction model integrating C1-C5 trajectory phenotype.",
  "Primary target", "30-day mortality after the T48 landmark.",
  "Main integration", "C1-C5 trajectory phenotype from Step03-final is explicitly included in the trajectory-enhanced dynamic GLM and all dynamic machine-learning models.",
  "Main comparison", "Organ-domain dynamic model versus trajectory-enhanced dynamic model.",
  "Clinical principle", "T0/T24/T48 variables are used for T48 prediction. T72 is not used to avoid future information leakage.",
  "Model evaluation", "Stratified 70/30 train-test split; AUROC, AUPRC, Brier, calibration intercept/slope, ECE, sensitivity, specificity, PPV, NPV, decision curve analysis.",
  "AMI-EDRS", "Predicted probability from the final selected model is scaled to 0-100 and grouped into <5%, 5-15%, 15-30%, and >=30% risk groups.",
  "Final model selection", paste0("Final model selected: ", final_model_name)
)

wb <- openxlsx::createWorkbook()

add_sheet(wb, "00_method_note", method_note)
add_sheet(wb, "01_model_performance", performance_df)
add_sheet(wb, "02_test_performance", test_perf)
add_sheet(wb, "03_incremental_C1C5", incremental_compare)
add_sheet(wb, "04_predictor_map", predictor_map)
add_sheet(wb, "05_variable_missingness", missing_df)
add_sheet(wb, "06_calibration_deciles", calibration_df)
add_sheet(wb, "07_decision_curve", dca_df)
add_sheet(wb, "08_AMI_EDRS_groups", risk_group_summary)
add_sheet(wb, "09_AMI_EDRS_by_C1C5", trajectory_risk_summary)
if (exists("shap_importance") && nrow(shap_importance) > 0) add_sheet(wb, "10_XGB_SHAP", shap_importance)
if (exists("xgb_importance") && nrow(xgb_importance) > 0) add_sheet(wb, "11_XGB_gain", xgb_importance)
if (exists("enet_importance") && nrow(enet_importance) > 0) add_sheet(wb, "12_elastic_net_coef", enet_importance)

openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

## =========================================================
## 16. Save model objects
## =========================================================

step05_object <- list(
  project = "MIMIC_AMI_DynamicRisk_Project",
  step = "Step05_T48_dynamic_prediction_model_with_C1C5",
  created_at = Sys.time(),
  primary_outcome = primary_outcome,
  final_model_name = final_model_name,
  model_predictor_sets = model_predictor_sets,
  model_objects = model_objects,
  train_test_split = split_df,
  predictions_all = predictions_all,
  performance_df = performance_df,
  test_perf = test_perf,
  incremental_compare = incremental_compare,
  calibration_df = calibration_df,
  dca_df = dca_df,
  risk_group_summary = risk_group_summary,
  trajectory_risk_summary = trajectory_risk_summary
)

saveRDS(step05_object, out_model_objects)

## =========================================================
## 17. Finish
## =========================================================

cat("\n============================================================\n")
cat("Step05 finished successfully.\n")
cat("============================================================\n\n")

cat("Summary workbook:\n", out_xlsx, "\n")
cat("Predictions:\n", out_predictions, "\n")
cat("AMI-EDRS score test set:\n", out_score_test, "\n")
cat("Model objects:\n", out_model_objects, "\n")
cat("Figures folder:\n", dir_fig, "\n")
cat("Log file:\n", log_file, "\n\n")

cat("Key result files:\n")
cat("tables/Step05_model_performance_train_test.csv\n")
cat("tables/Step05_incremental_value_C1C5_vs_dynamic_GLM.csv\n")
cat("tables/Step05_AMI_EDRS_risk_group_summary_test.csv\n")
cat("tables/Step05_AMI_EDRS_by_C1C5_trajectory_test.csv\n")
cat("figures/Figure5B_T48_model_performance_bars.png\n")
cat("figures/Figure5C_T48_ROC_curves.png\n")
cat("figures/Figure5E_T48_calibration_curves.png\n")
cat("figures/Figure5F_T48_decision_curve_analysis.png\n")
cat("figures/Figure5H_T48_AMI_EDRS_risk_groups.png\n")
cat("figures/Figure5I_AMI_EDRS_by_C1C5_trajectory.png\n\n")

cat("Test performance:\n")
print(test_perf)

cat("\nIncremental value of C1-C5 trajectory:\n")
print(incremental_compare)

cat("\nAMI-EDRS risk groups:\n")
print(risk_group_summary)

cat("\nAMI-EDRS by C1-C5 trajectory:\n")
print(trajectory_risk_summary)

cat("\nSession info:\n")
print(sessionInfo())

sink()
