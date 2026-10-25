# HISTORICAL PROVENANCE ONLY. Not the current AMI-EDRS model. See docs/REPRODUCIBILITY.md.
############################################################
## Step08 Benchmark-only analysis
## AMI-EDRS / dynamic models versus conventional clinical scores
## Project: MIMIC_AMI_DynamicRisk_Project
##
## This script is independent from the previous Step08 scripts.
## It only does model comparison with traditional / conventional scores.
##
## Main goals:
## 1) Read row-level Step05 model predictions.
## 2) Read the T48 patient-level analysis dataset.
## 3) Detect existing SOFA / SAPS II / OASIS / APS III / LODS / GRACE scores.
## 4) If official scores are unavailable, construct clearly labelled score proxies
##    from available T48/early variables and optional raw MIMIC-IV T7 components.
## 5) Calibrate each traditional score on the training set and compare on test set.
## 6) Produce publication-style PNG/PDF figures and Excel summary.
##
## IMPORTANT:
## - Official scores are only reported as official when an existing column is found.
## - Scores constructed from available variables are labelled as "-like proxy".
## - All benchmark score calibration is learned in Train and evaluated in Test.
## - Row-level Step05 predictions are required for ROC/PR/calibration/DCA.
############################################################

rm(list = ls())
gc()

options(stringsAsFactors = FALSE)
options(warn = 1)

## =========================================================
## 0. User paths
## =========================================================

project_dir <- Sys.getenv("AMI_MIMIC_PROJECT", unset=file.path(getwd(), "work/MIMIC"))

## MIMIC-IV raw folders on T7
hosp_dir <- file.path(Sys.getenv("AMI_MIMIC_RAW", unset=file.path(getwd(), "data/mimic-iv-3.1")), "hosp")
icu_dir  <- file.path(Sys.getenv("AMI_MIMIC_RAW", unset=file.path(getwd(), "data/mimic-iv-3.1")), "icu")

## Step05 row-level prediction file.
## This should usually be under analysis_datasets, not the tables summary workbook.
step05_prediction_file <- file.path(
  project_dir,
  "09_step05_T48_dynamic_prediction_model_with_C1C5",
  "analysis_datasets",
  "Step05_T48_model_predictions_train_test.csv"
)

## Fallback summary workbook; used only to locate sheets if row-level CSV is missing.
step05_summary_file <- file.path(
  project_dir,
  "09_step05_T48_dynamic_prediction_model_with_C1C5",
  "tables",
  "Step05_T48_dynamic_prediction_with_C1C5_summary.xlsx"
)

## T48 patient-level analysis dataset containing stay_id/hadm_id/outcome/features.
## The script will auto-detect if this exact file is missing.
t48_analysis_file <- file.path(
  project_dir,
  "09_step05_T48_dynamic_prediction_model_with_C1C5",
  "analysis_datasets",
  "Step05_T48_analysis_dataset_with_C1C5.csv"
)

## Raw extraction switch.
## If TRUE, the script attempts to extract selected first-24h vital/lab components
## from raw MIMIC-IV using duckdb when needed. This can take time because chartevents
## and labevents are large. If your Step05/T48 dataset already contains enough
## variables, you can set this to FALSE.
use_raw_t7_extract_if_needed <- TRUE

out_dir <- file.path(project_dir, "12_step08_benchmark_comparison_with_traditional_scores")
dir_tbl <- file.path(out_dir, "tables")
dir_fig <- file.path(out_dir, "figures")
dir_dat <- file.path(out_dir, "analysis_datasets")
dir_log <- file.path(out_dir, "logs")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tbl, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_dat, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_log, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(dir_log, paste0("Step08_benchmark_traditional_scores_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))

## =========================================================
## 1. Packages
## =========================================================

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

pkg_needed <- c(
  "dplyr", "tidyr", "purrr", "stringr", "tibble", "data.table",
  "readr", "readxl", "openxlsx", "ggplot2", "scales", "forcats",
  "pROC", "patchwork", "broom"
)
invisible(lapply(pkg_needed, install_if_missing))

if (use_raw_t7_extract_if_needed && !requireNamespace("duckdb", quietly = TRUE)) {
  install.packages("duckdb", repos = "https://cloud.r-project.org")
}

sink(log_file, split = TRUE)
cat("\n============================================================\n")
cat("Step08 benchmark-only comparison with traditional scores\n")
cat("Time:", as.character(Sys.time()), "\n")
cat("Project dir:", project_dir, "\n")
cat("Output dir:", out_dir, "\n")
cat("============================================================\n\n")

cat("Path checks:\n")
cat("hosp_dir exists:", dir.exists(hosp_dir), "\n")
cat("icu_dir exists :", dir.exists(icu_dir), "\n")
cat("step05_prediction_file exists:", file.exists(step05_prediction_file), "\n")
cat("step05_summary_file exists    :", file.exists(step05_summary_file), "\n")
cat("t48_analysis_file exists      :", file.exists(t48_analysis_file), "\n\n")

## =========================================================
## 2. Theme and plotting utilities
## =========================================================

pal_blue <- "#4A7494"
pal_gold <- "#F2B85B"
pal_gray <- "#E3E8EC"
pal_red <- "#DA1F2E"
pal_purple <- "#7A5D98"
pal_green <- "#13974A"
pal_orange <- "#EC7725"
pal_dark <- "#1F2933"

pal_group <- c(
  "AMI-EDRS / dynamic model" = pal_gold,
  "Traditional score" = pal_blue,
  "Traditional score proxy" = "#7A5D98",
  "Other model" = "#8B9092"
)

make_theme_pub <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = pal_gray, linewidth = 0.50),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold", color = "black", size = base_size + 1),
      axis.text = element_text(color = "black", size = base_size - 1),
      strip.background = element_rect(fill = "#F2F5F8", color = "#D8DEE6", linewidth = 0.35),
      strip.text = element_text(face = "bold", size = base_size, color = "black"),
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = base_size + 5, color = "black", hjust = 0),
      plot.subtitle = element_text(size = base_size, color = "grey35", hjust = 0),
      axis.line = element_line(color = "black", linewidth = 0.35),
      plot.margin = margin(10, 16, 10, 16)
    )
}

save_pub_plot <- function(plot_obj, filename_stub, width = 10, height = 7) {
  ggplot2::ggsave(file.path(dir_fig, paste0(filename_stub, ".png")), plot_obj,
                  width = width, height = height, dpi = 600, bg = "white")
  ggplot2::ggsave(file.path(dir_fig, paste0(filename_stub, ".pdf")), plot_obj,
                  width = width, height = height, device = cairo_pdf, bg = "white")
}

safe_fwrite <- function(x, path) {
  data.table::fwrite(x, path)
  cat("Saved:", path, "\n")
}

add_sheet <- function(wb, sheet_name, df) {
  sheet_name <- substr(sheet_name, 1, 31)
  if (is.null(df) || !inherits(df, "data.frame") || nrow(df) == 0 || ncol(df) == 0) {
    df <- tibble::tibble(note = "No data available")
  }
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeDataTable(wb, sheet = sheet_name, x = df, tableStyle = "TableStyleMedium2", withFilter = TRUE)
  openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet = sheet_name, cols = seq_len(ncol(df)), widths = "auto")
}

clean_names <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

standardize_names <- function(df) {
  names(df) <- clean_names(names(df))
  df
}

safe_read_any <- function(path, sheet = NULL) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("csv", "gz")) {
    data.table::fread(path) %>% as.data.frame() %>% standardize_names()
  } else if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(path, sheet = sheet %||% 1) %>% as.data.frame() %>% standardize_names()
  } else if (ext == "rds") {
    obj <- readRDS(path)
    if (inherits(obj, "data.frame")) {
      obj %>% as.data.frame() %>% standardize_names()
    } else if (is.list(obj) && "predictions_all" %in% names(obj)) {
      obj$predictions_all %>% as.data.frame() %>% standardize_names()
    } else {
      stop("Unsupported RDS structure: ", path)
    }
  } else {
    stop("Unsupported file type: ", path)
  }
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

find_xlsx_sheet_with_cols <- function(path, required_any = character(), required_all = character()) {
  sheets <- readxl::excel_sheets(path)
  for (s in sheets) {
    dat <- tryCatch(safe_read_any(path, sheet = s), error = function(e) NULL)
    if (is.null(dat)) next
    cn <- names(dat)
    ok_all <- length(required_all) == 0 || all(required_all %in% cn)
    ok_any <- length(required_any) == 0 || any(required_any %in% cn)
    if (ok_all && ok_any) return(list(path = path, sheet = s, data = dat))
  }
  NULL
}

list_project_data_files <- function(project_dir) {
  if (!dir.exists(project_dir)) return(character())
  list.files(project_dir, pattern = "\\.(csv|csv.gz|xlsx|xls|rds)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
}

find_file_by_patterns <- function(files, include, exclude = character()) {
  base <- tolower(basename(files))
  keep <- rep(TRUE, length(files))
  for (p in include) keep <- keep & grepl(p, base, ignore.case = TRUE)
  for (p in exclude) keep <- keep & !grepl(p, base, ignore.case = TRUE)
  files[keep]
}

## =========================================================
## 3. Metric functions
## =========================================================

bound_prob <- function(p) pmin(pmax(as.numeric(p), 1e-6), 1 - 1e-6)

safe_auc <- function(y, pred) {
  y <- as.integer(y)
  pred <- as.numeric(pred)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]; pred <- pred[ok]
  if (length(unique(y)) < 2) return(NA_real_)
  tryCatch(as.numeric(pROC::auc(pROC::roc(y, pred, quiet = TRUE, direction = "<"))), error = function(e) NA_real_)
}

safe_auc_ci <- function(y, pred) {
  y <- as.integer(y)
  pred <- as.numeric(pred)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]; pred <- pred[ok]
  if (length(unique(y)) < 2) return(c(NA_real_, NA_real_, NA_real_))
  tryCatch(as.numeric(pROC::ci.auc(pROC::roc(y, pred, quiet = TRUE, direction = "<"))), error = function(e) c(NA_real_, NA_real_, NA_real_))
}

safe_auprc <- function(y, pred) {
  y <- as.integer(y)
  pred <- as.numeric(pred)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]; pred <- pred[ok]
  if (length(unique(y)) < 2) return(NA_real_)
  ord <- order(pred, decreasing = TRUE)
  y <- y[ord]
  tp <- cumsum(y == 1)
  fp <- cumsum(y == 0)
  precision <- tp / pmax(tp + fp, 1)
  recall <- tp / sum(y == 1)
  recall2 <- c(0, recall)
  precision2 <- c(1, precision)
  sum(diff(recall2) * precision2[-1], na.rm = TRUE)
}

calibration_metrics <- function(y, pred, n_bins = 10) {
  y <- as.integer(y)
  pred <- bound_prob(pred)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]; pred <- pred[ok]
  if (length(y) == 0) {
    return(tibble::tibble(Brier = NA_real_, calibration_intercept = NA_real_, calibration_slope = NA_real_, ECE_10 = NA_real_))
  }
  brier <- mean((y - pred)^2)
  lp <- qlogis(pred)
  cal_fit <- if (length(unique(y)) >= 2) tryCatch(suppressWarnings(glm(y ~ lp, family = binomial())), error = function(e) NULL) else NULL
  intercept <- if (!is.null(cal_fit)) unname(coef(cal_fit)[1]) else NA_real_
  slope <- if (!is.null(cal_fit)) unname(coef(cal_fit)[2]) else NA_real_
  q <- unique(quantile(pred, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE))
  if (length(q) < 3) {
    ece <- NA_real_
  } else {
    bin <- cut(pred, breaks = q, include.lowest = TRUE)
    cal_df <- tibble(y = y, pred = pred, bin = bin) %>%
      group_by(bin) %>% summarise(n = n(), obs = mean(y), exp = mean(pred), .groups = "drop")
    ece <- weighted.mean(abs(cal_df$obs - cal_df$exp), w = cal_df$n, na.rm = TRUE)
  }
  tibble(Brier = brier, calibration_intercept = intercept, calibration_slope = slope, ECE_10 = ece)
}

evaluate_predictions <- function(y, pred, model, model_group, split_label) {
  auc_ci <- safe_auc_ci(y, pred)
  tibble(
    model = model,
    model_group = model_group,
    split = split_label,
    n = sum(!is.na(y) & !is.na(pred)),
    events = sum(y == 1, na.rm = TRUE),
    event_rate = mean(y == 1, na.rm = TRUE),
    AUROC = safe_auc(y, pred),
    AUROC_low = auc_ci[1],
    AUROC_high = auc_ci[3],
    AUPRC = safe_auprc(y, pred)
  ) %>% bind_cols(calibration_metrics(y, pred))
}

make_roc_df <- function(y, pred, model) {
  y <- as.integer(y); pred <- as.numeric(pred)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]; pred <- pred[ok]
  if (length(unique(y)) < 2) return(tibble())
  roc_obj <- tryCatch(pROC::roc(y, pred, quiet = TRUE, direction = "<"), error = function(e) NULL)
  if (is.null(roc_obj)) return(tibble())
  tibble(model = model, specificity = roc_obj$specificities, sensitivity = roc_obj$sensitivities, fpr = 1 - specificity)
}

make_pr_df <- function(y, pred, model) {
  y <- as.integer(y); pred <- as.numeric(pred)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]; pred <- pred[ok]
  if (length(unique(y)) < 2) return(tibble())
  ord <- order(pred, decreasing = TRUE)
  y <- y[ord]; pred <- pred[ord]
  tp <- cumsum(y == 1); fp <- cumsum(y == 0)
  tibble(model = model, recall = tp / sum(y == 1), precision = tp / pmax(tp + fp, 1), threshold = pred)
}

make_calibration_df <- function(y, pred, model, n_bins = 10) {
  y <- as.integer(y); pred <- bound_prob(pred)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]; pred <- pred[ok]
  if (length(y) < 20) return(tibble())
  q <- unique(quantile(pred, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE))
  if (length(q) < 3) return(tibble())
  tibble(y = y, pred = pred, bin = cut(pred, breaks = q, include.lowest = TRUE)) %>%
    group_by(bin) %>%
    summarise(n = n(), observed = mean(y), predicted = mean(pred), .groups = "drop") %>%
    mutate(lower = pmax(observed - 1.96 * sqrt(observed * (1 - observed) / pmax(n, 1)), 0),
           upper = pmin(observed + 1.96 * sqrt(observed * (1 - observed) / pmax(n, 1)), 1),
           model = model)
}

make_dca_df <- function(y, pred, model, thresholds = seq(0.01, 0.50, by = 0.01)) {
  y <- as.integer(y); pred <- bound_prob(pred)
  ok <- !is.na(y) & !is.na(pred) & y %in% c(0, 1)
  y <- y[ok]; pred <- pred[ok]
  n <- length(y)
  if (n == 0) return(tibble())
  prevalence <- mean(y == 1)
  purrr::map_dfr(thresholds, function(pt) {
    treat <- pred >= pt
    tp <- sum(treat & y == 1)
    fp <- sum(treat & y == 0)
    tibble(
      model = model,
      threshold = pt,
      net_benefit = tp / n - fp / n * pt / (1 - pt),
      net_benefit_all = prevalence - (1 - prevalence) * pt / (1 - pt),
      net_benefit_none = 0
    )
  })
}

## =========================================================
## 4. Load Step05 row-level predictions
## =========================================================

find_prediction_object <- function() {
  required_any <- clean_names(c("pred", "prediction", "predicted_risk", "prob", "probability", "test_pred", "train_pred"))
  required_all <- clean_names(c("model"))
  if (file.exists(step05_prediction_file)) {
    dat <- safe_read_any(step05_prediction_file)
    return(list(path = step05_prediction_file, sheet = NA_character_, data = dat))
  }
  if (file.exists(step05_summary_file)) {
    obj <- find_xlsx_sheet_with_cols(step05_summary_file, required_any = required_any, required_all = required_all)
    if (!is.null(obj)) return(obj)
  }
  files <- list_project_data_files(project_dir)
  cand <- find_file_by_patterns(files, include = c("step05"), exclude = c("figure", "plot", "png", "pdf"))
  cand <- cand[grepl("pred|prediction|dynamic|t48", basename(cand), ignore.case = TRUE)]
  for (f in cand) {
    obj <- tryCatch({
      if (tolower(tools::file_ext(f)) %in% c("xlsx", "xls")) {
        find_xlsx_sheet_with_cols(f, required_any, required_all)
      } else {
        dat <- safe_read_any(f)
        cn <- names(dat)
        if (all(required_all %in% cn) && any(required_any %in% cn)) list(path = f, sheet = NA_character_, data = dat) else NULL
      }
    }, error = function(e) NULL)
    if (!is.null(obj)) return(obj)
  }
  NULL
}

pred_obj <- find_prediction_object()
if (is.null(pred_obj)) {
  cat("Could not find row-level Step05 predictions. Candidate files:\n")
  print(list_project_data_files(project_dir)[grepl("step05|pred|prediction|dynamic|t48", list_project_data_files(project_dir), ignore.case = TRUE)])
  stop("Please set step05_prediction_file to the row-level Step05 prediction CSV, usually analysis_datasets/Step05_T48_model_predictions_train_test.csv")
}

step05_predictions <- pred_obj$data %>% as.data.frame() %>% standardize_names()
cat("Detected Step05 predictions:\n")
cat("Path :", pred_obj$path, "\n")
cat("Sheet:", ifelse(is.na(pred_obj$sheet), "NA", pred_obj$sheet), "\n")
cat("Rows :", nrow(step05_predictions), " Cols:", ncol(step05_predictions), "\n")
print(names(step05_predictions))

## Standardize prediction columns
if (!"pred" %in% names(step05_predictions)) {
  pred_candidates <- c("prediction", "predicted_risk", "prob", "probability", "test_pred", "train_pred")
  hit <- pred_candidates[pred_candidates %in% names(step05_predictions)]
  if (length(hit) == 0) stop("No prediction probability column found in Step05 predictions.")
  step05_predictions$pred <- step05_predictions[[hit[1]]]
}
if (!"y" %in% names(step05_predictions)) {
  y_candidates <- c("outcome", "death_30d_after_landmark", "event", "label", "mortality")
  hit <- y_candidates[y_candidates %in% names(step05_predictions)]
  if (length(hit) == 0) stop("No outcome column found in Step05 predictions.")
  step05_predictions$y <- step05_predictions[[hit[1]]]
}
if (!"split" %in% names(step05_predictions)) {
  if ("dataset" %in% names(step05_predictions)) {
    step05_predictions$split <- step05_predictions$dataset
  } else {
    stop("No split/dataset column found in Step05 predictions. Need Train/Test split.")
  }
}
if (!"stay_id" %in% names(step05_predictions)) stop("stay_id is required in Step05 predictions for merging benchmark scores.")

step05_predictions <- step05_predictions %>%
  mutate(
    model = as.character(model),
    split = stringr::str_to_title(as.character(split)),
    y = as.integer(y),
    pred = bound_prob(pred),
    model_group = case_when(
      str_detect(model, regex("elastic|xgboost|random forest|dynamic|trajectory", ignore_case = TRUE)) ~ "AMI-EDRS / dynamic model",
      TRUE ~ "Other model"
    )
  ) %>%
  filter(split %in% c("Train", "Test"))

safe_fwrite(step05_predictions, file.path(dir_dat, "Step08_loaded_step05_predictions.csv"))

## =========================================================
## 5. Load T48 patient-level analysis dataset
## =========================================================

find_analysis_dataset <- function() {
  if (file.exists(t48_analysis_file)) return(t48_analysis_file)
  candidates <- c(
    file.path(project_dir, "08_step04_v2_trajectory_clinical_meaning", "analysis_datasets", "Step04_v2_T48_dataset_with_k5_trajectory_and_modules.csv"),
    file.path(project_dir, "09_step05_T48_dynamic_prediction_model_with_C1C5", "analysis_datasets", "Step05_T48_analysis_dataset_with_C1C5.csv"),
    file.path(project_dir, "03_analysis_dataset", "Step02_AMI_T48_dataset.csv")
  )
  candidates[file.exists(candidates)][1] %||% NA_character_
}

analysis_path <- find_analysis_dataset()
if (is.na(analysis_path) || !file.exists(analysis_path)) {
  stop("Could not find T48 patient-level analysis dataset. Please check t48_analysis_file.")
}
analysis_df <- safe_read_any(analysis_path)
cat("\nLoaded T48 analysis dataset:\n", analysis_path, "\n")
cat("Rows:", nrow(analysis_df), "Cols:", ncol(analysis_df), "\n")

if (!"stay_id" %in% names(analysis_df)) stop("stay_id missing in analysis dataset.")

## Join train/test and outcome from Step05
split_key <- step05_predictions %>% distinct(stay_id, split, y)
analysis_df <- analysis_df %>%
  left_join(split_key, by = "stay_id", suffix = c("", "_step05"))

safe_fwrite(analysis_df, file.path(dir_dat, "Step08_analysis_dataset_for_benchmark.csv"))

## =========================================================
## 6. Optional raw MIMIC-IV extraction of first-24h components
## =========================================================

pick_col <- function(df, patterns) {
  cn <- names(df)
  for (p in patterns) {
    hit <- cn[str_detect(cn, regex(p, ignore_case = TRUE))]
    if (length(hit) > 0) return(hit[1])
  }
  NA_character_
}

num_or_na <- function(df, col) {
  if (is.na(col) || !col %in% names(df)) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[col]]))
}

maybe_extract_raw_components <- function(analysis_df) {
  raw_out <- file.path(dir_dat, "Step08_raw_mimic_first24h_components.csv")
  if (file.exists(raw_out)) {
    cat("Using existing raw component extract:\n", raw_out, "\n")
    return(safe_read_any(raw_out))
  }
  if (!use_raw_t7_extract_if_needed) {
    return(tibble(stay_id = analysis_df$stay_id))
  }
  if (!dir.exists(hosp_dir) || !dir.exists(icu_dir)) {
    warning("hosp_dir or icu_dir not found. Skip raw extraction.")
    return(tibble(stay_id = analysis_df$stay_id))
  }
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    warning("duckdb unavailable. Skip raw extraction.")
    return(tibble(stay_id = analysis_df$stay_id))
  }
  icustays_file <- file.path(icu_dir, "icustays.csv.gz")
  if (!file.exists(icustays_file)) {
    warning("icustays.csv.gz not found. Skip raw extraction.")
    return(tibble(stay_id = analysis_df$stay_id))
  }

  cat("\nAttempting optional raw MIMIC-IV extraction via duckdb. This may take time.\n")

  cohort_ids <- analysis_df %>% select(stay_id) %>% distinct() %>% filter(!is.na(stay_id))
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = file.path(out_dir, "step08_raw_extract.duckdb"), read_only = FALSE)
  on.exit(try(duckdb::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  duckdb::dbWriteTable(con, "cohort_ids", cohort_ids, overwrite = TRUE)

  ## Create cohort with intime/outtime
  duckdb::dbExecute(con, sprintf("\n    CREATE OR REPLACE TABLE cohort AS\n    SELECT i.subject_id, i.hadm_id, i.stay_id, CAST(i.intime AS TIMESTAMP) AS intime, CAST(i.outtime AS TIMESTAMP) AS outtime\n    FROM read_csv_auto('%s') i\n    INNER JOIN cohort_ids c ON i.stay_id = c.stay_id\n  ", icustays_file))

  ## Selected MIMIC-IV itemids.
  ## These lists cover common MetaVision itemids; if your local labels differ, the script still uses Step05 features as fallback.
  vit_itemids <- c(
    220045,                 ## Heart Rate
    220179, 220050,         ## SBP noninvasive / arterial
    220180, 220051,         ## DBP noninvasive / arterial
    220052, 220181,         ## MAP arterial / noninvasive
    220210,                 ## Respiratory Rate
    223761, 223762,         ## Temperature F / C
    220277,                 ## SpO2
    223900, 223901, 220739, ## GCS components
    223835                  ## FiO2
  )

  chartevents_file <- file.path(icu_dir, "chartevents.csv.gz")
  if (file.exists(chartevents_file)) {
    duckdb::dbExecute(con, sprintf("\n      CREATE OR REPLACE TABLE ce_first24 AS\n      SELECT ce.stay_id, ce.itemid, ce.valuenum\n      FROM read_csv_auto('%s') ce\n      INNER JOIN cohort c ON ce.stay_id = c.stay_id\n      WHERE ce.valuenum IS NOT NULL\n        AND ce.charttime IS NOT NULL\n        AND CAST(ce.charttime AS TIMESTAMP) >= c.intime\n        AND CAST(ce.charttime AS TIMESTAMP) <= c.intime + INTERVAL 24 HOUR\n        AND ce.itemid IN (%s)\n    ", chartevents_file, paste(vit_itemids, collapse = ",")))

    duckdb::dbExecute(con, "\n      CREATE OR REPLACE TABLE ce_summary AS\n      SELECT stay_id,\n        MAX(CASE WHEN itemid = 220045 THEN valuenum END) AS raw_hr_max_24h,\n        MIN(CASE WHEN itemid IN (220179,220050) THEN valuenum END) AS raw_sbp_min_24h,\n        MIN(CASE WHEN itemid IN (220052,220181) THEN valuenum END) AS raw_map_min_24h,\n        MAX(CASE WHEN itemid = 220210 THEN valuenum END) AS raw_rr_max_24h,\n        MIN(CASE WHEN itemid = 220210 THEN valuenum END) AS raw_rr_min_24h,\n        MIN(CASE WHEN itemid = 220277 THEN valuenum END) AS raw_spo2_min_24h,\n        MAX(CASE WHEN itemid = 223835 THEN valuenum END) AS raw_fio2_max_24h,\n        MAX(CASE WHEN itemid = 223761 THEN (valuenum - 32) * 5 / 9 WHEN itemid = 223762 THEN valuenum END) AS raw_temp_max_c_24h,\n        MIN(CASE WHEN itemid = 223761 THEN (valuenum - 32) * 5 / 9 WHEN itemid = 223762 THEN valuenum END) AS raw_temp_min_c_24h,\n        MIN(CASE WHEN itemid IN (223900,223901,220739) THEN valuenum END) AS raw_gcs_component_min_24h\n      FROM ce_first24\n      GROUP BY stay_id\n    ")
  } else {
    duckdb::dbExecute(con, "CREATE OR REPLACE TABLE ce_summary AS SELECT stay_id FROM cohort")
  }

  lab_itemids <- c(
    50912, 51006, 51265, 50885, 51301, 51300, 50983, 50971, 50882, 50821, 50813, 51237
  )
  labevents_file <- file.path(hosp_dir, "labevents.csv.gz")
  if (file.exists(labevents_file)) {
    duckdb::dbExecute(con, sprintf("\n      CREATE OR REPLACE TABLE lab_first24 AS\n      SELECT c.stay_id, le.itemid, le.valuenum\n      FROM read_csv_auto('%s') le\n      INNER JOIN cohort c ON le.hadm_id = c.hadm_id\n      WHERE le.valuenum IS NOT NULL\n        AND le.charttime IS NOT NULL\n        AND CAST(le.charttime AS TIMESTAMP) >= c.intime\n        AND CAST(le.charttime AS TIMESTAMP) <= c.intime + INTERVAL 24 HOUR\n        AND le.itemid IN (%s)\n    ", labevents_file, paste(lab_itemids, collapse = ",")))

    duckdb::dbExecute(con, "\n      CREATE OR REPLACE TABLE lab_summary AS\n      SELECT stay_id,\n        MAX(CASE WHEN itemid = 50912 THEN valuenum END) AS raw_creatinine_max_24h,\n        MAX(CASE WHEN itemid = 51006 THEN valuenum END) AS raw_bun_max_24h,\n        MIN(CASE WHEN itemid = 51265 THEN valuenum END) AS raw_platelet_min_24h,\n        MAX(CASE WHEN itemid = 50885 THEN valuenum END) AS raw_bilirubin_max_24h,\n        MAX(CASE WHEN itemid IN (51301,51300) THEN valuenum END) AS raw_wbc_max_24h,\n        MAX(CASE WHEN itemid = 50983 THEN valuenum END) AS raw_sodium_max_24h,\n        MIN(CASE WHEN itemid = 50983 THEN valuenum END) AS raw_sodium_min_24h,\n        MAX(CASE WHEN itemid = 50971 THEN valuenum END) AS raw_potassium_max_24h,\n        MIN(CASE WHEN itemid = 50971 THEN valuenum END) AS raw_potassium_min_24h,\n        MIN(CASE WHEN itemid = 50882 THEN valuenum END) AS raw_bicarbonate_min_24h,\n        MIN(CASE WHEN itemid = 50821 THEN valuenum END) AS raw_pao2_min_24h,\n        MAX(CASE WHEN itemid = 50813 THEN valuenum END) AS raw_lactate_max_24h,\n        MAX(CASE WHEN itemid = 51237 THEN valuenum END) AS raw_inr_max_24h\n      FROM lab_first24\n      GROUP BY stay_id\n    ")
  } else {
    duckdb::dbExecute(con, "CREATE OR REPLACE TABLE lab_summary AS SELECT stay_id FROM cohort")
  }

  duckdb::dbExecute(con, "\n    CREATE OR REPLACE TABLE raw_summary AS\n    SELECT c.stay_id, c.subject_id, c.hadm_id,\n           ce.*, lab.* EXCLUDE(stay_id)\n    FROM cohort c\n    LEFT JOIN ce_summary ce ON c.stay_id = ce.stay_id\n    LEFT JOIN lab_summary lab ON c.stay_id = lab.stay_id\n  ")
  raw_df <- duckdb::dbGetQuery(con, "SELECT * FROM raw_summary") %>% as_tibble() %>% standardize_names()
  safe_fwrite(raw_df, raw_out)
  raw_df
}

## Only extract if major components are missing from analysis_df.
critical_patterns <- c("heart_rate|hr", "map", "gcs", "creatinine", "platelet", "bilirubin", "bun")
component_hits <- sapply(critical_patterns, function(p) any(str_detect(names(analysis_df), regex(p, ignore_case = TRUE))))
raw_components <- if (sum(component_hits) < 4) maybe_extract_raw_components(analysis_df) else tibble(stay_id = analysis_df$stay_id)

analysis_df2 <- analysis_df %>% left_join(raw_components, by = "stay_id", suffix = c("", "_raw"))

## =========================================================
## 7. Build traditional scores / proxies
## =========================================================

score_manifest <- tibble(
  score = character(),
  column = character(),
  score_type = character(),
  source = character(),
  note = character()
)

add_manifest <- function(score, column, score_type, source, note) {
  tibble(score = score, column = column, score_type = score_type, source = source, note = note)
}

score_df <- analysis_df2 %>%
  select(any_of(c("subject_id", "hadm_id", "stay_id", "split", "y", "death_30d_after_landmark"))) %>%
  distinct(stay_id, .keep_all = TRUE)

## Official / existing score columns if present
existing_score_specs <- list(
  "SOFA" = c("^sofa$", "sofa_score", "first_sofa", "sofa_24"),
  "SAPS II" = c("^sapsii$", "saps_ii", "sapsii_score", "sapsii_24"),
  "OASIS" = c("^oasis$", "oasis_score"),
  "APS III" = c("^apsiii$", "aps_iii", "apsiii_score"),
  "LODS" = c("^lods$", "lods_score"),
  "GRACE" = c("^grace$", "grace_score")
)

for (score_name in names(existing_score_specs)) {
  col <- pick_col(analysis_df2, existing_score_specs[[score_name]])
  if (!is.na(col)) {
    new_col <- clean_names(score_name)
    score_df[[new_col]] <- suppressWarnings(as.numeric(analysis_df2[[col]]))[match(score_df$stay_id, analysis_df2$stay_id)]
    score_manifest <- bind_rows(score_manifest, add_manifest(
      score = score_name,
      column = new_col,
      score_type = "Traditional score",
      source = paste0("Existing column: ", col),
      note = "Detected directly from the patient-level analysis dataset."
    ))
  }
}

## Helper to retrieve variables from analysis_df2 or raw extract
getv <- function(patterns) {
  col <- pick_col(analysis_df2, patterns)
  num_or_na(analysis_df2, col)
}

age <- getv(c("^age$", "anchor_age"))
hr_max <- getv(c("heart_rate_max", "hr_max", "raw_hr_max_24h", "heart_rate_mean", "heart_rate_first"))
sbp_min <- getv(c("sbp_min", "systolic.*min", "raw_sbp_min_24h", "sbp_first", "sbp_mean"))
map_min <- getv(c("map_min", "mean_arterial.*min", "raw_map_min_24h", "map_first", "map_mean"))
rr_max <- getv(c("resp_rate_max", "respiratory_rate_max", "rr_max", "raw_rr_max_24h", "resp_rate_mean"))
rr_min <- getv(c("resp_rate_min", "respiratory_rate_min", "rr_min", "raw_rr_min_24h"))
temp_max <- getv(c("temperature_max", "temp_max", "raw_temp_max_c_24h", "temperature_mean"))
temp_min <- getv(c("temperature_min", "temp_min", "raw_temp_min_c_24h"))
spo2_min <- getv(c("spo2_min", "oxygen_saturation_min", "raw_spo2_min_24h", "spo2_mean"))
gcs_min <- getv(c("gcs_min", "gcs_total_min", "gcs_first", "gcs_mean", "raw_gcs_component_min_24h"))
creat_max <- getv(c("creatinine_max", "creatinine", "raw_creatinine_max_24h"))
bun_max <- getv(c("bun_max", "urea", "raw_bun_max_24h"))
platelet_min <- getv(c("platelet_min", "platelets_min", "raw_platelet_min_24h", "platelet_first"))
bilirubin_max <- getv(c("bilirubin_max", "total_bilirubin_max", "raw_bilirubin_max_24h"))
wbc_max <- getv(c("wbc_max", "white_blood", "raw_wbc_max_24h"))
sodium_max <- getv(c("sodium_max", "raw_sodium_max_24h"))
sodium_min <- getv(c("sodium_min", "raw_sodium_min_24h"))
potassium_max <- getv(c("potassium_max", "raw_potassium_max_24h"))
potassium_min <- getv(c("potassium_min", "raw_potassium_min_24h"))
bicarb_min <- getv(c("bicarbonate_min", "bicarb_min", "raw_bicarbonate_min_24h"))
lactate_max <- getv(c("lactate_max", "raw_lactate_max_24h"))

## SOFA-like proxy.
## Uses first-24h or early-window components when available; it is not labelled as official SOFA.
resp_sofa <- case_when(
  is.na(spo2_min) ~ NA_real_,
  spo2_min >= 95 ~ 0,
  spo2_min >= 90 ~ 1,
  spo2_min >= 85 ~ 2,
  spo2_min >= 80 ~ 3,
  TRUE ~ 4
)
coag_sofa <- case_when(
  is.na(platelet_min) ~ NA_real_,
  platelet_min >= 150 ~ 0,
  platelet_min >= 100 ~ 1,
  platelet_min >= 50 ~ 2,
  platelet_min >= 20 ~ 3,
  TRUE ~ 4
)
liver_sofa <- case_when(
  is.na(bilirubin_max) ~ NA_real_,
  bilirubin_max < 1.2 ~ 0,
  bilirubin_max < 2.0 ~ 1,
  bilirubin_max < 6.0 ~ 2,
  bilirubin_max < 12.0 ~ 3,
  TRUE ~ 4
)
cardio_sofa <- case_when(
  !is.na(lactate_max) & lactate_max >= 4 ~ 4,
  !is.na(map_min) & map_min < 60 ~ 3,
  !is.na(map_min) & map_min < 65 ~ 2,
  !is.na(map_min) & map_min < 70 ~ 1,
  !is.na(map_min) ~ 0,
  TRUE ~ NA_real_
)
cns_sofa <- case_when(
  is.na(gcs_min) ~ NA_real_,
  gcs_min >= 15 ~ 0,
  gcs_min >= 13 ~ 1,
  gcs_min >= 10 ~ 2,
  gcs_min >= 6 ~ 3,
  TRUE ~ 4
)
renal_sofa <- case_when(
  is.na(creat_max) ~ NA_real_,
  creat_max < 1.2 ~ 0,
  creat_max < 2.0 ~ 1,
  creat_max < 3.5 ~ 2,
  creat_max < 5.0 ~ 3,
  TRUE ~ 4
)
score_df$sofa_like_proxy <- rowSums(cbind(resp_sofa, coag_sofa, liver_sofa, cardio_sofa, cns_sofa, renal_sofa), na.rm = FALSE)
if (sum(!is.na(score_df$sofa_like_proxy)) >= 100 && !"sofa" %in% names(score_df)) {
  score_manifest <- bind_rows(score_manifest, add_manifest("SOFA-like proxy", "sofa_like_proxy", "Traditional score proxy", "Constructed from early vital/lab components", "Proxy score; not official SOFA because vasopressor/ventilation components may be incomplete."))
}

## OASIS-like proxy based on major OASIS domains.
oasis_like <- rowSums(cbind(
  case_when(is.na(age) ~ NA_real_, age < 24 ~ 0, age <= 53 ~ 3, age <= 77 ~ 6, age <= 89 ~ 9, TRUE ~ 7),
  case_when(is.na(hr_max) ~ NA_real_, hr_max < 33 ~ 4, hr_max <= 88 ~ 0, hr_max <= 106 ~ 1, hr_max <= 125 ~ 3, TRUE ~ 6),
  case_when(is.na(map_min) ~ NA_real_, map_min < 20 ~ 4, map_min <= 50 ~ 3, map_min <= 143 ~ 0, TRUE ~ 3),
  case_when(is.na(rr_max) ~ NA_real_, rr_max < 6 ~ 10, rr_max <= 12 ~ 1, rr_max <= 22 ~ 0, rr_max <= 30 ~ 1, TRUE ~ 6),
  case_when(is.na(temp_min) & is.na(temp_max) ~ NA_real_, pmin(temp_min, temp_max, na.rm = TRUE) < 33.2 ~ 3, pmax(temp_min, temp_max, na.rm = TRUE) > 39.9 ~ 3, TRUE ~ 0),
  case_when(is.na(gcs_min) ~ NA_real_, gcs_min >= 14 ~ 0, gcs_min >= 8 ~ 4, TRUE ~ 10)
), na.rm = FALSE)
score_df$oasis_like_proxy <- oasis_like
if (sum(!is.na(score_df$oasis_like_proxy)) >= 100 && !"oasis" %in% names(score_df)) {
  score_manifest <- bind_rows(score_manifest, add_manifest("OASIS-like proxy", "oasis_like_proxy", "Traditional score proxy", "Constructed from early vital/GCS/age components", "Proxy score; not official OASIS because several original OASIS components may be unavailable."))
}

## SAPS II-like proxy.
sapsii_like <- rowSums(cbind(
  case_when(is.na(age) ~ NA_real_, age < 40 ~ 0, age < 60 ~ 7, age < 70 ~ 12, age < 75 ~ 15, age < 80 ~ 16, TRUE ~ 18),
  case_when(is.na(hr_max) ~ NA_real_, hr_max < 40 ~ 11, hr_max < 70 ~ 2, hr_max < 120 ~ 0, hr_max < 160 ~ 4, TRUE ~ 7),
  case_when(is.na(sbp_min) ~ NA_real_, sbp_min < 70 ~ 13, sbp_min < 100 ~ 5, sbp_min < 200 ~ 0, TRUE ~ 2),
  case_when(is.na(temp_max) ~ NA_real_, temp_max >= 39 ~ 3, TRUE ~ 0),
  case_when(is.na(gcs_min) ~ NA_real_, gcs_min >= 14 ~ 0, gcs_min >= 11 ~ 5, gcs_min >= 9 ~ 7, gcs_min >= 6 ~ 13, TRUE ~ 26),
  case_when(is.na(bun_max) ~ NA_real_, bun_max < 28 ~ 0, bun_max < 84 ~ 6, TRUE ~ 10),
  case_when(is.na(wbc_max) ~ NA_real_, wbc_max < 1 ~ 12, wbc_max < 20 ~ 0, TRUE ~ 3),
  case_when(is.na(potassium_max) | is.na(potassium_min) ~ NA_real_, potassium_min < 3 ~ 3, potassium_max >= 5 ~ 3, TRUE ~ 0),
  case_when(is.na(sodium_max) | is.na(sodium_min) ~ NA_real_, sodium_min < 125 ~ 5, sodium_max >= 145 ~ 1, TRUE ~ 0),
  case_when(is.na(bicarb_min) ~ NA_real_, bicarb_min < 15 ~ 6, bicarb_min < 20 ~ 3, TRUE ~ 0),
  case_when(is.na(bilirubin_max) ~ NA_real_, bilirubin_max < 4 ~ 0, bilirubin_max < 6 ~ 4, TRUE ~ 9)
), na.rm = FALSE)
score_df$sapsii_like_proxy <- sapsii_like
if (sum(!is.na(score_df$sapsii_like_proxy)) >= 100 && !"saps_ii" %in% names(score_df) && !"sapsii" %in% names(score_df)) {
  score_manifest <- bind_rows(score_manifest, add_manifest("SAPS II-like proxy", "sapsii_like_proxy", "Traditional score proxy", "Constructed from early vital/lab/GCS/age components", "Proxy score; not official SAPS II because all original components may not be available."))
}

## GRACE-like proxy for AMI risk context.
gr_like <- rowSums(cbind(
  case_when(is.na(age) ~ NA_real_, age < 40 ~ 0, age < 50 ~ 18, age < 60 ~ 36, age < 70 ~ 55, age < 80 ~ 73, TRUE ~ 91),
  case_when(is.na(hr_max) ~ NA_real_, hr_max < 50 ~ 0, hr_max < 70 ~ 3, hr_max < 90 ~ 9, hr_max < 110 ~ 15, hr_max < 150 ~ 24, TRUE ~ 38),
  case_when(is.na(sbp_min) ~ NA_real_, sbp_min >= 200 ~ 0, sbp_min >= 180 ~ 10, sbp_min >= 160 ~ 20, sbp_min >= 140 ~ 30, sbp_min >= 120 ~ 43, sbp_min >= 100 ~ 53, sbp_min >= 80 ~ 63, TRUE ~ 73),
  case_when(is.na(creat_max) ~ NA_real_, creat_max < 0.4 ~ 1, creat_max < 0.8 ~ 4, creat_max < 1.2 ~ 7, creat_max < 1.6 ~ 10, creat_max < 2.0 ~ 13, creat_max < 4.0 ~ 21, TRUE ~ 28),
  case_when(is.na(lactate_max) ~ 0, lactate_max >= 4 ~ 20, lactate_max >= 2 ~ 10, TRUE ~ 0)
), na.rm = FALSE)
score_df$grace_like_proxy <- gr_like
if (sum(!is.na(score_df$grace_like_proxy)) >= 100 && !"grace" %in% names(score_df)) {
  score_manifest <- bind_rows(score_manifest, add_manifest("GRACE-like proxy", "grace_like_proxy", "Traditional score proxy", "Constructed from age, HR, SBP, creatinine, and hypoperfusion proxy", "Proxy score; not official GRACE because some original GRACE variables may be unavailable."))
}

score_cols <- score_manifest$column %>% unique()
score_cols <- score_cols[score_cols %in% names(score_df)]
score_manifest <- score_manifest %>% filter(column %in% score_cols) %>% distinct(score, column, .keep_all = TRUE)

safe_fwrite(score_manifest, file.path(dir_tbl, "Step08_detected_and_constructed_scores.csv"))
safe_fwrite(score_df, file.path(dir_dat, "Step08_traditional_score_dataset.csv"))

cat("\nDetected / constructed score columns:\n")
print(score_manifest)

if (length(score_cols) == 0) {
  stop("No traditional scores or score proxies available. Please check analysis variables or enable raw extraction.")
}

## =========================================================
## 8. Calibrate traditional scores on Train and predict Test
## =========================================================

split_y <- step05_predictions %>%
  distinct(stay_id, split, y)

score_long <- score_df %>%
  select(stay_id, any_of(score_cols)) %>%
  left_join(split_y, by = "stay_id")

calibrate_one_score <- function(score_col) {
  df0 <- score_long %>%
    select(stay_id, split, y, score = all_of(score_col)) %>%
    filter(!is.na(split), !is.na(y), !is.na(score))

  score_info <- score_manifest %>% dplyr::filter(column == score_col)
  if (nrow(score_info) == 0) return(NULL)
  score_info <- score_info[1, , drop = FALSE]
  score_name <- score_info$score[[1]]
  score_type <- score_info$score_type[[1]]

  train <- df0 %>% filter(split == "Train")
  test <- df0 %>% filter(split == "Test")

  if (nrow(train) < 100 || nrow(test) < 50 || length(unique(train$y)) < 2) {
    return(NULL)
  }

  ## Ensure higher score means higher risk. If train AUROC is below 0.5, reverse direction.
  raw_auc <- safe_auc(train$y, train$score)
  if (!is.na(raw_auc) && raw_auc < 0.5) {
    train$score <- -train$score
    test$score <- -test$score
  }

  fit <- tryCatch(
    suppressWarnings(glm(y ~ score, data = train, family = binomial())),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)

  train$pred <- bound_prob(predict(fit, newdata = train, type = "response"))
  test$pred <- bound_prob(predict(fit, newdata = test, type = "response"))

  bind_rows(train, test) %>%
    transmute(
      model = score_name,
      model_group = score_type,
      split,
      stay_id,
      y,
      pred,
      raw_score = score
    )
}

benchmark_predictions <- purrr::map_dfr(score_cols, calibrate_one_score)

if (nrow(benchmark_predictions) == 0) {
  stop("Traditional score calibration produced no valid predictions. Check missingness and outcome columns.")
}

safe_fwrite(benchmark_predictions, file.path(dir_dat, "Step08_traditional_score_predictions_train_test.csv"))

## =========================================================
## 9. Combine AMI-EDRS models and traditional scores
## =========================================================

## Keep key Step05 models to avoid overcrowded figures.
preferred_step05_models <- c(
  "Elastic net: dynamic + trajectory",
  "XGBoost: dynamic + trajectory",
  "Random forest: dynamic + trajectory",
  "Organ-domain dynamic model",
  "Trajectory-enhanced dynamic model",
  "Host vulnerability",
  "Conventional clinical severity"
)

step05_keep <- step05_predictions %>%
  filter(model %in% preferred_step05_models) %>%
  transmute(model, model_group, split, stay_id, y, pred, raw_score = NA_real_)

combined_predictions <- bind_rows(step05_keep, benchmark_predictions) %>%
  mutate(
    model_group = case_when(
      model_group %in% c("Traditional score", "Traditional score proxy") ~ model_group,
      str_detect(model, regex("elastic|xgboost|random forest|dynamic|trajectory", ignore_case = TRUE)) ~ "AMI-EDRS / dynamic model",
      TRUE ~ "Other model"
    )
  )

safe_fwrite(combined_predictions, file.path(dir_dat, "Step08_combined_predictions_train_test.csv"))

## All-available performance
perf_all <- combined_predictions %>%
  group_by(model, model_group, split) %>%
  group_split() %>%
  purrr::map_dfr(function(d) {
    evaluate_predictions(d$y, d$pred, d$model[[1]], d$model_group[[1]], d$split[[1]])
  }) %>%
  arrange(split, desc(AUROC), desc(AUPRC), Brier)

safe_fwrite(perf_all, file.path(dir_tbl, "Step08_performance_all_available_samples.csv"))

## Common-sample performance: require primary model + all benchmark scores available in Test.
primary_model <- "Elastic net: dynamic + trajectory"
models_for_common <- c(primary_model, unique(benchmark_predictions$model))
common_ids_test <- combined_predictions %>%
  filter(split == "Test", model %in% models_for_common) %>%
  filter(!is.na(pred)) %>%
  count(stay_id) %>%
  filter(n == length(models_for_common)) %>%
  pull(stay_id)

perf_common <- combined_predictions %>%
  filter(stay_id %in% common_ids_test, split == "Test", model %in% models_for_common) %>%
  group_by(model, model_group, split) %>%
  group_split() %>%
  purrr::map_dfr(function(d) {
    evaluate_predictions(d$y, d$pred, d$model[[1]], d$model_group[[1]], "Test_common_sample")
  }) %>%
  arrange(desc(AUROC), desc(AUPRC), Brier)

safe_fwrite(perf_common, file.path(dir_tbl, "Step08_performance_common_sample_TEST.csv"))

## Choose plot models: primary + top ML comparators + all traditional scores/proxies.
plot_models <- c(
  "Elastic net: dynamic + trajectory",
  "XGBoost: dynamic + trajectory",
  "Random forest: dynamic + trajectory",
  "Organ-domain dynamic model",
  unique(benchmark_predictions$model)
)
plot_models <- unique(plot_models[plot_models %in% unique(combined_predictions$model)])

perf_test <- perf_all %>%
  filter(split == "Test", model %in% plot_models) %>%
  arrange(desc(AUROC), desc(AUPRC), Brier)

safe_fwrite(perf_test, file.path(dir_tbl, "Step08_performance_plot_models_TEST.csv"))

cat("\nTest performance, plot models:\n")
print(perf_test)

## =========================================================
## 10. Generate curve data
## =========================================================

pred_test_plot <- combined_predictions %>%
  filter(split == "Test", model %in% plot_models)

roc_df <- pred_test_plot %>%
  group_by(model) %>% group_split() %>%
  purrr::map_dfr(function(d) make_roc_df(d$y, d$pred, d$model[[1]])) %>%
  left_join(perf_test %>% select(model, AUROC), by = "model")

pr_df <- pred_test_plot %>%
  group_by(model) %>% group_split() %>%
  purrr::map_dfr(function(d) make_pr_df(d$y, d$pred, d$model[[1]])) %>%
  left_join(perf_test %>% select(model, AUPRC), by = "model")

cal_df <- pred_test_plot %>%
  group_by(model) %>% group_split() %>%
  purrr::map_dfr(function(d) make_calibration_df(d$y, d$pred, d$model[[1]]))

dca_df <- pred_test_plot %>%
  group_by(model) %>% group_split() %>%
  purrr::map_dfr(function(d) make_dca_df(d$y, d$pred, d$model[[1]]))

dca_ref <- dca_df %>% distinct(threshold, net_benefit_all, net_benefit_none)

safe_fwrite(roc_df, file.path(dir_tbl, "Step08_ROC_curve_data.csv"))
safe_fwrite(pr_df, file.path(dir_tbl, "Step08_PR_curve_data.csv"))
safe_fwrite(cal_df, file.path(dir_tbl, "Step08_calibration_curve_data.csv"))
safe_fwrite(dca_df, file.path(dir_tbl, "Step08_DCA_curve_data.csv"))

## =========================================================
## 11. Figures
## =========================================================

short_model_label <- function(x) {
  dplyr::recode(
    x,
    "Elastic net: dynamic + trajectory" = "AMI-EDRS\n(Elastic net)",
    "XGBoost: dynamic + trajectory" = "XGBoost\n+ trajectory",
    "Random forest: dynamic + trajectory" = "Random forest\n+ trajectory",
    "Organ-domain dynamic model" = "Dynamic\norgan-domain",
    "Trajectory-enhanced dynamic model" = "Dynamic\n+ C1-C5",
    "Host vulnerability" = "Host\nvulnerability",
    "Conventional clinical severity" = "Clinical\nseverity",
    .default = x
  )
}

plot_label_df <- perf_test %>%
  mutate(
    model_short = short_model_label(model),
    model_short = factor(model_short, levels = short_model_label(model)),
    model_group = factor(model_group, levels = c("AMI-EDRS / dynamic model", "Traditional score", "Traditional score proxy", "Other model"))
  )

## Figure A: performance bars with Brier correctly interpreted as lower is better.
perf_long <- plot_label_df %>%
  select(model, model_short, model_group, AUROC, AUPRC, Brier) %>%
  pivot_longer(cols = c(AUROC, AUPRC, Brier), names_to = "metric", values_to = "value") %>%
  mutate(
    sort_value = if_else(metric == "Brier", -value, value),
    metric_label = factor(metric, levels = c("AUPRC", "AUROC", "Brier"),
                          labels = c("AUPRC\n(higher is better)", "AUROC\n(higher is better)", "Brier score\n(lower is better)")),
    value_label = sprintf("%.3f", value),
    model_metric = paste(model_short, metric, sep = "___")
  ) %>%
  group_by(metric) %>%
  arrange(desc(sort_value), .by_group = TRUE) %>%
  mutate(order_id = row_number()) %>%
  ungroup()

model_metric_levels <- perf_long %>%
  arrange(metric, order_id) %>%
  pull(model_metric) %>% unique()

perf_long <- perf_long %>%
  mutate(model_metric = factor(model_metric, levels = rev(model_metric_levels)))

fig_perf <- ggplot(perf_long, aes(x = value, y = model_metric, fill = model_group)) +
  geom_col(width = 0.68, color = "white", linewidth = 0.25) +
  geom_text(aes(label = value_label), hjust = -0.08, size = 3.7, fontface = "bold", color = "#222222") +
  facet_wrap(~ metric_label, scales = "free_x", ncol = 3) +
  scale_y_discrete(labels = function(x) sub("___.*$", "", x)) +
  scale_fill_manual(values = pal_group, drop = FALSE) +
  labs(
    title = "Comparison with conventional clinical benchmark scores",
    subtitle = "Traditional scores were calibrated on the training set and evaluated on the test set",
    x = "Metric value",
    y = NULL,
    fill = NULL
  ) +
  make_theme_pub(base_size = 13) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold")) +
  coord_cartesian(clip = "off")

save_pub_plot(fig_perf, "Step08_FigureS_Benchmark_Performance_vs_TraditionalScores", width = 13.8, height = 8.0)

curve_palette <- c(
  "Elastic net: dynamic + trajectory" = pal_gold,
  "XGBoost: dynamic + trajectory" = pal_purple,
  "Random forest: dynamic + trajectory" = pal_orange,
  "Organ-domain dynamic model" = pal_blue,
  "Trajectory-enhanced dynamic model" = "#13974A",
  "Host vulnerability" = "#8B9092",
  "Conventional clinical severity" = "#90B2CC"
)
extra_scores <- setdiff(plot_models, names(curve_palette))
if (length(extra_scores) > 0) {
  extra_cols <- c("#7A5D98", "#4A7494", "#A66E38", "#5F8D4E", "#B35C7E", "#6D6A75")
  names(extra_cols) <- extra_scores
  curve_palette <- c(curve_palette, extra_cols)
}
curve_palette <- curve_palette[plot_models]

## Figure B: ROC
roc_plot_df <- roc_df %>%
  mutate(model_label = paste0(short_model_label(model), " (AUROC=", sprintf("%.3f", AUROC), ")"))
roc_label_order <- perf_test %>%
  filter(model %in% plot_models) %>%
  mutate(model_label = paste0(short_model_label(model), " (AUROC=", sprintf("%.3f", AUROC), ")")) %>%
  pull(model_label)
roc_plot_df$model_label <- factor(roc_plot_df$model_label, levels = roc_label_order)

fig_roc <- ggplot(roc_plot_df, aes(x = fpr, y = sensitivity, color = model_label)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey60") +
  geom_line(linewidth = 1.15) +
  scale_color_manual(values = curve_palette[match(names(curve_palette), plot_models)], drop = FALSE) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  labs(title = "ROC comparison with conventional benchmark scores", x = "1 - Specificity", y = "Sensitivity", color = NULL) +
  make_theme_pub(base_size = 13) +
  theme(legend.position = "bottom", legend.text = element_text(size = 10))

save_pub_plot(fig_roc, "Step08_FigureS_Benchmark_ROC_vs_TraditionalScores", width = 10.5, height = 8.0)

## Figure C: PR
pr_plot_df <- pr_df %>%
  mutate(model_label = paste0(short_model_label(model), " (AUPRC=", sprintf("%.3f", AUPRC), ")"))
pr_label_order <- perf_test %>%
  filter(model %in% plot_models) %>%
  mutate(model_label = paste0(short_model_label(model), " (AUPRC=", sprintf("%.3f", AUPRC), ")")) %>%
  pull(model_label)
pr_plot_df$model_label <- factor(pr_plot_df$model_label, levels = pr_label_order)

fig_pr <- ggplot(pr_plot_df, aes(x = recall, y = precision, color = model_label)) +
  geom_line(linewidth = 1.15) +
  scale_color_manual(values = curve_palette[match(names(curve_palette), plot_models)], drop = FALSE) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = "Precision-recall comparison with conventional benchmark scores", x = "Recall", y = "Precision", color = NULL) +
  make_theme_pub(base_size = 13) +
  theme(legend.position = "bottom", legend.text = element_text(size = 10))

save_pub_plot(fig_pr, "Step08_FigureS_Benchmark_PR_vs_TraditionalScores", width = 10.5, height = 8.0)

## Figure D: Calibration
cal_plot_df <- cal_df %>%
  mutate(model_short = short_model_label(model), model_short = factor(model_short, levels = short_model_label(plot_models)))
fig_cal <- ggplot(cal_plot_df, aes(x = predicted, y = observed, color = model_short, fill = model_short)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey55") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.012, alpha = 0.55, linewidth = 0.5) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 3.0, alpha = 0.88) +
  scale_color_manual(values = curve_palette, drop = FALSE) +
  scale_fill_manual(values = curve_palette, drop = FALSE) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Calibration comparison with conventional benchmark scores", x = "Mean predicted risk", y = "Observed 30-day mortality", color = NULL, fill = NULL) +
  make_theme_pub(base_size = 13) +
  theme(legend.position = "bottom")

save_pub_plot(fig_cal, "Step08_FigureS_Benchmark_Calibration_vs_TraditionalScores", width = 10.8, height = 7.8)

## Figure E: DCA
plot_dca_df <- dca_df %>%
  mutate(model_short = short_model_label(model), model_short = factor(model_short, levels = short_model_label(plot_models)))
fig_dca <- ggplot() +
  geom_line(data = dca_ref, aes(x = threshold, y = net_benefit_all), linetype = 2, linewidth = 0.9, color = pal_blue) +
  geom_line(data = dca_ref, aes(x = threshold, y = net_benefit_none), linetype = 3, linewidth = 0.9, color = "grey45") +
  geom_line(data = plot_dca_df, aes(x = threshold, y = net_benefit, color = model_short), linewidth = 1.05) +
  scale_color_manual(values = curve_palette, drop = FALSE) +
  coord_cartesian(xlim = c(0.01, 0.50)) +
  labs(title = "Decision curve comparison with conventional benchmark scores", subtitle = "Dashed blue line = treat-all; dotted gray line = treat-none", x = "Risk threshold", y = "Net benefit", color = NULL) +
  make_theme_pub(base_size = 13) +
  theme(legend.position = "bottom")

save_pub_plot(fig_dca, "Step08_FigureS_Benchmark_DCA_vs_TraditionalScores", width = 11.4, height = 7.8)

## Combined figure
fig_combined <- (fig_perf / (fig_roc | fig_pr) / (fig_cal | fig_dca)) +
  plot_layout(heights = c(1.05, 1, 1))
save_pub_plot(fig_combined, "Step08_FigureS_Combined_Benchmark_TraditionalScores", width = 18, height = 20)

## =========================================================
## 12. Workbook and notes
## =========================================================

method_note <- tibble::tribble(
  ~item, ~description,
  "Purpose", "Compare the AMI-EDRS / dynamic prediction models with available conventional clinical severity scores and clearly labelled score proxies.",
  "Primary comparison", "Elastic net dynamic + trajectory model versus traditional score benchmarks on the Step05 test set.",
  "Prediction outcome", "30-day mortality after the T48 landmark.",
  "Traditional score handling", "Existing score columns were used when available. Otherwise, score-like proxies were constructed from early-window vital signs and laboratory components and labelled as proxies.",
  "Calibration", "Each traditional score was converted to predicted probability using logistic calibration learned in the Step05 training set and evaluated in the Step05 test set.",
  "Leakage prevention", "No T48-after information is used. Benchmark score calibration is trained on Train and assessed on Test. Raw first-24h components are extracted relative to ICU intime when needed.",
  "Common sample", "A common-sample performance table is generated for patients with complete primary AMI-EDRS and benchmark score predictions."
)

wb <- openxlsx::createWorkbook()
add_sheet(wb, "00_method_note", method_note)
add_sheet(wb, "01_score_manifest", score_manifest)
add_sheet(wb, "02_all_available_perf", perf_all)
add_sheet(wb, "03_test_perf_plot_models", perf_test)
add_sheet(wb, "04_common_sample_perf", perf_common)
add_sheet(wb, "05_score_missingness", score_df %>% summarise(across(all_of(score_cols), ~ mean(is.na(.x)) * 100)) %>% pivot_longer(everything(), names_to = "score_column", values_to = "missing_pct"))
add_sheet(wb, "06_prediction_inventory", tibble(path = pred_obj$path, sheet = pred_obj$sheet, n_rows = nrow(step05_predictions), n_cols = ncol(step05_predictions)))
add_sheet(wb, "07_analysis_inventory", tibble(path = analysis_path, n_rows = nrow(analysis_df), n_cols = ncol(analysis_df)))
add_sheet(wb, "08_benchmark_predictions", head(benchmark_predictions, 5000))
add_sheet(wb, "09_combined_predictions", head(combined_predictions, 5000))
openxlsx::saveWorkbook(wb, file.path(dir_tbl, "Step08_Benchmark_TraditionalScores_summary.xlsx"), overwrite = TRUE)

saveRDS(list(
  step05_predictions = step05_predictions,
  analysis_df = analysis_df2,
  score_manifest = score_manifest,
  score_df = score_df,
  benchmark_predictions = benchmark_predictions,
  combined_predictions = combined_predictions,
  perf_all = perf_all,
  perf_test = perf_test,
  perf_common = perf_common,
  roc_df = roc_df,
  pr_df = pr_df,
  cal_df = cal_df,
  dca_df = dca_df
), file.path(out_dir, "Step08_Benchmark_TraditionalScores_objects.rds"))

cat("\n============================================================\n")
cat("Step08 benchmark-only analysis finished successfully.\n")
cat("============================================================\n")
cat("Summary workbook:\n", file.path(dir_tbl, "Step08_Benchmark_TraditionalScores_summary.xlsx"), "\n")
cat("Figures folder:\n", dir_fig, "\n")
cat("Log file:\n", log_file, "\n\n")

cat("Detected / constructed scores:\n")
print(score_manifest)

cat("\nTest performance:\n")
print(perf_test)

sink()
