# HISTORICAL PROVENANCE ONLY. Not the current AMI-EDRS model. See docs/REPRODUCIBILITY.md.
############################################################
## Step 03-final: Fixed k=5 organ-domain trajectory phenotyping
## Project: AMI Dynamic Risk Evolution
##
## Final decision:
## - Main manuscript solution: k = 5
## - Rationale: k=5 balances clinical interpretability, mortality gradient,
##   class size, and distinct organ-domain evolution.
## - k=2 is conservative but too close to low/high risk dichotomy.
## - k=6 creates additional fragmentation with overlapping patterns and is
##   better kept as sensitivity/candidate evidence rather than the main solution.
##
## Outputs:
## <AMI_MIMIC_PROJECT>/
##   07_step03_final_k5_trajectory_phenotyping/
##
## Main figures:
## Figure 3A. k-selection diagnostics with final k=5 highlighted
## Figure 3B. PCA structure of final k=5 phenotypes
## Figure 3C. Early organ-domain trajectories across final k=5 phenotypes
## Figure 3D. Soft heatmap of organ-domain profiles
## Figure 3E. 30-day mortality after T48 by phenotype
## Figure 3F. Kaplan-Meier survival curves by phenotype
############################################################

rm(list = ls())
gc()

options(stringsAsFactors = FALSE)

## =========================================================
## 0. Packages
## =========================================================

pkg_needed <- c(
  "dplyr", "tidyr", "stringr", "purrr", "tibble", "data.table",
  "ggplot2", "scales", "openxlsx", "cluster", "mclust",
  "survival", "ggrepel", "forcats"
)

for (p in pkg_needed) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

invisible(lapply(pkg_needed, library, character.only = TRUE))

## =========================================================
## 1. Paths
## =========================================================

project_root <- Sys.getenv("AMI_MIMIC_PROJECT", unset=file.path(getwd(), "work/MIMIC"))
dir_03_dataset <- file.path(project_root, "03_analysis_dataset")

dir_step03 <- file.path(project_root, "07_step03_final_k5_trajectory_phenotyping")
dir_step03_fig <- file.path(dir_step03, "figures")
dir_step03_tbl <- file.path(dir_step03, "tables")
dir_step03_data <- file.path(dir_step03, "trajectory_datasets")
dir_step03_obj <- file.path(dir_step03, "objects")
dir_step03_logs <- file.path(dir_step03, "logs")

dir.create(dir_step03, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_step03_fig, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_step03_tbl, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_step03_data, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_step03_obj, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_step03_logs, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

log_file <- file.path(
  dir_step03_logs,
  paste0("Step03_final_k5_trajectory_phenotyping_", timestamp, ".log")
)

input_t0  <- file.path(dir_03_dataset, "Step02_AMI_T0_dataset.csv")
input_t24 <- file.path(dir_03_dataset, "Step02_AMI_T24_dataset.csv")
input_t48 <- file.path(dir_03_dataset, "Step02_AMI_T48_dataset.csv")
input_t72 <- file.path(dir_03_dataset, "Step02_AMI_T72_dataset.csv")

out_xlsx <- file.path(
  dir_step03_tbl,
  "Step03_final_k5_trajectory_phenotyping_summary.xlsx"
)

out_labels_csv <- file.path(
  dir_step03_data,
  "Step03_final_k5_T48_trajectory_labels.csv"
)

out_module_scores_csv <- file.path(
  dir_step03_data,
  "Step03_final_k5_organ_domain_scores_long.csv"
)

out_cluster_dataset_csv <- file.path(
  dir_step03_data,
  "Step03_final_k5_trajectory_clustering_dataset.csv"
)

out_object_rds <- file.path(
  dir_step03_obj,
  "Step03_final_k5_trajectory_model_object.rds"
)

## =========================================================
## 2. Colors and theme
## =========================================================

pal_gold <- "#F2B85B"
pal_blue <- "#4A7494"

## Final k=5 colors: clear but not overly harsh
pal_final <- c(
  "C1" = "#4A7494",  # gray-blue
  "C2" = "#F2B85B",  # warm gold
  "C3" = "#7A5D98",  # muted purple
  "C4" = "#13974A",  # green
  "C5" = "#EC7725"   # orange
)

## Soft heatmap palette
pal_heat_soft <- c(
  "#EAF2F8",
  "#F4F6F8",
  "#FDF5E6",
  "#F2B85B",
  "#D98C72"
)

pal_gray <- "#E3E8EC"
pal_dark <- "#1F2933"
pal_red <- "#DA1F2E"

theme_ami <- function(base_size = 16) {
  theme_minimal(base_size = base_size) +
    theme(
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
      plot.title = element_text(face = "bold", size = base_size + 4, color = "black", hjust = 0),
      plot.subtitle = element_text(size = base_size, color = "grey30", hjust = 0),
      plot.caption = element_text(size = base_size - 3, color = "grey40"),
      axis.line = element_line(color = "black", linewidth = 0.35),
      plot.margin = margin(12, 18, 12, 18)
    )
}

save_plot <- function(plot_obj, filename_stub, width = 9.5, height = 6.5) {
  ggsave(
    filename = file.path(dir_step03_fig, paste0(filename_stub, ".pdf")),
    plot = plot_obj,
    width = width,
    height = height,
    device = cairo_pdf
  )
  ggsave(
    filename = file.path(dir_step03_fig, paste0(filename_stub, ".png")),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = 600
  )
}

## =========================================================
## 3. Logging and input checks
## =========================================================

sink(log_file, split = TRUE)

cat("\n============================================================\n")
cat("Step03-final: fixed k=5 organ-domain trajectory phenotyping\n")
cat("Time:", as.character(Sys.time()), "\n")
cat("Project root:", project_root, "\n")
cat("Output folder:", dir_step03, "\n")
cat("============================================================\n\n")

required_inputs <- c(input_t0, input_t24, input_t48, input_t72)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop("Missing Step02 landmark dataset(s):\n", paste(missing_inputs, collapse = "\n"))
}

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

to_numeric_safe <- function(x) {
  suppressWarnings(as.numeric(x))
}

select_existing <- function(df, vars) {
  vars[vars %in% names(df)]
}

pretty_p <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

safe_rescale01 <- function(x) {
  x <- as.numeric(x)
  out <- rep(NA_real_, length(x))
  idx <- which(!is.na(x) & is.finite(x))
  if (length(idx) == 0) return(rep(0, length(x)))
  rng <- range(x[idx], na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || abs(diff(rng)) < 1e-12) {
    out[idx] <- 0.5
  } else {
    out[idx] <- scales::rescale(x[idx], to = c(0, 1), from = rng)
  }
  out[is.na(out)] <- 0
  out
}

score_component <- function(x, direction = c("high", "low")) {
  direction <- match.arg(direction)
  x <- to_numeric_safe(x)
  out <- rep(NA_real_, length(x))
  idx <- which(!is.na(x) & is.finite(x))
  if (length(idx) < 10) return(out)

  xx <- x[idx]
  if (length(unique(xx)) <= 1) {
    out[idx] <- 50
    return(out)
  }

  r <- rank(xx, ties.method = "average")
  pct <- (r - 1) / (length(xx) - 1) * 100

  if (direction == "high") {
    out[idx] <- pct
  } else {
    out[idx] <- 100 - pct
  }
  out
}

row_mean_min_available <- function(mat, min_available = 1) {
  if (is.null(mat) || ncol(mat) == 0) return(rep(NA_real_, 0))
  n_avail <- rowSums(!is.na(mat))
  out <- rowMeans(mat, na.rm = TRUE)
  out[n_avail < min_available] <- NA_real_
  out
}

standardize_matrix <- function(df, feature_cols) {
  base_df <- as.data.frame(df)

  feature_cols <- unique(feature_cols)
  feature_cols <- feature_cols[feature_cols %in% names(base_df)]

  if (length(feature_cols) == 0) {
    stop("No valid clustering features found.")
  }

  x <- base_df[, feature_cols, drop = FALSE]
  for (v in names(x)) x[[v]] <- to_numeric_safe(x[[v]])

  med <- purrr::map_dbl(x, ~ {
    m <- median(.x, na.rm = TRUE)
    if (!is.finite(m)) m <- 0
    m
  })

  for (v in names(x)) {
    x[[v]][is.na(x[[v]])] <- med[[v]]
  }

  sds <- purrr::map_dbl(x, sd, na.rm = TRUE)
  keep <- names(sds)[is.finite(sds) & sds > 0]

  if (length(keep) == 0) {
    stop("All clustering features have zero variance after imputation.")
  }

  x <- x[, keep, drop = FALSE]
  scaled <- scale(as.matrix(x))

  list(
    matrix = scaled,
    medians = med,
    kept_features = keep,
    center = attr(scaled, "scaled:center"),
    scale = attr(scaled, "scaled:scale")
  )
}

bootstrap_kmeans_stability <- function(x_scaled, full_cluster, k, n_boot = 60, sample_frac = 0.80, seed = 20260601) {
  set.seed(seed + k)
  n <- nrow(x_scaled)
  ari <- rep(NA_real_, n_boot)

  for (b in seq_len(n_boot)) {
    idx <- sort(sample(seq_len(n), size = floor(n * sample_frac), replace = FALSE))
    km_b <- tryCatch(
      kmeans(x_scaled[idx, , drop = FALSE], centers = k, nstart = 30, iter.max = 100),
      error = function(e) NULL
    )
    if (is.null(km_b)) next
    ari[b] <- mclust::adjustedRandIndex(full_cluster[idx], km_b$cluster)
  }

  tibble::tibble(
    k = k,
    bootstrap_ARI_mean = mean(ari, na.rm = TRUE),
    bootstrap_ARI_sd = sd(ari, na.rm = TRUE),
    bootstrap_ARI_median = median(ari, na.rm = TRUE),
    n_boot_success = sum(!is.na(ari))
  )
}

extract_survfit_df <- function(fit) {
  ss <- summary(fit)
  if (length(ss$time) == 0) return(tibble())
  tibble::tibble(
    time = ss$time,
    surv = ss$surv,
    lower = ss$lower,
    upper = ss$upper,
    strata = ss$strata
  ) %>%
    mutate(
      trajectory = stringr::str_replace(as.character(strata), "^trajectory=", "")
    )
}

## =========================================================
## 5. Read Step02 landmark datasets
## =========================================================

t0  <- data.table::fread(input_t0)
t24 <- data.table::fread(input_t24)
t48 <- data.table::fread(input_t48)
t72 <- data.table::fread(input_t72)

cat("Loaded Step02 datasets:\n")
cat("T0 rows:", nrow(t0), " cols:", ncol(t0), "\n")
cat("T24 rows:", nrow(t24), " cols:", ncol(t24), "\n")
cat("T48 rows:", nrow(t48), " cols:", ncol(t48), "\n")
cat("T72 rows:", nrow(t72), " cols:", ncol(t72), "\n\n")

if (!"stay_id" %in% names(t48)) stop("stay_id not found in T48 dataset.")

primary_outcome <- "death_30d_after_landmark"
if (!primary_outcome %in% names(t48)) {
  stop("Primary outcome not found in T48 dataset: ", primary_outcome)
}

## =========================================================
## 6. Organ-domain score specification
## =========================================================

component_spec <- tibble::tribble(
  ~module, ~module_pretty, ~variable, ~stat, ~direction, ~component_label,

  "hemodynamic", "Hemodynamic instability", "heart_rate", "max_value", "high", "Peak heart rate",
  "hemodynamic", "Hemodynamic instability", "heart_rate", "abnormal_ratio", "high", "Heart-rate abnormal burden",
  "hemodynamic", "Hemodynamic instability", "sbp", "min_value", "low", "Lowest SBP",
  "hemodynamic", "Hemodynamic instability", "sbp", "abnormal_ratio", "high", "SBP abnormal burden",
  "hemodynamic", "Hemodynamic instability", "map", "min_value", "low", "Lowest MAP",
  "hemodynamic", "Hemodynamic instability", "map", "abnormal_ratio", "high", "MAP abnormal burden",

  "respiratory_neurologic", "Respiratory-neurologic stress", "resp_rate", "max_value", "high", "Peak respiratory rate",
  "respiratory_neurologic", "Respiratory-neurologic stress", "resp_rate", "abnormal_ratio", "high", "Respiratory-rate abnormal burden",
  "respiratory_neurologic", "Respiratory-neurologic stress", "spo2", "min_value", "low", "Lowest SpO2",
  "respiratory_neurologic", "Respiratory-neurologic stress", "spo2", "abnormal_ratio", "high", "SpO2 abnormal burden",
  "respiratory_neurologic", "Respiratory-neurologic stress", "temperature", "abnormal_ratio", "high", "Temperature abnormal burden",
  "respiratory_neurologic", "Respiratory-neurologic stress", "gcs_eye", "min_value", "low", "Lowest GCS eye",
  "respiratory_neurologic", "Respiratory-neurologic stress", "gcs_verbal", "min_value", "low", "Lowest GCS verbal",
  "respiratory_neurologic", "Respiratory-neurologic stress", "gcs_motor", "min_value", "low", "Lowest GCS motor",

  "renal_metabolic", "Renal-metabolic dysfunction", "creatinine", "max_value", "high", "Maximum creatinine",
  "renal_metabolic", "Renal-metabolic dysfunction", "bun", "max_value", "high", "Maximum BUN",
  "renal_metabolic", "Renal-metabolic dysfunction", "sodium", "abnormal_ratio", "high", "Sodium abnormal burden",
  "renal_metabolic", "Renal-metabolic dysfunction", "potassium", "abnormal_ratio", "high", "Potassium abnormal burden",
  "renal_metabolic", "Renal-metabolic dysfunction", "chloride", "abnormal_ratio", "high", "Chloride abnormal burden",
  "renal_metabolic", "Renal-metabolic dysfunction", "bicarbonate", "min_value", "low", "Lowest bicarbonate",
  "renal_metabolic", "Renal-metabolic dysfunction", "bicarbonate", "abnormal_ratio", "high", "Bicarbonate abnormal burden",
  "renal_metabolic", "Renal-metabolic dysfunction", "glucose", "abnormal_ratio", "high", "Glucose abnormal burden",

  "inflammatory_coagulopathic", "Inflammatory-coagulopathic response", "wbc", "max_value", "high", "Maximum WBC",
  "inflammatory_coagulopathic", "Inflammatory-coagulopathic response", "wbc", "abnormal_ratio", "high", "WBC abnormal burden",
  "inflammatory_coagulopathic", "Inflammatory-coagulopathic response", "hemoglobin", "min_value", "low", "Lowest hemoglobin",
  "inflammatory_coagulopathic", "Inflammatory-coagulopathic response", "platelet", "min_value", "low", "Lowest platelet",
  "inflammatory_coagulopathic", "Inflammatory-coagulopathic response", "platelet", "abnormal_ratio", "high", "Platelet abnormal burden",
  "inflammatory_coagulopathic", "Inflammatory-coagulopathic response", "inr", "max_value", "high", "Maximum INR",
  "inflammatory_coagulopathic", "Inflammatory-coagulopathic response", "ptt", "max_value", "high", "Maximum PTT",
  "inflammatory_coagulopathic", "Inflammatory-coagulopathic response", "ptt", "abnormal_ratio", "high", "PTT abnormal burden",

  "cardiac_hypoperfusion", "Cardiac injury / hypoperfusion", "lactate", "max_value", "high", "Maximum lactate",
  "cardiac_hypoperfusion", "Cardiac injury / hypoperfusion", "lactate", "abnormal_ratio", "high", "Lactate abnormal burden",
  "cardiac_hypoperfusion", "Cardiac injury / hypoperfusion", "troponin_t", "max_value", "high", "Maximum troponin T",
  "cardiac_hypoperfusion", "Cardiac injury / hypoperfusion", "troponin_t", "abnormal_ratio", "high", "Troponin T abnormal burden"
) %>%
  mutate(column = paste0(variable, "_", stat))

main_modules <- c(
  "hemodynamic",
  "respiratory_neurologic",
  "renal_metabolic",
  "inflammatory_coagulopathic"
)

extended_modules <- c(main_modules, "cardiac_hypoperfusion")

module_pretty_map <- component_spec %>%
  distinct(module, module_pretty)

module_short_map <- tibble::tribble(
  ~module_pretty, ~module_short,
  "Hemodynamic instability", "Hemodynamic",
  "Respiratory-neurologic stress", "Resp-neuro",
  "Renal-metabolic dysfunction", "Renal-metabolic",
  "Inflammatory-coagulopathic response", "Inflam-coag",
  "Cardiac injury / hypoperfusion", "Cardiac-hypoperfusion"
)

## =========================================================
## 7. Construct module scores at each landmark
## =========================================================

compute_module_scores <- function(df, landmark_label) {

  df <- as.data.frame(df)

  component_scores <- list()
  component_avail <- list()

  for (i in seq_len(nrow(component_spec))) {
    col_i <- component_spec$column[i]
    comp_name <- paste0(component_spec$module[i], "__", component_spec$variable[i], "__", component_spec$stat[i])

    if (col_i %in% names(df)) {
      component_scores[[comp_name]] <- score_component(df[[col_i]], direction = component_spec$direction[i])
      component_avail[[comp_name]] <- !is.na(df[[col_i]])
    } else {
      component_scores[[comp_name]] <- rep(NA_real_, nrow(df))
      component_avail[[comp_name]] <- rep(FALSE, nrow(df))
    }
  }

  component_score_df <- as_tibble(component_scores)
  component_avail_df <- as_tibble(component_avail)

  score_out <- tibble::tibble(stay_id = df$stay_id)
  module_availability_rows <- list()

  for (m in unique(component_spec$module)) {
    comp_cols <- names(component_score_df)[stringr::str_detect(names(component_score_df), paste0("^", m, "__"))]
    mat_score <- as.matrix(component_score_df[, comp_cols, drop = FALSE])
    mat_avail <- as.matrix(component_avail_df[, comp_cols, drop = FALSE])

    score_out[[paste0(m, "_score")]] <- row_mean_min_available(mat_score, min_available = 1)
    score_out[[paste0(m, "_component_available_n")]] <- rowSums(mat_avail, na.rm = TRUE)
    score_out[[paste0(m, "_component_available_frac")]] <- rowMeans(mat_avail, na.rm = TRUE)

    module_availability_rows[[m]] <- tibble::tibble(
      landmark = landmark_label,
      module = m,
      module_pretty = module_pretty_map$module_pretty[match(m, module_pretty_map$module)],
      n_patients = nrow(df),
      n_with_module_score = sum(!is.na(score_out[[paste0(m, "_score")]])),
      module_score_availability_pct = mean(!is.na(score_out[[paste0(m, "_score")]])) * 100,
      mean_component_available_frac = mean(score_out[[paste0(m, "_component_available_frac")]], na.rm = TRUE) * 100
    )
  }

  component_availability <- component_spec %>%
    mutate(
      landmark = landmark_label,
      present_in_dataset = column %in% names(df),
      n_nonmissing = purrr::map_int(column, ~ if (.x %in% names(df)) sum(!is.na(df[[.x]])) else 0),
      availability_pct = n_nonmissing / nrow(df) * 100
    )

  score_long <- score_out %>%
    select(stay_id, ends_with("_score")) %>%
    pivot_longer(
      cols = ends_with("_score"),
      names_to = "module",
      values_to = "module_score"
    ) %>%
    mutate(
      module = stringr::str_replace(module, "_score$", ""),
      landmark = landmark_label,
      module_pretty = module_pretty_map$module_pretty[match(module, module_pretty_map$module)]
    ) %>%
    left_join(module_short_map, by = "module_pretty")

  list(
    score_wide = score_out,
    score_long = score_long,
    module_availability = bind_rows(module_availability_rows),
    component_availability = component_availability
  )
}

score_t0  <- compute_module_scores(t0, "T0")
score_t24 <- compute_module_scores(t24, "T24")
score_t48 <- compute_module_scores(t48, "T48")
score_t72 <- compute_module_scores(t72, "T72")

module_scores_long <- bind_rows(
  score_t0$score_long,
  score_t24$score_long,
  score_t48$score_long,
  score_t72$score_long
)

module_availability <- bind_rows(
  score_t0$module_availability,
  score_t24$module_availability,
  score_t48$module_availability,
  score_t72$module_availability
)

component_availability <- bind_rows(
  score_t0$component_availability,
  score_t24$component_availability,
  score_t48$component_availability,
  score_t72$component_availability
)

safe_fwrite(module_scores_long, out_module_scores_csv)
safe_fwrite(module_availability, file.path(dir_step03_tbl, "Step03_final_k5_module_score_availability.csv"))
safe_fwrite(component_availability, file.path(dir_step03_tbl, "Step03_final_k5_component_availability.csv"))

cat("Organ-domain module scores constructed.\n\n")

## =========================================================
## 8. Build clustering dataset
## =========================================================
## Main clustering uses patients alive at T48.
## T72 is retained descriptively but not used for main clustering.

score_wide_for_join <- module_scores_long %>%
  filter(landmark %in% c("T0", "T24", "T48")) %>%
  mutate(name = paste0(module, "_", landmark)) %>%
  select(stay_id, name, module_score) %>%
  pivot_wider(id_cols = stay_id, names_from = name, values_from = module_score)

metadata_vars <- c(
  "stay_id", "subject_id", "hadm_id", "age", "gender", "race",
  primary_outcome, "mortality_30d_from_icu", "days_from_landmark_to_death",
  "hospital_expire_flag", "icu_expire_flag", "icu_los_days",
  "hospital_los_days", "in_icu_at_landmark"
)

cluster_df <- t48 %>%
  select(any_of(metadata_vars)) %>%
  left_join(score_wide_for_join, by = "stay_id")

for (m in extended_modules) {
  c0 <- paste0(m, "_T0")
  c24 <- paste0(m, "_T24")
  c48 <- paste0(m, "_T48")

  if (all(c(c0, c24, c48) %in% names(cluster_df))) {
    cluster_df[[paste0(m, "_delta_T48_T0")]] <- cluster_df[[c48]] - cluster_df[[c0]]
    cluster_df[[paste0(m, "_delta_T48_T24")]] <- cluster_df[[c48]] - cluster_df[[c24]]
    cluster_df[[paste0(m, "_slope_T0_T48")]] <- (cluster_df[[c48]] - cluster_df[[c0]]) / 48
  }
}

main_cluster_features <- c(
  unlist(lapply(main_modules, function(m) paste0(m, "_", c("T0", "T24", "T48")))),
  unlist(lapply(main_modules, function(m) paste0(m, "_delta_T48_T0"))),
  unlist(lapply(main_modules, function(m) paste0(m, "_delta_T48_T24"))),
  unlist(lapply(main_modules, function(m) paste0(m, "_slope_T0_T48")))
)

main_cluster_features <- select_existing(cluster_df, main_cluster_features)

cluster_df <- cluster_df %>%
  mutate(death_30d_after_landmark = as.integer(.data[[primary_outcome]])) %>%
  filter(!is.na(death_30d_after_landmark))

prep_cluster <- standardize_matrix(cluster_df, main_cluster_features)
x_scaled <- prep_cluster$matrix

cat("Main clustering matrix:\n")
cat("Rows:", nrow(x_scaled), "\n")
cat("Features:", ncol(x_scaled), "\n\n")

cluster_dataset_out <- cluster_df %>%
  select(stay_id, subject_id, hadm_id, all_of(primary_outcome), mortality_30d_from_icu, all_of(main_cluster_features))

safe_fwrite(cluster_dataset_out, out_cluster_dataset_csv)

## =========================================================
## 9. k=2 to 6 metrics for selection figure
## =========================================================

k_range <- 2:6
fixed_k <- 5
set.seed(20260601)

cluster_results <- list()
cluster_metrics <- list()
stability_results <- list()
dist_x <- dist(x_scaled)

for (k in k_range) {
  cat("Running k-means for k =", k, "\n")

  km <- kmeans(x_scaled, centers = k, nstart = 100, iter.max = 200)
  sil <- cluster::silhouette(km$cluster, dist_x)

  class_tab <- table(km$cluster)
  min_class_n <- min(class_tab)
  min_class_pct <- min_class_n / length(km$cluster) * 100

  tmp <- cluster_df %>%
    mutate(cluster_raw = km$cluster) %>%
    group_by(cluster_raw) %>%
    summarise(
      n = n(),
      events = sum(death_30d_after_landmark == 1, na.rm = TRUE),
      mortality_pct = mean(death_30d_after_landmark == 1, na.rm = TRUE) * 100,
      .groups = "drop"
    )

  mortality_range <- max(tmp$mortality_pct, na.rm = TRUE) - min(tmp$mortality_pct, na.rm = TRUE)

  p_chi <- tryCatch({
    chisq.test(table(km$cluster, cluster_df$death_30d_after_landmark))$p.value
  }, error = function(e) NA_real_)

  stability <- bootstrap_kmeans_stability(
    x_scaled = x_scaled,
    full_cluster = km$cluster,
    k = k,
    n_boot = 60,
    sample_frac = 0.80,
    seed = 20260601
  )

  cluster_results[[as.character(k)]] <- km
  stability_results[[as.character(k)]] <- stability

  cluster_metrics[[as.character(k)]] <- tibble::tibble(
    k = k,
    total_withinss = km$tot.withinss,
    avg_silhouette = mean(sil[, "sil_width"], na.rm = TRUE),
    min_class_n = as.integer(min_class_n),
    min_class_pct = min_class_pct,
    mortality_range_pct = mortality_range,
    mortality_chisq_p = p_chi
  )
}

cluster_metrics_df <- bind_rows(cluster_metrics)
stability_df <- bind_rows(stability_results)

gmm_bic_df <- tryCatch({
  cat("Running Gaussian mixture model BIC comparison...\n")
  bic_obj <- mclust::mclustBIC(x_scaled, G = k_range, verbose = FALSE)
  bic_mat <- as.matrix(bic_obj)
  rn <- suppressWarnings(as.integer(rownames(bic_mat)))
  cn <- suppressWarnings(as.integer(colnames(bic_mat)))

  if (!is.null(rn) && all(k_range %in% rn)) {
    tibble::tibble(
      k = k_range,
      gmm_BIC = purrr::map_dbl(k_range, function(kk) {
        max(bic_mat[which(rn == kk), , drop = TRUE], na.rm = TRUE)
      })
    )
  } else if (!is.null(cn) && all(k_range %in% cn)) {
    tibble::tibble(
      k = k_range,
      gmm_BIC = purrr::map_dbl(k_range, function(kk) {
        max(bic_mat[, which(cn == kk), drop = TRUE], na.rm = TRUE)
      })
    )
  } else {
    tibble::tibble(k = k_range, gmm_BIC = NA_real_)
  }
}, error = function(e) {
  message("Mclust failed: ", e$message)
  tibble::tibble(k = k_range, gmm_BIC = NA_real_)
})

selection_metrics <- cluster_metrics_df %>%
  left_join(stability_df, by = "k") %>%
  left_join(gmm_bic_df, by = "k") %>%
  mutate(
    eligible_min_size = min_class_pct >= 5,
    silhouette_scaled = safe_rescale01(avg_silhouette),
    stability_scaled = safe_rescale01(bootstrap_ARI_mean),
    mortality_scaled = safe_rescale01(mortality_range_pct),
    bic_scaled = safe_rescale01(gmm_BIC),
    class_balance_scaled = safe_rescale01(min_class_pct),
    conservative_score =
      0.40 * silhouette_scaled +
      0.40 * stability_scaled +
      0.10 * class_balance_scaled +
      0.10 * mortality_scaled,
    balanced_score =
      0.25 * silhouette_scaled +
      0.25 * stability_scaled +
      0.20 * mortality_scaled +
      0.20 * bic_scaled +
      0.10 * class_balance_scaled,
    clinical_discovery_score =
      0.15 * silhouette_scaled +
      0.20 * stability_scaled +
      0.30 * mortality_scaled +
      0.25 * bic_scaled +
      0.10 * class_balance_scaled,
    final_manuscript_k = k == fixed_k
  ) %>%
  arrange(k)

safe_fwrite(selection_metrics, file.path(dir_step03_tbl, "Step03_final_k5_k_selection_metrics.csv"))

## =========================================================
## 10. Final k=5 trajectory labels and clinical names
## =========================================================

final_km <- cluster_results[[as.character(fixed_k)]]
cluster_df$cluster_raw <- final_km$cluster

raw_mortality_order <- cluster_df %>%
  group_by(cluster_raw) %>%
  summarise(
    n = n(),
    mortality_pct = mean(death_30d_after_landmark == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(mortality_pct, cluster_raw) %>%
  mutate(trajectory = paste0("C", row_number()))

cluster_map <- raw_mortality_order %>%
  select(cluster_raw, trajectory)

trajectory_name_map <- tibble::tribble(
  ~trajectory, ~trajectory_name, ~trajectory_name_short,
  "C1", "Low-dysfunction recovery phenotype", "Low-dysfunction\nrecovery",
  "C2", "Hemodynamic-respiratory stress phenotype", "Hemodynamic-\nrespiratory stress",
  "C3", "Renal-metabolic dominant phenotype", "Renal-metabolic\ndominant",
  "C4", "Rapidly progressive multiorgan dysfunction phenotype", "Rapidly progressive\nmultiorgan dysfunction",
  "C5", "Persistent high-burden multiorgan dysfunction phenotype", "Persistent high-burden\nmultiorgan dysfunction"
) %>%
  mutate(trajectory = factor(trajectory, levels = paste0("C", 1:fixed_k)))

cluster_labeled <- cluster_df %>%
  left_join(cluster_map, by = "cluster_raw") %>%
  mutate(trajectory = factor(trajectory, levels = paste0("C", 1:fixed_k))) %>%
  left_join(trajectory_name_map, by = "trajectory")

trajectory_labels <- cluster_labeled %>%
  select(
    stay_id,
    subject_id,
    hadm_id,
    trajectory,
    trajectory_name,
    trajectory_name_short,
    cluster_raw,
    all_of(primary_outcome),
    mortality_30d_from_icu,
    days_from_landmark_to_death,
    any_of(c("in_icu_at_landmark"))
  )

safe_fwrite(trajectory_labels, out_labels_csv)

module_scores_labeled <- module_scores_long %>%
  inner_join(
    trajectory_labels %>% select(stay_id, trajectory, trajectory_name, trajectory_name_short),
    by = "stay_id"
  )

safe_fwrite(
  module_scores_labeled,
  file.path(dir_step03_data, "Step03_final_k5_organ_domain_scores_with_trajectory.csv")
)

## =========================================================
## 11. Summaries
## =========================================================

trajectory_summary <- cluster_labeled %>%
  group_by(trajectory, trajectory_name, trajectory_name_short) %>%
  summarise(
    n = n(),
    pct = n / nrow(cluster_labeled) * 100,
    events_30d_after_T48 = sum(death_30d_after_landmark == 1, na.rm = TRUE),
    mortality_30d_after_T48_pct = mean(death_30d_after_landmark == 1, na.rm = TRUE) * 100,
    mortality_30d_from_icu_pct = mean(mortality_30d_from_icu == 1, na.rm = TRUE) * 100,
    median_age = median(age, na.rm = TRUE),
    in_icu_at_T48_pct = if ("in_icu_at_landmark" %in% names(cluster_labeled)) mean(in_icu_at_landmark == 1, na.rm = TRUE) * 100 else NA_real_,
    median_icu_los_days = if ("icu_los_days" %in% names(cluster_labeled)) median(icu_los_days, na.rm = TRUE) else NA_real_,
    median_hospital_los_days = if ("hospital_los_days" %in% names(cluster_labeled)) median(hospital_los_days, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  arrange(trajectory)

trajectory_profile_long <- module_scores_labeled %>%
  filter(landmark %in% c("T0", "T24", "T48")) %>%
  group_by(trajectory, trajectory_name, trajectory_name_short, module, module_pretty, module_short, landmark) %>%
  summarise(
    mean_score = mean(module_score, na.rm = TRUE),
    median_score = median(module_score, na.rm = TRUE),
    q25_score = quantile(module_score, 0.25, na.rm = TRUE),
    q75_score = quantile(module_score, 0.75, na.rm = TRUE),
    n = sum(!is.na(module_score)),
    se = sd(module_score, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

trajectory_profile_wide <- trajectory_profile_long %>%
  mutate(name = paste0(module, "_", landmark)) %>%
  select(trajectory, trajectory_name, name, mean_score) %>%
  pivot_wider(id_cols = c(trajectory, trajectory_name), names_from = name, values_from = mean_score)

delta_features <- grep("delta_T48_T0|delta_T48_T24|slope_T0_T48", names(cluster_labeled), value = TRUE)

trajectory_delta_summary <- cluster_labeled %>%
  group_by(trajectory, trajectory_name) %>%
  summarise(
    across(
      all_of(delta_features),
      ~ mean(.x, na.rm = TRUE),
      .names = "mean_{.col}"
    ),
    .groups = "drop"
  )

safe_fwrite(trajectory_summary, file.path(dir_step03_tbl, "Step03_final_k5_trajectory_summary.csv"))
safe_fwrite(trajectory_profile_long, file.path(dir_step03_tbl, "Step03_final_k5_trajectory_module_profile_long.csv"))
safe_fwrite(trajectory_profile_wide, file.path(dir_step03_tbl, "Step03_final_k5_trajectory_module_profile_wide.csv"))
safe_fwrite(trajectory_delta_summary, file.path(dir_step03_tbl, "Step03_final_k5_trajectory_delta_summary.csv"))

print(trajectory_summary)

## =========================================================
## 12. Survival analysis
## =========================================================

surv_df <- cluster_labeled %>%
  mutate(
    event = as.integer(death_30d_after_landmark == 1),
    time_days = case_when(
      event == 1 & !is.na(days_from_landmark_to_death) ~ pmax(as.numeric(days_from_landmark_to_death), 0.05),
      TRUE ~ 30
    )
  )

surv_fit <- survival::survfit(
  survival::Surv(time_days, event) ~ trajectory,
  data = surv_df
)

surv_curve_df <- extract_survfit_df(surv_fit) %>%
  mutate(
    trajectory = factor(trajectory, levels = paste0("C", 1:fixed_k))
  ) %>%
  left_join(trajectory_name_map, by = "trajectory")

surv_diff <- tryCatch({
  survival::survdiff(survival::Surv(time_days, event) ~ trajectory, data = surv_df)
}, error = function(e) NULL)

logrank_p <- if (!is.null(surv_diff)) {
  1 - pchisq(surv_diff$chisq, df = length(surv_diff$n) - 1)
} else {
  NA_real_
}

survival_summary <- tibble::tibble(
  final_k = fixed_k,
  logrank_p = logrank_p,
  logrank_p_pretty = pretty_p(logrank_p)
)

safe_fwrite(surv_curve_df, file.path(dir_step03_tbl, "Step03_final_k5_KM_curve_data.csv"))
safe_fwrite(survival_summary, file.path(dir_step03_tbl, "Step03_final_k5_survival_logrank.csv"))

## =========================================================
## 13. PCA
## =========================================================

pca_fit <- prcomp(x_scaled, center = FALSE, scale. = FALSE)
pca_var <- (pca_fit$sdev^2) / sum(pca_fit$sdev^2)

pca_df <- tibble::tibble(
  stay_id = cluster_labeled$stay_id,
  PC1 = pca_fit$x[, 1],
  PC2 = pca_fit$x[, 2],
  trajectory = cluster_labeled$trajectory,
  trajectory_name = cluster_labeled$trajectory_name,
  trajectory_name_short = cluster_labeled$trajectory_name_short,
  death_30d_after_landmark = cluster_labeled$death_30d_after_landmark
)

pca_centroids <- pca_df %>%
  group_by(trajectory) %>%
  summarise(
    PC1 = median(PC1, na.rm = TRUE),
    PC2 = median(PC2, na.rm = TRUE),
    .groups = "drop"
  )

safe_fwrite(pca_df, file.path(dir_step03_tbl, "Step03_final_k5_PCA_coordinates.csv"))

## =========================================================
## 14. Main manuscript figures
## =========================================================

## Figure 3A: k-selection diagnostics
selection_plot_df <- selection_metrics %>%
  select(
    k,
    avg_silhouette,
    bootstrap_ARI_mean,
    min_class_pct,
    mortality_range_pct,
    bic_scaled,
    conservative_score,
    balanced_score,
    clinical_discovery_score
  ) %>%
  pivot_longer(cols = -k, names_to = "metric", values_to = "value") %>%
  mutate(
    metric = recode(
      metric,
      avg_silhouette = "Average silhouette",
      bootstrap_ARI_mean = "Bootstrap stability (ARI)",
      min_class_pct = "Minimum class size (%)",
      mortality_range_pct = "Mortality gradient (%)",
      bic_scaled = "GMM BIC scaled",
      conservative_score = "Conservative score",
      balanced_score = "Balanced score",
      clinical_discovery_score = "Clinical-discovery score"
    ),
    metric = factor(
      metric,
      levels = c(
        "Average silhouette",
        "Bootstrap stability (ARI)",
        "Minimum class size (%)",
        "Mortality gradient (%)",
        "GMM BIC scaled",
        "Conservative score",
        "Balanced score",
        "Clinical-discovery score"
      )
    )
  )

fig3a <- ggplot(selection_plot_df, aes(x = k, y = value, color = metric)) +
  geom_line(linewidth = 1.25) +
  geom_point(size = 3.3) +
  geom_vline(xintercept = fixed_k, linetype = "dashed", color = pal_red, linewidth = 0.9) +
  facet_wrap(~ metric, scales = "free_y", ncol = 4) +
  scale_color_manual(values = rep(c("#4A7494", "#F2B85B", "#7A5D98", "#13974A", "#EC7725", "#CE72A4"), length.out = length(unique(selection_plot_df$metric)))) +
  scale_x_continuous(breaks = k_range) +
  labs(
    title = "Figure 3A. Selection of early AMI trajectory phenotypes",
    subtitle = "The final k=5 solution balances clinical interpretability, mortality gradient, and class stability",
    x = "Number of trajectory phenotypes (k)",
    y = "Metric value",
    color = NULL
  ) +
  theme_ami(base_size = 14) +
  theme(legend.position = "none")

save_plot(fig3a, "Figure3A_final_k5_selection_diagnostics", width = 14, height = 8)

## Figure 3B: PCA
fig3b <- ggplot(pca_df, aes(x = PC1, y = PC2, color = trajectory)) +
  geom_point(alpha = 0.58, size = 1.55) +
  stat_ellipse(linewidth = 0.9, alpha = 0.75, show.legend = FALSE) +
  ggrepel::geom_text_repel(
    data = pca_centroids,
    aes(label = trajectory),
    size = 6.0,
    fontface = "bold",
    color = "black",
    box.padding = 0.4,
    min.segment.length = 0,
    show.legend = FALSE
  ) +
  scale_color_manual(values = pal_final) +
  labs(
    title = "Figure 3B. Low-dimensional structure of early AMI trajectory phenotypes",
    subtitle = paste0(
      "PCA of standardized organ-domain dynamic features; PC1 = ",
      sprintf("%.1f", pca_var[1] * 100), "%, PC2 = ",
      sprintf("%.1f", pca_var[2] * 100), "%"
    ),
    x = "PC1",
    y = "PC2",
    color = "Trajectory"
  ) +
  theme_ami(base_size = 15) +
  theme(legend.position = "right")

save_plot(fig3b, "Figure3B_final_k5_PCA_trajectory_phenotypes", width = 10.2, height = 7.6)

## Figure 3C: organ-domain trajectory curves
trajectory_curve_df <- trajectory_profile_long %>%
  filter(module %in% extended_modules, landmark %in% c("T0", "T24", "T48")) %>%
  mutate(
    landmark = factor(landmark, levels = c("T0", "T24", "T48")),
    module_short = factor(module_short, levels = c(
      "Hemodynamic",
      "Resp-neuro",
      "Renal-metabolic",
      "Inflam-coag",
      "Cardiac-hypoperfusion"
    ))
  )

fig3c <- ggplot(
  trajectory_curve_df,
  aes(x = landmark, y = mean_score, group = trajectory, color = trajectory, fill = trajectory)
) +
  geom_ribbon(
    aes(ymin = pmax(mean_score - se, 0), ymax = pmin(mean_score + se, 100)),
    alpha = 0.09,
    color = NA
  ) +
  geom_line(linewidth = 1.45) +
  geom_point(size = 3.0) +
  facet_wrap(~ module_short, ncol = 3) +
  scale_color_manual(values = pal_final) +
  scale_fill_manual(values = pal_final) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  labs(
    title = "Figure 3C. Early organ-domain trajectories across AMI phenotypes",
    subtitle = "Scores range from 0 to 100; higher values indicate more severe dysfunction",
    x = "Landmark window",
    y = "Organ-domain dysfunction score",
    color = "Trajectory",
    fill = "Trajectory"
  ) +
  theme_ami(base_size = 15) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 14, face = "bold")
  )

save_plot(fig3c, "Figure3C_final_k5_organ_domain_trajectory_curves", width = 13.5, height = 8.6)

## Figure 3D: soft heatmap
heat_df <- trajectory_profile_long %>%
  filter(module %in% extended_modules, landmark %in% c("T0", "T24", "T48")) %>%
  mutate(
    landmark = factor(landmark, levels = c("T0", "T24", "T48")),
    module_short = factor(module_short, levels = c(
      "Hemodynamic",
      "Resp-neuro",
      "Renal-metabolic",
      "Inflam-coag",
      "Cardiac-hypoperfusion"
    )),
    trajectory_plot = factor(trajectory, levels = rev(paste0("C", 1:fixed_k)))
  )

fig3d <- ggplot(heat_df, aes(x = landmark, y = trajectory_plot, fill = mean_score)) +
  geom_tile(color = "white", linewidth = 1.05) +
  geom_text(aes(label = sprintf("%.1f", mean_score)), size = 4.0, fontface = "bold", color = "black") +
  facet_wrap(~ module_short, ncol = 5) +
  scale_fill_gradientn(colors = pal_heat_soft, limits = c(0, 100), name = "Mean score") +
  labs(
    title = "Figure 3D. Organ-domain profile heatmap of AMI trajectory phenotypes",
    subtitle = "Soft color scale; higher values indicate greater organ-domain dysfunction",
    x = "Landmark",
    y = "Trajectory"
  ) +
  theme_ami(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    strip.text = element_text(size = 12.5, face = "bold"),
    legend.position = "right"
  )

save_plot(fig3d, "Figure3D_final_k5_soft_heatmap_faceted", width = 14.5, height = 5.9)

## Figure 3E: mortality bars
mortality_plot_df <- trajectory_summary %>%
  mutate(
    trajectory_short_label = paste0(as.character(trajectory), "\n", trajectory_name_short)
  )

fig3e <- ggplot(mortality_plot_df, aes(x = trajectory, y = mortality_30d_after_T48_pct, fill = trajectory)) +
  geom_col(width = 0.70, color = "white") +
  geom_text(
    aes(label = paste0(sprintf("%.1f", mortality_30d_after_T48_pct), "%\n", events_30d_after_T48, "/", n)),
    vjust = -0.28,
    size = 4.4,
    fontface = "bold"
  ) +
  scale_fill_manual(values = pal_final) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Figure 3E. Thirty-day mortality after T48 across AMI trajectory phenotypes",
    subtitle = "Trajectory labels are ordered from lowest to highest observed mortality",
    x = "Trajectory phenotype",
    y = "Observed 30-day mortality after T48 (%)",
    fill = NULL
  ) +
  theme_ami(base_size = 15) +
  theme(legend.position = "none")

save_plot(fig3e, "Figure3E_final_k5_mortality_by_trajectory", width = 9.6, height = 6.4)

## Figure 3F: Kaplan-Meier curves
fig3f <- ggplot(surv_curve_df, aes(x = time, y = surv, color = trajectory, fill = trajectory)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.055, color = NA) +
  geom_step(linewidth = 1.32) +
  scale_color_manual(values = pal_final) +
  scale_fill_manual(values = pal_final) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.70, 1.0)) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
  labs(
    title = "Figure 3F. Kaplan-Meier survival curves by early AMI trajectory phenotype",
    subtitle = paste0("Log-rank p = ", pretty_p(logrank_p)),
    x = "Days after T48 landmark",
    y = "Survival probability",
    color = "Trajectory",
    fill = "Trajectory"
  ) +
  theme_ami(base_size = 15) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

save_plot(fig3f, "Figure3F_final_k5_KM_survival_by_trajectory", width = 10, height = 7.2)

## =========================================================
## 15. Export workbook
## =========================================================

method_note <- tibble::tribble(
  ~item, ~description,
  "Step", "Step03-final: fixed k=5 organ-domain dynamic trajectory phenotyping.",
  "Main input", "Step02 T0/T24/T48/T72 landmark datasets.",
  "Main clustering population", "Patients alive at the T48 landmark.",
  "Main clustering window", "T0, T24, and T48 organ-domain dynamic scores.",
  "Why T72 is not used for main clustering", "Many AMI ICU patients have been discharged from ICU by 72h; T72 is retained descriptively rather than used in the main phenotyping window.",
  "Score orientation", "All organ-domain scores are percentile-based from 0 to 100, with higher values indicating more severe dysfunction.",
  "Main modules used for clustering", "Hemodynamic, respiratory-neurologic, renal-metabolic, and inflammatory-coagulopathic modules.",
  "Extension module for description", "Cardiac injury/hypoperfusion is summarized and visualized but not required for main clustering because lactate and troponin have higher missingness.",
  "Final k", "k=5",
  "Reason for selecting k=5", "k=5 offers the best balance between clinical interpretability, mortality gradient, and class size. k=2 is stable but too dichotomous; k=6 creates additional fragmentation and overlapping phenotype patterns.",
  "Trajectory ordering", "C1 to C5 are ordered by ascending observed 30-day mortality after T48.",
  "C1", "Low-dysfunction recovery phenotype.",
  "C2", "Hemodynamic-respiratory stress phenotype.",
  "C3", "Renal-metabolic dominant phenotype.",
  "C4", "Rapidly progressive multiorgan dysfunction phenotype.",
  "C5", "Persistent high-burden multiorgan dysfunction phenotype.",
  "Output labels", "Step03_final_k5_T48_trajectory_labels.csv should be used in subsequent Step04 and Step05 analyses."
)

wb <- openxlsx::createWorkbook()

add_sheet(wb, "00_method_note", method_note)
add_sheet(wb, "01_module_score_spec", component_spec)
add_sheet(wb, "02_module_availability", module_availability)
add_sheet(wb, "03_component_availability", component_availability)
add_sheet(wb, "04_k_selection_metrics", selection_metrics)
add_sheet(wb, "05_trajectory_summary", trajectory_summary)
add_sheet(wb, "06_profile_long", trajectory_profile_long)
add_sheet(wb, "07_profile_wide", trajectory_profile_wide)
add_sheet(wb, "08_delta_summary", trajectory_delta_summary)
add_sheet(wb, "09_survival_logrank", survival_summary)
add_sheet(wb, "10_trajectory_names", trajectory_name_map)
add_sheet(wb, "11_labels_preview", head(trajectory_labels, 5000))
add_sheet(wb, "12_pca_preview", head(pca_df, 5000))

openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

## =========================================================
## 16. Save trajectory object
## =========================================================

trajectory_object <- list(
  project = "AMI Dynamic Risk Evolution",
  step = "Step03-final fixed k=5 trajectory phenotyping",
  created_at = Sys.time(),
  final_k = fixed_k,
  primary_outcome = primary_outcome,
  main_modules = main_modules,
  extended_modules = extended_modules,
  component_spec = component_spec,
  main_cluster_features = main_cluster_features,
  clustering_preprocessor = prep_cluster,
  selection_metrics = selection_metrics,
  final_kmeans = final_km,
  cluster_map = cluster_map,
  trajectory_name_map = trajectory_name_map,
  trajectory_summary = trajectory_summary,
  module_availability = module_availability,
  survival_summary = survival_summary,
  note = "Trajectory labels were relabeled C1-C5 by ascending 30-day mortality after T48."
)

saveRDS(trajectory_object, out_object_rds)

## =========================================================
## 17. Print outputs
## =========================================================

cat("\n============================================================\n")
cat("Step03-final k=5 finished successfully.\n")
cat("============================================================\n\n")

cat("Summary workbook:\n", out_xlsx, "\n")
cat("Trajectory labels:\n", out_labels_csv, "\n")
cat("Module scores:\n", out_module_scores_csv, "\n")
cat("Clustering dataset:\n", out_cluster_dataset_csv, "\n")
cat("Trajectory model object:\n", out_object_rds, "\n")
cat("Figures folder:\n", dir_step03_fig, "\n")
cat("Log file:\n", log_file, "\n\n")

cat("Final k:", fixed_k, "\n\n")

cat("Trajectory summary:\n")
print(trajectory_summary)

cat("\nSurvival summary:\n")
print(survival_summary)

cat("\nSession info:\n")
print(sessionInfo())

sink()
