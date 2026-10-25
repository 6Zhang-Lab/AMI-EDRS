# HISTORICAL PROVENANCE ONLY. Not the current AMI-EDRS model. See docs/REPRODUCIBILITY.md.
############################################################
## Step04_v2: Complete clinical meaning validation of C1-C5 AMI trajectories
## Project: MIMIC_AMI_DynamicRisk_Project
##
## Purpose:
## This script proves that final k=5 C1-C5 trajectories are clinically meaningful
## phenotypes rather than merely algorithm-derived clusters.
##
## Analyses:
## 1. Baseline characteristics by trajectory
## 2. Organ-domain profiles at T0/T24/T48/T72
## 3. Organ-domain dynamic changes from T0 to T48
## 4. Outcome burden and mortality gradients
## 5. Kaplan-Meier survival separation
## 6. Adjusted logistic and Cox models
## 7. Incremental prognostic value beyond baseline covariates and T0 severity
##
## Required upstream files:
## <AMI_MIMIC_PROJECT>/
##   03_analysis_dataset/Step02_AMI_T48_dataset.csv
##   07_step03_final_k5_trajectory_phenotyping/trajectory_datasets/
##     Step03_final_k5_T48_trajectory_labels.csv
##     Step03_final_k5_organ_domain_scores_with_trajectory.csv
##
## Output:
## <AMI_MIMIC_PROJECT>/
##   08_step04_v2_trajectory_clinical_meaning/
############################################################

rm(list = ls())
gc()
options(stringsAsFactors = FALSE)

## =========================================================
## 0. Packages
## =========================================================

pkg_needed <- c(
  "dplyr", "tidyr", "stringr", "purrr", "tibble", "data.table",
  "ggplot2", "scales", "openxlsx", "survival", "broom",
  "forcats", "ggrepel", "pROC", "patchwork"
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

dir_step02 <- file.path(project_root, "03_analysis_dataset")
dir_step03 <- file.path(project_root, "07_step03_final_k5_trajectory_phenotyping")

input_t48 <- file.path(dir_step02, "Step02_AMI_T48_dataset.csv")

input_labels <- file.path(
  dir_step03,
  "trajectory_datasets",
  "Step03_final_k5_T48_trajectory_labels.csv"
)

input_module_scores <- file.path(
  dir_step03,
  "trajectory_datasets",
  "Step03_final_k5_organ_domain_scores_with_trajectory.csv"
)

dir_step04 <- file.path(project_root, "08_step04_v2_trajectory_clinical_meaning")
dir_fig <- file.path(dir_step04, "figures")
dir_tbl <- file.path(dir_step04, "tables")
dir_data <- file.path(dir_step04, "analysis_datasets")
dir_obj <- file.path(dir_step04, "objects")
dir_log <- file.path(dir_step04, "logs")

dir.create(dir_step04, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tbl, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_data, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_obj, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_log, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(dir_log, paste0("Step04_v2_trajectory_clinical_meaning_", timestamp, ".log"))

out_xlsx <- file.path(dir_tbl, "Step04_v2_trajectory_clinical_meaning_summary.xlsx")
out_analysis_csv <- file.path(dir_data, "Step04_v2_T48_dataset_with_k5_trajectory_and_modules.csv")
out_object_rds <- file.path(dir_obj, "Step04_v2_trajectory_clinical_meaning_object.rds")

## =========================================================
## 2. Colors and theme
## =========================================================

pal_final <- c(
  "C1" = "#4A7494",
  "C2" = "#F2B85B",
  "C3" = "#7A5D98",
  "C4" = "#13974A",
  "C5" = "#EC7725"
)

pal_heat_soft <- c("#EAF2F8", "#F4F6F8", "#FDF5E6", "#F2B85B", "#D98C72")
pal_blue <- "#4A7494"
pal_gold <- "#F2B85B"
pal_gray <- "#E3E8EC"
pal_red <- "#DA1F2E"
pal_dark <- "#1F2933"

theme_ami <- function(base_size = 16) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.major = ggplot2::element_line(color = pal_gray, linewidth = 0.55),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(face = "bold", color = "black", size = base_size + 1),
      axis.text = ggplot2::element_text(color = "black", size = base_size - 1),
      legend.title = ggplot2::element_text(face = "bold", size = base_size),
      legend.text = ggplot2::element_text(size = base_size - 1),
      strip.background = ggplot2::element_rect(fill = pal_gray, color = "grey70", linewidth = 0.35),
      strip.text = ggplot2::element_text(face = "bold", size = base_size - 1, color = "black"),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 5, color = "black", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = base_size + 1, color = "grey30", hjust = 0),
      plot.caption = ggplot2::element_text(size = base_size - 3, color = "grey40"),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.35),
      plot.margin = ggplot2::margin(12, 18, 12, 18)
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
## 3. Logging and input checks
## =========================================================

sink(log_file, split = TRUE)

cat("\n============================================================\n")
cat("Step04_v2: complete clinical meaning validation of C1-C5 AMI trajectories\n")
cat("Time:", as.character(Sys.time()), "\n")
cat("Project root:", project_root, "\n")
cat("Output folder:", dir_step04, "\n")
cat("============================================================\n\n")

required_inputs <- c(input_t48, input_labels, input_module_scores)
missing_inputs <- required_inputs[!file.exists(required_inputs)]

if (length(missing_inputs) > 0) {
  stop(
    "Missing required input files. Please run Step03-final k=5 first.\n",
    paste(missing_inputs, collapse = "\n")
  )
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

to_numeric_safe <- function(x) suppressWarnings(as.numeric(x))

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

collapse_race <- function(x) {
  x <- toupper(as.character(x))
  dplyr::case_when(
    is.na(x) | x == "" ~ "Unknown",
    stringr::str_detect(x, "WHITE") ~ "White",
    stringr::str_detect(x, "BLACK|AFRICAN") ~ "Black",
    stringr::str_detect(x, "HISPANIC|LATINO") ~ "Hispanic",
    stringr::str_detect(x, "ASIAN") ~ "Asian",
    TRUE ~ "Other/Unknown"
  )
}

pretty_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ NA_character_,
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

fmt_cont <- function(x) {
  x <- to_numeric_safe(x)
  if (all(is.na(x))) return(NA_character_)
  sprintf(
    "%.1f [%.1f, %.1f]",
    median(x, na.rm = TRUE),
    quantile(x, 0.25, na.rm = TRUE),
    quantile(x, 0.75, na.rm = TRUE)
  )
}

fmt_cat <- function(x, level_value = 1) {
  n_total <- sum(!is.na(x))
  n_event <- sum(x == level_value, na.rm = TRUE)
  if (n_total == 0) return(NA_character_)
  sprintf("%d (%.1f%%)", n_event, 100 * n_event / n_total)
}

select_existing <- function(df, vars) unique(vars[vars %in% names(df)])

p_cont <- function(df, var, group = "trajectory") {
  tmp <- df %>%
    dplyr::select(dplyr::all_of(c(var, group))) %>%
    dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[group]]))
  if (nrow(tmp) < 10 || length(unique(tmp[[group]])) < 2) return(NA_real_)
  tryCatch(stats::kruskal.test(tmp[[var]] ~ tmp[[group]])$p.value, error = function(e) NA_real_)
}

p_cat <- function(df, var, group = "trajectory") {
  tmp <- df %>%
    dplyr::select(dplyr::all_of(c(var, group))) %>%
    dplyr::filter(!is.na(.data[[var]]), !is.na(.data[[group]]))
  if (nrow(tmp) < 10 || length(unique(tmp[[group]])) < 2 || length(unique(tmp[[var]])) < 2) return(NA_real_)
  tab <- table(tmp[[group]], tmp[[var]])
  tryCatch(
    suppressWarnings(stats::chisq.test(tab)$p.value),
    error = function(e) tryCatch(stats::fisher.test(tab)$p.value, error = function(e2) NA_real_)
  )
}

make_formula <- function(outcome, terms) {
  terms <- unique(terms[!is.na(terms) & terms != ""])
  stats::as.formula(paste(outcome, "~", paste(terms, collapse = " + ")))
}

safe_glm <- function(df, outcome, terms) {
  terms <- unique(terms)
  vars <- unique(c(outcome, terms))
  vars <- vars[vars %in% names(df)]
  terms <- terms[terms %in% names(df)]
  if (!outcome %in% vars || length(terms) == 0) return(NULL)
  
  dat <- df %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::filter(!is.na(.data[[outcome]]))
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  
  if (nrow(dat) < 100 || sum(dat[[outcome]] == 1, na.rm = TRUE) < 10) return(NULL)
  
  tryCatch(
    stats::glm(make_formula(outcome, terms), data = dat, family = binomial()),
    error = function(e) {
      message("GLM failed: ", e$message)
      NULL
    }
  )
}

safe_cox <- function(df, time_var, event_var, terms) {
  terms <- unique(terms)
  vars <- unique(c(time_var, event_var, terms))
  vars <- vars[vars %in% names(df)]
  terms <- terms[terms %in% names(df)]
  
  if (!all(c(time_var, event_var) %in% vars) || length(terms) == 0) return(NULL)
  
  dat <- df %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::filter(!is.na(.data[[time_var]]), !is.na(.data[[event_var]]))
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  
  if (nrow(dat) < 100 || sum(dat[[event_var]] == 1, na.rm = TRUE) < 10) return(NULL)
  
  f <- stats::as.formula(
    paste0("survival::Surv(", time_var, ", ", event_var, ") ~ ", paste(terms, collapse = " + "))
  )
  
  tryCatch(
    survival::coxph(f, data = dat),
    error = function(e) {
      message("Cox failed: ", e$message)
      NULL
    }
  )
}

tidy_glm_or <- function(fit, model_name) {
  if (is.null(fit)) return(tibble())
  broom::tidy(fit) %>%
    dplyr::filter(stringr::str_detect(term, "^trajectory")) %>%
    dplyr::mutate(
      model = model_name,
      contrast = stringr::str_replace(term, "^trajectory", ""),
      OR = exp(estimate),
      conf.low = exp(estimate - 1.96 * std.error),
      conf.high = exp(estimate + 1.96 * std.error),
      p.value.pretty = pretty_p(p.value)
    ) %>%
    dplyr::select(model, term, contrast, OR, conf.low, conf.high, p.value, p.value.pretty)
}

tidy_cox_hr <- function(fit, model_name) {
  if (is.null(fit)) return(tibble())
  broom::tidy(fit) %>%
    dplyr::filter(stringr::str_detect(term, "^trajectory")) %>%
    dplyr::mutate(
      model = model_name,
      contrast = stringr::str_replace(term, "^trajectory", ""),
      HR = exp(estimate),
      conf.low = exp(estimate - 1.96 * std.error),
      conf.high = exp(estimate + 1.96 * std.error),
      p.value.pretty = pretty_p(p.value)
    ) %>%
    dplyr::select(model, term, contrast, HR, conf.low, conf.high, p.value, p.value.pretty)
}

model_metrics_apparent <- function(fit, model_name) {
  if (is.null(fit)) return(tibble())
  mf <- stats::model.frame(fit)
  y <- stats::model.response(mf)
  pred <- stats::predict(fit, type = "response")
  
  auc <- tryCatch(
    as.numeric(pROC::auc(pROC::roc(y, pred, quiet = TRUE, direction = "<"))),
    error = function(e) NA_real_
  )
  
  tibble::tibble(
    model = model_name,
    n = length(y),
    events = sum(y == 1, na.rm = TRUE),
    AUROC = auc,
    Brier = mean((y - pred)^2, na.rm = TRUE),
    AIC = stats::AIC(fit)
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
    dplyr::mutate(
      trajectory = stringr::str_replace(as.character(strata), "^trajectory=", "")
    )
}

## =========================================================
## 5. Load and assemble data
## =========================================================

t48 <- data.table::fread(input_t48) %>% as.data.frame()
labels <- data.table::fread(input_labels) %>% as.data.frame()
module_scores <- data.table::fread(input_module_scores) %>% as.data.frame()

cat("Loaded data:\n")
cat("T48:", nrow(t48), "rows,", ncol(t48), "columns\n")
cat("Labels:", nrow(labels), "rows,", ncol(labels), "columns\n")
cat("Module scores:", nrow(module_scores), "rows,", ncol(module_scores), "columns\n\n")

if (!"stay_id" %in% names(t48)) stop("stay_id missing in T48 dataset.")
if (!"stay_id" %in% names(labels)) stop("stay_id missing in trajectory labels.")
if (!"trajectory" %in% names(labels)) stop("trajectory missing in trajectory labels.")

primary_outcome <- "death_30d_after_landmark"
trajectory_levels <- paste0("C", 1:5)

trajectory_name_map <- tibble::tribble(
  ~trajectory, ~trajectory_name, ~trajectory_name_short, ~trajectory_interpretation,
  "C1", "Low-dysfunction recovery phenotype", "Low-dysfunction recovery", "Low organ-domain burden with improving or stable early trajectory",
  "C2", "Hemodynamic-respiratory stress phenotype", "Hemodynamic-respiratory stress", "Intermediate hemodynamic and respiratory-neurologic stress with relatively limited renal-metabolic burden",
  "C3", "Renal-metabolic dominant phenotype", "Renal-metabolic dominant", "Prominent renal-metabolic dysfunction with intermediate overall mortality risk",
  "C4", "Rapidly progressive multiorgan dysfunction phenotype", "Rapidly progressive MOD", "Marked worsening from T0 to T48, indicating dynamic deterioration beyond baseline severity",
  "C5", "Persistent high-burden multiorgan dysfunction phenotype", "Persistent high-burden MOD", "High baseline and persistent multiorgan dysfunction burden"
) %>%
  dplyr::mutate(trajectory = factor(trajectory, levels = trajectory_levels))

module_wide <- module_scores %>%
  dplyr::filter(landmark %in% c("T0", "T24", "T48", "T72")) %>%
  dplyr::mutate(
    module_short = dplyr::case_when(
      module == "hemodynamic" ~ "Hemodynamic",
      module == "respiratory_neurologic" ~ "Resp-neuro",
      module == "renal_metabolic" ~ "Renal-metabolic",
      module == "inflammatory_coagulopathic" ~ "Inflam-coag",
      module == "cardiac_hypoperfusion" ~ "Cardiac-hypoperfusion",
      TRUE ~ module
    ),
    name = paste0(module, "_", landmark, "_score")
  ) %>%
  dplyr::select(stay_id, name, module_score) %>%
  tidyr::pivot_wider(id_cols = stay_id, names_from = name, values_from = module_score)

analysis_df <- t48 %>%
  dplyr::left_join(
    labels %>%
      dplyr::select(
        stay_id,
        trajectory,
        trajectory_name,
        trajectory_name_short,
        dplyr::any_of(c(primary_outcome, "mortality_30d_from_icu", "days_from_landmark_to_death"))
      ),
    by = "stay_id",
    suffix = c("", "_label")
  ) %>%
  dplyr::left_join(module_wide, by = "stay_id")

if (paste0(primary_outcome, "_label") %in% names(analysis_df)) {
  analysis_df[[primary_outcome]] <- analysis_df[[paste0(primary_outcome, "_label")]]
}

analysis_df <- analysis_df %>%
  dplyr::mutate(
    trajectory = factor(as.character(trajectory), levels = trajectory_levels),
    death_30d_after_landmark = as_binary01(.data[[primary_outcome]]),
    trajectory_ord = as.integer(trajectory),
    race_collapsed = if ("race_collapsed" %in% names(.)) as.character(race_collapsed) else if ("race" %in% names(.)) collapse_race(race) else "Unknown",
    gender = if ("gender" %in% names(.)) factor(gender) else factor(NA)
  ) %>%
  dplyr::left_join(
    trajectory_name_map %>% dplyr::select(trajectory, trajectory_interpretation),
    by = "trajectory"
  ) %>%
  dplyr::filter(!is.na(trajectory), !is.na(death_30d_after_landmark))

module_bases <- c("hemodynamic", "respiratory_neurologic", "renal_metabolic", "inflammatory_coagulopathic", "cardiac_hypoperfusion")

for (m in module_bases) {
  c0 <- paste0(m, "_T0_score")
  c24 <- paste0(m, "_T24_score")
  c48 <- paste0(m, "_T48_score")
  c72 <- paste0(m, "_T72_score")
  
  if (all(c(c0, c24, c48) %in% names(analysis_df))) {
    analysis_df[[paste0(m, "_delta_T48_T0")]] <- analysis_df[[c48]] - analysis_df[[c0]]
    analysis_df[[paste0(m, "_delta_T48_T24")]] <- analysis_df[[c48]] - analysis_df[[c24]]
    analysis_df[[paste0(m, "_slope_T0_T48")]] <- (analysis_df[[c48]] - analysis_df[[c0]]) / 48
  }
  
  if (all(c(c48, c72) %in% names(analysis_df))) {
    analysis_df[[paste0(m, "_delta_T72_T48")]] <- analysis_df[[c72]] - analysis_df[[c48]]
  }
}

analysis_df <- analysis_df %>%
  dplyr::mutate(
    time_to_death_or_censor_days = dplyr::case_when(
      death_30d_after_landmark == 1 & !is.na(days_from_landmark_to_death) ~ pmax(as.numeric(days_from_landmark_to_death), 0.05),
      TRUE ~ 30
    ),
    event_30d = death_30d_after_landmark
  )

safe_fwrite(analysis_df, out_analysis_csv)

cat("Analysis dataset:", nrow(analysis_df), "patients\n")
cat("Events:", sum(analysis_df$event_30d == 1, na.rm = TRUE), "\n\n")

## =========================================================
## 6. Baseline clinical differences: Table 1
## =========================================================

continuous_candidates <- c(
  "age", "icu_los_days", "hospital_los_days",
  "charlson_comorbidity_index", "sofa", "apsiii", "lods",
  "oasis", "sapsii", "weight", "height", "bmi"
)

binary_candidates <- c(
  "hospital_expire_flag", "icu_expire_flag", "mortality_30d_from_icu", "in_icu_at_landmark",
  "hypertension", "diabetes", "diabetes_without_cc", "diabetes_with_cc",
  "heart_failure", "congestive_heart_failure", "atrial_fibrillation",
  "ckd", "chronic_kidney_disease", "renal_disease",
  "copd", "chronic_pulmonary_disease",
  "stroke", "cerebrovascular_disease", "peripheral_vascular_disease",
  "liver_disease", "mild_liver_disease", "severe_liver_disease",
  "malignancy", "metastatic_solid_tumor",
  "sepsis", "shock", "cardiogenic_shock"
)

categorical_candidates <- c("gender", "race_collapsed", "admission_type", "first_careunit", "insurance", "marital_status")

continuous_vars <- select_existing(analysis_df, continuous_candidates)
binary_vars <- select_existing(analysis_df, binary_candidates)
categorical_vars <- select_existing(analysis_df, categorical_candidates)

continuous_vars <- continuous_vars[purrr::map_lgl(continuous_vars, ~ sum(!is.na(to_numeric_safe(analysis_df[[.x]]))) > 50)]

for (v in binary_vars) analysis_df[[v]] <- as_binary01(analysis_df[[v]])

binary_vars <- binary_vars[purrr::map_lgl(binary_vars, ~ length(unique(na.omit(analysis_df[[.x]]))) >= 2)]
categorical_vars <- categorical_vars[purrr::map_lgl(categorical_vars, ~ length(unique(na.omit(as.character(analysis_df[[.x]])))) >= 2)]

variable_availability <- tibble::tibble(
  variable = c(continuous_vars, binary_vars, categorical_vars),
  type = c(rep("continuous", length(continuous_vars)), rep("binary", length(binary_vars)), rep("categorical", length(categorical_vars)))
) %>%
  dplyr::mutate(
    n_nonmissing = purrr::map_int(variable, ~ sum(!is.na(analysis_df[[.x]]))),
    nonmissing_pct = n_nonmissing / nrow(analysis_df) * 100
  ) %>%
  dplyr::arrange(type, variable)

safe_fwrite(variable_availability, file.path(dir_tbl, "Step04_v2_variable_availability.csv"))

table1_continuous <- purrr::map_dfr(continuous_vars, function(v) {
  tmp <- analysis_df %>%
    dplyr::group_by(trajectory) %>%
    dplyr::summarise(value = fmt_cont(.data[[v]]), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = trajectory, values_from = value)
  
  tibble::tibble(
    variable = v,
    type = "continuous",
    level = NA_character_,
    p.value = p_cont(analysis_df, v),
    p.value.pretty = pretty_p(p.value)
  ) %>%
    dplyr::bind_cols(tmp %>% dplyr::select(-dplyr::any_of("trajectory")))
})

table1_binary <- purrr::map_dfr(binary_vars, function(v) {
  tmp <- analysis_df %>%
    dplyr::group_by(trajectory) %>%
    dplyr::summarise(value = fmt_cat(.data[[v]], 1), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = trajectory, values_from = value)
  
  tibble::tibble(
    variable = v,
    type = "binary",
    level = "1",
    p.value = p_cat(analysis_df, v),
    p.value.pretty = pretty_p(p.value)
  ) %>%
    dplyr::bind_cols(tmp %>% dplyr::select(-dplyr::any_of("trajectory")))
})

table1_categorical <- purrr::map_dfr(categorical_vars, function(v) {
  x <- as.character(analysis_df[[v]])
  tab_levels <- sort(unique(na.omit(x)))
  
  if (length(tab_levels) > 10) {
    top_levels <- names(sort(table(x), decreasing = TRUE))[1:8]
    dat_tmp <- analysis_df %>%
      dplyr::mutate(.cat_tmp = ifelse(as.character(.data[[v]]) %in% top_levels, as.character(.data[[v]]), "Other"))
    levels_use <- c(top_levels, "Other")
  } else {
    dat_tmp <- analysis_df %>% dplyr::mutate(.cat_tmp = as.character(.data[[v]]))
    levels_use <- tab_levels
  }
  
  p_all <- p_cat(dat_tmp, ".cat_tmp")
  
  purrr::map_dfr(levels_use, function(lv) {
    tmp <- dat_tmp %>%
      dplyr::group_by(trajectory) %>%
      dplyr::summarise(
        n_total = sum(!is.na(.cat_tmp)),
        n_level = sum(.cat_tmp == lv, na.rm = TRUE),
        value = ifelse(n_total > 0, sprintf("%d (%.1f%%)", n_level, 100 * n_level / n_total), NA_character_),
        .groups = "drop"
      ) %>%
      dplyr::select(trajectory, value) %>%
      tidyr::pivot_wider(names_from = trajectory, values_from = value)
    
    tibble::tibble(variable = v, type = "categorical", level = lv, p.value = p_all, p.value.pretty = pretty_p(p_all)) %>%
      dplyr::bind_cols(tmp %>% dplyr::select(-dplyr::any_of("trajectory")))
  })
})

table1_all <- dplyr::bind_rows(table1_continuous, table1_binary, table1_categorical) %>%
  dplyr::select(variable, type, level, dplyr::starts_with("C"), p.value, p.value.pretty)

safe_fwrite(table1_all, file.path(dir_tbl, "Step04_v2_Table1_baseline_by_trajectory.csv"))

## =========================================================
## 7. Organ-domain module profiles and dynamic change
## =========================================================

module_profile_long <- module_scores %>%
  dplyr::filter(landmark %in% c("T0", "T24", "T48", "T72")) %>%
  dplyr::mutate(
    trajectory = factor(as.character(trajectory), levels = trajectory_levels),
    module_short = dplyr::case_when(
      module == "hemodynamic" ~ "Hemodynamic",
      module == "respiratory_neurologic" ~ "Resp-neuro",
      module == "renal_metabolic" ~ "Renal-metabolic",
      module == "inflammatory_coagulopathic" ~ "Inflam-coag",
      module == "cardiac_hypoperfusion" ~ "Cardiac-hypoperfusion",
      TRUE ~ module
    ),
    landmark = factor(landmark, levels = c("T0", "T24", "T48", "T72"))
  ) %>%
  dplyr::group_by(trajectory, module, module_pretty, module_short, landmark) %>%
  dplyr::summarise(
    n = sum(!is.na(module_score)),
    mean_score = mean(module_score, na.rm = TRUE),
    median_score = median(module_score, na.rm = TRUE),
    q25_score = quantile(module_score, 0.25, na.rm = TRUE),
    q75_score = quantile(module_score, 0.75, na.rm = TRUE),
    sd_score = sd(module_score, na.rm = TRUE),
    se_score = sd_score / sqrt(n),
    .groups = "drop"
  ) %>%
  dplyr::arrange(module, landmark, trajectory)

module_profile_wide <- module_profile_long %>%
  dplyr::mutate(name = paste0(module, "_", landmark, "_mean")) %>%
  dplyr::select(trajectory, name, mean_score) %>%
  tidyr::pivot_wider(id_cols = trajectory, names_from = name, values_from = mean_score)

module_delta_features <- grep("(_delta_T48_T0|_delta_T48_T24|_slope_T0_T48|_delta_T72_T48)$", names(analysis_df), value = TRUE)

module_delta_summary <- analysis_df %>%
  dplyr::group_by(trajectory) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(module_delta_features),
      list(mean = ~ mean(.x, na.rm = TRUE), median = ~ median(.x, na.rm = TRUE), q25 = ~ quantile(.x, 0.25, na.rm = TRUE), q75 = ~ quantile(.x, 0.75, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

module_delta_long <- analysis_df %>%
  dplyr::select(stay_id, trajectory, dplyr::all_of(module_delta_features)) %>%
  tidyr::pivot_longer(cols = dplyr::all_of(module_delta_features), names_to = "feature", values_to = "value") %>%
  dplyr::mutate(
    module = stringr::str_replace(feature, "(_delta_T48_T0|_delta_T48_T24|_slope_T0_T48|_delta_T72_T48)$", ""),
    dynamic_metric = dplyr::case_when(
      stringr::str_detect(feature, "_delta_T48_T0$") ~ "Delta T48-T0",
      stringr::str_detect(feature, "_delta_T48_T24$") ~ "Delta T48-T24",
      stringr::str_detect(feature, "_slope_T0_T48$") ~ "Slope T0-T48",
      stringr::str_detect(feature, "_delta_T72_T48$") ~ "Delta T72-T48",
      TRUE ~ feature
    ),
    module_short = dplyr::case_when(
      module == "hemodynamic" ~ "Hemodynamic",
      module == "respiratory_neurologic" ~ "Resp-neuro",
      module == "renal_metabolic" ~ "Renal-metabolic",
      module == "inflammatory_coagulopathic" ~ "Inflam-coag",
      module == "cardiac_hypoperfusion" ~ "Cardiac-hypoperfusion",
      TRUE ~ module
    )
  ) %>%
  dplyr::group_by(trajectory, module, module_short, dynamic_metric) %>%
  dplyr::summarise(
    n = sum(!is.na(value)),
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    q25_value = quantile(value, 0.25, na.rm = TRUE),
    q75_value = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    p.value = purrr::map2_dbl(module, dynamic_metric, function(mm, dm) {
      ff <- dplyr::case_when(
        dm == "Delta T48-T0" ~ paste0(mm, "_delta_T48_T0"),
        dm == "Delta T48-T24" ~ paste0(mm, "_delta_T48_T24"),
        dm == "Slope T0-T48" ~ paste0(mm, "_slope_T0_T48"),
        dm == "Delta T72-T48" ~ paste0(mm, "_delta_T72_T48"),
        TRUE ~ NA_character_
      )
      if (is.na(ff) || !ff %in% names(analysis_df)) return(NA_real_)
      p_cont(analysis_df, ff)
    }),
    p.value.pretty = pretty_p(p.value)
  )

safe_fwrite(module_profile_long, file.path(dir_tbl, "Step04_v2_module_profile_T0_T24_T48_T72_long.csv"))
safe_fwrite(module_profile_wide, file.path(dir_tbl, "Step04_v2_module_profile_wide.csv"))
safe_fwrite(module_delta_summary, file.path(dir_tbl, "Step04_v2_module_delta_summary_wide.csv"))
safe_fwrite(module_delta_long, file.path(dir_tbl, "Step04_v2_module_delta_summary_long.csv"))

trajectory_module_signature <- module_profile_long %>%
  dplyr::filter(landmark %in% c("T0", "T48")) %>%
  dplyr::select(trajectory, module_short, landmark, mean_score) %>%
  tidyr::pivot_wider(names_from = landmark, values_from = mean_score) %>%
  dplyr::mutate(
    delta_T48_T0 = T48 - T0,
    baseline_level = dplyr::case_when(T0 < 35 ~ "Low", T0 < 55 ~ "Intermediate", TRUE ~ "High"),
    evolution_pattern = dplyr::case_when(delta_T48_T0 <= -5 ~ "Improving", delta_T48_T0 >= 5 ~ "Worsening", TRUE ~ "Stable"),
    signature = paste0(baseline_level, " / ", evolution_pattern)
  )

trajectory_interpretation_table <- trajectory_module_signature %>%
  dplyr::select(trajectory, module_short, signature) %>%
  tidyr::pivot_wider(id_cols = trajectory, names_from = module_short, values_from = signature) %>%
  dplyr::left_join(trajectory_name_map, by = "trajectory") %>%
  dplyr::select(trajectory, trajectory_name, trajectory_interpretation, dplyr::everything()) %>%
  dplyr::arrange(trajectory)

safe_fwrite(trajectory_interpretation_table, file.path(dir_tbl, "Step04_v2_trajectory_clinical_interpretation_table.csv"))

## =========================================================
## 8. Outcome burden and survival
## =========================================================

binary_outcome_candidates <- c("death_30d_after_landmark", "mortality_30d_from_icu", "hospital_expire_flag", "icu_expire_flag", "in_icu_at_landmark")
binary_outcome_vars <- select_existing(analysis_df, binary_outcome_candidates)
for (v in binary_outcome_vars) analysis_df[[v]] <- as_binary01(analysis_df[[v]])

outcome_binary_summary <- purrr::map_dfr(binary_outcome_vars, function(v) {
  tmp <- analysis_df %>%
    dplyr::group_by(trajectory) %>%
    dplyr::summarise(n = sum(!is.na(.data[[v]])), events = sum(.data[[v]] == 1, na.rm = TRUE), rate_pct = events / n * 100, .groups = "drop")
  
  trend_p <- tryCatch({
    stats::prop.trend.test(x = tmp$events, n = tmp$n, score = as.integer(stringr::str_replace(as.character(tmp$trajectory), "C", "")))$p.value
  }, error = function(e) NA_real_)
  
  tmp %>% dplyr::mutate(outcome = v, trend_p = trend_p, trend_p.pretty = pretty_p(trend_p), .before = 1)
})

continuous_outcome_candidates <- c("icu_los_days", "hospital_los_days")
continuous_outcome_vars <- select_existing(analysis_df, continuous_outcome_candidates)

outcome_continuous_summary <- purrr::map_dfr(continuous_outcome_vars, function(v) {
  analysis_df %>%
    dplyr::group_by(trajectory) %>%
    dplyr::summarise(n = sum(!is.na(.data[[v]])), median = median(.data[[v]], na.rm = TRUE), q25 = quantile(.data[[v]], 0.25, na.rm = TRUE), q75 = quantile(.data[[v]], 0.75, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(outcome = v, p.value = p_cont(analysis_df, v), p.value.pretty = pretty_p(p.value), .before = 1)
})

safe_fwrite(outcome_binary_summary, file.path(dir_tbl, "Step04_v2_binary_outcomes_by_trajectory.csv"))
safe_fwrite(outcome_continuous_summary, file.path(dir_tbl, "Step04_v2_continuous_outcomes_by_trajectory.csv"))

surv_fit <- survival::survfit(survival::Surv(time_to_death_or_censor_days, event_30d) ~ trajectory, data = analysis_df)

surv_curve_df <- extract_survfit_df(surv_fit) %>%
  dplyr::mutate(trajectory = factor(trajectory, levels = trajectory_levels)) %>%
  dplyr::left_join(trajectory_name_map, by = "trajectory")

surv_diff <- tryCatch({ survival::survdiff(survival::Surv(time_to_death_or_censor_days, event_30d) ~ trajectory, data = analysis_df) }, error = function(e) NULL)
logrank_p <- if (!is.null(surv_diff)) 1 - stats::pchisq(surv_diff$chisq, df = length(surv_diff$n) - 1) else NA_real_

survival_summary <- tibble::tibble(logrank_p = logrank_p, logrank_p.pretty = pretty_p(logrank_p))

safe_fwrite(surv_curve_df, file.path(dir_tbl, "Step04_v2_KM_curve_data.csv"))
safe_fwrite(survival_summary, file.path(dir_tbl, "Step04_v2_KM_logrank.csv"))

## =========================================================
## 9. Regression: independent prognostic value
## =========================================================

analysis_df <- analysis_df %>%
  dplyr::mutate(
    trajectory = stats::relevel(factor(trajectory, levels = trajectory_levels), ref = "C1"),
    race_collapsed = factor(race_collapsed),
    gender = factor(gender)
  )

demo_terms <- select_existing(analysis_df, c("age", "gender", "race_collapsed"))

comorbidity_terms <- select_existing(analysis_df, c(
  "hypertension", "diabetes", "heart_failure", "congestive_heart_failure",
  "atrial_fibrillation", "ckd", "chronic_kidney_disease",
  "renal_disease", "copd", "chronic_pulmonary_disease",
  "stroke", "cerebrovascular_disease", "peripheral_vascular_disease",
  "liver_disease", "mild_liver_disease", "severe_liver_disease",
  "malignancy", "metastatic_solid_tumor"
))

comorbidity_terms <- comorbidity_terms[purrr::map_lgl(comorbidity_terms, ~ length(unique(na.omit(analysis_df[[.x]]))) >= 2)]

t0_score_terms <- select_existing(analysis_df, c("hemodynamic_T0_score", "respiratory_neurologic_T0_score", "renal_metabolic_T0_score", "inflammatory_coagulopathic_T0_score"))

glm_model_terms <- list(
  "Model 1: unadjusted" = c("trajectory"),
  "Model 2: demographics" = c("trajectory", demo_terms),
  "Model 3: demographics + comorbidities" = c("trajectory", demo_terms, comorbidity_terms),
  "Model 4: Model 3 + baseline T0 organ-domain scores" = c("trajectory", demo_terms, comorbidity_terms, t0_score_terms)
)

glm_fits <- purrr::imap(glm_model_terms, ~ safe_glm(analysis_df, "death_30d_after_landmark", .x))

logistic_or <- purrr::imap_dfr(glm_fits, ~ tidy_glm_or(.x, .y)) %>%
  dplyr::mutate(contrast = factor(contrast, levels = c("C2", "C3", "C4", "C5")), contrast_label = paste0(contrast, " vs C1"))

glm_trend_terms <- list(
  "Model 1: unadjusted" = c("trajectory_ord"),
  "Model 2: demographics" = c("trajectory_ord", demo_terms),
  "Model 3: demographics + comorbidities" = c("trajectory_ord", demo_terms, comorbidity_terms),
  "Model 4: Model 3 + baseline T0 organ-domain scores" = c("trajectory_ord", demo_terms, comorbidity_terms, t0_score_terms)
)

glm_trend_fits <- purrr::imap(glm_trend_terms, ~ safe_glm(analysis_df, "death_30d_after_landmark", .x))

logistic_trend <- purrr::imap_dfr(glm_trend_fits, function(fit, nm) {
  if (is.null(fit)) return(tibble())
  broom::tidy(fit) %>%
    dplyr::filter(term == "trajectory_ord") %>%
    dplyr::mutate(model = nm, OR_per_class = exp(estimate), conf.low = exp(estimate - 1.96 * std.error), conf.high = exp(estimate + 1.96 * std.error), p.value.pretty = pretty_p(p.value)) %>%
    dplyr::select(model, term, OR_per_class, conf.low, conf.high, p.value, p.value.pretty)
})

cox_fits <- purrr::imap(glm_model_terms, ~ safe_cox(analysis_df, "time_to_death_or_censor_days", "event_30d", .x))
cox_hr <- purrr::imap_dfr(cox_fits, ~ tidy_cox_hr(.x, .y)) %>%
  dplyr::mutate(contrast = factor(contrast, levels = c("C2", "C3", "C4", "C5")), contrast_label = paste0(contrast, " vs C1"))

cox_trend_fits <- purrr::imap(glm_trend_terms, ~ safe_cox(analysis_df, "time_to_death_or_censor_days", "event_30d", .x))
cox_trend <- purrr::imap_dfr(cox_trend_fits, function(fit, nm) {
  if (is.null(fit)) return(tibble())
  broom::tidy(fit) %>%
    dplyr::filter(term == "trajectory_ord") %>%
    dplyr::mutate(model = nm, HR_per_class = exp(estimate), conf.low = exp(estimate - 1.96 * std.error), conf.high = exp(estimate + 1.96 * std.error), p.value.pretty = pretty_p(p.value)) %>%
    dplyr::select(model, term, HR_per_class, conf.low, conf.high, p.value, p.value.pretty)
})

safe_fwrite(logistic_or, file.path(dir_tbl, "Step04_v2_logistic_OR_trajectory_vs_C1.csv"))
safe_fwrite(logistic_trend, file.path(dir_tbl, "Step04_v2_logistic_OR_per_trajectory_class.csv"))
safe_fwrite(cox_hr, file.path(dir_tbl, "Step04_v2_cox_HR_trajectory_vs_C1.csv"))
safe_fwrite(cox_trend, file.path(dir_tbl, "Step04_v2_cox_HR_per_trajectory_class.csv"))

## =========================================================
## 10. Incremental prognostic value
## =========================================================

metric_model_terms <- list(
  "Demographics" = demo_terms,
  "Demographics + trajectory" = c(demo_terms, "trajectory"),
  "Demographics + comorbidities" = c(demo_terms, comorbidity_terms),
  "Demographics + comorbidities + trajectory" = c(demo_terms, comorbidity_terms, "trajectory"),
  "Demographics + comorbidities + T0 scores" = c(demo_terms, comorbidity_terms, t0_score_terms),
  "Demographics + comorbidities + T0 scores + trajectory" = c(demo_terms, comorbidity_terms, t0_score_terms, "trajectory")
)

metric_fits <- purrr::imap(metric_model_terms, ~ safe_glm(analysis_df, "death_30d_after_landmark", .x))
model_metrics <- purrr::imap_dfr(metric_fits, ~ model_metrics_apparent(.x, .y))

make_lrt <- function(base_terms, plus_terms, label) {
  full_terms <- unique(c(base_terms, plus_terms))
  vars <- unique(c("death_30d_after_landmark", full_terms, base_terms))
  vars <- vars[vars %in% names(analysis_df)]
  
  dat <- analysis_df %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::filter(!is.na(death_30d_after_landmark))
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  
  if (nrow(dat) < 100 || sum(dat$death_30d_after_landmark == 1, na.rm = TRUE) < 10) {
    return(tibble::tibble(comparison = label, n = nrow(dat), LRT_p = NA_real_, LRT_p.pretty = NA_character_))
  }
  
  fit_base <- tryCatch(stats::glm(make_formula("death_30d_after_landmark", base_terms), data = dat, family = binomial()), error = function(e) NULL)
  fit_full <- tryCatch(stats::glm(make_formula("death_30d_after_landmark", full_terms), data = dat, family = binomial()), error = function(e) NULL)
  
  if (is.null(fit_base) || is.null(fit_full)) {
    return(tibble::tibble(comparison = label, n = nrow(dat), LRT_p = NA_real_, LRT_p.pretty = NA_character_))
  }
  
  p <- tryCatch(stats::anova(fit_base, fit_full, test = "Chisq")$`Pr(>Chi)`[2], error = function(e) NA_real_)
  tibble::tibble(comparison = label, n = nrow(dat), LRT_p = p, LRT_p.pretty = pretty_p(p))
}

lrt_results <- dplyr::bind_rows(
  make_lrt(demo_terms, "trajectory", "Add trajectory to demographics"),
  make_lrt(c(demo_terms, comorbidity_terms), "trajectory", "Add trajectory to demographics + comorbidities"),
  make_lrt(c(demo_terms, comorbidity_terms, t0_score_terms), "trajectory", "Add trajectory to demographics + comorbidities + T0 scores")
)

safe_fwrite(model_metrics, file.path(dir_tbl, "Step04_v2_incremental_model_metrics_apparent.csv"))
safe_fwrite(lrt_results, file.path(dir_tbl, "Step04_v2_likelihood_ratio_tests_adding_trajectory.csv"))

## =========================================================
## 11. Figures
## =========================================================

## Figure 4A: phenotype interpretation matrix
interpretation_long <- trajectory_interpretation_table %>%
  dplyr::select(trajectory, dplyr::any_of(c("Hemodynamic", "Resp-neuro", "Renal-metabolic", "Inflam-coag", "Cardiac-hypoperfusion"))) %>%
  tidyr::pivot_longer(cols = -trajectory, names_to = "module_short", values_to = "signature") %>%
  dplyr::mutate(
    trajectory = factor(trajectory, levels = trajectory_levels),
    module_short = factor(module_short, levels = c("Hemodynamic", "Resp-neuro", "Renal-metabolic", "Inflam-coag", "Cardiac-hypoperfusion")),
    signature_score = dplyr::case_when(
      stringr::str_detect(signature, "High / Worsening") ~ 5,
      stringr::str_detect(signature, "High / Stable") ~ 4,
      stringr::str_detect(signature, "Intermediate / Worsening") ~ 4,
      stringr::str_detect(signature, "High / Improving") ~ 3,
      stringr::str_detect(signature, "Intermediate / Stable") ~ 3,
      stringr::str_detect(signature, "Low / Worsening") ~ 3,
      stringr::str_detect(signature, "Intermediate / Improving") ~ 2,
      stringr::str_detect(signature, "Low / Stable") ~ 2,
      stringr::str_detect(signature, "Low / Improving") ~ 1,
      TRUE ~ 2
    )
  )

fig4a <- ggplot(interpretation_long, aes(x = module_short, y = trajectory, fill = signature_score)) +
  geom_tile(color = "white", linewidth = 1.0) +
  geom_text(aes(label = signature), size = 3.6, fontface = "bold", color = "black") +
  scale_fill_gradientn(colors = pal_heat_soft, limits = c(1, 5), name = "Severity\npattern") +
  labs(
    title = "Figure 4A. Clinical interpretation matrix of C1-C5 trajectory phenotypes",
    subtitle = "Each cell summarizes baseline organ-domain burden and early evolution from T0 to T48",
    x = NULL,
    y = NULL
  ) +
  theme_ami(base_size = 14) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(face = "bold", angle = 20, hjust = 1), axis.text.y = element_text(face = "bold"), legend.position = "right")

save_plot(fig4a, "Figure4A_clinical_interpretation_matrix", width = 12.5, height = 6.8)

## Figure 4B: organ-domain trajectory curves including T72 as descriptive extension
module_curve_df <- module_profile_long %>%
  dplyr::mutate(
    landmark = factor(landmark, levels = c("T0", "T24", "T48", "T72")),
    module_short = factor(module_short, levels = c("Hemodynamic", "Resp-neuro", "Renal-metabolic", "Inflam-coag", "Cardiac-hypoperfusion"))
  )

fig4b <- ggplot(module_curve_df, aes(x = landmark, y = mean_score, color = trajectory, fill = trajectory, group = trajectory)) +
  geom_ribbon(aes(ymin = pmax(mean_score - se_score, 0), ymax = pmin(mean_score + se_score, 100)), alpha = 0.07, color = NA) +
  geom_line(linewidth = 1.35) +
  geom_point(size = 2.9) +
  facet_wrap(~ module_short, ncol = 3) +
  scale_color_manual(values = pal_final) +
  scale_fill_manual(values = pal_final) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  labs(
    title = "Figure 4B. Organ-domain profiles of C1-C5 trajectories with T72 descriptive extension",
    subtitle = "T0-T48 defines the main phenotypes; T72 is shown descriptively among patients with available data",
    x = "Landmark",
    y = "Organ-domain dysfunction score",
    color = "Trajectory",
    fill = "Trajectory"
  ) +
  theme_ami(base_size = 14) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold", size = 13))

save_plot(fig4b, "Figure4B_organ_domain_profiles_T0_T24_T48_T72", width = 13.5, height = 8.8)

## Figure 4C: dynamic change heatmap
delta_heat_df <- module_delta_long %>%
  dplyr::filter(dynamic_metric %in% c("Delta T48-T0", "Delta T48-T24")) %>%
  dplyr::mutate(
    trajectory = factor(trajectory, levels = rev(trajectory_levels)),
    module_short = factor(module_short, levels = c("Hemodynamic", "Resp-neuro", "Renal-metabolic", "Inflam-coag", "Cardiac-hypoperfusion")),
    dynamic_metric = factor(dynamic_metric, levels = c("Delta T48-T0", "Delta T48-T24")),
    feature_label = paste0(module_short, "\n", dynamic_metric)
  )

fig4c <- ggplot(delta_heat_df, aes(x = feature_label, y = trajectory, fill = mean_value)) +
  geom_tile(color = "white", linewidth = 1.0) +
  geom_text(aes(label = sprintf("%.1f", mean_value)), size = 3.8, fontface = "bold") +
  scale_fill_gradient2(low = "#4A7494", mid = "#F4F6F8", high = "#EC7725", midpoint = 0, name = "Mean\nchange") +
  labs(
    title = "Figure 4C. Dynamic organ-domain change patterns underlying C1-C5 phenotypes",
    subtitle = "Positive values indicate worsening; negative values indicate improvement",
    x = NULL,
    y = NULL
  ) +
  theme_ami(base_size = 13) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(face = "bold", angle = 45, hjust = 1), axis.text.y = element_text(face = "bold"), legend.position = "right")

save_plot(fig4c, "Figure4C_dynamic_module_change_heatmap", width = 14.5, height = 6.3)

## Figure 4D: outcome burden heatmap
outcome_heat_df <- outcome_binary_summary %>%
  dplyr::mutate(
    outcome_label = dplyr::recode(
      outcome,
      death_30d_after_landmark = "30-day mortality\nafter T48",
      mortality_30d_from_icu = "30-day mortality\nfrom ICU admission",
      hospital_expire_flag = "In-hospital\nmortality",
      icu_expire_flag = "ICU\nmortality",
      in_icu_at_landmark = "Still in ICU\nat T48"
    ),
    trajectory = factor(trajectory, levels = trajectory_levels)
  )

fig4d <- ggplot(outcome_heat_df, aes(x = trajectory, y = outcome_label, fill = rate_pct)) +
  geom_tile(color = "white", linewidth = 1.0) +
  geom_text(aes(label = sprintf("%.1f%%", rate_pct)), size = 4.2, fontface = "bold", color = "black") +
  scale_fill_gradientn(colors = pal_heat_soft, name = "Rate (%)") +
  labs(
    title = "Figure 4D. Clinical outcome burden across C1-C5 trajectory phenotypes",
    subtitle = "Rates of mortality-related outcomes across early AMI trajectory phenotypes",
    x = "Trajectory phenotype",
    y = NULL
  ) +
  theme_ami(base_size = 14) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(face = "bold"), axis.text.y = element_text(face = "bold"), legend.position = "right")

save_plot(fig4d, "Figure4D_outcome_burden_heatmap", width = 9.8, height = 6.2)

## Figure 4E: KM survival curves
fig4e <- ggplot(surv_curve_df, aes(x = time, y = surv, color = trajectory, fill = trajectory)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.05, color = NA) +
  geom_step(linewidth = 1.30) +
  scale_color_manual(values = pal_final) +
  scale_fill_manual(values = pal_final) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.70, 1.0)) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
  labs(
    title = "Figure 4E. Kaplan-Meier survival curves across C1-C5 trajectory phenotypes",
    subtitle = paste0("Log-rank p = ", pretty_p(logrank_p)),
    x = "Days after T48 landmark",
    y = "Survival probability",
    color = "Trajectory",
    fill = "Trajectory"
  ) +
  theme_ami(base_size = 14) +
  theme(legend.position = "bottom")

save_plot(fig4e, "Figure4E_KM_survival_by_trajectory", width = 10.0, height = 7.2)

## Figure 4F: adjusted OR forest
forest_model_name <- "Model 4: Model 3 + baseline T0 organ-domain scores"
if (!forest_model_name %in% logistic_or$model && nrow(logistic_or) > 0) forest_model_name <- tail(unique(logistic_or$model), 1)

or_forest_df <- logistic_or %>%
  dplyr::filter(model == forest_model_name) %>%
  dplyr::mutate(contrast = factor(contrast, levels = c("C2", "C3", "C4", "C5")), label = paste0(contrast, " vs C1"))

if (nrow(or_forest_df) > 0) {
  fig4f <- ggplot(or_forest_df, aes(x = OR, y = forcats::fct_rev(label), color = contrast)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey45", linewidth = 0.8) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.18, linewidth = 1.05) +
    geom_point(size = 4.0) +
    scale_color_manual(values = pal_final[names(pal_final) %in% as.character(or_forest_df$contrast)]) +
    scale_x_log10() +
    labs(
      title = "Figure 4F. Adjusted odds ratios for 30-day mortality after T48",
      subtitle = forest_model_name,
      x = "Odds ratio, log scale",
      y = NULL,
      color = NULL
    ) +
    theme_ami(base_size = 14) +
    theme(legend.position = "none")
  save_plot(fig4f, "Figure4F_adjusted_logistic_OR_forest", width = 8.8, height = 5.6)
}

## Figure 4G: adjusted HR forest
hr_forest_df <- cox_hr %>%
  dplyr::filter(model == forest_model_name) %>%
  dplyr::mutate(contrast = factor(contrast, levels = c("C2", "C3", "C4", "C5")), label = paste0(contrast, " vs C1"))

if (nrow(hr_forest_df) > 0) {
  fig4g <- ggplot(hr_forest_df, aes(x = HR, y = forcats::fct_rev(label), color = contrast)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey45", linewidth = 0.8) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.18, linewidth = 1.05) +
    geom_point(size = 4.0) +
    scale_color_manual(values = pal_final[names(pal_final) %in% as.character(hr_forest_df$contrast)]) +
    scale_x_log10() +
    labs(
      title = "Figure 4G. Adjusted hazard ratios for 30-day mortality after T48",
      subtitle = forest_model_name,
      x = "Hazard ratio, log scale",
      y = NULL,
      color = NULL
    ) +
    theme_ami(base_size = 14) +
    theme(legend.position = "none")
  save_plot(fig4g, "Figure4G_adjusted_cox_HR_forest", width = 8.8, height = 5.6)
}

## Figure 4H: incremental value dumbbell
metric_long <- model_metrics %>%
  dplyr::mutate(
    model_group = dplyr::case_when(
      stringr::str_detect(model, "Demographics \\+ comorbidities \\+ T0 scores") ~ "Demographics + comorbidities + T0 scores",
      stringr::str_detect(model, "Demographics \\+ comorbidities") ~ "Demographics + comorbidities",
      stringr::str_detect(model, "Demographics") ~ "Demographics",
      TRUE ~ model
    ),
    trajectory_status = ifelse(stringr::str_detect(model, "trajectory"), "With trajectory", "Without trajectory"),
    model_group = factor(model_group, levels = c("Demographics", "Demographics + comorbidities", "Demographics + comorbidities + T0 scores")),
    trajectory_status = factor(trajectory_status, levels = c("Without trajectory", "With trajectory"))
  ) %>%
  dplyr::select(model_group, trajectory_status, AUROC, Brier) %>%
  tidyr::pivot_longer(cols = c(AUROC, Brier), names_to = "metric", values_to = "value") %>%
  tidyr::pivot_wider(names_from = trajectory_status, values_from = value) %>%
  dplyr::mutate(
    delta = `With trajectory` - `Without trajectory`,
    delta_label = dplyr::case_when(metric == "AUROC" ~ paste0("+", sprintf("%.3f", delta)), metric == "Brier" ~ sprintf("%.3f", delta), TRUE ~ sprintf("%.3f", delta))
  )

fig4h <- ggplot(metric_long, aes(y = forcats::fct_rev(model_group))) +
  geom_segment(aes(x = `Without trajectory`, xend = `With trajectory`, yend = forcats::fct_rev(model_group)), linewidth = 1.2, color = "grey65") +
  geom_point(aes(x = `Without trajectory`, color = "Without trajectory"), size = 4.2) +
  geom_point(aes(x = `With trajectory`, color = "With trajectory"), size = 4.2) +
  geom_text(aes(x = pmax(`Without trajectory`, `With trajectory`, na.rm = TRUE), label = delta_label), hjust = -0.15, size = 4.0, fontface = "bold", color = "grey25") +
  facet_wrap(~ metric, scales = "free_x", ncol = 2) +
  scale_color_manual(values = c("Without trajectory" = pal_blue, "With trajectory" = pal_gold)) +
  labs(
    title = "Figure 4H. Incremental prognostic value of trajectory phenotype",
    subtitle = "Apparent model performance before and after adding C1-C5 trajectory phenotype",
    x = "Metric value",
    y = NULL,
    color = NULL
  ) +
  theme_ami(base_size = 14) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold", size = 15), panel.grid.major.y = element_line(color = "grey90")) +
  coord_cartesian(clip = "off")

save_plot(fig4h, "Figure4H_incremental_value_dumbbell", width = 12.5, height = 6.7)

## =========================================================
## 12. Export Excel workbook
## =========================================================

method_note <- tibble::tribble(
  ~item, ~description,
  "Step", "Step04_v2: complete clinical meaning validation of final k=5 AMI trajectory phenotypes.",
  "Main input", "Step03_final_k5_T48_trajectory_labels.csv and Step03_final_k5_organ_domain_scores_with_trajectory.csv.",
  "Primary outcome", "30-day mortality after the T48 landmark.",
  "Main objective", "To demonstrate that C1-C5 are clinically meaningful phenotypes rather than purely algorithmic clusters.",
  "Clinical meaning layer 1", "Baseline clinical characteristics and comorbidities differ across trajectories.",
  "Clinical meaning layer 2", "Organ-domain profiles at T0/T24/T48 show distinct severity patterns.",
  "Clinical meaning layer 3", "Organ-domain change from T0 to T48 identifies improving, stable, worsening, and persistent high-burden patterns.",
  "Clinical meaning layer 4", "Mortality, ICU/hospital outcomes, and Kaplan-Meier survival differ across trajectories.",
  "Clinical meaning layer 5", "Trajectory phenotype independently predicts mortality after adjustment for demographics, comorbidities, and baseline T0 organ-domain scores.",
  "Clinical meaning layer 6", "Trajectory phenotype adds incremental prognostic information beyond baseline covariates and T0 scores.",
  "T72 handling", "T72 is displayed descriptively as an extension of organ-domain profiles but is not used to define the main T48 trajectory phenotype."
)

wb <- openxlsx::createWorkbook()
add_sheet(wb, "00_method_note", method_note)
add_sheet(wb, "01_trajectory_names", trajectory_name_map)
add_sheet(wb, "02_variable_availability", variable_availability)
add_sheet(wb, "03_Table1_baseline", table1_all)
add_sheet(wb, "04_module_profile_long", module_profile_long)
add_sheet(wb, "05_module_profile_wide", module_profile_wide)
add_sheet(wb, "06_module_delta_wide", module_delta_summary)
add_sheet(wb, "07_module_delta_long", module_delta_long)
add_sheet(wb, "08_interpretation_table", trajectory_interpretation_table)
add_sheet(wb, "09_binary_outcomes", outcome_binary_summary)
add_sheet(wb, "10_continuous_outcomes", outcome_continuous_summary)
add_sheet(wb, "11_KM_logrank", survival_summary)
add_sheet(wb, "12_logistic_OR", logistic_or)
add_sheet(wb, "13_logistic_trend", logistic_trend)
add_sheet(wb, "14_cox_HR", cox_hr)
add_sheet(wb, "15_cox_trend", cox_trend)
add_sheet(wb, "16_incremental_metrics", model_metrics)
add_sheet(wb, "17_LRT_trajectory", lrt_results)
add_sheet(wb, "18_analysis_preview", head(analysis_df, 5000))
openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

## =========================================================
## 13. Save object
## =========================================================

step04_object <- list(
  project = "MIMIC_AMI_DynamicRisk_Project",
  step = "Step04_v2_complete_trajectory_clinical_meaning",
  created_at = Sys.time(),
  primary_outcome = primary_outcome,
  trajectory_levels = trajectory_levels,
  trajectory_name_map = trajectory_name_map,
  analysis_df = analysis_df,
  variable_availability = variable_availability,
  table1_all = table1_all,
  module_profile_long = module_profile_long,
  module_delta_long = module_delta_long,
  trajectory_interpretation_table = trajectory_interpretation_table,
  outcome_binary_summary = outcome_binary_summary,
  outcome_continuous_summary = outcome_continuous_summary,
  survival_summary = survival_summary,
  logistic_or = logistic_or,
  logistic_trend = logistic_trend,
  cox_hr = cox_hr,
  cox_trend = cox_trend,
  model_metrics = model_metrics,
  lrt_results = lrt_results
)

saveRDS(step04_object, out_object_rds)

## =========================================================
## 14. Print outputs
## =========================================================

cat("\n============================================================\n")
cat("Step04_v2 finished successfully.\n")
cat("============================================================\n\n")
cat("Summary workbook:\n", out_xlsx, "\n")
cat("Analysis dataset:\n", out_analysis_csv, "\n")
cat("Object:\n", out_object_rds, "\n")
cat("Figures folder:\n", dir_fig, "\n")
cat("Log file:\n", log_file, "\n\n")
cat("Key outputs:\n")
cat("1. Table 1 baseline by trajectory\n")
cat("2. Organ-domain module profile and dynamic change tables\n")
cat("3. Clinical interpretation matrix\n")
cat("4. Outcome burden and KM survival curves\n")
cat("5. Adjusted OR and HR forest plots\n")
cat("6. Incremental value dumbbell plot\n\n")
cat("Available covariates:\n")
cat("Demographics:", paste(demo_terms, collapse = ", "), "\n")
cat("Comorbidities:", paste(comorbidity_terms, collapse = ", "), "\n")
cat("T0 scores:", paste(t0_score_terms, collapse = ", "), "\n\n")
cat("Outcome summary:\n")
print(outcome_binary_summary)
cat("\nLogistic trend:\n")
print(logistic_trend)
cat("\nCox trend:\n")
print(cox_trend)
cat("\nIncremental metrics:\n")
print(model_metrics)
cat("\nLRT results:\n")
print(lrt_results)
cat("\nSession info:\n")
print(sessionInfo())

sink()
