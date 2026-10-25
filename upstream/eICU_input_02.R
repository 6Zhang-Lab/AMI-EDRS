############################################################
## Step E2C: eICU laboratory extraction for AMI-EDRS external validation
##
## Aim:
##   Extract laboratory features from T0 to T72 after ICU admission
##   for the eICU AMI T48 external validation cohort.
##
## Input:
##   StepE0_path_config.rds
##   StepE2A_eICU_AMI_T48_cohort_demographics_comorbidities.csv
##   lab.csv.gz
##   customLab.csv.gz
##
## Output:
##   1. Laboratory long table
##   2. Window-level laboratory summary table
##   3. Patient-level wide laboratory feature table
##   4. Laboratory availability table
##   5. Laboratory name mapping review table
##   6. Publication-style figures
##   7. RDS object for downstream steps
############################################################

rm(list = ls())
gc()

options(stringsAsFactors = FALSE)

## =========================================================
## 0. Packages
## =========================================================

pkg_needed <- c(
  "dplyr", "tidyr", "stringr", "purrr", "tibble",
  "data.table", "readr", "openxlsx", "DBI", "duckdb",
  "ggplot2", "patchwork", "scales"
)

for (p in pkg_needed) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

invisible(lapply(pkg_needed, library, character.only = TRUE))

## =========================================================
## 1. Load path configuration
## =========================================================

project_dir <- Sys.getenv("AMI_EICU_PROJECT", unset=file.path(getwd(), "work/eICU"))
config_file <- file.path(project_dir, "00_admin", "StepE0_path_config.rds")

if (!file.exists(config_file)) {
  stop("Cannot find StepE0 path config: ", config_file)
}

path_config <- readRDS(config_file)

raw_eicu_dir <- path_config$raw_eicu_dir
dirs <- path_config$dirs

dir_feature <- dirs$features
dir_tables <- file.path(dirs$tables, "feature_tables")
dir_objects <- dirs$objects
dir_logs <- dirs$logs

dir_e2a <- file.path(dir_feature, "E2A_demographics_comorbidities")
dir_e2c <- file.path(dir_feature, "E2C_laboratory")

fig_dir_avail <- file.path(dirs$figures, "variable_availability")
fig_dir_explore <- file.path(dirs$figures, "feature_exploration")

dir.create(dir_e2c, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_objects, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_logs, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir_avail, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir_explore, recursive = TRUE, showWarnings = FALSE)

csv_gz <- function(file_name) {
  file.path(raw_eicu_dir, file_name)
}

cat("Raw eICU folder:\n", raw_eicu_dir, "\n\n")
cat("Step E2C output folder:\n", dir_e2c, "\n\n")

## =========================================================
## 2. Helper functions
## =========================================================

sql_path <- function(path) {
  gsub("'", "''", normalizePath(path, mustWork = TRUE))
}

qid <- function(x) {
  paste0('"', gsub('"', '""', x), '"')
}

read_header <- function(path) {
  data.table::fread(path, nrows = 0, data.table = FALSE) %>%
    names()
}

pick_col <- function(nm, candidates) {
  nm_low <- tolower(nm)
  cand_low <- tolower(candidates)
  
  idx <- match(cand_low, nm_low)
  idx <- idx[!is.na(idx)]
  if (length(idx) > 0) return(nm[idx[1]])
  
  for (cand in cand_low) {
    hit <- nm[stringr::str_detect(nm_low, stringr::fixed(cand))]
    if (length(hit) > 0) return(hit[1])
  }
  
  NA_character_
}

sql_col_or_null <- function(nm, candidates, alias, type = c("varchar", "double")) {
  type <- match.arg(type)
  hit <- pick_col(nm, candidates)
  
  if (is.na(hit)) {
    if (type == "varchar") {
      return(paste0("CAST(NULL AS VARCHAR) AS ", alias))
    } else {
      return(paste0("CAST(NULL AS DOUBLE) AS ", alias))
    }
  }
  
  if (type == "varchar") {
    paste0("CAST(x.", qid(hit), " AS VARCHAR) AS ", alias)
  } else {
    paste0("TRY_CAST(x.", qid(hit), " AS DOUBLE) AS ", alias)
  }
}

extract_first_number <- function(x) {
  x <- as.character(x)
  suppressWarnings(as.numeric(
    stringr::str_extract(x, "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?")
  ))
}

clean_text <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_replace_all("[\r\n\t]", " ") %>%
    stringr::str_squish() %>%
    tolower()
}

standardize_lab_variable <- function(lab_name_clean) {
  
  dplyr::case_when(
    stringr::str_detect(lab_name_clean, "white blood|\\bwbc\\b") ~ "wbc",
    
    stringr::str_detect(lab_name_clean, "hemoglobin|\\bhgb\\b|\\bhb\\b") &
      !stringr::str_detect(lab_name_clean, "a1c|glyco") ~ "hemoglobin",
    
    stringr::str_detect(lab_name_clean, "platelet|\\bplt\\b") ~ "platelet",
    
    stringr::str_detect(lab_name_clean, "creatinine") &
      !stringr::str_detect(lab_name_clean, "kinase|\\bck\\b|phosphokinase") ~ "creatinine",
    
    stringr::str_detect(lab_name_clean, "\\bbun\\b|urea nitrogen") ~ "bun",
    
    stringr::str_detect(lab_name_clean, "\\bsodium\\b|\\bna\\b") ~ "sodium",
    
    stringr::str_detect(lab_name_clean, "\\bpotassium\\b|\\bk\\b") ~ "potassium",
    
    stringr::str_detect(lab_name_clean, "bicarbonate|\\bhco3\\b|total co2|\\bco2\\b") ~ "bicarbonate",
    
    stringr::str_detect(lab_name_clean, "\\bglucose\\b|blood sugar") ~ "glucose",
    
    stringr::str_detect(lab_name_clean, "\\blactate\\b|lactic acid") ~ "lactate",
    
    stringr::str_detect(lab_name_clean, "\\binr\\b|international normalized") ~ "inr",
    
    stringr::str_detect(lab_name_clean, "\\bptt\\b|partial thromboplastin|aptt") ~ "ptt",
    
    stringr::str_detect(lab_name_clean, "\\bpt\\b|prothrombin") &
      !stringr::str_detect(lab_name_clean, "\\bptt\\b|partial thromboplastin|aptt") ~ "pt",
    
    stringr::str_detect(lab_name_clean, "troponin") ~ "troponin",
    
    stringr::str_detect(lab_name_clean, "\\balbumin\\b") ~ "albumin",
    
    stringr::str_detect(lab_name_clean, "bilirubin") &
      !stringr::str_detect(lab_name_clean, "urine") ~ "bilirubin",
    
    stringr::str_detect(lab_name_clean, "\\bast\\b|sgot") ~ "ast",
    
    stringr::str_detect(lab_name_clean, "\\balt\\b|sgpt") ~ "alt",
    
    TRUE ~ NA_character_
  )
}

clean_lab_values <- function(df) {
  
  df %>%
    dplyr::mutate(
      value = suppressWarnings(as.numeric(value)),
      value = dplyr::case_when(
        variable == "wbc" & (value < 0.1 | value > 300) ~ NA_real_,
        variable == "hemoglobin" & (value < 1 | value > 25) ~ NA_real_,
        variable == "platelet" & (value < 1 | value > 2500) ~ NA_real_,
        variable == "creatinine" & (value < 0.05 | value > 40) ~ NA_real_,
        variable == "bun" & (value < 1 | value > 350) ~ NA_real_,
        variable == "sodium" & (value < 90 | value > 190) ~ NA_real_,
        variable == "potassium" & (value < 1 | value > 12) ~ NA_real_,
        variable == "bicarbonate" & (value < 1 | value > 70) ~ NA_real_,
        variable == "glucose" & (value < 5 | value > 2000) ~ NA_real_,
        variable == "lactate" & (value < 0.05 | value > 40) ~ NA_real_,
        variable == "inr" & (value < 0.3 | value > 30) ~ NA_real_,
        variable == "ptt" & (value < 5 | value > 300) ~ NA_real_,
        variable == "pt" & (value < 5 | value > 300) ~ NA_real_,
        variable == "troponin" & (value < 0 | value > 1000000) ~ NA_real_,
        variable == "albumin" & (value < 0.5 | value > 8) ~ NA_real_,
        variable == "bilirubin" & (value < 0 | value > 80) ~ NA_real_,
        variable == "ast" & (value < 0 | value > 50000) ~ NA_real_,
        variable == "alt" & (value < 0 | value > 50000) ~ NA_real_,
        TRUE ~ value
      )
    ) %>%
    dplyr::filter(!is.na(value))
}

assign_windows <- function(df) {
  df %>%
    dplyr::mutate(
      window_6h = dplyr::if_else(offset_min >= 0 & offset_min <= 6 * 60, "0-6h", NA_character_),
      window_24h = dplyr::if_else(offset_min >= 0 & offset_min <= 24 * 60, "0-24h", NA_character_),
      window_48h = dplyr::if_else(offset_min >= 0 & offset_min <= 48 * 60, "0-48h", NA_character_),
      window_72h = dplyr::if_else(offset_min >= 0 & offset_min <= 72 * 60, "0-72h", NA_character_)
    )
}

summarise_labs_by_window <- function(lab_long, cohort_ids) {
  
  window_long <- lab_long %>%
    assign_windows() %>%
    tidyr::pivot_longer(
      cols = c(window_6h, window_24h, window_48h, window_72h),
      names_to = "window_type",
      values_to = "window"
    ) %>%
    dplyr::filter(!is.na(window)) %>%
    dplyr::mutate(
      window = factor(window, levels = c("0-6h", "0-24h", "0-48h", "0-72h"))
    )
  
  summary_df <- window_long %>%
    dplyr::arrange(patientunitstayid, variable, window, offset_min) %>%
    dplyr::group_by(patientunitstayid, variable, window) %>%
    dplyr::summarise(
      n_measure = dplyr::n(),
      first_value = dplyr::first(value),
      last_value = dplyr::last(value),
      min_value = min(value, na.rm = TRUE),
      max_value = max(value, na.rm = TRUE),
      mean_value = mean(value, na.rm = TRUE),
      median_value = median(value, na.rm = TRUE),
      sd_value = dplyr::if_else(dplyr::n() >= 2, sd(value, na.rm = TRUE), NA_real_),
      first_offset_min = dplyr::first(offset_min),
      last_offset_min = dplyr::last(offset_min),
      .groups = "drop"
    )
  
  expected <- tidyr::expand_grid(
    patientunitstayid = cohort_ids,
    variable = sort(unique(lab_long$variable)),
    window = factor(c("0-6h", "0-24h", "0-48h", "0-72h"), levels = c("0-6h", "0-24h", "0-48h", "0-72h"))
  )
  
  summary_full <- expected %>%
    dplyr::left_join(summary_df, by = c("patientunitstayid", "variable", "window")) %>%
    dplyr::mutate(
      n_measure = tidyr::replace_na(n_measure, 0L),
      available = as.integer(n_measure > 0)
    )
  
  summary_full
}

make_wide_features <- function(summary_df) {
  
  stat_cols <- c(
    "n_measure", "first_value", "last_value", "min_value",
    "max_value", "mean_value", "median_value", "sd_value"
  )
  
  summary_df %>%
    dplyr::mutate(
      window_clean = dplyr::case_when(
        window == "0-6h" ~ "T0_6h",
        window == "0-24h" ~ "T24",
        window == "0-48h" ~ "T48",
        window == "0-72h" ~ "T72",
        TRUE ~ as.character(window)
      )
    ) %>%
    dplyr::select(patientunitstayid, variable, window_clean, dplyr::all_of(stat_cols)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(stat_cols),
      names_to = "stat",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      feature_name = paste(variable, window_clean, stat, sep = "_")
    ) %>%
    dplyr::select(patientunitstayid, feature_name, value) %>%
    tidyr::pivot_wider(
      id_cols = patientunitstayid,
      names_from = feature_name,
      values_from = value
    )
}

make_availability_table <- function(summary_df, total_n) {
  
  summary_df %>%
    dplyr::group_by(variable, window) %>%
    dplyr::summarise(
      n_available = sum(available == 1, na.rm = TRUE),
      n_total = total_n,
      availability_pct = n_available / n_total * 100,
      missing_pct = 100 - availability_pct,
      median_n_measure_if_available = median(n_measure[n_measure > 0], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      median_n_measure_if_available = ifelse(
        is.infinite(median_n_measure_if_available),
        NA_real_,
        median_n_measure_if_available
      )
    ) %>%
    dplyr::arrange(variable, window)
}

theme_eicu <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = base_size + 7, hjust = 0),
      plot.subtitle = element_text(size = base_size + 1, color = "grey30", hjust = 0),
      axis.text = element_text(color = "black"),
      axis.title = element_text(face = "bold", color = "black"),
      legend.title = element_text(face = "bold"),
      plot.margin = margin(8, 10, 8, 8)
    )
}

## =========================================================
## 3. Load T48 cohort from Step E2A
## =========================================================

t48_e2a_file <- file.path(
  dir_e2a,
  "StepE2A_eICU_AMI_T48_cohort_demographics_comorbidities.csv"
)

if (!file.exists(t48_e2a_file)) {
  stop("Cannot find Step E2A T48 cohort file: ", t48_e2a_file)
}

ami_t48 <- readr::read_csv(t48_e2a_file, show_col_types = FALSE) %>%
  dplyr::mutate(patientunitstayid = as.character(patientunitstayid))

cohort_ids <- unique(ami_t48$patientunitstayid)

cat("T48 cohort size:", length(cohort_ids), "\n\n")

## =========================================================
## 4. DuckDB extraction from lab.csv.gz and customLab.csv.gz
## =========================================================

lab_path <- csv_gz("lab.csv.gz")
custom_lab_path <- csv_gz("customLab.csv.gz")

if (!file.exists(lab_path)) {
  stop("Cannot find lab.csv.gz")
}

if (!file.exists(custom_lab_path)) {
  warning("customLab.csv.gz not found. Only lab.csv.gz will be used.")
}

lab_names <- read_header(lab_path)

cat("lab.csv.gz columns:\n")
print(lab_names)

if (file.exists(custom_lab_path)) {
  custom_names <- read_header(custom_lab_path)
  cat("\ncustomLab.csv.gz columns:\n")
  print(custom_names)
} else {
  custom_names <- character(0)
}

duckdb_path <- file.path(dir_e2c, "StepE2C_lab_extract.duckdb")
if (file.exists(duckdb_path)) file.remove(duckdb_path)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = duckdb_path, read_only = FALSE)

DBI::dbWriteTable(
  con,
  "cohort_ids",
  data.frame(patientunitstayid = cohort_ids),
  overwrite = TRUE
)

extract_lab_source <- function(con, path, nm, source_name,
                               id_candidates,
                               offset_candidates,
                               name_candidates,
                               result_candidates,
                               result_text_candidates,
                               unit_candidates) {
  
  if (!file.exists(path)) {
    return(tibble::tibble())
  }
  
  id_col <- pick_col(nm, id_candidates)
  offset_col <- pick_col(nm, offset_candidates)
  name_col <- pick_col(nm, name_candidates)
  result_col <- pick_col(nm, result_candidates)
  
  if (is.na(id_col) || is.na(offset_col) || is.na(name_col) || is.na(result_col)) {
    warning(
      "Skipping ", source_name,
      " because required columns were not detected. id=", id_col,
      ", offset=", offset_col,
      ", name=", name_col,
      ", result=", result_col
    )
    return(tibble::tibble())
  }
  
  result_text_expr <- sql_col_or_null(nm, result_text_candidates, "lab_result_text", type = "varchar")
  unit_expr <- sql_col_or_null(nm, unit_candidates, "lab_unit", type = "varchar")
  
  sql <- sprintf(
    "
    SELECT
      CAST(x.%s AS VARCHAR) AS patientunitstayid,
      TRY_CAST(x.%s AS DOUBLE) AS offset_min,
      CAST(x.%s AS VARCHAR) AS lab_name,
      TRY_CAST(x.%s AS DOUBLE) AS value_num,
      CAST(x.%s AS VARCHAR) AS lab_result_raw,
      %s,
      %s,
      '%s' AS source
    FROM read_csv_auto('%s', union_by_name=true) x
    INNER JOIN cohort_ids c
      ON CAST(x.%s AS VARCHAR) = c.patientunitstayid
    WHERE TRY_CAST(x.%s AS DOUBLE) BETWEEN 0 AND 4320
    ",
    qid(id_col),
    qid(offset_col),
    qid(name_col),
    qid(result_col),
    qid(result_col),
    result_text_expr,
    unit_expr,
    source_name,
    sql_path(path),
    qid(id_col),
    qid(offset_col)
  )
  
  DBI::dbGetQuery(con, sql) %>%
    tibble::as_tibble()
}

cat("\nExtracting lab.csv.gz using DuckDB...\n")

lab_raw <- extract_lab_source(
  con = con,
  path = lab_path,
  nm = lab_names,
  source_name = "lab",
  id_candidates = c("patientunitstayid"),
  offset_candidates = c("labresultoffset", "laboffset", "observationoffset"),
  name_candidates = c("labname", "labName"),
  result_candidates = c("labresult", "labResult"),
  result_text_candidates = c("labresulttext", "labResultText"),
  unit_candidates = c("labmeasurenamesystem", "labmeasurenameinterface", "labunit", "unit")
)

cat("lab extracted rows:", nrow(lab_raw), "\n")

if (file.exists(custom_lab_path)) {
  cat("\nExtracting customLab.csv.gz using DuckDB...\n")
  
  custom_raw <- extract_lab_source(
    con = con,
    path = custom_lab_path,
    nm = custom_names,
    source_name = "customLab",
    id_candidates = c("patientunitstayid"),
    offset_candidates = c("labotheroffset", "customlaboffset", "observationoffset"),
    name_candidates = c("labothername", "customlabname", "labname"),
    result_candidates = c("labotherresult", "customlabresult", "labresult"),
    result_text_candidates = c("labotherresulttext", "customlabresulttext", "labresulttext"),
    unit_candidates = c("labotherunit", "customlabunit", "unit")
  )
  
  cat("customLab extracted rows:", nrow(custom_raw), "\n")
  
} else {
  custom_raw <- tibble::tibble()
}

DBI::dbDisconnect(con, shutdown = TRUE)

## =========================================================
## 5. Harmonize laboratory names and values
## =========================================================

lab_raw_all <- dplyr::bind_rows(lab_raw, custom_raw) %>%
  dplyr::mutate(
    patientunitstayid = as.character(patientunitstayid),
    offset_min = suppressWarnings(as.numeric(offset_min)),
    offset_hour = offset_min / 60,
    lab_name_clean = clean_text(lab_name),
    lab_result_raw = as.character(lab_result_raw),
    lab_result_text = as.character(lab_result_text),
    value_text_num = extract_first_number(dplyr::coalesce(lab_result_raw, lab_result_text)),
    value = dplyr::coalesce(value_num, value_text_num),
    variable = standardize_lab_variable(lab_name_clean)
  ) %>%
  dplyr::filter(
    !is.na(patientunitstayid),
    !is.na(offset_min),
    offset_min >= 0,
    offset_min <= 72 * 60
  )

lab_name_mapping_review <- lab_raw_all %>%
  dplyr::mutate(mapped = !is.na(variable)) %>%
  dplyr::group_by(source, lab_name, lab_name_clean, variable, lab_unit, mapped) %>%
  dplyr::summarise(
    n_records = dplyr::n(),
    n_patients = dplyr::n_distinct(patientunitstayid),
    median_value = median(value, na.rm = TRUE),
    min_value = suppressWarnings(min(value, na.rm = TRUE)),
    max_value = suppressWarnings(max(value, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    min_value = ifelse(is.infinite(min_value), NA_real_, min_value),
    max_value = ifelse(is.infinite(max_value), NA_real_, max_value)
  ) %>%
  dplyr::arrange(mapped, variable, dplyr::desc(n_records))

readr::write_csv(
  lab_name_mapping_review,
  file.path(dir_e2c, "StepE2C_lab_name_mapping_review.csv")
)

lab_long <- lab_raw_all %>%
  dplyr::filter(!is.na(variable)) %>%
  clean_lab_values() %>%
  dplyr::select(
    patientunitstayid,
    offset_min,
    offset_hour,
    source,
    lab_name,
    lab_name_clean,
    lab_unit,
    variable,
    value
  ) %>%
  dplyr::distinct() %>%
  dplyr::arrange(patientunitstayid, variable, offset_min)

readr::write_csv(
  lab_long,
  file.path(dir_e2c, "StepE2C_laboratory_long_T0_T72.csv")
)

cat("\nUnified laboratory long rows:", nrow(lab_long), "\n")
cat("Unique patients with any mapped lab:", dplyr::n_distinct(lab_long$patientunitstayid), "\n\n")

## =========================================================
## 6. Summary by window and wide features
## =========================================================

lab_summary <- summarise_labs_by_window(
  lab_long = lab_long,
  cohort_ids = cohort_ids
)

readr::write_csv(
  lab_summary,
  file.path(dir_e2c, "StepE2C_laboratory_summary_by_window.csv")
)

lab_wide <- make_wide_features(lab_summary)

ami_t48_labs_wide <- ami_t48 %>%
  dplyr::select(
    patientunitstayid,
    dplyr::any_of(c(
      "uniquepid", "age", "sex", "ethnicity",
      "primary_external_endpoint",
      "secondary_icu_mortality_after_T48",
      "in_icu_at_T48",
      "hospital_los_from_icu_hours",
      "icu_los_hours"
    ))
  ) %>%
  dplyr::left_join(lab_wide, by = "patientunitstayid")

readr::write_csv(
  lab_wide,
  file.path(dir_e2c, "StepE2C_laboratory_wide_features_only.csv")
)

readr::write_csv(
  ami_t48_labs_wide,
  file.path(dir_e2c, "StepE2C_eICU_AMI_T48_cohort_with_laboratory_features.csv")
)

## =========================================================
## 7. Availability and coverage tables
## =========================================================

lab_availability <- make_availability_table(
  summary_df = lab_summary,
  total_n = length(cohort_ids)
)

readr::write_csv(
  lab_availability,
  file.path(dir_tables, "StepE2C_laboratory_availability_by_window.csv")
)

lab_patient_coverage <- lab_long %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(
    n_patients_any_0_72h = dplyr::n_distinct(patientunitstayid),
    pct_patients_any_0_72h = n_patients_any_0_72h / length(cohort_ids) * 100,
    n_records = dplyr::n(),
    median_records_per_patient = median(as.numeric(table(patientunitstayid))),
    .groups = "drop"
  ) %>%
  dplyr::arrange(variable)

readr::write_csv(
  lab_patient_coverage,
  file.path(dir_tables, "StepE2C_laboratory_patient_coverage.csv")
)

## =========================================================
## 8. Visualization settings
## =========================================================

key_lab_vars <- c(
  "wbc",
  "hemoglobin",
  "platelet",
  "creatinine",
  "bun",
  "sodium",
  "potassium",
  "bicarbonate",
  "glucose",
  "lactate",
  "inr",
  "ptt",
  "troponin"
)

pretty_lab_names <- c(
  wbc = "WBC",
  hemoglobin = "Hemoglobin",
  platelet = "Platelet",
  creatinine = "Creatinine",
  bun = "BUN",
  sodium = "Sodium",
  potassium = "Potassium",
  bicarbonate = "Bicarbonate",
  glucose = "Glucose",
  lactate = "Lactate",
  inr = "INR",
  ptt = "PTT",
  troponin = "Troponin"
)

window_order <- c("0-6h", "0-24h", "0-48h", "0-72h")

## =========================================================
## 9. Figure E2C-1: lab missingness heatmap + right barplot
## =========================================================

lab_heat_df <- lab_availability %>%
  dplyr::filter(variable %in% key_lab_vars) %>%
  dplyr::mutate(
    variable_pretty = factor(
      pretty_lab_names[variable],
      levels = rev(pretty_lab_names[key_lab_vars])
    ),
    window = factor(window, levels = window_order),
    missing_label = sprintf("%.1f", missing_pct)
  )

lab_bar_df <- lab_heat_df %>%
  dplyr::group_by(variable_pretty) %>%
  dplyr::summarise(
    mean_missing_pct = mean(missing_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    variable_pretty = factor(variable_pretty, levels = levels(lab_heat_df$variable_pretty))
  )

readr::write_csv(
  lab_heat_df,
  file.path(dir_tables, "StepE2C_Figure_lab_missingness_heatmap_data.csv")
)

max_missing <- max(lab_heat_df$missing_pct, na.rm = TRUE)
max_missing_round <- max(50, ceiling(max_missing / 10) * 10)
midpoint_use <- 20

p_lab_heat <- ggplot(
  lab_heat_df,
  aes(x = window, y = variable_pretty, fill = missing_pct)
) +
  geom_tile(
    color = "white",
    linewidth = 0.9,
    width = 0.98,
    height = 0.98
  ) +
  geom_text(
    aes(label = missing_label),
    size = 4.0,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_gradient2(
    low = "#4F7FC4",
    mid = "#FFFFFF",
    high = "#B84A4A",
    midpoint = midpoint_use,
    limits = c(0, max_missing_round),
    name = "Missing (%)"
  ) +
  labs(x = NULL, y = NULL) +
  coord_fixed(ratio = 0.72, clip = "off") +
  theme_eicu(base_size = 15) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(face = "bold", size = 13, angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 14)
  )

p_lab_bar <- ggplot(
  lab_bar_df,
  aes(x = mean_missing_pct, y = variable_pretty)
) +
  geom_col(
    width = 0.75,
    fill = "#69A8D8",
    color = "white",
    linewidth = 0.35
  ) +
  geom_text(
    aes(label = sprintf("%.1f", mean_missing_pct)),
    hjust = -0.15,
    size = 3.8,
    fontface = "bold",
    color = "black"
  ) +
  scale_x_continuous(
    limits = c(0, max(lab_bar_df$mean_missing_pct, na.rm = TRUE) * 1.25),
    expand = c(0, 0)
  ) +
  labs(x = "Mean missing (%)", y = NULL) +
  theme_eicu(base_size = 15) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    panel.grid.major.x = element_line(color = "#E5E7EB", linewidth = 0.45)
  )

fig_lab_missingness <- p_lab_heat + p_lab_bar +
  patchwork::plot_layout(widths = c(4.2, 1.7)) +
  patchwork::plot_annotation(
    title = "Availability of laboratory features across early eICU windows",
    subtitle = "Cell values indicate missingness; right bars summarize mean missingness across windows",
    theme = theme(
      plot.title = element_text(face = "bold", size = 23, hjust = 0),
      plot.subtitle = element_text(size = 15.5, color = "grey30", hjust = 0),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 12, 10, 10)
    )
  )

print(fig_lab_missingness)

ggsave(
  file.path(fig_dir_avail, "FigureE2C_1_laboratory_missingness_heatmap_barplot.pdf"),
  fig_lab_missingness,
  width = 10.5,
  height = 7.2,
  device = cairo_pdf,
  bg = "white"
)

ggsave(
  file.path(fig_dir_avail, "FigureE2C_1_laboratory_missingness_heatmap_barplot.png"),
  fig_lab_missingness,
  width = 10.5,
  height = 7.2,
  dpi = 600,
  bg = "white"
)

## =========================================================
## 10. Figure E2C-2: lab measurement intensity heatmap
## =========================================================

lab_intensity_df <- lab_availability %>%
  dplyr::filter(variable %in% key_lab_vars) %>%
  dplyr::mutate(
    variable_pretty = factor(
      pretty_lab_names[variable],
      levels = rev(pretty_lab_names[key_lab_vars])
    ),
    window = factor(window, levels = window_order),
    intensity_label = ifelse(
      is.na(median_n_measure_if_available),
      "",
      sprintf("%.0f", median_n_measure_if_available)
    )
  )

readr::write_csv(
  lab_intensity_df,
  file.path(dir_tables, "StepE2C_Figure_lab_measurement_intensity_data.csv")
)

p_lab_intensity <- ggplot(
  lab_intensity_df,
  aes(x = window, y = variable_pretty, fill = median_n_measure_if_available)
) +
  geom_tile(
    color = "white",
    linewidth = 0.9,
    width = 0.98,
    height = 0.98
  ) +
  geom_text(
    aes(label = intensity_label),
    size = 4.0,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_gradientn(
    colours = c("#F8FAFC", "#DBEAFE", "#93C5FD", "#3B82F6", "#1E3A8A"),
    trans = "sqrt",
    name = "Median\nrecords"
  ) +
  labs(
    title = "Measurement intensity of laboratory features across early eICU windows",
    subtitle = "Cell values indicate the median number of measurements among patients with available records",
    x = NULL,
    y = NULL
  ) +
  coord_fixed(ratio = 0.72, clip = "off") +
  theme_eicu(base_size = 15) +
  theme(
    axis.text.x = element_text(face = "bold", size = 13, angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(p_lab_intensity)

ggsave(
  file.path(fig_dir_avail, "FigureE2C_2_laboratory_measurement_intensity_heatmap.pdf"),
  p_lab_intensity,
  width = 10.0,
  height = 7.0,
  device = cairo_pdf,
  bg = "white"
)

ggsave(
  file.path(fig_dir_avail, "FigureE2C_2_laboratory_measurement_intensity_heatmap.png"),
  p_lab_intensity,
  width = 10.0,
  height = 7.0,
  dpi = 600,
  bg = "white"
)

## =========================================================
## 11. Figure E2C-3: outcome-associated lab profile
## =========================================================

endpoint_df <- ami_t48 %>%
  dplyr::select(
    patientunitstayid,
    primary_external_endpoint
  ) %>%
  dplyr::mutate(patientunitstayid = as.character(patientunitstayid))

skewed_labs <- c(
  "creatinine", "bun", "glucose", "lactate",
  "inr", "ptt", "pt", "troponin",
  "bilirubin", "ast", "alt"
)

lab_profile_df <- lab_summary %>%
  dplyr::mutate(patientunitstayid = as.character(patientunitstayid)) %>%
  dplyr::filter(
    variable %in% key_lab_vars,
    available == 1,
    window %in% window_order
  ) %>%
  dplyr::left_join(endpoint_df, by = "patientunitstayid") %>%
  dplyr::filter(!is.na(primary_external_endpoint)) %>%
  dplyr::mutate(
    profile_value = dplyr::if_else(
      variable %in% skewed_labs,
      log1p(median_value),
      median_value
    )
  ) %>%
  dplyr::group_by(variable, window) %>%
  dplyr::summarise(
    n_total = dplyr::n(),
    n_death = sum(primary_external_endpoint == 1, na.rm = TRUE),
    survivor_mean = mean(profile_value[primary_external_endpoint == 0], na.rm = TRUE),
    death_mean = mean(profile_value[primary_external_endpoint == 1], na.rm = TRUE),
    survivor_median_raw = median(median_value[primary_external_endpoint == 0], na.rm = TRUE),
    death_median_raw = median(median_value[primary_external_endpoint == 1], na.rm = TRUE),
    pooled_sd = sd(profile_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    standardized_difference = (death_mean - survivor_mean) / pooled_sd,
    variable_pretty = factor(
      pretty_lab_names[variable],
      levels = rev(pretty_lab_names[key_lab_vars])
    ),
    window = factor(window, levels = window_order),
    label = sprintf("%.2f", standardized_difference)
  )

readr::write_csv(
  lab_profile_df,
  file.path(dir_tables, "StepE2C_Figure_outcome_associated_lab_profile_data.csv")
)

max_abs_smd <- max(abs(lab_profile_df$standardized_difference), na.rm = TRUE)

p_lab_profile <- ggplot(
  lab_profile_df,
  aes(x = window, y = variable_pretty, fill = standardized_difference)
) +
  geom_tile(
    color = "white",
    linewidth = 0.9,
    width = 0.98,
    height = 0.98
  ) +
  geom_text(
    aes(label = label),
    size = 3.8,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_gradient2(
    low = "#2563EB",
    mid = "#FFFFFF",
    high = "#DC2626",
    midpoint = 0,
    limits = c(-max_abs_smd, max_abs_smd),
    name = "Std. difference\n(death-survivor)"
  ) +
  labs(
    title = "Outcome-associated laboratory patterns in the eICU T48 cohort",
    subtitle = "Standardized mean differences compare patients who died after T48 with survivors; skewed markers were log-transformed",
    x = NULL,
    y = NULL
  ) +
  coord_fixed(ratio = 0.72, clip = "off") +
  theme_eicu(base_size = 15) +
  theme(
    axis.text.x = element_text(face = "bold", size = 13, angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(p_lab_profile)

ggsave(
  file.path(fig_dir_explore, "FigureE2C_3_outcome_associated_laboratory_profile_heatmap.pdf"),
  p_lab_profile,
  width = 10.5,
  height = 7.2,
  device = cairo_pdf,
  bg = "white"
)

ggsave(
  file.path(fig_dir_explore, "FigureE2C_3_outcome_associated_laboratory_profile_heatmap.png"),
  p_lab_profile,
  width = 10.5,
  height = 7.2,
  dpi = 600,
  bg = "white"
)

## =========================================================
## 12. Export Excel summary
## =========================================================

wb <- openxlsx::createWorkbook()

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

method_note <- tibble::tibble(
  item = c(
    "Purpose",
    "Cohort",
    "Time origin",
    "Extraction window",
    "Data sources",
    "Laboratory variables",
    "Window summaries",
    "Troponin handling",
    "Downstream use"
  ),
  description = c(
    "Extract laboratory features for the eICU AMI-EDRS external validation cohort.",
    "Primary T48 external validation cohort from Step E1/E2A.",
    "T0 was defined as ICU admission; eICU laboratory offsets were used as minutes from ICU admission.",
    "Laboratory measurements were extracted from 0 to 72 hours after ICU admission.",
    "lab.csv.gz and customLab.csv.gz.",
    "WBC, hemoglobin, platelet, creatinine, BUN, sodium, potassium, bicarbonate, glucose, lactate, INR, PTT, PT, troponin, albumin, bilirubin, AST, and ALT were mapped when available.",
    "For each laboratory variable and window, first, last, min, max, mean, median, SD, and measurement count were calculated.",
    "Troponin I, troponin T, and generic troponin records were harmonized into a generic troponin marker for external validation.",
    "These features will be used for organ-domain score reconstruction, trajectory transfer, AMI-EDRS external prediction, and benchmark comparison."
  )
)

add_sheet(wb, "method_note", method_note)
add_sheet(wb, "availability", lab_availability)
add_sheet(wb, "patient_coverage", lab_patient_coverage)
add_sheet(wb, "lab_name_mapping", lab_name_mapping_review)
add_sheet(wb, "missingness_plot_data", lab_heat_df)
add_sheet(wb, "outcome_profile_data", lab_profile_df)
add_sheet(wb, "summary_preview", head(lab_summary, 5000))
add_sheet(wb, "wide_preview", head(ami_t48_labs_wide, 5000))

out_xlsx <- file.path(
  dir_e2c,
  "StepE2C_laboratory_extraction_summary.xlsx"
)

openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

## =========================================================
## 13. Save RDS object
## =========================================================

stepE2C_object <- list(
  ami_t48 = ami_t48,
  lab_raw_dim = dim(lab_raw),
  custom_raw_dim = dim(custom_raw),
  lab_name_mapping_review = lab_name_mapping_review,
  lab_long = lab_long,
  lab_summary = lab_summary,
  lab_wide = lab_wide,
  ami_t48_labs_wide = ami_t48_labs_wide,
  lab_availability = lab_availability,
  lab_patient_coverage = lab_patient_coverage,
  lab_heat_df = lab_heat_df,
  lab_profile_df = lab_profile_df,
  key_lab_vars = key_lab_vars,
  pretty_lab_names = pretty_lab_names,
  created_at = Sys.time()
)

saveRDS(
  stepE2C_object,
  file.path(dir_objects, "StepE2C_laboratory_object.rds")
)

## =========================================================
## 14. Save session info
## =========================================================

sink(file.path(dir_logs, "StepE2C_sessionInfo.txt"))
cat("Step E2C session information\n")
cat("============================\n\n")
print(sessionInfo())
sink()

## =========================================================
## 15. Console summary
## =========================================================

cat("\n================ Step E2C finished ================\n")
cat("T48 cohort:", length(cohort_ids), "\n")
cat("lab.csv extracted rows:", nrow(lab_raw), "\n")
cat("customLab.csv extracted rows:", nrow(custom_raw), "\n")
cat("Unified laboratory long rows:", nrow(lab_long), "\n")
cat("Patients with any mapped lab:", dplyr::n_distinct(lab_long$patientunitstayid), "\n\n")

cat("Laboratory availability by window:\n")
print(lab_availability, n = 100)

cat("\nMain outputs:\n")
cat(file.path(dir_e2c, "StepE2C_laboratory_long_T0_T72.csv"), "\n")
cat(file.path(dir_e2c, "StepE2C_laboratory_summary_by_window.csv"), "\n")
cat(file.path(dir_e2c, "StepE2C_laboratory_wide_features_only.csv"), "\n")
cat(file.path(dir_e2c, "StepE2C_eICU_AMI_T48_cohort_with_laboratory_features.csv"), "\n")
cat(file.path(dir_tables, "StepE2C_laboratory_availability_by_window.csv"), "\n")
cat(out_xlsx, "\n")
cat(file.path(dir_objects, "StepE2C_laboratory_object.rds"), "\n\n")

cat("Figures:\n")
cat(file.path(fig_dir_avail, "FigureE2C_1_laboratory_missingness_heatmap_barplot.pdf"), "\n")
cat(file.path(fig_dir_avail, "FigureE2C_2_laboratory_measurement_intensity_heatmap.pdf"), "\n")
cat(file.path(fig_dir_explore, "FigureE2C_3_outcome_associated_laboratory_profile_heatmap.pdf"), "\n")
cat("===================================================\n")

