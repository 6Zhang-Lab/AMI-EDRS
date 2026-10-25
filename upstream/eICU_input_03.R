############################################################
## Step E2B: eICU vital signs extraction for AMI-EDRS external validation
##
## Aim:
##   Extract vital signs from T0 to T72 after ICU admission
##   for the eICU AMI T48 external validation cohort.
##
## Input:
##   StepE0_path_config.rds
##   StepE2A_eICU_AMI_T48_cohort_demographics_comorbidities.csv
##   vitalPeriodic.csv.gz
##   vitalAperiodic.csv.gz
##
## Output:
##   1. Vital signs long table
##   2. Window-level summary table
##   3. Patient-level wide table
##   4. Missingness / availability table
##   5. RDS object for downstream steps
############################################################

rm(list = ls())
gc()

options(stringsAsFactors = FALSE)

## =========================================================
## 0. Packages
## =========================================================

pkg_needed <- c(
  "dplyr", "tidyr", "stringr", "purrr", "tibble",
  "data.table", "readr", "openxlsx", "DBI", "duckdb"
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
dir_e2b <- file.path(dir_feature, "E2B_vital_signs")

dir.create(dir_e2b, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_objects, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_logs, recursive = TRUE, showWarnings = FALSE)

csv_gz <- function(file_name) {
  file.path(raw_eicu_dir, file_name)
}

cat("Raw eICU folder:\n", raw_eicu_dir, "\n\n")
cat("Step E2B output folder:\n", dir_e2b, "\n\n")

## =========================================================
## 2. Helper functions
## =========================================================

sql_path <- function(path) {
  gsub("'", "''", normalizePath(path, mustWork = TRUE))
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

make_select_expr <- function(nm, mapping) {
  exprs <- purrr::imap_chr(mapping, function(candidates, out_name) {
    hit <- pick_col(nm, candidates)
    if (is.na(hit)) {
      paste0("CAST(NULL AS DOUBLE) AS ", out_name)
    } else {
      paste0("TRY_CAST(", hit, " AS DOUBLE) AS ", out_name)
    }
  })
  paste(exprs, collapse = ",\n      ")
}

clean_vital_values <- function(df) {
  df %>%
    dplyr::mutate(
      value = suppressWarnings(as.numeric(value)),
      value = dplyr::case_when(
        variable == "heart_rate" & (value < 20 | value > 250) ~ NA_real_,
        variable == "resp_rate" & (value < 1 | value > 80) ~ NA_real_,
        variable == "spo2" & (value < 40 | value > 100) ~ NA_real_,
        variable == "temperature" & value > 70 ~ (value - 32) * 5 / 9,
        TRUE ~ value
      ),
      value = dplyr::case_when(
        variable == "temperature" & (value < 25 | value > 45) ~ NA_real_,
        variable %in% c("sbp", "dbp", "map") & (value < 20 | value > 300) ~ NA_real_,
        variable == "sbp" & value < 40 ~ NA_real_,
        variable == "dbp" & value < 20 ~ NA_real_,
        variable == "map" & value < 20 ~ NA_real_,
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

summarise_vitals_by_window <- function(vital_long, cohort_ids) {
  
  window_long <- vital_long %>%
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
    variable = sort(unique(vital_long$variable)),
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
## 4. DuckDB extraction: vitalPeriodic
## =========================================================

vital_periodic_path <- csv_gz("vitalPeriodic.csv.gz")
vital_aperiodic_path <- csv_gz("vitalAperiodic.csv.gz")

if (!file.exists(vital_periodic_path)) {
  stop("Cannot find vitalPeriodic.csv.gz")
}
if (!file.exists(vital_aperiodic_path)) {
  stop("Cannot find vitalAperiodic.csv.gz")
}

vp_names <- read_header(vital_periodic_path)
va_names <- read_header(vital_aperiodic_path)

cat("vitalPeriodic columns:\n")
print(vp_names)

cat("\nvitalAperiodic columns:\n")
print(va_names)

vp_id_col <- pick_col(vp_names, c("patientunitstayid"))
vp_offset_col <- pick_col(vp_names, c("observationoffset"))

va_id_col <- pick_col(va_names, c("patientunitstayid"))
va_offset_col <- pick_col(va_names, c("observationoffset"))

if (is.na(vp_id_col) || is.na(vp_offset_col)) {
  stop("Cannot detect patientunitstayid or observationoffset in vitalPeriodic.")
}
if (is.na(va_id_col) || is.na(va_offset_col)) {
  stop("Cannot detect patientunitstayid or observationoffset in vitalAperiodic.")
}

vp_mapping <- list(
  heart_rate = c("heartrate", "heartRate", "hr"),
  resp_rate = c("respiration", "respiratoryrate", "respiratoryRate", "resp_rate"),
  spo2 = c("sao2", "spo2", "oxygenSaturation"),
  temperature = c("temperature", "temp"),
  sbp_invasive = c("systemicsystolic", "systemicSystolic"),
  dbp_invasive = c("systemicdiastolic", "systemicDiastolic"),
  map_invasive = c("systemicmean", "systemicMean")
)

va_mapping <- list(
  sbp_noninvasive = c("noninvasivesystolic", "nonInvasiveSystolic"),
  dbp_noninvasive = c("noninvasivediastolic", "nonInvasiveDiastolic"),
  map_noninvasive = c("noninvasivemean", "nonInvasiveMean")
)

vp_select_expr <- make_select_expr(vp_names, vp_mapping)
va_select_expr <- make_select_expr(va_names, va_mapping)

duckdb_path <- file.path(dir_e2b, "StepE2B_vitals_extract.duckdb")
if (file.exists(duckdb_path)) file.remove(duckdb_path)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = duckdb_path, read_only = FALSE)

DBI::dbWriteTable(
  con,
  "cohort_ids",
  data.frame(patientunitstayid = cohort_ids),
  overwrite = TRUE
)

cat("\nExtracting vitalPeriodic using DuckDB...\n")

sql_vp <- sprintf(
  "
  SELECT
    CAST(vp.%s AS VARCHAR) AS patientunitstayid,
    TRY_CAST(vp.%s AS DOUBLE) AS offset_min,
    %s
  FROM read_csv_auto('%s', union_by_name=true) vp
  INNER JOIN cohort_ids c
    ON CAST(vp.%s AS VARCHAR) = c.patientunitstayid
  WHERE TRY_CAST(vp.%s AS DOUBLE) BETWEEN 0 AND 4320
  ",
  vp_id_col,
  vp_offset_col,
  vp_select_expr,
  sql_path(vital_periodic_path),
  vp_id_col,
  vp_offset_col
)

vp_raw <- DBI::dbGetQuery(con, sql_vp) %>%
  tibble::as_tibble()

cat("vitalPeriodic extracted rows:", nrow(vp_raw), "\n")

cat("\nExtracting vitalAperiodic using DuckDB...\n")

sql_va <- sprintf(
  "
  SELECT
    CAST(va.%s AS VARCHAR) AS patientunitstayid,
    TRY_CAST(va.%s AS DOUBLE) AS offset_min,
    %s
  FROM read_csv_auto('%s', union_by_name=true) va
  INNER JOIN cohort_ids c
    ON CAST(va.%s AS VARCHAR) = c.patientunitstayid
  WHERE TRY_CAST(va.%s AS DOUBLE) BETWEEN 0 AND 4320
  ",
  va_id_col,
  va_offset_col,
  va_select_expr,
  sql_path(vital_aperiodic_path),
  va_id_col,
  va_offset_col
)

va_raw <- DBI::dbGetQuery(con, sql_va) %>%
  tibble::as_tibble()

cat("vitalAperiodic extracted rows:", nrow(va_raw), "\n")

DBI::dbDisconnect(con, shutdown = TRUE)

## =========================================================
## 5. Convert to unified vital long table
## =========================================================

vp_long <- vp_raw %>%
  tidyr::pivot_longer(
    cols = -c(patientunitstayid, offset_min),
    names_to = "raw_variable",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    source = "vitalPeriodic",
    variable = dplyr::case_when(
      raw_variable == "heart_rate" ~ "heart_rate",
      raw_variable == "resp_rate" ~ "resp_rate",
      raw_variable == "spo2" ~ "spo2",
      raw_variable == "temperature" ~ "temperature",
      raw_variable == "sbp_invasive" ~ "sbp",
      raw_variable == "dbp_invasive" ~ "dbp",
      raw_variable == "map_invasive" ~ "map",
      TRUE ~ raw_variable
    )
  ) %>%
  dplyr::select(patientunitstayid, offset_min, source, raw_variable, variable, value)

va_long <- va_raw %>%
  tidyr::pivot_longer(
    cols = -c(patientunitstayid, offset_min),
    names_to = "raw_variable",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    source = "vitalAperiodic",
    variable = dplyr::case_when(
      raw_variable == "sbp_noninvasive" ~ "sbp",
      raw_variable == "dbp_noninvasive" ~ "dbp",
      raw_variable == "map_noninvasive" ~ "map",
      TRUE ~ raw_variable
    )
  ) %>%
  dplyr::select(patientunitstayid, offset_min, source, raw_variable, variable, value)

vital_long <- dplyr::bind_rows(vp_long, va_long) %>%
  dplyr::mutate(
    patientunitstayid = as.character(patientunitstayid),
    offset_min = suppressWarnings(as.numeric(offset_min)),
    offset_hour = offset_min / 60
  ) %>%
  dplyr::filter(
    !is.na(patientunitstayid),
    !is.na(offset_min),
    offset_min >= 0,
    offset_min <= 72 * 60
  ) %>%
  clean_vital_values() %>%
  dplyr::arrange(patientunitstayid, variable, offset_min)

readr::write_csv(
  vital_long,
  file.path(dir_e2b, "StepE2B_vital_signs_long_T0_T72.csv")
)

cat("\nUnified vital long rows:", nrow(vital_long), "\n")
cat("Unique patients with any vital:", dplyr::n_distinct(vital_long$patientunitstayid), "\n\n")

## =========================================================
## 6. Summary by window and wide features
## =========================================================

vital_summary <- summarise_vitals_by_window(
  vital_long = vital_long,
  cohort_ids = cohort_ids
)

readr::write_csv(
  vital_summary,
  file.path(dir_e2b, "StepE2B_vital_signs_summary_by_window.csv")
)

vital_wide <- make_wide_features(vital_summary)

ami_t48_vitals_wide <- ami_t48 %>%
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
  dplyr::left_join(vital_wide, by = "patientunitstayid")

readr::write_csv(
  vital_wide,
  file.path(dir_e2b, "StepE2B_vital_signs_wide_features_only.csv")
)

readr::write_csv(
  ami_t48_vitals_wide,
  file.path(dir_e2b, "StepE2B_eICU_AMI_T48_cohort_with_vital_features.csv")
)

## =========================================================
## 7. Availability table
## =========================================================

vital_availability <- make_availability_table(
  summary_df = vital_summary,
  total_n = length(cohort_ids)
)

readr::write_csv(
  vital_availability,
  file.path(dir_tables, "StepE2B_vital_signs_availability_by_window.csv")
)

vital_patient_coverage <- vital_long %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(
    n_patients_any_0_72h = dplyr::n_distinct(patientunitstayid),
    pct_patients_any_0_72h = n_patients_any_0_72h / length(cohort_ids) * 100,
    n_records = dplyr::n(),
    median_records_per_patient = median(table(patientunitstayid)),
    .groups = "drop"
  ) %>%
  dplyr::arrange(variable)

readr::write_csv(
  vital_patient_coverage,
  file.path(dir_tables, "StepE2B_vital_signs_patient_coverage.csv")
)

## =========================================================
## 8. Export Excel summary
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
    "Variables",
    "Window summaries",
    "Temperature handling",
    "Downstream use"
  ),
  description = c(
    "Extract vital signs for the eICU AMI-EDRS external validation cohort.",
    "Primary T48 external validation cohort from Step E1/E2A.",
    "T0 was defined as ICU admission; eICU observationoffset was used as minutes from ICU admission.",
    "Vital signs were extracted from 0 to 72 hours after ICU admission.",
    "vitalPeriodic.csv.gz and vitalAperiodic.csv.gz.",
    "Heart rate, respiratory rate, SpO2, temperature, systolic blood pressure, diastolic blood pressure, and mean arterial pressure.",
    "For each variable and window, first, last, min, max, mean, median, SD, and measurement count were calculated.",
    "Temperature values >70 were interpreted as Fahrenheit and converted to Celsius; implausible values were removed.",
    "These features will be used for organ-domain score reconstruction and AMI-EDRS external prediction."
  )
)

add_sheet(wb, "method_note", method_note)
add_sheet(wb, "availability", vital_availability)
add_sheet(wb, "patient_coverage", vital_patient_coverage)
add_sheet(wb, "summary_preview", head(vital_summary, 5000))
add_sheet(wb, "wide_preview", head(ami_t48_vitals_wide, 5000))

out_xlsx <- file.path(
  dir_e2b,
  "StepE2B_vital_signs_extraction_summary.xlsx"
)

openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

## =========================================================
## 9. Save RDS object
## =========================================================

stepE2B_object <- list(
  ami_t48 = ami_t48,
  vp_raw_dim = dim(vp_raw),
  va_raw_dim = dim(va_raw),
  vital_long = vital_long,
  vital_summary = vital_summary,
  vital_wide = vital_wide,
  ami_t48_vitals_wide = ami_t48_vitals_wide,
  vital_availability = vital_availability,
  vital_patient_coverage = vital_patient_coverage,
  vp_mapping = vp_mapping,
  va_mapping = va_mapping,
  created_at = Sys.time()
)

saveRDS(
  stepE2B_object,
  file.path(dir_objects, "StepE2B_vital_signs_object.rds")
)

## =========================================================
## 10. Save session info
## =========================================================

sink(file.path(dir_logs, "StepE2B_sessionInfo.txt"))
cat("Step E2B session information\n")
cat("============================\n\n")
print(sessionInfo())
sink()

## =========================================================
## 11. Console summary
## =========================================================

cat("\n================ Step E2B finished ================\n")
cat("T48 cohort:", length(cohort_ids), "\n")
cat("vitalPeriodic extracted rows:", nrow(vp_raw), "\n")
cat("vitalAperiodic extracted rows:", nrow(va_raw), "\n")
cat("Unified vital long rows:", nrow(vital_long), "\n")
cat("Patients with any vital:", dplyr::n_distinct(vital_long$patientunitstayid), "\n\n")

cat("Vital availability by window:\n")
print(vital_availability)

cat("\nMain outputs:\n")
cat(file.path(dir_e2b, "StepE2B_vital_signs_long_T0_T72.csv"), "\n")
cat(file.path(dir_e2b, "StepE2B_vital_signs_summary_by_window.csv"), "\n")
cat(file.path(dir_e2b, "StepE2B_vital_signs_wide_features_only.csv"), "\n")
cat(file.path(dir_e2b, "StepE2B_eICU_AMI_T48_cohort_with_vital_features.csv"), "\n")
cat(file.path(dir_tables, "StepE2B_vital_signs_availability_by_window.csv"), "\n")
cat(out_xlsx, "\n")
cat(file.path(dir_objects, "StepE2B_vital_signs_object.rds"), "\n")
cat("===================================================\n")

