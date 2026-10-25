############################################################
## Step E2D-QC2: Recover IABP/MCS proxy after overly strict cleaning
## Purpose:
##   Keep cleaned proxy results, but re-identify IABP/MCS using broader
##   treatment-source text patterns.
############################################################

rm(list = ls())
gc()

options(stringsAsFactors = FALSE)

pkg_needed <- c(
  "dplyr", "tidyr", "stringr", "tibble",
  "readr", "openxlsx"
)

for (p in pkg_needed) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

invisible(lapply(pkg_needed, library, character.only = TRUE))

project_dir <- Sys.getenv("AMI_EICU_PROJECT", unset=file.path(getwd(), "work/eICU"))

config_file <- file.path(project_dir, "00_admin", "StepE0_path_config.rds")
path_config <- readRDS(config_file)
dirs <- path_config$dirs

dir_objects <- dirs$objects
dir_feature <- dirs$features
dir_e2d <- file.path(dir_feature, "E2D_support_treatment_proxies")
dir_tables <- file.path(dirs$tables, "feature_tables")

dir.create(dir_e2d, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)

obj_raw_file <- file.path(dir_objects, "StepE2D_support_treatment_proxy_object.rds")
obj_clean_file <- file.path(dir_objects, "StepE2D_QC_cleaned_support_treatment_proxy_object.rds")

if (!file.exists(obj_raw_file)) stop("Cannot find raw E2D object: ", obj_raw_file)
if (!file.exists(obj_clean_file)) stop("Cannot find cleaned E2D object: ", obj_clean_file)

obj_raw <- readRDS(obj_raw_file)
obj_clean <- readRDS(obj_clean_file)

proxy_raw <- obj_raw$proxy_evidence_all
proxy_clean <- obj_clean$proxy_clean
ami_t48 <- obj_clean$ami_t48

cohort_ids <- unique(as.character(ami_t48$patientunitstayid))

clean_text <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_replace_all("[\r\n\t]", " ") %>%
    stringr::str_squish() %>%
    tolower()
}

standardize_window <- function(df) {
  df %>%
    dplyr::mutate(
      window_6h  = dplyr::if_else(offset_min >= 0 & offset_min <= 6  * 60, "0-6h",  NA_character_),
      window_24h = dplyr::if_else(offset_min >= 0 & offset_min <= 24 * 60, "0-24h", NA_character_),
      window_48h = dplyr::if_else(offset_min >= 0 & offset_min <= 48 * 60, "0-48h", NA_character_),
      window_72h = dplyr::if_else(offset_min >= 0 & offset_min <= 72 * 60, "0-72h", NA_character_)
    )
}

## Recover IABP/MCS from raw evidence, but only trusted sources
iabp_recovered <- proxy_raw %>%
  dplyr::mutate(
    patientunitstayid = as.character(patientunitstayid),
    offset_min = suppressWarnings(as.numeric(offset_min)),
    offset_hour = offset_min / 60,
    source = as.character(source),
    proxy = as.character(proxy),
    evidence_text = as.character(evidence_text),
    evidence_clean = clean_text(evidence_text),
    value = suppressWarnings(as.numeric(value))
  ) %>%
  dplyr::filter(
    patientunitstayid %in% cohort_ids,
    proxy == "iabp_mcs",
    source %in% c("treatment", "nurseCharting", "intakeOutput"),
    !is.na(offset_min),
    offset_min >= 0,
    offset_min <= 72 * 60,
    stringr::str_detect(
      evidence_clean,
      paste(
        c(
          "iabp",
          "iabc",
          "intra[- ]?aortic",
          "intraaortic",
          "balloon pump",
          "aortic balloon",
          "impella",
          "ecmo",
          "extracorporeal membrane",
          "mechanical circulatory support",
          "ventricular assist",
          "\\blvad\\b",
          "\\brvad\\b",
          "\\bvad\\b"
        ),
        collapse = "|"
      )
    )
  ) %>%
  dplyr::transmute(
    patientunitstayid,
    offset_min,
    offset_hour,
    source,
    proxy = "iabp_mcs",
    value,
    evidence_text,
    binary_value = 1L
  ) %>%
  dplyr::distinct()

cat("Recovered IABP/MCS rows:", nrow(iabp_recovered), "\n")
cat("Recovered IABP/MCS patients:", dplyr::n_distinct(iabp_recovered$patientunitstayid), "\n\n")

## Replace old cleaned iabp_mcs with recovered version
proxy_qc2 <- proxy_clean %>%
  dplyr::filter(proxy != "iabp_mcs") %>%
  dplyr::bind_rows(iabp_recovered) %>%
  dplyr::distinct()

proxy_order <- c(
  "gcs_total",
  "gcs_eye",
  "gcs_verbal",
  "gcs_motor",
  "oxygen_support",
  "mechanical_ventilation",
  "vasopressor",
  "urine_output",
  "dialysis",
  "pci_cath",
  "iabp_mcs"
)

numeric_proxy <- c("gcs_total", "gcs_eye", "gcs_verbal", "gcs_motor")
binary_proxy <- setdiff(proxy_order, numeric_proxy)

proxy_window_long <- proxy_qc2 %>%
  standardize_window() %>%
  tidyr::pivot_longer(
    cols = c(window_6h, window_24h, window_48h, window_72h),
    names_to = "window_type",
    values_to = "window"
  ) %>%
  dplyr::filter(!is.na(window)) %>%
  dplyr::mutate(
    window = factor(window, levels = c("0-6h", "0-24h", "0-48h", "0-72h"))
  )

proxy_numeric_summary <- proxy_window_long %>%
  dplyr::filter(proxy %in% numeric_proxy, !is.na(value)) %>%
  dplyr::arrange(patientunitstayid, proxy, window, offset_min) %>%
  dplyr::group_by(patientunitstayid, proxy, window) %>%
  dplyr::summarise(
    n_measure = dplyr::n(),
    first_value = dplyr::first(value),
    last_value = dplyr::last(value),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    any_flag = 1L,
    .groups = "drop"
  )

proxy_binary_summary <- proxy_window_long %>%
  dplyr::filter(proxy %in% binary_proxy) %>%
  dplyr::group_by(patientunitstayid, proxy, window) %>%
  dplyr::summarise(
    n_measure = dplyr::n(),
    first_value = 1,
    last_value = 1,
    min_value = 1,
    max_value = 1,
    mean_value = 1,
    median_value = 1,
    any_flag = 1L,
    .groups = "drop"
  )

proxy_summary <- dplyr::bind_rows(
  proxy_numeric_summary,
  proxy_binary_summary
)

expected_proxy <- tidyr::expand_grid(
  patientunitstayid = cohort_ids,
  proxy = proxy_order,
  window = factor(
    c("0-6h", "0-24h", "0-48h", "0-72h"),
    levels = c("0-6h", "0-24h", "0-48h", "0-72h")
  )
)

proxy_summary_qc2 <- expected_proxy %>%
  dplyr::left_join(proxy_summary, by = c("patientunitstayid", "proxy", "window")) %>%
  dplyr::mutate(
    n_measure = tidyr::replace_na(n_measure, 0L),
    any_flag = tidyr::replace_na(any_flag, 0L),
    available = as.integer(n_measure > 0)
  )

proxy_wide_qc2 <- proxy_summary_qc2 %>%
  dplyr::mutate(
    window_clean = dplyr::case_when(
      window == "0-6h"  ~ "T0_6h",
      window == "0-24h" ~ "T24",
      window == "0-48h" ~ "T48",
      window == "0-72h" ~ "T72",
      TRUE ~ as.character(window)
    )
  ) %>%
  dplyr::select(
    patientunitstayid,
    proxy,
    window_clean,
    n_measure,
    first_value,
    last_value,
    min_value,
    max_value,
    mean_value,
    median_value,
    any_flag
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      n_measure,
      first_value,
      last_value,
      min_value,
      max_value,
      mean_value,
      median_value,
      any_flag
    ),
    names_to = "stat",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    feature_name = paste(proxy, window_clean, stat, sep = "_")
  ) %>%
  dplyr::select(patientunitstayid, feature_name, value) %>%
  tidyr::pivot_wider(
    id_cols = patientunitstayid,
    names_from = feature_name,
    values_from = value
  )

ami_t48_proxy_wide_qc2 <- ami_t48 %>%
  dplyr::select(
    patientunitstayid,
    dplyr::any_of(c(
      "uniquepid",
      "age",
      "sex",
      "ethnicity",
      "primary_external_endpoint",
      "secondary_icu_mortality_after_T48",
      "in_icu_at_T48",
      "hospital_los_from_icu_hours",
      "icu_los_hours"
    ))
  ) %>%
  dplyr::left_join(proxy_wide_qc2, by = "patientunitstayid")

proxy_availability_qc2 <- proxy_summary_qc2 %>%
  dplyr::group_by(proxy, window) %>%
  dplyr::summarise(
    n_available = sum(available == 1, na.rm = TRUE),
    n_total = length(cohort_ids),
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
  dplyr::arrange(proxy, window)

proxy_evidence_summary_qc2 <- proxy_qc2 %>%
  dplyr::group_by(proxy, source) %>%
  dplyr::summarise(
    n_records = dplyr::n(),
    n_patients = dplyr::n_distinct(patientunitstayid),
    example_text = paste(utils::head(unique(evidence_text), 3), collapse = " || "),
    .groups = "drop"
  ) %>%
  dplyr::arrange(proxy, dplyr::desc(n_patients))

readr::write_csv(
  proxy_qc2,
  file.path(dir_e2d, "StepE2D_QC2_proxy_evidence_long_T0_T72.csv")
)

readr::write_csv(
  proxy_summary_qc2,
  file.path(dir_e2d, "StepE2D_QC2_proxy_summary_by_window.csv")
)

readr::write_csv(
  proxy_wide_qc2,
  file.path(dir_e2d, "StepE2D_QC2_support_treatment_proxy_wide_features_only.csv")
)

readr::write_csv(
  ami_t48_proxy_wide_qc2,
  file.path(dir_e2d, "StepE2D_QC2_eICU_AMI_T48_cohort_with_support_treatment_features.csv")
)

readr::write_csv(
  proxy_availability_qc2,
  file.path(dir_tables, "StepE2D_QC2_support_treatment_proxy_availability_by_window.csv")
)

readr::write_csv(
  proxy_evidence_summary_qc2,
  file.path(dir_tables, "StepE2D_QC2_support_treatment_proxy_evidence_summary.csv")
)

qc2_object <- list(
  ami_t48 = ami_t48,
  proxy_qc2 = proxy_qc2,
  iabp_recovered = iabp_recovered,
  proxy_summary_qc2 = proxy_summary_qc2,
  proxy_wide_qc2 = proxy_wide_qc2,
  ami_t48_proxy_wide_qc2 = ami_t48_proxy_wide_qc2,
  proxy_availability_qc2 = proxy_availability_qc2,
  proxy_evidence_summary_qc2 = proxy_evidence_summary_qc2,
  created_at = Sys.time()
)

saveRDS(
  qc2_object,
  file.path(dir_objects, "StepE2D_QC2_support_treatment_proxy_object.rds")
)

wb <- openxlsx::createWorkbook()

add_sheet <- function(wb, sheet_name, df) {
  sheet_name <- substr(sheet_name, 1, 31)
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeDataTable(wb, sheet = sheet_name, x = df, tableStyle = "TableStyleMedium2")
  openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:ncol(df), widths = "auto")
}

method_note <- tibble::tibble(
  item = c(
    "Purpose",
    "Correction",
    "Downstream use"
  ),
  description = c(
    "Recover IABP/MCS proxy after overly strict E2D-QC cleaning.",
    "IABP/MCS evidence was re-identified from trusted sources using broader text patterns including IABP, intra-aortic, intraaortic, balloon pump, Impella, ECMO, and VAD-related terms.",
    "Use QC2 wide features and QC2 object for Step E3 organ-domain score reconstruction."
  )
)

add_sheet(wb, "method_note", method_note)
add_sheet(wb, "availability_qc2", proxy_availability_qc2)
add_sheet(wb, "evidence_summary_qc2", proxy_evidence_summary_qc2)
add_sheet(wb, "iabp_recovered", iabp_recovered)
add_sheet(wb, "wide_preview", head(ami_t48_proxy_wide_qc2, 5000))

openxlsx::saveWorkbook(
  wb,
  file.path(dir_e2d, "StepE2D_QC2_support_treatment_proxy_summary.xlsx"),
  overwrite = TRUE
)

cat("\n================ Step E2D-QC2 finished ================\n")
cat("Original cleaned proxy rows:", nrow(proxy_clean), "\n")
cat("QC2 proxy rows:", nrow(proxy_qc2), "\n")
cat("Recovered IABP/MCS patients:", dplyr::n_distinct(iabp_recovered$patientunitstayid), "\n\n")

cat("QC2 proxy availability:\n")
print(proxy_availability_qc2, n = 100)

cat("\nQC2 evidence summary:\n")
print(proxy_evidence_summary_qc2, n = 100)

cat("\nMain outputs for Step E3:\n")
cat(file.path(dir_e2d, "StepE2D_QC2_support_treatment_proxy_wide_features_only.csv"), "\n")
cat(file.path(dir_e2d, "StepE2D_QC2_eICU_AMI_T48_cohort_with_support_treatment_features.csv"), "\n")
cat(file.path(dir_objects, "StepE2D_QC2_support_treatment_proxy_object.rds"), "\n")
cat("========================================================\n")

