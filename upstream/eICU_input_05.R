############################################################
## Step E2D v2 robust: eICU support/treatment/proxy extraction
## Project: eICU AMI-EDRS external validation
## Fix: robust DuckDB CSV parsing for respiratoryCharting.csv.gz and other irregular CSV files
############################################################

rm(list = ls())
gc()
options(stringsAsFactors = FALSE)

pkg_needed <- c(
  "dplyr", "tidyr", "stringr", "purrr", "tibble",
  "data.table", "readr", "openxlsx", "DBI", "duckdb",
  "ggplot2", "patchwork", "scales"
)
for (p in pkg_needed) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkg_needed, library, character.only = TRUE))

## =========================================================
## 1. Paths
## =========================================================
project_dir <- Sys.getenv("AMI_EICU_PROJECT", unset=file.path(getwd(), "work/eICU"))
config_file <- file.path(project_dir, "00_admin", "StepE0_path_config.rds")
if (!file.exists(config_file)) stop("Cannot find StepE0 path config: ", config_file)
path_config <- readRDS(config_file)
raw_eicu_dir <- path_config$raw_eicu_dir
dirs <- path_config$dirs

dir_feature <- dirs$features
dir_tables  <- file.path(dirs$tables, "feature_tables")
dir_objects <- dirs$objects
dir_logs    <- dirs$logs

dir_e2a <- file.path(dir_feature, "E2A_demographics_comorbidities")
dir_e2d <- file.path(dir_feature, "E2D_support_treatment_proxies")
fig_dir_avail <- file.path(dirs$figures, "variable_availability")
fig_dir_explore <- file.path(dirs$figures, "feature_exploration")

for (d in c(dir_e2d, dir_tables, dir_objects, dir_logs, fig_dir_avail, fig_dir_explore)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

## =========================================================
## 2. Robust file locator
## =========================================================
inventory_file <- file.path(project_dir, "01_raw_inventory", "StepE0_raw_eICU_file_inventory.csv")
if (!file.exists(inventory_file)) stop("Cannot find raw inventory: ", inventory_file)
raw_inventory <- readr::read_csv(inventory_file, show_col_types = FALSE)

find_eicu_file <- function(target_file) {
  direct_path <- file.path(raw_eicu_dir, target_file)
  if (file.exists(direct_path)) return(direct_path)
  hit <- raw_inventory %>%
    dplyr::filter(tolower(.data$file_name) == tolower(target_file)) %>%
    dplyr::pull(file_path)
  if (length(hit) > 0 && file.exists(hit[1])) return(hit[1])
  recursive_hit <- list.files(
    raw_eicu_dir,
    pattern = paste0("^", gsub("\\.", "\\\\.", target_file), "$"),
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(recursive_hit) > 0 && file.exists(recursive_hit[1])) return(recursive_hit[1])
  warning("Cannot locate eICU raw file: ", target_file)
  NA_character_
}
csv_gz <- function(file_name) find_eicu_file(file_name)

cat("Raw eICU folder:\n", raw_eicu_dir, "\n\n")
cat("Step E2D output folder:\n", dir_e2d, "\n\n")

## =========================================================
## 3. Helper functions
## =========================================================
sql_path <- function(path) gsub("'", "''", normalizePath(path, mustWork = TRUE))
qid <- function(x) paste0('"', gsub('"', '""', x), '"')

read_header <- function(path) {
  if (is.na(path) || !file.exists(path)) return(character(0))
  data.table::fread(path, nrows = 0, data.table = FALSE) %>% names()
}

pick_col <- function(nm, candidates) {
  if (length(nm) == 0) return(NA_character_)
  nm_low <- tolower(nm); cand_low <- tolower(candidates)
  idx <- match(cand_low, nm_low); idx <- idx[!is.na(idx)]
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
    if (type == "varchar") return(paste0("CAST(NULL AS VARCHAR) AS ", alias))
    return(paste0("CAST(NULL AS DOUBLE) AS ", alias))
  }
  if (type == "varchar") paste0("CAST(x.", qid(hit), " AS VARCHAR) AS ", alias) else paste0("TRY_CAST(x.", qid(hit), " AS DOUBLE) AS ", alias)
}

## Robust CSV reader expression for DuckDB.
## This avoids failure from text values such as "Minute Volume, Spontaneous".
duckdb_csv_scan <- function(path) {
  sprintf(
    "read_csv_auto('%s', union_by_name=true, all_varchar=true, quote='\"', escape='\"', strict_mode=false, null_padding=true, ignore_errors=true)",
    sql_path(path)
  )
}

clean_text <- function(x) {
  x %>% as.character() %>% stringr::str_replace_all("[\r\n\t]", " ") %>% stringr::str_squish() %>% tolower()
}
extract_first_number <- function(x) {
  suppressWarnings(as.numeric(stringr::str_extract(as.character(x), "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?")))
}
standardize_window <- function(df) {
  df %>% dplyr::mutate(
    window_6h  = dplyr::if_else(offset_min >= 0 & offset_min <= 6  * 60, "0-6h",  NA_character_),
    window_24h = dplyr::if_else(offset_min >= 0 & offset_min <= 24 * 60, "0-24h", NA_character_),
    window_48h = dplyr::if_else(offset_min >= 0 & offset_min <= 48 * 60, "0-48h", NA_character_),
    window_72h = dplyr::if_else(offset_min >= 0 & offset_min <= 72 * 60, "0-72h", NA_character_)
  )
}
make_text_example <- function(x, n = 3) {
  x <- unique(x[!is.na(x) & x != ""])
  if (length(x) == 0) return(NA_character_)
  paste(utils::head(x, n), collapse = " || ")
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
## 4. Load T48 cohort
## =========================================================
t48_e2a_file <- file.path(dir_e2a, "StepE2A_eICU_AMI_T48_cohort_demographics_comorbidities.csv")
if (!file.exists(t48_e2a_file)) stop("Cannot find Step E2A T48 cohort file: ", t48_e2a_file)
ami_t48 <- readr::read_csv(t48_e2a_file, show_col_types = FALSE) %>%
  dplyr::mutate(patientunitstayid = as.character(patientunitstayid))
cohort_ids <- unique(ami_t48$patientunitstayid)
cat("T48 cohort size:", length(cohort_ids), "\n\n")

## =========================================================
## 5. Start DuckDB
## =========================================================
duckdb_path <- file.path(dir_e2d, "StepE2D_support_treatment_extract.duckdb")
if (file.exists(duckdb_path)) file.remove(duckdb_path)
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = duckdb_path, read_only = FALSE)
DBI::dbWriteTable(con, "cohort_ids", data.frame(patientunitstayid = cohort_ids), overwrite = TRUE)

## =========================================================
## 6. Generic extraction
## =========================================================
extract_text_source <- function(file_name, source_name,
                                id_candidates = c("patientunitstayid"),
                                offset_candidates = c(
                                  "nursingchartoffset", "observationoffset",
                                  "respchartoffset", "respcareoffset", "respcarestatusoffset",
                                  "infusionoffset", "drugstartoffset", "drugoffset",
                                  "treatmentoffset", "activeupondischargeoffset",
                                  "intakeoutputoffset", "apacheapsvaroffset"
                                ),
                                text_candidates = c(
                                  "nursingchartcelltypevalname", "nursingchartcelltypevallabel", "nursingchartvalue",
                                  "respchartvaluelabel", "respchartvalue",
                                  "respcarestatus", "respcaretype", "airwaytype", "airwaysize",
                                  "drugname", "infusiondrugname", "drugrate", "infusionrate", "drugamount", "volumeoffluid",
                                  "dosage", "routeadmin", "frequency",
                                  "treatmentstring", "treatmenttext", "treatment", "cellpath",
                                  "celllabel", "intakeoutputcelllabel", "intakeoutputvalue"
                                ),
                                value_candidates = c(
                                  "nursingchartvalue", "respchartvalue", "infusionrate", "drugrate", "dosage",
                                  "treatmentvalue", "intakeoutputvalue", "airwaysize"
                                )) {
  path <- csv_gz(file_name)
  if (is.na(path) || !file.exists(path)) {
    warning("Skipping missing file: ", file_name)
    return(tibble::tibble())
  }
  nm <- read_header(path)
  id_col <- pick_col(nm, id_candidates)
  offset_col <- pick_col(nm, offset_candidates)
  if (is.na(id_col) || is.na(offset_col)) {
    warning("Skipping ", file_name, ": cannot detect id or offset column.")
    return(tibble::tibble())
  }
  text_expr <- purrr::map_chr(text_candidates, function(cand) {
    hit <- pick_col(nm, cand)
    if (is.na(hit)) return(NA_character_)
    paste0("CAST(x.", qid(hit), " AS VARCHAR)")
  })
  text_expr <- text_expr[!is.na(text_expr) & text_expr != ""]
  text_sql <- if (length(text_expr) == 0) "CAST(NULL AS VARCHAR) AS text_raw" else paste0("concat_ws(' | ', ", paste(text_expr, collapse = ", "), ") AS text_raw")
  value_expr <- sql_col_or_null(nm, value_candidates, "value_num", type = "double")
  sql <- sprintf(
    "
    SELECT
      CAST(x.%s AS VARCHAR) AS patientunitstayid,
      TRY_CAST(x.%s AS DOUBLE) AS offset_min,
      %s,
      %s,
      '%s' AS source
    FROM %s x
    INNER JOIN cohort_ids c
      ON CAST(x.%s AS VARCHAR) = c.patientunitstayid
    WHERE TRY_CAST(x.%s AS DOUBLE) BETWEEN 0 AND 4320
    ",
    qid(id_col), qid(offset_col), text_sql, value_expr, source_name,
    duckdb_csv_scan(path), qid(id_col), qid(offset_col)
  )
  tryCatch(
    DBI::dbGetQuery(con, sql) %>% tibble::as_tibble(),
    error = function(e) {
      warning("Failed to extract ", file_name, ": ", conditionMessage(e))
      tibble::tibble()
    }
  )
}

## =========================================================
## 7. Extract source files
## =========================================================
cat("Extracting nurseCharting...\n")
nurse_raw <- extract_text_source("nurseCharting.csv.gz", "nurseCharting", offset_candidates = c("nursingchartoffset", "observationoffset"), text_candidates = c("nursingchartcelltypevalname", "nursingchartcelltypevallabel", "nursingchartvalue"), value_candidates = c("nursingchartvalue"))
cat("nurseCharting rows:", nrow(nurse_raw), "\n")

cat("Extracting respiratoryCharting...\n")
resp_chart_raw <- extract_text_source("respiratoryCharting.csv.gz", "respiratoryCharting", offset_candidates = c("respchartoffset", "observationoffset"), text_candidates = c("respchartvaluelabel", "respchartvalue"), value_candidates = c("respchartvalue"))
cat("respiratoryCharting rows:", nrow(resp_chart_raw), "\n")

cat("Extracting respiratoryCare...\n")
resp_care_raw <- extract_text_source("respiratoryCare.csv.gz", "respiratoryCare", offset_candidates = c("respcarestatusoffset", "respcareoffset", "observationoffset"), text_candidates = c("respcarestatus", "respcaretype", "airwaytype", "airwaysize"), value_candidates = c("airwaysize"))
cat("respiratoryCare rows:", nrow(resp_care_raw), "\n")

cat("Extracting infusionDrug...\n")
infusion_raw <- extract_text_source("infusionDrug.csv.gz", "infusionDrug", offset_candidates = c("infusionoffset", "drugstartoffset", "drugoffset", "observationoffset"), text_candidates = c("drugname", "infusiondrugname", "drugrate", "infusionrate", "drugamount", "volumeoffluid"), value_candidates = c("drugrate", "infusionrate"))
cat("infusionDrug rows:", nrow(infusion_raw), "\n")

cat("Extracting medication...\n")
medication_raw <- extract_text_source("medication.csv.gz", "medication", offset_candidates = c("drugstartoffset", "drugstopoffset", "observationoffset"), text_candidates = c("drugname", "drughiclseqno", "dosage", "routeadmin", "frequency"), value_candidates = c("dosage"))
cat("medication rows:", nrow(medication_raw), "\n")

cat("Extracting treatment...\n")
treatment_raw <- extract_text_source("treatment.csv.gz", "treatment", offset_candidates = c("treatmentoffset", "activeupondischargeoffset", "observationoffset"), text_candidates = c("treatmentstring", "treatmenttext", "treatment", "cellpath"), value_candidates = c("treatmentoffset"))
cat("treatment rows:", nrow(treatment_raw), "\n")

cat("Extracting intakeOutput...\n")
io_raw <- extract_text_source("intakeOutput.csv.gz", "intakeOutput", offset_candidates = c("intakeoutputoffset", "observationoffset"), text_candidates = c("celllabel", "intakeoutputcelllabel", "cellpath", "intakeoutputvalue"), value_candidates = c("intakeoutputvalue"))
cat("intakeOutput rows:", nrow(io_raw), "\n")

## =========================================================
## 8. Extract APACHE tables
## =========================================================
apache_aps_path <- csv_gz("apacheApsVar.csv.gz")
apache_result_path <- csv_gz("apachePatientResult.csv.gz")
apache_aps_raw <- tibble::tibble()
apache_result_raw <- tibble::tibble()

if (!is.na(apache_aps_path) && file.exists(apache_aps_path)) {
  cat("Extracting apacheApsVar...\n")
  nm <- read_header(apache_aps_path); id_col <- pick_col(nm, c("patientunitstayid"))
  if (!is.na(id_col)) {
    sql <- sprintf("SELECT x.* FROM %s x INNER JOIN cohort_ids c ON CAST(x.%s AS VARCHAR) = c.patientunitstayid", duckdb_csv_scan(apache_aps_path), qid(id_col))
    apache_aps_raw <- tryCatch(DBI::dbGetQuery(con, sql) %>% tibble::as_tibble() %>% dplyr::mutate(patientunitstayid = as.character(patientunitstayid)), error = function(e) {warning("Failed to extract apacheApsVar: ", conditionMessage(e)); tibble::tibble()})
  }
}
cat("apacheApsVar rows:", nrow(apache_aps_raw), "\n")

if (!is.na(apache_result_path) && file.exists(apache_result_path)) {
  cat("Extracting apachePatientResult...\n")
  nm <- read_header(apache_result_path); id_col <- pick_col(nm, c("patientunitstayid"))
  if (!is.na(id_col)) {
    sql <- sprintf("SELECT x.* FROM %s x INNER JOIN cohort_ids c ON CAST(x.%s AS VARCHAR) = c.patientunitstayid", duckdb_csv_scan(apache_result_path), qid(id_col))
    apache_result_raw <- tryCatch(DBI::dbGetQuery(con, sql) %>% tibble::as_tibble() %>% dplyr::mutate(patientunitstayid = as.character(patientunitstayid)), error = function(e) {warning("Failed to extract apachePatientResult: ", conditionMessage(e)); tibble::tibble()})
  }
}
cat("apachePatientResult rows:", nrow(apache_result_raw), "\n")

DBI::dbDisconnect(con, shutdown = TRUE)

## =========================================================
## 9. Define proxy patterns and standardize records
## =========================================================
proxy_patterns <- list(
  gcs_total = "gcs total|glasgow coma score|glasgow coma scale total|total gcs",
  gcs_eye = "gcs eyes|gcs eye|eye opening|\\beyes\\b",
  gcs_verbal = "gcs verbal|verbal response|\\bverbal\\b",
  gcs_motor = "gcs motor|motor response|\\bmotor\\b",
  mechanical_ventilation = "mechanical ventilation|ventilator|vent mode|assist control|pressure control|volume control|simv|prvc|bipap|cpap|intubated|endotracheal|\\bett\\b|trach",
  oxygen_support = "nasal cannula|face mask|non-rebreather|high flow|hfnc|oxygen therapy|fio2|o2 flow|oxygen flow|\\bo2\\b",
  vasopressor = "norepinephrine|levophed|epinephrine|adrenaline|dopamine|dobutamine|vasopressin|phenylephrine|neosynephrine|neo-synephrine|milrinone",
  dialysis = "dialysis|hemodialysis|haemodialysis|crrt|continuous renal replacement|cvvh|cvvhd|ultrafiltration",
  pci_cath = "\\bpci\\b|percutaneous coronary|cardiac cath|catheterization|catheterisation|angioplasty|stent|coronary intervention",
  iabp_mcs = "\\biabp\\b|intra-aortic balloon|intraaortic balloon|impella|\\becmo\\b|mechanical circulatory support",
  urine_output = "urine|\\buop\\b|foley|urinary output"
)

text_raw_all <- dplyr::bind_rows(nurse_raw, resp_chart_raw, resp_care_raw, infusion_raw, medication_raw, treatment_raw, io_raw) %>%
  dplyr::mutate(
    patientunitstayid = as.character(patientunitstayid),
    offset_min = suppressWarnings(as.numeric(offset_min)),
    offset_hour = offset_min / 60,
    text_raw = as.character(text_raw),
    text_clean = clean_text(text_raw),
    value_num = suppressWarnings(as.numeric(value_num)),
    value_from_text = extract_first_number(text_clean),
    value = dplyr::coalesce(value_num, value_from_text)
  ) %>%
  dplyr::filter(!is.na(patientunitstayid), !is.na(offset_min), offset_min >= 0, offset_min <= 72 * 60, !is.na(text_clean), text_clean != "")

readr::write_csv(text_raw_all, file.path(dir_e2d, "StepE2D_raw_text_proxy_records_T0_T72.csv"))
cat("All text/proxy raw rows:", nrow(text_raw_all), "\n")

proxy_evidence <- purrr::imap_dfr(proxy_patterns, function(pattern, proxy_name) {
  text_raw_all %>%
    dplyr::filter(stringr::str_detect(text_clean, pattern)) %>%
    dplyr::transmute(patientunitstayid, offset_min, offset_hour, source, proxy = proxy_name, value = value, evidence_text = text_raw)
}) %>%
  dplyr::distinct()

proxy_evidence <- proxy_evidence %>%
  dplyr::mutate(
    value = dplyr::case_when(
      proxy == "gcs_total" & (value < 3 | value > 15) ~ NA_real_,
      proxy == "gcs_eye" & (value < 1 | value > 4) ~ NA_real_,
      proxy == "gcs_verbal" & (value < 1 | value > 5) ~ NA_real_,
      proxy == "gcs_motor" & (value < 1 | value > 6) ~ NA_real_,
      proxy == "urine_output" & (value < 0 | value > 10000) ~ NA_real_,
      TRUE ~ value
    ),
    binary_value = dplyr::case_when(
      proxy %in% c("mechanical_ventilation", "oxygen_support", "vasopressor", "dialysis", "pci_cath", "iabp_mcs") ~ 1,
      proxy == "urine_output" & !is.na(value) ~ 1,
      proxy %in% c("gcs_total", "gcs_eye", "gcs_verbal", "gcs_motor") & !is.na(value) ~ 1,
      TRUE ~ 1
    )
  )

readr::write_csv(proxy_evidence, file.path(dir_e2d, "StepE2D_proxy_evidence_long_T0_T72.csv"))

## =========================================================
## 10. Add APACHE-derived GCS if present
## =========================================================
apache_proxy_long <- tibble::tibble()
if (nrow(apache_aps_raw) > 0) {
  aps_nm <- names(apache_aps_raw)
  gcs_cols <- aps_nm[stringr::str_detect(tolower(aps_nm), "gcs|glasgow|eyes|eye|motor|verbal")]
  if (length(gcs_cols) > 0) {
    apache_proxy_long <- apache_aps_raw %>%
      dplyr::select(patientunitstayid, dplyr::all_of(gcs_cols)) %>%
      tidyr::pivot_longer(cols = -patientunitstayid, names_to = "raw_variable", values_to = "value_raw") %>%
      dplyr::mutate(
        value = suppressWarnings(as.numeric(value_raw)),
        proxy = dplyr::case_when(
          stringr::str_detect(tolower(raw_variable), "total") ~ "gcs_total",
          stringr::str_detect(tolower(raw_variable), "eye|eyes") ~ "gcs_eye",
          stringr::str_detect(tolower(raw_variable), "verbal") ~ "gcs_verbal",
          stringr::str_detect(tolower(raw_variable), "motor") ~ "gcs_motor",
          stringr::str_detect(tolower(raw_variable), "gcs") ~ "gcs_total",
          TRUE ~ NA_character_
        ),
        offset_min = 0, offset_hour = 0, source = "apacheApsVar",
        evidence_text = paste0(raw_variable, "=", value_raw),
        binary_value = as.integer(!is.na(value))
      ) %>%
      dplyr::filter(!is.na(proxy), !is.na(value)) %>%
      dplyr::select(patientunitstayid, offset_min, offset_hour, source, proxy, value, evidence_text, binary_value)
  }
}

proxy_evidence_all <- dplyr::bind_rows(
  proxy_evidence %>% dplyr::select(patientunitstayid, offset_min, offset_hour, source, proxy, value, evidence_text, binary_value),
  apache_proxy_long
) %>%
  dplyr::mutate(patientunitstayid = as.character(patientunitstayid)) %>%
  dplyr::distinct()

readr::write_csv(proxy_evidence_all, file.path(dir_e2d, "StepE2D_proxy_evidence_long_T0_T72_with_APACHE.csv"))

## =========================================================
## 11. Summarise by windows
## =========================================================
proxy_window_long <- proxy_evidence_all %>%
  standardize_window() %>%
  tidyr::pivot_longer(cols = c(window_6h, window_24h, window_48h, window_72h), names_to = "window_type", values_to = "window") %>%
  dplyr::filter(!is.na(window)) %>%
  dplyr::mutate(window = factor(window, levels = c("0-6h", "0-24h", "0-48h", "0-72h")))

numeric_proxy <- c("gcs_total", "gcs_eye", "gcs_verbal", "gcs_motor", "urine_output")
binary_proxy <- setdiff(unique(proxy_window_long$proxy), numeric_proxy)

proxy_numeric_summary <- proxy_window_long %>%
  dplyr::filter(proxy %in% numeric_proxy, !is.na(value)) %>%
  dplyr::arrange(patientunitstayid, proxy, window, offset_min) %>%
  dplyr::group_by(patientunitstayid, proxy, window) %>%
  dplyr::summarise(
    n_measure = dplyr::n(), first_value = dplyr::first(value), last_value = dplyr::last(value),
    min_value = min(value, na.rm = TRUE), max_value = max(value, na.rm = TRUE),
    mean_value = mean(value, na.rm = TRUE), median_value = median(value, na.rm = TRUE),
    any_flag = 1L, .groups = "drop"
  )

proxy_binary_summary <- proxy_window_long %>%
  dplyr::filter(proxy %in% binary_proxy) %>%
  dplyr::group_by(patientunitstayid, proxy, window) %>%
  dplyr::summarise(
    n_measure = dplyr::n(), first_value = 1, last_value = 1,
    min_value = 1, max_value = 1, mean_value = 1, median_value = 1,
    any_flag = 1L, .groups = "drop"
  )

proxy_summary <- dplyr::bind_rows(proxy_numeric_summary, proxy_binary_summary)
all_proxy_levels <- sort(unique(c(names(proxy_patterns), proxy_summary$proxy, proxy_evidence_all$proxy)))

expected_proxy <- tidyr::expand_grid(
  patientunitstayid = cohort_ids,
  proxy = all_proxy_levels,
  window = factor(c("0-6h", "0-24h", "0-48h", "0-72h"), levels = c("0-6h", "0-24h", "0-48h", "0-72h"))
)

proxy_summary_full <- expected_proxy %>%
  dplyr::left_join(proxy_summary, by = c("patientunitstayid", "proxy", "window")) %>%
  dplyr::mutate(
    n_measure = tidyr::replace_na(n_measure, 0L),
    any_flag = tidyr::replace_na(any_flag, 0L),
    available = as.integer(n_measure > 0)
  )

readr::write_csv(proxy_summary_full, file.path(dir_e2d, "StepE2D_proxy_summary_by_window.csv"))

## =========================================================
## 12. Make wide feature table
## =========================================================
proxy_wide <- proxy_summary_full %>%
  dplyr::mutate(window_clean = dplyr::case_when(
    window == "0-6h" ~ "T0_6h", window == "0-24h" ~ "T24",
    window == "0-48h" ~ "T48", window == "0-72h" ~ "T72", TRUE ~ as.character(window)
  )) %>%
  dplyr::select(patientunitstayid, proxy, window_clean, n_measure, first_value, last_value, min_value, max_value, mean_value, median_value, any_flag) %>%
  tidyr::pivot_longer(cols = c(n_measure, first_value, last_value, min_value, max_value, mean_value, median_value, any_flag), names_to = "stat", values_to = "value") %>%
  dplyr::mutate(feature_name = paste(proxy, window_clean, stat, sep = "_")) %>%
  dplyr::select(patientunitstayid, feature_name, value) %>%
  tidyr::pivot_wider(id_cols = patientunitstayid, names_from = feature_name, values_from = value)

ami_t48_proxy_wide <- ami_t48 %>%
  dplyr::select(patientunitstayid, dplyr::any_of(c("uniquepid", "age", "sex", "ethnicity", "primary_external_endpoint", "secondary_icu_mortality_after_T48", "in_icu_at_T48", "hospital_los_from_icu_hours", "icu_los_hours"))) %>%
  dplyr::left_join(proxy_wide, by = "patientunitstayid")

readr::write_csv(proxy_wide, file.path(dir_e2d, "StepE2D_support_treatment_proxy_wide_features_only.csv"))
readr::write_csv(ami_t48_proxy_wide, file.path(dir_e2d, "StepE2D_eICU_AMI_T48_cohort_with_support_treatment_features.csv"))

## =========================================================
## 13. Save APACHE tables
## =========================================================
apache_result_tbl <- tibble::tibble()
if (nrow(apache_result_raw) > 0) {
  apache_result_tbl <- apache_result_raw %>% dplyr::mutate(patientunitstayid = as.character(patientunitstayid)) %>% dplyr::filter(patientunitstayid %in% cohort_ids)
  readr::write_csv(apache_result_tbl, file.path(dir_e2d, "StepE2D_APACHE_patient_result_for_T48_cohort.csv"))
}
apache_aps_tbl <- tibble::tibble()
if (nrow(apache_aps_raw) > 0) {
  apache_aps_tbl <- apache_aps_raw %>% dplyr::mutate(patientunitstayid = as.character(patientunitstayid)) %>% dplyr::filter(patientunitstayid %in% cohort_ids)
  readr::write_csv(apache_aps_tbl, file.path(dir_e2d, "StepE2D_APACHE_APS_variables_for_T48_cohort.csv"))
}

## =========================================================
## 14. Availability and evidence summaries
## =========================================================
proxy_availability <- proxy_summary_full %>%
  dplyr::group_by(proxy, window) %>%
  dplyr::summarise(
    n_available = sum(available == 1, na.rm = TRUE),
    n_total = length(cohort_ids),
    availability_pct = n_available / n_total * 100,
    missing_pct = 100 - availability_pct,
    median_n_measure_if_available = median(n_measure[n_measure > 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(median_n_measure_if_available = ifelse(is.infinite(median_n_measure_if_available), NA_real_, median_n_measure_if_available)) %>%
  dplyr::arrange(proxy, window)

proxy_evidence_summary <- proxy_evidence_all %>%
  dplyr::group_by(proxy, source) %>%
  dplyr::summarise(n_records = dplyr::n(), n_patients = dplyr::n_distinct(patientunitstayid), example_text = make_text_example(evidence_text, n = 3), .groups = "drop") %>%
  dplyr::arrange(proxy, dplyr::desc(n_patients))

readr::write_csv(proxy_availability, file.path(dir_tables, "StepE2D_support_treatment_proxy_availability_by_window.csv"))
readr::write_csv(proxy_evidence_summary, file.path(dir_tables, "StepE2D_support_treatment_proxy_evidence_summary.csv"))

## =========================================================
## 15. Figures
## =========================================================
proxy_order <- c("gcs_total", "gcs_eye", "gcs_verbal", "gcs_motor", "oxygen_support", "mechanical_ventilation", "vasopressor", "urine_output", "dialysis", "pci_cath", "iabp_mcs")
pretty_proxy_names <- c(
  gcs_total = "GCS total", gcs_eye = "GCS eye", gcs_verbal = "GCS verbal", gcs_motor = "GCS motor",
  oxygen_support = "Oxygen support", mechanical_ventilation = "Mechanical ventilation", vasopressor = "Vasopressor",
  urine_output = "Urine output", dialysis = "Dialysis / CRRT", pci_cath = "PCI / cath", iabp_mcs = "IABP / MCS"
)

plot_avail_df <- proxy_availability %>%
  dplyr::filter(proxy %in% proxy_order) %>%
  dplyr::mutate(
    proxy_pretty = factor(pretty_proxy_names[proxy], levels = rev(pretty_proxy_names[proxy_order])),
    window = factor(window, levels = c("0-6h", "0-24h", "0-48h", "0-72h")),
    label = sprintf("%.1f", availability_pct)
  )

plot_bar_df <- plot_avail_df %>%
  dplyr::group_by(proxy_pretty) %>%
  dplyr::summarise(mean_availability_pct = mean(availability_pct, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(proxy_pretty = factor(proxy_pretty, levels = levels(plot_avail_df$proxy_pretty)))

p_proxy_heat <- ggplot(plot_avail_df, aes(x = window, y = proxy_pretty, fill = availability_pct)) +
  geom_tile(color = "white", linewidth = 0.9, width = 0.98, height = 0.98) +
  geom_text(aes(label = label), size = 3.7, fontface = "bold", color = "black") +
  scale_fill_gradientn(colours = c("#F8FAFC", "#DBEAFE", "#93C5FD", "#3B82F6", "#1E3A8A"), limits = c(0, 100), name = "Availability (%)") +
  labs(x = NULL, y = NULL) + coord_fixed(ratio = 0.72, clip = "off") +
  theme_eicu(base_size = 15) + theme(legend.position = "none", axis.text.x = element_text(face = "bold", size = 13, angle = 45, hjust = 1), axis.text.y = element_text(face = "bold", size = 13))

p_proxy_bar <- ggplot(plot_bar_df, aes(x = mean_availability_pct, y = proxy_pretty)) +
  geom_col(width = 0.75, fill = "#5DA9D6", color = "white", linewidth = 0.35) +
  geom_text(aes(label = sprintf("%.1f", mean_availability_pct)), hjust = -0.15, size = 3.7, fontface = "bold") +
  scale_x_continuous(limits = c(0, 105), expand = c(0, 0)) +
  labs(x = "Mean availability (%)", y = NULL) +
  theme_eicu(base_size = 15) + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_text(size = 13), axis.text.x = element_text(size = 12), panel.grid.major.x = element_line(color = "#E5E7EB", linewidth = 0.45))

fig_proxy_avail <- p_proxy_heat + p_proxy_bar +
  patchwork::plot_layout(widths = c(4.2, 1.7)) +
  patchwork::plot_annotation(
    title = "Availability of support and treatment proxies across early eICU windows",
    subtitle = "Cell values indicate patient-level availability; right bars summarize mean availability across windows",
    theme = theme(plot.title = element_text(face = "bold", size = 23, hjust = 0), plot.subtitle = element_text(size = 15.5, color = "grey30", hjust = 0), plot.background = element_rect(fill = "white", color = NA), plot.margin = margin(10, 12, 10, 10))
  )
print(fig_proxy_avail)

ggsave(file.path(fig_dir_avail, "FigureE2D_1_support_treatment_proxy_availability_heatmap_barplot.pdf"), fig_proxy_avail, width = 10.5, height = 6.7, device = cairo_pdf, bg = "white")
ggsave(file.path(fig_dir_avail, "FigureE2D_1_support_treatment_proxy_availability_heatmap_barplot.png"), fig_proxy_avail, width = 10.5, height = 6.7, dpi = 600, bg = "white")

binary_profile_df <- proxy_summary_full %>%
  dplyr::filter(proxy %in% c("oxygen_support", "mechanical_ventilation", "vasopressor", "dialysis", "pci_cath", "iabp_mcs"), window %in% c("0-6h", "0-24h", "0-48h", "0-72h")) %>%
  dplyr::left_join(ami_t48 %>% dplyr::select(patientunitstayid, primary_external_endpoint), by = "patientunitstayid") %>%
  dplyr::group_by(proxy, window) %>%
  dplyr::summarise(
    exposed_n = sum(any_flag == 1, na.rm = TRUE),
    exposed_mortality_pct = ifelse(exposed_n > 0, mean(primary_external_endpoint[any_flag == 1] == 1, na.rm = TRUE) * 100, NA_real_),
    unexposed_mortality_pct = ifelse(sum(any_flag == 0, na.rm = TRUE) > 0, mean(primary_external_endpoint[any_flag == 0] == 1, na.rm = TRUE) * 100, NA_real_),
    risk_difference_pct = exposed_mortality_pct - unexposed_mortality_pct,
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    proxy_pretty = factor(pretty_proxy_names[proxy], levels = rev(pretty_proxy_names[c("oxygen_support", "mechanical_ventilation", "vasopressor", "dialysis", "pci_cath", "iabp_mcs")])),
    window = factor(window, levels = c("0-6h", "0-24h", "0-48h", "0-72h")),
    label = ifelse(is.na(risk_difference_pct), "", sprintf("%.1f", risk_difference_pct))
  )

readr::write_csv(binary_profile_df, file.path(dir_tables, "StepE2D_Figure_binary_proxy_outcome_profile_data.csv"))
max_abs_rd <- max(abs(binary_profile_df$risk_difference_pct), na.rm = TRUE)
if (!is.finite(max_abs_rd) || max_abs_rd == 0) max_abs_rd <- 10

p_binary_profile <- ggplot(binary_profile_df, aes(x = window, y = proxy_pretty, fill = risk_difference_pct)) +
  geom_tile(color = "white", linewidth = 0.9, width = 0.98, height = 0.98) +
  geom_text(aes(label = label), size = 3.8, fontface = "bold", color = "black") +
  scale_fill_gradient2(low = "#2563EB", mid = "#FFFFFF", high = "#DC2626", midpoint = 0, limits = c(-max_abs_rd, max_abs_rd), name = "Mortality risk\ndifference (%)") +
  labs(title = "Outcome-associated support and treatment proxy patterns", subtitle = "Values represent mortality difference between exposed and unexposed patients within each time window", x = NULL, y = NULL) +
  coord_fixed(ratio = 0.72, clip = "off") +
  theme_eicu(base_size = 15) + theme(axis.text.x = element_text(face = "bold", size = 13, angle = 45, hjust = 1), axis.text.y = element_text(face = "bold", size = 13), legend.position = "right")
print(p_binary_profile)

ggsave(file.path(fig_dir_explore, "FigureE2D_2_outcome_associated_support_treatment_profile_heatmap.pdf"), p_binary_profile, width = 10.5, height = 5.8, device = cairo_pdf, bg = "white")
ggsave(file.path(fig_dir_explore, "FigureE2D_2_outcome_associated_support_treatment_profile_heatmap.png"), p_binary_profile, width = 10.5, height = 5.8, dpi = 600, bg = "white")

## =========================================================
## 16. Export Excel workbook
## =========================================================
wb <- openxlsx::createWorkbook()
add_sheet <- function(wb, sheet_name, df) {
  sheet_name <- substr(sheet_name, 1, 31)
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeDataTable(wb, sheet = sheet_name, x = df, tableStyle = "TableStyleMedium2", withFilter = TRUE)
  openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:ncol(df), widths = "auto")
}

method_note <- tibble::tibble(
  item = c("Purpose", "Cohort", "Time origin", "Extraction window", "Data sources", "Proxy features", "GCS handling", "Binary treatment proxies", "CSV parsing", "Downstream use"),
  description = c(
    "Extract neurologic, respiratory support, hemodynamic support, urine output, treatment, and benchmark proxy variables for eICU AMI-EDRS external validation.",
    "Primary T48 external validation cohort from Step E1/E2A.",
    "T0 was defined as ICU admission; eICU offsets were used as minutes from ICU admission.",
    "Records from 0 to 72 hours after ICU admission were extracted.",
    "nurseCharting, respiratoryCharting, respiratoryCare, infusionDrug, medication, treatment, intakeOutput, apacheApsVar, and apachePatientResult.",
    "GCS components, oxygen support, mechanical ventilation, vasopressor exposure, urine output, dialysis/CRRT, PCI/cath, IABP/MCS.",
    "GCS values were extracted from nurseCharting and supplemented using apacheApsVar when available.",
    "Treatment proxies were encoded as patient-level binary exposure indicators within each time window.",
    "Irregular eICU CSV rows were handled using DuckDB read_csv_auto with strict_mode=false, null_padding=true, ignore_errors=true, and all_varchar=true.",
    "These features will be merged with vital signs and laboratory features for organ-domain score reconstruction and AMI-EDRS external prediction."
  )
)

add_sheet(wb, "method_note", method_note)
add_sheet(wb, "proxy_availability", proxy_availability)
add_sheet(wb, "evidence_summary", proxy_evidence_summary)
add_sheet(wb, "binary_outcome_profile", binary_profile_df)
add_sheet(wb, "proxy_summary_preview", head(proxy_summary_full, 5000))
add_sheet(wb, "wide_preview", head(ami_t48_proxy_wide, 5000))
if (nrow(apache_result_tbl) > 0) add_sheet(wb, "apache_result_preview", head(apache_result_tbl, 5000))
if (nrow(apache_aps_tbl) > 0) add_sheet(wb, "apache_aps_preview", head(apache_aps_tbl, 5000))

out_xlsx <- file.path(dir_e2d, "StepE2D_support_treatment_proxy_extraction_summary.xlsx")
openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

## =========================================================
## 17. Save RDS object and session info
## =========================================================
stepE2D_object <- list(
  ami_t48 = ami_t48,
  nurse_raw_dim = dim(nurse_raw), resp_chart_raw_dim = dim(resp_chart_raw), resp_care_raw_dim = dim(resp_care_raw),
  infusion_raw_dim = dim(infusion_raw), medication_raw_dim = dim(medication_raw), treatment_raw_dim = dim(treatment_raw), io_raw_dim = dim(io_raw),
  apache_aps_raw_dim = dim(apache_aps_raw), apache_result_raw_dim = dim(apache_result_raw),
  text_raw_all = text_raw_all,
  proxy_evidence = proxy_evidence,
  apache_proxy_long = apache_proxy_long,
  proxy_evidence_all = proxy_evidence_all,
  proxy_summary_full = proxy_summary_full,
  proxy_wide = proxy_wide,
  ami_t48_proxy_wide = ami_t48_proxy_wide,
  apache_aps_tbl = apache_aps_tbl,
  apache_result_tbl = apache_result_tbl,
  proxy_availability = proxy_availability,
  proxy_evidence_summary = proxy_evidence_summary,
  binary_profile_df = binary_profile_df,
  proxy_patterns = proxy_patterns,
  pretty_proxy_names = pretty_proxy_names,
  created_at = Sys.time()
)

saveRDS(stepE2D_object, file.path(dir_objects, "StepE2D_support_treatment_proxy_object.rds"))

sink(file.path(dir_logs, "StepE2D_sessionInfo.txt"))
cat("Step E2D session information\n")
cat("============================\n\n")
print(sessionInfo())
sink()

## =========================================================
## 18. Console summary
## =========================================================
cat("\n================ Step E2D finished ================\n")
cat("T48 cohort:", length(cohort_ids), "\n")
cat("nurseCharting rows:", nrow(nurse_raw), "\n")
cat("respiratoryCharting rows:", nrow(resp_chart_raw), "\n")
cat("respiratoryCare rows:", nrow(resp_care_raw), "\n")
cat("infusionDrug rows:", nrow(infusion_raw), "\n")
cat("medication rows:", nrow(medication_raw), "\n")
cat("treatment rows:", nrow(treatment_raw), "\n")
cat("intakeOutput rows:", nrow(io_raw), "\n")
cat("apacheApsVar rows:", nrow(apache_aps_raw), "\n")
cat("apachePatientResult rows:", nrow(apache_result_raw), "\n")
cat("Proxy evidence rows:", nrow(proxy_evidence_all), "\n")
cat("Patients with any proxy evidence:", dplyr::n_distinct(proxy_evidence_all$patientunitstayid), "\n\n")

cat("Proxy availability by window:\n")
print(proxy_availability, n = 100)

cat("\nEvidence summary:\n")
print(proxy_evidence_summary, n = 100)

cat("\nMain outputs:\n")
cat(file.path(dir_e2d, "StepE2D_raw_text_proxy_records_T0_T72.csv"), "\n")
cat(file.path(dir_e2d, "StepE2D_proxy_evidence_long_T0_T72_with_APACHE.csv"), "\n")
cat(file.path(dir_e2d, "StepE2D_proxy_summary_by_window.csv"), "\n")
cat(file.path(dir_e2d, "StepE2D_support_treatment_proxy_wide_features_only.csv"), "\n")
cat(file.path(dir_e2d, "StepE2D_eICU_AMI_T48_cohort_with_support_treatment_features.csv"), "\n")
cat(file.path(dir_tables, "StepE2D_support_treatment_proxy_availability_by_window.csv"), "\n")
cat(out_xlsx, "\n")
cat(file.path(dir_objects, "StepE2D_support_treatment_proxy_object.rds"), "\n\n")
cat("Figures:\n")
cat(file.path(fig_dir_avail, "FigureE2D_1_support_treatment_proxy_availability_heatmap_barplot.pdf"), "\n")
cat(file.path(fig_dir_explore, "FigureE2D_2_outcome_associated_support_treatment_profile_heatmap.pdf"), "\n")
cat("===================================================\n")
