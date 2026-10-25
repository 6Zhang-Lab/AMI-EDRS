############################################################
## Step E2A: eICU demographics and comorbidity extraction
## Project: eICU AMI-EDRS external validation
##
## Input:
##   StepE0_path_config.rds
##   StepE1 eICU AMI T48 cohort
##   pastHistory.csv.gz
##   diagnosis.csv.gz
##   admissionDx.csv.gz
##
## Output:
##   1. T48 cohort with demographics and comorbidity flags
##   2. Comorbidity evidence long table
##   3. Comorbidity summary tables
##   4. RDS object for downstream steps
############################################################

rm(list = ls())
gc()

options(stringsAsFactors = FALSE)

## =========================================================
## 0. Packages
## =========================================================

pkg_needed <- c(
  "dplyr", "tidyr", "stringr", "purrr", "tibble",
  "data.table", "readr", "openxlsx"
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

dir_cohort <- dirs$cohort
dir_feature <- dirs$features
dir_tables <- file.path(dirs$tables, "feature_tables")
dir_objects <- dirs$objects
dir_logs <- dirs$logs

dir_e2a <- file.path(dir_feature, "E2A_demographics_comorbidities")
dir.create(dir_e2a, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_objects, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_logs, recursive = TRUE, showWarnings = FALSE)

csv_gz <- function(file_name) {
  file.path(raw_eicu_dir, file_name)
}

cat("Raw eICU folder:\n", raw_eicu_dir, "\n\n")
cat("Step E2A output folder:\n", dir_e2a, "\n\n")

## =========================================================
## 2. Helper functions
## =========================================================

safe_fread <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  data.table::fread(path, data.table = FALSE, showProgress = TRUE) %>%
    tibble::as_tibble()
}

clean_text <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_replace_all("[\r\n\t]", " ") %>%
    stringr::str_squish() %>%
    tolower()
}

to_numeric_safe <- function(x) {
  suppressWarnings(as.numeric(x))
}

find_col <- function(df, candidates) {
  nm <- names(df)
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

collapse_text_cols <- function(df, id_col, source_name) {
  
  if (!id_col %in% names(df)) {
    stop("ID column not found in ", source_name, ": ", id_col)
  }
  
  text_cols <- names(df)[
    purrr::map_lgl(df, ~ is.character(.x) || is.factor(.x))
  ]
  
  text_cols <- setdiff(text_cols, id_col)
  
  if (length(text_cols) == 0) {
    return(tibble::tibble())
  }
  
  df %>%
    dplyr::mutate(
      patientunitstayid = as.character(.data[[id_col]])
    ) %>%
    dplyr::select(patientunitstayid, dplyr::all_of(text_cols)) %>%
    dplyr::mutate(
      text_raw = apply(
        dplyr::select(., dplyr::all_of(text_cols)),
        1,
        function(z) paste(z[!is.na(z) & z != ""], collapse = " | ")
      ),
      text_clean = clean_text(text_raw),
      source = source_name
    ) %>%
    dplyr::filter(!is.na(text_clean), text_clean != "") %>%
    dplyr::select(patientunitstayid, source, text_raw, text_clean)
}

make_evidence_rows <- function(df_text, comorbidity_name, pattern_include, pattern_exclude = NULL) {
  
  out <- df_text %>%
    dplyr::mutate(
      include_flag = stringr::str_detect(text_clean, pattern_include),
      exclude_flag = if (is.null(pattern_exclude)) FALSE else stringr::str_detect(text_clean, pattern_exclude)
    ) %>%
    dplyr::filter(include_flag, !exclude_flag) %>%
    dplyr::transmute(
      patientunitstayid,
      source,
      comorbidity = comorbidity_name,
      evidence_text = text_raw
    )
  
  out
}

make_text_example <- function(x, n = 3) {
  x <- unique(x[!is.na(x) & x != ""])
  if (length(x) == 0) return(NA_character_)
  paste(utils::head(x, n), collapse = " || ")
}

## =========================================================
## 3. Load Step E1 cohorts
## =========================================================

t48_file <- file.path(
  dir_cohort,
  "StepE1_eICU_AMI_T48_landmark_external_validation_cohort.csv"
)

t48_inicu_file <- file.path(
  dir_cohort,
  "StepE1_eICU_AMI_T48_inICU_sensitivity_cohort.csv"
)

first_stay_file <- file.path(
  dir_cohort,
  "StepE1_eICU_conservative_AMI_first_ICU_stay_per_patient.csv"
)

if (!file.exists(t48_file)) {
  stop("Cannot find T48 cohort file: ", t48_file)
}

ami_t48 <- readr::read_csv(t48_file, show_col_types = FALSE) %>%
  dplyr::mutate(patientunitstayid = as.character(patientunitstayid))

ami_t48_inicu <- readr::read_csv(t48_inicu_file, show_col_types = FALSE) %>%
  dplyr::mutate(patientunitstayid = as.character(patientunitstayid))

ami_first <- readr::read_csv(first_stay_file, show_col_types = FALSE) %>%
  dplyr::mutate(patientunitstayid = as.character(patientunitstayid))

t48_ids <- unique(ami_t48$patientunitstayid)
first_ids <- unique(ami_first$patientunitstayid)

cat("T48 cohort:", nrow(ami_t48), "\n")
cat("T48 in-ICU cohort:", nrow(ami_t48_inicu), "\n")
cat("First AMI cohort:", nrow(ami_first), "\n\n")

## =========================================================
## 4. Read diagnosis and past history files
## =========================================================

cat("Reading pastHistory.csv.gz...\n")
past_history <- safe_fread(csv_gz("pastHistory.csv.gz"))

cat("Reading diagnosis.csv.gz...\n")
diagnosis <- safe_fread(csv_gz("diagnosis.csv.gz"))

cat("Reading admissionDx.csv.gz...\n")
admission_dx <- safe_fread(csv_gz("admissionDx.csv.gz"))

cat("\nRows loaded:\n")
cat("pastHistory:", nrow(past_history), "\n")
cat("diagnosis:", nrow(diagnosis), "\n")
cat("admissionDx:", nrow(admission_dx), "\n\n")

## =========================================================
## 5. Collapse text fields and restrict to AMI stays
## =========================================================

id_past <- find_col(past_history, c("patientunitstayid"))
id_diag <- find_col(diagnosis, c("patientunitstayid"))
id_admdx <- find_col(admission_dx, c("patientunitstayid"))

if (is.na(id_past)) stop("patientunitstayid not found in pastHistory.csv.gz")
if (is.na(id_diag)) stop("patientunitstayid not found in diagnosis.csv.gz")
if (is.na(id_admdx)) stop("patientunitstayid not found in admissionDx.csv.gz")

past_text <- collapse_text_cols(
  past_history %>% dplyr::filter(as.character(.data[[id_past]]) %in% first_ids),
  id_col = id_past,
  source_name = "pastHistory"
)

diag_text <- collapse_text_cols(
  diagnosis %>% dplyr::filter(as.character(.data[[id_diag]]) %in% first_ids),
  id_col = id_diag,
  source_name = "diagnosis"
)

admdx_text <- collapse_text_cols(
  admission_dx %>% dplyr::filter(as.character(.data[[id_admdx]]) %in% first_ids),
  id_col = id_admdx,
  source_name = "admissionDx"
)

text_all <- dplyr::bind_rows(
  past_text,
  diag_text,
  admdx_text
) %>%
  dplyr::distinct(patientunitstayid, source, text_raw, text_clean)

readr::write_csv(
  text_all,
  file.path(dir_e2a, "StepE2A_all_history_diagnosis_text_for_AMI_stays.csv")
)

cat("Collapsed text rows:", nrow(text_all), "\n\n")

## =========================================================
## 6. Define comorbidity patterns
## =========================================================

patterns <- list(
  cad_ihd = list(
    include = "\\bcoronary artery disease\\b|\\bcad\\b|\\bischemic heart disease\\b|\\bischaemic heart disease\\b|\\bcoronary disease\\b|\\bcoronary atherosclerosis\\b|\\bangina\\b|\\bprior pci\\b|\\bcabg\\b|\\bcoronary bypass\\b",
    exclude = NULL
  ),
  heart_failure = list(
    include = "\\bcongestive heart failure\\b|\\bchf\\b|\\bheart failure\\b|\\bcardiomyopathy\\b|\\blv dysfunction\\b|\\breduced ejection fraction\\b|\\bhfref\\b|\\bhfpef\\b",
    exclude = NULL
  ),
  hypertension = list(
    include = "\\bhypertension\\b|\\bhypertensive\\b|\\bhtn\\b",
    exclude = NULL
  ),
  diabetes = list(
    include = "\\bdiabetes\\b|\\bdiabetic\\b|\\bdm\\b|\\binsulin dependent\\b|\\biddm\\b|\\bniddm\\b|\\btype 1 diabetes\\b|\\btype 2 diabetes\\b",
    exclude = NULL
  ),
  ckd = list(
    include = "\\bchronic kidney disease\\b|\\bckd\\b|\\bchronic renal\\b|\\bend stage renal\\b|\\besrd\\b|\\bdialysis\\b|\\bhemodialysis\\b|\\bhaemodialysis\\b|\\brenal failure chronic\\b",
    exclude = "\\bacute renal failure\\b|\\bacute kidney injury\\b|\\baki\\b"
  ),
  copd = list(
    include = "\\bcopd\\b|\\bchronic obstructive pulmonary\\b|\\bemphysema\\b|\\bchronic bronchitis\\b",
    exclude = NULL
  ),
  asthma = list(
    include = "\\basthma\\b",
    exclude = NULL
  ),
  stroke = list(
    include = "\\bstroke\\b|\\bcerebrovascular accident\\b|\\bcva\\b|\\btia\\b|\\btransient ischemic attack\\b|\\bcerebral infarction\\b",
    exclude = NULL
  ),
  malignancy = list(
    include = "\\bcancer\\b|\\bmalignancy\\b|\\bmalignant\\b|\\bcarcinoma\\b|\\blymphoma\\b|\\bleukemia\\b|\\bmetastatic\\b|\\bmetastasis\\b|\\btumor\\b|\\btumour\\b",
    exclude = NULL
  ),
  liver_disease = list(
    include = "\\bcirrhosis\\b|\\bliver disease\\b|\\bhepatic failure\\b|\\bhepatitis\\b|\\bportal hypertension\\b|\\besophageal varices\\b|\\bascites\\b",
    exclude = NULL
  ),
  atrial_fibrillation = list(
    include = "\\batrial fibrillation\\b|\\bafib\\b|\\baf\\b|\\batrial flutter\\b",
    exclude = NULL
  ),
  shock_hypoperfusion = list(
    include = "\\bshock\\b|\\bcardiogenic shock\\b|\\bhypoperfusion\\b|\\bhypotension\\b|\\bvasopressor\\b|\\binotrope\\b",
    exclude = NULL
  )
)

## =========================================================
## 7. Extract comorbidity evidence
## =========================================================

comorbidity_evidence <- purrr::imap_dfr(
  patterns,
  function(pat, nm) {
    make_evidence_rows(
      df_text = text_all,
      comorbidity_name = nm,
      pattern_include = pat$include,
      pattern_exclude = pat$exclude
    )
  }
) %>%
  dplyr::distinct(patientunitstayid, source, comorbidity, evidence_text)

readr::write_csv(
  comorbidity_evidence,
  file.path(dir_e2a, "StepE2A_comorbidity_evidence_long.csv")
)

## =========================================================
## 8. Stay-level comorbidity flags
## =========================================================

comorbidity_flags <- comorbidity_evidence %>%
  dplyr::mutate(value = 1L) %>%
  dplyr::select(patientunitstayid, comorbidity, value) %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(
    id_cols = patientunitstayid,
    names_from = comorbidity,
    values_from = value,
    values_fill = 0
  )

## Ensure all columns exist
for (nm in names(patterns)) {
  if (!nm %in% names(comorbidity_flags)) {
    comorbidity_flags[[nm]] <- 0L
  }
}

comorbidity_flags <- comorbidity_flags %>%
  dplyr::mutate(patientunitstayid = as.character(patientunitstayid))

readr::write_csv(
  comorbidity_flags,
  file.path(dir_e2a, "StepE2A_comorbidity_flags_by_stay.csv")
)

## =========================================================
## 9. Merge with T48 cohort
## =========================================================

ami_t48_e2a <- ami_t48 %>%
  dplyr::left_join(comorbidity_flags, by = "patientunitstayid")

for (nm in names(patterns)) {
  ami_t48_e2a[[nm]] <- tidyr::replace_na(ami_t48_e2a[[nm]], 0L)
}

ami_t48_inicu_e2a <- ami_t48_inicu %>%
  dplyr::left_join(comorbidity_flags, by = "patientunitstayid")

for (nm in names(patterns)) {
  ami_t48_inicu_e2a[[nm]] <- tidyr::replace_na(ami_t48_inicu_e2a[[nm]], 0L)
}

ami_first_e2a <- ami_first %>%
  dplyr::left_join(comorbidity_flags, by = "patientunitstayid")

for (nm in names(patterns)) {
  ami_first_e2a[[nm]] <- tidyr::replace_na(ami_first_e2a[[nm]], 0L)
}

readr::write_csv(
  ami_t48_e2a,
  file.path(dir_e2a, "StepE2A_eICU_AMI_T48_cohort_demographics_comorbidities.csv")
)

readr::write_csv(
  ami_t48_inicu_e2a,
  file.path(dir_e2a, "StepE2A_eICU_AMI_T48_inICU_cohort_demographics_comorbidities.csv")
)

readr::write_csv(
  ami_first_e2a,
  file.path(dir_e2a, "StepE2A_eICU_AMI_first_stay_demographics_comorbidities.csv")
)

## =========================================================
## 10. Summary tables
## =========================================================

summarise_binary <- function(df, var, outcome = "primary_external_endpoint") {
  tibble::tibble(
    variable = var,
    n_available = sum(!is.na(df[[var]])),
    n_positive = sum(df[[var]] == 1, na.rm = TRUE),
    positive_pct = mean(df[[var]] == 1, na.rm = TRUE) * 100,
    mortality_positive_pct = if (outcome %in% names(df) && sum(df[[var]] == 1, na.rm = TRUE) > 0) {
      mean(df[[outcome]][df[[var]] == 1] == 1, na.rm = TRUE) * 100
    } else {
      NA_real_
    },
    mortality_negative_pct = if (outcome %in% names(df) && sum(df[[var]] == 0, na.rm = TRUE) > 0) {
      mean(df[[outcome]][df[[var]] == 0] == 1, na.rm = TRUE) * 100
    } else {
      NA_real_
    }
  )
}

comorbidity_summary_t48 <- purrr::map_dfr(
  names(patterns),
  ~ summarise_binary(ami_t48_e2a, .x, outcome = "primary_external_endpoint")
) %>%
  dplyr::arrange(dplyr::desc(positive_pct))

readr::write_csv(
  comorbidity_summary_t48,
  file.path(dir_tables, "StepE2A_T48_comorbidity_summary.csv")
)

basic_summary <- tibble::tibble(
  cohort = c("T48 landmark cohort", "T48 in-ICU sensitivity cohort", "First AMI ICU stay"),
  n = c(nrow(ami_t48_e2a), nrow(ami_t48_inicu_e2a), nrow(ami_first_e2a)),
  median_age = c(
    median(ami_t48_e2a$age, na.rm = TRUE),
    median(ami_t48_inicu_e2a$age, na.rm = TRUE),
    median(ami_first_e2a$age, na.rm = TRUE)
  ),
  male_pct = c(
    mean(tolower(ami_t48_e2a$sex) == "male", na.rm = TRUE) * 100,
    mean(tolower(ami_t48_inicu_e2a$sex) == "male", na.rm = TRUE) * 100,
    mean(tolower(ami_first_e2a$sex) == "male", na.rm = TRUE) * 100
  ),
  hospital_mortality_after_T48_pct = c(
    mean(ami_t48_e2a$primary_external_endpoint == 1, na.rm = TRUE) * 100,
    mean(ami_t48_inicu_e2a$primary_external_endpoint == 1, na.rm = TRUE) * 100,
    NA_real_
  ),
  icu_mortality_after_T48_pct = c(
    mean(ami_t48_e2a$secondary_icu_mortality_after_T48 == 1, na.rm = TRUE) * 100,
    mean(ami_t48_inicu_e2a$secondary_icu_mortality_after_T48 == 1, na.rm = TRUE) * 100,
    NA_real_
  )
)

readr::write_csv(
  basic_summary,
  file.path(dir_tables, "StepE2A_basic_cohort_summary.csv")
)

evidence_summary <- comorbidity_evidence %>%
  dplyr::group_by(comorbidity, source) %>%
  dplyr::summarise(
    n_evidence_rows = dplyr::n(),
    n_stays = dplyr::n_distinct(patientunitstayid),
    example_text = make_text_example(evidence_text, n = 3),
    .groups = "drop"
  ) %>%
  dplyr::arrange(comorbidity, dplyr::desc(n_stays))

readr::write_csv(
  evidence_summary,
  file.path(dir_tables, "StepE2A_comorbidity_evidence_summary.csv")
)

## =========================================================
## 11. Export Excel workbook
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
    "Main cohort",
    "Comorbidity sources",
    "Text rule",
    "Output use"
  ),
  description = c(
    "Extract demographics and baseline comorbidity flags for the eICU AMI-EDRS external validation cohort.",
    "The primary cohort is the T48 landmark external validation cohort constructed in Step E1.",
    "Comorbidities were extracted from pastHistory, diagnosis, and admissionDx text fields.",
    "Comorbidity flags were generated using prespecified case-insensitive keyword rules with selected exclusions.",
    "These variables will be used as host vulnerability and comorbidity features in downstream AMI-EDRS external prediction."
  )
)

add_sheet(wb, "method_note", method_note)
add_sheet(wb, "basic_summary", basic_summary)
add_sheet(wb, "comorbidity_summary_T48", comorbidity_summary_t48)
add_sheet(wb, "evidence_summary", evidence_summary)
add_sheet(wb, "T48_preview", head(ami_t48_e2a, 5000))
add_sheet(wb, "comorbidity_flags", head(comorbidity_flags, 5000))

out_xlsx <- file.path(
  dir_e2a,
  "StepE2A_demographics_comorbidities_summary.xlsx"
)

openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

## =========================================================
## 12. Save RDS object
## =========================================================

stepE2A_object <- list(
  ami_t48_e2a = ami_t48_e2a,
  ami_t48_inicu_e2a = ami_t48_inicu_e2a,
  ami_first_e2a = ami_first_e2a,
  text_all = text_all,
  comorbidity_evidence = comorbidity_evidence,
  comorbidity_flags = comorbidity_flags,
  comorbidity_summary_t48 = comorbidity_summary_t48,
  evidence_summary = evidence_summary,
  basic_summary = basic_summary,
  patterns = patterns,
  created_at = Sys.time()
)

saveRDS(
  stepE2A_object,
  file.path(dir_objects, "StepE2A_demographics_comorbidities_object.rds")
)

## =========================================================
## 13. Save session info
## =========================================================

sink(file.path(dir_logs, "StepE2A_sessionInfo.txt"))
cat("Step E2A session information\n")
cat("============================\n\n")
print(sessionInfo())
sink()

## =========================================================
## 14. Console summary
## =========================================================

cat("\n================ Step E2A finished ================\n")
cat("T48 cohort:", nrow(ami_t48_e2a), "\n")
cat("T48 in-ICU cohort:", nrow(ami_t48_inicu_e2a), "\n\n")

cat("Basic summary:\n")
print(basic_summary)

cat("\nComorbidity summary in T48 cohort:\n")
print(comorbidity_summary_t48)

cat("\nMain outputs:\n")
cat(file.path(dir_e2a, "StepE2A_eICU_AMI_T48_cohort_demographics_comorbidities.csv"), "\n")
cat(file.path(dir_e2a, "StepE2A_eICU_AMI_T48_inICU_cohort_demographics_comorbidities.csv"), "\n")
cat(file.path(dir_e2a, "StepE2A_comorbidity_evidence_long.csv"), "\n")
cat(file.path(dir_e2a, "StepE2A_comorbidity_flags_by_stay.csv"), "\n")
cat(out_xlsx, "\n")
cat(file.path(dir_objects, "StepE2A_demographics_comorbidities_object.rds"), "\n")
cat("===================================================\n")
