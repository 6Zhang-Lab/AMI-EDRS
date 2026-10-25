############################################################
## Step E1: Construct eICU AMI cohort for AMI-EDRS external validation
##
## Input:
##   StepE0_path_config.rds
##   eICU raw files:
##     patient.csv.gz
##     admissionDx.csv.gz
##     diagnosis.csv.gz
##     apachePredVar.csv.gz
##
## Output:
##   1. AMI diagnosis evidence long table
##   2. AMI stay-level summary
##   3. Adult conservative AMI ICU stays
##   4. First eligible AMI ICU stay per patient
##   5. T48 landmark external validation cohort
##   6. Cohort flow table
##   7. Excel workbook with all key tables
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
## 1. Load Step E0 path configuration
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
dir_tables <- file.path(dirs$tables, "cohort_tables")
dir_objects <- dirs$objects
dir_logs <- dirs$logs

dir.create(dir_cohort, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_objects, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_logs, recursive = TRUE, showWarnings = FALSE)

cat("Raw eICU folder:\n", raw_eicu_dir, "\n\n")
cat("Cohort output folder:\n", dir_cohort, "\n\n")

## =========================================================
## 2. Helper functions
## =========================================================

csv_gz <- function(file_name) {
  file.path(raw_eicu_dir, file_name)
}

safe_fread <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  data.table::fread(path, data.table = FALSE, showProgress = TRUE) %>%
    tibble::as_tibble()
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

get_col <- function(df, candidates, default = NA, required = FALSE, label = NULL) {
  hit <- find_col(df, candidates)
  
  if (is.na(hit)) {
    if (required) {
      stop(
        "Cannot find required column: ",
        ifelse(is.null(label), paste(candidates, collapse = ", "), label)
      )
    }
    return(rep(default, nrow(df)))
  }
  
  df[[hit]]
}

to_numeric_safe <- function(x) {
  suppressWarnings(as.numeric(x))
}

parse_eicu_age <- function(x) {
  x_chr <- stringr::str_squish(as.character(x))
  
  dplyr::case_when(
    is.na(x_chr) | x_chr == "" ~ NA_real_,
    stringr::str_detect(x_chr, "^>\\s*89$|^>\\s*89") ~ 90,
    stringr::str_detect(x_chr, "^[0-9]+") ~ suppressWarnings(as.numeric(stringr::str_extract(x_chr, "[0-9]+"))),
    TRUE ~ NA_real_
  )
}

clean_text <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_replace_all("[\r\n\t]", " ") %>%
    stringr::str_squish() %>%
    tolower()
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
    dplyr::select(patientunitstayid, dplyr::all_of(text_cols), dplyr::everything()) %>%
    dplyr::mutate(
      diagnosis_text_raw = apply(
        dplyr::select(., dplyr::all_of(text_cols)),
        1,
        function(z) paste(z[!is.na(z) & z != ""], collapse = " | ")
      ),
      diagnosis_text_clean = clean_text(diagnosis_text_raw),
      source = source_name
    ) %>%
    dplyr::filter(!is.na(diagnosis_text_clean), diagnosis_text_clean != "") %>%
    dplyr::select(
      patientunitstayid,
      source,
      diagnosis_text_raw,
      diagnosis_text_clean,
      dplyr::everything()
    )
}

extract_offset_if_any <- function(df) {
  offset_col <- names(df)[stringr::str_detect(tolower(names(df)), "offset")]
  
  if (length(offset_col) == 0) {
    return(rep(NA_real_, nrow(df)))
  }
  
  suppressWarnings(as.numeric(df[[offset_col[1]]]))
}

status_expired <- function(x) {
  x0 <- tolower(stringr::str_squish(as.character(x)))
  as.integer(stringr::str_detect(x0, "expired|dead|death|deceased"))
}

status_alive <- function(x) {
  x0 <- tolower(stringr::str_squish(as.character(x)))
  as.integer(stringr::str_detect(x0, "alive|home|rehab|skilled|nursing|other|transfer"))
}

## AMI include and exclude rules
ami_include_pattern <- paste(
  c(
    "\\bacute myocardial infarction\\b",
    "\\bmyocardial infarction\\b",
    "\\bstemi\\b",
    "\\bnstemi\\b",
    "\\bst elevation myocardial infarction\\b",
    "\\bnon[- ]?st elevation myocardial infarction\\b",
    "\\bnon stemi\\b",
    "\\bacute coronary syndrome\\b",
    "\\bsubendocardial infarction\\b",
    "\\bacute mi\\b",
    "\\bami\\b",
    "\\binferior wall mi\\b",
    "\\banterior wall mi\\b",
    "\\bcardiac infarction\\b"
  ),
  collapse = "|"
)

ami_exclude_pattern <- paste(
  c(
    "\\bold myocardial infarction\\b",
    "\\bold mi\\b",
    "\\bhistory of myocardial infarction\\b",
    "\\bhistory of mi\\b",
    "\\bprior myocardial infarction\\b",
    "\\bprior mi\\b",
    "\\bprevious myocardial infarction\\b",
    "\\brule out myocardial infarction\\b",
    "\\br\\/o myocardial infarction\\b",
    "\\brule out mi\\b",
    "\\bdemand ischemia\\b",
    "\\bdemand ischaemia\\b",
    "\\btype 2 myocardial infarction\\b",
    "\\btype ii myocardial infarction\\b",
    "\\btroponin elevation only\\b",
    "\\belevated troponin only\\b",
    "\\bchest pain only\\b"
  ),
  collapse = "|"
)

classify_ami_subtype <- function(txt) {
  dplyr::case_when(
    stringr::str_detect(txt, "\\bstemi\\b|\\bst elevation myocardial infarction\\b") ~ "STEMI",
    stringr::str_detect(txt, "\\bnstemi\\b|\\bnon[- ]?st elevation myocardial infarction\\b|\\bsubendocardial infarction\\b") ~ "NSTEMI/subendocardial",
    stringr::str_detect(txt, "\\bacute coronary syndrome\\b") ~ "ACS",
    stringr::str_detect(txt, "\\bmyocardial infarction\\b|\\bacute mi\\b|\\bami\\b") ~ "AMI/unspecified",
    TRUE ~ "Other/uncertain"
  )
}

make_text_example <- function(x, n = 3) {
  x <- unique(x[!is.na(x) & x != ""])
  if (length(x) == 0) return(NA_character_)
  paste(utils::head(x, n), collapse = " || ")
}

## =========================================================
## 3. Read core files
## =========================================================

cat("Reading patient.csv.gz...\n")
patient <- safe_fread(csv_gz("patient.csv.gz"))

cat("Reading admissionDx.csv.gz...\n")
admission_dx <- safe_fread(csv_gz("admissionDx.csv.gz"))

cat("Reading diagnosis.csv.gz...\n")
diagnosis <- safe_fread(csv_gz("diagnosis.csv.gz"))

cat("Reading apachePredVar.csv.gz...\n")
apache_pred <- safe_fread(csv_gz("apachePredVar.csv.gz"))

cat("\nRows loaded:\n")
cat("patient:", nrow(patient), "\n")
cat("admissionDx:", nrow(admission_dx), "\n")
cat("diagnosis:", nrow(diagnosis), "\n")
cat("apachePredVar:", nrow(apache_pred), "\n\n")

## =========================================================
## 4. Clean patient-level core table
## =========================================================

id_col_patient <- find_col(patient, c("patientunitstayid"))
if (is.na(id_col_patient)) stop("patientunitstayid not found in patient.csv.gz")

patient_clean <- patient %>%
  dplyr::mutate(
    patientunitstayid = as.character(.data[[id_col_patient]]),
    uniquepid = as.character(get_col(., c("uniquepid"), default = NA)),
    patienthealthsystemstayid = as.character(get_col(., c("patienthealthsystemstayid"), default = NA)),
    hospitalid = as.character(get_col(., c("hospitalid"), default = NA)),
    age_raw = as.character(get_col(., c("age"), default = NA)),
    age = parse_eicu_age(age_raw),
    sex = as.character(get_col(., c("gender", "sex"), default = NA)),
    ethnicity = as.character(get_col(., c("ethnicity", "race"), default = NA)),
    admissionweight = to_numeric_safe(get_col(., c("admissionweight"), default = NA)),
    admissionheight = to_numeric_safe(get_col(., c("admissionheight"), default = NA)),
    hospitaladmitoffset = to_numeric_safe(get_col(., c("hospitaladmitoffset"), default = NA)),
    hospitaldischargeoffset = to_numeric_safe(get_col(., c("hospitaldischargeoffset"), default = NA)),
    unitdischargeoffset = to_numeric_safe(get_col(., c("unitdischargeoffset"), default = NA)),
    hospitaldischargestatus = as.character(get_col(., c("hospitaldischargestatus"), default = NA)),
    unitdischargestatus = as.character(get_col(., c("unitdischargestatus"), default = NA)),
    unitvisitnumber = to_numeric_safe(get_col(., c("unitvisitnumber"), default = NA)),
    unittype = as.character(get_col(., c("unittype"), default = NA)),
    unitstaytype = as.character(get_col(., c("unitstaytype"), default = NA)),
    apacheadmissiondx = as.character(get_col(., c("apacheadmissiondx"), default = NA)),
    adult = as.integer(!is.na(age) & age >= 18),
    hospital_mortality = status_expired(hospitaldischargestatus),
    icu_mortality = status_expired(unitdischargestatus),
    hospital_los_from_icu_hours = hospitaldischargeoffset / 60,
    icu_los_hours = unitdischargeoffset / 60,
    alive_at_T48_hospital = as.integer(
      !is.na(hospitaldischargeoffset) &
        hospitaldischargeoffset > 48 * 60 &
        !(hospital_mortality == 1 & hospitaldischargeoffset <= 48 * 60)
    ),
    in_icu_at_T48 = as.integer(
      !is.na(unitdischargeoffset) &
        unitdischargeoffset > 48 * 60 &
        !(icu_mortality == 1 & unitdischargeoffset <= 48 * 60)
    ),
    hospital_mortality_after_T48 = as.integer(
      hospital_mortality == 1 &
        !is.na(hospitaldischargeoffset) &
        hospitaldischargeoffset > 48 * 60
    ),
    icu_mortality_after_T48 = as.integer(
      icu_mortality == 1 &
        !is.na(unitdischargeoffset) &
        unitdischargeoffset > 48 * 60
    ),
    person_id_for_first_stay = dplyr::case_when(
      !is.na(uniquepid) & uniquepid != "" ~ uniquepid,
      !is.na(patienthealthsystemstayid) & patienthealthsystemstayid != "" ~ patienthealthsystemstayid,
      TRUE ~ patientunitstayid
    )
  ) %>%
  dplyr::distinct(patientunitstayid, .keep_all = TRUE)

readr::write_csv(
  patient_clean,
  file.path(dir_cohort, "StepE1_patient_clean.csv")
)

## =========================================================
## 5. Build AMI diagnosis evidence table
## =========================================================

## 5.1 Patient apache admission diagnosis
patient_apache_evidence <- patient_clean %>%
  dplyr::filter(!is.na(apacheadmissiondx), apacheadmissiondx != "") %>%
  dplyr::transmute(
    patientunitstayid,
    source = "patient_apacheadmissiondx",
    diagnosis_offset = NA_real_,
    diagnosis_text_raw = apacheadmissiondx,
    diagnosis_text_clean = clean_text(apacheadmissiondx)
  )

## 5.2 admissionDx
id_col_admdx <- find_col(admission_dx, c("patientunitstayid"))
if (is.na(id_col_admdx)) stop("patientunitstayid not found in admissionDx.csv.gz")

admission_dx_evidence <- collapse_text_cols(
  admission_dx,
  id_col = id_col_admdx,
  source_name = "admissionDx"
) %>%
  dplyr::mutate(
    diagnosis_offset = extract_offset_if_any(.)
  ) %>%
  dplyr::select(
    patientunitstayid,
    source,
    diagnosis_offset,
    diagnosis_text_raw,
    diagnosis_text_clean
  )

## 5.3 diagnosis
id_col_diag <- find_col(diagnosis, c("patientunitstayid"))
if (is.na(id_col_diag)) stop("patientunitstayid not found in diagnosis.csv.gz")

diagnosis_evidence <- collapse_text_cols(
  diagnosis,
  id_col = id_col_diag,
  source_name = "diagnosis"
) %>%
  dplyr::mutate(
    diagnosis_offset = extract_offset_if_any(.)
  ) %>%
  dplyr::select(
    patientunitstayid,
    source,
    diagnosis_offset,
    diagnosis_text_raw,
    diagnosis_text_clean
  )

## 5.4 apachePredVar diagnosis fields, if present
id_col_apache <- find_col(apache_pred, c("patientunitstayid"))

if (!is.na(id_col_apache)) {
  apache_evidence <- collapse_text_cols(
    apache_pred,
    id_col = id_col_apache,
    source_name = "apachePredVar"
  ) %>%
    dplyr::mutate(
      diagnosis_offset = NA_real_
    ) %>%
    dplyr::select(
      patientunitstayid,
      source,
      diagnosis_offset,
      diagnosis_text_raw,
      diagnosis_text_clean
    )
} else {
  apache_evidence <- tibble::tibble()
}

diagnosis_evidence_all <- dplyr::bind_rows(
  patient_apache_evidence,
  admission_dx_evidence,
  diagnosis_evidence,
  apache_evidence
) %>%
  dplyr::mutate(
    patientunitstayid = as.character(patientunitstayid),
    ami_keyword_flag = as.integer(stringr::str_detect(diagnosis_text_clean, ami_include_pattern)),
    ami_exclude_flag = as.integer(stringr::str_detect(diagnosis_text_clean, ami_exclude_pattern)),
    ami_valid_evidence = as.integer(ami_keyword_flag == 1 & ami_exclude_flag == 0),
    ami_subtype = classify_ami_subtype(diagnosis_text_clean),
    conservative_source = as.integer(source %in% c(
      "patient_apacheadmissiondx",
      "admissionDx",
      "apachePredVar"
    )),
    conservative_ami_evidence = as.integer(ami_valid_evidence == 1 & conservative_source == 1)
  )

readr::write_csv(
  diagnosis_evidence_all,
  file.path(dir_cohort, "StepE1_AMI_diagnosis_evidence_long.csv")
)

## Also save only AMI-related evidence for manual review
diagnosis_evidence_ami_only <- diagnosis_evidence_all %>%
  dplyr::filter(ami_keyword_flag == 1 | ami_exclude_flag == 1)

readr::write_csv(
  diagnosis_evidence_ami_only,
  file.path(dir_cohort, "StepE1_AMI_related_diagnosis_evidence_for_review.csv")
)

## =========================================================
## 6. Summarize AMI evidence at ICU-stay level
## =========================================================

ami_by_stay <- diagnosis_evidence_all %>%
  dplyr::group_by(patientunitstayid) %>%
  dplyr::summarise(
    any_ami_keyword = as.integer(any(ami_keyword_flag == 1, na.rm = TRUE)),
    any_ami_exclusion_text = as.integer(any(ami_exclude_flag == 1, na.rm = TRUE)),
    broad_ami = as.integer(any(ami_valid_evidence == 1, na.rm = TRUE)),
    conservative_ami = as.integer(any(conservative_ami_evidence == 1, na.rm = TRUE)),
    n_ami_evidence_rows = sum(ami_valid_evidence == 1, na.rm = TRUE),
    n_conservative_ami_evidence_rows = sum(conservative_ami_evidence == 1, na.rm = TRUE),
    evidence_sources = paste(sort(unique(source[ami_valid_evidence == 1])), collapse = "; "),
    conservative_sources = paste(sort(unique(source[conservative_ami_evidence == 1])), collapse = "; "),
    ami_subtype_summary = paste(sort(unique(ami_subtype[ami_valid_evidence == 1])), collapse = "; "),
    first_ami_offset = suppressWarnings(min(diagnosis_offset[ami_valid_evidence == 1], na.rm = TRUE)),
    ami_text_examples = make_text_example(diagnosis_text_raw[ami_valid_evidence == 1], n = 4),
    excluded_text_examples = make_text_example(diagnosis_text_raw[ami_exclude_flag == 1], n = 3),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    first_ami_offset = ifelse(is.infinite(first_ami_offset), NA_real_, first_ami_offset)
  )

readr::write_csv(
  ami_by_stay,
  file.path(dir_cohort, "StepE1_AMI_candidate_by_stay.csv")
)

## Keyword-level summary
keyword_summary <- diagnosis_evidence_all %>%
  dplyr::filter(ami_keyword_flag == 1 | ami_exclude_flag == 1) %>%
  dplyr::count(source, ami_subtype, ami_keyword_flag, ami_exclude_flag, name = "n_rows") %>%
  dplyr::arrange(source, dplyr::desc(n_rows))

readr::write_csv(
  keyword_summary,
  file.path(dir_cohort, "StepE1_diagnosis_keyword_summary.csv")
)

## =========================================================
## 7. Merge patient table and AMI evidence
## =========================================================

cohort_all_stays <- patient_clean %>%
  dplyr::left_join(ami_by_stay, by = "patientunitstayid") %>%
  dplyr::mutate(
    any_ami_keyword = tidyr::replace_na(any_ami_keyword, 0L),
    any_ami_exclusion_text = tidyr::replace_na(any_ami_exclusion_text, 0L),
    broad_ami = tidyr::replace_na(broad_ami, 0L),
    conservative_ami = tidyr::replace_na(conservative_ami, 0L),
    n_ami_evidence_rows = tidyr::replace_na(n_ami_evidence_rows, 0L),
    n_conservative_ami_evidence_rows = tidyr::replace_na(n_conservative_ami_evidence_rows, 0L),
    eicu_ami_cohort_all_stays = as.integer(adult == 1 & conservative_ami == 1)
  )

readr::write_csv(
  cohort_all_stays,
  file.path(dir_cohort, "StepE1_eICU_AMI_cohort_all_stays_with_flags.csv")
)

## =========================================================
## 8. Select adult conservative AMI ICU stays
## =========================================================

ami_all_stays <- cohort_all_stays %>%
  dplyr::filter(
    adult == 1,
    conservative_ami == 1
  ) %>%
  dplyr::mutate(
    ami_subtype_main = dplyr::case_when(
      stringr::str_detect(ami_subtype_summary, "STEMI") ~ "STEMI",
      stringr::str_detect(ami_subtype_summary, "NSTEMI|subendocardial") ~ "NSTEMI/subendocardial",
      stringr::str_detect(ami_subtype_summary, "ACS") ~ "ACS",
      stringr::str_detect(ami_subtype_summary, "AMI/unspecified") ~ "AMI/unspecified",
      TRUE ~ "Other/uncertain"
    )
  )

readr::write_csv(
  ami_all_stays,
  file.path(dir_cohort, "StepE1_eICU_conservative_AMI_adult_all_ICU_stays.csv")
)

## First eligible AMI ICU stay per patient
ami_first_stay <- ami_all_stays %>%
  dplyr::arrange(
    person_id_for_first_stay,
    dplyr::coalesce(unitvisitnumber, 9999),
    dplyr::coalesce(hospitaladmitoffset, 999999),
    as.numeric(patientunitstayid)
  ) %>%
  dplyr::group_by(person_id_for_first_stay) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    first_eligible_ami_icu_stay = 1L
  )

readr::write_csv(
  ami_first_stay,
  file.path(dir_cohort, "StepE1_eICU_conservative_AMI_first_ICU_stay_per_patient.csv")
)

## T48 landmark external validation cohort
ami_t48_cohort <- ami_first_stay %>%
  dplyr::filter(
    alive_at_T48_hospital == 1
  ) %>%
  dplyr::mutate(
    eicu_external_validation_T48 = 1L,
    primary_external_endpoint = hospital_mortality_after_T48,
    secondary_icu_mortality_after_T48 = icu_mortality_after_T48
  )

readr::write_csv(
  ami_t48_cohort,
  file.path(dir_cohort, "StepE1_eICU_AMI_T48_landmark_external_validation_cohort.csv")
)

## In-ICU-at-T48 cohort, useful for sensitivity analysis
ami_t48_inicu_cohort <- ami_first_stay %>%
  dplyr::filter(
    in_icu_at_T48 == 1
  ) %>%
  dplyr::mutate(
    eicu_external_validation_T48_inICU = 1L,
    primary_external_endpoint = hospital_mortality_after_T48,
    secondary_icu_mortality_after_T48 = icu_mortality_after_T48
  )

readr::write_csv(
  ami_t48_inicu_cohort,
  file.path(dir_cohort, "StepE1_eICU_AMI_T48_inICU_sensitivity_cohort.csv")
)

## =========================================================
## 9. Cohort flow table
## =========================================================

flow_table <- tibble::tribble(
  ~step_order, ~step, ~n_stays, ~n_unique_persons, ~excluded_from_previous, ~description,
  1,
  "All eICU ICU stays",
  nrow(patient_clean),
  dplyr::n_distinct(patient_clean$person_id_for_first_stay),
  NA_integer_,
  "All ICU stays in patient.csv.gz.",
  
  2,
  "Adult ICU stays",
  sum(patient_clean$adult == 1, na.rm = TRUE),
  dplyr::n_distinct(patient_clean$person_id_for_first_stay[patient_clean$adult == 1]),
  nrow(patient_clean) - sum(patient_clean$adult == 1, na.rm = TRUE),
  "Restricted to age >=18 years; age >89 was coded as 90.",
  
  3,
  "Broad AMI-related ICU stays",
  sum(cohort_all_stays$adult == 1 & cohort_all_stays$broad_ami == 1, na.rm = TRUE),
  dplyr::n_distinct(cohort_all_stays$person_id_for_first_stay[cohort_all_stays$adult == 1 & cohort_all_stays$broad_ami == 1]),
  sum(patient_clean$adult == 1, na.rm = TRUE) -
    sum(cohort_all_stays$adult == 1 & cohort_all_stays$broad_ami == 1, na.rm = TRUE),
  "AMI-related evidence identified from admissionDx, diagnosis, apache admission diagnosis, or apachePredVar after excluding old/prior/rule-out/type-2/demand ischemia text.",
  
  4,
  "Conservative adult AMI ICU stays",
  nrow(ami_all_stays),
  dplyr::n_distinct(ami_all_stays$person_id_for_first_stay),
  sum(cohort_all_stays$adult == 1 & cohort_all_stays$broad_ami == 1, na.rm = TRUE) - nrow(ami_all_stays),
  "Restricted to conservative AMI evidence from admission diagnosis or APACHE admission diagnosis fields.",
  
  5,
  "First eligible AMI ICU stay per patient",
  nrow(ami_first_stay),
  dplyr::n_distinct(ami_first_stay$person_id_for_first_stay),
  nrow(ami_all_stays) - nrow(ami_first_stay),
  "Only the first eligible AMI ICU stay per patient was retained.",
  
  6,
  "T48 landmark external validation cohort",
  nrow(ami_t48_cohort),
  dplyr::n_distinct(ami_t48_cohort$person_id_for_first_stay),
  nrow(ami_first_stay) - nrow(ami_t48_cohort),
  "Patients alive and not hospital-discharged before the T48 landmark; primary endpoint is in-hospital mortality after T48.",
  
  7,
  "T48 in-ICU sensitivity cohort",
  nrow(ami_t48_inicu_cohort),
  dplyr::n_distinct(ami_t48_inicu_cohort$person_id_for_first_stay),
  nrow(ami_first_stay) - nrow(ami_t48_inicu_cohort),
  "Patients still in ICU at T48; used for sensitivity analysis with ICU-continuous data availability."
)

readr::write_csv(
  flow_table,
  file.path(dir_cohort, "StepE1_eICU_AMI_cohort_flow.csv")
)

## =========================================================
## 10. Descriptive summary tables
## =========================================================

subtype_summary <- ami_first_stay %>%
  dplyr::count(ami_subtype_main, name = "n") %>%
  dplyr::mutate(percent = n / sum(n) * 100) %>%
  dplyr::arrange(dplyr::desc(n))

t48_outcome_summary <- tibble::tibble(
  cohort = c(
    "First eligible AMI ICU stay per patient",
    "T48 landmark external validation cohort",
    "T48 in-ICU sensitivity cohort"
  ),
  n = c(nrow(ami_first_stay), nrow(ami_t48_cohort), nrow(ami_t48_inicu_cohort)),
  hospital_mortality_n = c(
    sum(ami_first_stay$hospital_mortality == 1, na.rm = TRUE),
    sum(ami_t48_cohort$primary_external_endpoint == 1, na.rm = TRUE),
    sum(ami_t48_inicu_cohort$primary_external_endpoint == 1, na.rm = TRUE)
  ),
  hospital_mortality_pct = hospital_mortality_n / n * 100,
  icu_mortality_n = c(
    sum(ami_first_stay$icu_mortality == 1, na.rm = TRUE),
    sum(ami_t48_cohort$secondary_icu_mortality_after_T48 == 1, na.rm = TRUE),
    sum(ami_t48_inicu_cohort$secondary_icu_mortality_after_T48 == 1, na.rm = TRUE)
  ),
  icu_mortality_pct = icu_mortality_n / n * 100,
  median_hospital_los_h = c(
    median(ami_first_stay$hospital_los_from_icu_hours, na.rm = TRUE),
    median(ami_t48_cohort$hospital_los_from_icu_hours, na.rm = TRUE),
    median(ami_t48_inicu_cohort$hospital_los_from_icu_hours, na.rm = TRUE)
  ),
  median_icu_los_h = c(
    median(ami_first_stay$icu_los_hours, na.rm = TRUE),
    median(ami_t48_cohort$icu_los_hours, na.rm = TRUE),
    median(ami_t48_inicu_cohort$icu_los_hours, na.rm = TRUE)
  )
)

readr::write_csv(
  subtype_summary,
  file.path(dir_tables, "StepE1_AMI_subtype_summary.csv")
)

readr::write_csv(
  t48_outcome_summary,
  file.path(dir_tables, "StepE1_T48_cohort_outcome_summary.csv")
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
    "AMI identification",
    "Conservative AMI cohort",
    "Exclusion text",
    "First-stay rule",
    "T48 landmark cohort",
    "Primary endpoint",
    "Sensitivity cohort"
  ),
  description = c(
    "AMI evidence was identified from patient apache admission diagnosis, admissionDx, diagnosis, and apachePredVar text fields using prespecified keyword rules.",
    "The main cohort required valid AMI evidence from admission diagnosis or APACHE admission diagnosis fields, after excluding old/prior/rule-out/type-2/demand ischemia text.",
    "Old MI, prior/history of MI, rule-out MI, type 2 MI, demand ischemia, and troponin elevation only were excluded from valid AMI evidence.",
    "When multiple eligible AMI ICU stays existed for the same patient/person identifier, only the first eligible stay was retained.",
    "The main external validation cohort included patients alive and not hospital-discharged before 48 hours after ICU admission.",
    "Because post-discharge 30-day mortality is not uniformly available in eICU, in-hospital mortality after T48 was used as the primary external validation endpoint.",
    "A stricter in-ICU-at-T48 cohort was also saved for sensitivity analysis."
  )
)

add_sheet(wb, "method_note", method_note)
add_sheet(wb, "cohort_flow", flow_table)
add_sheet(wb, "outcome_summary", t48_outcome_summary)
add_sheet(wb, "ami_subtype_summary", subtype_summary)
add_sheet(wb, "keyword_summary", keyword_summary)
add_sheet(wb, "ami_by_stay_preview", head(ami_by_stay, 5000))
add_sheet(wb, "t48_cohort_preview", head(ami_t48_cohort, 5000))

out_xlsx <- file.path(
  dir_cohort,
  "StepE1_eICU_AMI_cohort_construction_summary.xlsx"
)

openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

## =========================================================
## 12. Save RDS object
## =========================================================

stepE1_object <- list(
  patient_clean = patient_clean,
  diagnosis_evidence_all = diagnosis_evidence_all,
  diagnosis_evidence_ami_only = diagnosis_evidence_ami_only,
  ami_by_stay = ami_by_stay,
  cohort_all_stays = cohort_all_stays,
  ami_all_stays = ami_all_stays,
  ami_first_stay = ami_first_stay,
  ami_t48_cohort = ami_t48_cohort,
  ami_t48_inicu_cohort = ami_t48_inicu_cohort,
  flow_table = flow_table,
  keyword_summary = keyword_summary,
  subtype_summary = subtype_summary,
  t48_outcome_summary = t48_outcome_summary,
  ami_include_pattern = ami_include_pattern,
  ami_exclude_pattern = ami_exclude_pattern,
  created_at = Sys.time()
)

saveRDS(
  stepE1_object,
  file.path(dir_objects, "StepE1_eICU_AMI_cohort_construction_object.rds")
)

## =========================================================
## 13. Save session info
## =========================================================

sink(file.path(dir_logs, "StepE1_sessionInfo.txt"))
cat("Step E1 session information\n")
cat("===========================\n\n")
print(sessionInfo())
sink()

## =========================================================
## 14. Console summary
## =========================================================

cat("\n================ Step E1 finished ================\n")
cat("All eICU ICU stays:", nrow(patient_clean), "\n")
cat("Adult ICU stays:", sum(patient_clean$adult == 1, na.rm = TRUE), "\n")
cat("Broad AMI adult ICU stays:", sum(cohort_all_stays$adult == 1 & cohort_all_stays$broad_ami == 1, na.rm = TRUE), "\n")
cat("Conservative adult AMI ICU stays:", nrow(ami_all_stays), "\n")
cat("First eligible AMI ICU stay per patient:", nrow(ami_first_stay), "\n")
cat("T48 landmark external validation cohort:", nrow(ami_t48_cohort), "\n")
cat("T48 in-ICU sensitivity cohort:", nrow(ami_t48_inicu_cohort), "\n\n")

cat("Primary endpoint in T48 cohort: in-hospital mortality after T48\n")
cat("Deaths:", sum(ami_t48_cohort$primary_external_endpoint == 1, na.rm = TRUE), "\n")
cat("Mortality rate:", sprintf("%.1f%%", mean(ami_t48_cohort$primary_external_endpoint == 1, na.rm = TRUE) * 100), "\n\n")

cat("Main outputs:\n")
cat(file.path(dir_cohort, "StepE1_patient_clean.csv"), "\n")
cat(file.path(dir_cohort, "StepE1_AMI_diagnosis_evidence_long.csv"), "\n")
cat(file.path(dir_cohort, "StepE1_AMI_related_diagnosis_evidence_for_review.csv"), "\n")
cat(file.path(dir_cohort, "StepE1_AMI_candidate_by_stay.csv"), "\n")
cat(file.path(dir_cohort, "StepE1_eICU_conservative_AMI_adult_all_ICU_stays.csv"), "\n")
cat(file.path(dir_cohort, "StepE1_eICU_conservative_AMI_first_ICU_stay_per_patient.csv"), "\n")
cat(file.path(dir_cohort, "StepE1_eICU_AMI_T48_landmark_external_validation_cohort.csv"), "\n")
cat(file.path(dir_cohort, "StepE1_eICU_AMI_T48_inICU_sensitivity_cohort.csv"), "\n")
cat(file.path(dir_cohort, "StepE1_eICU_AMI_cohort_flow.csv"), "\n")
cat(out_xlsx, "\n")
cat(file.path(dir_objects, "StepE1_eICU_AMI_cohort_construction_object.rds"), "\n")
cat("==================================================\n")

