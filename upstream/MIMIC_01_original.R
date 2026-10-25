############################################################
## Step 01: MIMIC-IV 3.1 AMI ICU cohort construction
## Project: AMI Dynamic Risk Evolution
##
## Aim:
## 1. Strict AMI diagnosis extraction
## 2. Exclude Type 2 MI / ambiguous other MI in main cohort
## 3. Build adult index AMI ICU cohort
## 4. Save flowchart counts and diagnosis audit
## 5. Check T0/T24/T48/T72 landmark eligibility
## 6. Check variable availability across 0-6h, 0-24h, 0-48h, 0-72h
##
## Raw data:
## <AMI_MIMIC_RAW>
##
## Output:
## <AMI_MIMIC_PROJECT>
############################################################

rm(list = ls())
gc()

options(stringsAsFactors = FALSE)

## =========================================================
## 0. Packages
## =========================================================

pkg_needed <- c(
  "DBI", "duckdb", "dplyr", "tidyr", "stringr",
  "purrr", "tibble", "data.table", "openxlsx", "readr"
)

for (p in pkg_needed) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

invisible(lapply(pkg_needed, library, character.only = TRUE))

## =========================================================
## 1. Paths and project settings
## =========================================================

mimic_iv_root <- Sys.getenv("AMI_MIMIC_RAW", unset=file.path(getwd(), "data/mimic-iv-3.1"))

desktop_root <- dirname(Sys.getenv("AMI_MIMIC_PROJECT", unset=file.path(getwd(), "work/MIMIC")))

project_root <- Sys.getenv("AMI_MIMIC_PROJECT", unset=file.path(getwd(), "work/MIMIC"))

dir_00_logs     <- file.path(project_root, "00_logs")
dir_01_cohort   <- file.path(project_root, "01_cohort")
dir_02_maps     <- file.path(project_root, "02_diagnosis_and_variable_maps")
dir_03_dataset  <- file.path(project_root, "03_analysis_dataset")
dir_04_tables   <- file.path(project_root, "04_flowchart_and_descriptive_tables")
dir_05_duckdb   <- file.path(project_root, "05_duckdb_cache")

dir.create(project_root, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_00_logs, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_01_cohort, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_02_maps, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_03_dataset, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_04_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_05_duckdb, recursive = TRUE, showWarnings = FALSE)

hosp_dir <- file.path(mimic_iv_root, "hosp")
icu_dir  <- file.path(mimic_iv_root, "icu")

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

log_file <- file.path(
  dir_00_logs,
  paste0("Step01_AMI_cohort_construction_", timestamp, ".log")
)

duckdb_file <- file.path(
  dir_05_duckdb,
  "Step01_AMI_cohort_construction.duckdb"
)

out_xlsx <- file.path(
  dir_04_tables,
  "Step01_AMI_cohort_diagnosis_flowchart_missingness.xlsx"
)

out_cohort_csv <- file.path(
  dir_01_cohort,
  "Step01_AMI_index_ICU_cohort_base.csv"
)

out_dx_audit_csv <- file.path(
  dir_02_maps,
  "Step01_AMI_ICD_code_audit.csv"
)

out_main_dx_audit_csv <- file.path(
  dir_02_maps,
  "Step01_AMI_main_definition_selected_ICD_audit.csv"
)

out_lab_map_csv <- file.path(
  dir_02_maps,
  "Step01_precise_lab_item_map.csv"
)

out_vital_map_csv <- file.path(
  dir_02_maps,
  "Step01_precise_vital_item_map.csv"
)

## Main AMI definition
## Conservative AMI:
## ICD-9: 410*, excluding 410x2
## ICD-10: I21* or I22*, excluding I21A1 and I21A9
## Position: seq_num <= 3
main_position <- "top3"
main_definition <- "conservative_ami_excluding_type2_and_ambiguous_other"

## Landmark windows
landmark_hours <- c(0, 24, 48, 72)
availability_windows <- c(6, 24, 48, 72)

## Start log
sink(log_file, split = TRUE)

cat("\n============================================================\n")
cat("Step 01: AMI cohort construction started\n")
cat("Time:", as.character(Sys.time()), "\n")
cat("Raw data:", mimic_iv_root, "\n")
cat("Project root:", project_root, "\n")
cat("Main AMI definition:", main_definition, "\n")
cat("Main diagnosis position:", main_position, "\n")
cat("============================================================\n\n")

## =========================================================
## 2. Check required files
## =========================================================

required_files <- c(
  file.path(hosp_dir, "admissions.csv.gz"),
  file.path(hosp_dir, "patients.csv.gz"),
  file.path(hosp_dir, "diagnoses_icd.csv.gz"),
  file.path(hosp_dir, "d_icd_diagnoses.csv.gz"),
  file.path(hosp_dir, "labevents.csv.gz"),
  file.path(hosp_dir, "d_labitems.csv.gz"),
  file.path(icu_dir, "icustays.csv.gz"),
  file.path(icu_dir, "chartevents.csv.gz"),
  file.path(icu_dir, "d_items.csv.gz")
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required files:\n",
    paste(missing_files, collapse = "\n")
  )
}

cat("All required files found.\n\n")

## =========================================================
## 3. Helper functions
## =========================================================

sql_path <- function(path) {
  path <- gsub("\\\\", "/", path)
  paste0("'", gsub("'", "''", path), "'")
}

create_csv_view <- function(con, view_name, path) {
  sql <- sprintf(
    "
    CREATE OR REPLACE VIEW %s AS
    SELECT *
    FROM read_csv_auto(%s, header = true, sample_size = 100000);
    ",
    view_name,
    sql_path(path)
  )
  DBI::dbExecute(con, sql)
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

safe_fwrite <- function(x, path) {
  data.table::fwrite(x, path)
  cat("Saved:", path, "\n")
}

## =========================================================
## 4. Connect DuckDB and create views
## =========================================================

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = duckdb_file,
  read_only = FALSE
)

DBI::dbExecute(con, "PRAGMA threads=4;")
DBI::dbExecute(con, "PRAGMA memory_limit='8GB';")

create_csv_view(con, "admissions",      file.path(hosp_dir, "admissions.csv.gz"))
create_csv_view(con, "patients",        file.path(hosp_dir, "patients.csv.gz"))
create_csv_view(con, "diagnoses_icd",   file.path(hosp_dir, "diagnoses_icd.csv.gz"))
create_csv_view(con, "d_icd_diagnoses", file.path(hosp_dir, "d_icd_diagnoses.csv.gz"))
create_csv_view(con, "labevents",       file.path(hosp_dir, "labevents.csv.gz"))
create_csv_view(con, "d_labitems",      file.path(hosp_dir, "d_labitems.csv.gz"))
create_csv_view(con, "icustays",        file.path(icu_dir, "icustays.csv.gz"))
create_csv_view(con, "chartevents",     file.path(icu_dir, "chartevents.csv.gz"))
create_csv_view(con, "d_items",         file.path(icu_dir, "d_items.csv.gz"))

cat("DuckDB views created.\n\n")

## =========================================================
## 5. Clean diagnosis table and define AMI flags
## =========================================================
## AMI definitions:
##
## Broad AMI:
##   ICD-9: 410*
##   ICD-10: I21* or I22*
##
## Exclude Type 2 MI:
##   Exclude ICD-10 I21A1
##
## Conservative AMI:
##   ICD-9: 410*, excluding 410x2
##   ICD-10: I21* or I22*, excluding I21A1 and I21A9
##
## Rationale:
##   ICD-9 410x2 indicates subsequent episode of care and is less suitable
##   for defining an index acute MI hospitalization.
##   ICD-10 I21A1 is Type 2 MI.
##   ICD-10 I21A9 is an ambiguous other MI type and is excluded in the
##   conservative main cohort.
############################################################

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE dx_all_clean AS
SELECT
  CAST(d.subject_id AS BIGINT) AS subject_id,
  CAST(d.hadm_id AS BIGINT) AS hadm_id,
  TRY_CAST(d.seq_num AS INTEGER) AS seq_num,
  CAST(d.icd_version AS INTEGER) AS icd_version,
  upper(replace(trim(CAST(d.icd_code AS VARCHAR)), '.', '')) AS icd_code_clean,
  CAST(dic.long_title AS VARCHAR) AS diagnosis_title
FROM diagnoses_icd d
LEFT JOIN d_icd_diagnoses dic
  ON upper(replace(trim(CAST(d.icd_code AS VARCHAR)), '.', ''))
     = upper(replace(trim(CAST(dic.icd_code AS VARCHAR)), '.', ''))
 AND CAST(d.icd_version AS INTEGER) = CAST(dic.icd_version AS INTEGER);
")

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE ami_dx_candidates AS
SELECT
  *,
  
  CASE
    WHEN icd_version = 9
         AND icd_code_clean LIKE '410%'
      THEN 1
    WHEN icd_version = 10
         AND (
           icd_code_clean LIKE 'I21%'
           OR icd_code_clean LIKE 'I22%'
         )
      THEN 1
    ELSE 0
  END AS ami_broad_flag,

  CASE
    WHEN icd_version = 10
         AND icd_code_clean = 'I21A1'
      THEN 1
    ELSE 0
  END AS type2_mi_flag,

  CASE
    WHEN icd_version = 10
         AND icd_code_clean = 'I21A9'
      THEN 1
    ELSE 0
  END AS ambiguous_other_mi_flag,

  CASE
    WHEN icd_version = 9
         AND icd_code_clean LIKE '410%'
         AND length(icd_code_clean) >= 5
         AND right(icd_code_clean, 1) = '2'
      THEN 1
    ELSE 0
  END AS icd9_subsequent_episode_flag,

  CASE
    WHEN icd_version = 9
         AND icd_code_clean LIKE '410%'
      THEN
        CASE
          WHEN length(icd_code_clean) >= 5
               AND right(icd_code_clean, 1) = '2'
          THEN 0
          ELSE 1
        END
    WHEN icd_version = 10
         AND (
           icd_code_clean LIKE 'I21%'
           OR icd_code_clean LIKE 'I22%'
         )
         AND icd_code_clean <> 'I21A1'
      THEN 1
    ELSE 0
  END AS ami_exclude_type2_flag,

  CASE
    WHEN icd_version = 9
         AND icd_code_clean LIKE '410%'
      THEN
        CASE
          WHEN length(icd_code_clean) >= 5
               AND right(icd_code_clean, 1) = '2'
          THEN 0
          ELSE 1
        END
    WHEN icd_version = 10
         AND (
           icd_code_clean LIKE 'I21%'
           OR icd_code_clean LIKE 'I22%'
         )
         AND icd_code_clean NOT IN ('I21A1', 'I21A9')
      THEN 1
    ELSE 0
  END AS ami_conservative_flag,

  CASE
    WHEN seq_num = 1 THEN 'primary'
    WHEN seq_num BETWEEN 2 AND 3 THEN 'top2_3'
    WHEN seq_num >= 4 THEN 'after_top3'
    ELSE 'unknown'
  END AS diagnosis_position_group,

  CASE
    WHEN icd_version = 10 AND icd_code_clean LIKE 'I210%' THEN 'STEMI'
    WHEN icd_version = 10 AND icd_code_clean LIKE 'I211%' THEN 'STEMI'
    WHEN icd_version = 10 AND icd_code_clean LIKE 'I212%' THEN 'STEMI'
    WHEN icd_version = 10 AND icd_code_clean LIKE 'I213%' THEN 'STEMI'
    WHEN icd_version = 10 AND icd_code_clean LIKE 'I214%' THEN 'NSTEMI'
    WHEN icd_version = 10 AND icd_code_clean LIKE 'I21A%' THEN 'Other_MI_type'
    WHEN icd_version = 10 AND icd_code_clean LIKE 'I219%' THEN 'AMI_unspecified'
    WHEN icd_version = 10 AND icd_code_clean LIKE 'I22%' THEN 'Subsequent_AMI'
    
    WHEN icd_version = 9
         AND icd_code_clean LIKE '4107%'
      THEN 'NSTEMI_or_subendocardial'
    WHEN icd_version = 9
         AND icd_code_clean LIKE '4109%'
      THEN 'AMI_unspecified'
    WHEN icd_version = 9
         AND icd_code_clean LIKE '410%'
      THEN 'STEMI_or_location_specific'
    ELSE 'other'
  END AS ami_subtype_group

FROM dx_all_clean
WHERE
  (
    icd_version = 9
    AND icd_code_clean LIKE '410%'
  )
  OR
  (
    icd_version = 10
    AND (
      icd_code_clean LIKE 'I21%'
      OR icd_code_clean LIKE 'I22%'
    )
  );
")

cat("AMI diagnosis candidates created.\n\n")

## =========================================================
## 6. AMI ICD code audit
## =========================================================

ami_icd_code_audit <- DBI::dbGetQuery(con, "
SELECT
  icd_version,
  icd_code_clean,
  diagnosis_title,
  ami_subtype_group,
  MAX(ami_broad_flag) AS ami_broad_flag,
  MAX(type2_mi_flag) AS type2_mi_flag,
  MAX(ambiguous_other_mi_flag) AS ambiguous_other_mi_flag,
  MAX(icd9_subsequent_episode_flag) AS icd9_subsequent_episode_flag,
  MAX(ami_exclude_type2_flag) AS ami_exclude_type2_flag,
  MAX(ami_conservative_flag) AS ami_conservative_flag,
  COUNT(*) AS n_diagnosis_rows,
  COUNT(DISTINCT subject_id) AS n_patients,
  COUNT(DISTINCT hadm_id) AS n_admissions,
  MIN(seq_num) AS min_seq_num,
  MAX(seq_num) AS max_seq_num
FROM ami_dx_candidates
GROUP BY
  icd_version,
  icd_code_clean,
  diagnosis_title,
  ami_subtype_group
ORDER BY
  icd_version,
  icd_code_clean;
")

safe_fwrite(ami_icd_code_audit, out_dx_audit_csv)

## =========================================================
## 7. Compare AMI definitions
## =========================================================

ami_definition_counts <- DBI::dbGetQuery(con, "
WITH base AS (
  SELECT
    subject_id,
    hadm_id,
    MIN(seq_num) AS min_seq_num,
    MAX(ami_broad_flag) AS ami_broad_flag,
    MAX(ami_exclude_type2_flag) AS ami_exclude_type2_flag,
    MAX(ami_conservative_flag) AS ami_conservative_flag,
    MAX(type2_mi_flag) AS has_type2_mi,
    MAX(ambiguous_other_mi_flag) AS has_ambiguous_other_mi,
    MAX(icd9_subsequent_episode_flag) AS has_icd9_subsequent_episode
  FROM ami_dx_candidates
  GROUP BY subject_id, hadm_id
),
defs AS (
  SELECT 'broad_primary' AS definition, subject_id, hadm_id
  FROM base
  WHERE ami_broad_flag = 1 AND min_seq_num = 1

  UNION ALL

  SELECT 'broad_top3' AS definition, subject_id, hadm_id
  FROM base
  WHERE ami_broad_flag = 1 AND min_seq_num <= 3

  UNION ALL

  SELECT 'broad_any_position' AS definition, subject_id, hadm_id
  FROM base
  WHERE ami_broad_flag = 1

  UNION ALL

  SELECT 'exclude_type2_primary' AS definition, subject_id, hadm_id
  FROM base
  WHERE ami_exclude_type2_flag = 1 AND min_seq_num = 1

  UNION ALL

  SELECT 'exclude_type2_top3' AS definition, subject_id, hadm_id
  FROM base
  WHERE ami_exclude_type2_flag = 1 AND min_seq_num <= 3

  UNION ALL

  SELECT 'exclude_type2_any_position' AS definition, subject_id, hadm_id
  FROM base
  WHERE ami_exclude_type2_flag = 1

  UNION ALL

  SELECT 'conservative_primary' AS definition, subject_id, hadm_id
  FROM base
  WHERE ami_conservative_flag = 1 AND min_seq_num = 1

  UNION ALL

  SELECT 'conservative_top3' AS definition, subject_id, hadm_id
  FROM base
  WHERE ami_conservative_flag = 1 AND min_seq_num <= 3

  UNION ALL

  SELECT 'conservative_any_position' AS definition, subject_id, hadm_id
  FROM base
  WHERE ami_conservative_flag = 1
)
SELECT
  definition,
  COUNT(*) AS n_admissions,
  COUNT(DISTINCT subject_id) AS n_patients
FROM defs
GROUP BY definition
ORDER BY
  CASE definition
    WHEN 'broad_primary' THEN 1
    WHEN 'broad_top3' THEN 2
    WHEN 'broad_any_position' THEN 3
    WHEN 'exclude_type2_primary' THEN 4
    WHEN 'exclude_type2_top3' THEN 5
    WHEN 'exclude_type2_any_position' THEN 6
    WHEN 'conservative_primary' THEN 7
    WHEN 'conservative_top3' THEN 8
    WHEN 'conservative_any_position' THEN 9
    ELSE 99
  END;
")

print(ami_definition_counts)

## =========================================================
## 8. Select main AMI diagnosis cohort
## =========================================================
## Main cohort:
##   conservative AMI flag = 1
##   diagnosis position seq_num <= 3

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE ami_selected_dx AS
SELECT
  subject_id,
  hadm_id,
  MIN(seq_num) AS ami_min_seq_num,
  COUNT(*) AS n_ami_dx_rows,
  MAX(type2_mi_flag) AS has_type2_mi_code,
  MAX(ambiguous_other_mi_flag) AS has_ambiguous_other_mi_code,
  MAX(icd9_subsequent_episode_flag) AS has_icd9_subsequent_episode_code,
  string_agg(DISTINCT icd_code_clean, '; ') AS ami_icd_codes,
  string_agg(DISTINCT diagnosis_title, '; ') AS ami_diagnosis_titles,
  string_agg(DISTINCT ami_subtype_group, '; ') AS ami_subtype_groups
FROM ami_dx_candidates
WHERE ami_conservative_flag = 1
  AND seq_num <= 3
GROUP BY subject_id, hadm_id;
")

main_selected_dx_audit <- DBI::dbGetQuery(con, "
SELECT
  d.icd_version,
  d.icd_code_clean,
  d.diagnosis_title,
  d.ami_subtype_group,
  d.seq_num,
  COUNT(*) AS n_rows,
  COUNT(DISTINCT d.subject_id) AS n_patients,
  COUNT(DISTINCT d.hadm_id) AS n_admissions
FROM ami_dx_candidates d
INNER JOIN ami_selected_dx s
  ON d.subject_id = s.subject_id
 AND d.hadm_id = s.hadm_id
WHERE d.ami_conservative_flag = 1
  AND d.seq_num <= 3
GROUP BY
  d.icd_version,
  d.icd_code_clean,
  d.diagnosis_title,
  d.ami_subtype_group,
  d.seq_num
ORDER BY
  d.seq_num,
  d.icd_version,
  d.icd_code_clean;
")

safe_fwrite(main_selected_dx_audit, out_main_dx_audit_csv)

cat("Main AMI diagnosis cohort selected.\n\n")

## =========================================================
## 9. Build hospital and ICU cohorts
## =========================================================

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE ami_hosp_all AS
SELECT
  s.subject_id,
  s.hadm_id,
  s.ami_min_seq_num,
  s.n_ami_dx_rows,
  s.has_type2_mi_code,
  s.has_ambiguous_other_mi_code,
  s.has_icd9_subsequent_episode_code,
  s.ami_icd_codes,
  s.ami_diagnosis_titles,
  s.ami_subtype_groups,

  CAST(a.admittime AS TIMESTAMP) AS admittime,
  CAST(a.dischtime AS TIMESTAMP) AS dischtime,
  CAST(a.deathtime AS TIMESTAMP) AS deathtime,
  CAST(a.hospital_expire_flag AS INTEGER) AS hospital_expire_flag,
  CAST(a.admission_type AS VARCHAR) AS admission_type,
  CAST(a.admission_location AS VARCHAR) AS admission_location,
  CAST(a.discharge_location AS VARCHAR) AS discharge_location,
  CAST(a.insurance AS VARCHAR) AS insurance,
  CAST(a.language AS VARCHAR) AS language,
  CAST(a.marital_status AS VARCHAR) AS marital_status,
  CAST(a.race AS VARCHAR) AS race,

  CAST(p.gender AS VARCHAR) AS gender,
  CAST(p.anchor_age AS INTEGER) AS anchor_age,
  CAST(p.anchor_year AS INTEGER) AS anchor_year,
  CAST(p.dod AS DATE) AS dod,

  CASE
    WHEN CAST(p.anchor_age AS INTEGER)
         + CAST(date_part('year', CAST(a.admittime AS TIMESTAMP)) AS INTEGER)
         - CAST(p.anchor_year AS INTEGER) > 89
    THEN 91
    ELSE CAST(p.anchor_age AS INTEGER)
         + CAST(date_part('year', CAST(a.admittime AS TIMESTAMP)) AS INTEGER)
         - CAST(p.anchor_year AS INTEGER)
  END AS age,

  date_diff('hour', CAST(a.admittime AS TIMESTAMP), CAST(a.dischtime AS TIMESTAMP)) / 24.0
    AS hospital_los_days,

  CASE
    WHEN CAST(p.dod AS DATE) IS NOT NULL
         AND date_diff('day', CAST(a.admittime AS TIMESTAMP), CAST(p.dod AS TIMESTAMP)) BETWEEN 0 AND 30
    THEN 1 ELSE 0
  END AS mortality_30d_from_admission,

  CASE
    WHEN CAST(p.dod AS DATE) IS NOT NULL
         AND date_diff('day', CAST(a.admittime AS TIMESTAMP), CAST(p.dod AS TIMESTAMP)) BETWEEN 0 AND 365
    THEN 1 ELSE 0
  END AS mortality_1y_from_admission

FROM ami_selected_dx s
INNER JOIN admissions a
  ON s.subject_id = CAST(a.subject_id AS BIGINT)
 AND s.hadm_id = CAST(a.hadm_id AS BIGINT)
INNER JOIN patients p
  ON s.subject_id = CAST(p.subject_id AS BIGINT);
")

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE ami_hosp_adult AS
SELECT *
FROM ami_hosp_all
WHERE age >= 18;
")

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE ami_index_hosp AS
SELECT *
FROM (
  SELECT
    *,
    ROW_NUMBER() OVER (
      PARTITION BY subject_id
      ORDER BY admittime, hadm_id
    ) AS rn_index_hosp
  FROM ami_hosp_adult
)
WHERE rn_index_hosp = 1;
")

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE ami_icu_all AS
SELECT
  h.*,
  CAST(i.stay_id AS BIGINT) AS stay_id,
  CAST(i.first_careunit AS VARCHAR) AS first_careunit,
  CAST(i.last_careunit AS VARCHAR) AS last_careunit,
  CAST(i.intime AS TIMESTAMP) AS intime,
  CAST(i.outtime AS TIMESTAMP) AS outtime,
  CAST(i.los AS DOUBLE) AS icu_los_days,
  date_diff('hour', CAST(i.intime AS TIMESTAMP), CAST(i.outtime AS TIMESTAMP)) AS icu_los_hours,

  CASE
    WHEN h.deathtime IS NOT NULL
         AND h.deathtime >= CAST(i.intime AS TIMESTAMP)
         AND h.deathtime <= CAST(i.outtime AS TIMESTAMP)
    THEN 1 ELSE 0
  END AS icu_expire_flag,

  CASE
    WHEN h.dod IS NOT NULL
         AND date_diff('day', CAST(i.intime AS TIMESTAMP), CAST(h.dod AS TIMESTAMP)) BETWEEN 0 AND 30
    THEN 1 ELSE 0
  END AS mortality_30d_from_icu,

  CASE
    WHEN h.dod IS NOT NULL
         AND date_diff('day', CAST(i.intime AS TIMESTAMP), CAST(h.dod AS TIMESTAMP)) BETWEEN 0 AND 365
    THEN 1 ELSE 0
  END AS mortality_1y_from_icu,

  CASE
    WHEN h.deathtime IS NOT NULL
         AND h.deathtime >= CAST(i.intime AS TIMESTAMP)
         AND h.deathtime < CAST(i.intime AS TIMESTAMP) + INTERVAL '24 hours'
    THEN 1 ELSE 0
  END AS death_within_24h_after_icu,

  CASE
    WHEN h.deathtime IS NOT NULL
         AND h.deathtime >= CAST(i.intime AS TIMESTAMP)
         AND h.deathtime < CAST(i.intime AS TIMESTAMP) + INTERVAL '48 hours'
    THEN 1 ELSE 0
  END AS death_within_48h_after_icu,

  CASE
    WHEN h.deathtime IS NOT NULL
         AND h.deathtime >= CAST(i.intime AS TIMESTAMP)
         AND h.deathtime < CAST(i.intime AS TIMESTAMP) + INTERVAL '72 hours'
    THEN 1 ELSE 0
  END AS death_within_72h_after_icu,

  CASE
    WHEN CAST(i.outtime AS TIMESTAMP) < CAST(i.intime AS TIMESTAMP) + INTERVAL '24 hours'
    THEN 1 ELSE 0
  END AS icu_discharge_before_24h,

  CASE
    WHEN CAST(i.outtime AS TIMESTAMP) < CAST(i.intime AS TIMESTAMP) + INTERVAL '48 hours'
    THEN 1 ELSE 0
  END AS icu_discharge_before_48h,

  CASE
    WHEN CAST(i.outtime AS TIMESTAMP) < CAST(i.intime AS TIMESTAMP) + INTERVAL '72 hours'
    THEN 1 ELSE 0
  END AS icu_discharge_before_72h

FROM ami_hosp_adult h
INNER JOIN icustays i
  ON h.subject_id = CAST(i.subject_id AS BIGINT)
 AND h.hadm_id = CAST(i.hadm_id AS BIGINT);
")

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE ami_index_icu AS
SELECT *
FROM (
  SELECT
    *,
    ROW_NUMBER() OVER (
      PARTITION BY subject_id
      ORDER BY intime, stay_id
    ) AS rn_index_icu
  FROM ami_icu_all
)
WHERE rn_index_icu = 1;
")

cat("AMI hospital and ICU cohorts created.\n\n")

## =========================================================
## 10. Comorbidity flags
## =========================================================

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE comorbidity_flags AS
SELECT
  subject_id,
  hadm_id,

  MAX(CASE
    WHEN icd_version = 9 AND icd_code_clean LIKE '250%' THEN 1
    WHEN icd_version = 10 AND (
      icd_code_clean LIKE 'E10%' OR icd_code_clean LIKE 'E11%' OR
      icd_code_clean LIKE 'E12%' OR icd_code_clean LIKE 'E13%' OR
      icd_code_clean LIKE 'E14%'
    ) THEN 1 ELSE 0 END) AS diabetes,

  MAX(CASE
    WHEN icd_version = 9 AND (
      icd_code_clean LIKE '401%' OR icd_code_clean LIKE '402%' OR
      icd_code_clean LIKE '403%' OR icd_code_clean LIKE '404%' OR
      icd_code_clean LIKE '405%'
    ) THEN 1
    WHEN icd_version = 10 AND (
      icd_code_clean LIKE 'I10%' OR icd_code_clean LIKE 'I11%' OR
      icd_code_clean LIKE 'I12%' OR icd_code_clean LIKE 'I13%' OR
      icd_code_clean LIKE 'I15%'
    ) THEN 1 ELSE 0 END) AS hypertension,

  MAX(CASE
    WHEN icd_version = 9 AND (icd_code_clean LIKE '585%' OR icd_code_clean LIKE '586%') THEN 1
    WHEN icd_version = 10 AND (icd_code_clean LIKE 'N18%' OR icd_code_clean LIKE 'N19%') THEN 1
    ELSE 0 END) AS ckd,

  MAX(CASE
    WHEN icd_version = 9 AND (
      icd_code_clean LIKE '490%' OR icd_code_clean LIKE '491%' OR
      icd_code_clean LIKE '492%' OR icd_code_clean LIKE '4932%' OR
      icd_code_clean LIKE '494%' OR icd_code_clean LIKE '496%'
    ) THEN 1
    WHEN icd_version = 10 AND (
      icd_code_clean LIKE 'J40%' OR icd_code_clean LIKE 'J41%' OR
      icd_code_clean LIKE 'J42%' OR icd_code_clean LIKE 'J43%' OR
      icd_code_clean LIKE 'J44%'
    ) THEN 1 ELSE 0 END) AS copd,

  MAX(CASE
    WHEN icd_version = 9 AND (
      icd_code_clean LIKE '410%' OR icd_code_clean LIKE '411%' OR
      icd_code_clean LIKE '412%' OR icd_code_clean LIKE '413%' OR
      icd_code_clean LIKE '414%'
    ) THEN 1
    WHEN icd_version = 10 AND (
      icd_code_clean LIKE 'I20%' OR icd_code_clean LIKE 'I21%' OR
      icd_code_clean LIKE 'I22%' OR
      icd_code_clean LIKE 'I23%' OR icd_code_clean LIKE 'I24%' OR
      icd_code_clean LIKE 'I25%'
    ) THEN 1 ELSE 0 END) AS cad_or_ihd,

  MAX(CASE
    WHEN icd_version = 9 AND icd_code_clean = '42731' THEN 1
    WHEN icd_version = 10 AND icd_code_clean LIKE 'I48%' THEN 1
    ELSE 0 END) AS atrial_fibrillation,

  MAX(CASE
    WHEN icd_version = 9 AND (
      icd_code_clean LIKE '430%' OR icd_code_clean LIKE '431%' OR
      icd_code_clean LIKE '432%' OR icd_code_clean LIKE '433%' OR
      icd_code_clean LIKE '434%' OR icd_code_clean LIKE '435%' OR
      icd_code_clean LIKE '436%' OR icd_code_clean LIKE '437%' OR
      icd_code_clean LIKE '438%'
    ) THEN 1
    WHEN icd_version = 10 AND (
      icd_code_clean LIKE 'I60%' OR icd_code_clean LIKE 'I61%' OR
      icd_code_clean LIKE 'I62%' OR icd_code_clean LIKE 'I63%' OR
      icd_code_clean LIKE 'I64%' OR icd_code_clean LIKE 'I65%' OR
      icd_code_clean LIKE 'I66%' OR icd_code_clean LIKE 'I67%' OR
      icd_code_clean LIKE 'I68%' OR icd_code_clean LIKE 'I69%'
    ) THEN 1 ELSE 0 END) AS stroke,

  MAX(CASE
    WHEN icd_version = 9 AND (
      icd_code_clean LIKE '570%' OR icd_code_clean LIKE '571%' OR
      icd_code_clean LIKE '572%' OR icd_code_clean LIKE '573%'
    ) THEN 1
    WHEN icd_version = 10 AND (
      icd_code_clean LIKE 'K70%' OR icd_code_clean LIKE 'K71%' OR
      icd_code_clean LIKE 'K72%' OR icd_code_clean LIKE 'K73%' OR
      icd_code_clean LIKE 'K74%' OR icd_code_clean LIKE 'K75%' OR
      icd_code_clean LIKE 'K76%' OR icd_code_clean LIKE 'K77%'
    ) THEN 1 ELSE 0 END) AS liver_disease,

  MAX(CASE
    WHEN icd_version = 9
         AND TRY_CAST(substr(icd_code_clean, 1, 3) AS INTEGER) BETWEEN 140 AND 209
      THEN 1
    WHEN icd_version = 10 AND icd_code_clean LIKE 'C%'
      THEN 1
    ELSE 0 END) AS malignancy,

  MAX(CASE
    WHEN icd_version = 9 AND icd_code_clean IN ('99591','99592','78552') THEN 1
    WHEN icd_version = 10 AND (
      icd_code_clean LIKE 'A40%' OR icd_code_clean LIKE 'A41%' OR icd_code_clean LIKE 'R652%'
    ) THEN 1 ELSE 0 END) AS sepsis

FROM dx_all_clean
GROUP BY subject_id, hadm_id;
")

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE ami_index_icu_base AS
SELECT
  c.*,
  COALESCE(cm.diabetes, 0) AS diabetes,
  COALESCE(cm.hypertension, 0) AS hypertension,
  COALESCE(cm.ckd, 0) AS ckd,
  COALESCE(cm.copd, 0) AS copd,
  COALESCE(cm.cad_or_ihd, 0) AS cad_or_ihd,
  COALESCE(cm.atrial_fibrillation, 0) AS atrial_fibrillation,
  COALESCE(cm.stroke, 0) AS stroke,
  COALESCE(cm.liver_disease, 0) AS liver_disease,
  COALESCE(cm.malignancy, 0) AS malignancy,
  COALESCE(cm.sepsis, 0) AS sepsis
FROM ami_index_icu c
LEFT JOIN comorbidity_flags cm
  ON c.subject_id = cm.subject_id
 AND c.hadm_id = cm.hadm_id;
")

cat("Comorbidity flags created.\n\n")

## =========================================================
## 11. Landmark eligibility counts
## =========================================================
## T0: all index ICU patients
## T24/T48/T72:
##   alive at landmark
##   alive and still in ICU at landmark
##
## Both are retained because later dynamic models may use either:
##   1. alive landmark set
##   2. alive-and-in-ICU landmark set
############################################################

landmark_counts <- DBI::dbGetQuery(con, "
SELECT
  'T0' AS landmark,
  0 AS landmark_hour,
  COUNT(*) AS n_index_icu_patients,
  COUNT(*) AS n_alive_at_landmark,
  COUNT(*) AS n_alive_and_in_icu_at_landmark
FROM ami_index_icu_base

UNION ALL

SELECT
  'T24' AS landmark,
  24 AS landmark_hour,
  COUNT(*) AS n_index_icu_patients,
  SUM(CASE
    WHEN deathtime IS NULL
         OR deathtime >= intime + INTERVAL '24 hours'
    THEN 1 ELSE 0 END) AS n_alive_at_landmark,
  SUM(CASE
    WHEN (deathtime IS NULL OR deathtime >= intime + INTERVAL '24 hours')
         AND outtime >= intime + INTERVAL '24 hours'
    THEN 1 ELSE 0 END) AS n_alive_and_in_icu_at_landmark
FROM ami_index_icu_base

UNION ALL

SELECT
  'T48' AS landmark,
  48 AS landmark_hour,
  COUNT(*) AS n_index_icu_patients,
  SUM(CASE
    WHEN deathtime IS NULL
         OR deathtime >= intime + INTERVAL '48 hours'
    THEN 1 ELSE 0 END) AS n_alive_at_landmark,
  SUM(CASE
    WHEN (deathtime IS NULL OR deathtime >= intime + INTERVAL '48 hours')
         AND outtime >= intime + INTERVAL '48 hours'
    THEN 1 ELSE 0 END) AS n_alive_and_in_icu_at_landmark
FROM ami_index_icu_base

UNION ALL

SELECT
  'T72' AS landmark,
  72 AS landmark_hour,
  COUNT(*) AS n_index_icu_patients,
  SUM(CASE
    WHEN deathtime IS NULL
         OR deathtime >= intime + INTERVAL '72 hours'
    THEN 1 ELSE 0 END) AS n_alive_at_landmark,
  SUM(CASE
    WHEN (deathtime IS NULL OR deathtime >= intime + INTERVAL '72 hours')
         AND outtime >= intime + INTERVAL '72 hours'
    THEN 1 ELSE 0 END) AS n_alive_and_in_icu_at_landmark
FROM ami_index_icu_base

ORDER BY landmark_hour;
")

## =========================================================
## 12. Flowchart counts
## =========================================================

flowchart_counts <- DBI::dbGetQuery(con, "
SELECT
  1 AS step_order,
  'All hospital admissions in MIMIC-IV' AS step,
  COUNT(DISTINCT hadm_id) AS n_admissions,
  COUNT(DISTINCT subject_id) AS n_patients,
  NULL::BIGINT AS n_icu_stays
FROM admissions

UNION ALL

SELECT
  2 AS step_order,
  'Candidate broad AMI admissions: ICD-9 410* or ICD-10 I21*/I22*, any diagnosis position' AS step,
  COUNT(DISTINCT hadm_id) AS n_admissions,
  COUNT(DISTINCT subject_id) AS n_patients,
  NULL::BIGINT AS n_icu_stays
FROM (
  SELECT DISTINCT subject_id, hadm_id
  FROM ami_dx_candidates
  WHERE ami_broad_flag = 1
)

UNION ALL

SELECT
  3 AS step_order,
  'Conservative AMI admissions: excluding ICD-9 410x2, ICD-10 I21A1/I21A9, any diagnosis position' AS step,
  COUNT(DISTINCT hadm_id) AS n_admissions,
  COUNT(DISTINCT subject_id) AS n_patients,
  NULL::BIGINT AS n_icu_stays
FROM (
  SELECT DISTINCT subject_id, hadm_id
  FROM ami_dx_candidates
  WHERE ami_conservative_flag = 1
)

UNION ALL

SELECT
  4 AS step_order,
  'Main AMI admissions: conservative AMI in the first three diagnosis positions' AS step,
  COUNT(DISTINCT hadm_id) AS n_admissions,
  COUNT(DISTINCT subject_id) AS n_patients,
  NULL::BIGINT AS n_icu_stays
FROM ami_selected_dx

UNION ALL

SELECT
  5 AS step_order,
  'Adult main AMI hospitalizations, age >=18 years' AS step,
  COUNT(DISTINCT hadm_id) AS n_admissions,
  COUNT(DISTINCT subject_id) AS n_patients,
  NULL::BIGINT AS n_icu_stays
FROM ami_hosp_adult

UNION ALL

SELECT
  6 AS step_order,
  'First main AMI hospitalization per patient' AS step,
  COUNT(DISTINCT hadm_id) AS n_admissions,
  COUNT(DISTINCT subject_id) AS n_patients,
  NULL::BIGINT AS n_icu_stays
FROM ami_index_hosp

UNION ALL

SELECT
  7 AS step_order,
  'Main AMI hospitalizations with at least one ICU stay' AS step,
  COUNT(DISTINCT hadm_id) AS n_admissions,
  COUNT(DISTINCT subject_id) AS n_patients,
  COUNT(DISTINCT stay_id) AS n_icu_stays
FROM ami_icu_all

UNION ALL

SELECT
  8 AS step_order,
  'First main AMI ICU stay per patient' AS step,
  COUNT(DISTINCT hadm_id) AS n_admissions,
  COUNT(DISTINCT subject_id) AS n_patients,
  COUNT(DISTINCT stay_id) AS n_icu_stays
FROM ami_index_icu_base

ORDER BY step_order;
")

print(flowchart_counts)

## =========================================================
## 13. Outcomes and baseline descriptive tables
## =========================================================

outcome_summary <- DBI::dbGetQuery(con, "
SELECT
  COUNT(*) AS n,
  SUM(icu_expire_flag) AS icu_deaths,
  100.0 * SUM(icu_expire_flag) / COUNT(*) AS icu_mortality_pct,
  SUM(hospital_expire_flag) AS hospital_deaths,
  100.0 * SUM(hospital_expire_flag) / COUNT(*) AS hospital_mortality_pct,
  SUM(mortality_30d_from_icu) AS deaths_30d_from_icu,
  100.0 * SUM(mortality_30d_from_icu) / COUNT(*) AS mortality_30d_from_icu_pct,
  SUM(mortality_1y_from_icu) AS deaths_1y_from_icu,
  100.0 * SUM(mortality_1y_from_icu) / COUNT(*) AS mortality_1y_from_icu_pct,
  SUM(death_within_24h_after_icu) AS deaths_within_24h_after_icu,
  100.0 * SUM(death_within_24h_after_icu) / COUNT(*) AS death_within_24h_pct,
  SUM(death_within_48h_after_icu) AS deaths_within_48h_after_icu,
  100.0 * SUM(death_within_48h_after_icu) / COUNT(*) AS death_within_48h_pct,
  SUM(death_within_72h_after_icu) AS deaths_within_72h_after_icu,
  100.0 * SUM(death_within_72h_after_icu) / COUNT(*) AS death_within_72h_pct,
  SUM(icu_discharge_before_24h) AS icu_discharge_before_24h,
  100.0 * SUM(icu_discharge_before_24h) / COUNT(*) AS icu_discharge_before_24h_pct,
  SUM(icu_discharge_before_48h) AS icu_discharge_before_48h,
  100.0 * SUM(icu_discharge_before_48h) / COUNT(*) AS icu_discharge_before_48h_pct,
  SUM(icu_discharge_before_72h) AS icu_discharge_before_72h,
  100.0 * SUM(icu_discharge_before_72h) / COUNT(*) AS icu_discharge_before_72h_pct,
  AVG(age) AS mean_age,
  MEDIAN(age) AS median_age,
  AVG(icu_los_days) AS mean_icu_los_days,
  MEDIAN(icu_los_days) AS median_icu_los_days,
  AVG(hospital_los_days) AS mean_hospital_los_days,
  MEDIAN(hospital_los_days) AS median_hospital_los_days
FROM ami_index_icu_base;
")

careunit_summary <- DBI::dbGetQuery(con, "
SELECT
  first_careunit,
  COUNT(*) AS n,
  100.0 * COUNT(*) / SUM(COUNT(*)) OVER () AS pct
FROM ami_index_icu_base
GROUP BY first_careunit
ORDER BY n DESC;
")

gender_summary <- DBI::dbGetQuery(con, "
SELECT
  gender,
  COUNT(*) AS n,
  100.0 * COUNT(*) / SUM(COUNT(*)) OVER () AS pct
FROM ami_index_icu_base
GROUP BY gender
ORDER BY n DESC;
")

race_summary <- DBI::dbGetQuery(con, "
SELECT
  race,
  COUNT(*) AS n,
  100.0 * COUNT(*) / SUM(COUNT(*)) OVER () AS pct
FROM ami_index_icu_base
GROUP BY race
ORDER BY n DESC;
")

subtype_summary <- DBI::dbGetQuery(con, "
SELECT
  ami_subtype_groups,
  COUNT(*) AS n,
  100.0 * COUNT(*) / SUM(COUNT(*)) OVER () AS pct
FROM ami_index_icu_base
GROUP BY ami_subtype_groups
ORDER BY n DESC;
")

comorbidity_summary <- DBI::dbGetQuery(con, "
SELECT 'diabetes' AS comorbidity, SUM(diabetes) AS n, 100.0 * SUM(diabetes) / COUNT(*) AS pct FROM ami_index_icu_base
UNION ALL SELECT 'hypertension', SUM(hypertension), 100.0 * SUM(hypertension) / COUNT(*) FROM ami_index_icu_base
UNION ALL SELECT 'ckd', SUM(ckd), 100.0 * SUM(ckd) / COUNT(*) FROM ami_index_icu_base
UNION ALL SELECT 'copd', SUM(copd), 100.0 * SUM(copd) / COUNT(*) FROM ami_index_icu_base
UNION ALL SELECT 'cad_or_ihd', SUM(cad_or_ihd), 100.0 * SUM(cad_or_ihd) / COUNT(*) FROM ami_index_icu_base
UNION ALL SELECT 'atrial_fibrillation', SUM(atrial_fibrillation), 100.0 * SUM(atrial_fibrillation) / COUNT(*) FROM ami_index_icu_base
UNION ALL SELECT 'stroke', SUM(stroke), 100.0 * SUM(stroke) / COUNT(*) FROM ami_index_icu_base
UNION ALL SELECT 'liver_disease', SUM(liver_disease), 100.0 * SUM(liver_disease) / COUNT(*) FROM ami_index_icu_base
UNION ALL SELECT 'malignancy', SUM(malignancy), 100.0 * SUM(malignancy) / COUNT(*) FROM ami_index_icu_base
UNION ALL SELECT 'sepsis', SUM(sepsis), 100.0 * SUM(sepsis) / COUNT(*) FROM ami_index_icu_base
ORDER BY pct DESC;
")

## =========================================================
## 14. Save base cohort
## =========================================================

ami_index_icu_base <- DBI::dbGetQuery(con, "
SELECT
  subject_id,
  hadm_id,
  stay_id,
  age,
  gender,
  race,
  insurance,
  marital_status,
  admission_type,
  admission_location,
  discharge_location,
  ami_min_seq_num,
  n_ami_dx_rows,
  ami_icd_codes,
  ami_diagnosis_titles,
  ami_subtype_groups,
  admittime,
  dischtime,
  deathtime,
  dod,
  intime,
  outtime,
  first_careunit,
  last_careunit,
  hospital_los_days,
  icu_los_days,
  icu_los_hours,
  hospital_expire_flag,
  icu_expire_flag,
  mortality_30d_from_admission,
  mortality_1y_from_admission,
  mortality_30d_from_icu,
  mortality_1y_from_icu,
  death_within_24h_after_icu,
  death_within_48h_after_icu,
  death_within_72h_after_icu,
  icu_discharge_before_24h,
  icu_discharge_before_48h,
  icu_discharge_before_72h,
  diabetes,
  hypertension,
  ckd,
  copd,
  cad_or_ihd,
  atrial_fibrillation,
  stroke,
  liver_disease,
  malignancy,
  sepsis
FROM ami_index_icu_base
ORDER BY subject_id, hadm_id, stay_id;
")

safe_fwrite(ami_index_icu_base, out_cohort_csv)

## =========================================================
## 15. Precise laboratory and vital-sign item maps
## =========================================================
## These are intentionally conservative and clinically interpretable.
## Do not use broad regex matching here.

lab_itemid_manual <- tibble::tribble(
  ~domain, ~variable, ~itemid, ~preferred_label, ~scientific_module,
  "lab", "wbc", 51300, "WBC Count", "inflammatory",
  "lab", "wbc", 51301, "White Blood Cells", "inflammatory",
  "lab", "hemoglobin", 50811, "Hemoglobin", "oxygen_delivery",
  "lab", "hemoglobin", 51222, "Hemoglobin", "oxygen_delivery",
  "lab", "hematocrit", 51221, "Hematocrit", "oxygen_delivery",
  "lab", "platelet", 51265, "Platelet Count", "coagulation",
  "lab", "platelet", 53189, "Platelet Count", "coagulation",
  "lab", "creatinine", 50912, "Creatinine", "renal_metabolic",
  "lab", "bun", 51006, "Urea Nitrogen", "renal_metabolic",
  "lab", "sodium", 50983, "Sodium", "renal_metabolic",
  "lab", "sodium", 50824, "Sodium, Whole Blood", "renal_metabolic",
  "lab", "potassium", 50971, "Potassium", "renal_metabolic",
  "lab", "potassium", 50822, "Potassium, Whole Blood", "renal_metabolic",
  "lab", "chloride", 50902, "Chloride", "renal_metabolic",
  "lab", "chloride", 50806, "Chloride, Whole Blood", "renal_metabolic",
  "lab", "bicarbonate", 50882, "Bicarbonate", "acid_base",
  "lab", "glucose", 50931, "Glucose", "metabolic_stress",
  "lab", "glucose", 50809, "Glucose, Whole Blood", "metabolic_stress",
  "lab", "lactate", 50813, "Lactate", "hypoperfusion",
  "lab", "albumin", 50862, "Albumin", "nutritional_inflammatory_reserve",
  "lab", "total_bilirubin", 50885, "Bilirubin, Total", "hepatic",
  "lab", "alt", 50861, "Alanine Aminotransferase", "hepatic",
  "lab", "ast", 50878, "Aspartate Aminotransferase", "hepatic",
  "lab", "inr", 51237, "INR", "coagulation",
  "lab", "pt", 51274, "PT", "coagulation",
  "lab", "ptt", 51275, "PTT", "coagulation",
  "lab", "troponin_t", 51003, "Troponin T", "cardiac_injury"
)

vital_itemid_manual <- tibble::tribble(
  ~domain, ~variable, ~itemid, ~preferred_label, ~scientific_module,
  "vital", "heart_rate", 220045, "Heart Rate", "hemodynamic",
  "vital", "sbp", 220179, "Non Invasive Blood Pressure systolic", "hemodynamic",
  "vital", "sbp", 220050, "Arterial Blood Pressure systolic", "hemodynamic",
  "vital", "dbp", 220180, "Non Invasive Blood Pressure diastolic", "hemodynamic",
  "vital", "dbp", 220051, "Arterial Blood Pressure diastolic", "hemodynamic",
  "vital", "map", 220181, "Non Invasive Blood Pressure mean", "hemodynamic",
  "vital", "map", 220052, "Arterial Blood Pressure mean", "hemodynamic",
  "vital", "resp_rate", 220210, "Respiratory Rate", "respiratory_neurologic",
  "vital", "spo2", 220277, "SpO2", "respiratory_neurologic",
  "vital", "temperature", 223761, "Temperature Fahrenheit", "inflammatory_stress",
  "vital", "temperature", 223762, "Temperature Celsius", "inflammatory_stress",
  "vital", "gcs_eye", 220739, "GCS - Eye Opening", "respiratory_neurologic",
  "vital", "gcs_verbal", 223900, "GCS - Verbal Response", "respiratory_neurologic",
  "vital", "gcs_motor", 223901, "GCS - Motor Response", "respiratory_neurologic"
)

DBI::dbWriteTable(con, "lab_itemid_manual", lab_itemid_manual, overwrite = TRUE)
DBI::dbWriteTable(con, "vital_itemid_manual", vital_itemid_manual, overwrite = TRUE)

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE lab_item_map AS
SELECT
  m.domain,
  m.variable,
  CAST(m.itemid AS BIGINT) AS itemid,
  m.preferred_label,
  m.scientific_module,
  CAST(d.label AS VARCHAR) AS mimic_label,
  CAST(d.fluid AS VARCHAR) AS fluid,
  CAST(d.category AS VARCHAR) AS category
FROM lab_itemid_manual m
LEFT JOIN d_labitems d
  ON CAST(m.itemid AS BIGINT) = CAST(d.itemid AS BIGINT)
WHERE d.itemid IS NOT NULL;
")

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE vital_item_map AS
SELECT
  m.domain,
  m.variable,
  CAST(m.itemid AS BIGINT) AS itemid,
  m.preferred_label,
  m.scientific_module,
  CAST(d.label AS VARCHAR) AS mimic_label,
  CAST(d.category AS VARCHAR) AS category
FROM vital_itemid_manual m
LEFT JOIN d_items d
  ON CAST(m.itemid AS BIGINT) = CAST(d.itemid AS BIGINT)
WHERE d.itemid IS NOT NULL;
")

lab_item_map <- DBI::dbGetQuery(con, "
SELECT *
FROM lab_item_map
ORDER BY scientific_module, variable, itemid;
")

vital_item_map <- DBI::dbGetQuery(con, "
SELECT *
FROM vital_item_map
ORDER BY scientific_module, variable, itemid;
")

safe_fwrite(lab_item_map, out_lab_map_csv)
safe_fwrite(vital_item_map, out_vital_map_csv)

## =========================================================
## 16. Extract targeted lab/vital records within 0-72h
## =========================================================

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE lab_target_72h AS
SELECT
  c.subject_id,
  c.hadm_id,
  c.stay_id,
  c.intime,
  m.variable,
  m.scientific_module,
  m.itemid,
  CAST(l.charttime AS TIMESTAMP) AS charttime,
  date_diff('minute', c.intime, CAST(l.charttime AS TIMESTAMP)) / 60.0 AS hours_from_icu,
  TRY_CAST(l.valuenum AS DOUBLE) AS valuenum
FROM ami_index_icu_base c
INNER JOIN labevents l
  ON c.subject_id = CAST(l.subject_id AS BIGINT)
 AND c.hadm_id = CAST(l.hadm_id AS BIGINT)
INNER JOIN lab_item_map m
  ON CAST(l.itemid AS BIGINT) = m.itemid
WHERE
  TRY_CAST(l.valuenum AS DOUBLE) IS NOT NULL
  AND CAST(l.charttime AS TIMESTAMP) >= c.intime
  AND CAST(l.charttime AS TIMESTAMP) < c.intime + INTERVAL '72 hours';
")

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE lab_target_72h_clean AS
SELECT *
FROM lab_target_72h
WHERE
  CASE
    WHEN variable = 'wbc' THEN valuenum > 0 AND valuenum < 500
    WHEN variable = 'hemoglobin' THEN valuenum > 0 AND valuenum < 25
    WHEN variable = 'hematocrit' THEN valuenum > 0 AND valuenum < 80
    WHEN variable = 'platelet' THEN valuenum > 0 AND valuenum < 3000
    WHEN variable = 'creatinine' THEN valuenum >= 0 AND valuenum < 40
    WHEN variable = 'bun' THEN valuenum >= 0 AND valuenum < 400
    WHEN variable = 'sodium' THEN valuenum > 80 AND valuenum < 200
    WHEN variable = 'potassium' THEN valuenum > 1 AND valuenum < 12
    WHEN variable = 'chloride' THEN valuenum > 50 AND valuenum < 160
    WHEN variable = 'bicarbonate' THEN valuenum > 0 AND valuenum < 80
    WHEN variable = 'glucose' THEN valuenum > 0 AND valuenum < 1500
    WHEN variable = 'lactate' THEN valuenum >= 0 AND valuenum < 50
    WHEN variable = 'albumin' THEN valuenum > 0 AND valuenum < 10
    WHEN variable = 'total_bilirubin' THEN valuenum >= 0 AND valuenum < 80
    WHEN variable = 'alt' THEN valuenum >= 0 AND valuenum < 50000
    WHEN variable = 'ast' THEN valuenum >= 0 AND valuenum < 50000
    WHEN variable = 'inr' THEN valuenum > 0 AND valuenum < 30
    WHEN variable = 'pt' THEN valuenum > 0 AND valuenum < 300
    WHEN variable = 'ptt' THEN valuenum > 0 AND valuenum < 300
    WHEN variable = 'troponin_t' THEN valuenum >= 0 AND valuenum < 10000
    ELSE valuenum IS NOT NULL
  END;
")

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE vital_target_72h AS
SELECT
  c.subject_id,
  c.hadm_id,
  c.stay_id,
  c.intime,
  m.variable,
  m.scientific_module,
  m.itemid,
  CAST(ce.charttime AS TIMESTAMP) AS charttime,
  date_diff('minute', c.intime, CAST(ce.charttime AS TIMESTAMP)) / 60.0 AS hours_from_icu,

  CASE
    WHEN m.variable = 'temperature' AND m.itemid = 223761
      THEN (TRY_CAST(ce.valuenum AS DOUBLE) - 32.0) * 5.0 / 9.0
    ELSE TRY_CAST(ce.valuenum AS DOUBLE)
  END AS valuenum

FROM ami_index_icu_base c
INNER JOIN chartevents ce
  ON c.subject_id = CAST(ce.subject_id AS BIGINT)
 AND c.hadm_id = CAST(ce.hadm_id AS BIGINT)
 AND c.stay_id = CAST(ce.stay_id AS BIGINT)
INNER JOIN vital_item_map m
  ON CAST(ce.itemid AS BIGINT) = m.itemid
WHERE
  TRY_CAST(ce.valuenum AS DOUBLE) IS NOT NULL
  AND CAST(ce.charttime AS TIMESTAMP) >= c.intime
  AND CAST(ce.charttime AS TIMESTAMP) < c.intime + INTERVAL '72 hours';
")

DBI::dbExecute(con, "
CREATE OR REPLACE TABLE vital_target_72h_clean AS
SELECT *
FROM vital_target_72h
WHERE
  CASE
    WHEN variable = 'heart_rate' THEN valuenum BETWEEN 20 AND 260
    WHEN variable = 'sbp' THEN valuenum BETWEEN 40 AND 300
    WHEN variable = 'dbp' THEN valuenum BETWEEN 10 AND 200
    WHEN variable = 'map' THEN valuenum BETWEEN 20 AND 250
    WHEN variable = 'resp_rate' THEN valuenum BETWEEN 4 AND 80
    WHEN variable = 'spo2' THEN valuenum BETWEEN 20 AND 100
    WHEN variable = 'temperature' THEN valuenum BETWEEN 25 AND 45
    WHEN variable = 'gcs_eye' THEN valuenum BETWEEN 1 AND 4
    WHEN variable = 'gcs_verbal' THEN valuenum BETWEEN 1 AND 5
    WHEN variable = 'gcs_motor' THEN valuenum BETWEEN 1 AND 6
    ELSE valuenum IS NOT NULL
  END;
")

cat("Targeted lab and vital records extracted within 0-72h.\n\n")

## =========================================================
## 17. Missingness by variable and window
## =========================================================

get_missingness_sql <- function(target_table, map_table, window_hour, total_n) {
  
  sprintf(
    "
    WITH vars AS (
      SELECT DISTINCT
        variable,
        scientific_module
      FROM %s
    ),
    measured AS (
      SELECT
        variable,
        COUNT(DISTINCT stay_id) AS n_with_any_measure,
        COUNT(*) AS n_total_records
      FROM %s
      WHERE hours_from_icu >= 0
        AND hours_from_icu < %d
      GROUP BY variable
    )
    SELECT
      '0-%dh' AS window,
      v.scientific_module,
      v.variable,
      %d AS n_total_icu_patients,
      COALESCE(m.n_with_any_measure, 0) AS n_with_any_measure,
      %d - COALESCE(m.n_with_any_measure, 0) AS n_missing,
      100.0 * COALESCE(m.n_with_any_measure, 0) / %d AS availability_pct,
      100.0 * (%d - COALESCE(m.n_with_any_measure, 0)) / %d AS missing_pct,
      COALESCE(m.n_total_records, 0) AS n_total_records
    FROM vars v
    LEFT JOIN measured m
      ON v.variable = m.variable
    ORDER BY missing_pct ASC, v.scientific_module, v.variable;
    ",
    map_table,
    target_table,
    window_hour,
    window_hour,
    total_n,
    total_n,
    total_n,
    total_n,
    total_n
  )
}

total_icu_n <- nrow(ami_index_icu_base)

lab_missingness <- purrr::map_dfr(
  availability_windows,
  ~ DBI::dbGetQuery(
    con,
    get_missingness_sql(
      target_table = "lab_target_72h_clean",
      map_table = "lab_item_map",
      window_hour = .x,
      total_n = total_icu_n
    )
  )
)

vital_missingness <- purrr::map_dfr(
  availability_windows,
  ~ DBI::dbGetQuery(
    con,
    get_missingness_sql(
      target_table = "vital_target_72h_clean",
      map_table = "vital_item_map",
      window_hour = .x,
      total_n = total_icu_n
    )
  )
)

## =========================================================
## 18. Module-level completeness
## =========================================================

module_missingness <- bind_rows(
  lab_missingness %>% mutate(domain = "lab"),
  vital_missingness %>% mutate(domain = "vital")
) %>%
  group_by(window, domain, scientific_module) %>%
  summarise(
    n_variables = n_distinct(variable),
    mean_availability_pct = mean(availability_pct, na.rm = TRUE),
    median_availability_pct = median(availability_pct, na.rm = TRUE),
    min_availability_pct = min(availability_pct, na.rm = TRUE),
    max_missing_pct = max(missing_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(window, domain, scientific_module)

## =========================================================
## 19. Method note and variable science table
## =========================================================

method_note <- tibble::tribble(
  ~item, ~description,
  "Project", "AMI Dynamic Risk Evolution Project",
  "Database", "MIMIC-IV v3.1",
  "Raw data path", mimic_iv_root,
  "Output path", project_root,
  "Main AMI definition", "Conservative AMI: ICD-9 410* excluding 410x2; ICD-10 I21*/I22* excluding I21A1 and I21A9.",
  "Diagnosis position", "Main analysis requires AMI code in the first three diagnosis positions.",
  "Reason for excluding ICD-9 410x2", "ICD-9 410x2 indicates subsequent episode of care and is less suitable for defining an index acute MI hospitalization.",
  "Reason for excluding ICD-10 I21A1", "I21A1 indicates Type 2 myocardial infarction, which reflects oxygen supply-demand imbalance rather than classic acute coronary thrombosis.",
  "Reason for excluding ICD-10 I21A9", "I21A9 indicates other myocardial infarction type and is treated as ambiguous in the conservative main cohort.",
  "Main cohort", "Adult patients with conservative AMI definition and at least one ICU stay.",
  "Index ICU stay", "First ICU stay among eligible AMI hospitalizations per patient.",
  "Primary future outcome", "30-day mortality after ICU admission.",
  "Future landmark framework", "T0, T24, T48, and T72 landmark models will use only information available before each landmark.",
  "Important note", "This step constructs the cohort and assesses data availability only. No modeling or clustering is performed in Step 01."
)

variable_science <- bind_rows(
  lab_itemid_manual,
  vital_itemid_manual
) %>%
  distinct(domain, variable, scientific_module) %>%
  mutate(
    scientific_rationale = case_when(
      scientific_module == "cardiac_injury" ~ "Direct marker of myocardial injury; used as AMI-specific injury severity signal.",
      scientific_module == "hemodynamic" ~ "Reflects shock, sympathetic activation, and perfusion pressure after AMI.",
      scientific_module == "respiratory_neurologic" ~ "Reflects hypoxemia, respiratory stress, and global neurologic severity.",
      scientific_module == "renal_metabolic" ~ "Reflects renal dysfunction and electrolyte disturbance after low perfusion or systemic stress.",
      scientific_module == "acid_base" ~ "Reflects metabolic acidosis and systemic hypoperfusion.",
      scientific_module == "metabolic_stress" ~ "Reflects stress hyperglycemia and metabolic dysregulation.",
      scientific_module == "hypoperfusion" ~ "Reflects tissue hypoperfusion and shock physiology.",
      scientific_module == "inflammatory" ~ "Reflects systemic inflammatory response.",
      scientific_module == "oxygen_delivery" ~ "Reflects anemia and oxygen carrying capacity.",
      scientific_module == "coagulation" ~ "Reflects coagulation disturbance and systemic severity.",
      scientific_module == "hepatic" ~ "Reflects hepatic injury or congestion in critical illness.",
      scientific_module == "nutritional_inflammatory_reserve" ~ "Reflects baseline reserve and inflammatory/nutritional vulnerability.",
      scientific_module == "inflammatory_stress" ~ "Reflects fever or hypothermia and systemic stress.",
      TRUE ~ "Clinically relevant critical illness marker."
    )
  ) %>%
  arrange(domain, scientific_module, variable)

## =========================================================
## 20. Export Excel workbook
## =========================================================

wb <- openxlsx::createWorkbook()

add_sheet(wb, "00_method_note", method_note)
add_sheet(wb, "01_flowchart_counts", flowchart_counts)
add_sheet(wb, "02_ami_definition_counts", ami_definition_counts)
add_sheet(wb, "03_ami_icd_code_audit", ami_icd_code_audit)
add_sheet(wb, "04_main_dx_audit", main_selected_dx_audit)
add_sheet(wb, "05_landmark_counts", landmark_counts)
add_sheet(wb, "06_outcome_summary", outcome_summary)
add_sheet(wb, "07_lab_missingness", lab_missingness)
add_sheet(wb, "08_vital_missingness", vital_missingness)
add_sheet(wb, "09_module_missingness", module_missingness)
add_sheet(wb, "10_lab_item_map", lab_item_map)
add_sheet(wb, "11_vital_item_map", vital_item_map)
add_sheet(wb, "12_variable_science", variable_science)
add_sheet(wb, "13_comorbidity_summary", comorbidity_summary)
add_sheet(wb, "14_careunit_summary", careunit_summary)
add_sheet(wb, "15_gender_summary", gender_summary)
add_sheet(wb, "16_race_summary", race_summary)
add_sheet(wb, "17_subtype_summary", subtype_summary)

openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

cat("\nExcel workbook saved:\n")
cat(out_xlsx, "\n\n")

## =========================================================
## 21. Print key outputs
## =========================================================

cat("\n============================================================\n")
cat("Step 01 finished successfully.\n")
cat("============================================================\n\n")

cat("Project folder:\n")
cat(project_root, "\n\n")

cat("Main Excel output:\n")
cat(out_xlsx, "\n\n")

cat("Base cohort CSV:\n")
cat(out_cohort_csv, "\n\n")

cat("ICD code audit CSV:\n")
cat(out_dx_audit_csv, "\n\n")

cat("Main selected diagnosis audit CSV:\n")
cat(out_main_dx_audit_csv, "\n\n")

cat("Log file:\n")
cat(log_file, "\n\n")

cat("Flowchart counts:\n")
print(flowchart_counts)

cat("\nAMI definition counts:\n")
print(ami_definition_counts)

cat("\nLandmark counts:\n")
print(landmark_counts)

cat("\nOutcome summary:\n")
print(outcome_summary)

cat("\nTop lab availability at 0-48h:\n")
print(
  lab_missingness %>%
    filter(window == "0-48h") %>%
    arrange(missing_pct) %>%
    select(window, scientific_module, variable, n_total_icu_patients,
           n_with_any_measure, availability_pct, missing_pct) %>%
    head(30)
)

cat("\nTop vital availability at 0-48h:\n")
print(
  vital_missingness %>%
    filter(window == "0-48h") %>%
    arrange(missing_pct) %>%
    select(window, scientific_module, variable, n_total_icu_patients,
           n_with_any_measure, availability_pct, missing_pct) %>%
    head(30)
)

cat("\nSession info:\n")
print(sessionInfo())

DBI::dbDisconnect(con, shutdown = TRUE)

sink()

