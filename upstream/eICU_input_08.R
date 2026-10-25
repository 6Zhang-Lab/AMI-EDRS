############################################################
## Step E0: Initialize eICU AMI-EDRS external validation project
## Purpose:
##   1. Create a dedicated eICU external validation project folder
##   2. Record raw eICU file inventory
##   3. Check key required files
##   4. Save path configuration for all subsequent steps
##
## Raw eICU path:
##   <AMI_EICU_RAW>
##
## Output project folder:
##   <AMI_EICU_PROJECT>
############################################################

rm(list = ls())
gc()

options(stringsAsFactors = FALSE)

## =========================================================
## 0. Packages
## =========================================================

pkg_needed <- c(
  "dplyr", "tibble", "stringr", "purrr",
  "data.table", "readr", "openxlsx"
)

for (p in pkg_needed) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

invisible(lapply(pkg_needed, library, character.only = TRUE))

## =========================================================
## 1. Define paths
## =========================================================

raw_eicu_dir <- Sys.getenv("AMI_EICU_RAW", unset=file.path(getwd(), "data/eicu-crd-2.0"))

project_dir <- Sys.getenv("AMI_EICU_PROJECT", unset=file.path(getwd(), "work/eICU"))

if (!dir.exists(raw_eicu_dir)) {
  stop("Raw eICU folder not found: ", raw_eicu_dir)
}

dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)

## Main subfolders
dirs <- list(
  admin          = file.path(project_dir, "00_admin"),
  raw_inventory  = file.path(project_dir, "01_raw_inventory"),
  cohort         = file.path(project_dir, "02_cohort_construction"),
  features       = file.path(project_dir, "03_feature_extraction"),
  organ_scores   = file.path(project_dir, "04_organ_domain_scores"),
  trajectory     = file.path(project_dir, "05_trajectory_transfer"),
  prediction     = file.path(project_dir, "06_external_prediction"),
  benchmark      = file.path(project_dir, "07_benchmark_scores"),
  figures        = file.path(project_dir, "08_figures"),
  tables         = file.path(project_dir, "09_tables"),
  objects        = file.path(project_dir, "10_objects"),
  logs           = file.path(project_dir, "11_logs")
)

purrr::walk(dirs, ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))

## More detailed figure/table folders
figure_dirs <- c(
  "cohort_flow",
  "variable_availability",
  "trajectory_profiles",
  "prediction_performance",
  "calibration",
  "decision_curve",
  "risk_strata"
)

table_dirs <- c(
  "cohort_tables",
  "feature_tables",
  "trajectory_tables",
  "prediction_tables",
  "benchmark_tables",
  "method_notes"
)

purrr::walk(
  file.path(dirs$figures, figure_dirs),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

purrr::walk(
  file.path(dirs$tables, table_dirs),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

cat("Project folder created:\n", project_dir, "\n\n")

## =========================================================
## 2. Save path configuration
## =========================================================

path_config <- list(
  raw_eicu_dir = raw_eicu_dir,
  project_dir = project_dir,
  dirs = dirs,
  created_at = as.character(Sys.time()),
  project_name = "eICU external validation of AMI-EDRS",
  main_endpoint_plan = "In-hospital mortality after T48 landmark",
  secondary_endpoint_plan = c(
    "ICU mortality after T48",
    "overall hospital mortality",
    "overall ICU mortality",
    "prolonged ICU length of stay",
    "prolonged hospital length of stay"
  )
)

saveRDS(
  path_config,
  file.path(dirs$admin, "StepE0_path_config.rds")
)

writeLines(
  c(
    "eICU AMI-EDRS External Validation Project",
    "==========================================",
    "",
    paste0("Created at: ", Sys.time()),
    paste0("Raw eICU folder: ", raw_eicu_dir),
    paste0("Project folder: ", project_dir),
    "",
    "Planned workflow:",
    "Step E0: Project initialization and raw file inventory.",
    "Step E1: eICU AMI cohort construction.",
    "Step E2: T0-T48 raw feature extraction.",
    "Step E3: Organ-domain score reconstruction.",
    "Step E4: C1-C5 trajectory phenotype transfer.",
    "Step E5: AMI-EDRS external prediction.",
    "Step E6: Benchmark score comparison.",
    "",
    "Primary external validation endpoint:",
    "In-hospital mortality after the T48 landmark.",
    "",
    "Reason:",
    "eICU does not uniformly provide post-discharge 30-day mortality, so hospital mortality after T48 is used as the closest transportability endpoint."
  ),
  con = file.path(dirs$admin, "README_external_validation_project.txt")
)

## =========================================================
## 3. Raw eICU file inventory
## =========================================================

raw_files <- list.files(
  raw_eicu_dir,
  pattern = "\\.csv\\.gz$",
  full.names = TRUE,
  recursive = FALSE
)

if (length(raw_files) == 0) {
  stop("No .csv.gz files found in raw eICU folder.")
}

raw_inventory <- tibble::tibble(
  file_name = basename(raw_files),
  file_path = raw_files,
  file_size_mb = round(file.info(raw_files)$size / 1024^2, 2),
  modified_time = as.character(file.info(raw_files)$mtime)
) %>%
  dplyr::arrange(file_name)

readr::write_csv(
  raw_inventory,
  file.path(dirs$raw_inventory, "StepE0_raw_eICU_file_inventory.csv")
)

## =========================================================
## 4. Check expected files
## =========================================================

expected_files <- c(
  "patient.csv.gz",
  "admissionDx.csv.gz",
  "diagnosis.csv.gz",
  "pastHistory.csv.gz",
  "vitalPeriodic.csv.gz",
  "vitalAperiodic.csv.gz",
  "nurseCharting.csv.gz",
  "lab.csv.gz",
  "customLab.csv.gz",
  "respiratoryCharting.csv.gz",
  "respiratoryCare.csv.gz",
  "infusionDrug.csv.gz",
  "medication.csv.gz",
  "admissionDrug.csv.gz",
  "treatment.csv.gz",
  "intakeOutput.csv.gz",
  "apacheApsVar.csv.gz",
  "apachePredVar.csv.gz",
  "apachePatientResult.csv.gz",
  "hospital.csv.gz"
)

expected_status <- tibble::tibble(
  file_name = expected_files,
  exists = expected_files %in% raw_inventory$file_name
) %>%
  dplyr::left_join(
    raw_inventory %>%
      dplyr::select(file_name, file_path, file_size_mb, modified_time),
    by = "file_name"
  ) %>%
  dplyr::mutate(
    planned_use = dplyr::case_when(
      file_name == "patient.csv.gz" ~ "Patient demographics, ICU/hospital admission and discharge, mortality endpoints.",
      file_name == "admissionDx.csv.gz" ~ "Admission diagnosis; primary AMI cohort identification.",
      file_name == "diagnosis.csv.gz" ~ "ICU diagnosis; supplementary AMI identification and comorbidity extraction.",
      file_name == "pastHistory.csv.gz" ~ "Comorbidities including CAD/IHD, heart failure, CKD, COPD, stroke, diabetes.",
      file_name == "vitalPeriodic.csv.gz" ~ "Periodic vital signs within T0-T48.",
      file_name == "vitalAperiodic.csv.gz" ~ "Aperiodic blood pressure and additional vital signs.",
      file_name == "nurseCharting.csv.gz" ~ "GCS, temperature, oxygen support and nursing-derived measurements.",
      file_name == "lab.csv.gz" ~ "Main laboratory features including CBC, chemistry, coagulation, lactate, troponin.",
      file_name == "customLab.csv.gz" ~ "Supplementary laboratory features.",
      file_name == "respiratoryCharting.csv.gz" ~ "Respiratory support, oxygenation-related variables.",
      file_name == "respiratoryCare.csv.gz" ~ "Ventilation and respiratory care variables.",
      file_name == "infusionDrug.csv.gz" ~ "Vasopressors and vasoactive infusions.",
      file_name == "medication.csv.gz" ~ "Medication exposure; supplementary vasoactive drug identification.",
      file_name == "admissionDrug.csv.gz" ~ "Admission medications; supplementary medication context.",
      file_name == "treatment.csv.gz" ~ "Procedures/treatments including ventilation, dialysis, PCI/IABP if available.",
      file_name == "intakeOutput.csv.gz" ~ "Urine output and fluid balance.",
      file_name == "apacheApsVar.csv.gz" ~ "APACHE acute physiology variables.",
      file_name == "apachePredVar.csv.gz" ~ "APACHE prediction variables.",
      file_name == "apachePatientResult.csv.gz" ~ "APACHE predicted mortality and benchmark outputs.",
      file_name == "hospital.csv.gz" ~ "Hospital-level metadata for multicenter description.",
      TRUE ~ NA_character_
    )
  )

readr::write_csv(
  expected_status,
  file.path(dirs$raw_inventory, "StepE0_expected_file_status.csv")
)

## =========================================================
## 5. Extract column names from key files without loading full data
## =========================================================

read_header_only <- function(path) {
  tryCatch(
    {
      dt <- data.table::fread(path, nrows = 0, data.table = FALSE)
      tibble::tibble(
        column_name = names(dt),
        column_index = seq_along(names(dt))
      )
    },
    error = function(e) {
      tibble::tibble(
        column_name = paste0("ERROR: ", conditionMessage(e)),
        column_index = NA_integer_
      )
    }
  )
}

core_files_for_columns <- expected_status %>%
  dplyr::filter(exists) %>%
  dplyr::pull(file_path)

column_dictionary <- purrr::map_dfr(
  core_files_for_columns,
  function(f) {
    read_header_only(f) %>%
      dplyr::mutate(
        file_name = basename(f),
        file_path = f,
        .before = 1
      )
  }
)

readr::write_csv(
  column_dictionary,
  file.path(dirs$raw_inventory, "StepE0_core_file_column_dictionary.csv")
)

## =========================================================
## 6. Create method plan table
## =========================================================

method_plan <- tibble::tribble(
  ~step, ~folder, ~main_task, ~main_outputs,
  "Step E0",
  "00_admin / 01_raw_inventory",
  "Initialize project folder and record raw eICU file inventory.",
  "Path config RDS; raw file inventory; expected file status; column dictionary.",
  
  "Step E1",
  "02_cohort_construction",
  "Construct conservative eICU AMI cohort using admission and ICU diagnosis records.",
  "AMI cohort CSV; diagnosis keyword table; cohort flow table.",
  
  "Step E2",
  "03_feature_extraction",
  "Extract T0-T48 raw physiologic, laboratory, treatment, respiratory, medication, and comorbidity features.",
  "Raw feature long files; variable availability tables; feature dictionaries.",
  
  "Step E3",
  "04_organ_domain_scores",
  "Reconstruct organ-domain scores at T0, T24, and T48 using the MIMIC-compatible scoring framework.",
  "Organ-domain score matrix; dynamic feature matrix; missingness reports.",
  
  "Step E4",
  "05_trajectory_transfer",
  "Assign eICU patients to MIMIC-derived C1-C5 trajectory phenotypes without refitting the clustering model.",
  "Transferred C1-C5 phenotype labels; phenotype profile tables; outcome gradient tables.",
  
  "Step E5",
  "06_external_prediction",
  "Apply the trained AMI-EDRS model to eICU patients without model retraining.",
  "Predicted risk; AMI-EDRS risk strata; AUROC/AUPRC/Brier/calibration/DCA outputs.",
  
  "Step E6",
  "07_benchmark_scores",
  "Compare AMI-EDRS with eICU APACHE outputs and reconstructed traditional severity scores.",
  "Benchmark prediction tables; performance comparison figures and tables."
)

readr::write_csv(
  method_plan,
  file.path(dirs$admin, "StepE0_external_validation_method_plan.csv")
)

## =========================================================
## 7. Export one Excel workbook for Step E0
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

add_sheet(wb, "raw_file_inventory", raw_inventory)
add_sheet(wb, "expected_file_status", expected_status)
add_sheet(wb, "column_dictionary", column_dictionary)
add_sheet(wb, "method_plan", method_plan)

openxlsx::saveWorkbook(
  wb,
  file.path(dirs$raw_inventory, "StepE0_eICU_raw_inventory_and_plan.xlsx"),
  overwrite = TRUE
)

## =========================================================
## 8. Save session information
## =========================================================

sink(file.path(dirs$logs, "StepE0_sessionInfo.txt"))
cat("Step E0 session information\n")
cat("===========================\n\n")
print(sessionInfo())
sink()

## =========================================================
## 9. Console summary
## =========================================================

cat("\n================ Step E0 finished ================\n")
cat("Project folder:\n", project_dir, "\n\n")

cat("Created folders:\n")
print(unlist(dirs))

cat("\nRaw eICU files found:", nrow(raw_inventory), "\n")
cat("Expected key files present:", sum(expected_status$exists), "/", nrow(expected_status), "\n\n")

cat("Main outputs:\n")
cat(file.path(dirs$admin, "StepE0_path_config.rds"), "\n")
cat(file.path(dirs$admin, "README_external_validation_project.txt"), "\n")
cat(file.path(dirs$admin, "StepE0_external_validation_method_plan.csv"), "\n")
cat(file.path(dirs$raw_inventory, "StepE0_raw_eICU_file_inventory.csv"), "\n")
cat(file.path(dirs$raw_inventory, "StepE0_expected_file_status.csv"), "\n")
cat(file.path(dirs$raw_inventory, "StepE0_core_file_column_dictionary.csv"), "\n")
cat(file.path(dirs$raw_inventory, "StepE0_eICU_raw_inventory_and_plan.xlsx"), "\n")
cat(file.path(dirs$logs, "StepE0_sessionInfo.txt"), "\n")
cat("==================================================\n")
