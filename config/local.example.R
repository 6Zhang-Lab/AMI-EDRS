# Paths to credentialed local data and work directories. Never commit local.R.
# Use absolute paths. The examples are deliberately outside the code repository.
ami_config <- list(
  MIMIC_RAW = '/path/to/mimic-iv-3.1',
  EICU_RAW = '/path/to/eicu-crd-2.0',
  MIMIC_PROJECT = '/path/to/work/MIMIC_AMI_DynamicRisk_Project',
  EICU_PROJECT = '/path/to/work/eICU_AMI_EDRS_ExternalValidation',
  OUTPUT_PARENT = '/path/to/work/AMI_EDRS_results',
  # Needed for the exact held-out phenotype analysis in stage 19.
  LEGACY_SPLIT_FILE = '/path/to/work/MIMIC_AMI_DynamicRisk_Project/09_step05_T48_dynamic_prediction_model_with_C1C5/analysis_datasets/Step05_train_test_split.csv',
  step_options = list(
    # Explicit file overrides avoid ambiguity when duplicate raw files exist.
    # '15' = list(RAW_FILES=list('eICU.carePlanGeneral'='/path/to/carePlanGeneral.csv.gz'))
  )
)
