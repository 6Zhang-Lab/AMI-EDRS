# Reproducing the analyses

## Input lineage

`upstream/run_order.csv` specifies the extraction order. The MIMIC cohort script produces the index ICU cohort and item maps. `MIMIC_02_landmark_inputs.R` produces the legacy longitudinal cache and landmark tables consumed by the first current analysis stages. For eICU, configuration precedes cohort construction, demographics, vital signs, laboratory records and support proxies.

The upstream code preserves historical definitions so that the original inputs can be reconstructed. The **current** endpoint, eligibility, harmonization and scoring definitions are imposed by `analysis/`, not by historical captions or preliminary scores. Use `analysis/run_order.csv` for the current sequence. Both stage-5 scripts are necessary, in the listed order.

The supplied runner sets paths from one configuration file and isolates legacy environment-clearing commands. It does not reduce bootstrap counts, select a different number of phenotypes, substitute missing variables, change endpoints or retrain only a favorable subset. Path-only changes are recorded in `provenance/source_manifest.csv`.

## Frozen discovery/evaluation split

The held-out phenotype analysis retains the original split file:

`09_step05_T48_dynamic_prediction_model_with_C1C5/analysis_datasets/Step05_train_test_split.csv`

Recorded MD5: `9a412679bac0f7672b7b4b5a98822243`.

This file contains patient-level membership and is not included in the public package. The current stage 19 refuses a missing or changed file. The historical split-generating source is supplied under `provenance/legacy/`, together with its phenotype/clinical-input predecessors, to document the original process. Its stratified split seed is 20260601; the original cohort, row order, R sampling behavior and CSV serialization also matter. Repeating the seed on a differently ordered or corrected cohort is not equivalent.

In a reconstruction from raw data, obtain the original membership through an appropriate credentialed route or reproduce and verify the original file against its checksum. Do not disable the checksum or invent a replacement split to make a run complete. Stage 19 discovers phenotypes within the retained discovery group and evaluates frozen assignment in the evaluation group; the historical five-class phenotype is not the current prediction model or current held-out phenotype result.

## What is and is not demonstrated by this source release

- The current source covers cohort construction/correction, feature extraction and scoring, fold-isolated preprocessing and tuning, frozen external inference, recalibration, paired resampling, decision curves and model export.
- Current application logic is checked on an independent artificial software fixture. It returns no risk on model absence/corruption or invalid input and clears stale results.
- The final production predictor is byte-identical to the successful export used for the existing app. The two actual model files are not present in the feedback package and are not part of this public source archive.
- The entire raw-data-to-manuscript pipeline was not rerun in a clean environment while packaging. The restricted data, historical membership and original local caches were not available here. Software checks must not be described as a new validation of model performance.
- Aggregate analytical figures and tables are produced by the stage scripts. Final panel assembly, journal typography and manuscript prose are editorial steps; this repository does not claim to reproduce every manually arranged page pixel-for-pixel.

## Outputs

Each current stage writes under the configured `OUTPUT_PARENT/StepN`. Read `to_share/StepN_STATUS.txt` before using results. Detailed errors, model objects, patient-level inputs and caches remain under `local_only/`. A stage may catch an R error and write `FAILED`; `scripts/run_analysis.R` additionally checks the completion status and stops the sequence.

Do not move earlier stage outputs between directories after a run: recorded input paths and hashes protect downstream provenance. For a new work location, configure that location before execution.
