# Installation and environments

## Requirements

- R 4.3.2 is the recorded analysis version. The portable software test results and their actual runtime are recorded in `provenance/software_checks/`.
- macOS and Linux are suitable targets; the complete analysis was run by the authors on macOS ARM64. Windows has not been validated for this release.
- No GPU is required. Full raw-data extraction needs storage for the databases and local caches. It is much more demanding than the three-record software example; a full-cohort clean-run runtime has not been measured for this package.
- Install system build tools if CRAN has no binary for a dependency. For publication plots, install Arial locally or explicitly configure an available PDF font. Font files are not redistributed.

Run all commands from the repository root:

```sh
Rscript scripts/install_dependencies.R web
Rscript scripts/install_dependencies.R analysis
# Only for explicit hosting:
Rscript scripts/install_dependencies.R deploy
# Only to inspect or rerun historical sources:
Rscript scripts/install_dependencies.R legacy
```

`dependencies.csv` distinguishes these profiles. The web profile includes Shiny, testthat, bslib, jsonlite, htmltools, httpuv and zip. Matrix is listed in the analysis profile. The current web application accepts CSV, not Excel; readxl is required only by historical source and is listed under `legacy`.

Installation time depends on existing packages, network and compilation. A fresh full installation was not timed here. The local software-test time is reported separately; it is not the duration of model development or validation.

## Historical and future environments

The supplied sessionInfo files record versions observed during the completed study analyses, including R, glmnet, Matrix, duckdb and Shiny. Empty `recorded_version` cells mean the supplied logs did not record that package version; they are not guessed.

For exact reruns, use the recorded R and package versions, the same raw database release, original input ordering, and the recorded split. For a new installation, record its environment after verification. You can use `renv::snapshot()` in your own working copy, but a newly generated lockfile describes that new environment rather than proving equivalence to the historical one.

The GitHub workflow performs software checks with synthetic inputs. It does not access credentialed databases or rerun the manuscript analyses.
