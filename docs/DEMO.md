# Deterministic software example

Run `Rscript scripts/run_demo.R` from the repository root. The example constructs three artificial rows and two artificial logistic models in a temporary directory, calls the production scoring/inference code, compares the results with analytically calculated probabilities, and removes the temporary model files.

Expected values (rounded here; the script compares unrounded values with tolerance 1e-12):

| Record | Artificial 30-day-output model | Artificial hospital-output model |
|---|---:|---:|
| 1 | 0.1978161 | 0.1301085 |
| 2 | 0.3543437 | 0.2497399 |
| 3 | 0.1978161 | 0.1301085 |

These are not estimates from the trained AMI-EDRS model and have no clinical interpretation. The repeated values deliberately exercise empirical-score ties. The app never selects these models when a formal model fails.

Outputs go to ignored `work/demo/`. `scripts/run_tests.R` adds 34 application checks, source parsing, a known-probability check and exact CSV tie preservation, for 37 software checks in total. Outputs go to `work/tests/`.

During packaging, the example completed in 0.29 seconds and the 37 checks in 7.43 seconds on the supplied Linux x86_64 environment with R 4.3.3 and dependencies already installed. These are observed software-test times, not general performance guarantees or timings of the full analyses. The author's recorded cohort analyses used R 4.3.2 on macOS ARM64.
