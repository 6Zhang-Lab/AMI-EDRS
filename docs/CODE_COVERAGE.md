# Analysis and software coverage

| Requirement | Implementation |
|---|---|
| Cohort construction and eligibility | `upstream/`, current stages 1–2 |
| Window extraction, units and feature definitions | Current stages 3–6; `app/feature_dictionary.csv` |
| Fold-isolated references, imputation, scaling, clustering and model tuning | Current stage 7 |
| Baseline and observation-process comparisons | Stages 8–10 |
| Frozen external evaluation of the matched hospital endpoint | Stages 12–13 |
| External calibration/update separation | Stage 14 |
| Care-limitation and benchmark sensitivity analyses | Stages 15–18 |
| Held-out categorical phenotype associations | Stage 19; original split checksum required |
| Paired performance uncertainty and decision curves | Stages 7–9, 11–14, 16, 18 and 20 |
| Parameter-only export and offline/UI consistency | Stage 21; current `app/` |
| No unofficial fallback, no stale result | `app/predictor.R`, `app/app_server.R`; automated tests |
| Dependency and environment disclosure | `dependencies.csv`, recorded sessionInfo files, installation guide |
| Reproducible no-data software demonstration | `scripts/run_demo.R`, `scripts/run_tests.R` |
| Permanent reference to the released source | Version, citation metadata and release guide; public release/DOI still require publication |

This map identifies code coverage. It does not equate inclusion of code or passing software tests with successful independent reproduction of every cohort-level result.
