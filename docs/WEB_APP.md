# Web application

The current application lives in `app/`. Its explicit `app.R` entry point sources `app_server.R`; there is no competing root `server.R`. The predictor, model manifest and preprocessing contract are verified before use. Calculation failure, model loss, input changes or endpoint changes clear the old result; an invalid model never triggers a replacement heuristic.

The latest author-reported hosted calculator is https://kangnan.shinyapps.io/ami-edrs-research.

## Local startup

From the repository root, install the web dependencies and run:

```sh
Rscript scripts/prepare_web.R /absolute/path/to/completed/Step21
Rscript scripts/launch_web.R
```

The directory argument contains `local_only/webapp`. The preparation command checks the two actual trained bundles against `deployment/expected_model_manifest.csv`; if you refit a different model, this release deliberately does not silently accept it as the recorded model. Use the export produced by that analysis as a new version and verify it separately.

The app offers single-patient entry of prepared summaries, batch CSV import and clearly marked synthetic input examples. Example inputs pass through the same actual fitted model when used in the formal app. The independent artificial-coefficient fixture under `tests/` is used only for software testing.

## Explicit hosting

Hosting is separate from putting source code on GitHub. Connect an account with `rsconnect` locally and review the prepared app before using:

```sh
Rscript scripts/install_dependencies.R deploy
Rscript deployment/deploy.R ACCOUNT SERVER APP_NAME
# To update a known existing deployment, append its APP_ID.
```

No token or account secret belongs in the repository. The deployment helper lists app files explicitly and does not include analysis directories or patient files. Configure `work/webapp/web_config.R` to add the final repository URL and choose allowed input modes. This changes configuration only, not model coefficients.

The interface has no automatic EHR extraction. The input process, two outcomes, intended population, original-model calibration status and research-use scope remain visible. Hosting and redistribution of trained bundles must follow the data/model access conditions in `DATA_ACCESS.md`.
