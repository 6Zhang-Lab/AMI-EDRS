# Data and model access

The source code and artificial examples are public-release material. This package contains no patient-level tables, patient identifiers, real patient predictions, raw database extracts, cohort caches or trained clinical model RDS files.

The analyses use:

- [MIMIC-IV 3.1](https://physionet.org/content/mimiciv/3.1/), dataset DOI [10.13026/kpb9-mt58](https://doi.org/10.13026/kpb9-mt58).
- [eICU-CRD 2.0](https://physionet.org/content/eicu-crd/2.0/), dataset DOI [10.13026/C2WM1R](https://doi.org/10.13026/C2WM1R).

Access is managed by PhysioNet. Obtain the required credentialed access and use the original data-use agreements. The MIMIC project guidance specifically covers derived datasets and models and directs their sharing through PhysioNet under the source agreement. The trained bundles contain empirical scoring reference distributions as well as coefficients and preprocessing parameters. Removing patient identifiers does not turn those bundles into unrestricted source code.

Source code needed to fit and export the models is included. The original train/test membership file and trained bundles require an appropriate controlled-access arrangement for exact reproduction. No controlled-access deposit or DOI has been created by this package; those must be established by the authors before claiming they are available at a particular location.

`scripts/prepare_web.R` reads an existing successful local export and keeps it in the ignored `work/` directory. The software-test fixture is generated in a temporary directory and is never used as a fallback by the formal calculator.

Database publications: Johnson et al., *Scientific Data* (2023), [10.1038/s41597-022-01899-x](https://doi.org/10.1038/s41597-022-01899-x); Pollard et al., *Scientific Data* (2018), [10.1038/sdata.2018.178](https://doi.org/10.1038/sdata.2018.178).
