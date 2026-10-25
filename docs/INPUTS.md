# Prediction input contract

The public definition is in `app/feature_dictionary.csv`; units and bounds are in `app/component_units_and_bounds.csv`. `app/input_template.csv` is a blank template and `app/synthetic_example.csv` contains artificial example records.

The 308 columns comprise 300 unscaled physiological window statistics, three baseline predictors (age, sex, ICU status at 48 hours), and five eligibility/version fields. The 252 continuous model summaries and 12 derived domain scores are not identical to the full input contract. Additional extrema are required for score construction. A single measurement per variable cannot replace the window summaries.

Windows are cumulative 0–6, 0–24 and 0–48 hours after index ICU admission. Statistics include mean, sample standard deviation, slope per hour, abnormal fraction and specified minima/maxima. Abnormal fractions are between 0 and 1. Preserve genuinely missing values; all-empty physiological input is rejected.

The 21 measurement variables are heart rate, systolic blood pressure, mean arterial pressure, respiratory rate, SpO2, temperature, GCS eye/verbal/motor responses, creatinine, blood urea nitrogen, sodium, potassium, chloride, bicarbonate, glucose, white blood cell count, hemoglobin, platelets, INR and PTT. ECG waveforms, LVEF and phenotype indicators are not model-C inputs.

Use `ami_api$write_csv()` for exact exports and `ami_api$read_csv()` for import. Some noninteger values are encoded as C99 hexadecimal floats to preserve binary precision and empirical-score ties. The calculator displays conventional decimal probabilities; its rounded result report is not an input template.

The 30-day outcome begins at the 48-hour landmark. The hospital outcome is death in the remaining index admission and requires presence in that hospital at the landmark. eICU in-hospital evaluation does not constitute external validation of the 30-day outcome. The app uses frozen original MIMIC models; hospital-blocked cross-fitted updating is an evaluation procedure, not an arbitrary calibration fold selected for a new patient.
