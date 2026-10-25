# Current analysis order

Run in the order below. Stage 5 has two required scripts; the second corrects the first-stage cache. Numbered filenames preserve source provenance.

| Order | Analysis | Source |
|---|---|---|
| 1 | Legacy input audit (optional) | [Step0_AMI_EDRS_Audit.R](../analysis/Step0_AMI_EDRS_Audit.R) |
| 2 | Cohort and endpoint reconstruction | [Step1_AMI_EDRS_Cohort_Endpoints-1.R](../analysis/Step1_AMI_EDRS_Cohort_Endpoints-1.R) |
| 3 | Frozen cohorts and variable audit | [Step2_AMI_EDRS_Cohort_Freeze_and_Variable_Audit.R](../analysis/Step2_AMI_EDRS_Cohort_Freeze_and_Variable_Audit.R) |
| 4 | eICU measurement repair | [Step3_AMI_EDRS_eICU_Measurement_Repair.R](../analysis/Step3_AMI_EDRS_eICU_Measurement_Repair.R) |
| 5 | MIMIC repair and common features | [Step4_AMI_EDRS_MIMIC_Repair_and_Common_Features-1.R](../analysis/Step4_AMI_EDRS_MIMIC_Repair_and_Common_Features-1.R) |
| 6 | Timed clinical context and correction | [Step5_AMI_EDRS_Timed_Clinical_Context-1.R](../analysis/Step5_AMI_EDRS_Timed_Clinical_Context-1.R) |
| 7 | Timed clinical context and correction | [Step5_AMI_EDRS_Timed_Clinical_Context-2.R](../analysis/Step5_AMI_EDRS_Timed_Clinical_Context-2.R) |
| 8 | Scoring specification and model inputs | [Step6_AMI_EDRS_Scoring_Specification_and_Model_Inputs-1.R](../analysis/Step6_AMI_EDRS_Scoring_Specification_and_Model_Inputs-1.R) |
| 9 | Nested T48 phenotyping and A/B/C/D models | [Step7_AMI_EDRS_T48_Nested_Phenotyping_and_Models.R](../analysis/Step7_AMI_EDRS_T48_Nested_Phenotyping_and_Models.R) |
| 10 | Clinical and observation-process comparators | [Step8_AMI_EDRS_T48_Clinical_and_Observation_Comparators.R](../analysis/Step8_AMI_EDRS_T48_Clinical_and_Observation_Comparators.R) |
| 11 | Earlier 6-hour and 24-hour landmarks | [Step9_AMI_EDRS_T6_T24_Early_Landmark_Validation.R](../analysis/Step9_AMI_EDRS_T6_T24_Early_Landmark_Validation.R) |
| 12 | Frozen phenotype profiles and missingness | [Step10_AMI_EDRS_Frozen_Phenotype_Profiles_and_Missingness.R](../analysis/Step10_AMI_EDRS_Frozen_Phenotype_Profiles_and_Missingness.R) |
| 13 | T48 decision curves and descriptive risk bands | [Step11_AMI_EDRS_T48_Decision_Curves_and_Risk_Bands.R](../analysis/Step11_AMI_EDRS_T48_Decision_Curves_and_Risk_Bands.R) |
| 14 | Frozen external inference | [Step12_AMI_EDRS_eICU_Frozen_External_Evaluation.R](../analysis/Step12_AMI_EDRS_eICU_Frozen_External_Evaluation.R) |
| 15 | Matched in-hospital endpoint models | [Step13_AMI_EDRS_Hospital_Endpoint_Companion_Models-1.R](../analysis/Step13_AMI_EDRS_Hospital_Endpoint_Companion_Models-1.R) |
| 16 | Hospital-blocked cross-fitted recalibration | [Step14_AMI_EDRS_eICU_CrossFitted_Recalibration.R](../analysis/Step14_AMI_EDRS_eICU_CrossFitted_Recalibration.R) |
| 17 | Timed care-limitation classification | [Step15_AMI_EDRS_Timed_Care_Limitation_Audit-2.R](../analysis/Step15_AMI_EDRS_Timed_Care_Limitation_Audit-2.R) |
| 18 | Documented comfort-care sensitivity | [Step16_AMI_EDRS_Documented_CMO_Sensitivity.R](../analysis/Step16_AMI_EDRS_Documented_CMO_Sensitivity.R) |
| 19 | Clinical-score provenance and feasibility | [Step17_AMI_EDRS_Traditional_Score_Provenance_and_Feasibility.R](../analysis/Step17_AMI_EDRS_Traditional_Score_Provenance_and_Feasibility.R) |
| 20 | Native APACHE comparison | [Step18_AMI_EDRS_Native_APACHE_Benchmark_Comparison.R](../analysis/Step18_AMI_EDRS_Native_APACHE_Benchmark_Comparison.R) |
| 21 | Held-out phenotype associations | [Step19_AMI_EDRS_Heldout_Phenotype_Associations-1.R](../analysis/Step19_AMI_EDRS_Heldout_Phenotype_Associations-1.R) |
| 22 | External decision curves and final aggregate exports | [Step20_AMI_EDRS_External_Decision_Utility_and_Final_Exports.R](../analysis/Step20_AMI_EDRS_External_Decision_Utility_and_Final_Exports.R) |
| 23 | Frozen model export and consistency | [Step21_AMI_EDRS_Frozen_Web_Prototype_and_Consistency-2.R](../analysis/Step21_AMI_EDRS_Frozen_Web_Prototype_and_Consistency-2.R) |

The original extraction prerequisites have a separate ordered list in `upstream/run_order.csv`. The historical files in `provenance/legacy/` document previous inputs and split generation; they are not alternative final models.
