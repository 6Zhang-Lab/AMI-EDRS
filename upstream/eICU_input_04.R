############################################################
## Step E2D-QC: Clean support/treatment proxy evidence
## Purpose:
##   Remove false-positive proxy evidence before Step E3
##   and regenerate cleaned proxy summary, wide table, and figures.
############################################################

rm(list = ls())
gc()

options(stringsAsFactors = FALSE)

pkg_needed <- c(
  "dplyr", "tidyr", "stringr", "purrr", "tibble",
  "readr", "ggplot2", "patchwork", "scales", "openxlsx"
)

for (p in pkg_needed) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

invisible(lapply(pkg_needed, library, character.only = TRUE))

## =========================================================
## 1. Paths
## =========================================================

project_dir <- Sys.getenv("AMI_EICU_PROJECT", unset=file.path(getwd(), "work/eICU"))

config_file <- file.path(project_dir, "00_admin", "StepE0_path_config.rds")
path_config <- readRDS(config_file)
dirs <- path_config$dirs

dir_feature <- dirs$features
dir_e2d <- file.path(dir_feature, "E2D_support_treatment_proxies")
dir_tables <- file.path(dirs$tables, "feature_tables")
dir_objects <- dirs$objects
dir_logs <- dirs$logs

fig_dir_avail <- file.path(dirs$figures, "variable_availability")
fig_dir_explore <- file.path(dirs$figures, "feature_exploration")

dir.create(dir_e2d, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir_avail, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir_explore, recursive = TRUE, showWarnings = FALSE)

obj_file <- file.path(
  dir_objects,
  "StepE2D_support_treatment_proxy_object.rds"
)

if (!file.exists(obj_file)) {
  stop("Cannot find StepE2D object: ", obj_file)
}

obj <- readRDS(obj_file)

proxy_evidence_all <- obj$proxy_evidence_all
ami_t48 <- obj$ami_t48

if (is.null(proxy_evidence_all) || nrow(proxy_evidence_all) == 0) {
  stop("proxy_evidence_all is empty in StepE2D object.")
}

if (is.null(ami_t48) || nrow(ami_t48) == 0) {
  stop("ami_t48 is empty in StepE2D object.")
}

cohort_ids <- unique(as.character(ami_t48$patientunitstayid))

cat("T48 cohort:", length(cohort_ids), "\n")
cat("Original proxy evidence rows:", nrow(proxy_evidence_all), "\n")

## =========================================================
## 2. Helper functions
## =========================================================

clean_text <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_replace_all("[\r\n\t]", " ") %>%
    stringr::str_squish() %>%
    tolower()
}

standardize_window <- function(df) {
  df %>%
    dplyr::mutate(
      window_6h  = dplyr::if_else(offset_min >= 0 & offset_min <= 6  * 60, "0-6h",  NA_character_),
      window_24h = dplyr::if_else(offset_min >= 0 & offset_min <= 24 * 60, "0-24h", NA_character_),
      window_48h = dplyr::if_else(offset_min >= 0 & offset_min <= 48 * 60, "0-48h", NA_character_),
      window_72h = dplyr::if_else(offset_min >= 0 & offset_min <= 72 * 60, "0-72h", NA_character_)
    )
}

make_text_example <- function(x, n = 3) {
  x <- unique(x[!is.na(x) & x != ""])
  if (length(x) == 0) return(NA_character_)
  paste(utils::head(x, n), collapse = " || ")
}

theme_eicu <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = base_size + 7, hjust = 0),
      plot.subtitle = element_text(size = base_size + 1, color = "grey30", hjust = 0),
      axis.text = element_text(color = "black"),
      axis.title = element_text(face = "bold", color = "black"),
      legend.title = element_text(face = "bold"),
      plot.margin = margin(8, 10, 8, 8)
    )
}

## =========================================================
## 3. Source-specific proxy cleaning rules
## =========================================================

proxy_clean <- proxy_evidence_all %>%
  dplyr::mutate(
    patientunitstayid = as.character(patientunitstayid),
    offset_min = suppressWarnings(as.numeric(offset_min)),
    offset_hour = offset_min / 60,
    source = as.character(source),
    proxy = as.character(proxy),
    evidence_text = as.character(evidence_text),
    evidence_clean = clean_text(evidence_text),
    value = suppressWarnings(as.numeric(value))
  ) %>%
  dplyr::filter(
    patientunitstayid %in% cohort_ids,
    !is.na(offset_min),
    offset_min >= 0,
    offset_min <= 72 * 60
  ) %>%
  dplyr::mutate(
    keep = dplyr::case_when(
      
      ## GCS: only nurseCharting and apacheApsVar are trusted
      proxy %in% c("gcs_total", "gcs_eye", "gcs_verbal", "gcs_motor") ~
        source %in% c("nurseCharting", "apacheApsVar"),
      
      ## Mechanical ventilation: exclude medication to avoid SIMVASTATIN false positive
      proxy == "mechanical_ventilation" ~
        source %in% c("nurseCharting", "respiratoryCharting", "respiratoryCare", "treatment") &
        stringr::str_detect(
          evidence_clean,
          "mechanical ventilation|ventilator|vent mode|assist control|pressure control|volume control|\\bsimv\\b|\\bprvc\\b|\\bbipap\\b|\\bcpap\\b|intubated|endotracheal|\\bett\\b|\\btrach\\b|oral ett"
        ) &
        !stringr::str_detect(
          evidence_clean,
          "simvastatin|atorvastatin|pravastatin|rosuvastatin|lovastatin"
        ),
      
      ## Oxygen support
      proxy == "oxygen_support" ~
        source %in% c("nurseCharting", "respiratoryCharting", "respiratoryCare", "treatment") &
        stringr::str_detect(
          evidence_clean,
          "nasal cannula|face mask|non-rebreather|high flow|hfnc|oxygen therapy|fio2|o2 flow|oxygen flow|lpm o2|o2 saturation|o2 admin"
        ),
      
      ## Vasopressor: trusted medication/infusion/treatment sources only
      proxy == "vasopressor" ~
        source %in% c("infusionDrug", "medication", "treatment") &
        stringr::str_detect(
          evidence_clean,
          "norepinephrine|levophed|epinephrine|adrenaline|dopamine|dobutamine|vasopressin|phenylephrine|neosynephrine|milrinone"
        ),
      
      ## Dialysis: treatment and intakeOutput only; exclude catheter medication false positives
      proxy == "dialysis" ~
        source %in% c("treatment", "intakeOutput") &
        stringr::str_detect(
          evidence_clean,
          "dialysis|hemodialysis|haemodialysis|crrt|continuous renal replacement|cvvh|cvvhd|ultrafiltration|uf removed"
        ),
      
      ## PCI/cath: treatment only
      proxy == "pci_cath" ~
        source == "treatment" &
        stringr::str_detect(
          evidence_clean,
          "pci|percutaneous coronary|cardiac cath|catheterization|angioplasty|stent|coronary intervention"
        ),
      
      ## IABP/MCS
      proxy == "iabp_mcs" ~
        source %in% c("treatment", "nurseCharting", "intakeOutput") &
        stringr::str_detect(
          evidence_clean,
          "iabp|intra-aortic balloon|impella|ecmo|mechanical circulatory support"
        ),
      
      ## Urine output: treat as a recorded-output proxy, not strict numeric urine volume
      proxy == "urine_output" ~
        source == "intakeOutput" &
        stringr::str_detect(
          evidence_clean,
          "\\burine\\b|\\buop\\b|urinary output|foley"
        ),
      
      TRUE ~ FALSE
    )
  ) %>%
  dplyr::filter(keep) %>%
  dplyr::mutate(
    value = dplyr::case_when(
      proxy == "gcs_total"  & (value < 3 | value > 15) ~ NA_real_,
      proxy == "gcs_eye"    & (value < 1 | value > 4)  ~ NA_real_,
      proxy == "gcs_verbal" & (value < 1 | value > 5)  ~ NA_real_,
      proxy == "gcs_motor"  & (value < 1 | value > 6)  ~ NA_real_,
      TRUE ~ value
    ),
    binary_value = 1L
  ) %>%
  dplyr::select(
    patientunitstayid,
    offset_min,
    offset_hour,
    source,
    proxy,
    value,
    evidence_text,
    binary_value
  ) %>%
  dplyr::distinct()

cat("Cleaned proxy evidence rows:", nrow(proxy_clean), "\n")
cat("Patients with cleaned proxy evidence:", dplyr::n_distinct(proxy_clean$patientunitstayid), "\n\n")

## =========================================================
## 4. Add APACHE-derived total GCS from eye + verbal + motor
## =========================================================

apache_gcs_total <- proxy_clean %>%
  dplyr::filter(
    source == "apacheApsVar",
    proxy %in% c("gcs_eye", "gcs_verbal", "gcs_motor"),
    !is.na(value)
  ) %>%
  dplyr::group_by(patientunitstayid, proxy) %>%
  dplyr::summarise(value = dplyr::first(value), .groups = "drop") %>%
  tidyr::pivot_wider(
    id_cols = patientunitstayid,
    names_from = proxy,
    values_from = value
  ) %>%
  dplyr::mutate(
    gcs_total_value = gcs_eye + gcs_verbal + gcs_motor
  ) %>%
  dplyr::filter(!is.na(gcs_total_value), gcs_total_value >= 3, gcs_total_value <= 15) %>%
  dplyr::transmute(
    patientunitstayid,
    offset_min = 0,
    offset_hour = 0,
    source = "apacheApsVar",
    proxy = "gcs_total",
    value = gcs_total_value,
    evidence_text = paste0(
      "APACHE-derived total GCS = eye + verbal + motor = ",
      gcs_total_value
    ),
    binary_value = 1L
  )

proxy_clean <- dplyr::bind_rows(proxy_clean, apache_gcs_total) %>%
  dplyr::distinct()

cat("Cleaned proxy evidence rows after APACHE total GCS:", nrow(proxy_clean), "\n")
cat("Patients after APACHE total GCS:", dplyr::n_distinct(proxy_clean$patientunitstayid), "\n\n")

readr::write_csv(
  proxy_clean,
  file.path(dir_e2d, "StepE2D_QC_cleaned_proxy_evidence_long_T0_T72.csv")
)

## =========================================================
## 5. Rebuild summary by window
## =========================================================

proxy_window_long <- proxy_clean %>%
  standardize_window() %>%
  tidyr::pivot_longer(
    cols = c(window_6h, window_24h, window_48h, window_72h),
    names_to = "window_type",
    values_to = "window"
  ) %>%
  dplyr::filter(!is.na(window)) %>%
  dplyr::mutate(
    window = factor(window, levels = c("0-6h", "0-24h", "0-48h", "0-72h"))
  )

numeric_proxy <- c("gcs_total", "gcs_eye", "gcs_verbal", "gcs_motor")

binary_proxy <- setdiff(
  sort(unique(proxy_window_long$proxy)),
  numeric_proxy
)

proxy_numeric_summary <- proxy_window_long %>%
  dplyr::filter(proxy %in% numeric_proxy, !is.na(value)) %>%
  dplyr::arrange(patientunitstayid, proxy, window, offset_min) %>%
  dplyr::group_by(patientunitstayid, proxy, window) %>%
  dplyr::summarise(
    n_measure = dplyr::n(),
    first_value = dplyr::first(value),
    last_value = dplyr::last(value),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    any_flag = 1L,
    .groups = "drop"
  )

proxy_binary_summary <- proxy_window_long %>%
  dplyr::filter(proxy %in% binary_proxy) %>%
  dplyr::group_by(patientunitstayid, proxy, window) %>%
  dplyr::summarise(
    n_measure = dplyr::n(),
    first_value = 1,
    last_value = 1,
    min_value = 1,
    max_value = 1,
    mean_value = 1,
    median_value = 1,
    any_flag = 1L,
    .groups = "drop"
  )

proxy_summary <- dplyr::bind_rows(
  proxy_numeric_summary,
  proxy_binary_summary
)

proxy_order <- c(
  "gcs_total",
  "gcs_eye",
  "gcs_verbal",
  "gcs_motor",
  "oxygen_support",
  "mechanical_ventilation",
  "vasopressor",
  "urine_output",
  "dialysis",
  "pci_cath",
  "iabp_mcs"
)

expected_proxy <- tidyr::expand_grid(
  patientunitstayid = cohort_ids,
  proxy = proxy_order,
  window = factor(
    c("0-6h", "0-24h", "0-48h", "0-72h"),
    levels = c("0-6h", "0-24h", "0-48h", "0-72h")
  )
)

proxy_summary_clean <- expected_proxy %>%
  dplyr::left_join(
    proxy_summary,
    by = c("patientunitstayid", "proxy", "window")
  ) %>%
  dplyr::mutate(
    n_measure = tidyr::replace_na(n_measure, 0L),
    any_flag = tidyr::replace_na(any_flag, 0L),
    available = as.integer(n_measure > 0)
  )

readr::write_csv(
  proxy_summary_clean,
  file.path(dir_e2d, "StepE2D_QC_cleaned_proxy_summary_by_window.csv")
)

## =========================================================
## 6. Cleaned wide features
## =========================================================

proxy_wide_clean <- proxy_summary_clean %>%
  dplyr::mutate(
    window_clean = dplyr::case_when(
      window == "0-6h"  ~ "T0_6h",
      window == "0-24h" ~ "T24",
      window == "0-48h" ~ "T48",
      window == "0-72h" ~ "T72",
      TRUE ~ as.character(window)
    )
  ) %>%
  dplyr::select(
    patientunitstayid,
    proxy,
    window_clean,
    n_measure,
    first_value,
    last_value,
    min_value,
    max_value,
    mean_value,
    median_value,
    any_flag
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      n_measure,
      first_value,
      last_value,
      min_value,
      max_value,
      mean_value,
      median_value,
      any_flag
    ),
    names_to = "stat",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    feature_name = paste(proxy, window_clean, stat, sep = "_")
  ) %>%
  dplyr::select(patientunitstayid, feature_name, value) %>%
  tidyr::pivot_wider(
    id_cols = patientunitstayid,
    names_from = feature_name,
    values_from = value
  )

ami_t48_proxy_wide_clean <- ami_t48 %>%
  dplyr::select(
    patientunitstayid,
    dplyr::any_of(c(
      "uniquepid",
      "age",
      "sex",
      "ethnicity",
      "primary_external_endpoint",
      "secondary_icu_mortality_after_T48",
      "in_icu_at_T48",
      "hospital_los_from_icu_hours",
      "icu_los_hours"
    ))
  ) %>%
  dplyr::left_join(proxy_wide_clean, by = "patientunitstayid")

readr::write_csv(
  proxy_wide_clean,
  file.path(dir_e2d, "StepE2D_QC_cleaned_support_treatment_proxy_wide_features_only.csv")
)

readr::write_csv(
  ami_t48_proxy_wide_clean,
  file.path(dir_e2d, "StepE2D_QC_cleaned_eICU_AMI_T48_cohort_with_support_treatment_features.csv")
)

## =========================================================
## 7. Cleaned availability and evidence summary
## =========================================================

proxy_availability_clean <- proxy_summary_clean %>%
  dplyr::group_by(proxy, window) %>%
  dplyr::summarise(
    n_available = sum(available == 1, na.rm = TRUE),
    n_total = length(cohort_ids),
    availability_pct = n_available / n_total * 100,
    missing_pct = 100 - availability_pct,
    median_n_measure_if_available = median(n_measure[n_measure > 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    median_n_measure_if_available = ifelse(
      is.infinite(median_n_measure_if_available),
      NA_real_,
      median_n_measure_if_available
    )
  ) %>%
  dplyr::arrange(proxy, window)

proxy_evidence_summary_clean <- proxy_clean %>%
  dplyr::group_by(proxy, source) %>%
  dplyr::summarise(
    n_records = dplyr::n(),
    n_patients = dplyr::n_distinct(patientunitstayid),
    example_text = make_text_example(evidence_text, n = 3),
    .groups = "drop"
  ) %>%
  dplyr::arrange(proxy, dplyr::desc(n_patients))

readr::write_csv(
  proxy_availability_clean,
  file.path(dir_tables, "StepE2D_QC_cleaned_support_treatment_proxy_availability_by_window.csv")
)

readr::write_csv(
  proxy_evidence_summary_clean,
  file.path(dir_tables, "StepE2D_QC_cleaned_support_treatment_proxy_evidence_summary.csv")
)

## =========================================================
## 8. Cleaned availability figure
## =========================================================

pretty_proxy_names <- c(
  gcs_total = "GCS total",
  gcs_eye = "GCS eye",
  gcs_verbal = "GCS verbal",
  gcs_motor = "GCS motor",
  oxygen_support = "Oxygen support",
  mechanical_ventilation = "Mechanical ventilation",
  vasopressor = "Vasopressor",
  urine_output = "Urine output",
  dialysis = "Dialysis / CRRT",
  pci_cath = "PCI / cath",
  iabp_mcs = "IABP / MCS"
)

plot_avail_df <- proxy_availability_clean %>%
  dplyr::filter(proxy %in% proxy_order) %>%
  dplyr::mutate(
    proxy_pretty = factor(
      pretty_proxy_names[proxy],
      levels = rev(pretty_proxy_names[proxy_order])
    ),
    window = factor(window, levels = c("0-6h", "0-24h", "0-48h", "0-72h")),
    label = sprintf("%.1f", availability_pct)
  )

plot_bar_df <- plot_avail_df %>%
  dplyr::group_by(proxy_pretty) %>%
  dplyr::summarise(
    mean_availability_pct = mean(availability_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    proxy_pretty = factor(proxy_pretty, levels = levels(plot_avail_df$proxy_pretty))
  )

p_proxy_heat <- ggplot(
  plot_avail_df,
  aes(x = window, y = proxy_pretty, fill = availability_pct)
) +
  geom_tile(
    color = "white",
    linewidth = 0.9,
    width = 0.98,
    height = 0.98
  ) +
  geom_text(
    aes(label = label),
    size = 3.7,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_gradientn(
    colours = c("#F8FAFC", "#DBEAFE", "#93C5FD", "#3B82F6", "#1E3A8A"),
    limits = c(0, 100),
    name = "Availability (%)"
  ) +
  labs(x = NULL, y = NULL) +
  coord_fixed(ratio = 0.72, clip = "off") +
  theme_eicu(base_size = 15) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(face = "bold", size = 13, angle = 45, hjust = 1),
    axis.text.y = element_text(face = "bold", size = 13)
  )

p_proxy_bar <- ggplot(
  plot_bar_df,
  aes(x = mean_availability_pct, y = proxy_pretty)
) +
  geom_col(
    width = 0.75,
    fill = "#5DA9D6",
    color = "white",
    linewidth = 0.35
  ) +
  geom_text(
    aes(label = sprintf("%.1f", mean_availability_pct)),
    hjust = -0.15,
    size = 3.7,
    fontface = "bold"
  ) +
  scale_x_continuous(
    limits = c(0, 105),
    expand = c(0, 0)
  ) +
  labs(x = "Mean availability (%)", y = NULL) +
  theme_eicu(base_size = 15) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    panel.grid.major.x = element_line(color = "#E5E7EB", linewidth = 0.45)
  )

fig_proxy_avail_clean <- p_proxy_heat + p_proxy_bar +
  patchwork::plot_layout(widths = c(4.2, 1.7)) +
  patchwork::plot_annotation(
    title = "Cleaned availability of support and treatment proxies across early eICU windows",
    subtitle = "Source-specific rules were applied to reduce false-positive proxy assignments",
    theme = theme(
      plot.title = element_text(face = "bold", size = 23, hjust = 0),
      plot.subtitle = element_text(size = 15.5, color = "grey30", hjust = 0),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 12, 10, 10)
    )
  )

print(fig_proxy_avail_clean)

ggsave(
  file.path(fig_dir_avail, "FigureE2D_QC_cleaned_support_treatment_proxy_availability_heatmap_barplot.pdf"),
  fig_proxy_avail_clean,
  width = 10.5,
  height = 6.7,
  device = cairo_pdf,
  bg = "white"
)

ggsave(
  file.path(fig_dir_avail, "FigureE2D_QC_cleaned_support_treatment_proxy_availability_heatmap_barplot.png"),
  fig_proxy_avail_clean,
  width = 10.5,
  height = 6.7,
  dpi = 600,
  bg = "white"
)

## =========================================================
## 9. Save cleaned object and Excel workbook
## =========================================================

cleaned_object <- list(
  ami_t48 = ami_t48,
  proxy_clean = proxy_clean,
  proxy_summary_clean = proxy_summary_clean,
  proxy_wide_clean = proxy_wide_clean,
  ami_t48_proxy_wide_clean = ami_t48_proxy_wide_clean,
  proxy_availability_clean = proxy_availability_clean,
  proxy_evidence_summary_clean = proxy_evidence_summary_clean,
  created_at = Sys.time()
)

saveRDS(
  cleaned_object,
  file.path(dir_objects, "StepE2D_QC_cleaned_support_treatment_proxy_object.rds")
)

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

method_note <- tibble::tibble(
  item = c(
    "Purpose",
    "Reason for QC cleaning",
    "GCS rule",
    "Mechanical ventilation rule",
    "Vasopressor rule",
    "Urine output rule",
    "Downstream use"
  ),
  description = c(
    "Clean source-specific support and treatment proxy evidence before organ-domain score reconstruction.",
    "The raw text-based extraction can generate false positives, such as SIMVASTATIN being matched by SIMV and eye drops being matched as GCS eye evidence.",
    "GCS evidence was restricted to nurseCharting and apacheApsVar. APACHE total GCS was reconstructed from eye, verbal, and motor components.",
    "Mechanical ventilation evidence was restricted to respiratory/nursing/treatment sources and medication-derived false positives were removed.",
    "Vasopressor evidence was restricted to infusionDrug, medication, and treatment records with specific vasoactive drug names.",
    "Urine output was treated as a recorded-output proxy from intakeOutput rather than as a strict continuous quantitative urine-volume variable.",
    "The cleaned wide feature table should be used for Step E3 organ-domain score reconstruction."
  )
)

add_sheet(wb, "method_note", method_note)
add_sheet(wb, "cleaned_availability", proxy_availability_clean)
add_sheet(wb, "cleaned_evidence_summary", proxy_evidence_summary_clean)
add_sheet(wb, "cleaned_summary_preview", head(proxy_summary_clean, 5000))
add_sheet(wb, "cleaned_wide_preview", head(ami_t48_proxy_wide_clean, 5000))

out_xlsx <- file.path(
  dir_e2d,
  "StepE2D_QC_cleaned_support_treatment_proxy_summary.xlsx"
)

openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

sink(file.path(dir_logs, "StepE2D_QC_cleaned_sessionInfo.txt"))
cat("Step E2D-QC cleaned session information\n")
cat("=======================================\n\n")
print(sessionInfo())
sink()

## =========================================================
## 10. Console summary
## =========================================================

cat("\n================ Step E2D-QC cleaned finished ================\n")
cat("Original proxy evidence rows:", nrow(proxy_evidence_all), "\n")
cat("Cleaned proxy evidence rows:", nrow(proxy_clean), "\n")
cat("Patients with cleaned proxy evidence:", dplyr::n_distinct(proxy_clean$patientunitstayid), "\n\n")

cat("Cleaned proxy availability:\n")
print(proxy_availability_clean, n = 100)

cat("\nCleaned evidence summary:\n")
print(proxy_evidence_summary_clean, n = 100)

cat("\nMain outputs:\n")
cat(file.path(dir_e2d, "StepE2D_QC_cleaned_proxy_evidence_long_T0_T72.csv"), "\n")
cat(file.path(dir_e2d, "StepE2D_QC_cleaned_proxy_summary_by_window.csv"), "\n")
cat(file.path(dir_e2d, "StepE2D_QC_cleaned_support_treatment_proxy_wide_features_only.csv"), "\n")
cat(file.path(dir_e2d, "StepE2D_QC_cleaned_eICU_AMI_T48_cohort_with_support_treatment_features.csv"), "\n")
cat(file.path(dir_tables, "StepE2D_QC_cleaned_support_treatment_proxy_availability_by_window.csv"), "\n")
cat(out_xlsx, "\n")
cat(file.path(dir_objects, "StepE2D_QC_cleaned_support_treatment_proxy_object.rds"), "\n")
cat(file.path(fig_dir_avail, "FigureE2D_QC_cleaned_support_treatment_proxy_availability_heatmap_barplot.pdf"), "\n")
cat("==============================================================\n")