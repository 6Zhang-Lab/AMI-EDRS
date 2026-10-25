# ============================================================================
# AMI-EDRS revision | Step0: existing-file and data-field audit
# Version 1.0 | 2026-10-21
# 运行方式：source(file.choose(), encoding = "UTF-8")，选择本文件。
# 沿用原脚本路径；无需先运行任何旧脚本。不会清空R环境、重训或改写旧文件。
# 仅输出汇总审计结果；to_share/及反馈ZIP不包含患者ID或患者逐行数据。
# local_only/是留在本机的部分旧文件备份，不要作为反馈上传。
# ============================================================================

local({
  # ---------- 1. 路径：与你上传的原代码一致 ----------
  cfg <- list(
    MIMIC_PROJECT = Sys.getenv("AMI_MIMIC_PROJECT", unset=file.path(getwd(), "work/MIMIC")),
    EICU_PROJECT = Sys.getenv("AMI_EICU_PROJECT", unset=file.path(getwd(), "work/eICU")),
    MIMIC_RAW = Sys.getenv("AMI_MIMIC_RAW", unset=file.path(getwd(), "data/mimic-iv-3.1")),
    EICU_RAW = Sys.getenv("AMI_EICU_RAW", unset=file.path(getwd(), "data/eicu-crd-2.0")),
    APP_PROJECT = Sys.getenv("AMI_LEGACY_APP", unset=file.path(getwd(), "work/legacy_app")),
    APP_RELEASE = Sys.getenv("AMI_LEGACY_RELEASE", unset=file.path(getwd(), "work/legacy_release")),
    OUTPUT_PARENT = Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    EXTRA_CODE_DIRS = character(), # 旧R脚本另存他处时，可填目录；不填也可运行
    MAX_CSV_MB = 128,              # 更大的CSV只读字段，不载入患者数据
    MAX_AUDIT_ROWS = 200000L,      # 超出时明确标为截取审计，不冒充全量统计
    SNAPSHOT_MAX_FILE_MB = 256,
    SNAPSHOT_TOTAL_MB = 2048,      # 仅备份关键文件及小型代码，不复制整库
    INSPECT_DUCKDB = TRUE          # 有DBI/duckdb时只读系统目录，不扫描大表
  )
  # 可选：供自动化运行覆盖配置；普通RStudio运行无需设置。
  cfg <- utils::modifyList(cfg, getOption("ami_step0.config", list()))
  norm <- function(x) normalizePath(path.expand(x), winslash = "/", mustWork = FALSE)
  path_keys <- c("MIMIC_PROJECT", "EICU_PROJECT", "MIMIC_RAW", "EICU_RAW",
                 "APP_PROJECT", "APP_RELEASE", "OUTPUT_PARENT")
  for (k in path_keys) cfg[[k]] <- norm(cfg[[k]])
  if (!dir.exists(cfg$MIMIC_PROJECT)) {
    stop("找不到MIMIC旧项目：", cfg$MIMIC_PROJECT,
         "\n请核对脚本顶部MIMIC_PROJECT；不要为通过检查新建空目录。")
  }
  if (!dir.exists(file.path(cfg$MIMIC_PROJECT, "03_analysis_dataset")) &&
      !dir.exists(file.path(cfg$MIMIC_PROJECT, "01_cohort"))) {
    stop("MIMIC_PROJECT下既无01_cohort也无03_analysis_dataset，请确认项目根目录。")
  }
  stamp <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", Sys.getpid())
  out <- file.path(cfg$OUTPUT_PARENT, paste0("Step0_", stamp))
  if (dir.exists(out)) stop("本次输出目录已存在，请稍后重新运行。")
  share <- file.path(out, "to_share")
  private <- file.path(out, "local_only", "legacy_snapshot")
  dir.create(share, recursive = TRUE, showWarnings = FALSE)
  dir.create(private, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(share) || file.access(share, 2) != 0L) stop("无法写入：", out)
  cat("\nStep0开始。输出目录：\n", out, "\n", sep = "")
  log_path <- file.path(share, "Step0_run_log.txt")
  log_line <- function(...) {
    s <- paste0(format(Sys.time(), "%H:%M:%S"), " | ", paste0(..., collapse = ""))
    cat(s, "\n"); cat(s, "\n", file = log_path, append = TRUE)
  }
  rows <- new.env(parent = emptyenv())
  emit <- function(.table_name, ...) {
    r <- data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
    rows[[.table_name]] <- c(rows[[.table_name]], list(r))
    invisible(NULL)
  }
  issue <- function(level, context, message) {
    emit("14_issues", severity = level, context = context, message = message)
  }
  # 不把读入警告中的患者值写入反馈；详细异常由后续针对性检查处理。
  attempt <- function(expr, context) {
    nw <- 0L
    ans <- tryCatch(withCallingHandlers(expr, warning = function(w) {
      nw <<- nw + 1L; invokeRestart("muffleWarning")
    }), error = function(e) {
      issue("CHECK", context, "操作失败；本文件/模块未完成。未导出原始错误中的数据值。")
      NULL
    })
    if (nw > 0L) issue("CHECK", context, paste0("发生", nw, "条读取/解析警告；不视为无异常。"))
    ans
  }
  write_csv <- function(x, name) {
    utils::write.csv(x, file.path(share, paste0(name, ".csv")),
                     row.names = FALSE, na = "", fileEncoding = "UTF-8")
  }
  is_missing <- function(x) is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "nan", "null")
  finite_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  ntrue <- function(x) sum(x, na.rm = TRUE)
  check <- function(file, item, n, denominator = NA_real_, note = "") {
    emit("09_consistency_checks", file = file, check = item, n = as.numeric(n),
         denominator = as.numeric(denominator), note = note)
  }

  # ---------- 2. 清点旧项目，不递归扫描原始数据库 ----------
  roots <- c(MIMIC = cfg$MIMIC_PROJECT, eICU = cfg$EICU_PROJECT,
             APP = cfg$APP_PROJECT, APP_RELEASE = cfg$APP_RELEASE)
  if (length(cfg$EXTRA_CODE_DIRS)) {
    extra <- vapply(cfg$EXTRA_CODE_DIRS, norm, character(1))
    roots <- c(roots, stats::setNames(extra, paste0("CODE_", seq_along(extra))))
  }
  root_table <- data.frame(source = names(roots), path = unname(roots),
                           exists = dir.exists(roots), stringsAsFactors = FALSE)
  write_csv(root_table, "01_project_roots")
  inv_list <- list()
  for (src in names(roots)) {
    root <- roots[[src]]
    if (!dir.exists(root)) {
      issue(if (src == "eICU") "CHECK" else "INFO", src, "目录不存在；该部分跳过，不影响其余审计。")
      next
    }
    log_line("清点项目：", src)
    ff <- list.files(root, full.names = TRUE, recursive = TRUE, all.files = FALSE,
                     include.dirs = FALSE)
    ff <- ff[!grepl("(^|/)(__MACOSX|renv|packrat|node_modules|\\.git)(/|$)", ff)]
    ff <- ff[!startsWith(basename(ff), "._")]
    ff <- ff[!startsWith(ff, paste0(norm(cfg$OUTPUT_PARENT), "/"))]
    if (!length(ff)) next
    fi <- file.info(ff)
    keep <- !is.na(fi$isdir) & !fi$isdir
    ff <- ff[keep]; fi <- fi[keep, , drop = FALSE]
    inv_list[[src]] <- data.frame(source = src, file = basename(ff),
      relative_path = substring(ff, nchar(root) + 2L), absolute_path = ff,
      bytes = as.numeric(fi$size), mb = round(fi$size / 1024^2, 4),
      modified = format(fi$mtime, "%Y-%m-%d %H:%M:%S %z"),
      md5 = NA_character_, stringsAsFactors = FALSE)
  }
  inv <- do.call(rbind, inv_list)
  if (is.null(inv) || !nrow(inv)) stop("项目中没有可清点文件，请检查路径。")
  rownames(inv) <- NULL

  # exact-relative优先；若只有文件名匹配且有多个版本，全部列出并停止自动选择。
  targets <- data.frame(key = character(), source = character(), filename = character(),
                        relative = character(), mode = character(), stringsAsFactors = FALSE)
  target <- function(key, src, name, rel = "", mode = "header") {
    targets <<- rbind(targets, data.frame(key = key, source = src, filename = name,
                    relative = rel, mode = mode, stringsAsFactors = FALSE))
  }
  target("MIMIC_base", "MIMIC", "Step01_AMI_index_ICU_cohort_base.csv",
         "01_cohort/Step01_AMI_index_ICU_cohort_base.csv", "audit")
  for (h in c(0, 24, 48, 72)) {
    fn <- paste0("Step02_AMI_T", h, "_dataset.csv")
    target(paste0("MIMIC_T", h), "MIMIC", fn, file.path("03_analysis_dataset", fn), "audit")
  }
  for (h in c(6, 24, 48, 72)) {
    fn <- paste0("Step02_AMI_feature_long_", h, "h.csv")
    target(paste0("MIMIC_feature_summary_", h), "MIMIC", fn, file.path("03_analysis_dataset", fn))
  }
  target("MIMIC_cache", "MIMIC", "Step02_AMI_landmark_refined_figures.duckdb",
         "05_duckdb_cache/Step02_AMI_landmark_refined_figures.duckdb", "database")
  step05 <- "09_step05_T48_dynamic_prediction_model_with_C1C5"
  for (item in list(
    c("MIMIC_split", "Step05_train_test_split.csv", "analysis_datasets", "audit"),
    c("MIMIC_predictions", "Step05_T48_model_predictions_train_test.csv", "analysis_datasets", "audit"),
    c("MIMIC_model_data", "Step05_T48_analysis_dataset_with_C1C5.csv", "analysis_datasets", "audit"),
    c("MIMIC_model", "Step05_T48_model_objects_with_C1C5.rds", "objects", "object"),
    c("MIMIC_predictor_map", "Step05_model_predictor_map.csv", "tables", "header"),
    c("MIMIC_performance", "Step05_model_performance_train_test.csv", "tables", "header"))) {
    target(item[1], "MIMIC", item[2], file.path(step05, item[3], item[2]), item[4])
  }
  target("MIMIC_labels", "MIMIC", "Step03_final_k5_T48_trajectory_labels.csv",
         "07_step03_final_k5_trajectory_phenotyping/trajectory_datasets/Step03_final_k5_T48_trajectory_labels.csv", "audit")
  target("MIMIC_cluster", "MIMIC", "Step03_final_k5_trajectory_model_object.rds", mode = "object")
  for (fn in c("Step06_analysis_dataset_with_oof_predictions.csv", "Step07_analysis_dataset_used.csv")) {
    target(sub("\\.csv$", "", fn), "MIMIC", fn, mode = "audit")
  }
  for (fn in c("Step07_variable_dictionary_resolved.csv", "Step07_outcome_dictionary_resolved.csv",
               "Step07_detected_key_columns.csv", "Step08_detected_and_constructed_scores.csv",
               "Step08_performance_common_sample_TEST.csv", "Step01_precise_lab_item_map.csv",
               "Step01_precise_vital_item_map.csv")) target(sub("\\.csv$", "", fn), "MIMIC", fn)
  target("eICU_config", "eICU", "StepE0_path_config.rds", "00_admin/StepE0_path_config.rds", "object")
  for (item in list(
    c("eICU_first_stay", "StepE1_eICU_conservative_AMI_first_ICU_stay_per_patient.csv"),
    c("eICU_T48", "StepE1_eICU_AMI_T48_landmark_external_validation_cohort.csv"),
    c("eICU_T48_inICU", "StepE1_eICU_AMI_T48_inICU_sensitivity_cohort.csv"))) {
    target(item[1], "eICU", item[2], file.path("02_cohort_construction", item[2]), "audit")
  }
  for (fn in c("StepE1_AMI_diagnosis_evidence_long.csv", "StepE1_AMI_related_diagnosis_evidence_for_review.csv",
               "StepE2B_vital_signs_long_T0_T72.csv", "StepE2C_laboratory_long_T0_T72.csv",
               "StepE2C_lab_name_mapping_review.csv", "StepE2D_raw_text_proxy_records_T0_T72.csv",
               "StepE2D_QC2_support_treatment_proxy_wide_features_only.csv")) {
    target(sub("\\.csv$", "", fn), "eICU", fn)
  }
  for (item in list(
    c("eICU_demographics", "StepE2A_eICU_AMI_T48_cohort_demographics_comorbidities.csv"),
    c("eICU_model_data", "StepE5B_QC3_eICU_full_feature_table_final_model_compatible.csv"),
    c("eICU_predictions", "StepE5C_fixed_external_predictions_correct_AMI_EDRS_long.csv"),
    c("eICU_recalibrated", "StepE5D_fixed_AMI_EDRS_recalibrated_patient_level_predictions.csv"))) {
    target(item[1], "eICU", item[2], mode = "audit")
  }
  for (fn in c("StepE4_QC2_eICU_C1C5_trajectory_transfer_fixed_object.rds",
               "StepE2D_support_treatment_proxy_object.rds")) {
    target(sub("\\.rds$", "", fn), "eICU", fn, mode = "object")
  }
  for (src in c("APP", "APP_RELEASE")) {
    target(paste0(src, "_model"), src, "Step05_T48_model_objects_with_C1C5.rds", mode = "object")
  }
  selected <- stats::setNames(rep(NA_character_, nrow(targets)), targets$key)
  all_target_paths <- character()
  for (i in seq_len(nrow(targets))) {
    t <- targets[i, ]; hits <- inv[inv$source == t$source & inv$file == t$filename, , drop = FALSE]
    all_target_paths <- c(all_target_paths, hits$absolute_path)
    exact <- if (nzchar(t$relative)) which(hits$relative_path == t$relative) else integer()
    status <- if (length(exact) == 1L) "EXACT_PATH" else if (nrow(hits) == 1L) "UNIQUE_FILENAME" else if (nrow(hits) > 1L) "AMBIGUOUS" else "MISSING"
    if (status == "EXACT_PATH") selected[t$key] <- hits$absolute_path[exact]
    if (status == "UNIQUE_FILENAME") selected[t$key] <- hits$absolute_path[1]
    emit("03_expected_files", key = t$key, source = t$source, filename = t$filename,
         expected_relative = t$relative, status = status, n_matches = nrow(hits),
         selected_path = selected[[t$key]], all_matches = paste(hits$absolute_path, collapse = " | "))
    if (status %in% c("MISSING", "AMBIGUOUS")) issue("CHECK", t$key,
      if (status == "MISSING") "未找到该输出；记录缺失，不要求现在重跑旧步骤。" else "存在多个版本且无唯一标准路径；未按时间选最新。")
    if (length(exact) == 1L && nrow(hits) > 1L) issue("INFO", t$key, "存在同名副本；本次读取原脚本标准路径，其余副本仍列入清单。")
  }
  all_target_paths <- unique(all_target_paths)

  # ---------- 3. CSV字段、人数、ID、旧结局与时间字段 ----------
  use_dt <- requireNamespace("data.table", quietly = TRUE)
  read_header <- function(p) {
    if (use_dt) names(data.table::fread(p, nrows = 0L, showProgress = FALSE, check.names = FALSE))
    else names(utils::read.csv(p, nrows = 0L, check.names = FALSE, stringsAsFactors = FALSE))
  }
  read_selected <- function(p, cols, nmax) {
    if (use_dt) as.data.frame(data.table::fread(p, select = cols, nrows = nmax,
      colClasses = "character", showProgress = FALSE, check.names = FALSE))
    else {
      hdr <- read_header(p)
      cc <- ifelse(hdr %in% cols, "character", "NULL")
      utils::read.csv(p, nrows = nmax, colClasses = cc, check.names = FALSE,
                      stringsAsFactors = FALSE, comment.char = "")
    }
  }
  id_fields <- c("subject_id", "hadm_id", "stay_id", "uniquepid", "patienthealthsystemstayid", "patientunitstayid")
  time_fields <- c("admittime", "dischtime", "deathtime", "dod", "intime", "outtime", "death_time_any", "landmark_time")
  offset_fields <- c("hospitaladmitoffset", "hospitaldischargeoffset", "unitdischargeoffset")
  flag_pattern <- "(^death_|^mortality_|_mortality$|mortality_after|^primary_external_endpoint$|^secondary_icu|^hospital_expire_flag$|^icu_expire_flag$|^alive_at_|^in_icu_at_|^cad_|^prior_cad|^prior_ihd|^coronary_artery_disease$)"
  timestamp <- function(x) {
    z <- trimws(as.character(x)); z[is_missing(z)] <- NA_character_
    z <- gsub("T", " ", z, fixed = TRUE)
    z <- sub("Z$", "", z)
    # 尊重带时区的时间；不猜测纯数字的纪元单位。
    ans <- rep(as.POSIXct(NA, tz = "UTC"), length(z))
    tz_rows <- !is.na(z) & grepl("[+-][0-9]{2}:?[0-9]{2}$", z)
    if (any(tz_rows)) {
      zz <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", z[tz_rows])
      ans[tz_rows] <- as.POSIXct(strptime(zz, "%Y-%m-%d %H:%M:%OS%z", tz = "UTC"))
    }
    date_rows <- !is.na(z) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", z)
    ans[date_rows] <- as.POSIXct(z[date_rows], format = "%Y-%m-%d", tz = "UTC")
    dt_rows <- !is.na(z) & !tz_rows & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?$", z)
    ans[dt_rows] <- as.POSIXct(z[dt_rows], format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
    ans
  }
  feature_patterns <- c(lactate = "lactate", troponin = "troponin|trop_", cardiac_function = "lvef|ejection|rv_function",
    intervention = "(^|_)(pci|cabg|iabp|impella|ecmo|revasc|cath)(_|$)",
    support = "vaso|pressor|ventilat", observation = "n_measures|missing|measurement|count",
    disposition = "in_icu|discharge|outtime", comfort_care = "comfort|palliat|dnr|code_status")
  cache <- new.env(parent = emptyenv())
  schemas <- new.env(parent = emptyenv())
  completed <- character()
  for (i in seq_len(nrow(targets))) {
    t <- targets[i, ]; p <- selected[[t$key]]
    if (is.na(p) || !grepl("\\.csv$", p, ignore.case = TRUE)) next
    log_line("审计：", t$key)
    hdr <- attempt(read_header(p), t$key)
    if (is.null(hdr)) next
    schemas[[t$key]] <- hdr
    if (anyDuplicated(hdr)) { issue("CHECK", t$key, "CSV有重复列名，跳过内容审计。"); next }
    needed <- unique(c(intersect(hdr, c(id_fields, time_fields, offset_fields, "landmark_hour",
                  "days_from_landmark_to_death", "model", "split", "dataset", "y", "pred")),
                  grep(flag_pattern, hdr, value = TRUE, ignore.case = TRUE)))
    # 排除像death_time_any这样的时间列，不把它们当0/1结局。
    flags <- setdiff(grep(flag_pattern, hdr, value = TRUE, ignore.case = TRUE), time_fields)
    d <- NULL; scope <- "HEADER_ONLY"; bytes <- file.info(p)$size
    if (t$mode == "audit" && bytes <= cfg$MAX_CSV_MB * 1024^2 && length(needed)) {
      d <- attempt(read_selected(p, needed, cfg$MAX_AUDIT_ROWS + 1L), t$key)
      if (!is.null(d)) {
        scope <- if (nrow(d) > cfg$MAX_AUDIT_ROWS) "FIRST_ROWS_ONLY" else "FULL"
        if (scope != "FULL") {
          d <- d[seq_len(cfg$MAX_AUDIT_ROWS), , drop = FALSE]
          issue("CHECK", t$key, "超过行数上限；统计仅覆盖前部记录，不能代表全量。")
        }
      }
    } else if (t$mode == "audit") issue("CHECK", t$key, "文件超过读取上限或未识别到审计字段；只检查表头。")
    emit("05_table_summary", key = t$key, file = p, n_columns = length(hdr),
         scope = scope, n_rows_read = if (is.null(d)) 0L else nrow(d),
         n_rows_total = if (scope == "FULL") nrow(d) else NA_integer_)
    for (col in hdr) {
      role <- if (col %in% id_fields) "ID" else if (col %in% time_fields) "TIME" else if (col %in% offset_fields) "OFFSET_MINUTES" else if (col %in% flags) "FLAG_CANDIDATE" else "OTHER"
      nr <- if (!is.null(d) && col %in% names(d)) nrow(d) else NA_integer_
      nm <- if (!is.na(nr)) sum(is_missing(d[[col]])) else NA_integer_
      emit("04_column_dictionary", key = t$key, column = col, role = role,
           scope = if (is.na(nr)) "HEADER_ONLY" else scope, n_checked = nr, n_missing = nm)
    }
    for (group in names(feature_patterns)) {
      mm <- grep(feature_patterns[[group]], hdr, value = TRUE, ignore.case = TRUE)
      emit("10_feature_name_candidates", key = t$key, group = group,
           n_candidate_columns = length(mm), candidate_columns = paste(mm, collapse = " | "),
           note = "仅按列名筛选候选；不代表定义、单位、可用时间已验证。")
    }
    if (is.null(d)) next
    n <- nrow(d)
    for (col in intersect(id_fields, names(d))) {
      v <- d[[col]]; good <- !is_missing(v)
      emit("06_id_checks", key = t$key, column = col, scope = scope, n_rows = n,
           n_missing = sum(!good), n_unique_nonmissing = length(unique(v[good])),
           n_duplicate_nonmissing_rows = sum(duplicated(v[good])),
           note = "预测长表按模型重复ID是预期结构；不能直接解释为队列重复纳入。")
    }
    for (col in intersect(flags, names(d))) {
      v <- tolower(trimws(d[[col]])); missing <- is_missing(v)
      emit("07_existing_flag_counts", key = t$key, column = col, scope = scope, n_rows = n,
        n_zero = ntrue(v %in% c("0", "false")), n_one = ntrue(v %in% c("1", "true")),
        n_missing = sum(missing), n_other = sum(!missing & !v %in% c("0", "1", "false", "true")))
    }
    parsed <- list()
    for (col in intersect(time_fields, names(d))) {
      v <- d[[col]]; parsed[[col]] <- attempt(timestamp(v), paste(t$key, col))
      if (is.null(parsed[[col]])) next
      nf <- sum(!is_missing(v) & is.na(parsed[[col]]))
      emit("08_time_field_audit", key = t$key, column = col, scope = scope, n_rows = n,
           n_nonmissing = sum(!is_missing(v)), n_parse_failed = nf,
           n_date_only = ntrue(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", trimws(v))),
           note = "不导出个体日期；仅日期值保留精度标记，不能据此声称精确死亡时刻。")
      if (nf > 0L) issue("CHECK", paste(t$key, col), paste0(nf, "个非空时间无法按明确格式解析。"))
    }
    if (all(c("intime", "outtime") %in% names(parsed))) check(t$key, "outtime_before_intime", ntrue(parsed$outtime < parsed$intime), n)
    if (all(c("landmark_time", "intime") %in% names(parsed))) {
      dh <- as.numeric(difftime(parsed$landmark_time, parsed$intime, units = "hours"))
      if ("landmark_hour" %in% names(d)) check(t$key, "landmark_hour_mismatch", ntrue(abs(dh - finite_num(d$landmark_hour)) > 1e-6), n)
      if ("dischtime" %in% names(parsed)) check(t$key, "hospital_discharge_at_or_before_landmark", ntrue(parsed$dischtime <= parsed$landmark_time), n, "含边界病例；Step1再确定入组规则。")
      if ("death_time_any" %in% names(parsed)) {
        dd <- as.numeric(difftime(parsed$death_time_any, parsed$landmark_time, units = "days"))
        check(t$key, "recorded_death_before_landmark", ntrue(dd < 0), n)
        check(t$key, "recorded_death_exactly_at_landmark", ntrue(dd == 0), n, "日期精度需要另核查。")
        check(t$key, "recorded_death_after_day28_through_day30", ntrue(dd > 28 & dd <= 30), n,
              "时间字段诊断计数，不生成或替换正式结局。")
      }
    }
    for (col in intersect(offset_fields, names(d))) {
      v <- finite_num(d[[col]])
      check(t$key, paste0(col, "_nonmissing"), sum(is.finite(v)), n, "原eICU offset单位为分钟。")
      if (col != "hospitaladmitoffset") check(t$key, paste0(col, "_at_or_before_2880min"), ntrue(v <= 2880), n)
    }
    if (all(c("hospital_mortality", "icu_mortality") %in% names(d))) check(t$key,
      "ICU_death_with_hospital_survival", ntrue(finite_num(d$icu_mortality) == 1 & finite_num(d$hospital_mortality) == 0), n)
    if (all(c("model", "pred", "y") %in% names(d))) {
      splitcol <- if ("split" %in% names(d)) "split" else if ("dataset" %in% names(d)) "dataset" else NULL
      gp <- if (is.null(splitcol)) rep("External", n) else d[[splitcol]]
      # 模型标签可反馈；不导出任何风险值列表或患者标识。
      groups <- unique(data.frame(model = d$model, dataset = gp, stringsAsFactors = FALSE))
      if (nrow(groups) > 100L) issue("CHECK", t$key, "模型/数据集标签超过100种；跳过分组，需核对字段。") else {
        for (j in seq_len(nrow(groups))) {
          ix <- which(!is.na(d$model) & !is.na(gp) & d$model == groups$model[j] & gp == groups$dataset[j])
          pr <- finite_num(d$pred[ix]); yy <- finite_num(d$y[ix]); valid <- is.finite(pr)
          idcol <- intersect(c("stay_id", "patientunitstayid"), names(d))
          emit("15_prediction_audit", key = t$key, model = groups$model[j], dataset = groups$dataset[j], scope = scope,
            n_rows = length(ix), n_y1 = ntrue(yy == 1), n_y0 = ntrue(yy == 0), n_y_missing_or_other = sum(!yy %in% c(0, 1)),
            n_prediction_missing_or_nonfinite = sum(!valid), n_probability_outside_0_1 = ntrue(valid & (pr < 0 | pr > 1)),
            n_unique_probabilities = length(unique(pr[valid])), mean_probability = if (any(valid)) mean(pr[valid]) else NA_real_,
            duplicate_ID_rows_within_group = if (length(idcol)) sum(duplicated(d[[idcol[1]]][ix][!is_missing(d[[idcol[1]]][ix])])) else NA_integer_)
        }
      }
    }
    if (scope == "FULL") {
      cache[[t$key]] <- d[, intersect(c(id_fields, "death_30d_after_landmark", "split", "y", "model"), names(d)), drop = FALSE]
      completed <- c(completed, t$key)
    }
    rm(d); invisible(gc(FALSE))
  }

  # ---------- 4. 跨文件只比较集合与旧标签，不重新定义终点 ----------
  compare_ids <- function(ka, kb) {
    a <- cache[[ka]]; b <- cache[[kb]]
    if (is.null(a) || is.null(b) || !"stay_id" %in% names(a) || !"stay_id" %in% names(b)) return(invisible(NULL))
    ai <- unique(a$stay_id[!is_missing(a$stay_id)]); bi <- unique(b$stay_id[!is_missing(b$stay_id)])
    nm <- paste(ka, kb, sep = " vs ")
    check(nm, "unique_ID_in_both", length(intersect(ai, bi)))
    check(nm, "unique_ID_only_in_first", length(setdiff(ai, bi)))
    check(nm, "unique_ID_only_in_second", length(setdiff(bi, ai)))
    col <- "death_30d_after_landmark"
    if (col %in% names(a) && col %in% names(b) && !anyDuplicated(a$stay_id) && !anyDuplicated(b$stay_id)) {
      m <- match(a$stay_id, b$stay_id); good <- !is.na(m) & !is_missing(a$stay_id)
      x <- a[[col]][good]; y <- b[[col]][m[good]]
      check(nm, "old_endpoint_disagreement_nonmissing", ntrue(!is_missing(x) & !is_missing(y) & x != y), sum(good))
    }
  }
  compare_ids("MIMIC_base", "MIMIC_T0")
  for (other in c("MIMIC_T24", "MIMIC_T48", "MIMIC_T72")) compare_ids("MIMIC_base", other)
  for (other in c("MIMIC_split", "MIMIC_model_data", "MIMIC_labels", "MIMIC_predictions")) compare_ids("MIMIC_T48", other)
  sp <- cache[["MIMIC_split"]]
  if (!is.null(sp) && all(c("stay_id", "split") %in% names(sp))) {
    lab <- tolower(trimws(sp$split)); good <- !is_missing(sp$stay_id)
    train <- unique(sp$stay_id[which(good & lab == "train")]); test <- unique(sp$stay_id[which(good & lab == "test")])
    check("MIMIC_split", "train_test_ID_overlap", length(intersect(train, test)))
    check("MIMIC_split", "train_unique_ID", length(train)); check("MIMIC_split", "test_unique_ID", length(test))
    check("MIMIC_split", "unknown_split_label_rows", sum(!lab %in% c("train", "test")), nrow(sp))
  }

  # ---------- 5. 原始数据库：仅文件存在性及表头 ----------
  raw_candidates <- list(
    MIMIC = c("hosp/patients.csv", "hosp/admissions.csv", "hosp/diagnoses_icd.csv", "hosp/d_labitems.csv", "hosp/labevents.csv", "icu/icustays.csv", "icu/d_items.csv", "icu/chartevents.csv"),
    eICU = c("patient.csv", "diagnosis.csv", "admissionDx.csv", "lab.csv", "vitalPeriodic.csv", "vitalAperiodic.csv", "treatment.csv", "pastHistory.csv", "apacheApsVar.csv"))
  for (src in names(raw_candidates)) {
    root <- if (src == "MIMIC") cfg$MIMIC_RAW else cfg$EICU_RAW
    for (rel in raw_candidates[[src]]) {
      candidates <- file.path(root, c(paste0(rel, ".gz"), rel))
      found <- candidates[file.exists(candidates)]
      hdr <- character()
      if (length(found) == 1L) hdr <- attempt({
        con <- if (grepl("\\.gz$", found)) gzfile(found, "rt") else file(found, "rt")
        tryCatch({ first <- readLines(con, n = 1L, warn = FALSE)
          names(utils::read.csv(text = first, nrows = 0L, check.names = FALSE))
        }, finally = close(con))
      }, paste(src, rel))
      emit("16_raw_source_files", source = src, root = root, expected_file = rel,
           status = if (!length(found)) "NOT_FOUND_AT_CONFIGURED_PATH" else if (length(found) > 1L) "BOTH_GZ_AND_CSV" else "FOUND",
           found_paths = paste(found, collapse = " | "), columns = paste(hdr, collapse = " | "))
    }
  }

  # ---------- 6. DuckDB只读元数据；不COUNT大表、不查询原始文件视图 ----------
  dbfiles <- inv$absolute_path[grepl("\\.duckdb$", inv$file, ignore.case = TRUE)]
  if (cfg$INSPECT_DUCKDB && length(dbfiles)) {
    if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("duckdb", quietly = TRUE)) {
      for (p in dbfiles) {
        log_line("只读缓存结构：", basename(p))
        attempt((function(dbpath) {
          con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbpath, read_only = TRUE)
          on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
          tt <- DBI::dbGetQuery(con, "SELECT table_catalog, table_schema, table_name, table_type FROM information_schema.tables WHERE table_schema NOT IN ('information_schema', 'pg_catalog') ORDER BY table_schema, table_name")
          cc <- DBI::dbGetQuery(con, "SELECT table_catalog, table_schema, table_name, column_name, data_type, ordinal_position FROM information_schema.columns WHERE table_schema NOT IN ('information_schema', 'pg_catalog') ORDER BY table_schema, table_name, ordinal_position")
          if (nrow(tt)) for (j in seq_len(nrow(tt))) emit("11_duckdb_tables", file = dbpath, tt[j, , drop = FALSE])
          if (nrow(cc)) for (j in seq_len(nrow(cc))) emit("12_duckdb_columns", file = dbpath, cc[j, , drop = FALSE])
          TRUE
        })(p), basename(p))
      }
    } else issue("CHECK", "DuckDB", "缺DBI或duckdb，缓存结构未读取；其余审计继续。无需立即安装或重跑旧代码。")
  } else issue("INFO", "DuckDB", "没有找到缓存或已关闭缓存检查。")

  # ---------- 7. 关键旧产物备份；不读取/展开RDS，不扫描模型环境 ----------
  # 这不是整个项目的完整镜像；大文件/预算外文件明确记为未复制。
  snapshot_paths <- unique(c(unname(selected[!is.na(selected)]), all_target_paths,
    inv$absolute_path[grepl("\\.(R|Rmd|qmd|py|md|txt)$", inv$file, ignore.case = TRUE)]))
  used <- 0
  for (p in snapshot_paths) {
    k <- match(p, inv$absolute_path); if (is.na(k)) next
    bytes <- inv$bytes[k]; status <- "SKIPPED_SIZE_OR_BUDGET"; digest <- NA_character_
    dest <- file.path(private, inv$source[k], inv$relative_path[k])
    if (is.finite(bytes) && bytes <= cfg$SNAPSHOT_MAX_FILE_MB * 1024^2 &&
        used + bytes <= cfg$SNAPSHOT_TOTAL_MB * 1024^2 &&
        !grepl("\\.(duckdb|wal)$", p, ignore.case = TRUE)) {
      before <- file.info(p)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      copied <- attempt(file.copy(p, dest, overwrite = FALSE, copy.date = TRUE), basename(p))
      if (isTRUE(copied)) {
        hh <- attempt(tools::md5sum(c(p, dest)), basename(p))
        after <- file.info(p)
        stable <- isTRUE(before$size == after$size) && isTRUE(before$mtime == after$mtime)
        ok <- !is.null(hh) && !anyNA(hh) && unname(hh[1]) == unname(hh[2]) && stable
        status <- if (isTRUE(ok)) "COPIED_MD5_VERIFIED" else "COPIED_NOT_VERIFIED_OR_SOURCE_CHANGED"
        if (isTRUE(ok)) { digest <- unname(hh[1]); inv$md5[inv$absolute_path == p] <- digest }
        used <- used + bytes
        if (!isTRUE(ok)) issue("CHECK", basename(p), "备份校验失败或源文件在审计期间变化；不能据此冻结版本。")
      } else status <- "COPY_FAILED"
    }
    emit("13_snapshot_manifest", source_file = p, local_copy = if (startsWith(status, "COPIED")) dest else "",
         bytes = bytes, status = status, md5 = digest)
  }
  write_csv(inv, "02_file_inventory")

  # ---------- 8. 写出反馈；只打包to_share ----------
  for (nm in sort(ls(rows))) {
    rr <- rows[[nm]]
    if (length(rr)) write_csv(do.call(rbind, rr), nm)
  }
  utils::capture.output(sessionInfo(), file = file.path(share, "17_R_sessionInfo.txt"))
  writeLines(c("Step0配置（仅路径和运行参数）", paste0(path_keys, " = ", unlist(cfg[path_keys])),
    paste0("CSV reader = ", if (use_dt) "data.table" else "base R"),
    paste0("MAX_CSV_MB = ", cfg$MAX_CSV_MB), paste0("MAX_AUDIT_ROWS = ", cfg$MAX_AUDIT_ROWS),
    paste0("Local snapshot budget MB = ", cfg$SNAPSHOT_TOTAL_MB)),
    file.path(share, "18_run_configuration.txt"), useBytes = TRUE)
  issues <- rows[["14_issues"]]
  issue_n <- if (length(issues)) nrow(do.call(rbind, issues)) else 0L
  writeLines(c(
    "AMI-EDRS Step0 已完成文件审计流程", "",
    paste0("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    paste0("清点文件数：", nrow(inv)), paste0("完整读取审计字段的表数：", length(completed)),
    paste0("待核对/信息记录数：", issue_n), "",
    "本轮仅核查旧文件、字段、样本数、旧结局计数和时间字段；没有重训、改标签或修订正式结果。",
    "FULL表示该表所有行均已读取所需审计字段；HEADER_ONLY仅检查字段；FIRST_ROWS_ONLY不能解释为全量统计。",
    "03_expected_files.csv列出缺失、多版本及实际选择路径；没有按修改日期猜选最新文件。",
    "文件时间和MD5仅用于版本识别，不自动证明该版本就是投稿版本。",
    "07_existing_flag_counts.csv是原有标签计数；09_consistency_checks.csv仅提供诊断计数。",
    "Step02的feature_long_6h/24h/48h/72h是窗口汇总，不等于原始带时间戳的逐次测量长表。",
    "原始测量是否可复用，请结合DuckDB中feature_long_72h_clean等表的字段判定。",
    "模型RDS仅清点和按预算备份，未展开其公式环境或输出患者值。",
    "local_only/legacy_snapshot是有限预算的关键文件备份，不是整个旧项目的完整镜像。请继续保留旧项目原目录。",
    "to_share/仅含文件路径、字段名、总体计数及运行信息，不含患者ID值、患者逐行表或模型对象。",
    "缺eICU、移动硬盘、某些中间文件或DuckDB依赖时，可先将本次结果回传，不要据此自行重跑旧分析。", "",
    "下一步：把Step0_feedback_*.zip发回；若ZIP未生成，压缩整个to_share文件夹后发回。",
    "不需要上传local_only，也不需要上传原始数据库。"
  ), file.path(share, "00_READ_ME_FIRST.txt"), useBytes = TRUE)
  log_line("审计结束；待核对事项请见14_issues.csv。")
  writeLines(c("AUDIT_FINISHED", "含CHECK不代表流程失败；它表示需要根据实际文件进一步判断。"),
             file.path(share, "99_AUDIT_FINISHED.txt"), useBytes = TRUE)
  zip_path <- file.path(out, paste0("Step0_feedback_", stamp, ".zip"))
  zip_ok <- tryCatch((function() {
    oldwd <- getwd(); on.exit(setwd(oldwd), add = TRUE); setwd(out)
    fs <- list.files("to_share", recursive = TRUE, full.names = TRUE)
    if (requireNamespace("zip", quietly = TRUE)) {
      zip::zipr(zip_path, files = fs, root = out)
    } else if (nzchar(Sys.which("zip"))) {
      utils::zip(zipfile = zip_path, files = fs, flags = "-q")
    } else return(FALSE)
    file.exists(zip_path) && file.info(zip_path)$size > 0
  })(), error = function(e) FALSE)
  cat("\n================ Step0 完成 ================\n")
  cat("请发回：\n", if (isTRUE(zip_ok)) zip_path else share, "\n", sep = "")
  if (!isTRUE(zip_ok)) cat("ZIP未自动生成：在Finder中右键压缩to_share文件夹后发回即可。\n")
  cat("原始文件未改写。此次不生成新的分析结论。\n")
  invisible(list(output_dir = out, feedback = if (isTRUE(zip_ok)) zip_path else share))
})
