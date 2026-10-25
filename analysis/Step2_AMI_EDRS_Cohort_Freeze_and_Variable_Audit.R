# AMI-EDRS revision | Step2: cohort freeze and variable audit
# 运行：source(file.choose(), encoding = "UTF-8")
# 需要已成功运行Step1-1。固定输出：修改/Step2；重跑覆盖本步骤已登记结果。
# 原始项目与Step1只读；患者逐行数据仅保存在local_only，反馈ZIP只含汇总。
# 本步冻结纳入/结局规则，核查变量来源与单位；不训练、插补或合并同名指标。
local({
  cfg <- list(
    MIMIC_PROJECT = Sys.getenv("AMI_MIMIC_PROJECT", unset=file.path(getwd(), "work/MIMIC")),
    EICU_PROJECT = Sys.getenv("AMI_EICU_PROJECT", unset=file.path(getwd(), "work/eICU")),
    OUTPUT_PARENT = Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    AUDIT_MEASUREMENTS = TRUE, MAKE_ZIP = TRUE
  )
  cfg <- utils::modifyList(cfg, getOption("ami_step2.config", list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in c("MIMIC_PROJECT", "EICU_PROJECT", "OUTPUT_PARENT")) cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step2")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step2输出目录。")
  if (is_link(out)) stop("Step2输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP2")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step2目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP2_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP2_MANAGED_OUTPUT")) {
    stop("Step2输出目录的归属标记不正确。")
  }
  safe_path <- function(rel) {
    if (length(rel) != 1L || is.na(rel) || !nzchar(rel) ||
        grepl("(^/|^[A-Za-z]:|\\\\|(^|/)\\.\\.(/|$))", rel)) stop("输出路径越界。")
    p <- file.path(out, rel)
    par <- dirname(p)
    while (nchar(par) >= nchar(out)) {
      if (is_link(par)) stop("输出目录内存在符号链接，停止清理。")
      if (par == out) break
      par <- dirname(par)
    }
    if (is_link(p)) stop("输出文件为符号链接，停止覆盖。")
    p
  }
  # 登记过的文件才会删除。不会遍历清空修改目录、旧项目或用户另存的文件。
  if (file.exists(manifest)) {
    old <- unique(readLines(manifest, warn = FALSE))
    for (rel in old[nzchar(old)]) {
      p <- safe_path(rel)
      if (file.exists(p)) {
        if (dir.exists(p)) stop("登记的输出应为文件，不能为目录。")
        if (unlink(p) != 0L) stop("不能覆盖旧Step2输出；请关闭正在使用它的程序。")
      }
    }
  }
  writeLines(character(), manifest)
  registered <- character()
  reg <- function(rel) {
    p <- safe_path(rel)
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    if (!(rel %in% registered)) {
      registered <<- c(registered, rel)
      writeLines(registered, manifest)
    }
    p
  }
  csv_out <- function(d, rel) utils::write.csv(d, reg(rel), row.names = FALSE, na = "", fileEncoding = "UTF-8")
  txt_out <- function(x, rel) writeLines(enc2utf8(x), reg(rel), useBytes = TRUE)
  logp <- reg("to_share/Step2_run_log.txt")
  logmsg <- function(s) { cat(s, "\n"); cat(s, "\n", file = logp, append = TRUE) }
  agg <- new.env(parent = emptyenv())
  emit <- function(.table_name, ...) {
    x <- data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
    agg[[.table_name]] <- c(agg[[.table_name]], list(x))
  }
  issue <- function(severity, context, message) emit("09_issues", severity = severity, context = context, message = message)
  metric <- function(source, landmark, metric, n, denominator = NA_real_) {
    emit("02_checks", source = source, landmark = landmark, metric = metric,
         n = as.numeric(n), denominator = as.numeric(denominator))
  }
  ntrue <- function(x) sum(x %in% TRUE)
  yes <- function(x) x %in% TRUE
  missing <- function(x) is.na(x) | tolower(trimws(as.character(x))) %in% c("", "na", "nan", "null")
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  bit <- function(x) { y <- num(x); y[!(y %in% c(0, 1))] <- NA_real_; y }
  read_csv <- function(p, required = character(), optional = FALSE) {
    if (!file.exists(p)) {
      if (optional) { issue("CHECK", basename(p), "文件缺失；相关补充核查未完成。"); return(NULL) }
      stop("缺少必要输入：", p)
    }
    d <- if (requireNamespace("data.table", quietly = TRUE)) {
      as.data.frame(data.table::fread(p, colClasses = "character", na.strings = c("", "NA"),
                                    showProgress = FALSE, check.names = FALSE))
    } else utils::read.csv(p, colClasses = "character", check.names = FALSE, stringsAsFactors = FALSE,
                          na.strings = c("", "NA"), fileEncoding = "UTF-8")
    if (anyDuplicated(names(d))) stop("输入存在重复列名：", basename(p))
    absent <- setdiff(required, names(d))
    if (length(absent)) stop("输入缺少字段：", basename(p), " / ", paste(absent, collapse = ", "))
    emit("01_inputs", file = p, n_rows = nrow(d), n_columns = ncol(d),
         md5 = unname(tools::md5sum(p)))
    d
  }
  assert_unique <- function(d, keys, label) {
    for (k in keys) {
      if (any(missing(d[[k]])) || anyDuplicated(d[[k]])) stop(label, "的", k, "有缺失或重复；不自动去重。")
    }
  }
  parse_time <- function(x, label) {
    z <- trimws(as.character(x)); z[missing(z)] <- NA_character_
    z <- sub("Z$|[+]00:00$", "", z); z <- sub("T", " ", z, fixed = TRUE)
    valid <- is.na(z) | grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}([.][0-9]+)?$", z)
    ans <- suppressWarnings(as.POSIXct(z, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))
    if (any(!valid | (!is.na(z) & is.na(ans)))) stop(label, "存在不能解析的时间；未把异常当缺失。")
    as.numeric(ans)
  }
  parse_day <- function(x, label) {
    z <- trimws(as.character(x)); z[missing(z)] <- NA_character_
    valid <- is.na(z) | grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}($|[ T]00:00:00(Z)?)", z)
    ans <- suppressWarnings(as.Date(substr(z, 1, 10), format = "%Y-%m-%d"))
    if (any(!valid | (!is.na(z) & is.na(ans)))) stop(label, "存在不能解析的死亡日期。")
    as.numeric(ans) * 86400
  }
  time_text <- function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  grouped_counts <- function(name, d, groups) {
    if (!nrow(d)) return(invisible(NULL))
    dd <- d[groups]
    for (k in groups) { dd[[k]] <- as.character(dd[[k]]); dd[[k]][is.na(dd[[k]])] <- "UNKNOWN" }
    counts <- aggregate(rep(1L, nrow(dd)), by = dd, FUN = sum)
    names(counts)[ncol(counts)] <- "n"
    agg[[name]] <- c(agg[[name]], list(counts))
  }
  as_flag <- function(x) tolower(trimws(as.character(x))) %in% c("true", "1")
  same_values <- function(x, y, label) {
    if (length(x) != length(y) || any(is.na(x) != is.na(y)) ||
        any(x[!is.na(x)] != y[!is.na(y)])) stop(label, "与Step1-1不一致。")
  }
  align <- function(d, ids, key, label) {
    assert_unique(d, key, label)
    j <- match(ids, d[[key]])
    if (anyNA(j)) stop(label, "缺少对应ID。")
    d[j, , drop = FALSE]
  }
  step1 <- file.path(cfg$OUTPUT_PARENT, "Step1")
  r1 <- function(rel, columns) read_csv(file.path(step1, rel), columns)
  # 明确冻结的规则：起点为ICU入科；不按结局比例或模型表现调节纳入。
  # 仅行政时间先后异常不直接排除，但仍要求landmark时活着且记录显示仍在院。
  # 死亡状态矛盾、核心时间无效、未知结局继续排除；原始时间绝不改写。
  summary_policy <- function(source, d, id, endpoint, expected, policy) {
    for (lab in c("T6", "T24", "T48")) {
      z <- d[d$landmark == lab, , drop = FALSE]
      ok <- z$analysis_eligible %in% 1
      cohort <- z$cohort_eligible %in% 1
      yy <- z[[endpoint]]
      a <- c(sum(cohort), sum(ok), sum(yy[ok]), sum(yy[ok] == 0), sum(cohort & is.na(yy)))
      ex <- expected[expected$source == source & expected$landmark == lab &
                     expected$policy == policy, , drop = FALSE]
      if (nrow(ex) != 1L) stop("缺少唯一的Step1-1情景汇总。")
      b <- num(unlist(ex[c("eligible_n", "known_endpoint_n", "events", "non_events", "unresolved_endpoint_n")]))
      same_values(a, b, paste(source, lab, "冻结人数"))
      stopifnot(nrow(z) > 0, !anyDuplicated(z[[id]]), all(yy[ok] %in% c(0, 1)))
      emit("03_frozen_cohort_summary", source = source, landmark = lab,
           role = if (source == "eICU" && lab != "T48") "TIMING_ONLY_NOT_EXTERNAL_VALIDATION" else "PLANNED_ANALYSIS",
           base_n = nrow(z), eligible_n = a[1], analyzable_n = a[2],
           events = a[3], non_events = a[4], endpoint_unresolved_n = a[5],
           strict_sensitivity_n = sum(z$strict_sensitivity_eligible %in% 1),
           strict_sensitivity_events = sum(yy[z$strict_sensitivity_eligible %in% 1]),
           inside_index_ICU_n = sum(ok & z$in_index_icu_at_landmark %in% 1),
           outside_index_ICU_n = sum(ok & z$in_index_icu_at_landmark %in% 0),
           ICU_status_unknown_n = sum(ok & is.na(z$in_index_icu_at_landmark)))
      grouped_counts("04_exclusive_flow", data.frame(source = source, landmark = lab,
        reason = z$eligibility_reason), c("source", "landmark", "reason"))
    }
  }
  audit_measurements <- function(m, e) {
    if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("duckdb", quietly = TRUE))
      stop("逐次测量核查需要已安装的DBI与duckdb；本脚本不会自动安装包。")
    logmsg("审核逐次测量：只读MIMIC缓存；分批扫描eICU文件，反馈仅保存汇总。")
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    spill <- NULL
    on.exit({
      try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
      if (!is.null(spill)) unlink(spill, recursive = TRUE)
    }, add = TRUE)
    q <- function(x) as.character(DBI::dbQuoteString(con, x))
    run <- function(sql) DBI::dbExecute(con, sql)
    get <- function(sql) DBI::dbGetQuery(con, sql)
    spill <- tempfile("AMI_EDRS_Step2_", tmpdir = tempdir())
    dir.create(spill)
    run(paste("SET temp_directory =", q(spill)))
    run("SET memory_limit = '2GB'")
    run("SET threads = 2")
    cc <- rbind(
      data.frame(source = "MIMIC", landmark = m$landmark[m$analysis_eligible %in% 1],
        cutoff_hour = m$landmark_hour[m$analysis_eligible %in% 1], stay_id = m$stay_id[m$analysis_eligible %in% 1]),
      data.frame(source = "eICU", landmark = e$landmark[e$analysis_eligible %in% 1 & e$landmark == "T48"],
        cutoff_hour = 48, stay_id = e$patientunitstayid[e$analysis_eligible %in% 1 & e$landmark == "T48"]))
    cc$stay_id <- as.character(cc$stay_id)
    DBI::dbWriteTable(con, "cohorts", cc, overwrite = TRUE)
    cache <- file.path(cfg$MIMIC_PROJECT, "05_duckdb_cache/Step02_AMI_landmark_refined_figures.duckdb")
    if (!file.exists(cache)) stop("缺少MIMIC逐次测量缓存。")
    run(paste("ATTACH", q(cache), "AS mimic_cache (READ_ONLY)"))
    views <- list(MIMIC = list(source = "MIMIC",
      sql = "SELECT CAST(stay_id AS VARCHAR) AS stay_id, CAST(variable AS VARCHAR) AS variable,
        CAST(domain AS VARCHAR) AS measurement_source, CAST(variable AS VARCHAR) AS raw_name,
        'UNIT_NOT_RETAINED_IN_CACHE' AS raw_unit, TRY_CAST(hours_from_icu AS DOUBLE) AS hour,
        TRY_CAST(hours_from_icu AS DOUBLE) AS supplied_hour, TRY_CAST(valuenum AS DOUBLE) AS value
        FROM mimic_cache.feature_long_72h_clean"))
    vf <- file.path(cfg$EICU_PROJECT, "03_feature_extraction/E2B_vital_signs/StepE2B_vital_signs_long_T0_T72.csv")
    lf <- file.path(cfg$EICU_PROJECT, "03_feature_extraction/E2C_laboratory/StepE2C_laboratory_long_T0_T72.csv")
    for (p in c(vf, lf)) if (!file.exists(p)) stop("缺少逐次测量文件：", basename(p))
    csv_sql <- function(p) paste0("read_csv(", q(p), ", header=true, all_varchar=true, ignore_errors=false)")
    views$eICU_vital <- list(source = "eICU", sql = paste0(
      "SELECT CAST(patientunitstayid AS VARCHAR) AS stay_id, CAST(variable AS VARCHAR) AS variable,
       CAST(source AS VARCHAR) AS measurement_source, CAST(raw_variable AS VARCHAR) AS raw_name,
       'UNIT_NOT_RETAINED_IN_VITAL_FILE' AS raw_unit, TRY_CAST(offset_min AS DOUBLE)/60.0 AS hour,
       TRY_CAST(offset_hour AS DOUBLE) AS supplied_hour, TRY_CAST(value AS DOUBLE) AS value FROM ", csv_sql(vf)))
    views$eICU_lab <- list(source = "eICU", sql = paste0(
      "SELECT CAST(patientunitstayid AS VARCHAR) AS stay_id, CAST(variable AS VARCHAR) AS variable,
       CAST(source AS VARCHAR) AS measurement_source, CAST(lab_name AS VARCHAR) AS raw_name,
       COALESCE(NULLIF(TRIM(lab_unit),''),'UNIT_MISSING') AS raw_unit,
       TRY_CAST(offset_min AS DOUBLE)/60.0 AS hour, TRY_CAST(offset_hour AS DOUBLE) AS supplied_hour,
       TRY_CAST(value AS DOUBLE) AS value FROM ", csv_sql(lf)))
    for (block in names(views)) {
      v <- views[[block]]
      logmsg(paste0("测量审核：", block))
      # 先按已冻结的ID筛选；不将大CSV整表读入R。临时表/溢出文件在结束时清理。
      run("DROP TABLE IF EXISTS audit_raw")
      run(paste0("CREATE TEMP TABLE audit_raw AS SELECT r.* FROM (", v$sql, ") r
         INNER JOIN (SELECT DISTINCT stay_id FROM cohorts WHERE source=", q(v$source), ") c USING(stay_id)"))
      quality <- get("SELECT COUNT(*) AS rows_in_selected_IDs,
        COUNT(DISTINCT stay_id) AS stays_with_any_row,
        SUM(CASE WHEN hour IS NULL OR NOT isfinite(hour) THEN 1 ELSE 0 END) AS invalid_hour_rows,
        SUM(CASE WHEN value IS NULL OR NOT isfinite(value) THEN 1 ELSE 0 END) AS invalid_value_rows,
        SUM(CASE WHEN hour < 0 THEN 1 ELSE 0 END) AS negative_hour_rows,
        SUM(CASE WHEN hour > 72 THEN 1 ELSE 0 END) AS after_72h_rows,
        SUM(CASE WHEN isfinite(hour) AND isfinite(supplied_hour) AND ABS(hour-supplied_hour)>0.000001
                 THEN 1 ELSE 0 END) AS offset_min_hour_disagreement_rows FROM audit_raw")
      quality$source <- v$source; quality$block <- block
      csv_out(quality, paste0("to_share/06_measurement_QC_", block, ".csv"))
      if (isTRUE(quality$offset_min_hour_disagreement_rows > 0))
        issue("CHECK", block, "offset_min与offset_hour不完全一致；本步以原始分钟offset为准，具体数量见06表。")
      run("DROP TABLE IF EXISTS audit_valid")
      run(paste0("CREATE TEMP TABLE audit_valid AS
        SELECT c.landmark,r.* FROM audit_raw r INNER JOIN cohorts c ON r.stay_id=c.stay_id
        WHERE c.source=", q(v$source), " AND isfinite(r.hour) AND r.hour>=0 AND r.hour<=c.cutoff_hour
        AND isfinite(r.value) AND r.variable IS NOT NULL AND TRIM(r.variable)<>''"))
      avail <- get(paste0("WITH catalog AS (SELECT DISTINCT variable FROM audit_raw
          WHERE variable IS NOT NULL AND TRIM(variable)<>''),
        denominators AS (SELECT landmark,COUNT(*) AS cohort_n FROM cohorts WHERE source=", q(v$source),
        " GROUP BY landmark),
        measured AS (SELECT landmark,variable,COUNT(*) AS measurement_rows,
          COUNT(DISTINCT stay_id) AS measured_patients FROM audit_valid GROUP BY landmark,variable)
        SELECT d.landmark,v.variable,d.cohort_n,COALESCE(a.measured_patients,0) AS measured_patients,
          COALESCE(a.measurement_rows,0) AS measurement_rows,
          1.0*COALESCE(a.measured_patients,0)/NULLIF(d.cohort_n,0) AS availability
        FROM denominators d CROSS JOIN catalog v LEFT JOIN measured a
          ON d.landmark=a.landmark AND v.variable=a.variable ORDER BY d.landmark,v.variable"))
      avail$source <- v$source; avail$block <- block
      csv_out(avail, paste0("to_share/07_availability_", block, ".csv"))
      profile <- get("SELECT landmark,variable,measurement_source,raw_name,raw_unit,
        COUNT(*) AS measurement_rows,COUNT(DISTINCT stay_id) AS measured_patients,
        MIN(hour) AS min_hour,MAX(hour) AS max_hour,MIN(value) AS min_value,
        quantile_cont(value,0.01) AS p01,quantile_cont(value,0.25) AS p25,
        quantile_cont(value,0.5) AS median,quantile_cont(value,0.75) AS p75,
        quantile_cont(value,0.99) AS p99,MAX(value) AS max_value
        FROM audit_valid GROUP BY landmark,variable,measurement_source,raw_name,raw_unit
        ORDER BY landmark,variable,measurement_source,raw_name,raw_unit")
      profile$source <- v$source; profile$block <- block
      profile$unit_harmonized <- FALSE
      csv_out(profile, paste0("to_share/07_value_unit_profile_", block, ".csv"))
    }
    issue("PENDING", "unit harmonization",
      "单位与同名变量尚未自动统一；需审阅本次字典和分布。缓存已受旧清洗规则影响，不能据此证明原始记录完整。")
  }
  status <- "FAILED"
  current_module <- "input verification"
  main <- function() {
    logmsg("核对Step1-1完成状态和原始输入指纹。")
    sf <- file.path(step1, "to_share/Step1_STATUS.txt")
    if (!file.exists(sf)) stop("缺少Step1状态文件，请先运行Step1-1。")
    ss <- readLines(sf, warn = FALSE)
    if (!all(c("STATUS = COMPLETED_FOR_REVIEW", "SCRIPT_VERSION = Step1-1") %in% ss))
      stop("本脚本要求成功完成的Step1-1。")
    inp <- r1("to_share/01_inputs.csv", c("file", "md5"))
    # 原文件变化后必须重跑Step1-1，不能让旧的逐行判定套到新的原始数据。
    for (i in seq_len(nrow(inp))) {
      p <- inp$file[i]
      if (!file.exists(p) || missing(inp$md5[i]) ||
          unname(tools::md5sum(p)) != inp$md5[i]) stop("Step1输入已变更或不可访问：", basename(p))
    }
    expected <- r1("to_share/12_policy_comparison_NOT_FINAL.csv",
      c("source", "landmark", "policy", "eligible_n", "known_endpoint_n", "events", "non_events", "unresolved_endpoint_n"))
    current_module <<- "MIMIC cohort freeze"
    mr <- r1("local_only/MIMIC_landmark_roster.csv",
      c("stay_id", "landmark", "analysis_eligible_30d", "QC_only_eligible_if_admin_clock_flags_only",
        "QC_only_event_if_admin_clock_flags_only", "death_interval_start", "death_interval_end",
        "landmark_time", "endpoint_30d_time", "death_precision"))
    mi <- r1("local_only/MIMIC_time_issue_details.csv",
      c("stay_id", "death_after_hospital_discharge", "hospital_admission_after_index_ICU_admission"))
    mb <- read_csv(file.path(cfg$MIMIC_PROJECT, "01_cohort/Step01_AMI_index_ICU_cohort_base.csv"),
      c("stay_id", "subject_id", "hadm_id", "hospital_expire_flag"))
    mb <- align(mb, mr$stay_id, "stay_id", "MIMIC基础队列")
    mi <- align(mi, mr$stay_id, "stay_id", "MIMIC时间标记")
    ok <- bit(mr$QC_only_eligible_if_admin_clock_flags_only) %in% 1
    yy <- bit(mr$QC_only_event_if_admin_clock_flags_only)
    yy[!ok] <- NA_integer_
    eligible <- ok & !is.na(yy)
    # 只采用明示的新列；不携带旧的严格排除版NA结局或旧随访长度。
    keep <- c("subject_id", "hadm_id", "stay_id", "landmark", "landmark_hour", "age",
              "intime", "outtime", "dischtime", "deathtime", "dod", "landmark_time",
              "endpoint_30d_time", "death_precision", "death_interval_start", "death_interval_end",
              "alive_at_landmark", "in_hospital_at_landmark", "in_index_icu_at_landmark",
              "administrative_coverage_floor", "coverage_reaches_30d", "deathtime_dod_date_disagreement")
    if (length(setdiff(keep, names(mr)))) stop("MIMIC Step1逐行字段不完整。")
    m <- mr[keep]
    for (k in c("landmark_hour", "age", "alive_at_landmark", "in_hospital_at_landmark",
                "in_index_icu_at_landmark", "coverage_reaches_30d")) m[[k]] <- num(m[[k]])
    lm <- parse_time(m$landmark_time, "landmark")
    cut <- parse_time(m$endpoint_30d_time, "30d cutoff")
    lo <- parse_time(m$death_interval_start, "death interval lower")
    hi <- parse_time(m$death_interval_end, "death interval upper")
    date_only <- m$death_precision == "date_only_interval"
    midpoint <- ifelse(date_only, (lo + hi) / 2, lo)
    m$cohort_eligible <- as.integer(ok)
    m$analysis_eligible <- as.integer(eligible)
    m$death_30d_after_landmark <- yy
    m$death_in_index_hospital_after_landmark <- ifelse(ok, bit(mb$hospital_expire_flag), NA_integer_)
    m$analysis_eligible_hospital_endpoint <- as.integer(ok & !is.na(m$death_in_index_hospital_after_landmark))
    m$strict_sensitivity_eligible <- as.integer(eligible & bit(mr$analysis_eligible_30d) %in% 1)
    m$flag_death_after_hospital_discharge <- as.integer(as_flag(mi$death_after_hospital_discharge))
    m$flag_hospital_admission_after_ICU_admission <- as.integer(as_flag(mi$hospital_admission_after_index_ICU_admission))
    m$flag_ICU_discharge_after_hospital_discharge <- as.integer(as_flag(mi$index_ICU_discharge_after_hospital_discharge))
    m$endpoint_boundary_ambiguous <- as.integer(ok & date_only & !is.na(lo) & lo <= cut & hi > cut)
    amb <- m$endpoint_boundary_ambiguous %in% 1
    m$death_30d_lower_bound <- m$death_30d_upper_bound <- yy
    m$death_30d_lower_bound[amb] <- 0L
    m$death_30d_upper_bound[amb] <- 1L
    m$death_30d_midday_sensitivity <- yy
    m$death_30d_midday_sensitivity[amb] <- as.integer(midpoint[amb] <= cut[amb])
    ev <- eligible & yy %in% 1
    m$followup_time_days_midpoint_if_date_only <- ifelse(eligible & yy %in% 0, 30, NA_real_)
    m$followup_time_days_midpoint_if_date_only[ev] <- (midpoint[ev] - lm[ev]) / 86400
    m$event_time_lower_days <- m$event_time_upper_days <- NA_real_
    m$event_time_lower_days[ev] <- (lo[ev] - lm[ev]) / 86400
    m$event_time_upper_days[ev] <- (hi[ev] - lm[ev]) / 86400
    m$event_time_is_approximate <- as.integer(ev & date_only)
    m$eligibility_reason <- ifelse(eligible, "eligible",
                                  ifelse(ok, "30d_endpoint_unresolved", mr$eligibility_reason))
    m$cohort_policy <- "ICU_ORIGIN_ADMIN_CLOCK_FLAGS_RETAINED"
    if (anyNA(m$followup_time_days_midpoint_if_date_only[eligible]) ||
        any(m$followup_time_days_midpoint_if_date_only[eligible] <= 0 |
            m$followup_time_days_midpoint_if_date_only[eligible] > 30)) stop("MIMIC随访长度异常。")
    if (any(ok & m$coverage_reaches_30d != 1)) stop("MIMIC观察窗覆盖未确认。")
    summary_policy("MIMIC", m, "stay_id", "death_30d_after_landmark", expected, "BOTH_ADMIN_CLOCK_FLAGS_ONLY")
    csv_out(m, "local_only/MIMIC_landmark_cohorts.csv")

    current_module <<- "eICU cohort freeze"
    er <- r1("local_only/eICU_landmark_timing_audit.csv",
      c("patientunitstayid", "landmark", "outcome_resolved", "QC_only_eligible_if_admission_offset_flag_only",
        "QC_only_hospital_event_if_admission_offset_flag_only"))
    ef <- r1("local_only/eICU_AMI_evidence_flags.csv",
      c("patientunitstayid", "specific_acute_MI_text_in_conservative_sources",
        "specific_acute_MI_text_documented_by_T48", "nstemi_evidence", "stemi_evidence"))
    es <- r1("local_only/eICU_selection_issue_details.csv",
      c("patientunitstayid", "n_eligible_hospital_encounters", "is_unique_earliest_known_AMI_unit_in_selected_hospital"))
    ei <- r1("local_only/eICU_time_status_issue_details.csv",
      c("patientunitstayid", "hospitaladmitoffset"))
    ep <- file.path(cfg$EICU_PROJECT, "02_cohort_construction")
    eb <- read_csv(file.path(ep, "StepE1_eICU_conservative_AMI_first_ICU_stay_per_patient.csv"),
      c("patientunitstayid", "uniquepid"))
    allstay <- read_csv(file.path(ep, "StepE1_eICU_conservative_AMI_adult_all_ICU_stays.csv"),
      c("patientunitstayid", "uniquepid", "unitvisitnumber", "hospitaladmitoffset"))
    assert_unique(eb, c("patientunitstayid", "uniquepid"), "eICU一人一次选例")
    assert_unique(allstay, "patientunitstayid", "eICU全部候选")
    if (any(missing(allstay$uniquepid))) stop("eICU候选患者标识缺失，不能复核一人一次选例。")
    uv <- num(allstay$unitvisitnumber); uv[is.na(uv)] <- 9999
    ha <- num(allstay$hospitaladmitoffset); ha[is.na(ha)] <- 999999
    sid <- num(allstay$patientunitstayid)
    if (anyNA(sid)) stop("eICU unit ID不是预期的数字标识。")
    oo <- order(allstay$uniquepid, uv, ha, sid, method = "radix")
    chosen <- allstay[oo, , drop = FALSE]
    chosen <- chosen[!duplicated(chosen$uniquepid), , drop = FALSE]
    chosen <- align(chosen, eb$uniquepid, "uniquepid", "eICU固定选例规则")
    same_values(chosen$patientunitstayid, eb$patientunitstayid, "eICU确定性选例")
    ef0 <- align(ef, eb$patientunitstayid, "patientunitstayid", "eICU AMI证据")
    if (!all(as_flag(ef0$specific_acute_MI_text_in_conservative_sources)))
      stop("存在未确认特异急性心梗证据的选例，不能按本次审核结论冻结。")
    ef <- align(ef, er$patientunitstayid, "patientunitstayid", "eICU AMI证据")
    es <- align(es, er$patientunitstayid, "patientunitstayid", "eICU选例标记")
    ei <- align(ei, er$patientunitstayid, "patientunitstayid", "eICU时间标记")
    earliest <- tolower(es$is_unique_earliest_known_AMI_unit_in_selected_hospital)
    if (any(earliest %in% c("false", "0")))
      stop("出现同一住院内已知不是最早AMI ICU段的选例，须重新审核。")
    fields <- c("patientunitstayid", "uniquepid", "patienthealthsystemstayid", "landmark", "landmark_hour",
                "hospitaldischargeoffset", "unitdischargeoffset", "hospital_status_death",
                "unit_status_death", "in_index_icu_at_landmark", "conflict_flag")
    if (length(setdiff(fields, names(er)))) stop("eICU Step1逐行字段不完整。")
    e <- er[fields]
    for (k in setdiff(fields, c("patientunitstayid", "uniquepid", "patienthealthsystemstayid", "landmark")))
      e[[k]] <- num(e[[k]])
    ok <- bit(er$QC_only_eligible_if_admission_offset_flag_only) %in% 1
    yy <- bit(er$QC_only_hospital_event_if_admission_offset_flag_only)
    yy[!ok] <- NA_integer_
    e$cohort_eligible <- as.integer(ok)
    e$analysis_eligible <- as.integer(ok & !is.na(yy))
    e$hospital_death_after_landmark <- yy
    e$strict_sensitivity_eligible <- as.integer(ok & !is.na(yy) & bit(er$outcome_resolved) %in% 1)
    e$hospitaladmitoffset <- num(ei$hospitaladmitoffset)
    e$flag_positive_hospitaladmitoffset <- as.integer(!is.na(e$hospitaladmitoffset) & e$hospitaladmitoffset > 0)
    e$n_eligible_hospital_encounters <- num(es$n_eligible_hospital_encounters)
    e$flag_multiple_hospital_encounters <- as.integer(e$n_eligible_hospital_encounters > 1)
    e$flag_within_hospital_order_unresolved <- as.integer(!(earliest %in% c("true", "1")))
    e$acute_MI_evidence_documented_by_T48 <- as.integer(as_flag(ef$specific_acute_MI_text_documented_by_T48))
    e$AMI_subtype_from_explicit_text <- ifelse(as_flag(ef$nstemi_evidence) & as_flag(ef$stemi_evidence),
       "mixed_or_conflicting", ifelse(as_flag(ef$nstemi_evidence), "NSTEMI",
          ifelse(as_flag(ef$stemi_evidence), "STEMI", "AMI_unspecified")))
    e$eligibility_reason <- ifelse(e$analysis_eligible %in% 1, "eligible",
      ifelse(ok, "hospital_outcome_unknown", er$eligibility_reason))
    e$cohort_policy <- "ICU_ORIGIN_ADMISSION_OFFSET_FLAG_RETAINED"
    e$selection_policy <- "ONE_PER_PERSON_FIXED_LEGACY_SORT_NOT_CHRONOLOGICAL_FIRST"
    summary_policy("eICU", e, "patientunitstayid", "hospital_death_after_landmark", expected,
                   "POSITIVE_ADMISSION_OFFSET_FLAG_ONLY")
    for (h in c(6, 24, 48)) {
      z <- e[e$landmark_hour == h & e$analysis_eligible %in% 1, , drop = FALSE]
      metric("eICU", paste0("T", h), "multiple_eligible_hospital_encounters",
             sum(z$flag_multiple_hospital_encounters %in% 1), nrow(z))
      metric("eICU", paste0("T", h), "within_hospital_order_unresolved",
             sum(z$flag_within_hospital_order_unresolved %in% 1), nrow(z))
      metric("eICU", paste0("T", h), "AMI_evidence_not_known_documented_by_T48",
             sum(!z$acute_MI_evidence_documented_by_T48), nrow(z))
    }
    csv_out(e, "local_only/eICU_landmark_cohorts.csv")
    # 旧逐次eICU测量只覆盖原T48队列。本步仅在T48审核；不从此子集构造T6/T24外部验证。
    olde <- read_csv(file.path(ep, "StepE1_eICU_AMI_T48_landmark_external_validation_cohort.csv"), "patientunitstayid")
    if (any(!e$patientunitstayid[e$landmark == "T48" & e$analysis_eligible %in% 1] %in% olde$patientunitstayid))
      stop("冻结的eICU T48超出旧特征抽取人群，需要重新抽取原始记录。")
    logmsg("队列规则已固定并与Step1-1情景汇总核对一致。")

    current_module <<- "variable dictionaries"
    # 以下是数据字典，不含患者逐行数据。仅允许明确的字典列进入反馈。
    dictionaries <- list(
      MIMIC_lab = list(path = file.path(cfg$MIMIC_PROJECT, "02_diagnosis_and_variable_maps/Step01_precise_lab_item_map.csv"),
        cols = c("domain", "variable", "itemid", "preferred_label", "scientific_module", "mimic_label", "fluid", "category")),
      MIMIC_vital = list(path = file.path(cfg$MIMIC_PROJECT, "02_diagnosis_and_variable_maps/Step01_precise_vital_item_map.csv"),
        cols = c("domain", "variable", "itemid", "preferred_label", "scientific_module", "mimic_label", "category")),
      eICU_lab = list(path = file.path(cfg$EICU_PROJECT, "03_feature_extraction/E2C_laboratory/StepE2C_lab_name_mapping_review.csv"),
        cols = c("source", "lab_name", "lab_name_clean", "variable", "lab_unit", "mapped", "n_records", "n_patients",
                 "median_value", "min_value", "max_value")))
    for (nm in names(dictionaries)) {
      x <- dictionaries[[nm]]
      d <- read_csv(x$path, intersect(c("variable", "lab_unit"), x$cols))
      d <- d[intersect(x$cols, names(d))]
      d$scope <- "LEGACY_EXTRACTION_DICTIONARY_NOT_FROZEN_COHORT_STATISTICS"
      csv_out(d, paste0("to_share/05_dictionary_", nm, ".csv"))
    }
    current_module <<- "measurement audit"
    if (isTRUE(cfg$AUDIT_MEASUREMENTS)) {
      audit_measurements(m, e)
      status <<- "COMPLETED_FOR_REVIEW"
    } else {
      issue("PENDING", "measurement audit", "逐次测量核查被配置关闭；队列已核对，变量单位和可用率尚待审核。")
      status <<- "COHORTS_FROZEN_MEASUREMENT_AUDIT_SKIPPED"
    }
    txt_out(c(
      "Step2：队列规则冻结与逐次测量核查。固定目录与文件名；重复运行覆盖本步骤登记的旧结果。",
      "MIMIC主结局：landmark之后30天死亡，区间为(L,L+30天]；T6/T24从完整基础队列独立构建。",
      "eICU主外部验证：T48后至医院出院的院内死亡，不称为30天外部验证。",
      "主队列要求landmark时仍活着且仍在院。单纯入院/ICU时钟先后或死亡/出院时钟先后不自动排除。",
      "原始时间不修改；保留行政时间标记和严格排除敏感性人群。真实状态矛盾、核心时间无效仍排除。",
      "未知医院结局不赋值0。MIMIC死亡日期跨越30天边界者主二分类分析排除，同时保存上下界敏感性标签。",
      "日期级死亡的生存时间中点仅为近似值；保留真实区间，不能将其称为精确死亡时刻。",
      "eICU采用已复核的一人一次固定选例规则；不声称能确定跨住院的chronological first。",
      "eICU全部已选病例需有特异急性MI文本；亚型仅按明确文本，ACS不等于STEMI/NSTEMI。",
      "05为旧抽取字典，不能把其中的旧样本数作为新队列统计。",
      "06/07为新队列在相应landmark之前的可用率/测量分布；仅审计既有清洗后缓存，不证明原始抽取无遗漏。",
      "MIMIC缓存未保留逐条单位，反馈中明确标为未验证；eICU肌钙蛋白I/T及不同单位保持分开。",
      "同名变量不是已确认跨库等价；本步不拟合预处理、不插补、不重训模型。",
      "eICU已有逐次测量只支持原T48人群；本步T6/T24仅保留时间资格，不能用于早期外部模型。",
      "local_only为逐行队列，只留本机。发回Step2_feedback.zip，勿上传local_only。"
    ), "to_share/00_READ_ME_FIRST.txt")
    txt_out(c(
      "eICU patient fields and chronological-order limitations: https://eicu.mit.edu/eicutables/patient/",
      "MIMIC administrative timestamp caveat: https://github.com/MIT-LCP/mimic-code/issues/966",
      "MIMIC date-of-death and one-year linkage: https://mimic.mit.edu/docs/iv/modules/hosp/patients",
      "These sources describe fields/limitations; the flag-retention policy is an explicit study decision, not an official data correction."
    ), "to_share/08_definition_sources.txt")
  }
  failure_message <- NULL
  tryCatch(main(), error = function(err) {
    status <<- "FAILED"
    failure_message <<- conditionMessage(err)
    issue("ERROR", current_module, "此模块未完成；详细错误仅记录于local_only，避免原始数据值进入反馈。")
    txt_out(c(paste0("Module: ", current_module), conditionMessage(err)), "local_only/Step2_error_detail.txt")
  })
  for (nm in ls(agg)) csv_out(do.call(rbind, agg[[nm]]), paste0("to_share/", nm, ".csv"))
  txt_out(c(paste0("STATUS = ", status),"SCRIPT_VERSION = Step2",
            "COMPLETED_FOR_REVIEW表示队列规则已核对、逐次测量核查完成，变量单位仍待审核。",
            "FAILED表示本次不完整，不要使用local_only中的部分结果建模。"), "to_share/Step2_STATUS.txt")
  txt_out(capture.output(sessionInfo()), "to_share/Step2_sessionInfo.txt")
  logmsg(paste0("Step2状态：", status))
  if (status == "FAILED") logmsg("请回传本次反馈包；若需要完整错误，查看local_only/Step2_error_detail.txt。")
  zip_ok <- FALSE
  if (isTRUE(cfg$MAKE_ZIP)) {
    zp <- reg("Step2_feedback.zip")
    share <- file.path(out, "to_share")
    files <- sort(basename(registered[startsWith(registered, "to_share/")]))
    zip_ok <- tryCatch({
      if (requireNamespace("zip", quietly = TRUE)) {
        zip::zipr(zipfile = zp, files = files, root = share)
      } else {
        wd <- getwd()
        tryCatch({ setwd(share); utils::zip(zipfile = zp, files = files, flags = "-q") }, finally = setwd(wd))
      }
      file.exists(zp) && file.info(zp)$size > 0
    }, error = function(err) FALSE)
    if (!zip_ok && file.exists(zp)) unlink(zp)
  }
  cat("\n固定输出：\n", out, "\n", sep = "")
  if (zip_ok) cat("请回传：\n", file.path(out, "Step2_feedback.zip"), "\n", sep = "")
  else cat("请压缩并回传：\n", file.path(out, "to_share"), "\n", sep = "")
  if (!is.null(failure_message)) cat("本次未完成：", failure_message, "\n")
  invisible(list(output_dir = out, status = status, zip_ok = zip_ok))
})
