# AMI-EDRS revision | Step1-1: targeted adjudication of timing and diagnosis flags
# 本版保留严格排除结果作参考，新增时间差、分项原因和行政时间仅标记情景；尚不冻结最终队列。
# Run in RStudio: source(file.choose(), encoding = "UTF-8")
# 固定输出：修改/Step1。重复运行仅清理本脚本登记的输出，不创建时间戳目录。
# 旧项目只读；不重训模型。患者逐行数据仅写入 local_only，反馈ZIP仅含汇总。
# eICU本步沿用旧选例作时间核查；AMI文本和跨住院选例经反馈审核后再冻结。

local({
  cfg <- list(
    MIMIC_PROJECT = Sys.getenv("AMI_MIMIC_PROJECT", unset=file.path(getwd(), "work/MIMIC")),
    EICU_PROJECT = Sys.getenv("AMI_EICU_PROJECT", unset=file.path(getwd(), "work/eICU")),
    OUTPUT_PARENT = Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    INSPECT_CACHE = TRUE,
    MAKE_ZIP = TRUE,
    LANDMARK_HOURS = c(6, 24, 48)
  )
  cfg <- utils::modifyList(cfg, getOption("ami_step1.config", list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in c("MIMIC_PROJECT", "EICU_PROJECT", "OUTPUT_PARENT")) cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step1")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step1输出目录。")
  if (is_link(out)) stop("Step1输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP1")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step1目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP1_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP1_MANAGED_OUTPUT")) {
    stop("Step1输出目录的归属标记不正确。")
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
        if (unlink(p) != 0L) stop("不能覆盖旧Step1输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step1_run_log.txt")
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
  # 反馈只含总体分布，不输出患者时间、ID或患者逐行文本。
  time_distribution <- function(source, scope, measure, minutes) {
    z <- minutes[is.finite(minutes)]
    br <- c(-Inf, -1440, -60, -5, 0, 1, 5, 60, 360, 1440, Inf)
    labs <- c('<=-24h','(-24h,-1h]','(-1h,-5min]','(-5min,0]',
              '(0,1min]','(1,5min]','(5min,1h]','(1h,6h]','(6h,24h]','>24h')
    counts <- table(factor(cut(z, br, labels = labs, right = TRUE), levels = labs))
    for (i in seq_along(labs)) emit('11_time_difference_bins', source = source, scope = scope,
      measure = measure, time_difference_bin = labs[i], n = as.integer(counts[i]))
    emit('11_time_difference_bins', source = source, scope = scope, measure = measure,
      time_difference_bin = 'MISSING_OR_NONFINITE', n = sum(!is.finite(minutes)))
  }
  component_audit <- function(source, scope, flags, take, old_y) {
    for (nm in names(flags)) {
      ix <- yes(take) & yes(flags[[nm]])
      emit('10_exclusion_components', source = source, scope = scope, component = nm,
           n = sum(ix), denominator = sum(yes(take)), old_events = ntrue(old_y[ix] == 1),
           old_outcome_unknown = sum(is.na(old_y[ix])))
    }
  }
  policy_summary <- function(source, landmark, policy, cohort, outcome) {
    ok <- yes(cohort); known <- ok & !is.na(outcome)
    emit('12_policy_comparison_NOT_FINAL', source = source, landmark = landmark,
      policy = policy, eligible_n = sum(ok), known_endpoint_n = sum(known),
      events = ntrue(outcome[known] == 1), non_events = ntrue(outcome[known] == 0),
      unresolved_endpoint_n = sum(ok & is.na(outcome)),
      status = 'QC_SCENARIO_ONLY_NOT_A_SELECTED_ANALYSIS_POLICY')
  }
  status <- "FAILED"
  current_module <- "setup"
  main <- function() {
    current_module <<- "MIMIC"
    logmsg("读取MIMIC基础队列，重建T6/T24/T48；不从旧T48子集倒推早期人群。")
    mb <- file.path(cfg$MIMIC_PROJECT, "01_cohort/Step01_AMI_index_ICU_cohort_base.csv")
    m <- read_csv(mb, c("subject_id", "hadm_id", "stay_id", "age", "admittime", "dischtime", "intime", "outtime", "deathtime", "dod", "hospital_expire_flag"))
    assert_unique(m, c("subject_id", "hadm_id", "stay_id"), "MIMIC基础队列")
    if (!nrow(m)) stop("MIMIC基础队列为空。")
    it <- parse_time(m$intime, "intime"); ot <- parse_time(m$outtime, "outtime")
    at <- parse_time(m$admittime, "admittime"); dt <- parse_time(m$dischtime, "dischtime")
    if (anyNA(it) || anyNA(at) || anyNA(dt)) stop("MIMIC入ICU/入院/出院时间有缺失，须先补查原始时间。")
    exact <- parse_time(m$deathtime, "deathtime"); day <- parse_day(m$dod, "dod")
    hf <- bit(m$hospital_expire_flag); age <- num(m$age)
    has_exact <- !is.na(exact); date_only <- !has_exact & !is.na(day)
    death_known <- has_exact | date_only
    dlo <- exact; dhi <- exact
    dlo[date_only] <- day[date_only]; dhi[date_only] <- day[date_only] + 86400
    dmid <- exact; dmid[date_only] <- day[date_only] + 43200
    # dhi为日期区间右开端点；精确时间则dlo=dhi。
    death_precision <- ifelse(has_exact, "exact_deathtime", ifelse(date_only, "date_only_interval", "no_recorded_death"))
    date_disagreement <- has_exact & !is.na(day) & floor(exact / 86400) != floor(day / 86400)
    time_invalid <- is.na(it) | is.na(at) | is.na(dt) | at > it | dt <= it | (!is.na(ot) & ot < it)
    # 不把出院死亡标记为1、却完全缺失死亡时间/日期的患者当作存活。
    death_conflict <- (hf %in% 1 & !death_known) | (hf %in% 0 & has_exact) |
      (hf %in% 0 & date_only & !is.na(dt) & dhi <= dt) |
      (has_exact & !is.na(at) & exact < at) | (has_exact & !is.na(dt) & exact > dt)
    death_conflict[is.na(death_conflict)] <- TRUE
    metric("MIMIC", "base", "base_rows", nrow(m))
    metric("MIMIC", "base", "date_only_deaths", ntrue(date_only), nrow(m))
    metric("MIMIC", "base", "deathtime_dod_calendar_date_disagreement", ntrue(date_disagreement), nrow(m))
    metric("MIMIC", "base", "death_or_status_conflict", ntrue(death_conflict), nrow(m))
    metric("MIMIC", "base", "invalid_core_time", ntrue(time_invalid), nrow(m))
    metric("MIMIC", "base", "icu_outtime_after_hospital_discharge", ntrue(ot > dt), nrow(m))
    if (ntrue(death_conflict)) issue("CHECK", "MIMIC", "旧严格规则标记时间/状态冲突；本版按具体原因和时间差拆解，不能将全部标记等同于不可用病例。")
    if (ntrue(date_disagreement)) issue("CHECK", "MIMIC", "deathtime与dod日期不一致；精确deathtime优先，冲突数量单独报告。")
    # 官方MIMIC死亡链接覆盖出院后最多一年。使用本次出院+365天作保守行政观察下界，
    # 而不是把研究日期最大值或本机当前日期当末次随访。仍在院人群的30天窗应在范围内。
    coverage_floor <- dt + 365 * 86400
    roster <- list()
    old48 <- read_csv(file.path(cfg$MIMIC_PROJECT, "03_analysis_dataset/Step02_AMI_T48_dataset.csv"),
                      c("stay_id", "death_30d_after_landmark"))
    assert_unique(old48, "stay_id", "旧MIMIC T48")
    old_y_base <- bit(old48$death_30d_after_landmark)[match(m$stay_id, old48$stay_id)]
    old_member <- m$stay_id %in% old48$stay_id
    mflags <- list(
      death_flag_but_no_death_time_or_date = hf %in% 1 & !death_known,
      hospital_alive_but_exact_admission_death_time = hf %in% 0 & has_exact,
      hospital_alive_but_date_of_death_before_discharge = hf %in% 0 & date_only & dhi <= dt,
      death_before_hospital_admission = has_exact & exact < at,
      death_after_hospital_discharge = has_exact & exact > dt,
      hospital_admission_after_index_ICU_admission = at > it,
      hospital_discharge_at_or_before_index_ICU_admission = dt <= it,
      index_ICU_discharge_before_index_ICU_admission = !is.na(ot) & ot < it,
      index_ICU_discharge_after_hospital_discharge = !is.na(ot) & ot > dt)
    mflags <- lapply(mflags, yes)
    for (scope in c('base','old_T48')) {
      take <- if (scope == 'base') rep(TRUE,nrow(m)) else old_member
      component_audit('MIMIC', scope, mflags, take, old_y_base)
      for (nm in c('death_after_hospital_discharge','hospital_admission_after_index_ICU_admission',
                   'index_ICU_discharge_after_hospital_discharge')) {
        delta <- switch(nm, death_after_hospital_discharge = (exact-dt)/60,
                        hospital_admission_after_index_ICU_admission = (at-it)/60,
                        index_ICU_discharge_after_hospital_discharge = (ot-dt)/60)
        time_distribution('MIMIC', scope, nm, delta[take & mflags[[nm]]])
      }
    }
    # 下述情景仅回答“若行政时间不一致只作标记，会影响多少人”。
    # 不修改死亡时间/出院时间，不按事件率或模型表现选择纳入规则。
    # 其他死亡状态矛盾、死亡早于起点及核心时间问题继续保留排除。
    death_hard <- mflags$death_flag_but_no_death_time_or_date |
      mflags$hospital_alive_but_exact_admission_death_time |
      mflags$hospital_alive_but_date_of_death_before_discharge |
      mflags$death_before_hospital_admission
    time_hard <- mflags$hospital_discharge_at_or_before_index_ICU_admission |
      mflags$index_ICU_discharge_before_index_ICU_admission
    csv_out(data.frame(subject_id=m$subject_id, hadm_id=m$hadm_id, stay_id=m$stay_id,
      old_T48_member=old_member, old_T48_outcome=old_y_base, mflags,
      death_minus_discharge_minutes=(exact-dt)/60,
      hospital_admission_minus_ICU_admission_minutes=(at-it)/60,
      ICU_discharge_minus_hospital_discharge_minutes=(ot-dt)/60),
      'local_only/MIMIC_time_issue_details.csv')
    for (h in cfg$LANDMARK_HOURS) {
      lab <- paste0("T", h); lm <- it + h * 3600; cutoff <- lm + 30 * 86400
      alive <- rep(NA_integer_, nrow(m))
      alive[!death_known & !(hf %in% 1)] <- 1L
      alive[has_exact] <- as.integer(exact[has_exact] > lm[has_exact])
      alive[date_only & dhi <= lm] <- 0L
      alive[date_only & dlo > lm] <- 1L
      # 死亡日期跨越landmark时保留UNKNOWN，不把中午假定值当真实死亡时刻。
      in_hosp <- !is.na(dt) & !is.na(lm) & dt > lm
      in_index_icu <- ifelse(is.na(ot) | is.na(lm), NA_integer_, as.integer(ot > lm))
      cohort_ok <- !time_invalid & !death_conflict & !is.na(age) & age >= 18 & alive %in% 1 & in_hosp
      y <- rep(NA_integer_, nrow(m))
      y[!death_known & !(hf %in% 1) & coverage_floor >= cutoff] <- 0L
      y[has_exact & exact > lm] <- as.integer(exact[has_exact & exact > lm] <= cutoff[has_exact & exact > lm])
      y[date_only & dlo > lm & dhi <= cutoff] <- 1L
      y[date_only & dlo > cutoff] <- 0L
      y_before_qc <- y
      y[death_conflict | time_invalid | !(alive %in% 1)] <- NA_integer_
      end_ambiguous <- date_only & dlo <= cutoff & dhi > cutoff
      end_ambiguous[is.na(end_ambiguous)] <- FALSE
      y_lo <- y; y_hi <- y
      y_lo[end_ambiguous & cohort_ok] <- 0L; y_hi[end_ambiguous & cohort_ok] <- 1L
      y_mid <- y
      y_mid[end_ambiguous & cohort_ok] <- as.integer(dmid[end_ambiguous & cohort_ok] <= cutoff[end_ambiguous & cohort_ok])
      analyzable <- cohort_ok & !is.na(y)
      basic_candidate <- !is.na(age) & age >= 18 & alive %in% 1 & in_hosp
      p_death_flag <- basic_candidate & !time_invalid & !death_hard
      p_admit_flag <- basic_candidate & !time_hard & !death_conflict
      p_both_flag <- basic_candidate & !time_hard & !death_hard
      policy_summary('MIMIC',lab,'STRICT_REFERENCE',cohort_ok,y_before_qc)
      policy_summary('MIMIC',lab,'DEATH_AFTER_DISCHARGE_FLAG_ONLY',p_death_flag,y_before_qc)
      policy_summary('MIMIC',lab,'ADMISSION_CLOCK_FLAG_ONLY',p_admit_flag,y_before_qc)
      policy_summary('MIMIC',lab,'BOTH_ADMIN_CLOCK_FLAGS_ONLY',p_both_flag,y_before_qc)
      hosp_y <- ifelse(cohort_ok, hf, NA_real_)
      # 时间仅用于已明确结局者；date-only事件的中点时间必须标记为近似。
      surv <- ifelse(analyzable & y == 0, 30, NA_real_)
      event <- analyzable & y %in% 1
      surv[event] <- (dmid[event] - lm[event]) / 86400
      lo_days <- hi_days <- rep(NA_real_, nrow(m))
      lo_days[event] <- (dlo[event] - lm[event]) / 86400
      hi_days[event] <- (dhi[event] - lm[event]) / 86400
      old30 <- as.integer(!is.na(dmid) & dmid >= lm & dmid <= it + 30 * 86400)
      # 另一个明确命名的终点：landmark以后至入ICU第30天；只作比较。
      shorter_end <- it + 30 * 86400
      y_to_icu30 <- rep(NA_integer_, nrow(m))
      y_to_icu30[!death_known & !(hf %in% 1) & coverage_floor >= shorter_end] <- 0L
      ix <- has_exact & exact > lm
      y_to_icu30[ix] <- as.integer(exact[ix] <= shorter_end[ix])
      y_to_icu30[date_only & dlo > lm & dhi <= shorter_end] <- 1L
      y_to_icu30[date_only & dlo > shorter_end] <- 0L
      y_to_icu30[death_conflict | time_invalid | !(alive %in% 1)] <- NA_integer_
      reason <- rep("eligible", nrow(m))
      put <- function(cond, text) { use <- reason == "eligible" & cond %in% TRUE; reason[use] <<- text }
      put(time_invalid, "invalid_core_time")
      put(is.na(age) | age < 18, "age_missing_or_under18")
      put(death_conflict, "death_or_status_conflict")
      put(alive %in% 0, "death_at_or_before_landmark")
      put(is.na(alive), "death_date_overlaps_landmark")
      put(!in_hosp, "discharged_at_or_before_landmark")
      put(is.na(y), "30d_endpoint_unresolved")
      r <- data.frame(subject_id = m$subject_id, hadm_id = m$hadm_id, stay_id = m$stay_id,
        landmark = lab, landmark_hour = h, age = age, intime = time_text(it), outtime = time_text(ot),
        dischtime = time_text(dt), deathtime = time_text(exact), dod = m$dod,
        landmark_time = time_text(lm), endpoint_30d_time = time_text(cutoff),
        death_precision = death_precision, death_interval_start = time_text(dlo), death_interval_end = time_text(dhi),
        alive_at_landmark = alive, in_hospital_at_landmark = as.integer(in_hosp),
        in_index_icu_at_landmark = in_index_icu, cohort_eligible = as.integer(cohort_ok),
        analysis_eligible_30d = as.integer(analyzable), eligibility_reason = reason,
        death_30d_after_landmark = y, death_after_landmark_through_icu_day30 = y_to_icu30,
        death_in_index_hospital_after_landmark = hosp_y,
        endpoint_boundary_ambiguous = as.integer(end_ambiguous),
        death_30d_lower_bound = y_lo, death_30d_upper_bound = y_hi, death_30d_midday_sensitivity = y_mid,
        followup_time_days_midpoint_if_date_only = surv,
        event_time_lower_days = lo_days, event_time_upper_days = hi_days,
        event_time_is_approximate = as.integer(event & date_only),
        death_status_conflict = as.integer(death_conflict),
        deathtime_dod_date_disagreement = as.integer(date_disagreement),
        administrative_coverage_floor = time_text(coverage_floor),
        QC_only_eligible_if_admin_clock_flags_only = as.integer(p_both_flag),
        QC_only_event_if_admin_clock_flags_only = ifelse(p_both_flag,y_before_qc,NA_integer_),
        cohort_definition_status = 'STRICT_REFERENCE_PENDING_TIME_ADJUDICATION',
        coverage_reaches_30d = as.integer(coverage_floor >= cutoff),
        legacy_formula_endpoint_for_comparison = old30, stringsAsFactors = FALSE)
      stopifnot(!anyDuplicated(r$stay_id), all(r$death_30d_after_landmark[analyzable] %in% c(0,1)),
                all(surv[analyzable] > 0 & surv[analyzable] <= 30),
                all(!is.na(y[analyzable])))
      roster[[lab]] <- r
      emit("03_landmark_summary", source = "MIMIC", landmark = lab, base_n = nrow(m),
           timing_eligible_n = sum(cohort_ok), endpoint_resolved_n = sum(analyzable),
           deaths = sum(y[analyzable]), nondeaths = sum(y[analyzable] == 0),
           event_rate = if (sum(analyzable)) mean(y[analyzable]) else NA_real_,
           index_icu_n = ntrue(cohort_ok & in_index_icu %in% 1),
           outside_index_icu_n = ntrue(cohort_ok & in_index_icu %in% 0),
           icu_status_unknown_n = ntrue(cohort_ok & is.na(in_index_icu)),
           endpoint_unresolved_n = sum(cohort_ok & is.na(y)),
           definition_status = "timing_rebuilt_from_legacy_AMI_base_review_required")
      grouped_counts("04_cohort_flow", data.frame(source = "MIMIC", landmark = lab, reason = reason), c("source", "landmark", "reason"))
      metric("MIMIC", lab, "death_date_overlaps_landmark", ntrue(date_only & is.na(alive)), nrow(m))
      metric("MIMIC", lab, "30d_boundary_date_uncertain_in_eligible", sum(end_ambiguous & cohort_ok), sum(cohort_ok))
      metric("MIMIC", lab, "30d_coverage_not_confirmed_in_eligible", ntrue(cohort_ok & (is.na(coverage_floor) | coverage_floor < cutoff)), sum(cohort_ok))
      metric("MIMIC", lab, "deaths_between_icu_day30_and_landmark_plus30_noon_diagnostic",
             ntrue(cohort_ok & !is.na(dmid) & dmid > it + 30*86400 & dmid <= cutoff), sum(cohort_ok))
      extra_noon <- cohort_ok & !is.na(dmid) & dmid > it+30*86400 & dmid <= cutoff
      extra_noon[is.na(extra_noon)] <- FALSE
      metric('MIMIC',lab,'window_extension_events_with_definite_endpoint',ntrue(extra_noon & y %in% 1),sum(extra_noon))
      metric('MIMIC',lab,'window_extension_noon_events_with_uncertain_endpoint',sum(extra_noon & is.na(y)),sum(extra_noon))
      if (h == 48) {
        j <- match(old48$stay_id, r$stay_id)
        if (anyNA(j)) stop("旧MIMIC T48存在不属于基础队列的ID，停止隐式丢弃。")
        cc <- data.frame(old_label = bit(old48$death_30d_after_landmark),
                         old_formula_reproduced = old30[j], new_label = y[j],
                         new_analysis_eligible = as.integer(analyzable[j]), reason = reason[j])
        grouped_counts("05_MIMIC_old_new_T48", cc, names(cc))
        metric("MIMIC", lab, "old_endpoint_formula_disagreements", ntrue(bit(old48$death_30d_after_landmark) != old30[j]), nrow(old48))
        metric("MIMIC", lab, "old_T48_discharged_at_or_before_landmark", ntrue(!in_hosp[j]), nrow(old48))
        metric("MIMIC", lab, "new_analyzable_not_in_old_T48", sum(analyzable & !(m$stay_id %in% old48$stay_id)), sum(analyzable))
      }
    }
    csv_out(do.call(rbind, roster), "local_only/MIMIC_landmark_roster.csv")
    # 旧T0使用0-6h特征但起点为0；仅复核差异，不作为新的T6队列。
    t0 <- read_csv(file.path(cfg$MIMIC_PROJECT, "03_analysis_dataset/Step02_AMI_T0_dataset.csv"), c("stay_id"), optional = TRUE)
    if (!is.null(t0)) {
      j <- !(m$stay_id %in% t0$stay_id)
      metric("MIMIC", "legacy_T0", "base_not_in_old_T0", sum(j), nrow(m))
      metric("MIMIC", "legacy_T0", "omitted_with_noon_death_before_icu", ntrue(j & !is.na(dmid) & dmid < it), sum(j))
      metric("MIMIC", "legacy_T0", "omitted_date_only_death_on_icu_calendar_day", ntrue(j & date_only & floor(day/86400) == floor(it/86400)), sum(j))
    }

    current_module <<- "eICU"
    logmsg("核查eICU现有选例的时间与出院结局；不把院内死亡改名为30天死亡。")
    ep <- file.path(cfg$EICU_PROJECT, "02_cohort_construction")
    e <- read_csv(file.path(ep, "StepE1_eICU_conservative_AMI_first_ICU_stay_per_patient.csv"),
      c("patientunitstayid", "uniquepid", "patienthealthsystemstayid", "age", "hospitaldischargeoffset", "unitdischargeoffset", "hospitaldischargestatus", "unitdischargestatus", "hospitaladmitoffset"))
    assert_unique(e, c("patientunitstayid", "uniquepid"), "eICU旧选例")
    if (!nrow(e)) stop("eICU基础选例表为空。")
    decode_status <- function(x) {
      z <- tolower(trimws(x)); ifelse(z == "expired", 1L, ifelse(z == "alive", 0L, NA_integer_))
    }
    hy <- decode_status(e$hospitaldischargestatus); uy <- decode_status(e$unitdischargestatus)
    ho <- num(e$hospitaldischargeoffset); uo <- num(e$unitdischargeoffset); ha <- num(e$hospitaladmitoffset)
    ea <- num(gsub("[ >]", "", e$age)); ea[grepl(">", e$age, fixed = TRUE)] <- 90
    conflict <- uy %in% 1 & hy %in% 0
    time_bad <- is.na(ho) | ho <= 0 | (!is.na(uo) & uo < 0) | (!is.na(ha) & ha > 0)
    eold <- read_csv(file.path(ep, 'StepE1_eICU_AMI_T48_landmark_external_validation_cohort.csv'),
                     c('patientunitstayid','primary_external_endpoint'))
    assert_unique(eold,'patientunitstayid','旧eICU T48')
    eold_member <- e$patientunitstayid %in% eold$patientunitstayid
    eold_y <- bit(eold$primary_external_endpoint)[match(e$patientunitstayid,eold$patientunitstayid)]
    eflags <- lapply(list(hospital_discharge_missing=is.na(ho),hospital_discharge_nonpositive=!is.na(ho)&ho<=0,
      ICU_discharge_negative=!is.na(uo)&uo<0, hospital_admission_offset_positive=!is.na(ha)&ha>0,
      hospital_status_unknown=is.na(hy), ICU_status_unknown=is.na(uy),
      ICU_death_hospital_alive=conflict),yes)
    for (scope in c('base','old_T48')) {
      take <- if(scope=='base') rep(TRUE,nrow(e)) else eold_member
      component_audit('eICU',scope,eflags,take,eold_y)
      time_distribution('eICU',scope,'positive_hospital_admission_offset_minutes',ha[take & eflags$hospital_admission_offset_positive])
    }
    csv_out(data.frame(patientunitstayid=e$patientunitstayid,old_T48_member=eold_member,
      old_T48_outcome=eold_y,eflags,hospitaladmitoffset=ha,hospitaldischargeoffset=ho,
      unitdischargeoffset=uo,hospitaldischargestatus=e$hospitaldischargestatus,
      unitdischargestatus=e$unitdischargestatus),'local_only/eICU_time_status_issue_details.csv')
    status_bucket <- function(x) {
      z <- tolower(trimws(as.character(x)));z[missing(z)] <- 'MISSING'
      z[!(z %in% c('alive','expired','dead','death','deceased','unknown','MISSING'))] <- 'OTHER_UNRECOGNIZED'
      z
    }
    for(scope in c('base','old_T48')) {
      take<-if(scope=='base') rep(TRUE,nrow(e)) else eold_member
      grouped_counts('15_eICU_status_categories',data.frame(scope=scope,
        hospital_status=status_bucket(e$hospitaldischargestatus[take]),
        unit_status=status_bucket(e$unitdischargestatus[take]),
        old_outcome=eold_y[take]),c('scope','hospital_status','unit_status','old_outcome'))
    }
    er <- list()
    for (h in cfg$LANDMARK_HOURS) {
      lab <- paste0("T", h); lm <- h * 60
      still_hosp <- !is.na(ho) & ho > lm
      dead_before <- uy %in% 1 & !is.na(uo) & uo <= lm
      ok <- !time_bad & !conflict & !is.na(ea) & ea >= 18 & still_hosp & !dead_before
      outcome <- ifelse(ok, hy, NA_integer_)
      alt_ok <- !(eflags$hospital_discharge_missing | eflags$hospital_discharge_nonpositive |
                    eflags$ICU_discharge_negative) & !conflict & !is.na(ea) & ea>=18 & still_hosp & !dead_before
      policy_summary('eICU',lab,'STRICT_REFERENCE',ok,hy)
      policy_summary('eICU',lab,'POSITIVE_ADMISSION_OFFSET_FLAG_ONLY',alt_ok,hy)
      inc <- ifelse(is.na(uo), NA_integer_, as.integer(uo > lm))
      reason <- ifelse(time_bad, "invalid_or_missing_time",
        ifelse(is.na(ea) | ea < 18, "age_missing_or_under18",
        ifelse(conflict, "ICU_death_hospital_alive_conflict",
        ifelse(!still_hosp, "hospital_discharge_at_or_before_landmark",
        ifelse(dead_before, "ICU_death_at_or_before_landmark",
        ifelse(is.na(hy), "hospital_outcome_unknown", "eligible_under_legacy_AMI_selection"))))))
      rr <- data.frame(patientunitstayid = e$patientunitstayid, uniquepid = e$uniquepid,
        patienthealthsystemstayid = e$patienthealthsystemstayid, landmark = lab, landmark_hour = h,
        hospitaldischargeoffset = ho, unitdischargeoffset = uo,
        hospital_status_death = hy, unit_status_death = uy, conflict_flag = as.integer(conflict),
        in_index_icu_at_landmark = inc, timing_eligible = as.integer(ok),
        outcome_resolved = as.integer(ok & !is.na(hy)), hospital_death_after_landmark = outcome,
        eligibility_reason = reason, AMI_definition_and_selection_finalized = FALSE,
        QC_only_eligible_if_admission_offset_flag_only=as.integer(alt_ok),
        QC_only_hospital_event_if_admission_offset_flag_only=ifelse(alt_ok,hy,NA_integer_))
      er[[lab]] <- rr
      valid <- ok & !is.na(hy)
      emit("03_landmark_summary", source = "eICU", landmark = lab, base_n = nrow(e),
        timing_eligible_n = sum(ok), endpoint_resolved_n = sum(valid), deaths = sum(hy[valid]),
        nondeaths = sum(hy[valid] == 0), event_rate = if (sum(valid)) mean(hy[valid]) else NA_real_,
        index_icu_n = ntrue(ok & inc %in% 1), outside_index_icu_n = ntrue(ok & inc %in% 0),
        icu_status_unknown_n = ntrue(ok & is.na(inc)), endpoint_unresolved_n = sum(ok & is.na(hy)),
        definition_status = "legacy_AMI_membership_only_NOT_FINAL_external_cohort")
      grouped_counts("04_cohort_flow", data.frame(source = "eICU", landmark = lab, reason = reason), c("source", "landmark", "reason"))
      metric("eICU", lab, "hospital_alive_ICU_dead_conflict_in_still_hospital", ntrue(conflict & still_hosp), sum(still_hosp))
    }
    csv_out(do.call(rbind, er), "local_only/eICU_landmark_timing_audit.csv")
    j <- match(eold$patientunitstayid, e$patientunitstayid)
    if (anyNA(j)) stop("旧eICU T48存在不属于基础选例的ID。")
    cmp <- data.frame(old_label = bit(eold$primary_external_endpoint),
                      reconstructed_hospital_label = er$T48$hospital_death_after_landmark[j], reason = er$T48$eligibility_reason[j])
    grouped_counts("06_eICU_old_new_T48", cmp, names(cmp))
    if (any(conflict)) issue("CHECK", "eICU", "ICU死亡与医院存活冲突者暂列待核查，未自动改写任一原始状态。")
    if(any(eold_member & is.na(hy))) issue('CHECK','eICU',
      paste0('旧T48中有',sum(eold_member & is.na(hy)),'例医院出院状态未明确解码；不能把未知状态自动当作存活。'))

    current_module <<- "eICU AMI evidence audit"
    # 使用约15MB的AMI相关证据子表，不把约763MB全诊断文本读入内存。
    ev <- read_csv(file.path(ep, "StepE1_AMI_related_diagnosis_evidence_for_review.csv"),
       c("patientunitstayid", "source", "diagnosis_offset", "diagnosis_text_clean", "ami_valid_evidence"), optional = TRUE)
    if (!is.null(ev)) {
      ev <- ev[ev$patientunitstayid %in% e$patientunitstayid, , drop = FALSE]
      txt <- tolower(ifelse(missing(ev$diagnosis_text_clean), "", ev$diagnosis_text_clean))
      # 只生成待审证据分类，不以粗略正则直接重定临床诊断。
      # 旧版explicit实际是包含独立MI缩写的宽泛文本筛查，不能独自证明AMI纳入有效。
      explicit <- grepl("myocardial infarct|\\bami\\b|\\bstemi\\b|\\bnstemi\\b|\\bmi\\b|subendocardial infarct", txt, perl = TRUE)
      acs <- grepl("acute coronary syndrome|\\bacs\\b", txt, perl = TRUE)
      good <- bit(ev$ami_valid_evidence) %in% 1
      conservative <- ev$source %in% c("patient_apacheadmissiondx", "admissionDx", "apachePredVar")
      match_term <- function(p) grepl(p,txt,perl=TRUE)
      non_st <- match_term('\\bnstemi\\b|\\bnon[ -]*st[ -]*(elevation|elevated|emi)\\b|subendocardial infarct')
      st <- match_term('\\bstemi\\b|\\bst[ -]*(elevation|elevated)[ -]*myocardial infarct') & !non_st
      reversed_acute <- match_term('infarction[ ,;:-]+acute myocardial')
      mi_delayed <- match_term('\\bmi admitted[ ]*>[ ]*24')
      specific_acute <- reversed_acute | mi_delayed | non_st | st |
        match_term('\\bacute[ ]+(myocardial infarct|mi\\b)|\\bami\\b')
      history_or_ruleout <- match_term('(old|prior|previous|history of|rule out|r/o|no evidence of)[ ]+(myocardial infarct|mi\\b)|type[ -]*(2|ii)[ ]+(mi\\b|myocardial infarct)|demand isch')
      specific_screen <- specific_acute & !history_or_ruleout
      signs <- list(
        broad_MI_keyword=explicit, acute_MI_specific_phrase=specific_acute,
        acute_MI_specific_phrase_without_history_ruleout_type2_text=specific_screen,
        infarction_comma_acute_myocardial_APACHE_label=reversed_acute,
        MI_admitted_over24h_APACHE_label=mi_delayed,
        myocardial_infarction_phrase=match_term('myocardial infarct'),
        MI_abbreviation_without_specific_acute_phrase=match_term('\\bmi\\b') & !specific_acute,
        ACS_phrase=acs, NSTEMI_or_nonST_elevation_phrase=non_st, STEMI_or_ST_elevation_phrase=st,
        nonQ_wave_phrase=match_term('non[ -]*q[ -]*wave'),
        historical_ruleout_type2_or_demand_text=history_or_ruleout)
      off <- num(ev$diagnosis_offset)
      # 来源层面计数：不导出原始诊断文本，防止合并字符串夹带非诊断信息。
      for(src in sort(unique(ev$source[!missing(ev$source)]))) {
        src_take <- ev$source==src
        for (elig in c('all_related_rows','old_valid_rows')) {
          take <- yes(src_take) & if(elig=='old_valid_rows') good else rep(TRUE,nrow(ev))
          for(nm in names(signs)) {
            ix<-take & yes(signs[[nm]])
            emit('13_eICU_diagnosis_signatures',source=src,evidence_subset=elig,signature=nm,
                 n_rows=sum(ix),n_selected_stays=length(unique(ev$patientunitstayid[ix])),
                 n_with_known_offset_le48h=length(unique(ev$patientunitstayid[ix & !is.na(off) & off<=2880])),
                 n_with_missing_offset=length(unique(ev$patientunitstayid[ix & is.na(off)])))
          }
        }
      }
      has <- function(cond) e$patientunitstayid %in% unique(ev$patientunitstayid[cond %in% TRUE])
      eg <- data.frame(patientunitstayid = e$patientunitstayid,
        has_related_evidence_row = has(rep(TRUE, nrow(ev))),
        broad_MI_keyword_in_old_valid_evidence = has(good & explicit),
        broad_MI_keyword_in_conservative_sources = has(good & explicit & conservative),
        specific_acute_MI_text_in_old_valid_evidence = has(good & specific_screen),
        specific_acute_MI_text_in_conservative_sources = has(good & specific_screen & conservative),
        ACS_in_old_valid_evidence = has(good & acs),
        ACS_without_specific_acute_MI_text = has(good & acs) & !has(good & specific_screen),
        nstemi_evidence = has(good & non_st), stemi_evidence = has(good & st),
        nonQ_wave_text_without_assigning_ST_subtype = has(good & signs$nonQ_wave_phrase),
        specific_acute_MI_text_documented_by_T48 = has(good & specific_screen & !is.na(off) & off <= 2880),
        specific_acute_MI_text_time_missing = has(good & specific_screen & is.na(off)),
        no_specific_acute_MI_text_in_screen = !has(good & specific_screen))
      eg$legacy_T48_member <- eg$patientunitstayid %in% eold$patientunitstayid
      csv_out(eg, "local_only/eICU_AMI_evidence_flags.csv")
      for (scope in c("legacy_first_selected", "legacy_T48")) {
        ix <- if (scope == "legacy_T48") eg$legacy_T48_member else rep(TRUE, nrow(eg))
        for (v in setdiff(names(eg), c("patientunitstayid", "legacy_T48_member"))) metric("eICU", scope, v, ntrue(eg[[v]][ix]), sum(ix))
      }
      if ("ami_subtype_main" %in% names(e)) {
        metric("eICU", "legacy_first_selected", "old_STEMI_with_NSTEMI_but_no_separate_STEMI_evidence",
          ntrue(e$ami_subtype_main == "STEMI" & eg$nstemi_evidence & !eg$stemi_evidence), nrow(e))
      }
    }
    allstay <- read_csv(file.path(ep, "StepE1_eICU_conservative_AMI_adult_all_ICU_stays.csv"),
                        c("uniquepid", "patienthealthsystemstayid", "patientunitstayid"), optional = TRUE)
    if (!is.null(allstay)) {
      pp <- unique(allstay[c("uniquepid", "patienthealthsystemstayid")])
      pp <- pp[!missing(pp$uniquepid) & !missing(pp$patienthealthsystemstayid), , drop = FALSE]
      n_hosp <- table(pp$uniquepid)
      metric("eICU", "all_legacy_AMI_stays", "patients_with_multiple_hospital_encounters", sum(n_hosp > 1), length(n_hosp))
      metric("eICU", "all_legacy_AMI_stays", "duplicate_unit_stay_rows", sum(duplicated(allstay$patientunitstayid)), nrow(allstay))
      if(anyDuplicated(allstay$patientunitstayid)) stop('eICU全部AMI候选住院段存在重复ID，不能审核选例。')
      if('hospitaladmitoffset' %in% names(allstay)) {
        ah<-num(allstay$hospitaladmitoffset)
        lookup<-split(seq_len(nrow(allstay)),paste(allstay$uniquepid,allstay$patienthealthsystemstayid,sep='::'))
        n_units<-rep(NA_integer_,nrow(e)); earliest_unique<-rep(NA,nrow(e)); offset_unknown<-rep(FALSE,nrow(e))
        for(ii in seq_len(nrow(e))) {
          jj<-lookup[[paste(e$uniquepid[ii],e$patienthealthsystemstayid[ii],sep='::')]]
          if(!length(jj)) next
          n_units[ii]<-length(jj)
          offset_unknown[ii]<-anyNA(ah[jj])
          if(offset_unknown[ii]) next
          first_ids<-allstay$patientunitstayid[jj[ah[jj]==max(ah[jj])]]
          if(length(first_ids)==1L) earliest_unique[ii]<-e$patientunitstayid[ii]==first_ids
        }
        # 同一次住院中较大的hospitaladmitoffset意味着更早的ICU入科；
        # 跨住院不比较offset，ID也不视为时间先后。
        selection<-data.frame(patientunitstayid=e$patientunitstayid,
          n_eligible_hospital_encounters=as.integer(n_hosp[e$uniquepid]),
          n_eligible_units_in_selected_hospital=n_units,
          is_unique_earliest_known_AMI_unit_in_selected_hospital=earliest_unique,
          selected_hospital_has_unknown_offsets=offset_unknown,
          old_T48_member=eold_member)
        csv_out(selection,'local_only/eICU_selection_issue_details.csv')
        for(scope in c('base','old_T48')) {
          take<-if(scope=='base') rep(TRUE,nrow(e)) else eold_member
          metric('eICU',scope,'selected_unit_not_earliest_known_AMI_unit_within_same_hospital',
                 ntrue(take & earliest_unique %in% FALSE),sum(take))
          metric('eICU',scope,'within_hospital_order_unresolved_or_tied',ntrue(take & is.na(earliest_unique)),sum(take))
          metric('eICU',scope,'selected_patients_with_multiple_eligible_hospital_encounters',
                 ntrue(take & selection$n_eligible_hospital_encounters>1),sum(take))
        }
      } else issue('CHECK','eICU selection','全部候选住院段缺hospitaladmitoffset，无法核查同住院内顺序。')
      if('hospitaldischargeyear' %in% names(allstay)) {
        yy<-unique(allstay[c('uniquepid','patienthealthsystemstayid','hospitaldischargeyear')])
        yy<-yy[!missing(yy$uniquepid) & !missing(yy$patienthealthsystemstayid) & !missing(yy$hospitaldischargeyear),,drop=FALSE]
        yy_key<-paste(yy$uniquepid,yy$hospitaldischargeyear,sep='::')
        repeated<-duplicated(yy_key) | duplicated(yy_key,fromLast=TRUE)
        metric('eICU','all_legacy_AMI_stays','patients_with_multiple_eligible_hospitals_in_same_discharge_year',
          length(unique(yy$uniquepid[repeated])),length(unique(yy$uniquepid)))
      }
    }
    issue("PENDING", "eICU", "AMI明确证据、NSTEMI/STEMI分类及跨住院选例需按本步反馈核定；本步eICU表不是最终外部验证队列。")

    current_module <<- "cache"
    cache <- file.path(cfg$MIMIC_PROJECT, "05_duckdb_cache/Step02_AMI_landmark_refined_figures.duckdb")
    if (isTRUE(cfg$INSPECT_CACHE) && file.exists(cache) && requireNamespace("DBI", quietly = TRUE) && requireNamespace("duckdb", quietly = TRUE)) {
      logmsg("只读核查MIMIC逐次测量缓存。")
      tryCatch({
        con <- DBI::dbConnect(duckdb::duckdb(), dbdir = cache, read_only = TRUE)
        tryCatch({
          z <- DBI::dbGetQuery(con, "SELECT variable, COUNT(*) AS n_rows, COUNT(DISTINCT stay_id) AS n_stays,
              SUM(CASE WHEN hours_from_icu IS NULL THEN 1 ELSE 0 END) AS n_missing_hour,
              SUM(CASE WHEN valuenum IS NULL THEN 1 ELSE 0 END) AS n_missing_value,
              MIN(hours_from_icu) AS min_hour, MAX(hours_from_icu) AS max_hour,
              SUM(CASE WHEN hours_from_icu < 0 OR hours_from_icu > 72 THEN 1 ELSE 0 END) AS n_outside_0_72h
              FROM feature_long_72h_clean GROUP BY variable ORDER BY variable")
          csv_out(z, "to_share/07_MIMIC_cache_profile.csv")
        }, finally = DBI::dbDisconnect(con, shutdown = TRUE))
      }, error = function(err) issue("CHECK", "MIMIC cache", "逐次测量缓存未能查询；不影响本步队列与结局计算。"))
    } else issue("CHECK", "MIMIC cache", "缓存核查跳过：缺文件、缺DBI/duckdb或配置关闭；未自动安装依赖。")
    txt_out(c(
      "Step1-1：分项核查版；尚未冻结最终队列，不要直接用严格参考人数更新论文。",
      "所有路径固定；重复运行覆盖本脚本登记的结果。原始文件和旧投稿结果只读。",
      "03/04/05/06及local_only默认资格列仍是严格排除的参考结果，用于与首次Step1直接核对。",
      "10_exclusion_components：每类标记分别计数，可重叠，不能相加作为队列流程。old_events只指旧T48结局。",
      "11_time_difference_bins：时间差分布，区分分钟/小时偏移和更大的矛盾；不导出患者时间。",
      "12_policy_comparison_NOT_FINAL：仅改变行政时间标记处理的核查情景，没有更改原始时间或正式选定规则。",
      "行政时间不一致不是已确诊数据错误；本版不自动删除额外患者，也不自动把所有标记者恢复入组。",
      "13_eICU_diagnosis_signatures：按来源统计具体诊断词组；宽泛MI缩写匹配不能单独证明AMI有效。",
      "15_eICU_status_categories：区分Alive/Expired/Unknown/缺失等规范类别；其他文本仅标OTHER_UNRECOGNIZED。",
      "02_checks还包含同住院内选例顺序与跨住院不确定人数；不把跨住院相对offset当绝对时间。",
      "MIMIC：从5579例所在的基础文件独立构建T6/T24/T48，实际人数以本次输入为准。",
      "主候选条件：成人、landmark时明确存活且该次住院仍在院；死亡/出院恰在起点者不纳入。",
      "主终点：(landmark, landmark+30天]的全因死亡；另存landmark至入ICU第30天的明确命名终点。",
      "日期精度：deathtime优先；仅dod者按一整天区间处理。跨起点/终点者标记不确定。",
      "30天边界不确定者不进入已明确终点主分析，同时保留上下界及中午假定的敏感性标签。",
      "随访：无事件者30天；日期型死亡保留事件时间区间和明确标为近似的中点时间。",
      "MIMIC官方死亡链接覆盖至出院后一年；本步使用当前住院出院+365天作行政覆盖保守下界。",
      "该覆盖规则不代表每名患者都经过主动电话随访，也不使用最大脱敏日期估计研究终止日。",
      "in_index_icu_at_landmark仅指索引ICU住院段是否仍未结束，不能排除之后转入其他ICU或再入ICU。",
      "eICU：院内死亡用医院出院状态核查，ICU/医院矛盾状态暂列待核查，不强行更改。",
      "eICU的AMI文本分类只作核查；当前继续沿用旧选例，不能写成已完成新的外部队列定义。",
      "没有明确STEMI/NSTEMI文本时保留亚型未明确；不把Q波/非Q波自动等同STEMI/NSTEMI。",
      "跨住院时间无法靠相对offset或patientunitstayid顺序恢复；后续须冻结可解释的选例规则。",
      "eICU早期人群的特征不能直接使用只为旧T48患者提取的长表；本步不提取早期预测特征。",
      "本步未重训、未重新算AUROC、未创建新训练/测试划分。",
      "03_landmark_summary.csv：新时间定义下人数与事件；05/06：旧新交叉表；02/09：检查与待处理项。",
      "local_only含患者逐行表，仅供本机后续步骤使用；不要上传。",
      "发回Step1_feedback.zip；若未生成ZIP，压缩to_share目录发回。"
    ), "to_share/00_READ_ME_FIRST.txt")
    txt_out(c("MIMIC patients documentation: https://mimic.mit.edu/docs/iv/modules/hosp/patients",
              "eICU patient documentation: https://eicu.mit.edu/eicutables/patient/",
              "eICU admissionDx documentation: https://eicu.mit.edu/eicutables/admissiondx/",
              "MIMIC maintainer on administrative time inconsistencies: https://github.com/MIT-LCP/mimic-code/issues/966"), "to_share/08_definition_sources.txt")
    status <<- "COMPLETED_FOR_REVIEW"
  }
  failure_message <- NULL
  tryCatch(main(), error = function(err) {
    failure_message <<- conditionMessage(err)
    issue("ERROR", current_module, "此模块未完成；详细错误仅记录于local_only，避免原始数据值进入反馈。")
    txt_out(c(paste0("Module: ", current_module), conditionMessage(err)), "local_only/Step1_error_detail.txt")
  })
  for (nm in ls(agg)) csv_out(do.call(rbind, agg[[nm]]), paste0("to_share/", nm, ".csv"))
  txt_out(c(paste0("STATUS = ", status),"SCRIPT_VERSION = Step1-1",
            "COMPLETED_FOR_REVIEW表示计算完成、等待审阅，不表示AMI纳入规则和外部队列已全部冻结。",
            "FAILED表示本次不完整，不要使用local_only中的部分结果建模。"), "to_share/Step1_STATUS.txt")
  txt_out(capture.output(sessionInfo()), "to_share/Step1_sessionInfo.txt")
  logmsg(paste0("Step1状态：", status))
  if (status == "FAILED") logmsg("请回传本次反馈包；若需要完整错误，查看local_only/Step1_error_detail.txt。")
  zip_ok <- FALSE
  if (isTRUE(cfg$MAKE_ZIP)) {
    zp <- reg("Step1_feedback.zip")
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
  if (zip_ok) cat("请回传：\n", file.path(out, "Step1_feedback.zip"), "\n", sep = "")
  else cat("请压缩并回传：\n", file.path(out, "to_share"), "\n", sep = "")
  if (!is.null(failure_message)) cat("本次未完成：", failure_message, "\n")
  invisible(list(output_dir = out, status = status, zip_ok = zip_ok))
})
