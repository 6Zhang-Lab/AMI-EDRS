# AMI-EDRS revision | Step3: eICU laboratory mapping repair and nursing recovery
# 运行：source(file.choose(), encoding = "UTF-8")
# 本步仅修复eICU数值输入，不训练模型，不改动Step2队列。
# 固定输出：修改/Step3；重跑仅覆盖本步骤登记的结果，不创建时间戳文件。
# 需要T7中的原始lab.csv(.gz)，用于补回旧代码遗漏的Hct/chloride和未清洗的检测值。
# 原始lab找不到时，交互式R会允许选择该文件；护理记录优先原始nurseCharting，其次已保存的原始文本快照。
# local_only仅保存在本机；反馈ZIP不包含患者ID、逐行数值或自由文本。
local({
  cfg <- list(
    EICU_PROJECT = Sys.getenv("AMI_EICU_PROJECT", unset=file.path(getwd(), "work/eICU")),
    OUTPUT_PARENT = Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    RAW_EICU_DIR = Sys.getenv("AMI_EICU_RAW", unset=file.path(getwd(), "data/eicu-crd-2.0")),
    LAB_FILE = "", NURSE_FILE = "", ALLOW_FILE_CHOOSE = TRUE, MAKE_ZIP = TRUE
  )
  cfg <- utils::modifyList(cfg, getOption("ami_step3.config", list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in c("EICU_PROJECT", "OUTPUT_PARENT")) cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step3")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step3输出目录。")
  if (is_link(out)) stop("Step3输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP3")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step3目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP3_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP3_MANAGED_OUTPUT")) {
    stop("Step3输出目录的归属标记不正确。")
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
        if (unlink(p) != 0L) stop("不能覆盖旧Step3输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step3_run_log.txt")
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
  # 只按明确检测名称匹配，不用“包含creatinine/hemoglobin”等宽泛子串。
  lab_key <- function(x) tolower(gsub("[[:space:]]+", " ", trimws(as.character(x))))
  unit_key <- function(x) {
    z <- tolower(trimws(as.character(x)))
    z <- gsub("µ|μ", "u", z)
    z <- gsub("[[:space:]]+", "", z)
    z <- sub("^units/", "u/", z)
    z[z %in% c("", "na", "n/a", "null", "none")] <- NA_character_
    z
  }
  lab_rules <- data.frame(
    raw_name = c("albumin","ALT (SGPT)","AST (SGOT)","bicarbonate","HCO3","Total CO2",
      "total bilirubin","BUN","creatinine","bedside glucose","glucose","Hgb","Hct","chloride",
      "PT - INR","lactate","platelets x 1000","potassium","PT","PTT","sodium",
      "troponin - I","troponin - T","WBC x 1000"),
    variable = c("albumin","alt","ast","bicarbonate","bicarbonate_bloodgas","bicarbonate_totalco2",
      "total_bilirubin","bun","creatinine","glucose","glucose","hemoglobin","hematocrit","chloride",
      "inr","lactate","platelet","potassium","pt","ptt","sodium","troponin_i","troponin_t","wbc"),
    unit = c("g/dL","U/L","U/L","mmol/L","mmol/L","mmol/L","mg/dL","mg/dL","mg/dL",
      "mg/dL","mg/dL","g/dL","%","mmol/L","ratio","mmol/L","10^9/L","mmol/L","s","s",
      "mmol/L","ng/mL","ng/mL","10^9/L"),
    lower = c(0,0,0,0,0,0,0,0,0,0,0,0,0,50,0,0,0,1,0,0,80,0,0,0),
    upper = c(10,50000,50000,80,80,80,80,400,40,1500,1500,25,80,160,30,50,3000,12,
      300,300,200,10000,10000,500),
    stringsAsFactors = FALSE)
  lab_rules$raw_key <- lab_key(lab_rules$raw_name)
  factor_for_unit <- function(raw_unit, target) {
    u <- unit_key(raw_unit); z <- rep(NA_real_, length(u))
    target_key <- unit_key(target)
    z[(!is.na(u) & u == target_key) %in% TRUE] <- 1
    z[target %in% "U/L" & u %in% c("u/l","iu/l")] <- 1
    z[target %in% "s" & u %in% c("sec","secs","second","seconds","s")] <- 1
    z[target %in% "10^9/L" & u %in% c("k/mcl","k/ul","10^3/ul","10^9/l","x10^3/ul")] <- 1
    z[target %in% "mmol/L" & u %in% c("mmol/l","meq/l")] <- 1
    z[target %in% "g/dL" & u %in% "g/l"] <- 0.1
    z[target %in% "mg/dL" & u %in% "mg/l"] <- 0.1
    z[target %in% "ng/mL" & u %in% c("ug/l","ng/ml")] <- 1
    z[target %in% "ng/mL" & u %in% "ng/l"] <- 0.001
    z
  }
  map_lab_catalog <- function(d) {
    j <- match(lab_key(d$raw_name), lab_rules$raw_key)
    d$raw_key <- lab_key(d$raw_name)
    d$variable <- lab_rules$variable[j]; d$unit <- lab_rules$unit[j]
    d$lower <- lab_rules$lower[j]; d$upper <- lab_rules$upper[j]
    a <- factor_for_unit(d$unit_system, d$unit)
    b <- factor_for_unit(d$unit_interface, d$unit)
    d$factor <- ifelse(!is.na(a), a, b)
    d$unit_conflict <- !is.na(a) & !is.na(b) & abs(a-b)>1e-12
    d$decision <- ifelse(is.na(j), "NOT_IN_EXACT_ANALYTE_ALLOWLIST",
      ifelse(d$unit_conflict, "CONFLICTING_UNIT_FACTORS",
        ifelse(is.na(d$factor), "UNKNOWN_OR_INCOMPATIBLE_UNIT", "KEEP")))
    d$factor[d$decision != "KEEP"] <- NA_real_
    d
  }
  # 标签与数值各自解析；不会从包含“GCS/SpO2”等标签的整段文本抽第一个数字。
  repair_nurse <- function(d) {
    name <- lab_key(d$raw_name); label <- lab_key(d$raw_label); value_text <- trimws(d$raw_value)
    n <- nrow(d); v <- rep(NA_character_, n); unit <- rep(NA_character_, n)
    lo <- hi <- rep(NA_real_, n); factor <- rep(1, n); shift <- rep(0, n)
    put <- function(ix, variable, u, low, high) {
      ix <- which(ix %in% TRUE)
      v[ix] <<- variable; unit[ix] <<- u; lo[ix] <<- low; hi[ix] <<- high
    }
    put(label=="temperature" & name=="temperature (c)", "temperature","degC",25,45)
    ix <- label=="temperature" & name=="temperature (f)"
    put(ix, "temperature","degC",25,45); factor[ix %in% TRUE] <- 5/9; shift[ix %in% TRUE] <- -32*5/9
    put((label=="glasgow coma score" & name=="gcs total") |
        (label=="score (glasgow coma scale)" & name=="value"), "gcs_total","score",3,15)
    for (row in list(c("eyes","best eye response","gcs_eye","4"),
                     c("motor","best motor response","gcs_motor","6"),
                     c("verbal","best verbal response","gcs_verbal","5"))) {
      put((label=="glasgow coma score" & name==row[1]) | label==row[2],
          row[3],"score",1,as.numeric(row[4]))
    }
    value <- num(value_text)
    # 仅在已确认的GCS组件字段中接受数字编码，如4-->(E4) spontaneous。
    coded <- !is.na(v) & v %in% c("gcs_eye","gcs_motor","gcs_verbal") &
             grepl("^[1-6]-->", value_text)
    value[coded] <- num(substr(value_text[coded],1,1))
    descriptions <- list(
      gcs_motor = c("flaccid"=1,"abnormal extension"=2,"abnormal flexion"=3,
                    "withdraws"=4,"localizes to noxious stimuli"=5,"obeys simple commands"=6),
      gcs_verbal = c("none"=1,"clearly unresponsive"=1,"incomprehensible sounds"=2,
                    "inappropriate words"=3,"confused"=4,"oriented"=5,
                    "clearly oriented/can indicate needs"=5))
    for (nm in names(descriptions)) {
      hit <- which(v %in% nm & is.na(value))
      value[hit] <- unname(descriptions[[nm]][lab_key(value_text[hit])])
    }
    value <- value*factor + shift
    qualified <- grepl("[<>≤≥]", value_text) & !coded
    reason <- ifelse(is.na(v), "UNRECOGNIZED_NURSING_FIELD",
      ifelse(qualified, "QUALIFIED_VALUE_NOT_EXACT",
       ifelse(!is.finite(value), "NONNUMERIC_OR_UNASSESSABLE",
        ifelse(value<lo | value>hi | (unit=="score" & value != floor(value)),
               "OUTSIDE_RANGE_OR_NONINTEGER_SCORE", "KEEP"))))
    data.frame(patientunitstayid=as.character(d$patientunitstayid), hour=num(d$hour),
      available_hour=num(d$available_hour), variable=v, unit=unit, value=value,
      raw_name=d$raw_name, raw_label=d$raw_label, decision=reason,
      timing_basis=d$timing_basis, stringsAsFactors=FALSE)
  }
  # Chart contract: manuscript replacements, not exploratory model comparisons.
  # Fixed T48 population; cumulative 0-6/24/48h; missingness denominator includes everyone.
  # Match the original Figure S4: white background, bold dark labels, blue/red tiles,
  # light-blue side bars. Percent scale is fixed at 0-100; intensity uses measured patients.
  make_visuals <- function(avail, intensity, n_cohort, make_png=TRUE) {
    vital <- c("heart_rate","sbp","dbp","map","resp_rate","spo2","temperature")
    labs <- c("wbc","hemoglobin","hematocrit","platelet","creatinine","bun","sodium","potassium",
      "chloride","bicarbonate","glucose","lactate","inr","pt","ptt","troponin_t","troponin_i",
      "albumin","total_bilirubin","alt","ast","bicarbonate_bloodgas","bicarbonate_totalco2")
    pretty <- c(heart_rate="Heart rate",sbp="SBP",dbp="DBP",map="MAP",resp_rate="Respiratory rate",
      spo2="SpO2",temperature="Temperature",wbc="WBC",hemoglobin="Hemoglobin",hematocrit="Hematocrit",
      platelet="Platelet",creatinine="Creatinine",bun="BUN",sodium="Sodium",potassium="Potassium",
      chloride="Chloride",bicarbonate="Bicarbonate (chemistry)",glucose="Glucose",lactate="Lactate",
      inr="INR",pt="PT",ptt="PTT",troponin_t="Troponin T",troponin_i="Troponin I",albumin="Albumin",
      total_bilirubin="Total bilirubin",alt="ALT",ast="AST",bicarbonate_bloodgas="HCO3 (blood gas)",
      bicarbonate_totalco2="Total CO2",gcs_eye="GCS eye",gcs_motor="GCS motor",gcs_verbal="GCS verbal",
      gcs_total="GCS total")
    wins <- c("0-6h","0-24h","0-48h")
    mtx <- function(d,vars,field) {
      ans <- matrix(NA_real_,length(vars),length(wins),dimnames=list(vars,wins))
      key <- paste(d$variable,d$window,sep="|")
      if(anyDuplicated(key))stop("绘图汇总存在重复的变量/窗口。")
      for(j in seq_along(wins)) ans[,j] <- num(d[[field]][match(paste(vars,wins[j],sep="|"),key)])
      ans
    }
    draw_heat <- function(z,labels,title,missingness=TRUE) {
      nr <- nrow(z); yy <- nr:1
      oldpar <- graphics::par(no.readonly=TRUE)
      on.exit({graphics::layout(1);graphics::par(oldpar)},add=TRUE)
      graphics::layout(matrix(1:2,1),widths=if(missingness)c(4.5,1.8) else c(5,1))
      graphics::par(oma=c(1.2,0,3.2,0),family="sans",mar=c(4,10,0.4,0.3),xaxs="i",yaxs="i")
      graphics::plot.new();graphics::plot.window(xlim=c(.5,3.5),ylim=c(.5,nr+.5))
      pal <- if(missingness) grDevices::colorRampPalette(c("#4F7FC4","#FFFFFF","#B84A4A"))(101) else
        grDevices::colorRampPalette(c("#F8FAFC","#DBEAFE","#93C5FD","#3B82F6","#1E3A8A"))(101)
      top <- if(missingness)100 else max(1,z,na.rm=TRUE)
      for(i in seq_len(nr)) for(j in 1:3) {
        a <- z[i,j]
        frac <- if(missingness)a/100 else sqrt(a/top)
        fill <- if(is.finite(a))pal[1+round(100*min(1,max(0,frac)))] else "#EEEEEE"
        graphics::rect(j-.49,yy[i]-.48,j+.49,yy[i]+.48,col=fill,border="white",lwd=1)
        lab <- if(!is.finite(a))"NA" else if(missingness)sprintf("%.1f",a) else format(round(a,1),trim=TRUE)
        ink <- if(is.finite(a) && !missingness && frac>.82)"white" else "black"
        graphics::text(j,yy[i],lab,cex=.77,font=2,col=ink)
      }
      graphics::axis(2,at=yy,labels=labels,las=1,tick=FALSE,cex.axis=.85,font.axis=2)
      graphics::axis(1,at=1:3,labels=wins,tick=FALSE,cex.axis=.85,font.axis=2)
      if(missingness) {
        graphics::par(mar=c(4,.4,.4,1),xaxs="i",yaxs="i")
        graphics::plot.new();graphics::plot.window(xlim=c(0,122),ylim=c(.5,nr+.5))
        for(t in c(0,50,100))graphics::abline(v=t,col="#E5E7EB",lwd=.7)
        means <- rowMeans(z)
        for(i in seq_len(nr))if(is.finite(means[i])) {
          graphics::rect(0,yy[i]-.34,means[i],yy[i]+.34,col="#69A8D8",border=NA)
          graphics::text(means[i]+2,yy[i],sprintf("%.1f",means[i]),adj=0,cex=.73,font=2)
        }
        graphics::axis(1,at=c(0,50,100),cex.axis=.8)
        graphics::mtext("Mean missing (%)",1,line=2.4,cex=.8,font=2)
      } else {
        graphics::par(mar=c(4,.4,.4,3.5),xaxs="i",yaxs="i")
        graphics::plot.new();graphics::plot.window(xlim=c(0,1),ylim=c(0,1))
        for(k in 0:99)graphics::rect(.15,k/100,.55,(k+1)/100,col=pal[k+1],border=NA)
        ticks <- unique(round(c(0,top/4,top/2,top)))
        graphics::axis(4,at=sqrt(ticks/top),labels=ticks,las=1,cex.axis=.75)
        graphics::mtext("Median chart times",1,line=2.4,cex=.72,font=2)
      }
      graphics::mtext(title,3,outer=TRUE,line=1.6,adj=.02,cex=1.02,font=2)
      graphics::mtext(paste0("Fixed eICU T48 cohort (n = ",format(n_cohort,big.mark=","),
        "); values available by each window end"),3,outer=TRUE,line=.25,adj=.02,cex=.72,col="#444444")
      graphics::mtext(if(missingness)"Cell values: missing (%)   |   Blue = lower missingness; red = higher missingness" else
        "Cell values: median distinct chart times among patients with measurements; NA = no measured patients",
        1,outer=TRUE,line=.1,cex=.68)
    }
    specs <- list(
      list(file="Figure_S4A_vital_missingness",vars=vital,title="A  Availability of vital signs",missing=TRUE),
      list(file="Figure_S4B_vital_intensity",vars=vital,title="B  Measurement intensity of vital signs",missing=FALSE),
      list(file="Figure_S4C_laboratory_missingness",vars=labs,title="C  Availability of laboratory features",missing=TRUE),
      list(file="Figure_S4D_laboratory_intensity",vars=labs,title="D  Measurement intensity of laboratory features",missing=FALSE))
    for(sp in specs) {
      z <- if(sp$missing)100*(1-mtx(avail,sp$vars,"availability")) else
        mtx(intensity,sp$vars,"median_chart_times_if_measured")
      h <- max(4.3,1.9+length(sp$vars)*.31)
      grDevices::pdf(reg(paste0("to_share/",sp$file,".pdf")),width=9,height=h,family="Helvetica",useDingbats=FALSE)
      tryCatch(draw_heat(z,unname(pretty[sp$vars]),sp$title,sp$missing),finally=grDevices::dev.off())
      if(make_png) {
        grDevices::png(reg(paste0("to_share/",sp$file,".png")),width=9,height=h,units="in",res=400,bg="white")
        tryCatch(draw_heat(z,unname(pretty[sp$vars]),sp$title,sp$missing),finally=grDevices::dev.off())
      }
    }
    vars <- c(vital,labs,c("gcs_eye","gcs_motor","gcs_verbal","gcs_total"))
    cnt <- mtx(avail,vars,"measured_patients"); pct <- 100*mtx(avail,vars,"availability")
    cells <- matrix("",length(vars),4)
    cells[,1] <- unname(pretty[vars])
    for(j in 1:3)cells[,j+1] <- ifelse(is.finite(cnt[,j]),
      paste0(format(cnt[,j],trim=TRUE,big.mark=",")," (",sprintf("%.1f",pct[,j]),")"),"NA")
    colnames(cells) <- c("Component","0-6 h, n (%)","0-24 h, n (%)","0-48 h, n (%)")
    csv_out(as.data.frame(cells),"to_share/Table_S3_eICU_component_availability.csv")
    title <- "Table S3. eICU component availability (replacement section)"
    note <- paste0("Denominator: ",format(n_cohort,big.mark=",")," patients in the fixed T48 cohort. Windows are cumulative.")
    grDevices::pdf(reg("to_share/Table_S3_eICU_component_availability.pdf"),width=8.27,height=11.69,
                   family="Helvetica",useDingbats=FALSE)
    tryCatch({
      grid::grid.newpage()
      grid::grid.text(title,x=.06,y=.965,just="left",gp=grid::gpar(fontsize=11,fontface="bold"))
      grid::grid.text(note,x=.06,y=.936,just="left",gp=grid::gpar(fontsize=8))
      xs <- c(.065,.58,.745,.91); top <- .91; dy <- .021
      grid::grid.lines(x=c(.06,.95),y=rep(top,2),gp=grid::gpar(lwd=1.3))
      for(j in 1:4)grid::grid.text(colnames(cells)[j],x=xs[j],y=top-dy*.7,
        just=if(j==1)"left" else "right",gp=grid::gpar(fontsize=8.5,fontface="bold"))
      grid::grid.lines(x=c(.06,.95),y=rep(top-dy*1.45,2),gp=grid::gpar(lwd=.8))
      for(i in seq_len(nrow(cells)))for(j in 1:4)grid::grid.text(cells[i,j],x=xs[j],
        y=top-dy*(i+1.2),just=if(j==1)"left" else "right",gp=grid::gpar(fontsize=8))
      bottom <- top-dy*(nrow(cells)+1.9)
      grid::grid.lines(x=c(.06,.95),y=rep(bottom,2),gp=grid::gpar(lwd=1.3))
      grid::grid.text("Troponin I/T and chemistry/blood-gas bicarbonate remain separate; missing values are not set to normal.",
        x=.06,y=bottom-.025,just="left",gp=grid::gpar(fontsize=7))
      grid::grid.text("GCS uses time-stamped nursing records. This table does not define final organ-domain scoring rules.",
        x=.06,y=bottom-.045,just="left",gp=grid::gpar(fontsize=7))
    },finally=grDevices::dev.off())
    # Word可编辑RTF：仅表顶、表头下、表底三条横线，不含竖线。
    esc <- function(x) {x<-gsub("\\","\\\\",x,fixed=TRUE);x<-gsub("{","\\{",x,fixed=TRUE);gsub("}","\\}",x,fixed=TRUE)}
    rtf <- c("{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 Arial;}}\\paperw11906\\paperh16838\\margl720\\margr720\\margt720\\margb720",
      paste0("\\f0\\fs22\\b ",esc(title),"\\b0\\par\\fs16 ",esc(note),"\\par\\par"))
    rows <- rbind(colnames(cells),cells); ends <- c(4750,6650,8550,10450)
    for(i in seq_len(nrow(rows))) {
      border <- paste0(if(i==1)"\\clbrdrt\\brdrs\\brdrw15\\clbrdrb\\brdrs\\brdrw10" else "",
                       if(i==nrow(rows))"\\clbrdrb\\brdrs\\brdrw15" else "")
      defs <- paste0(border,"\\cellx",ends,collapse="")
      content <- paste(vapply(1:4,function(j)paste0("\\pard\\intbl",
        if(j==1)"\\ql " else "\\qr ","\\fs16 ",if(i==1)"\\b " else "\\b0 ",
        esc(rows[i,j]),"\\cell"),character(1)),collapse="")
      rtf <- c(rtf,paste0("\\trowd\\trgaph90",if(i==1)"\\trhdr" else "",defs,content,"\\row"))
    }
    rtf <- c(rtf,"\\pard\\fs15\\b0 Troponin I/T and chemistry/blood-gas bicarbonate remain separate.\\par",
      "GCS uses time-stamped nursing records; this section does not define final organ-domain scoring.\\par}")
    txt_out(rtf,"to_share/Table_S3_eICU_component_availability.rtf")
    map <- data.frame(output=c(vapply(specs,function(x)x$file,character(1)),
      "Table_S3_eICU_component_availability"),
      placement=c("REPLACE Figure S4A","REPLACE Figure S4B","REPLACE Figure S4C","REPLACE Figure S4D",
                  "REPLACE eICU component-availability section of Table S3"),
      scope=c(rep("Updated frozen T48 cohort; cumulative windows stop at 48 h",4),
              "Component availability only; final domain definitions updated later"))
    csv_out(map,"to_share/11_figure_table_replacement_map.csv")
    txt_out(c("本步图表对应关系：Figure S4A/B/C/D分别替换原有同名面板。",
      "保留原稿的白底、粗体标签、蓝红缺失率热图和蓝色测量强度风格；PDF为矢量版，PNG为400 dpi预览。",
      "此次窗口截至48小时，移除0-72小时列；所有早期窗口分母均为同一个已冻结T48人群。",
      "B/D显示有测量患者的独立记录时点数中位数；不是生物学独立样本数。",
      "Table S3输出仅替换/补充eICU组件可用率部分，三线表PDF和可编辑RTF同时提供。",
      "Figure S4E/F/G/H尚需在支持治疗代理和结局关联重建后更新，本步不替换这些面板。",
      "不与旧Figure S4E-H拼接后直接作为终稿；所有面板完成后统一拼版。",
      "本步没有新增正文图，也不占用新的正式Supplementary Figure编号。",
      "英文图注：A/C, Missingness of vital signs/laboratory features in the fixed eICU T48 cohort.",
      "Cell values indicate missingness percentages within cumulative 0-6, 0-24 and 0-48 h windows; side bars show their mean.",
      "B/D, Median numbers of distinct chart times among patients with measurements in each cumulative window.",
      "Raw laboratory results were separated by analyte and unit. Nursing records supplemented temperature and GCS.",
      "When available, result revision and chart entry times were also required to precede the window end."
    ),"to_share/11_figure_table_notes.txt")
  }
  # 测试只导出函数，不读取患者文件或运行数据库。
  if (isTRUE(getOption("ami_step3.functions_only", FALSE))) {
    return(invisible(list(map_lab_catalog=map_lab_catalog, repair_nurse=repair_nurse,
                          lab_rules=lab_rules, factor_for_unit=factor_for_unit,make_visuals=make_visuals)))
  }
  resolve_raw <- function(stem, explicit="", required=FALSE) {
    if (nzchar(explicit)) {
      if (!file.exists(explicit)) stop("指定原始文件不存在：", explicit)
      return(norm(explicit))
    }
    candidates <- file.path(cfg$RAW_EICU_DIR, c(paste0(stem,".csv.gz"),paste0(stem,".csv")))
    hit <- candidates[file.exists(candidates)]
    if (!length(hit) && dir.exists(cfg$RAW_EICU_DIR)) {
      hit <- list.files(cfg$RAW_EICU_DIR, pattern=paste0("^",stem,"[.]csv([.]gz)?$"),
                        recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
    }
    if (length(hit)==1L) return(norm(hit))
    if (length(hit)>1L) stop("发现多个原始",stem,"文件，请在配置中指定唯一文件。")
    if (required && isTRUE(cfg$ALLOW_FILE_CHOOSE) && interactive()) {
      cat("请选择原始eICU ",stem,".csv或.csv.gz（不是旧特征文件）。\n",sep="")
      return(norm(file.choose()))
    }
    if (required) stop("找不到原始eICU lab.csv(.gz)。请接上T7硬盘，或设置cfg$LAB_FILE后重跑。")
    NULL
  }
  status <- "FAILED"
  current_module <- "input verification"
  main <- function() {
    step2 <- file.path(cfg$OUTPUT_PARENT,"Step2")
    ss <- readLines(file.path(step2,"to_share/Step2_STATUS.txt"),warn=FALSE)
    if (!"STATUS = COMPLETED_FOR_REVIEW" %in% ss) stop("必须先成功完成Step2。")
    inp <- read_csv(file.path(step2,"to_share/01_inputs.csv"),c("file","md5"))
    for (i in seq_len(nrow(inp))) {
      if (!file.exists(inp$file[i]) || is.na(inp$md5[i]) ||
          unname(tools::md5sum(inp$file[i])) != inp$md5[i]) stop("Step2上游输入已变化，请先重跑Step2。")
    }
    ec <- read_csv(file.path(step2,"local_only/eICU_landmark_cohorts.csv"),
      c("patientunitstayid","landmark","analysis_eligible","hospital_death_after_landmark"))
    ec <- ec[ec$landmark=="T48" & bit(ec$analysis_eligible)%in%1,,drop=FALSE]
    assert_unique(ec,"patientunitstayid","冻结的eICU T48队列")
    sy <- read_csv(file.path(step2,"to_share/03_frozen_cohort_summary.csv"),
                   c("source","landmark","analyzable_n","events"))
    sy <- sy[sy$source=="eICU" & sy$landmark=="T48",,drop=FALSE]
    if (nrow(sy)!=1L || nrow(ec)!=num(sy$analyzable_n) ||
        sum(bit(ec$hospital_death_after_landmark))!=num(sy$events)) stop("冻结队列人数/事件数核对失败。")
    labfile <- resolve_raw("lab",cfg$LAB_FILE,TRUE)
    nursefile <- resolve_raw("nurseCharting",cfg$NURSE_FILE,FALSE)
    if (is.null(nursefile)) {
      adjacent <- file.path(dirname(labfile),c("nurseCharting.csv.gz","nurseCharting.csv"))
      hit <- adjacent[file.exists(adjacent)]
      if (length(hit)==1L) nursefile <- hit
    }
    if (!requireNamespace("DBI",quietly=TRUE) || !requireNamespace("duckdb",quietly=TRUE))
      stop("需要DBI和duckdb。")
    dbpath <- reg("local_only/Step3_eICU_measurements.duckdb")
    reg("local_only/Step3_eICU_measurements.duckdb.wal")
    con <- DBI::dbConnect(duckdb::duckdb(),dbdir=dbpath)
    spill <- tempfile("AMI_EDRS_Step3_",tmpdir=tempdir());dir.create(spill)
    on.exit({try(DBI::dbDisconnect(con,shutdown=TRUE),silent=TRUE);unlink(spill,recursive=TRUE)},add=TRUE)
    q <- function(x) as.character(DBI::dbQuoteString(con,x))
    qi <- function(x) as.character(DBI::dbQuoteIdentifier(con,x))
    run <- function(sql) DBI::dbExecute(con,sql)
    get <- function(sql) DBI::dbGetQuery(con,sql)
    run(paste("SET temp_directory =",q(spill)))
    run("SET memory_limit='2GB'");run("SET threads=2")
    DBI::dbWriteTable(con,"cohort_T48",ec,overwrite=TRUE)
    DBI::dbWriteTable(con,"cohort_ids",data.frame(patientunitstayid=ec$patientunitstayid),overwrite=TRUE)
    scan <- function(p) paste0("read_csv(",q(p),",header=true,all_varchar=true,ignore_errors=false)")
    columns <- function(p) names(get(paste("SELECT * FROM",scan(p),"LIMIT 0")))
    findcol <- function(nm,key,optional=FALSE) {
      j <- which(tolower(nm)==tolower(key))
      if (length(j)==1L) return(qi(nm[j]))
      if (optional) return("NULL")
      stop("原始文件缺少唯一字段：",key)
    }
    record_input <- function(label,p) emit("01_large_input_sources",source=label,file=p,
      bytes=file.info(p)$size,md5="NOT_COMPUTED_FOR_LARGE_RAW_FILE")
    record_input("eICU raw lab",labfile)
    current_module <<- "raw laboratory repair"
    logmsg("按冻结的T48人群扫描原始lab，补回Hct/chloride，分开标本和检测类型。")
    nm <- columns(labfile)
    col <- function(k,opt=FALSE) findcol(nm,k,opt)
    off <- paste0("TRY_CAST(x.",col("labresultoffset")," AS DOUBLE)/60.0")
    revcol <- col("labresultrevisedoffset",TRUE)
    revised <- if(revcol=="NULL") "NULL" else paste0("TRY_CAST(x.",revcol," AS DOUBLE)/60.0")
    strcol <- function(k) {
      z<-col(k,TRUE);if(z=="NULL") "CAST(NULL AS VARCHAR)" else paste0("CAST(x.",z," AS VARCHAR)")
    }
    run(paste0("CREATE TEMP TABLE lab_raw AS SELECT CAST(x.",col("patientunitstayid")," AS VARCHAR) AS patientunitstayid,",
      off," AS hour,",revised," AS revised_hour,",strcol("labname")," AS raw_name,",
      strcol("labmeasurenamesystem")," AS unit_system,",strcol("labmeasurenameinterface")," AS unit_interface,",
      strcol("labresult")," AS raw_numeric,",strcol("labresulttext")," AS raw_text
      FROM ",scan(labfile)," x INNER JOIN cohort_ids c ON CAST(x.",col("patientunitstayid"),
      " AS VARCHAR)=c.patientunitstayid WHERE ",off," BETWEEN 0 AND 48"))
    catalog <- get("SELECT raw_name,unit_system,unit_interface,COUNT(*) AS n_rows,
                   COUNT(DISTINCT patientunitstayid) AS n_patients
                   FROM lab_raw GROUP BY raw_name,unit_system,unit_interface")
    catalog <- map_lab_catalog(catalog)
    csv_out(catalog,"to_share/03_lab_mapping_decisions.csv")
    DBI::dbWriteTable(con,"lab_mapping",catalog,overwrite=TRUE)
    csv_out(lab_rules,"to_share/03_exact_analyte_rules.csv")
    run("CREATE TEMP TABLE lab_decisions AS
      SELECT r.*,m.variable,m.unit,m.factor,m.lower,m.upper,
        TRY_CAST(r.raw_numeric AS DOUBLE)*m.factor AS value,
        CASE WHEN r.revised_hour IS NULL THEN r.hour ELSE GREATEST(r.hour,r.revised_hour) END AS available_hour,
        CASE WHEN m.decision<>'KEEP' THEN m.decision
             WHEN regexp_matches(COALESCE(r.raw_text,''),'[<>≤≥]') OR regexp_matches(COALESCE(r.raw_numeric,''),'[<>≤≥]')
               THEN 'QUALIFIED_VALUE_NOT_EXACT'
             WHEN NOT COALESCE(isfinite(TRY_CAST(r.raw_numeric AS DOUBLE)),FALSE) THEN 'NO_EXACT_NUMERIC_VALUE'
             WHEN TRY_CAST(r.raw_numeric AS DOUBLE)*m.factor < m.lower OR TRY_CAST(r.raw_numeric AS DOUBLE)*m.factor > m.upper
               THEN 'OUTSIDE_BROAD_RANGE'
             ELSE 'KEEP' END AS row_decision
      FROM lab_raw r INNER JOIN lab_mapping m ON r.raw_name IS NOT DISTINCT FROM m.raw_name
       AND r.unit_system IS NOT DISTINCT FROM m.unit_system AND r.unit_interface IS NOT DISTINCT FROM m.unit_interface")
    csv_out(get("SELECT raw_name,variable,row_decision,COUNT(*) AS n_rows,
      COUNT(DISTINCT patientunitstayid) AS n_patients FROM lab_decisions
      GROUP BY raw_name,variable,row_decision ORDER BY row_decision,raw_name"),"to_share/04_lab_record_decisions.csv")
    run("CREATE TABLE measurements AS SELECT DISTINCT patientunitstayid,hour,available_hour,
      variable,unit,value,'lab' AS measurement_source,raw_name,
      CASE WHEN revised_hour IS NULL THEN 'DRAW_TIME_ONLY_REVISION_TIME_MISSING'
           ELSE 'DRAW_AND_REVISED_TIME' END AS timing_basis
      FROM lab_decisions WHERE row_decision='KEEP'")
    metric("eICU","T48","lab_retained_records",get("SELECT COUNT(*) AS n FROM measurements")$n,nrow(ec))
    metric("eICU","T48","lab_records_revised_after_T48_not_available_at_T48",
      get("SELECT COUNT(*) AS n FROM measurements WHERE available_hour>48")$n,nrow(ec))

    current_module <<- "vital signs"
    logmsg("保留现有逐次生命体征，并统一宽范围有效性筛选。")
    vf <- file.path(cfg$EICU_PROJECT,"03_feature_extraction/E2B_vital_signs/StepE2B_vital_signs_long_T0_T72.csv")
    if(!file.exists(vf))stop("缺少旧逐次生命体征文件。")
    record_input("eICU vital cache",vf)
    vr <- data.frame(variable=c("heart_rate","sbp","dbp","map","resp_rate","spo2","temperature"),
      unit=c("bpm","mmHg","mmHg","mmHg","per_min","%","degC"),
      lower=c(20,40,10,20,4,20,25),upper=c(260,300,200,250,80,100,45))
    DBI::dbWriteTable(con,"vital_rules",vr,overwrite=TRUE)
    run(paste0("CREATE TEMP TABLE vital_raw AS SELECT CAST(r.patientunitstayid AS VARCHAR) AS patientunitstayid,
      TRY_CAST(r.offset_min AS DOUBLE)/60.0 AS hour,r.variable,r.source AS measurement_source,
      r.raw_variable AS raw_name,TRY_CAST(r.value AS DOUBLE) AS value,m.unit,m.lower,m.upper
      FROM ",scan(vf)," r INNER JOIN cohort_ids c ON r.patientunitstayid=c.patientunitstayid
      INNER JOIN vital_rules m ON r.variable=m.variable
      WHERE TRY_CAST(r.offset_min AS DOUBLE) BETWEEN 0 AND 2880"))
    csv_out(get("SELECT variable,COUNT(*) AS n_rows,
      SUM(CASE WHEN value BETWEEN lower AND upper AND isfinite(value) THEN 1 ELSE 0 END) AS retained_rows
      FROM vital_raw GROUP BY variable"),"to_share/05_vital_record_decisions.csv")
    run("INSERT INTO measurements SELECT DISTINCT patientunitstayid,hour,hour AS available_hour,
      variable,unit,value,measurement_source,raw_name,'OBSERVATION_OFFSET_SOURCE_DOCUMENTED_UNITS'
      FROM vital_raw WHERE isfinite(value) AND value BETWEEN lower AND upper")

    current_module <<- "nursing temperature and GCS"
    logmsg("补充护理记录中的体温和GCS，保留真实记录时间，不回填为入ICU零时。")
    nurse_origin <- "RAW_NURSECHARTING"
    if(!is.null(nursefile)) {
      record_input("raw nurseCharting",nursefile)
      nm <- columns(nursefile)
      nc <- function(k,opt=FALSE) findcol(nm,k,opt)
      nt <- paste0("TRY_CAST(x.",nc("nursingchartoffset")," AS DOUBLE)/60.0")
      ent <- nc("nursingchartentryoffset",TRUE)
      ent <- if(ent=="NULL") "NULL" else paste0("TRY_CAST(x.",ent," AS DOUBLE)/60.0")
      raw_nurse <- get(paste0("SELECT CAST(x.",nc("patientunitstayid")," AS VARCHAR) AS patientunitstayid,",
        nt," AS hour,GREATEST(",nt,",COALESCE(",ent,",",nt,")) AS available_hour,
        CAST(x.",nc("nursingchartcelltypevalname")," AS VARCHAR) AS raw_name,
        CAST(x.",nc("nursingchartcelltypevallabel")," AS VARCHAR) AS raw_label,
        CAST(x.",nc("nursingchartvalue")," AS VARCHAR) AS raw_value,
        CASE WHEN ",ent," IS NULL THEN 'CHART_TIME_ONLY_ENTRY_TIME_MISSING' ELSE 'CHART_AND_ENTRY_TIME' END AS timing_basis
        FROM ",scan(nursefile)," x INNER JOIN cohort_ids c ON CAST(x.",nc("patientunitstayid")," AS VARCHAR)=c.patientunitstayid
        WHERE ",nt," BETWEEN 0 AND 48 AND LOWER(CAST(x.",nc("nursingchartcelltypevallabel")," AS VARCHAR))
        IN ('temperature','glasgow coma score','score (glasgow coma scale)','best eye response','best motor response','best verbal response')"))
    } else {
      nurse_origin <- "SAVED_RAW_NURSING_TEXT_NO_ENTRY_TIME"
      nf <- file.path(cfg$EICU_PROJECT,"03_feature_extraction/E2D_support_treatment_proxies/StepE2D_raw_text_proxy_records_T0_T72.csv")
      if(!file.exists(nf))stop("缺少原始nurseCharting和保存的护理文本快照。")
      record_input("saved nursing raw text",nf)
      nr <- get(paste0("SELECT CAST(x.patientunitstayid AS VARCHAR) AS patientunitstayid,
        TRY_CAST(x.offset_min AS DOUBLE)/60.0 AS hour,x.text_raw
        FROM ",scan(nf)," x INNER JOIN cohort_ids c ON x.patientunitstayid=c.patientunitstayid
        WHERE LOWER(x.source)='nursecharting' AND TRY_CAST(x.offset_min AS DOUBLE) BETWEEN 0 AND 2880
        AND (LOWER(x.text_raw) LIKE '%temperature%' OR LOWER(x.text_raw) LIKE '%glasgow%'
          OR LOWER(x.text_raw) LIKE '%best eye response%' OR LOWER(x.text_raw) LIKE '%best motor response%'
          OR LOWER(x.text_raw) LIKE '%best verbal response%')"))
      parts <- strsplit(nr$text_raw," | ",fixed=TRUE)
      valid <- lengths(parts)==3L
      metric("eICU","T48","saved_nurse_text_not_exactly_three_fields",sum(!valid),nrow(nr))
      raw_nurse <- data.frame(patientunitstayid=nr$patientunitstayid[valid],hour=nr$hour[valid],
        available_hour=nr$hour[valid],raw_name=vapply(parts[valid],function(x)x[1],character(1)),
        raw_label=vapply(parts[valid],function(x)x[2],character(1)),
        raw_value=vapply(parts[valid],function(x)x[3],character(1)),
        timing_basis=rep("SAVED_CHART_TIME_ENTRY_TIME_NOT_RETAINED",sum(valid)))
      issue("PENDING","nurse entry time","使用已保存护理原始文本恢复数值；原录入时间未保留，不能据此声称所有记录当时已在床旁可用。")
    }
    repaired <- repair_nurse(raw_nurse)
    grouped_counts("06_nursing_decisions",repaired,c("raw_name","raw_label","variable","decision","timing_basis"))
    repaired <- repaired[repaired$decision=="KEEP" & is.finite(repaired$hour),,drop=FALSE]
    repaired$measurement_source <- rep("nurseCharting",nrow(repaired))
    DBI::dbWriteTable(con,"nurse_repaired",repaired,overwrite=TRUE)
    run("INSERT INTO measurements SELECT DISTINCT patientunitstayid,hour,available_hour,variable,unit,value,
        measurement_source,raw_name,timing_basis FROM nurse_repaired")
    # 仅同一时间的完整、无矛盾组件可合成总分；不跨时间拼接最低组件。
    run("CREATE TEMP TABLE gcs_components AS SELECT patientunitstayid,hour,
      MAX(CASE WHEN variable='gcs_eye' THEN value END) AS eye,
      MAX(CASE WHEN variable='gcs_motor' THEN value END) AS motor,
      MAX(CASE WHEN variable='gcs_verbal' THEN value END) AS verbal,
      COUNT(DISTINCT CASE WHEN variable='gcs_eye' THEN value END) AS ne,
      COUNT(DISTINCT CASE WHEN variable='gcs_motor' THEN value END) AS nm,
      COUNT(DISTINCT CASE WHEN variable='gcs_verbal' THEN value END) AS nv,
      MAX(available_hour) AS available_hour
      FROM measurements WHERE variable IN ('gcs_eye','gcs_motor','gcs_verbal') GROUP BY patientunitstayid,hour")
    csv_out(get("SELECT COUNT(*) AS matching_record_pairs,
      SUM(CASE WHEN g.eye+g.motor+g.verbal <> t.value THEN 1 ELSE 0 END) AS explicit_total_disagrees_with_components
      FROM gcs_components g JOIN measurements t ON g.patientunitstayid=t.patientunitstayid AND g.hour=t.hour
      WHERE t.variable='gcs_total' AND g.ne=1 AND g.nm=1 AND g.nv=1"),"to_share/06_GCS_consistency.csv")
    run("INSERT INTO measurements SELECT g.patientunitstayid,g.hour,g.available_hour,'gcs_total','score',
      g.eye+g.motor+g.verbal,'nurseCharting_components','same_time_component_sum','DERIVED_FROM_SAME_CHART_TIME_COMPONENTS'
      FROM gcs_components g WHERE g.ne=1 AND g.nm=1 AND g.nv=1 AND NOT EXISTS
      (SELECT 1 FROM measurements t WHERE t.patientunitstayid=g.patientunitstayid AND t.hour=g.hour AND t.variable='gcs_total')")
    metric("eICU","T48","nursing_values_recovered",nrow(repaired),nrow(ec))
    metric("eICU","T48","nursing_rows_with_entry_after_T48",
      sum(repaired$available_hour>48,na.rm=TRUE),nrow(repaired))
    current_module <<- "fixed-window summaries"
    logmsg("生成修复后的可用率和每人每窗口摘要；不插补缺失值。")
    windows <- data.frame(window=c("0-6h","0-24h","0-48h","6-24h","24-48h"),
      start_hour=c(0,0,0,6,24),end_hour=c(6,24,48,24,48),include_start=c(1L,1L,1L,0L,0L))
    DBI::dbWriteTable(con,"windows",windows,overwrite=TRUE)
    run("CREATE TABLE feature_summary AS SELECT x.patientunitstayid,w.window,x.variable,x.unit,
      COUNT(*) AS n_records,COUNT(DISTINCT x.hour) AS n_chart_times,
      MIN(x.hour) AS first_hour,MAX(x.hour) AS last_hour,
      MIN(x.value) AS min_value,MAX(x.value) AS max_value,MEDIAN(x.value) AS median_value
      FROM measurements x JOIN windows w ON
        ((w.include_start=1 AND x.hour>=w.start_hour) OR (w.include_start=0 AND x.hour>w.start_hour))
        AND x.hour<=w.end_hour AND x.available_hour<=w.end_hour
      GROUP BY x.patientunitstayid,w.window,x.variable,x.unit")
    # 分母始终为固定的T48队列，不能把其早期特征标为独立T6/T24外部验证。
    catalog_vars <- unique(c(lab_rules$variable,vr$variable,"gcs_eye","gcs_motor","gcs_verbal","gcs_total"))
    DBI::dbWriteTable(con,"variable_catalog",data.frame(variable=catalog_vars),overwrite=TRUE)
    avail <- get(paste0("SELECT w.window,v.variable,",nrow(ec)," AS cohort_n,
      COUNT(DISTINCT f.patientunitstayid) AS measured_patients,
      1.0*COUNT(DISTINCT f.patientunitstayid)/",nrow(ec)," AS availability
      FROM windows w CROSS JOIN variable_catalog v LEFT JOIN feature_summary f
        ON w.window=f.window AND v.variable=f.variable GROUP BY w.window,v.variable ORDER BY w.window,v.variable"))
    avail$population <- "FROZEN_T48_COHORT_ALL_WINDOWS"
    csv_out(avail,"to_share/07_repaired_availability.csv")
    intensity <- get("SELECT \"window\",variable,MEDIAN(n_chart_times) AS median_chart_times_if_measured
       FROM feature_summary GROUP BY \"window\",variable")
    csv_out(intensity,"to_share/07_repaired_intensity.csv")
    current_module <<- "figures and three-line table"
    make_visuals(avail,intensity,nrow(ec))
    current_module <<- "final aggregate summaries"
    csv_out(get("SELECT variable,unit,measurement_source,COUNT(*) AS n_rows,
      COUNT(DISTINCT patientunitstayid) AS n_patients,MIN(value) AS min_value,
      quantile_cont(value,0.01) AS p01,MEDIAN(value) AS median_value,quantile_cont(value,0.99) AS p99,MAX(value) AS max_value
      FROM measurements WHERE available_hour<=48 GROUP BY variable,unit,measurement_source
      ORDER BY variable,measurement_source"),"to_share/07_repaired_value_profile.csv")
    old <- rbind(read_csv(file.path(step2,"to_share/07_availability_eICU_lab.csv")),
                 read_csv(file.path(step2,"to_share/07_availability_eICU_vital.csv")))
    old <- old[old$landmark=="T48",c("variable","measured_patients","availability")]
    old$variable[old$variable=="bilirubin"] <- "total_bilirubin"
    names(old)[2:3] <- c("old_mixed_definition_patients","old_mixed_definition_availability")
    now <- avail[avail$window=="0-48h",c("variable","measured_patients","availability")]
    comparison <- merge(now,old,by="variable",all=TRUE)
    comparison$note <- "OLD_DEFINITIONS_MAY_BE_MIXED; repaired value is not necessarily the same analyte"
    csv_out(comparison,"to_share/08_old_repaired_coverage.csv")
    emit("02_cohort_check",source="eICU",landmark="T48",n=nrow(ec),
         events=sum(bit(ec$hospital_death_after_landmark)),nurse_source=nurse_origin)
    # 保留可复用的清洁数值和摘要；原始全表、自由文本与临时中间表不落入交付数据库。
    run("DROP TABLE nurse_repaired")
    run("CHECKPOINT")
    issue("PENDING","remaining model work",
      "本步完成eICU数值输入修复；器官域权重、治疗代理、MIMIC逐条单位核验和训练流程在后续统一。")
    txt_out(c(
      "Step3只修复eICU数值特征，队列固定为Step2的T48人群。",
      "所有窗口（包括0-6h/0-24h）均来自该T48人群，不是早期外部验证队列。",
      "直接从原始lab重新提取，精确区分血液/尿液/脑脊液，分开总胆红素与直接胆红素、PTT秒与比值。",
      "HCO3血气和Total CO2保留独立名称；未知单位不自动视为mmol/L。",
      "Troponin I与T分别保留，即使单位相同也不互换；没有ULN时不假造标准化倍数。",
      "Hct、chloride从原始lab补回；不从旧清洗后长表推断它们不存在。",
      "体温与GCS从护理记录补充，F仅在明确F字段中换算为C。不能评估的GCS保持缺失。",
      "实验室截断同时检查采样时间和最终修订时间；护理原表可用时同时检查记录和录入时间。",
      "护理文本快照只保留记录时间时，单独标记此限制，不假造录入时间。",
      "GCS总分可由同一时刻三个完整、无冲突组件合成；已有总分优先，不跨时刻拼最低组件。",
      "旧监护生命体征缓存仍受旧抽取范围影响；本步不声称原始生命体征全部重新抽取。",
      "模型未训练、缺失未填0、器官域尚未打分。",
      "固定输出Step3，重复运行清理登记的旧结果。回传Step3_feedback.zip，local_only留在本机。"
    ),"to_share/00_READ_ME_FIRST.txt")
    txt_out(c("https://eicu.mit.edu/eicutables/lab/","https://eicu.mit.edu/eicutables/nursecharting/",
      "https://eicu.mit.edu/eicutables/vitalperiodic/",
      "https://github.com/MIT-LCP/eicu-code/blob/main/concepts/pivoted/pivoted-score.sql",
      "https://github.com/MIT-LCP/eicu-code/blob/main/concepts/pivoted/pivoted-vital.sql"),"to_share/10_definition_sources.txt")
    status <<- "COMPLETED_FOR_REVIEW"
  }
  failure_message <- NULL
  tryCatch(main(), error = function(err) {
    status <<- "FAILED"
    failure_message <<- conditionMessage(err)
    issue("ERROR", current_module, "此模块未完成；详细错误仅记录于local_only，避免原始数据值进入反馈。")
    txt_out(c(paste0("Module: ", current_module), conditionMessage(err)), "local_only/Step3_error_detail.txt")
  })
  for (nm in ls(agg)) csv_out(do.call(rbind, agg[[nm]]), paste0("to_share/", nm, ".csv"))
  txt_out(c(paste0("STATUS = ", status),"SCRIPT_VERSION = Step3",
            "COMPLETED_FOR_REVIEW表示本步eICU数值特征修复完成、等待审阅，不表示模型已重建。",
            "FAILED表示本次不完整，不要使用local_only中的部分结果建模。"), "to_share/Step3_STATUS.txt")
  txt_out(capture.output(sessionInfo()), "to_share/Step3_sessionInfo.txt")
  logmsg(paste0("Step3状态：", status))
  if (status == "FAILED") logmsg("请回传本次反馈包；若需要完整错误，查看local_only/Step3_error_detail.txt。")
  zip_ok <- FALSE
  if (isTRUE(cfg$MAKE_ZIP)) {
    zp <- reg("Step3_feedback.zip")
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
  if (zip_ok) cat("请回传：\n", file.path(out, "Step3_feedback.zip"), "\n", sep = "")
  else cat("请压缩并回传：\n", file.path(out, "to_share"), "\n", sep = "")
  if (!is.null(failure_message)) cat("本次未完成：", failure_message, "\n")
  invisible(list(output_dir = out, status = status, zip_ok = zip_ok))
})
