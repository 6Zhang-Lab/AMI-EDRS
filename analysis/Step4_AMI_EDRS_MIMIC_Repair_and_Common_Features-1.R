# AMI-EDRS revision | Step4: MIMIC units/availability and common dynamic features
# 运行：source(file.choose(), encoding = "UTF-8")
# 修复版-1：单线程、按患者分批；先校验现有提取缓存，通过则跳过两张原始大表扫描。
# 不要删除已有Step4目录，也不要先运行旧版。无缓存时接上T7，可直接读取.csv.gz。
# 复用Step2冻结队列和Step3 eICU数据库；不重跑eICU原始提取，不训练模型。
# 固定输出：修改/Step4；重跑仅覆盖本步骤登记文件，不创建时间戳文件。
# 本步生成Figure 1C及Table S3的MIMIC组件可用率部分。
# 反馈包不含患者ID、逐行数值、自由文本；local_only仅留本机。
local({
  cfg <- list(
    MIMIC_PROJECT = Sys.getenv("AMI_MIMIC_PROJECT", unset=file.path(getwd(), "work/MIMIC")),
    OUTPUT_PARENT = Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    RAW_MIMIC_DIR = Sys.getenv("AMI_MIMIC_RAW", unset=file.path(getwd(), "data/mimic-iv-3.1")),
    LAB_FILE = "", CHART_FILE = "", LAB_DICTIONARY_FILE = "", CHART_DICTIONARY_FILE = "",
    ALLOW_FILE_CHOOSE = TRUE, MAKE_ZIP = TRUE, MAKE_PNG = TRUE,
    PATIENTS_PER_BATCH = 50L, MEMORY_LIMIT = "2GB", FORCE_REEXTRACT = FALSE
  )
  cfg <- utils::modifyList(cfg, getOption("ami_step4.config", list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in c("MIMIC_PROJECT", "OUTPUT_PARENT")) cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step4")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step4输出目录。")
  if (is_link(out)) stop("Step4输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP4")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step4目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP4_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP4_MANAGED_OUTPUT")) {
    stop("Step4输出目录的归属标记不正确。")
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
  # 保留可恢复的提取缓存；只清理本步骤登记的下游输出。不会遍历清空目录。
  cache_files <- c("local_only/Step4_harmonized_features.duckdb",
    "local_only/Step4_harmonized_features.duckdb.wal", "to_share/01_inputs.csv",
    "to_share/01_large_input_sources.csv", "to_share/03_MIMIC_item_dictionary.csv",
    "to_share/03_common_component_rules.csv", "to_share/04_unit_decisions.csv",
    "to_share/04_record_decisions.csv", "to_share/05_availability_time_audit.csv",
    "to_share/05_GCS_reconstruction.csv")
  old <- character()
  if (file.exists(manifest)) {
    old <- unique(readLines(manifest, warn = FALSE))
    for (rel in old[nzchar(old)]) {
      p <- safe_path(rel)
      if (file.exists(p)) {
        if (dir.exists(p)) stop("登记的输出应为文件，不能为目录。")
        if (!(rel %in% cache_files) && unlink(p) != 0L) stop("不能覆盖旧Step4输出；请关闭正在使用它的程序。")
      }
    }
  }
  registered <- intersect(old, cache_files)
  writeLines(registered, manifest)
  reg <- function(rel) {
    p <- safe_path(rel)
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    if (!(rel %in% registered)) {
      if (file.exists(p) && !(rel %in% old)) stop("同名文件未登记为本步骤输出，停止覆盖：", rel)
      registered <<- c(registered, rel)
      writeLines(registered, manifest)
    }
    p
  }
  csv_out <- function(d, rel) utils::write.csv(d, reg(rel), row.names = FALSE, na = "", fileEncoding = "UTF-8")
  txt_out <- function(x, rel) writeLines(enc2utf8(x), reg(rel), useBytes = TRUE)
  logp <- reg("to_share/Step4_run_log.txt")
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
  make_mimic_visuals <- function(avail, intensity, n_cohort, make_png=TRUE) {
    vital <- c("heart_rate","sbp","dbp","map","resp_rate","spo2","temperature")
    labs <- c("wbc","hemoglobin","hematocrit","platelet","creatinine","bun","sodium","potassium",
      "chloride","bicarbonate","glucose","lactate","inr","pt","ptt","troponin_t",
      "albumin","total_bilirubin","alt","ast")
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
      graphics::mtext(paste0("Fixed MIMIC-IV T48 cohort (n = ",format(n_cohort,big.mark=","),
        "); values available by each window end"),3,outer=TRUE,line=.25,adj=.02,cex=.72,col="#444444")
      graphics::mtext(if(missingness)"Cell values: missing (%)   |   Blue = lower missingness; red = higher missingness" else
        "Cell values: median distinct chart times among patients with measurements; NA = no measured patients",
        1,outer=TRUE,line=.1,cex=.68)
    }
    specs <- list(list(file="Figure_1C_MIMIC_feature_missingness",
      vars=c(vital,labs,"gcs_eye","gcs_motor","gcs_verbal","gcs_total"),
      title="C  Availability of MIMIC-IV physiological features",missing=TRUE))
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
    csv_out(as.data.frame(cells),"to_share/Table_S3_MIMIC_component_availability.csv")
    title <- "Table S3. MIMIC-IV component availability (replacement section)"
    note <- paste0("Denominator: ",format(n_cohort,big.mark=",")," patients in the fixed T48 cohort. Windows are cumulative.")
    grDevices::pdf(reg("to_share/Table_S3_MIMIC_component_availability.pdf"),width=8.27,height=11.69,
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
      grid::grid.text("Measurements must be charted and available by each window end; missing values are not set to normal.",
        x=.06,y=bottom-.025,just="left",gp=grid::gpar(fontsize=7))
      grid::grid.text("GCS totals require all three valid components at the same chart time. This table does not define final organ-domain scoring rules.",
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
    rtf <- c(rtf,"\\pard\\fs15\\b0 Measurements are restricted to chart and availability times before the window end.\\par",
      "GCS totals require three valid same-time components; this section does not define final organ-domain scoring.\\par}")
    txt_out(rtf,"to_share/Table_S3_MIMIC_component_availability.rtf")
    csv_out(data.frame(output=c("Figure_1C_MIMIC_feature_missingness","Table_S3_MIMIC_component_availability"),
      placement=c("REPLACE Figure 1C","UPDATE MIMIC component-availability section of Table S3")),
      "to_share/11_figure_table_replacement_map.csv")
    txt_out(c("Figure 1C替换原MIMIC变量缺失率面板；分母为冻结T48人群。",
      "早期列仅描述同一T48人群的累计特征，不是各早期landmark的独立人群。",
      "Table S3仅更新MIMIC组件可用率部分；器官域定义与Figure 1D后续统一。",
      "本步不新增正式Figure编号，也不替换Step3的S4A-D。",
      "PDF为矢量图；PNG为400 dpi；三线表同时提供PDF与Word可编辑RTF。",
      "Figure 1C legend: Missingness of physiological features within cumulative 0-6, 0-24 and 0-48 h windows in the fixed MIMIC-IV T48 cohort.",
      "Cell values indicate missingness percentages; side bars show their mean across the three windows.",
      "Measurements were filtered by both chart time and system availability time when recorded. GCS totals required complete, unambiguous components at the same chart time."
    ),"to_share/11_figure_table_notes.txt")
  }
  # 固定分析字典；阈值用于复现既定描述性异常比例，不等于临床决策阈值。
  rules <- unique(lab_rules[c("variable","unit","lower","upper")])
  rules <- rbind(rules, data.frame(
    variable=c("heart_rate","sbp","dbp","map","resp_rate","spo2","temperature",
               "gcs_eye","gcs_motor","gcs_verbal","gcs_total"),
    unit=c("bpm","mmHg","mmHg","mmHg","per_min","%","degC",rep("score",4)),
    lower=c(20,40,10,20,4,20,25,1,1,1,3),
    upper=c(260,300,200,250,80,100,45,4,6,5,15)))
  rules$abnormal_low <- NA_real_;rules$abnormal_high <- NA_real_
  ablo <- c(wbc=4,hemoglobin=10,hematocrit=30,platelet=150,sodium=135,potassium=3.5,
    chloride=98,bicarbonate=22,glucose=70,albumin=3.5,heart_rate=50,sbp=90,dbp=60,
    map=65,resp_rate=10,spo2=90,temperature=36,gcs_eye=4,gcs_motor=6,gcs_verbal=5,gcs_total=15)
  abhi <- c(wbc=12,creatinine=1.5,bun=25,sodium=145,potassium=5,chloride=107,
    glucose=180,lactate=2,total_bilirubin=1.2,alt=40,ast=40,inr=1.5,pt=14,ptt=40,
    heart_rate=100,resp_rate=24,temperature=38)
  rules$abnormal_low <- unname(ablo[rules$variable]);rules$abnormal_high <- unname(abhi[rules$variable])
  # 肌钙蛋白没有跨试剂统一ULN，因此不构造其异常比例。I/T仍分别保存。
  labmap <- data.frame(itemid=c(51300,51301,50811,51222,51221,51265,53189,50912,51006,
    50983,50824,50971,50822,50902,50806,50882,50931,50809,50813,50862,50885,50861,50878,
    51237,51274,51275,51003),
    variable=c("wbc","wbc","hemoglobin","hemoglobin","hematocrit","platelet","platelet",
    "creatinine","bun","sodium","sodium","potassium","potassium","chloride","chloride",
    "bicarbonate","glucose","glucose","lactate","albumin","total_bilirubin","alt","ast",
    "inr","pt","ptt","troponin_t"),origin="lab",stringsAsFactors=FALSE)
  chartmap <- data.frame(itemid=c(220045,220179,220050,220180,220051,220181,220052,
    220210,220277,223761,223762,220739,223901,223900),
    variable=c("heart_rate","sbp","sbp","dbp","dbp","map","map","resp_rate","spo2",
    "temperature","temperature","gcs_eye","gcs_motor","gcs_verbal"),origin="chart",
    stringsAsFactors=FALSE)
  itemmap <- merge(rbind(labmap,chartmap),rules,by="variable",sort=FALSE)
  itemmap$itemid <- as.character(itemmap$itemid)

  # 原始记录单位优先；仅单位为空时才用该itemid的字典单位，禁止按数值大小猜单位。
  map_mimic_units <- function(d) {
    u <- unit_key(d$raw_unit);du <- unit_key(d$dictionary_unit)
    supplied <- !is.na(u);u[!supplied] <- du[!supplied]
    d$unit_basis <- ifelse(supplied,"RAW_VALUEUOM",ifelse(!is.na(du),"D_ITEMS_UNITNAME","UNRESOLVED"))
    target <- d$unit
    fac <- factor_for_unit(u,target);shift <- rep(0,nrow(d))
    fac[target %in% "bpm" & u %in% c("bpm","beats/min","beat/min","beats/minute")] <- 1
    fac[target %in% "per_min" & u %in% c("insp/min","breaths/min","breath/min","per_min","/min")] <- 1
    fac[target %in% "mmHg" & u %in% c("mmhg","mm(hg)")] <- 1
    fac[target %in% "%" & u %in% c("%","percent")] <- 1
    fac[target %in% "score" & u %in% c("score","points","point")] <- 1
    fac[target %in% "ratio" & u %in% c("ratio","inr")] <- 1
    fac[target %in% "10^9/L" & u %in% c("k/ul","k/uL","k/mcl","10^3/ul","10^9/l")] <- 1
    # INR和已确认GCS项目无量纲，允许单位为空；不把其他未知单位默认为标准单位。
    dimless <- is.na(u) & target %in% c("score","ratio")
    fac[dimless] <- 1;d$unit_basis[dimless] <- "DIMENSIONLESS_EXACT_ITEM_DEFINITION"
    cunit <- u %in% c("degc","°c","degrees c","degreec","c","deg.c")
    funit <- u %in% c("degf","°f","degrees f","degreef","f","deg.f")
    temp <- d$variable %in% "temperature"
    fac[temp & cunit] <- 1
    fac[temp & funit] <- 5/9;shift[temp & funit] <- -32*5/9
    # 温度itemid本身指定单位；记录/字典出现相反单位时拒绝，不能两次换算。
    mismatch <- temp & ((d$itemid %in% "223761" & cunit) | (d$itemid %in% "223762" & funit))
    d$factor <- fac;d$shift <- shift
    d$decision <- ifelse(mismatch,"UNIT_CONFLICTS_WITH_TEMPERATURE_ITEM",
      ifelse(is.na(fac),"UNKNOWN_OR_INCOMPATIBLE_UNIT","KEEP"))
    d$factor[d$decision!="KEEP"] <- NA_real_
    d
  }
  # 用于小型边界测试的参考实现，与下方SQL共享窗口、同刻去重和斜率规则。
  summarize_reference <- function(d,start=0,end=48,include_start=TRUE) {
    z <- d[is.finite(d$hour)&is.finite(d$available_hour)&is.finite(d$value)&
      d$hour<=end&d$available_hour<=end&if(include_start)d$hour>=start else d$hour>start,,drop=FALSE]
    if(!nrow(z))return(c(n_chart_times=0,mean=NA,sd=NA,slope_per_hour=NA))
    x <- sort(unique(z$hour));y <- vapply(x,function(h)median(unique(z$value[z$hour==h])),numeric(1))
    c(n_chart_times=length(x),mean=mean(y),sd=if(length(y)>1)sd(y) else NA_real_,
      slope_per_hour=if(length(x)>1 && diff(range(x))>=1)sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2) else NA_real_)
  }
  feature_sql <- function(measurements="all_measurements",cohort_table="cohorts",result_table="feature_summary") {
    ids <- c(measurements,cohort_table,result_table)
    if(any(!grepl("^[A-Za-z_][A-Za-z_0-9]*$",ids)))stop("内部SQL表名无效。")
    ans <- list(
    timepoints="CREATE TEMP TABLE timepoints AS SELECT a.source,a.stay_id,w.window,a.variable,a.unit,a.hour,
      MEDIAN(DISTINCT a.value) AS value,COUNT(*) AS n_source_rows,COUNT(DISTINCT a.value) AS n_distinct_values
      FROM all_measurements a JOIN windows w ON
        ((w.include_start=1 AND a.hour>=w.start_hour) OR (w.include_start=0 AND a.hour>w.start_hour))
        AND a.hour<=w.end_hour AND a.available_hour<=w.end_hour
      GROUP BY a.source,a.stay_id,w.window,a.variable,a.unit,a.hour",
    compact="CREATE TEMP TABLE compact_features AS SELECT a.source,a.stay_id,a.window,a.variable,a.unit,
      COUNT(*) AS n_chart_times,SUM(a.n_source_rows) AS n_source_rows,MIN(a.hour) AS first_hour,MAX(a.hour) AS last_hour,
      MIN(a.value) AS min_value,MAX(a.value) AS max_value,AVG(a.value) AS mean_value,
      MEDIAN(a.value) AS median_value,STDDEV_SAMP(a.value) AS sd_value,
      arg_min(a.value,a.hour) AS first_value,arg_max(a.value,a.hour) AS last_value,
      CASE WHEN COUNT(*)>=2 AND MAX(a.hour)-MIN(a.hour)>=1 THEN regr_slope(a.value,a.hour) ELSE NULL END AS slope_per_hour,
      AVG(CASE WHEN r.abnormal_low IS NULL AND r.abnormal_high IS NULL THEN NULL
        WHEN (r.abnormal_low IS NOT NULL AND a.value<r.abnormal_low) OR
             (r.abnormal_high IS NOT NULL AND a.value>r.abnormal_high) THEN 1.0 ELSE 0.0 END) AS abnormal_fraction
      FROM timepoints a JOIN component_rules r ON a.variable=r.variable AND a.unit=r.unit
      GROUP BY a.source,a.stay_id,a.window,a.variable,a.unit",
    features="CREATE TABLE feature_summary AS SELECT c.source,c.stay_id,c.landmark,w.window,r.variable,r.unit,
      COALESCE(f.n_chart_times,0) AS n_chart_times,COALESCE(f.n_source_rows,0) AS n_source_rows,
      f.first_hour,f.last_hour,f.min_value,f.max_value,f.mean_value,f.median_value,f.sd_value,
      f.first_value,f.last_value,f.slope_per_hour,f.abnormal_fraction,
      CASE WHEN f.n_chart_times IS NULL THEN 1 ELSE 0 END AS missing_flag
      FROM cohorts c JOIN windows w ON w.end_hour<=c.cutoff_hour CROSS JOIN component_rules r
      LEFT JOIN compact_features f ON c.source=f.source AND c.stay_id=f.stay_id
        AND w.window=f.window AND r.variable=f.variable AND r.unit=f.unit"
    )
    ans$timepoints <- sub("FROM all_measurements a",paste("FROM",measurements,"a"),ans$timepoints,fixed=TRUE)
    ans$features <- sub("FROM cohorts c",paste("FROM",cohort_table,"c"),ans$features,fixed=TRUE)
    ans$features <- sub("CREATE TABLE feature_summary",paste("CREATE TEMP TABLE",result_table),ans$features,fixed=TRUE)
    # 正式全量表持久化；预检亦使用相同定义。
    if(result_table=="feature_summary")ans$features<-sub("CREATE TEMP TABLE","CREATE TABLE",ans$features,fixed=TRUE)
    ans
  }
  # 每名患者的全部记录放在同一批；只切分患者，不切分时点或先平均批均值。
  build_features_batched <- function(con,batch_size=50L,progress=function(x)NULL,checkpoint=TRUE) {
    if(length(batch_size)!=1L || !is.finite(batch_size) || batch_size<1 || batch_size!=floor(batch_size))
      stop("PATIENTS_PER_BATCH必须为正整数。")
    run<-function(sql)DBI::dbExecute(con,sql);get<-function(sql)DBI::dbGetQuery(con,sql)
    sqls<-feature_sql("batch_measurements","batch_cohorts","batch_features")
    ids<-get("SELECT DISTINCT source,stay_id FROM cohorts ORDER BY source,stay_id")
    if(!nrow(ids))stop("特征队列为空。")
    run("DROP TABLE IF EXISTS feature_summary")
    audits<-list();duplicates<-list();batch_no<-0L;created<-FALSE
    drop_batch<-function() {
      for(tab in c("batch_features","compact_features","timepoints","batch_measurements","batch_cohorts","batch_ids"))
        run(paste("DROP TABLE IF EXISTS",tab))
    }
    drop_batch()
    for(src in unique(ids$source)) {
      ss<-ids[ids$source==src,,drop=FALSE]
      starts<-seq.int(1L,nrow(ss),by=as.integer(batch_size))
      for(b in seq_along(starts)) {
        batch_no<-batch_no+1L
        ii<-starts[b]:min(nrow(ss),starts[b]+batch_size-1L)
        DBI::dbWriteTable(con,"batch_ids",ss[ii,,drop=FALSE],temporary=TRUE)
        run("CREATE TEMP TABLE batch_cohorts AS SELECT c.* FROM cohorts c
          JOIN batch_ids b ON c.source=b.source AND c.stay_id=b.stay_id")
        run("CREATE TEMP TABLE batch_measurements AS SELECT a.* FROM all_measurements a
          JOIN batch_ids b ON a.source=b.source AND a.stay_id=b.stay_id")
        run(sqls$timepoints)
        du<-get("SELECT source,variable,COUNT(*) AS chart_times,
          SUM(CASE WHEN n_source_rows>1 THEN 1 ELSE 0 END) AS times_with_multiple_source_rows,
          SUM(CASE WHEN n_distinct_values>1 THEN 1 ELSE 0 END) AS times_with_different_values
          FROM timepoints WHERE \"window\"='0-48h' GROUP BY source,variable")
        run(sqls$compact)
        run(sqls$features)
        # 每批包括未测量患者，左连接逻辑与原版完全一致。
        if(!created) {
          run("CREATE TABLE feature_summary AS SELECT * FROM batch_features WHERE FALSE")
          created<-TRUE
        }
        run("INSERT INTO feature_summary SELECT * FROM batch_features")
        audit<-data.frame(source=src,batch=b,patients=length(ii),
          measurement_rows=as.numeric(get("SELECT COUNT(*) AS n FROM batch_measurements")$n),
          timepoint_rows=as.numeric(get("SELECT COUNT(*) AS n FROM timepoints")$n),
          feature_rows=as.numeric(get("SELECT COUNT(*) AS n FROM batch_features")$n))
        audits[[batch_no]]<-audit;duplicates[[batch_no]]<-du
        drop_batch()
        if(checkpoint && (batch_no%%10L==0L || b==length(starts)))run("CHECKPOINT")
        if(b==1L || b%%10L==0L || b==length(starts))
          progress(paste0(src," 分批汇总：",b,"/",length(starts),"；本批",length(ii),"名患者。"))
      }
    }
    du<-do.call(rbind,duplicates)
    if(nrow(du)) {
      nn<-setdiff(names(du),c("source","variable"))
      du[nn]<-lapply(du[nn],as.numeric)
      du<-aggregate(du[nn],by=du[c("source","variable")],FUN=sum)
      du<-du[order(du$source,du$variable),,drop=FALSE]
    }
    list(same_time=du,batches=do.call(rbind,audits))
  }
  same_frame <- function(a,b,keys) {
    if(!setequal(names(a),names(b)) || nrow(a)!=nrow(b))return(FALSE)
    a<-a[names(b)]
    ord<-function(d)do.call(order,c(unname(d[keys]),list(na.last=TRUE)))
    a<-a[ord(a),,drop=FALSE];b<-b[ord(b),,drop=FALSE]
    all(vapply(names(a),function(k)identical(as.character(a[[k]]),as.character(b[[k]])),logical(1)))
  }
  duckdb_preflight <- function() {
    cc<-DBI::dbConnect(duckdb::duckdb(),dbdir=":memory:")
    on.exit(DBI::dbDisconnect(cc,shutdown=TRUE),add=TRUE)
    d<-data.frame(source="MIMIC",stay_id="fixture",hour=c(0,0,0,2,6,6,24,48),
      available_hour=c(0,0,0,2,6,7,25,48),variable="heart_rate",unit="bpm",value=c(1,1,3,5,9,99,20,49))
    co<-expand.grid(stay_id=c("fixture","unmeasured"),landmark=c("T6","T24","T48"),stringsAsFactors=FALSE)
    co$source<-"MIMIC";co$cutoff_hour<-as.numeric(sub("T","",co$landmark))
    ww<-data.frame(window=c("0-6h","0-24h","0-48h","6-24h","24-48h"),
      start_hour=c(0,0,0,6,24),end_hour=c(6,24,48,24,48),include_start=c(1L,1L,1L,0L,0L))
    DBI::dbWriteTable(cc,"all_measurements",d)
    DBI::dbWriteTable(cc,"cohorts",co)
    DBI::dbWriteTable(cc,"windows",ww)
    DBI::dbWriteTable(cc,"component_rules",rules[rules$variable=="heart_rate",])
    for(sql in feature_sql())DBI::dbExecute(cc,sql)
    z<-DBI::dbGetQuery(cc,"SELECT * FROM feature_summary")
    a<-z[z$stay_id=="fixture"&z$landmark=="T6",]
    ref<-summarize_reference(d,0,6,TRUE)
    stopifnot(nrow(a)==1,a$n_chart_times==ref['n_chart_times'],
      abs(a$mean_value-ref['mean'])<1e-10,abs(a$sd_value-ref['sd'])<1e-10,
      abs(a$slope_per_hour-ref['slope_per_hour'])<1e-10)
    a<-z[z$stay_id=="unmeasured",]
    stopifnot(nrow(a)==9,all(a$missing_flag==1),all(a$n_chart_times==0),all(is.na(a$mean_value)))
    a<-z[z$stay_id=="fixture"&z$landmark=="T48"&z$window=="24-48h",]
    stopifnot(nrow(a)==1,a$n_chart_times==1,is.na(a$sd_value),is.na(a$slope_per_hour))
    a<-DBI::dbGetQuery(cc,"SELECT epoch(TIMESTAMP '2100-01-01 06:00:00'-TIMESTAMP '2100-01-01 00:00:00')/3600.0 AS h")
    stopifnot(a$h==6)
    # 在同一小数据上比较全量/分批全部字段，包含两库相同ID、空患者及异常比例。
    ed<-d;ed$source<-"eICU";ed$value<-ed$value+60
    ec<-co;ec$source<-"eICU";ec<-ec[ec$landmark=="T48",,drop=FALSE]
    DBI::dbWriteTable(cc,"all_measurements",rbind(d,ed),overwrite=TRUE)
    DBI::dbWriteTable(cc,"cohorts",rbind(co,ec),overwrite=TRUE)
    for(tab in c("feature_summary","compact_features","timepoints"))DBI::dbExecute(cc,paste("DROP TABLE",tab))
    for(sql in feature_sql())DBI::dbExecute(cc,sql)
    expected<-DBI::dbGetQuery(cc,"SELECT * FROM feature_summary ORDER BY source,stay_id,landmark,\"window\",variable")
    for(bs in c(1L,2L)) {
      chk<-build_features_batched(cc,bs,checkpoint=FALSE)
      actual<-DBI::dbGetQuery(cc,"SELECT * FROM feature_summary ORDER BY source,stay_id,landmark,\"window\",variable")
      stopifnot(isTRUE(all.equal(expected,actual,tolerance=1e-10,check.attributes=FALSE)),
        sum(chk$batches$patients)==4,sum(chk$batches$feature_rows)==nrow(expected))
    }
    TRUE
  }
  if(isTRUE(getOption("ami_step4.functions_only",FALSE))) {
    return(invisible(list(map_mimic_units=map_mimic_units,rules=rules,itemmap=itemmap,
      summarize_reference=summarize_reference,make_mimic_visuals=make_mimic_visuals,
      feature_sql=feature_sql,build_features_batched=build_features_batched,same_frame=same_frame,
      duckdb_preflight=duckdb_preflight)))
  }
  resolve_raw <- function(stem,subdir,explicit="") {
    if(nzchar(explicit)) {
      if(!file.exists(explicit))stop("指定文件不存在：",explicit)
      return(norm(explicit))
    }
    hit <- file.path(cfg$RAW_MIMIC_DIR,subdir,paste0(stem,c(".csv.gz",".csv")))
    hit <- hit[file.exists(hit)]
    if(!length(hit)&&dir.exists(cfg$RAW_MIMIC_DIR))hit<-list.files(cfg$RAW_MIMIC_DIR,
      pattern=paste0("^",stem,"[.]csv([.]gz)?$"),recursive=TRUE,full.names=TRUE,ignore.case=TRUE)
    if(length(hit)==1)return(norm(hit))
    if(length(hit)>1)stop("发现多个",stem,"文件，请在配置中指定唯一文件。")
    if(isTRUE(cfg$ALLOW_FILE_CHOOSE)&&interactive()) {
      cat("请选择原始MIMIC ",stem,".csv或.csv.gz。\n",sep="")
      return(norm(file.choose()))
    }
    stop("未找到",stem,"；请接上T7或修改RAW_MIMIC_DIR/对应文件配置。")
  }
  status <- "FAILED";current_module <- "input verification"
  main <- function() {
    step2 <- file.path(cfg$OUTPUT_PARENT,"Step2");step3<-file.path(cfg$OUTPUT_PARENT,"Step3")
    for(p in c(file.path(step2,"to_share/Step2_STATUS.txt"),file.path(step3,"to_share/Step3_STATUS.txt")))
      if(!file.exists(p)||!"STATUS = COMPLETED_FOR_REVIEW" %in% readLines(p,warn=FALSE))stop("先成功完成Step2及Step3。")
    inp<-read_csv(file.path(step3,"to_share/01_inputs.csv"),c("file","md5"))
    for(i in seq_len(nrow(inp)))if(!file.exists(inp$file[i])||is.na(inp$md5[i])||
      unname(tools::md5sum(inp$file[i]))!=inp$md5[i])stop("Step3上游输入已变化，不能混用旧数据库。")
    mr<-read_csv(file.path(step2,"local_only/MIMIC_landmark_cohorts.csv"),
      c("subject_id","hadm_id","stay_id","landmark","landmark_hour","analysis_eligible","intime",
        "in_index_icu_at_landmark","death_30d_after_landmark"))
    er<-read_csv(file.path(step2,"local_only/eICU_landmark_cohorts.csv"),
      c("patientunitstayid","landmark","analysis_eligible","in_index_icu_at_landmark","hospital_death_after_landmark"))
    mr<-mr[bit(mr$analysis_eligible)%in%1,,drop=FALSE]
    er<-er[er$landmark=="T48"&bit(er$analysis_eligible)%in%1,,drop=FALSE]
    sy<-read_csv(file.path(step2,"to_share/03_frozen_cohort_summary.csv"),c("source","landmark","analyzable_n","events"))
    for(lm in c("T6","T24","T48")) {
      x<-mr[mr$landmark==lm,,drop=FALSE];assert_unique(x,"stay_id",paste("MIMIC",lm))
      t<-sy[sy$source=="MIMIC"&sy$landmark==lm,,drop=FALSE]
      if(nrow(t)!=1||nrow(x)!=num(t$analyzable_n)||sum(bit(x$death_30d_after_landmark))!=num(t$events))stop("MIMIC队列校验失败。")
    }
    assert_unique(er,"patientunitstayid","eICU T48")
    echeck<-read_csv(file.path(step3,"to_share/02_cohort_check.csv"),c("n","events"))
    if(nrow(echeck)!=1||nrow(er)!=num(echeck$n)||sum(bit(er$hospital_death_after_landmark))!=num(echeck$events))stop("eICU队列校验失败。")
    # 保存完整冻结名单便于下一步直接连接结局；特征计算不读取结局值。
    cohorts<-rbind(data.frame(source="MIMIC",stay_id=mr$stay_id,landmark=mr$landmark,
      cutoff_hour=num(mr$landmark_hour),in_index_icu=bit(mr$in_index_icu_at_landmark)),
      data.frame(source="eICU",stay_id=er$patientunitstayid,landmark="T48",cutoff_hour=48,
        in_index_icu=bit(er$in_index_icu_at_landmark)))
    base<-unique(mr[c("subject_id","hadm_id","stay_id","intime")])
    assert_unique(base,"stay_id","MIMIC提取名单");assert_unique(base,"subject_id","MIMIC唯一患者")
    parse_time(base$intime,"MIMIC intime")
    if(!requireNamespace("DBI",quietly=TRUE)||!requireNamespace("duckdb",quietly=TRUE))stop("需要DBI和duckdb。")
    current_module<<-"DuckDB preflight"
    if(length(cfg$PATIENTS_PER_BATCH)!=1L || !is.finite(cfg$PATIENTS_PER_BATCH) ||
      cfg$PATIENTS_PER_BATCH<1 || cfg$PATIENTS_PER_BATCH!=floor(cfg$PATIENTS_PER_BATCH))
      stop("PATIENTS_PER_BATCH必须为正整数。")
    logmsg("先运行小型DuckDB预检：窗口、晚到记录、同刻合并，以及分批/全量统计等价性。")
    stopifnot(duckdb_preflight())
    txt_out("PASS: boundary fixtures matched the R reference; patient batches of 1 and 2 matched the unbatched SQL for both sources, all feature columns and missing patients.","to_share/02_SQL_preflight.txt")
    dbfile<-reg("local_only/Step4_harmonized_features.duckdb")
    reg("local_only/Step4_harmonized_features.duckdb.wal")
    reuse<-file.exists(dbfile)&&file.info(dbfile)$size>0
    if(isTRUE(cfg$FORCE_REEXTRACT)) {
      logmsg("FORCE_REEXTRACT=TRUE：重建本步骤登记的提取缓存。")
      for(p in c(dbfile,paste0(dbfile,".wal")))if(file.exists(p)&&unlink(p)!=0L)stop("无法重建已登记数据库。")
      reuse<-FALSE
    }
    con<-DBI::dbConnect(duckdb::duckdb(),dbdir=dbfile)
    spill<-tempfile("AMI_EDRS_Step4_",tmpdir=tempdir());dir.create(spill)
    on.exit({try(DBI::dbDisconnect(con,shutdown=TRUE),silent=TRUE);unlink(spill,recursive=TRUE)},add=TRUE)
    q<-function(x)as.character(DBI::dbQuoteString(con,as.character(x)))
    qi<-function(x)as.character(DBI::dbQuoteIdentifier(con,x))
    run<-function(sql)DBI::dbExecute(con,sql);get<-function(sql)DBI::dbGetQuery(con,sql)
    run(paste("SET temp_directory =",q(spill)));run(paste("SET memory_limit =",q(cfg$MEMORY_LIMIT)))
    run("SET threads=1");run("SET preserve_insertion_order=false")
    csv_out(get("SELECT name,value FROM duckdb_settings() WHERE name IN
      ('threads','memory_limit','preserve_insertion_order')"),"to_share/02_runtime_settings.csv")
    scan<-function(p)paste0("read_csv(",q(p),",header=true,all_varchar=true,ignore_errors=false)")
    columns<-function(p)names(get(paste("SELECT * FROM",scan(p),"LIMIT 0")))
    require_cols<-function(p,required){n<-columns(p);if(length(setdiff(required,n)))stop("文件缺字段：",basename(p)," / ",paste(setdiff(required,n),collapse=","));n}
    if(reuse) {
      current_module<<-"saved extraction cache validation"
      logmsg("发现上次Step4数据库：校验冻结名单、项目/单位规则、提取审计和GCS完整性。")
      cache_stop<-function(s)stop("提取缓存未通过校验：",s,
        "。缓存未被删除；若确需重扫，请设置 options(ami_step4.config=list(FORCE_REEXTRACT=TRUE)) 后重新source本修正版。")
      required_tables<-c("mimic_base","cohorts","MIMIC_frozen_roster","eICU_frozen_roster",
        "component_rules","item_map","unit_mapping","MIMIC_measurements")
      if(length(setdiff(required_tables,DBI::dbListTables(con))))cache_stop("缺少已完成提取的必要表")
      required_audits<-setdiff(cache_files,c("local_only/Step4_harmonized_features.duckdb",
        "local_only/Step4_harmonized_features.duckdb.wal","to_share/01_inputs.csv"))
      if(!all(file.exists(file.path(out,required_audits))))cache_stop("原始提取审计文件不完整")
      if(!all(required_audits%in%old))cache_stop("审计文件未登记为本步骤输出")
      if(!same_frame(get("SELECT * FROM mimic_base"),base,"stay_id") ||
         !same_frame(get("SELECT * FROM MIMIC_frozen_roster"),mr,c("stay_id","landmark")) ||
         !same_frame(get("SELECT * FROM eICU_frozen_roster"),er,c("patientunitstayid","landmark")) ||
         !same_frame(get("SELECT * FROM cohorts"),cohorts,c("source","stay_id","landmark")))cache_stop("冻结名单或时间锚点改变")
      if(!same_frame(get("SELECT * FROM component_rules"),rules,"variable"))cache_stop("组件定义改变")
      im<-get("SELECT * FROM item_map")
      if(!all(names(itemmap)%in%names(im)) || !same_frame(im[names(itemmap)],itemmap,c("origin","itemid")) ||
        any(!im$dictionary_ok%in%TRUE))cache_stop("项目映射改变或字典未通过")
      um<-get("SELECT * FROM unit_mapping");um_check<-map_mimic_units(um)
      if(!same_frame(um,um_check,c("origin","itemid","raw_unit")))cache_stop("单位换算规则改变")
      # 保留原始源文件指纹；文件仍挂载时核对大小和小字典MD5，不重复扫描大表。
      rs<-utils::read.csv(file.path(out,"to_share/01_large_input_sources.csv"),stringsAsFactors=FALSE)
      if(nrow(rs)!=4L || !all(c("file","bytes","md5")%in%names(rs)))cache_stop("原始输入记录不完整")
      for(i in seq_len(nrow(rs)))if(file.exists(rs$file[i])) {
        if(file.info(rs$file[i])$size!=num(rs$bytes[i]))cache_stop("原始文件大小改变")
        if(rs$md5[i]!="NOT_COMPUTED_FOR_LARGE_RAW_FILE" &&
          unname(tools::md5sum(rs$file[i]))!=rs$md5[i])cache_stop("原始字典改变")
      }
      explicit<-unlist(cfg[c("LAB_FILE","CHART_FILE","LAB_DICTIONARY_FILE","CHART_DICTIONARY_FILE")])
      explicit<-explicit[nzchar(explicit)]
      if(length(explicit)&&any(!vapply(explicit,norm,character(1))%in%vapply(rs$file,norm,character(1))))
        cache_stop("本次显式指定的原始文件与缓存不同")
      rd<-utils::read.csv(file.path(out,"to_share/04_record_decisions.csv"),stringsAsFactors=FALSE)
      keep<-rd[rd$row_decision=="KEEP",,drop=FALSE]
      actual<-get("SELECT origin,variable,COUNT(*) AS n_rows,COUNT(DISTINCT stay_id) AS n_patients
        FROM MIMIC_measurements WHERE variable<>'gcs_total' GROUP BY origin,variable")
      jj<-match(paste(keep$origin,keep$variable),paste(actual$origin,actual$variable))
      if(nrow(keep)!=nrow(actual)||anyNA(jj)||any(actual$n_rows[jj]>num(keep$n_rows))||
        any(actual$n_patients[jj]!=num(keep$n_patients))||sum(num(rd$n_rows))!=sum(num(um$n_rows)))
        cache_stop("提取记录与原始审计不一致")
      gg<-utils::read.csv(file.path(out,"to_share/05_GCS_reconstruction.csv"))
      ng<-get("SELECT COUNT(*) AS n FROM MIMIC_measurements WHERE variable='gcs_total'")$n
      if(nrow(gg)!=1L||ng!=num(gg$complete_unambiguous_groups))cache_stop("GCS合成未完整保存")
      invalid<-get("SELECT COUNT(*) AS n FROM MIMIC_measurements m LEFT JOIN mimic_base b
        ON m.stay_id=b.stay_id AND m.subject_id=b.subject_id AND m.hadm_id=b.hadm_id
        LEFT JOIN component_rules r ON m.variable=r.variable AND m.unit=r.unit
        WHERE b.stay_id IS NULL OR r.variable IS NULL OR NOT COALESCE(isfinite(m.value),FALSE)
          OR NOT COALESCE(isfinite(m.hour),FALSE) OR NOT COALESCE(isfinite(m.available_hour),FALSE)
          OR m.hour<0 OR m.hour>48 OR m.available_hour<m.hour OR m.value<r.lower OR m.value>r.upper")$n
      if(invalid!=0)cache_stop("缓存有无效患者、时点、单位或数值")
      logmsg("缓存校验通过：跳过labevents和chartevents扫描，直接继续分批汇总。")
      csv_out(data.frame(cache_action="REUSED_VALIDATED_EXTRACTION",mimic_rows=sum(as.numeric(actual$n_rows))+as.numeric(ng),
        derived_gcs_rows=as.numeric(ng),mounted_raw_files=sum(file.exists(rs$file)),
        large_raw_hash="not computed; recorded file size checked when available"),"to_share/02_extraction_cache.csv")
    } else {
    current_module<<-"fresh MIMIC extraction"
    obsolete_audits<-setdiff(cache_files[c(grepl("^to_share/",cache_files))],"to_share/01_inputs.csv")
    for(rel in intersect(obsolete_audits,registered)) {
      p<-safe_path(rel);if(file.exists(p)&&unlink(p)!=0L)stop("不能覆盖原始提取审计文件。")
    }
    registered<<-setdiff(registered,obsolete_audits);writeLines(registered,manifest)
    labfile<-resolve_raw("labevents","hosp",cfg$LAB_FILE)
    chartfile<-resolve_raw("chartevents","icu",cfg$CHART_FILE)
    ldfile<-resolve_raw("d_labitems","hosp",cfg$LAB_DICTIONARY_FILE)
    cdfile<-resolve_raw("d_items","icu",cfg$CHART_DICTIONARY_FILE)
    for(p in c(labfile,chartfile,ldfile,cdfile))emit("01_large_input_sources",file=p,bytes=file.info(p)$size,
      md5=if(p%in%c(ldfile,cdfile))unname(tools::md5sum(p)) else "NOT_COMPUTED_FOR_LARGE_RAW_FILE")
    csv_out(do.call(rbind,agg[["01_large_input_sources"]]),"to_share/01_large_input_sources.csv")
    DBI::dbWriteTable(con,"mimic_base",base,overwrite=TRUE)
    DBI::dbWriteTable(con,"cohorts",cohorts,overwrite=TRUE)
    DBI::dbWriteTable(con,"MIMIC_frozen_roster",mr,overwrite=TRUE)
    DBI::dbWriteTable(con,"eICU_frozen_roster",er,overwrite=TRUE)
    DBI::dbWriteTable(con,"component_rules",rules,overwrite=TRUE)
    # 以原始字典确认血液标本及chartevents来源；不靠变量名模糊匹配。
    require_cols(ldfile,c("itemid","label","fluid","category"))
    require_cols(cdfile,c("itemid","label","linksto","unitname"))
    ld<-get(paste0("SELECT itemid,label,fluid,category FROM ",scan(ldfile)," WHERE itemid IN (",paste(q(labmap$itemid),collapse=","),")"))
    cd<-get(paste0("SELECT itemid,label,linksto,unitname FROM ",scan(cdfile)," WHERE itemid IN (",paste(q(chartmap$itemid),collapse=","),")"))
    if(anyDuplicated(ld$itemid)||anyDuplicated(cd$itemid))stop("原始字典itemid重复。")
    im<-itemmap
    im$dictionary_label<-ifelse(im$origin=="lab",ld$label[match(im$itemid,ld$itemid)],cd$label[match(im$itemid,cd$itemid)])
    im$dictionary_unit<-ifelse(im$origin=="chart",cd$unitname[match(im$itemid,cd$itemid)],NA_character_)
    im$fluid<-ifelse(im$origin=="lab",ld$fluid[match(im$itemid,ld$itemid)],NA_character_)
    im$linksto<-ifelse(im$origin=="chart",cd$linksto[match(im$itemid,cd$itemid)],"labevents")
    im$dictionary_ok<-!is.na(im$dictionary_label)&ifelse(im$origin=="lab",tolower(im$fluid)=="blood",im$linksto=="chartevents")
    csv_out(im,"to_share/03_MIMIC_item_dictionary.csv")
    if(any(!im$dictionary_ok %in% TRUE))stop("原始字典缺少指定项目或来源/标本不匹配；详见03_MIMIC_item_dictionary.csv。")
    DBI::dbWriteTable(con,"item_map",im,overwrite=TRUE)
    csv_out(rules,"to_share/03_common_component_rules.csv")
    current_module<<-"MIMIC raw laboratory and chart scan"
    for(kind in c("lab","chart")) {
      p<-if(kind=="lab")labfile else chartfile
      logmsg(paste0("扫描MIMIC ",if(kind=="lab")"labevents" else "chartevents","：仅保留目标患者和0-48小时指定项目。原始大表扫描可能较慢。"))
      nm<-require_cols(p,c("subject_id","hadm_id","itemid","charttime","storetime","value","valuenum","valueuom",if(kind=="chart")"stay_id"))
      record_id<-if(kind=="lab"&&"labevent_id"%in%nm)"CAST(x.labevent_id AS VARCHAR)" else "CAST(NULL AS VARCHAR)"
      warning<-if("warning"%in%nm)"TRY_CAST(x.warning AS INTEGER)" else "CAST(NULL AS INTEGER)"
      source_join<-if(kind=="chart")" AND x.stay_id=c.stay_id" else ""
      sql<-paste0("SELECT c.stay_id,c.subject_id,c.hadm_id,",record_id," AS record_id,m.itemid,m.origin,m.variable,m.unit,m.lower,m.upper,
        m.dictionary_unit,m.dictionary_label,CAST(x.valueuom AS VARCHAR) AS raw_unit,
        CAST(x.value AS VARCHAR) AS raw_text,TRY_CAST(x.valuenum AS DOUBLE) AS raw_numeric,
        epoch(TRY_CAST(x.charttime AS TIMESTAMP)-CAST(c.intime AS TIMESTAMP))/3600.0 AS hour,
        epoch(TRY_CAST(x.storetime AS TIMESTAMP)-CAST(c.intime AS TIMESTAMP))/3600.0 AS store_hour,
        (x.storetime IS NULL OR LOWER(TRIM(x.storetime)) IN ('','na','null')) AS store_missing,
        ",warning," AS warning
        FROM ",scan(p)," x JOIN mimic_base c ON x.subject_id=c.subject_id AND x.hadm_id=c.hadm_id",source_join,
        " JOIN item_map m ON x.itemid=m.itemid AND m.origin=",q(kind),"
        WHERE TRY_CAST(x.charttime AS TIMESTAMP) BETWEEN CAST(c.intime AS TIMESTAMP)
          AND CAST(c.intime AS TIMESTAMP)+INTERVAL '48 hours'")
      run(paste0(if(kind=="lab")"CREATE TEMP TABLE raw_mimic AS " else "INSERT INTO raw_mimic ",sql))
    }
    current_module<<-"MIMIC units and availability repair"
    uc<-get("SELECT itemid,origin,variable,unit,lower,upper,dictionary_unit,raw_unit,COUNT(*) AS n_rows,
      COUNT(DISTINCT stay_id) AS n_patients FROM raw_mimic
      GROUP BY itemid,origin,variable,unit,lower,upper,dictionary_unit,raw_unit")
    uc<-map_mimic_units(uc);csv_out(uc,"to_share/04_unit_decisions.csv")
    DBI::dbWriteTable(con,"unit_mapping",uc,overwrite=TRUE)
    run("CREATE TEMP TABLE decisions AS SELECT r.*,u.factor,u.shift,u.unit_basis,
      r.raw_numeric*u.factor+u.shift AS value,
      GREATEST(r.hour,COALESCE(r.store_hour,r.hour)) AS available_hour,
      CASE WHEN u.decision<>'KEEP' THEN u.decision
        WHEN r.store_hour IS NULL AND NOT r.store_missing THEN 'INVALID_STORETIME'
        WHEN r.variable LIKE 'gcs_%' AND regexp_matches(LOWER(COALESCE(r.raw_text,'')),
          'ett|trach|intubat|sedat|unable|unassess') THEN 'GCS_UNASSESSABLE'
        WHEN regexp_matches(COALESCE(r.raw_text,''),'[<>≤≥]') THEN 'QUALIFIED_VALUE_NOT_EXACT'
        WHEN NOT COALESCE(isfinite(r.raw_numeric),FALSE) THEN 'NO_NUMERIC_VALUE'
        WHEN r.raw_numeric*u.factor+u.shift<r.lower OR r.raw_numeric*u.factor+u.shift>r.upper THEN 'OUTSIDE_BROAD_RANGE'
        WHEN r.unit='score' AND r.raw_numeric<>FLOOR(r.raw_numeric) THEN 'NONINTEGER_SCORE'
        ELSE 'KEEP' END AS row_decision
      FROM raw_mimic r JOIN unit_mapping u ON r.itemid=u.itemid AND r.origin=u.origin
       AND r.raw_unit IS NOT DISTINCT FROM u.raw_unit")
    csv_out(get("SELECT origin,variable,row_decision,COUNT(*) AS n_rows,COUNT(DISTINCT stay_id) AS n_patients
      FROM decisions GROUP BY origin,variable,row_decision ORDER BY origin,variable,row_decision"),"to_share/04_record_decisions.csv")
    run("CREATE TABLE MIMIC_measurements AS SELECT DISTINCT stay_id,subject_id,hadm_id,record_id,
      itemid,origin,variable,unit,value,hour,available_hour,unit_basis,warning,
      CASE WHEN store_missing THEN 'CHART_TIME_ONLY_STORETIME_MISSING' ELSE 'CHART_AND_STORE_TIME' END AS timing_basis
      FROM decisions WHERE row_decision='KEEP'")
    csv_out(get("SELECT origin,variable,COUNT(*) AS n_rows,
      SUM(CASE WHEN store_missing THEN 1 ELSE 0 END) AS storetime_missing_rows,
      SUM(CASE WHEN store_hour<hour THEN 1 ELSE 0 END) AS store_before_chart_rows,
      SUM(CASE WHEN hour<=6 AND available_hour>6 THEN 1 ELSE 0 END) AS delayed_beyond_6h,
      SUM(CASE WHEN hour<=24 AND available_hour>24 THEN 1 ELSE 0 END) AS delayed_beyond_24h,
      SUM(CASE WHEN hour<=48 AND available_hour>48 THEN 1 ELSE 0 END) AS delayed_beyond_48h
      FROM decisions WHERE row_decision='KEEP' GROUP BY origin,variable"),"to_share/05_availability_time_audit.csv")
    # 与eICU相同：仅同一记录时刻的三个完整无矛盾组件相加，保留最晚组件可用时间。
    run("CREATE TEMP TABLE gcs AS SELECT stay_id,MAX(subject_id) AS subject_id,MAX(hadm_id) AS hadm_id,hour,
      MAX(CASE WHEN variable='gcs_eye' THEN value END) AS eye,
      MAX(CASE WHEN variable='gcs_motor' THEN value END) AS motor,
      MAX(CASE WHEN variable='gcs_verbal' THEN value END) AS verbal,
      COUNT(DISTINCT CASE WHEN variable='gcs_eye' THEN value END) AS ne,
      COUNT(DISTINCT CASE WHEN variable='gcs_motor' THEN value END) AS nm,
      COUNT(DISTINCT CASE WHEN variable='gcs_verbal' THEN value END) AS nv,
      MAX(available_hour) AS available_hour
      FROM MIMIC_measurements WHERE variable IN ('gcs_eye','gcs_motor','gcs_verbal') GROUP BY stay_id,hour")
    csv_out(get("SELECT COUNT(*) AS component_time_groups,
      SUM(CASE WHEN ne=1 AND nm=1 AND nv=1 THEN 1 ELSE 0 END) AS complete_unambiguous_groups,
      SUM(CASE WHEN ne>1 OR nm>1 OR nv>1 THEN 1 ELSE 0 END) AS conflicting_groups FROM gcs"),"to_share/05_GCS_reconstruction.csv")
    run("INSERT INTO MIMIC_measurements SELECT stay_id,subject_id,hadm_id,NULL,'SAME_TIME_COMPONENT_SUM','chart',
      'gcs_total','score',eye+motor+verbal,hour,available_hour,'DIMENSIONLESS_COMPONENT_SUM',NULL,
      'DERIVED_FROM_SAME_CHART_TIME_COMPONENTS' FROM gcs WHERE ne=1 AND nm=1 AND nv=1")
    csv_out(data.frame(cache_action="CREATED_FRESH_EXTRACTION",
      mimic_rows=as.numeric(get("SELECT COUNT(*) AS n FROM MIMIC_measurements")$n),
      derived_gcs_rows=as.numeric(get("SELECT COUNT(*) AS n FROM MIMIC_measurements WHERE variable='gcs_total'")$n),
      mounted_raw_files=4,large_raw_hash="not computed; file sizes and dictionary MD5 recorded"),"to_share/02_extraction_cache.csv")
    }
    # 原版扫描临时表仍占用内存；汇总之前显式释放并保存可复用的提取阶段。
    for(tab in c("gcs","decisions","raw_mimic"))run(paste("DROP TABLE IF EXISTS",tab))
    DBI::dbWriteTable(con,"extraction_checkpoint",data.frame(stage="MIMIC_EXTRACTION_COMPLETE",
      definition_version="Step4_original_extraction",writer="Step4-1"),overwrite=TRUE)
    run("CHECKPOINT")
    efile<-file.path(step3,"local_only/Step3_eICU_measurements.duckdb")
    if(!file.exists(efile))stop("缺少Step3 eICU数据库；请不要只保留反馈ZIP。")
    run(paste0("ATTACH ",q(efile)," AS eicu_step3 (READ_ONLY)"))
    run("CREATE TEMP VIEW all_measurements AS
      SELECT 'MIMIC' AS source,stay_id,hour,available_hour,variable,unit,value FROM MIMIC_measurements
      UNION ALL SELECT 'eICU',CAST(patientunitstayid AS VARCHAR),hour,available_hour,variable,unit,value
       FROM eicu_step3.measurements")
    bad<-get("SELECT a.source,a.variable,a.unit,COUNT(*) AS n FROM all_measurements a
      LEFT JOIN component_rules r ON a.variable=r.variable AND a.unit=r.unit
      WHERE r.variable IS NULL GROUP BY a.source,a.variable,a.unit")
    if(nrow(bad)) {csv_out(bad,"to_share/05_unmatched_units.csv");stop("两库存在未统一的变量/单位。")}
    current_module<<-"common dynamic feature construction"
    logmsg(paste0("用同一算法分批生成两库窗口特征，每批最多",cfg$PATIENTS_PER_BATCH,
      "名患者；按真实观测时点计算均值、SD、异常比例和斜率。"))
    windows<-data.frame(window=c("0-6h","0-24h","0-48h","6-24h","24-48h"),
      start_hour=c(0,0,0,6,24),end_hour=c(6,24,48,24,48),include_start=c(1L,1L,1L,0L,0L))
    DBI::dbWriteTable(con,"windows",windows,overwrite=TRUE)
    # 相同观察时刻先按不同数值取中位数，避免C/F双重记载、重复上报改变统计权重。
    # 每个窗口先筛可用时间，再做同刻合并，避免后到记录改变早期窗口数值。
    batched<-build_features_batched(con,cfg$PATIENTS_PER_BATCH,logmsg)
    csv_out(batched$same_time,"to_share/06_same_time_record_summary.csv")
    csv_out(batched$batches,"to_share/06_batch_execution.csv")
    av<-get("SELECT source,landmark,\"window\",variable,COUNT(*) AS cohort_n,
      SUM(CASE WHEN n_chart_times>0 THEN 1 ELSE 0 END) AS measured_patients,
      AVG(CASE WHEN n_chart_times>0 THEN 1.0 ELSE 0.0 END) AS availability,
      SUM(CASE WHEN slope_per_hour IS NOT NULL THEN 1 ELSE 0 END) AS slope_available_patients
      FROM feature_summary GROUP BY source,landmark,\"window\",variable ORDER BY source,landmark,\"window\",variable")
    csv_out(av,"to_share/07_common_feature_availability.csv")
    # eICU可用率必须复现Step3；同刻合并仅改变数值权重，不能丢失患者。
    prev<-read_csv(file.path(step3,"to_share/07_repaired_availability.csv"),c("window","variable","measured_patients"))
    now<-av[av$source=="eICU"&av$landmark=="T48",]
    j<-match(paste(prev$window,prev$variable),paste(now$window,now$variable))
    if(anyNA(j)||any(num(prev$measured_patients)!=now$measured_patients[j]))stop("eICU变量可用率未复现Step3；停止交付特征。")
    csv_out(get("SELECT source,landmark,\"window\",variable,
      MEDIAN(mean_value) AS median_patient_mean,quantile_cont(mean_value,0.01) AS p01_patient_mean,
      quantile_cont(mean_value,0.99) AS p99_patient_mean,
      MEDIAN(CASE WHEN n_chart_times>0 THEN n_chart_times END) AS median_chart_times_if_measured,
      MEDIAN(slope_per_hour) AS median_slope_per_hour
      FROM feature_summary GROUP BY source,landmark,\"window\",variable"),"to_share/07_dynamic_feature_profile.csv")
    csv_out(get("SELECT c.source,c.landmark,c.in_index_icu,f.window,f.variable,COUNT(*) AS cohort_n,
      SUM(CASE WHEN f.n_chart_times>0 THEN 1 ELSE 0 END) AS measured_patients,
      AVG(CASE WHEN f.n_chart_times>0 THEN 1.0 ELSE 0.0 END) AS availability
      FROM cohorts c JOIN feature_summary f ON c.source=f.source AND c.stay_id=f.stay_id AND c.landmark=f.landmark
      GROUP BY c.source,c.landmark,c.in_index_icu,f.window,f.variable"),"to_share/08_availability_by_ICU_status.csv")
    fc<-get("SELECT source,landmark,COUNT(DISTINCT stay_id) AS patients_with_feature_rows,
      COUNT(*) AS feature_rows FROM feature_summary GROUP BY source,landmark")
    for(i in seq_len(nrow(fc))) {
      co<-cohorts[cohorts$source==fc$source[i]&cohorts$landmark==fc$landmark[i],,drop=FALSE]
      nw<-vapply(co$cutoff_hour,function(h)sum(windows$end_hour<=h),numeric(1))
      if(fc$patients_with_feature_rows[i]!=nrow(co)||fc$feature_rows[i]!=sum(nw)*nrow(rules))
        stop("分批结果的患者数或特征行数未复现冻结队列。")
    }
    if(nrow(fc)!=4L)stop("分批结果缺少必要队列。")
    csv_out(fc,"to_share/02_feature_cohort_check.csv")
    # 额外逐人宽表：值缺失保持NA，只有次数/缺失指示明确为0或1。
    logmsg("保存患者级长表数据库和各landmark宽表；这些文件仅保存在local_only。")
    for(src in c("MIMIC","eICU"))for(lm in if(src=="MIMIC")c("T6","T24","T48") else "T48") {
      f<-get(paste0("SELECT * FROM feature_summary WHERE source=",q(src)," AND landmark=",q(lm)))
      wide<-cohorts[cohorts$source==src&cohorts$landmark==lm,,drop=FALSE]
      stats<-c("n_chart_times","mean_value","median_value","min_value","max_value","sd_value",
        "first_value","last_value","slope_per_hour","abnormal_fraction","missing_flag")
      for(win in windows$window[windows$end_hour<=as.numeric(sub("T","",lm))])for(v in rules$variable) {
        d<-f[f$window==win&f$variable==v,,drop=FALSE];j<-match(wide$stay_id,d$stay_id)
        if(anyNA(j)||anyDuplicated(d$stay_id))stop("宽表连接键不唯一或不完整。")
        for(st in stats)wide[[paste(v,gsub("-","_",win),st,sep="__")]]<-d[[st]][j]
      }
      saveRDS(wide,reg(paste0("local_only/",src,"_",lm,"_numeric_features.rds")),compress="gzip")
      rm(f,wide);gc(verbose=FALSE)
    }
    current_module<<-"Figure 1C and three-line table"
    a<-av[av$source=="MIMIC"&av$landmark=="T48",,drop=FALSE]
    n48<-sum(mr$landmark=="T48")
    make_mimic_visuals(a,NULL,n48,make_png=isTRUE(cfg$MAKE_PNG))
    csv_out(sy[sy$source=="MIMIC"|sy$landmark=="T48",],"to_share/02_frozen_cohort_reference.csv")
    csv_out(data.frame(feature=c("mean/SD/min/max/median","slope_per_hour","abnormal_fraction","n_chart_times","missing_flag"),
      definition=c("computed from same-time medians of distinct available numeric values",
      "OLS over observed chart times; at least two distinct times spanning >=1 hour; units per hour",
      "proportion of chart times outside fixed analysis thresholds; missing for analytes without a rule",
      "number of distinct chart times; zero when unmeasured","one when no valid available measurement exists")),
      "to_share/10_feature_definitions.csv")
    txt_out(c("MIMIC原始labevents/chartevents按准确itemid、单位、记录时间及storetime重新处理。",
      "主分母为Step2冻结队列，不读取旧模型分型或风险；eICU复用Step3清洁数值。",
      "MIMIC存在真正T6/T24/T48队列；eICU仅T48，不能把其早期特征称为早期外部验证。",
      "化验以该次住院hadm_id关联，可包含转出ICU后的住院化验；生命体征仅来自该次ICU。",
      "未带hadm_id的MIMIC化验未按患者号跨住院补配；与既定项目关联范围一致。",
      "早期窗口先按可用时间过滤，再合并同刻重复，避免晚到数值改变早期特征。",
      "同一时刻的不同数值取中位数；多个项目、来源或C/F双重记录不增加该时刻统计权重。",
      "GCS只合成同刻完整无矛盾组件；不把气管插管/不能评估记为正常15分。",
      "均值、SD和异常比例直接用观测时点计算；斜率为值/小时，不把累计窗口均值再平均。",
      "修复版按来源和患者分批，所有患者各自的完整记录处于同一批；统计定义和Figure/Table位置不变。",
      "每次重跑覆盖本步骤的下游结果；先验证提取缓存，通过后跳过原始大表扫描。",
      "肌钙蛋白I/T分开；无可靠ULN时不生成肌钙蛋白异常比例。",
      "描述性异常阈值沿用既定分析定义，不是器官评分权重，也不是临床行动阈值。",
      "没有插补、标准化、训练评分映射、聚类或拟合预测模型；这些必须在训练折内完成。",
      "原始大表未计算MD5以避免重复整表读取；文件路径、大小及小文件MD5均保留。",
      "本步采用严格计时而非事后完整信息；原始storetime缺失的记录仍保留并明确标记。",
      "回传Step4_feedback.zip；患者数据库和RDS留在local_only。"
    ),"to_share/00_READ_ME_FIRST.txt")
    txt_out(c("https://mimic.mit.edu/docs/iv/modules/hosp/labevents.html",
      "https://mimic.mit.edu/docs/iv/modules/icu/chartevents.html",
      "https://mimic.mit.edu/docs/iv/modules/icu/d_items.html",
      "https://github.com/MIT-LCP/mimic-code/blob/main/mimic-iv/concepts/measurement/gcs.sql",
      "https://duckdb.org/docs/stable/guides/performance/how_to_tune_workloads",
      "https://duckdb.org/docs/stable/guides/troubleshooting/oom_errors",
      "GCS item identifiers follow source documentation; this study uses complete same-time components and does not adopt the official score-query normal-value fallback."),
      "to_share/10_definition_sources.txt")
    issue("PENDING","remaining work","治疗代理和基线临床信息仍需按时间整理；随后在训练集内拟合器官评分、聚类和预测模型。")
    run("CHECKPOINT")
    status<<-"COMPLETED_FOR_REVIEW"
  }
  failure_message <- NULL
  tryCatch(main(), error = function(err) {
    status <<- "FAILED"
    failure_message <<- conditionMessage(err)
    issue("ERROR", current_module, "此模块未完成；详细错误仅记录于local_only，避免原始数据值进入反馈。")
    txt_out(c(paste0("Module: ", current_module), conditionMessage(err)), "local_only/Step4_error_detail.txt")
  })
  for (nm in ls(agg)) csv_out(do.call(rbind, agg[[nm]]), paste0("to_share/", nm, ".csv"))
  txt_out(c(paste0("STATUS = ", status),"SCRIPT_VERSION = Step4-1",
            "COMPLETED_FOR_REVIEW表示本步两库数值特征构建完成、等待审阅，不表示模型已重建。",
            "FAILED表示本次不完整，不要使用local_only中的部分结果建模。"), "to_share/Step4_STATUS.txt")
  txt_out(capture.output(sessionInfo()), "to_share/Step4_sessionInfo.txt")
  logmsg(paste0("Step4状态：", status))
  if (status == "FAILED") logmsg("请回传本次反馈包；若需要完整错误，查看local_only/Step4_error_detail.txt。")
  zip_ok <- FALSE
  if (isTRUE(cfg$MAKE_ZIP)) {
    zp <- reg("Step4_feedback.zip")
    share <- file.path(out, "to_share")
    files <- sort(basename(registered[startsWith(registered, "to_share/") & file.exists(file.path(out,registered))]))
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
  if (zip_ok) cat("请回传：\n", file.path(out, "Step4_feedback.zip"), "\n", sep = "")
  else cat("请压缩并回传：\n", file.path(out, "to_share"), "\n", sep = "")
  if (!is.null(failure_message)) cat("本次未完成：", failure_message, "\n")
  invisible(list(output_dir = out, status = status, zip_ok = zip_ok))
})
