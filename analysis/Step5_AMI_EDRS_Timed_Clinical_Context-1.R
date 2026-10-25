# AMI-EDRS revision | Step5: time-aware clinical documentation and baseline context
# 运行：source(file.choose(), encoding="UTF-8")。先接上T7。
# 复用Step4冻结名单；不重新扫描labevents/chartevents，不拟合模型。
# 提取给药记录、治疗记录及既往病史代理；区分未记录与证实未治疗。
# PCI/CABG的日期级ICD编码仅作描述性补充，不作为小时级预测输入。
# 固定输出 修改/Step5；重跑覆盖本步骤登记文件，无时间戳。
# local_only保留患者记录；反馈仅含聚合计数及公开项目字典。
# Step5-1：固定CSV分隔符、双引号和转义规则；严格校验，不跳过坏行。
local({
  cfg <- list(
    MIMIC_PROJECT=Sys.getenv("AMI_MIMIC_PROJECT", unset=file.path(getwd(), "work/MIMIC")),
    EICU_PROJECT=Sys.getenv("AMI_EICU_PROJECT", unset=file.path(getwd(), "work/eICU")),
    OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    RAW_MIMIC_DIR=Sys.getenv("AMI_MIMIC_RAW", unset=file.path(getwd(), "data/mimic-iv-3.1")), RAW_EICU_DIR=Sys.getenv("AMI_EICU_RAW", unset=file.path(getwd(), "data/eicu-crd-2.0")),
    RAW_FILES=list(), MEMORY_LIMIT="2GB", MAKE_ZIP=TRUE, MAKE_PNG=TRUE
  )
  cfg <- utils::modifyList(cfg, getOption("ami_step5.config", list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in c("MIMIC_PROJECT", "OUTPUT_PARENT")) cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step5")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step5输出目录。")
  if (is_link(out)) stop("Step5输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP5")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step5目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP5_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP5_MANAGED_OUTPUT")) {
    stop("Step5输出目录的归属标记不正确。")
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
  old <- character()
  if (file.exists(manifest)) {
    old <- unique(readLines(manifest, warn = FALSE))
    for (rel in old[nzchar(old)]) {
      p <- safe_path(rel)
      if (file.exists(p)) {
        if (dir.exists(p)) stop("登记的输出应为文件，不能为目录。")
        if (unlink(p) != 0L) stop("不能覆盖旧Step5输出；请关闭正在使用它的程序。")
      }
    }
  }
  writeLines(character(), manifest)
  registered <- character()
  reg <- function(rel) {
    p <- safe_path(rel)
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    if (!(rel %in% registered)) {
      if(file.exists(p) && !(rel %in% old))stop("同名文件未登记，不能覆盖：",rel)
      registered <<- c(registered, rel)
      writeLines(registered, manifest)
    }
    p
  }
  csv_out <- function(d, rel) utils::write.csv(d, reg(rel), row.names = FALSE, na = "", fileEncoding = "UTF-8")
  txt_out <- function(x, rel) writeLines(enc2utf8(x), reg(rel), useBytes = TRUE)
  logp <- reg("to_share/Step5_run_log.txt")
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
  # 文本规则只识别明确记录；原文证据全部留在本地，供复核。
  clean_text <- function(x) {z<-tolower(trimws(as.character(x)));z[is.na(z)]<-"";gsub("[[:space:]]+"," ",z)}
  negated <- function(x) grepl("\\b(no|not|without|denies|denied|negative|absent|refused|declined|planned|scheduled|consider|candidate)\\b|rule out|not obtainable|unable to obtain|unknown|family history",clean_text(x),perl=TRUE)
  drug_rules <- c(norepinephrine="\\b(norepinephrine|noradrenaline|levophed)\\b",
    epinephrine="\\b(epinephrine|adrenaline)\\b",phenylephrine="\\b(phenylephrine|neosynephrine|neo-synephrine)\\b",
    vasopressin="\\bvasopressin\\b",dopamine="\\bdopamine\\b",dobutamine="\\bdobutamine\\b",milrinone="\\bmilrinone\\b")
  treatment_rules <- c(pci="\\bpci\\b|percutaneous coronary intervention|coronary angioplasty|coronary stent|ptca",
    cabg="\\bcabg\\b|coronary artery bypass|coronary bypass graft",
    iabp="\\biabp\\b|intra[- ]?aortic balloon",impella="\\bimpella\\b",
    ecmo="\\becmo\\b|extracorporeal membrane oxygenation",
    diagnostic_cath="cardiac cath|coronary angiograph|coronary catheter")
  history_rules <- c(prior_cad="\\bcad\\b|coronary artery disease|ischemic heart disease|ischaemic heart disease|myocardial infarction|\\bcabg\\b|coronary.*stent|angina",
    heart_failure="heart failure|\\bchf\\b|cardiomyopathy",diabetes="diabet",
    hypertension="hypertension|\\bhtn\\b",chronic_kidney_disease="chronic renal|chronic kidney|\\besrd\\b|\\bckd\\b|dialysis",
    copd="\\bcopd\\b|chronic obstructive|emphysema|chronic bronchitis",
    stroke="\\bstroke\\b|\\bcva\\b|cerebrovascular accident|cerebral infarct|transient ischemic|transient ischaemic",
    atrial_fibrillation="atrial fibrillation|atrial flutter")
  history_icd9 <- c(prior_cad="^41[0-4]",heart_failure="^428",diabetes="^250",hypertension="^40[1-5]",
    chronic_kidney_disease="^585",copd="^49[0-6]",stroke="^43[0-8]",atrial_fibrillation="^4273[12]")
  history_icd10 <- c(prior_cad="^I2[0-5]",heart_failure="^I50",diabetes="^E(0[89]|1[0-3])",hypertension="^I1[0-5]",
    chronic_kidney_disease="^N18",copd="^J4[0-4]|^J47",stroke="^I6[0-9]|^G45",atrial_fibrillation="^I48")
  classify <- function(x,rules,exclude_history=FALSE) {
    z<-clean_text(x);bad<-negated(z)
    if(exclude_history)bad<-bad|grepl("history of|status post|s/p|previous|prior",z,perl=TRUE)
    ans<-lapply(names(rules),function(v){i<-which(!bad&grepl(rules[[v]],z,perl=TRUE));data.frame(row=i,variable=rep(v,length(i)))})
    do.call(rbind,ans)
  }
  age_value <- function(x) {z<-trimws(as.character(x));a<-num(z);a[z%in%c("> 89",">89")]<-90;a[!is.finite(a)|a<18|a>120]<-NA_real_;a}
  sex_value <- function(x) {z<-clean_text(x);ifelse(z%in%c("m","male"),"Male",ifelse(z%in%c("f","female"),"Female",NA_character_))}
  # gzip自动采样可能只见到前段无引号记录，误把后段文本逗号识别为新列。
  # 对MIMIC/eICU标准CSV统一明确指定规则；仍遇到真正坏行时停止，不删行或补列。
  raw_csv_sql <- function(p,sql_quote) paste0("read_csv(",sql_quote(p),
    ",delim=',',quote='\"',escape='\"',header=true,all_varchar=true,",
    "strict_mode=true,null_padding=false,ignore_errors=false)")
  csv_preflight <- function(cc) {
    p<-tempfile("Step5_csv_preflight_",fileext=".csv.gz")
    badp<-tempfile("Step5_csv_invalid_",fileext=".csv.gz")
    on.exit(unlink(c(p,badp)),add=TRUE)
    header<-paste(c("pasthistoryid","patientunitstayid","pasthistoryoffset","pasthistoryenteredoffset",
      "pasthistorynotetype","pasthistorypath","pasthistoryvalue","pasthistoryvaluetext"),collapse=",")
    plain<-"900001,700001,17,50,Comprehensive Progress,plain path,plain value,plain text"
    qt<-function(x)paste0('"',gsub('"','""',x,fixed=TRUE),'"')
    label<-"Anthracyclines (adriamycin, daunorubicin)"
    path<-paste0("notes/Progress Notes/Past History/Organ Systems/Hematology/Oncology (R)/Cancer Therapy/Chemotherapy/",label)
    edge1<-paste(c("900002","700002","17","50","Comprehensive Progress",qt(path),qt(label),qt(label)),collapse=",")
    edge2<-paste(c("900003","700003","17","50","Comprehensive Progress",qt('quoted "term", with comma'),qt("line 1\nline 2"),""),collapse=",")
    edge3<-paste(c("900004","700004","17","50","Comprehensive Progress",qt("病史 α, β"),"yes","text"),collapse=",")
    write_gz<-function(lines,path){gz<-gzfile(path,"wb");tryCatch(writeLines(enc2utf8(lines),gz,useBytes=TRUE),finally=close(gz))}
    # 第一条带逗号的引号记录恰好在物理行25751，超过默认20480行抽样范围。
    write_gz(c(header,rep(plain,25749L),edge1,edge2,edge3),p)
    sql_quote<-function(x)as.character(DBI::dbQuoteString(cc,x))
    DBI::dbExecute(cc,paste("CREATE TEMP TABLE csv_fixture AS SELECT * FROM",raw_csv_sql(p,sql_quote)))
    on.exit(DBI::dbExecute(cc,"DROP TABLE IF EXISTS csv_fixture"),add=TRUE)
    z<-DBI::dbGetQuery(cc,"SELECT * FROM csv_fixture WHERE pasthistoryid<>'900001' ORDER BY pasthistoryid")
    stopifnot(DBI::dbGetQuery(cc,"SELECT COUNT(*) AS n FROM csv_fixture")$n==25752,
      ncol(z)==8,nrow(z)==3,z$pasthistorypath[1]==path,z$pasthistoryvalue[1]==label,
      z$pasthistoryvaluetext[1]==label,z$pasthistorypath[2]=='quoted "term", with comma',
      z$pasthistoryvalue[2]=="line 1\nline 2",is.na(z$pasthistoryvaluetext[2]),z$pasthistorypath[3]=="病史 α, β")
    write_gz(c(header,rep(plain,25749L),paste0(plain,",EXTRA_COLUMN")),badp)
    invalid<-try(DBI::dbGetQuery(cc,paste("SELECT * FROM",raw_csv_sql(badp,sql_quote))),silent=TRUE)
    stopifnot(inherits(invalid,"try-error"))
    TRUE
  }
  clinical_vars <- c("vasoactive",names(treatment_rules),names(history_rules))
  pretty <- c(vasoactive="Vasoactive infusion",pci="PCI documentation",cabg="CABG documentation",iabp="IABP documentation",
    impella="Impella documentation",ecmo="ECMO documentation",diagnostic_cath="Diagnostic cath / angiography",
    prior_cad="Prior CAD / MI record",heart_failure="Heart failure history",diabetes="Diabetes history",
    hypertension="Hypertension history",chronic_kidney_disease="CKD history",copd="COPD history",
    stroke="Stroke / TIA history",atrial_fibrillation="Atrial fibrillation / flutter history")
  feature_sql <- function() "CREATE TABLE clinical_features_long AS
    SELECT c.source,c.stay_id,c.landmark,s.variable,s.stream,s.source_enabled,
      CASE WHEN MIN(e.chart_hour) IS NOT NULL THEN 1
        WHEN s.source_enabled=1 AND MIN(k.available_hour)<=c.cutoff_hour THEN 0 ELSE NULL END AS documented_positive,
      CASE WHEN s.source_enabled=1 AND MIN(k.available_hour)<=c.cutoff_hour THEN 1 ELSE 0 END AS patient_source_covered,
      MIN(e.chart_hour) AS first_record_hour,MIN(e.available_hour) AS first_available_hour,
      COUNT(DISTINCT e.evidence_type) AS evidence_types
    FROM cohorts c JOIN source_spec s ON c.source=s.source
    LEFT JOIN coverage k ON c.source=k.source AND c.stay_id=k.stay_id AND s.stream=k.stream
    LEFT JOIN evidence e ON c.source=e.source AND c.stay_id=e.stay_id AND s.variable=e.variable
      AND e.chart_hour<=c.cutoff_hour AND e.available_hour<=c.cutoff_hour
    GROUP BY c.source,c.stay_id,c.landmark,c.cutoff_hour,s.variable,s.stream,s.source_enabled"
  preflight <- function() {
    stopifnot(nrow(classify("norepinephrine",drug_rules))==1,
      classify("norepinephrine",drug_rules)$variable=="norepinephrine",
      nrow(classify(c("cardiac catheterization","coronary angiography"),treatment_rules))==2,
      !"pci"%in%classify("cardiac catheterization",treatment_rules)$variable,
      nrow(classify(c("planned PCI","no CABG","history of CABG"),treatment_rules,TRUE))==0,
      "pci"%in%classify("cardiovascular | coronary angioplasty",treatment_rules,TRUE)$variable,
      nrow(classify("no diabetes",history_rules))==0,age_value("> 89")==90,is.na(sex_value("Unknown")))
    cc<-DBI::dbConnect(duckdb::duckdb(),dbdir=":memory:");on.exit(DBI::dbDisconnect(cc,shutdown=TRUE))
    stopifnot(csv_preflight(cc))
    DBI::dbWriteTable(cc,"cohorts",data.frame(source="MIMIC",stay_id=c("late","empty","covered"),landmark="T6",cutoff_hour=6))
    DBI::dbWriteTable(cc,"source_spec",data.frame(source="MIMIC",variable=c("pci","cabg"),stream="procedure",source_enabled=c(1L,0L)))
    DBI::dbWriteTable(cc,"coverage",data.frame(source="MIMIC",stay_id=c("late","covered"),stream="procedure",available_hour=c(0,0)))
    DBI::dbWriteTable(cc,"evidence",data.frame(source="MIMIC",stay_id="late",variable="pci",chart_hour=2,available_hour=7,evidence_type="fixture"))
    DBI::dbExecute(cc,feature_sql());z<-DBI::dbGetQuery(cc,"SELECT * FROM clinical_features_long")
    stopifnot(nrow(z)==6,all(is.na(z$documented_positive[z$variable=="cabg"])),
      is.na(z$documented_positive[z$stay_id=="empty"&z$variable=="pci"]),
      z$documented_positive[z$stay_id=="late"&z$variable=="pci"]==0)
    TRUE
  }
  make_visuals <- function(a) {
    open_pdf<-function(path,width,height){
      if(isTRUE(capabilities("cairo")))grDevices::cairo_pdf(path,width=width,height=height,family="Arial")
      else grDevices::pdf(path,width=width,height=height,family="Helvetica",useDingbats=FALSE)
    }
    aa<-a[a$landmark=="T48",,drop=FALSE]
    vars<-clinical_vars;nr<-length(vars);cols<-c(MIMIC="#4F7FC4",eICU="#D98C57")
    draw<-function(){
      op<-par(no.readonly=TRUE);on.exit(par(op));par(mar=c(6,13,4,1),family="sans",xaxs="i",yaxs="i")
      plot(NA,xlim=c(0,100),ylim=c(.3,nr+.7),xaxt="n",yaxt="n",xlab="Patients with documented positive evidence (%)",ylab="",bty="n")
      abline(v=seq(0,100,25),col="#E6EAF0",lwd=.8);axis(1,at=seq(0,100,25));axis(2,at=nr:1,labels=pretty[vars],las=1,tick=FALSE,cex.axis=.75)
      for(s in names(cols))for(i in seq_along(vars)){
        d<-aa[aa$source==s&aa$variable==vars[i],,drop=FALSE];y<-nr-i+1+if(s=="MIMIC").16 else -.16
        if(nrow(d)!=1)stop("绘图汇总不完整。")
        if(d$source_enabled==0)text(1,y,"Source not established",adj=0,cex=.58,col="#777777") else {
          rect(0,y-.12,100*d$positive_n/d$cohort_n,y+.12,col=cols[s],border=NA)
          pct<-100*d$positive_n/d$cohort_n
          text(if(pct>90)pct-1 else pct+1,y,sprintf("%.1f",pct),adj=if(pct>90)1 else 0,cex=.64)
        }
      }
      title("Clinical documentation by T48",adj=0,cex.main=1.05)
      legend("topright",legend=names(cols),fill=cols,bty="n",cex=.8)
      mtext("Recorded presence, not treatment prevalence. Missing documentation does not confirm absence.",1,line=4.7,cex=.57)
    }
    open_pdf(reg("to_share/Figure_S_clinical_documentation.pdf"),width=9,height=7.5)
    tryCatch(draw(),finally=grDevices::dev.off())
    if(isTRUE(cfg$MAKE_PNG)){grDevices::png(reg("to_share/Figure_S_clinical_documentation.png"),width=9,height=7.5,units="in",res=400);tryCatch(draw(),finally=grDevices::dev.off())}
    tab<-data.frame(Concept=unname(pretty[vars]),stringsAsFactors=FALSE)
    for(s in c("MIMIC","eICU")){
      d<-aa[aa$source==s,,drop=FALSE];d<-d[match(vars,d$variable),]
      tab[[paste(s,"recorded +")]]<-ifelse(d$source_enabled==0,"Not established",sprintf("%s (%.1f%%)",format(d$positive_n,trim=TRUE,big.mark=","),100*d$positive_n/d$cohort_n))
      tab[[paste(s,"source covered")]]<-ifelse(d$source_enabled==0,"Not established",sprintf("%.1f%%",100*d$source_covered_n/d$cohort_n))
    }
    csv_out(tab,"to_share/Table_S_clinical_documentation.csv")
    open_pdf(reg("to_share/Table_S_clinical_documentation.pdf"),width=11.69,height=8.27)
    tryCatch({
      grid::grid.newpage();grid::grid.text("Supplementary table. Clinical documentation by T48",x=.04,y=.95,just="left",gp=grid::gpar(fontsize=13,fontface="bold"))
      grid::grid.text("MIMIC-IV: n = 5,133; eICU: n = 5,284. All proportions use the full frozen cohort.",x=.04,y=.91,just="left",gp=grid::gpar(fontsize=9))
      xs<-c(.04,.45,.60,.78,.96);top<-.87;dy<-.037
      grid::grid.lines(x=c(.04,.97),y=c(top,top),gp=grid::gpar(lwd=1.3))
      for(j in 1:5)grid::grid.text(names(tab)[j],x=xs[j],y=top-.022,just=if(j==1)"left" else "right",gp=grid::gpar(fontsize=8,fontface="bold"))
      grid::grid.lines(x=c(.04,.97),y=c(top-.045,top-.045),gp=grid::gpar(lwd=.8))
      for(i in seq_len(nrow(tab)))for(j in 1:5)grid::grid.text(as.character(tab[i,j]),x=xs[j],y=top-.035-i*dy,just=if(j==1)"left" else "right",gp=grid::gpar(fontsize=8))
      bot<-top-.055-nrow(tab)*dy;grid::grid.lines(x=c(.04,.97),y=c(bot,bot),gp=grid::gpar(lwd=1.3))
      grid::grid.text("Recorded +: at least one positive record by T48. Source covered: at least one time-eligible record in the relevant source stream.",x=.04,y=bot-.03,just="left",gp=grid::gpar(fontsize=8))
      grid::grid.text("No record does not confirm no treatment or no disease. MIMIC history uses previous-admission codes; eICU uses entered past-history records.",x=.04,y=bot-.055,just="left",gp=grid::gpar(fontsize=8))
    },finally=grDevices::dev.off())
    esc<-function(x){x<-gsub("\\","\\\\",x,fixed=TRUE);x<-gsub("{","\\{",x,fixed=TRUE);gsub("}","\\}",x,fixed=TRUE)}
    rtf<-c("{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 Arial;}}\\landscape\\paperw16838\\paperh11906\\margl600\\margr600\\f0\\fs20",
      "\\b Supplementary table. Clinical documentation by T48\\b0\\par MIMIC-IV n=5,133; eICU n=5,284.\\par\\par")
    rows<-rbind(names(tab),as.matrix(tab));ends<-c(5100,7600,10100,12600,15100)
    for(i in seq_len(nrow(rows))){bd<-paste0(if(i==1)"\\clbrdrt\\brdrs\\brdrw15\\clbrdrb\\brdrs\\brdrw10" else "",if(i==nrow(rows))"\\clbrdrb\\brdrs\\brdrw15" else "")
      rtf<-c(rtf,paste0("\\trowd\\trgaph80",paste0(bd,"\\cellx",ends,collapse=""),paste(vapply(1:5,function(j)paste0("\\pard\\intbl",if(j==1)"\\ql " else "\\qr ","\\fs17 ",if(i==1)"\\b " else "\\b0 ",esc(rows[i,j]),"\\cell"),character(1)),collapse=""),"\\row"))}
    rtf<-c(rtf,"\\pard\\fs16 No documentation is not confirmed absence. Source coverage is patient-level record availability, not complete treatment ascertainment.\\par}")
    txt_out(rtf,"to_share/Table_S_clinical_documentation.rtf")
    csv_out(data.frame(output=c("Figure_S_clinical_documentation","Table_S_clinical_documentation"),
      placement=c("ADD supplementary figure: clinical documentation; assign final S number after merging supplements",
      "ADD supplementary three-line table: clinical documentation and source coverage; no existing table replaced")),"to_share/11_figure_table_replacement_map.csv")
  }
  if(isTRUE(getOption("ami_step5.functions_only",FALSE)))return(invisible(list(classify=classify,drug_rules=drug_rules,
    treatment_rules=treatment_rules,history_rules=history_rules,age_value=age_value,sex_value=sex_value,
    feature_sql=feature_sql,make_visuals=make_visuals,clinical_vars=clinical_vars,
    raw_csv_sql=raw_csv_sql,csv_preflight=csv_preflight)))
  status<-"FAILED";current_module<-"input verification"
  main<-function(){
    step4<-file.path(cfg$OUTPUT_PARENT,"Step4")
    if(!file.exists(file.path(step4,"to_share/Step4_STATUS.txt"))||
      !"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(file.path(step4,"to_share/Step4_STATUS.txt")))stop("请先成功完成Step4。")
    inp<-read_csv(file.path(step4,"to_share/01_inputs.csv"),c("file","md5"))
    for(i in seq_len(nrow(inp)))if(!file.exists(inp$file[i])||unname(tools::md5sum(inp$file[i]))!=inp$md5[i])stop("Step4上游输入发生变化。")
    if(!requireNamespace("DBI",quietly=TRUE)||!requireNamespace("duckdb",quietly=TRUE))stop("需要DBI和duckdb。")
    logmsg("小型预检：gzip后段引号/逗号/转义及坏行拦截；晚到记录、未记录/来源缺失、PCI与单纯造影。")
    stopifnot(preflight());txt_out(c("PASS: explicit CSV dialect; 25,752 rows and all 8 fields retained.",
      "PASS: quoted commas first appearing on line 25,751, escaped quotes, embedded newline, UTF-8 and empty trailing field.",
      "PASS: malformed extra-column row is rejected; no ignore_errors or null padding.",
      "PASS: classifier and time-aware clinical documentation fixtures."),"to_share/02_preflight.txt")
    csv_out(data.frame(delimiter=",",quote='"',escape='"',header=TRUE,all_varchar=TRUE,
      strict_mode=TRUE,null_padding=FALSE,ignore_errors=FALSE,scope="All raw MIMIC/eICU CSV and CSV.GZ reads"),"to_share/01_csv_reader_settings.csv")
    dbfile<-reg("local_only/Step5_clinical_context.duckdb");reg("local_only/Step5_clinical_context.duckdb.wal")
    con<-DBI::dbConnect(duckdb::duckdb(),dbdir=dbfile)
    spill<-tempfile("AMI_EDRS_Step5_",tmpdir=tempdir());dir.create(spill)
    on.exit({try(DBI::dbDisconnect(con,shutdown=TRUE),silent=TRUE);unlink(spill,recursive=TRUE)},add=TRUE)
    q<-function(x)as.character(DBI::dbQuoteString(con,as.character(x)));qi<-function(x)as.character(DBI::dbQuoteIdentifier(con,x))
    run<-function(sql)DBI::dbExecute(con,sql);get<-function(sql)DBI::dbGetQuery(con,sql)
    run("SET threads=1");run("SET preserve_insertion_order=false");run(paste("SET memory_limit =",q(cfg$MEMORY_LIMIT)));run(paste("SET temp_directory =",q(spill)))
    f4<-file.path(step4,"local_only/Step4_harmonized_features.duckdb");if(!file.exists(f4))stop("缺少Step4本地数据库。")
    run(paste("ATTACH",q(f4),"AS step4 (READ_ONLY)"))
    run("CREATE TABLE cohorts AS SELECT * FROM step4.cohorts")
    run("CREATE TABLE mimic_base AS SELECT * FROM step4.mimic_base")
    run("CREATE TABLE eicu_base AS SELECT DISTINCT stay_id FROM cohorts WHERE source='eICU'")
    co<-get("SELECT * FROM cohorts ORDER BY source,landmark,stay_id");mr<-get("SELECT * FROM step4.MIMIC_frozen_roster")
    check<-get("SELECT source,landmark,COUNT(*) AS n FROM cohorts GROUP BY source,landmark")
    frozen<-read_csv(file.path(step4,"to_share/02_feature_cohort_check.csv"))
    ix<-match(paste(check$source,check$landmark),paste(frozen$source,frozen$landmark))
    if(anyNA(ix)||any(check$n!=num(frozen$patients_with_feature_rows[ix])))stop("冻结队列不一致。")
    csv_out(check,"to_share/02_cohort_check.csv")
    # 只读明确文件名；不按修改时间选择，也不猜相似特征文件。
    locate<-function(src,stem,subdir="",required=FALSE){
      key<-paste(src,stem,sep=".");explicit<-cfg$RAW_FILES[[key]]
      if(!is.null(explicit)&&nzchar(explicit)){p<-norm(explicit);if(!file.exists(p))stop("指定文件不存在：",p)} else {
        root<-if(src=="MIMIC")cfg$RAW_MIMIC_DIR else cfg$RAW_EICU_DIR
        cand<-file.path(root,subdir,paste0(stem,c(".csv.gz",".csv")));p<-cand[file.exists(cand)]
        if(!length(p)&&dir.exists(root))p<-list.files(root,pattern=paste0("^",stem,"[.]csv([.]gz)?$"),recursive=TRUE,full.names=TRUE,ignore.case=TRUE)
        if(length(p)>1)stop("发现多个",key,"，请用RAW_FILES显式指定。")
      }
      if(!length(p)){emit("01_raw_sources",source=src,table=stem,path="",status="UNAVAILABLE",bytes=NA_real_);if(required)stop("缺少",key,"；请接上T7。") else return(NULL)}
      emit("01_raw_sources",source=src,table=stem,path=norm(p),status="FOUND",bytes=file.info(p)$size);norm(p)
    }
    scan<-function(p)raw_csv_sql(p,q)
    view<-function(p,v,cols){if(is.null(p))return(FALSE);run(paste("CREATE TEMP VIEW",v,"AS SELECT * FROM",scan(p)))
      nn<-names(get(paste("SELECT * FROM",v,"LIMIT 0")));if(length(setdiff(cols,nn)))stop("原始表字段不完整：",basename(p)," / ",paste(setdiff(cols,nn),collapse=","));TRUE}
    run("CREATE TABLE evidence(source VARCHAR,stay_id VARCHAR,variable VARCHAR,chart_hour DOUBLE,available_hour DOUBLE,evidence_type VARCHAR)")
    run("CREATE TABLE coverage(source VARCHAR,stay_id VARCHAR,stream VARCHAR,available_hour DOUBLE)")
    spec<-expand.grid(source=c("MIMIC","eICU"),variable=clinical_vars,stringsAsFactors=FALSE)
    spec$stream<-ifelse(spec$variable=="vasoactive","infusion",ifelse(spec$variable%in%names(history_rules),"history","procedure"));spec$source_enabled<-0L
    enable<-function(src,vars){spec$source_enabled[spec$source==src&spec$variable%in%vars]<<-1L}
    add_coverage<-function(d,src,stream){
      if(!nrow(d))return(invisible(NULL));z<-d[is.finite(d$available_hour),c("stay_id","available_hour"),drop=FALSE]
      if(!nrow(z))return(invisible(NULL));z<-aggregate(available_hour~stay_id,z,min)
      z<-data.frame(source=src,stay_id=as.character(z$stay_id),stream=stream,available_hour=z$available_hour)
      DBI::dbWriteTable(con,"coverage",z,append=TRUE)
    }
    add_evidence<-function(d,map,src,type){
      if(!nrow(map))return(invisible(NULL));z<-d[map$row,,drop=FALSE]
      z<-data.frame(source=src,stay_id=as.character(z$stay_id),variable=as.character(map$variable),
        chart_hour=num(z$chart_hour),available_hour=num(z$available_hour),evidence_type=type)
      z<-z[is.finite(z$chart_hour)&is.finite(z$available_hour),,drop=FALSE]
      if(nrow(z))DBI::dbWriteTable(con,"evidence",unique(z),append=TRUE)
    }
    current_module<<-"baseline demographics"
    mp<-locate("MIMIC","patients","hosp",TRUE);ep<-locate("eICU","patient",required=TRUE)
    view(mp,"mp_raw",c("subject_id","gender"));view(ep,"ep_raw",c("patientunitstayid","age","gender","hospitalid"))
    ms<-get("SELECT b.stay_id,p.gender FROM mimic_base b LEFT JOIN mp_raw p ON b.subject_id=p.subject_id")
    es<-get("SELECT b.stay_id,p.age,p.gender,p.hospitalid FROM eicu_base b LEFT JOIN ep_raw p ON b.stay_id=p.patientunitstayid")
    if(anyDuplicated(ms$stay_id)||anyDuplicated(es$stay_id))stop("人口学连接重复。")
    dem<-co[c("source","stay_id","landmark")];dem$age<-NA_real_;dem$sex<-NA_character_;dem$hospital_id<-NA_character_;dem$age_topcoded<-0L
    mi<-dem$source=="MIMIC";ii<-match(paste(dem$stay_id[mi],dem$landmark[mi]),paste(mr$stay_id,mr$landmark))
    dem$age[mi]<-num(mr$age[ii]);dem$sex[mi]<-sex_value(ms$gender[match(dem$stay_id[mi],ms$stay_id)])
    ii<-match(dem$stay_id[!mi],es$stay_id);dem$age[!mi]<-age_value(es$age[ii]);dem$sex[!mi]<-sex_value(es$gender[ii])
    dem$hospital_id[!mi]<-as.character(es$hospitalid[ii]);dem$age_topcoded[!mi]<-as.integer(grepl(">",es$age[ii],fixed=TRUE))
    DBI::dbWriteTable(con,"demographics",dem)
    csv_out(get("SELECT source,landmark,COUNT(*) AS n,SUM(CASE WHEN age IS NULL THEN 1 ELSE 0 END) AS age_missing,
      SUM(CASE WHEN sex IS NULL THEN 1 ELSE 0 END) AS sex_missing,MEDIAN(age) AS median_age,
      SUM(CASE WHEN sex='Female' THEN 1 ELSE 0 END) AS female_n FROM demographics GROUP BY source,landmark"),"to_share/03_demographics.csv")
    current_module<<-"MIMIC timed treatment records"
    df<-locate("MIMIC","d_items","icu",TRUE);view(df,"dict_raw",c("itemid","label","linksto"))
    di<-get("SELECT itemid,label,linksto FROM dict_raw")
    dm<-classify(di$label,drug_rules);dm<-dm[di$linksto[dm$row]%in%"inputevents",,drop=FALSE]
    pm<-classify(di$label,treatment_rules,TRUE);pm<-pm[di$linksto[pm$row]%in%"procedureevents",,drop=FALSE]
    drugmap<-data.frame(itemid=as.character(di$itemid[dm$row]),drug=dm$variable,label=di$label[dm$row])
    procmap<-data.frame(itemid=as.character(di$itemid[pm$row]),variable=pm$variable,label=di$label[pm$row])
    csv_out(drugmap,"to_share/04_MIMIC_drug_item_map.csv");csv_out(procmap,"to_share/04_MIMIC_procedure_item_map.csv")
    DBI::dbWriteTable(con,"drug_item_map",drugmap);DBI::dbWriteTable(con,"procedure_item_map",procmap)
    mf<-locate("MIMIC","inputevents","icu")
    if(view(mf,"mi_raw",c("stay_id","itemid","starttime","storetime","rate","statusdescription"))){
      logmsg("扫描MIMIC inputevents，仅保留目标患者及预测时点前记录。")
      run("CREATE TABLE mimic_inputs AS SELECT b.stay_id,x.itemid,TRY_CAST(x.rate AS DOUBLE) AS rate,
        LOWER(COALESCE(x.statusdescription,'')) AS status,
        epoch(TRY_CAST(x.starttime AS TIMESTAMP)-CAST(b.intime AS TIMESTAMP))/3600.0 AS chart_hour,
        epoch(TRY_CAST(x.storetime AS TIMESTAMP)-CAST(b.intime AS TIMESTAMP))/3600.0 AS entry_hour,
        x.storetime IS NULL OR trim(x.storetime)='' AS entry_missing
        FROM mi_raw x JOIN mimic_base b ON x.stay_id=b.stay_id
        WHERE TRY_CAST(x.starttime AS TIMESTAMP)<=CAST(b.intime AS TIMESTAMP)+INTERVAL '48 hours'")
      # 缺少entry time时保留明确标记；非空但不能解析的entry time不回退。
      run("CREATE TEMP VIEW mimic_inputs_valid AS SELECT *,GREATEST(chart_hour,COALESCE(entry_hour,chart_hour)) AS available_hour
        FROM mimic_inputs WHERE (entry_hour IS NOT NULL OR entry_missing) AND status NOT IN ('rewritten','cancelled','canceled')")
      cv<-get("SELECT stay_id,MIN(available_hour) AS available_hour FROM mimic_inputs_valid GROUP BY stay_id")
      add_coverage(cv,"MIMIC","infusion")
      z<-get("SELECT x.stay_id,x.chart_hour,x.available_hour,m.drug FROM mimic_inputs_valid x JOIN drug_item_map m USING(itemid) WHERE isfinite(rate) AND rate>0")
      add_evidence(z,data.frame(row=seq_len(nrow(z)),variable=rep("vasoactive",nrow(z))),"MIMIC","positive_rate_inputevent")
      if(nrow(drugmap))enable("MIMIC","vasoactive")
      csv_out(get("SELECT COUNT(*) AS rows,SUM(CASE WHEN entry_missing THEN 1 ELSE 0 END) AS entry_missing,
        SUM(CASE WHEN entry_hour IS NULL AND NOT entry_missing THEN 1 ELSE 0 END) AS invalid_entry_time,
        SUM(CASE WHEN entry_hour<chart_hour THEN 1 ELSE 0 END) AS entry_before_start FROM mimic_inputs"),"to_share/04_MIMIC_infusion_timing.csv")
      if(nrow(z))csv_out(as.data.frame(table(z$drug),stringsAsFactors=FALSE),"to_share/04_MIMIC_positive_drug_records.csv")
    }
    mf<-locate("MIMIC","procedureevents","icu")
    if(view(mf,"mpr_raw",c("stay_id","itemid","starttime","storetime","statusdescription"))){
      logmsg("提取MIMIC procedureevents中的明确介入/机械支持记录。")
      run("CREATE TABLE mimic_procedures AS SELECT b.stay_id,x.itemid,
        epoch(TRY_CAST(x.starttime AS TIMESTAMP)-CAST(b.intime AS TIMESTAMP))/3600.0 AS chart_hour,
        epoch(TRY_CAST(x.storetime AS TIMESTAMP)-CAST(b.intime AS TIMESTAMP))/3600.0 AS entry_hour,
        x.storetime IS NULL OR trim(x.storetime)='' AS entry_missing,
        LOWER(COALESCE(x.statusdescription,'')) AS status
        FROM mpr_raw x JOIN mimic_base b ON x.stay_id=b.stay_id
        WHERE TRY_CAST(x.starttime AS TIMESTAMP)<=CAST(b.intime AS TIMESTAMP)+INTERVAL '48 hours'")
      run("CREATE TEMP VIEW mimic_procedures_valid AS SELECT *,GREATEST(chart_hour,COALESCE(entry_hour,chart_hour)) AS available_hour
        FROM mimic_procedures WHERE (entry_hour IS NOT NULL OR entry_missing) AND status NOT IN ('rewritten','cancelled','canceled')")
      add_coverage(get("SELECT stay_id,MIN(available_hour) AS available_hour FROM mimic_procedures_valid GROUP BY stay_id"),"MIMIC","procedure")
      z<-get("SELECT x.stay_id,x.chart_hour,x.available_hour,m.variable FROM mimic_procedures_valid x JOIN procedure_item_map m USING(itemid)")
      add_evidence(z,data.frame(row=seq_len(nrow(z)),variable=z$variable),"MIMIC","timed_procedureevent")
      enable("MIMIC",unique(procmap$variable))
    }
    current_module<<-"eICU timed treatment records"
    ef<-locate("eICU","infusionDrug")
    if(view(ef,"ei_raw",c("patientunitstayid","infusionoffset","drugname","drugrate","infusionrate"))){
      logmsg("扫描eICU infusionDrug，保留真实记录时点和正输注速率。")
      run("CREATE TABLE eicu_infusions AS SELECT b.stay_id,x.drugname,
        TRY_CAST(x.infusionoffset AS DOUBLE)/60.0 AS chart_hour,
        TRY_CAST(x.infusionoffset AS DOUBLE)/60.0 AS available_hour,
        COALESCE(TRY_CAST(x.drugrate AS DOUBLE),TRY_CAST(x.infusionrate AS DOUBLE)) AS rate
        FROM ei_raw x JOIN eicu_base b ON x.patientunitstayid=b.stay_id WHERE TRY_CAST(x.infusionoffset AS DOUBLE)<=2880")
      add_coverage(get("SELECT stay_id,MIN(available_hour) AS available_hour FROM eicu_infusions GROUP BY stay_id"),"eICU","infusion")
      z<-get("SELECT * FROM eicu_infusions WHERE isfinite(rate) AND rate>0");m<-classify(z$drugname,drug_rules)
      if(nrow(m))csv_out(as.data.frame(table(m$variable),stringsAsFactors=FALSE),"to_share/04_eICU_positive_drug_records.csv")
      m$variable<-rep("vasoactive",nrow(m));add_evidence(z,m,"eICU","positive_rate_infusion_documentation");enable("eICU","vasoactive")
      csv_out(get("SELECT COUNT(*) AS rows,SUM(CASE WHEN NOT COALESCE(isfinite(rate),FALSE) THEN 1 ELSE 0 END) AS rate_unusable,
        SUM(CASE WHEN rate<=0 THEN 1 ELSE 0 END) AS nonpositive_rate,
        SUM(CASE WHEN chart_hour=0 THEN 1 ELSE 0 END) AS zero_offset_records FROM eicu_infusions"),"to_share/04_eICU_infusion_check.csv")
    }
    ef<-locate("eICU","treatment")
    if(view(ef,"et_raw",c("patientunitstayid","treatmentoffset","treatmentstring"))){
      logmsg("提取eICU治疗文档；单纯造影、PCI、CABG及各机械支持分别编码。")
      run("CREATE TABLE eicu_treatments AS SELECT b.stay_id,x.treatmentstring,
        TRY_CAST(x.treatmentoffset AS DOUBLE)/60.0 AS chart_hour,
        TRY_CAST(x.treatmentoffset AS DOUBLE)/60.0 AS available_hour
        FROM et_raw x JOIN eicu_base b ON x.patientunitstayid=b.stay_id WHERE TRY_CAST(x.treatmentoffset AS DOUBLE)<=2880")
      z<-get("SELECT * FROM eicu_treatments");add_coverage(z,"eICU","procedure")
      m<-classify(z$treatmentstring,treatment_rules,TRUE);add_evidence(z,m,"eICU","structured_active_treatment_documentation")
      enable("eICU",names(treatment_rules))
      if(nrow(m))csv_out(as.data.frame(table(m$variable),stringsAsFactors=FALSE),"to_share/04_eICU_treatment_mapping_counts.csv")
      # 映射到哪些文字可在本机查看；自由文本不进入ZIP。
      if(nrow(m))csv_out(unique(data.frame(variable=m$variable,text=z$treatmentstring[m$row])),"local_only/eICU_treatment_dictionary_review.csv")
    }
    current_module<<-"time-aware prior-history proxies"
    af<-locate("MIMIC","admissions","hosp");hf<-locate("MIMIC","diagnoses_icd","hosp")
    if(view(af,"ma_raw",c("subject_id","hadm_id","admittime","dischtime")) &&
      view(hf,"md_raw",c("subject_id","hadm_id","icd_code","icd_version"))){
      logmsg("重建MIMIC既往住院编码代理；不把本次AMI的出院编码当作既往CAD。")
      run("CREATE TABLE prior_admissions AS SELECT b.stay_id,p.hadm_id,
        epoch(TRY_CAST(p.dischtime AS TIMESTAMP)-CAST(b.intime AS TIMESTAMP))/3600.0 AS chart_hour
        FROM mimic_base b JOIN ma_raw current_adm ON b.hadm_id=current_adm.hadm_id
        JOIN ma_raw p ON p.subject_id=b.subject_id AND p.hadm_id<>b.hadm_id
        WHERE TRY_CAST(p.dischtime AS TIMESTAMP)<LEAST(CAST(b.intime AS TIMESTAMP),TRY_CAST(current_adm.admittime AS TIMESTAMP))")
      run("CREATE TABLE prior_diagnosis_codes AS SELECT p.stay_id,p.chart_hour,p.chart_hour AS available_hour,
        d.icd_version,REPLACE(UPPER(d.icd_code),'.','') AS code FROM prior_admissions p JOIN md_raw d USING(hadm_id)")
      z<-get("SELECT * FROM prior_diagnosis_codes");add_coverage(z,"MIMIC","history")
      mm<-lapply(names(history_rules),function(v){i<-which((z$icd_version=="9"&grepl(history_icd9[v],z$code))|
        (z$icd_version=="10"&grepl(history_icd10[v],z$code)));data.frame(row=i,variable=rep(v,length(i)))})
      add_evidence(z,do.call(rbind,mm),"MIMIC","previous_hospitalization_code_proxy");enable("MIMIC",names(history_rules))
      csv_out(get("SELECT COUNT(*) AS code_rows,COUNT(DISTINCT stay_id) AS patients_with_prior_coded_encounter FROM prior_diagnosis_codes"),"to_share/05_MIMIC_prior_history_coverage.csv")
      cur<-get("SELECT b.stay_id,d.icd_version,REPLACE(UPPER(d.icd_code),'.','') AS code FROM mimic_base b JOIN md_raw d ON b.hadm_id=d.hadm_id")
      acute<-(cur$icd_version=="9"&grepl("^410",cur$code))|(cur$icd_version=="10"&grepl("^I2[12]",cur$code))
      chronic<-(cur$icd_version=="9"&grepl("^41[234]",cur$code))|(cur$icd_version=="10"&grepl("^I20|^I25",cur$code))
      csv_out(data.frame(definition=c("current_admission_acute_MI_code","current_admission_chronic_IHD_code"),
        patients=c(length(unique(cur$stay_id[acute])),length(unique(cur$stay_id[chronic]))),
        predictor_use="NO: index-admission discharge coding has no confirmed pre-landmark entry time"),"to_share/05_MIMIC_CAD_definition_audit.csv")
    }
    ef<-locate("eICU","pastHistory")
    if(view(ef,"eh_raw",c("patientunitstayid","pasthistoryoffset","pasthistoryenteredoffset","pasthistorypath","pasthistoryvalue"))){
      logmsg("提取eICU pastHistory，同时限制记录和录入时间。")
      run("CREATE TABLE eicu_history AS SELECT b.stay_id,
        COALESCE(x.pasthistorypath,'')||' | '||COALESCE(x.pasthistoryvalue,'') AS history_text,
        TRY_CAST(x.pasthistoryoffset AS DOUBLE)/60.0 AS chart_hour,
        GREATEST(TRY_CAST(x.pasthistoryoffset AS DOUBLE),TRY_CAST(x.pasthistoryenteredoffset AS DOUBLE))/60.0 AS available_hour
        FROM eh_raw x JOIN eicu_base b ON x.patientunitstayid=b.stay_id
        WHERE TRY_CAST(x.pasthistoryoffset AS DOUBLE)<=2880 AND TRY_CAST(x.pasthistoryenteredoffset AS DOUBLE) IS NOT NULL")
      z<-get("SELECT * FROM eicu_history");add_coverage(z,"eICU","history")
      m<-classify(z$history_text,history_rules);add_evidence(z,m,"eICU","time_filtered_past_history_documentation");enable("eICU",names(history_rules))
      csv_out(get("SELECT COUNT(*) AS records,SUM(CASE WHEN available_hour>48 THEN 1 ELSE 0 END) AS entered_after_T48,
        SUM(CASE WHEN chart_hour=0 AND available_hour=0 THEN 1 ELSE 0 END) AS zero_offset_records FROM eicu_history"),"to_share/05_eICU_history_timing.csv")
      if(nrow(m))csv_out(unique(data.frame(variable=m$variable,text=z$history_text[m$row])),"local_only/eICU_history_dictionary_review.csv")
    }
    current_module<<-"date-only coronary procedure supplement"
    pf<-locate("MIMIC","procedures_icd","hosp");pd<-locate("MIMIC","d_icd_procedures","hosp")
    if(view(pf,"picd_raw",c("hadm_id","chartdate","icd_code","icd_version")) &&
      view(pd,"picd_dict",c("icd_code","icd_version","long_title"))){
      dd<-get("SELECT * FROM picd_dict");tx<-clean_text(dd$long_title)
      pci<-grepl("coronary.*angioplasty|coronary.*stent|dilation of coronary artery.*percutaneous",tx)
      cabg<-grepl("coronary artery bypass|coronary bypass|bypass coronary artery",tx)
      mm<-rbind(data.frame(icd_code=dd$icd_code[pci],icd_version=dd$icd_version[pci],variable=rep("pci",sum(pci)),title=dd$long_title[pci]),
        data.frame(icd_code=dd$icd_code[cabg],icd_version=dd$icd_version[cabg],variable=rep("cabg",sum(cabg)),title=dd$long_title[cabg]))
      csv_out(mm,"to_share/06_coronary_procedure_code_map.csv");DBI::dbWriteTable(con,"coronary_code_map",mm)
      run("CREATE TABLE date_only_coronary_procedures AS SELECT b.stay_id,m.variable,
        epoch(CAST(TRY_CAST(x.chartdate AS DATE) AS TIMESTAMP)-CAST(b.intime AS TIMESTAMP))/3600.0 AS lower_hour,
        epoch(CAST(TRY_CAST(x.chartdate AS DATE) AS TIMESTAMP)+INTERVAL '1 day'-CAST(b.intime AS TIMESTAMP))/3600.0 AS upper_hour
        FROM picd_raw x JOIN mimic_base b ON x.hadm_id=b.hadm_id
        JOIN coronary_code_map m ON x.icd_code=m.icd_code AND x.icd_version=m.icd_version")
      csv_out(get("SELECT c.landmark,d.variable,COUNT(DISTINCT CASE WHEN d.upper_hour<=c.cutoff_hour THEN c.stay_id END) AS definitely_before_landmark_by_date,
        COUNT(DISTINCT CASE WHEN d.lower_hour<=c.cutoff_hour AND d.upper_hour>c.cutoff_hour THEN c.stay_id END) AS landmark_day_ambiguous,
        COUNT(DISTINCT CASE WHEN d.lower_hour IS NULL THEN c.stay_id END) AS invalid_date_patients
        FROM cohorts c JOIN date_only_coronary_procedures d ON c.stay_id=d.stay_id WHERE c.source='MIMIC'
        GROUP BY c.landmark,d.variable"),"to_share/06_date_only_coronary_procedures.csv")
    }
    current_module<<-"clinical feature summary and outputs"
    DBI::dbWriteTable(con,"source_spec",spec)
    csv_out(spec,"to_share/07_clinical_source_specification.csv")
    run(feature_sql())
    bad<-get("SELECT COUNT(*) AS n FROM clinical_features_long WHERE documented_positive=1 AND
      (source_enabled=0 OR patient_source_covered=0)")$n
    if(bad!=0)stop("阳性证据与来源覆盖不一致。")
    a<-get("SELECT source,landmark,variable,stream,source_enabled,COUNT(*) AS cohort_n,
      SUM(CASE WHEN documented_positive=1 THEN 1 ELSE 0 END) AS positive_n,
      SUM(CASE WHEN documented_positive=0 THEN 1 ELSE 0 END) AS no_positive_record_n,
      SUM(CASE WHEN documented_positive IS NULL THEN 1 ELSE 0 END) AS source_unobserved_n,
      SUM(patient_source_covered) AS source_covered_n
      FROM clinical_features_long GROUP BY source,landmark,variable,stream,source_enabled ORDER BY source,landmark,variable")
    if(nrow(a)!=4L*length(clinical_vars)||any(a$positive_n+a$no_positive_record_n+a$source_unobserved_n!=a$cohort_n))stop("临床特征汇总人数不完整。")
    csv_out(a,"to_share/07_clinical_documentation.csv")
    csv_out(get("SELECT source,variable,evidence_type,COUNT(*) AS rows,
      COUNT(DISTINCT stay_id) AS patients_with_any_evidence,
      SUM(CASE WHEN chart_hour<=6 AND available_hour>6 THEN 1 ELSE 0 END) AS late_for_T6,
      SUM(CASE WHEN chart_hour<=24 AND available_hour>24 THEN 1 ELSE 0 END) AS late_for_T24,
      SUM(CASE WHEN chart_hour<=48 AND available_hour>48 THEN 1 ELSE 0 END) AS late_for_T48
      FROM evidence GROUP BY source,variable,evidence_type"),"to_share/07_evidence_timing_audit.csv")
    # 医院仅输出覆盖率分布，医院ID和逐患者结果均留在local_only。
    run("CREATE TABLE hospital_source_coverage AS SELECT d.hospital_id,f.stream,
      COUNT(DISTINCT f.stay_id) AS patients,COUNT(DISTINCT CASE WHEN f.patient_source_covered=1 THEN f.stay_id END) AS covered
      FROM demographics d JOIN clinical_features_long f USING(source,stay_id,landmark)
      WHERE d.source='eICU' AND d.landmark='T48' AND d.hospital_id IS NOT NULL
      GROUP BY d.hospital_id,f.stream")
    csv_out(get("SELECT stream,COUNT(*) AS hospitals,SUM(CASE WHEN covered=0 THEN 1 ELSE 0 END) AS hospitals_without_observed_records,
      MIN(covered*1.0/patients) AS min_coverage,MEDIAN(covered*1.0/patients) AS median_coverage,
      MAX(covered*1.0/patients) AS max_coverage FROM hospital_source_coverage GROUP BY stream"),"to_share/08_eICU_hospital_source_coverage.csv")
    gaps<-data.frame(concept=c("LVEF","RV_function","exact_reperfusion_delay","confirmed_absence_of_treatment"),
      status=c("NOT_ESTABLISHED","NOT_ESTABLISHED","NOT_ESTABLISHED","NOT_ESTABLISHED"),
      reason=c("No validated timestamped echocardiographic measurement supplied; not inferred from diagnoses or severity scores",
        "No validated timestamped right-ventricular measurement supplied",
        "Documentation offsets and date-only billing codes do not establish symptom-to-reperfusion delay",
        "Absence of documentation is not verified absence of treatment"))
    csv_out(gaps,"to_share/08_remaining_clinical_information.csv")
    for(src in c("MIMIC","eICU"))for(lm in if(src=="MIMIC")c("T6","T24","T48") else "T48"){
      wide<-dem[dem$source==src&dem$landmark==lm,,drop=FALSE]
      f<-get(paste0("SELECT * FROM clinical_features_long WHERE source=",q(src)," AND landmark=",q(lm)))
      for(v in clinical_vars){d<-f[f$variable==v,,drop=FALSE];j<-match(wide$stay_id,d$stay_id)
        if(anyNA(j)||anyDuplicated(d$stay_id))stop("临床宽表连接不完整。")
        for(st in c("documented_positive","patient_source_covered","first_record_hour","first_available_hour"))wide[[paste(v,st,sep="__")]]<-d[[st]][j]
      }
      saveRDS(wide,reg(paste0("local_only/",src,"_",lm,"_clinical_context.rds")),compress="gzip")
    }
    make_visuals(a)
    csv_out(data.frame(variable=c(clinical_vars,"age","sex"),role=c(rep("candidate_documentation_proxy_review_before_modeling",length(clinical_vars)),"baseline_demographic","baseline_demographic")),"to_share/10_candidate_feature_roles.csv")
    csv_out(rbind(data.frame(concept=names(drug_rules),rule=unname(drug_rules),kind="drug_name"),
      data.frame(concept=names(treatment_rules),rule=unname(treatment_rules),kind="treatment_text"),
      data.frame(concept=names(history_rules),rule=unname(history_rules),kind="history_text"),
      data.frame(concept=names(history_icd9),rule=unname(history_icd9),kind="prior_admission_ICD9"),
      data.frame(concept=names(history_icd10),rule=unname(history_icd10),kind="prior_admission_ICD10")),"to_share/10_mapping_rules.csv")
    txt_out(c("本步固定Step4队列；不重新拟合器官评分、分型或预测模型。",
      "原始CSV固定逗号分隔、双引号包围及双写双引号转义；严格读取，不跳过错误记录、不用补列掩盖结构异常。",
      "人口学：MIMIC年龄沿用冻结名单；eICU >89记为90并保留topcoded标记，不能当作精确年龄。",
      "documented_positive=1：该时点前有明确阳性记录；0：相关来源有记录但未检出该概念；NA：来源未建立或患者该时点前无相关来源记录。",
      "0不是确定没有疾病/没有接受治疗；patient_source_covered=1也不保证记录完整。",
      "MIMIC血管活性药限明确药名项目且rate>0，排除rewritten/cancelled；eICU限同名药物的正输注速率记录。",
      "MIMIC已存储时间可用时按max(starttime,storetime)截断；缺失storetime单独报告，不把无效时间强行设为0。",
      "两库治疗记录均为该次ICU关联的当前诊疗过程内、landmark前记录；允许与该次ICU关联的负offset，不宣称精确再灌注时间。",
      "eICU infusionoffset和treatmentoffset是记录/录入时间；治疗文档不是逐次执行清单。",
      "PCI、CABG、单纯造影、IABP、Impella及ECMO分开。缺乏可验证项目映射时不生成全0变量。",
      "MIMIC procedures_icd只提供日期级补充；即使日期确定在landmark前，也未证明编码当时已可见，因此不进入预测特征。",
      "MIMIC病史仅用本次住院以前已出院的既往住院编码；其为既往住院编码代理，不代表完整病史或已核实的实时病历可见性。",
      "eICU病史仅用pastHistory且同时约束记录与录入offset；不混入全住院diagnosis出院信息。",
      "eICU部分0 offset可能源自原系统缺失时间默认值；本步报告0 offset记录数，保留此时间精度限制。",
      "LVEF/RV功能未建立可靠提取，不以正常值填补；不能写成已排除这些因素的影响。",
      "本步补充图展示记录阳性比例，不是治疗真实使用率；正式S编号待整合补充材料时统一。",
      "回传Step5_feedback.zip；患者表、原始文本和数据库只保存在local_only。"),"to_share/00_READ_ME_FIRST.txt")
    txt_out(c("https://mimic.mit.edu/docs/iv/modules/icu/inputevents/",
      "https://mimic.mit.edu/docs/iv/modules/icu/procedureevents/",
      "https://mimic.mit.edu/docs/iv/modules/hosp/procedures_icd/",
      "https://eicu.mit.edu/eicutables/infusiondrug/","https://eicu.mit.edu/eicutables/treatment/",
      "https://eicu.mit.edu/eicutables/pasthistory/","https://github.com/MIT-LCP/eicu-code/issues/49"),"to_share/10_definition_sources.txt")
    if(any(spec$source_enabled==0))issue("SOURCE_GAP","clinical documentation","部分来源/项目未建立，相关变量保持NA；详见07_clinical_source_specification.csv。")
    issue("PENDING","clinical interpretation","本步是时间约束下的记录代理提取；需审阅覆盖率后锁定模型输入。LVEF/RV及精确再灌注延迟未建立。")
    run("CHECKPOINT");status<<-"COMPLETED_FOR_REVIEW"
  }
  failure_message<-NULL
  tryCatch(main(),error=function(err){status<<-"FAILED";failure_message<<-conditionMessage(err)
    issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step5_error_detail.txt")})
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step5-1",
    "COMPLETED_FOR_REVIEW表示提取结束、候选临床记录代理等待审核；不表示已训练模型或全部临床信息可用。",
    "FAILED表示不完整，不能使用本次患者级结果建模。"),"to_share/Step5_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step5_sessionInfo.txt")
  logmsg(paste0("Step5状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step5_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share) else {
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step5_feedback.zip"),"\n",sep="") else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
