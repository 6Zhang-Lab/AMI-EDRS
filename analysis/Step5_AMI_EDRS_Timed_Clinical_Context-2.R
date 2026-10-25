# AMI-EDRS | Step5-2: correct eICU coronary-intervention mapping from the existing Step5 cache
# 运行：source(file.choose(), encoding="UTF-8")。
# 需已有Step5-1成功输出；直接复用local_only数据库，无需T7，不扫描原始大表。
# 同名替换本次受影响的Step5表、图和反馈ZIP；不产生时间戳结果目录。
# 原始文本和患者级结果仍仅保存在local_only。
local({
  cfg<-utils::modifyList(list(
    OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MEMORY_LIMIT="2GB",MAKE_ZIP=TRUE,MAKE_PNG=TRUE),getOption("ami_step5.config",list()))
  out<-normalizePath(file.path(path.expand(cfg$OUTPUT_PARENT),"Step5"),winslash="/",mustWork=FALSE)
  stage<-tempfile("AMI_EDRS_Step5_refresh_",tmpdir=tempdir())
  old<-character();written<-character()
  is_link<-function(p){z<-Sys.readlink(p);any(!is.na(z)&nzchar(z))}
  safe_path<-function(rel){
    if(length(rel)!=1||is.na(rel)||!nzchar(rel)||grepl("(^/|^[A-Za-z]:|\\\\|(^|/)\\.\\.(/|$))",rel))stop("输出路径越界。")
    p<-file.path(out,rel);par<-dirname(p)
    while(nchar(par)>=nchar(out)){
      if(is_link(par))stop("输出目录不能为符号链接。")
      if(par==out)break
      par<-dirname(par)
    }
    if(is_link(p))stop("输出文件不能为符号链接。")
    if(file.exists(p)&&!rel%in%old)stop("不能覆盖未登记文件：",rel)
    p
  }
  reg<-function(rel){
    safe_path(rel);p<-file.path(stage,rel);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE)
    written<<-unique(c(written,rel));p
  }
  csv_out<-function(d,rel)utils::write.csv(d,reg(rel),row.names=FALSE,na="",fileEncoding="UTF-8")
  txt_out<-function(x,rel)writeLines(enc2utf8(x),reg(rel),useBytes=TRUE)
  logmsg<-function(s){cat(s,"\n");cat(s,"\n",file=reg("to_share/Step5_run_log.txt"),append=TRUE)}
  read_csv<-function(rel)utils::read.csv(file.path(out,rel),colClasses="character",check.names=FALSE,stringsAsFactors=FALSE,na.strings=c("","NA"),fileEncoding="UTF-8")
  num<-function(x)suppressWarnings(as.numeric(as.character(x)))
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
    diagnostic_cath="cardiac cath|coronary angiograph|coronary catheter|(^|\\|)cardiac angiography(\\||$)")
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
  treatment_rules["pci"]<-paste0(treatment_rules["pci"],
    "|^cardiovascular\\|non-operative procedures\\|cardiac angiography\\|with (angioplasty|stent placement)(\\||$)")
  treatment_map <- function(x) {
    z<-gsub("[[:space:]]*\\|[[:space:]]*","|",clean_text(x),perl=TRUE)
    m<-classify(z,treatment_rules,TRUE)
    # Each row with explicit PCI is PCI, not a diagnostic-only cath row.
    m<-m[!(m$variable=="diagnostic_cath" & m$row%in%m$row[m$variable=="pci"]),,drop=FALSE]
    m
  }
  mapping_preflight <- function() {
    tx<-c("cardiovascular|non-operative procedures|cardiac angiography|with angioplasty",
      "cardiovascular|non-operative procedures|cardiac angiography|with stent placement|drug-eluting",
      "cardiovascular | non-operative procedures | cardiac angiography | with stent placement",
      "cardiovascular|non-operative procedures|cardiac angiography",
      "cardiac catheterization","coronary angiography",
      "neurologic|procedures / diagnostics|angiogram|with cerebral angioplasty",
      "neurologic|ICH/ cerebral infarct|surgery|carotid stenting",
      "gastrointestinal|endoscopy/gastric instrumentation|ERCP|with stent",
      "renal|procedures|ureteral stenting","planned PCI","no CABG","history of CABG",
      "cardiovascular|cardiac surgery|CABG","cardiovascular|shock|intraaortic balloon pump",
      "cardiovascular|non-operative procedures|non-cardiac angiography",
      "cardiovascular|non-operative procedures|non-cardiac angiography|carotid angiogram")
    m<-treatment_map(tx)
    stopifnot(identical(sort(m$row[m$variable=="pci"]),1:3),
      identical(sort(m$row[m$variable=="diagnostic_cath"]),4:6),
      identical(m$row[m$variable=="cabg"],14L),identical(m$row[m$variable=="iabp"],15L),
      nrow(m)==8L,nrow(treatment_map(character()))==0)
    TRUE
  }
  age_value <- function(x) {z<-trimws(as.character(x));a<-num(z);a[z%in%c("> 89",">89")]<-90;a[!is.finite(a)|a<18|a>120]<-NA_real_;a}
  sex_value <- function(x) {z<-clean_text(x);ifelse(z%in%c("m","male"),"Male",ifelse(z%in%c("f","female"),"Female",NA_character_))}
  clinical_vars <- c("vasoactive",names(treatment_rules),names(history_rules))
  pretty <- c(vasoactive="Vasoactive infusion",pci="PCI documentation",cabg="CABG documentation",iabp="IABP documentation",
    impella="Impella documentation",ecmo="ECMO documentation",diagnostic_cath="Cath / angiography, PCI unconfirmed",
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
  if(isTRUE(getOption("ami_step5.functions_only",FALSE)))return(invisible(list(
    classify=classify,treatment_map=treatment_map,mapping_preflight=mapping_preflight,
    drug_rules=drug_rules,treatment_rules=treatment_rules,history_rules=history_rules,
    feature_sql=feature_sql,clinical_vars=clinical_vars,make_visuals=make_visuals)))
  core<-function(){
    if(!requireNamespace("DBI",quietly=TRUE)||!requireNamespace("duckdb",quietly=TRUE))stop("需要DBI和duckdb。")
    logmsg("小型预检：eICU标准冠脉介入路径；单纯/未注明介入的造影、非冠脉支架及否定记录分开。")
    stopifnot(mapping_preflight())
    old_status<-readLines(file.path(out,"to_share/Step5_STATUS.txt"),warn=FALSE)
    ver<-sub("SCRIPT_VERSION = ","",old_status[startsWith(old_status,"SCRIPT_VERSION = ")])
    completed<-"STATUS = COMPLETED_FOR_REVIEW"%in%old_status
    if(length(ver)!=1||!ver%in%c("Step5-1","Step5-2")||(!completed&&ver!="Step5-2"))stop("请先完成Step5-1；此修正版仅更新已建立的本地缓存。")
    # 无需T7：只核对本机Step4/5的冻结文件和缓存。
    prior_inputs<-read_csv("to_share/01_inputs.csv")
    if(!all(c("file","md5")%in%names(prior_inputs)))stop("Step5输入登记不完整。")
    for(i in seq_len(nrow(prior_inputs)))if(!file.exists(prior_inputs$file[i])||
      unname(tools::md5sum(prior_inputs$file[i]))!=prior_inputs$md5[i])stop("Step4冻结输入发生变化，不能沿用Step5缓存。")
    step4<-file.path(dirname(out),"Step4")
    if(!"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(file.path(step4,"to_share/Step4_STATUS.txt"),warn=FALSE))stop("Step4未成功完成。")
    f4<-file.path(step4,"local_only/Step4_harmonized_features.duckdb")
    dbfile<-safe_path("local_only/Step5_clinical_context.duckdb")
    if(!file.exists(dbfile)||!file.exists(f4))stop("缺少本地Step4/5数据库。")
    con<-DBI::dbConnect(duckdb::duckdb(),dbdir=dbfile)
    transaction_open<-FALSE
    spill<-tempfile("Step5_mapping_spill_",tmpdir=tempdir());dir.create(spill)
    on.exit({if(transaction_open)try(DBI::dbExecute(con,"ROLLBACK"),silent=TRUE)
      try(DBI::dbDisconnect(con,shutdown=TRUE),silent=TRUE);unlink(spill,recursive=TRUE)},add=TRUE)
    q<-function(x)as.character(DBI::dbQuoteString(con,as.character(x)))
    run<-function(x)DBI::dbExecute(con,x);get<-function(x)DBI::dbGetQuery(con,x)
    run("SET threads=1");run("SET preserve_insertion_order=false")
    run(paste("SET memory_limit =",q(cfg$MEMORY_LIMIT)));run(paste("SET temp_directory =",q(spill)))
    need<-c("cohorts","evidence","coverage","source_spec","clinical_features_long","demographics","eicu_treatments")
    if(length(setdiff(need,DBI::dbListTables(con))))stop("Step5缓存表不完整。")
    run(paste("ATTACH",q(f4),"AS step4 (READ_ONLY)"))
    if(get("SELECT COUNT(*) AS n FROM ((SELECT * FROM cohorts EXCEPT ALL SELECT * FROM step4.cohorts)
      UNION ALL (SELECT * FROM step4.cohorts EXCEPT ALL SELECT * FROM cohorts)) d")$n!=0)stop("缓存名单与Step4不一致。")
    summarize<-function(tbl)get(paste0("SELECT source,landmark,variable,stream,source_enabled,COUNT(*) AS cohort_n,
      SUM(CASE WHEN documented_positive=1 THEN 1 ELSE 0 END) AS positive_n,
      SUM(CASE WHEN documented_positive=0 THEN 1 ELSE 0 END) AS no_positive_record_n,
      SUM(CASE WHEN documented_positive IS NULL THEN 1 ELSE 0 END) AS source_unobserved_n,
      SUM(patient_source_covered) AS source_covered_n
      FROM ",tbl," GROUP BY source,landmark,variable,stream,source_enabled ORDER BY source,landmark,variable"))
    before<-summarize("clinical_features_long")
    if(completed){
      old_a<-read_csv("to_share/07_clinical_documentation.csv")
      key<-function(d)paste(d$source,d$landmark,d$variable)
      j<-match(key(before),key(old_a))
      if(nrow(old_a)!=nrow(before)||anyNA(j)||anyDuplicated(key(old_a)))stop("缓存与反馈汇总不一致。")
      for(k in c("source_enabled","cohort_n","positive_n","no_positive_record_n","source_unobserved_n","source_covered_n"))
        if(any(num(before[[k]])!=num(old_a[[k]][j])))stop("缓存与反馈的",k,"不一致。")
    }
    z<-get("SELECT * FROM eicu_treatments")
    if(!all(c("stay_id","treatmentstring","chart_hour","available_hour")%in%names(z)))stop("缓存缺少治疗原文或时间。")
    ec<-get("SELECT stay_id FROM cohorts WHERE source='eICU' AND landmark='T48'")$stay_id
    if(any(!z$stay_id%in%ec)||any(!is.finite(z$chart_hour))||any(!is.finite(z$available_hour))||
      any(z$chart_hour>48|z$available_hour>48))stop("治疗缓存不符合冻结名单/时间范围。")
    old_rules<-treatment_rules
    old_rules["pci"]<-"\\bpci\\b|percutaneous coronary intervention|coronary angioplasty|coronary stent|ptca"
    old_rules["diagnostic_cath"]<-"cardiac cath|coronary angiograph|coronary catheter"
    old_m<-classify(z$treatmentstring,old_rules,TRUE)
    m<-treatment_map(z$treatmentstring)
    logmsg(paste0("缓存核对通过：读取",nrow(z),"条eICU治疗文档，更新明确冠脉介入与造影映射。"))
    run("BEGIN TRANSACTION");transaction_open<-TRUE
    run("CREATE TEMP TABLE clinical_before AS SELECT * FROM clinical_features_long")
    run("DELETE FROM evidence WHERE source='eICU' AND evidence_type='structured_active_treatment_documentation'")
    if(nrow(m)){
      e<-unique(data.frame(source="eICU",stay_id=as.character(z$stay_id[m$row]),variable=as.character(m$variable),
        chart_hour=num(z$chart_hour[m$row]),available_hour=num(z$available_hour[m$row]),
        evidence_type="structured_active_treatment_documentation",stringsAsFactors=FALSE))
      DBI::dbWriteTable(con,"evidence",e,append=TRUE)
    }
    run("DROP TABLE clinical_features_long");run(feature_sql())
    if(get("SELECT COUNT(*) AS n FROM clinical_features_long WHERE documented_positive=1 AND
      (source_enabled=0 OR patient_source_covered=0)")$n!=0)stop("阳性证据与来源覆盖不一致。")
    # 除eICU的PCI/造影映射外，所有患者级临床特征必须完全一致。
    unchanged<-"source<>'eICU' OR variable NOT IN ('pci','diagnostic_cath')"
    sql<-paste0("SELECT COUNT(*) AS n FROM ((SELECT * FROM clinical_features_long WHERE ",unchanged,
      " EXCEPT ALL SELECT * FROM clinical_before WHERE ",unchanged,
      ") UNION ALL (SELECT * FROM clinical_before WHERE ",unchanged,
      " EXCEPT ALL SELECT * FROM clinical_features_long WHERE ",unchanged,")) d")
    if(get(sql)$n!=0)stop("出现非预期临床特征变化；撤销本次更新。")
    a<-summarize("clinical_features_long")
    if(nrow(a)!=4L*length(clinical_vars)||any(a$positive_n+a$no_positive_record_n+a$source_unobserved_n!=a$cohort_n))stop("临床特征人数核对失败。")
    if(any(a$source_covered_n!=before$source_covered_n))stop("来源覆盖不应随词典更新改变。")
    csv_out(a,"to_share/07_clinical_documentation.csv")
    delta<-merge(before[c("source","landmark","variable","positive_n")],a[c("source","landmark","variable","positive_n")],
      by=c("source","landmark","variable"),suffixes=c("_before","_after"))
    delta$change<-delta$positive_n_after-delta$positive_n_before
    csv_out(delta,"to_share/04_mapping_update_patient_counts.csv")
    countmap<-function(mm)as.integer(table(factor(mm$variable,levels=names(treatment_rules))))
    csv_out(data.frame(Var1=names(treatment_rules),Freq=countmap(m)),"to_share/04_eICU_treatment_mapping_counts.csv")
    csv_out(data.frame(variable=names(treatment_rules),old_rule_records=countmap(old_m),updated_rule_records=countmap(m)),
      "to_share/04_eICU_mapping_rule_comparison.csv")
    csv_out(unique(data.frame(variable=m$variable,text=z$treatmentstring[m$row])),"local_only/eICU_treatment_dictionary_review.csv")
    # 未匹配候选仅供本地复核，绝不自动转成治疗阳性，也不把原文放进反馈包。
    candidate<-grepl("angiograph|angioplast|stent|catheter|iabp|balloon pump|impella|ecmo|extracorporeal",clean_text(z$treatmentstring))
    unmatched<-which(candidate&!seq_len(nrow(z))%in%m$row)
    unmatched_text<-if(length(unmatched))as.data.frame(table(z$treatmentstring[unmatched]),stringsAsFactors=FALSE) else data.frame(Var1=character(),Freq=integer())
    names(unmatched_text)<-c("text","records")
    csv_out(unmatched_text,"local_only/eICU_unmatched_procedure_candidates.csv")
    csv_out(data.frame(candidate_records=sum(candidate),matched_candidate_records=sum(which(candidate)%in%m$row),
      unmatched_candidate_records=length(unmatched),unmatched_candidate_paths=nrow(unmatched_text),
      note="Includes deliberately excluded non-coronary catheters/stents; raw text remains local_only"),"to_share/04_eICU_unmatched_candidate_audit.csv")
    csv_out(get("SELECT source,variable,evidence_type,COUNT(*) AS rows,
      COUNT(DISTINCT stay_id) AS patients_with_any_evidence,
      SUM(CASE WHEN chart_hour<=6 AND available_hour>6 THEN 1 ELSE 0 END) AS late_for_T6,
      SUM(CASE WHEN chart_hour<=24 AND available_hour>24 THEN 1 ELSE 0 END) AS late_for_T24,
      SUM(CASE WHEN chart_hour<=48 AND available_hour>48 THEN 1 ELSE 0 END) AS late_for_T48
      FROM evidence GROUP BY source,variable,evidence_type"),"to_share/07_evidence_timing_audit.csv")
    wide<-get("SELECT * FROM demographics WHERE source='eICU' AND landmark='T48' ORDER BY stay_id")
    ff<-get("SELECT * FROM clinical_features_long WHERE source='eICU' AND landmark='T48'")
    for(v in clinical_vars){d<-ff[ff$variable==v,,drop=FALSE];j<-match(wide$stay_id,d$stay_id)
      if(anyNA(j)||anyDuplicated(d$stay_id))stop("临床宽表连接不完整。")
      for(st in c("documented_positive","patient_source_covered","first_record_hour","first_available_hour"))
        wide[[paste(v,st,sep="__")]]<-d[[st]][j]
    }
    saveRDS(wide,reg("local_only/eICU_T48_clinical_context.rds"),compress="gzip")
    logmsg("重新输出同名临床记录图、三线表、患者级宽表及映射差异计数。")
    make_visuals(a)
    rules<-read_csv("to_share/10_mapping_rules.csv")
    rules<-rules[rules$kind!="treatment_text",,drop=FALSE]
    rules<-rbind(rules,data.frame(concept=names(treatment_rules),rule=unname(treatment_rules),kind="treatment_text"))
    csv_out(rules,"to_share/10_mapping_rules.csv")
    notes<-readLines(file.path(out,"to_share/00_READ_ME_FIRST.txt"),warn=FALSE,encoding="UTF-8")
    notes<-unique(c(notes,
      "Step5-2补充识别eICU明确路径：cardiac angiography|with angioplasty，以及with stent placement及其子项。",
      "diagnostic_cath为兼容字段名，实际表示心导管/造影记录而未由该条记录证实PCI；不能断言只做了诊断造影。",
      "明确PCI的同一条文档不重复计为未证实PCI的造影；同一患者若有不同文档，仍可能同时有两类记录。",
      "MIMIC时间级PCI/CABG来源仍未建立；日期级ICD仅供描述，不进入小时级预测特征。",
      "输注及病史来源覆盖差异仍存在；当前变量为文档代理，不能把两库比例差异全部解释为临床差异。"))
    txt_out(notes,"to_share/00_READ_ME_FIRST.txt")
    sources<-readLines(file.path(out,"to_share/10_definition_sources.txt"),warn=FALSE)
    txt_out(unique(c(sources,"https://physionet.org/content/eicu-crd-demo/2.0.1/treatment.csv.gz")),"to_share/10_definition_sources.txt")
    txt_out(c("PASS: cached Step4/5 roster and prior aggregate counts checked.",
      "PASS: explicit eICU coronary angioplasty/stent paths; generic cath and non-coronary stents kept separate.",
      "PASS: prior/planned/negated treatments excluded; empty dictionary input handled.",
      "PASS: all patient-level fields outside eICU PCI/cath unchanged; source coverage and cohort totals preserved.",
      "CSV reader regression passed in Step5-1. This update reads only existing local DuckDB tables."),"to_share/02_preflight.txt")
    issues<-read_csv("to_share/09_issues.csv");issues<-issues[issues$severity!="ERROR",,drop=FALSE]
    info<-data.frame(severity="RESOLVED",context="eICU coronary procedure mapping",message="Added standard cardiac-angiography paths with angioplasty/stent; refreshed cached features and plots.")
    csv_out(unique(rbind(issues,info)),"to_share/09_issues.csv")
    csv_out(data.frame(output=c("Figure_S_clinical_documentation","Table_S_clinical_documentation"),
      placement=c("UPDATE same Step5 supplementary figure after mapping correction; no additional duplicate figure",
      "UPDATE same Step5 supplementary three-line table after mapping correction; final S number pending")),"to_share/11_figure_table_replacement_map.csv")
    txt_out(c("STATUS = COMPLETED_FOR_REVIEW","SCRIPT_VERSION = Step5-2",
      "缓存词典修复完成；两库覆盖差异和日期级编码限制保留。正式建模前仍按变量角色与可用性选择输入。"),"to_share/Step5_STATUS.txt")
    txt_out(capture.output(sessionInfo()),"to_share/Step5_sessionInfo.txt")
    # 文件全部暂存成功后才提交患者级更新；失败时自动回滚。
    run("COMMIT");transaction_open<-FALSE;run("CHECKPOINT")
    invisible(TRUE)
  }
  publish<-function(){
    # 先登记所有已获准覆盖的目标，使意外中断后可安全重试。
    for(rel in written)safe_path(rel)
    old<<-unique(c(old,written));writeLines(old,file.path(out,".generated_files.txt"))
    for(rel in written){dst<-safe_path(rel);dir.create(dirname(dst),recursive=TRUE,showWarnings=FALSE)
      if(!file.copy(file.path(stage,rel),dst,overwrite=TRUE))stop("不能覆盖输出文件：",rel)
    }
    old<<-unique(c(old,written));writeLines(old,file.path(out,".generated_files.txt"))
  }
  execute<-function(){
    on.exit(unlink(stage,recursive=TRUE),add=TRUE)
    if(!dir.exists(out)||is_link(out))stop("未找到可用的Step5输出目录。")
    owner<-file.path(out,".AMI_EDRS_STEP5")
    if(!file.exists(owner)||!identical(readLines(owner,warn=FALSE),"AMI_EDRS_STEP5_MANAGED_OUTPUT"))stop("Step5归属标记缺失或不正确。")
    manifest<-file.path(out,".generated_files.txt")
    if(!file.exists(manifest))stop("Step5输出登记缺失。")
    old<<-unique(readLines(manifest,warn=FALSE));dir.create(stage)
    failed<-NULL
    ok<-tryCatch({core();publish();TRUE},error=function(e){failed<<-conditionMessage(e);FALSE})
    if(!ok){
      unlink(stage,recursive=TRUE);dir.create(stage);written<<-character()
      txt_out(c("STATUS = FAILED","SCRIPT_VERSION = Step5-2","更新未完整完成，请回传反馈包。"),"to_share/Step5_STATUS.txt")
      txt_out(failed,"local_only/Step5_error_detail.txt")
      txt_out("Step5-2更新失败，完整错误保留在local_only。","to_share/Step5_run_log.txt")
      txt_out(capture.output(sessionInfo()),"to_share/Step5_sessionInfo.txt")
      issues<-data.frame(severity="ERROR",context="cached mapping update",message="Update incomplete; detailed error remains local_only")
      csv_out(issues,"to_share/09_issues.csv");publish()
    } else {
      stale<-"local_only/Step5_error_detail.txt"
      if(stale%in%old&&file.exists(safe_path(stale)))unlink(safe_path(stale))
      old<<-old[old!=stale];writeLines(old,manifest)
    }
    zip_ok<-FALSE
    if(isTRUE(cfg$MAKE_ZIP)){
      rel<-"Step5_feedback.zip";zp<-safe_path(rel)
      if(file.exists(zp))unlink(zp)
      old<<-unique(c(old,rel));writeLines(old,manifest)
      share<-file.path(out,"to_share")
      files<-sort(basename(old[startsWith(old,"to_share/")&file.exists(file.path(out,old))]))
      zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share) else {
        wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))
      };file.exists(zp)&&file.info(zp)$size>0},error=function(e)FALSE)
    }
    cat("\nStep5状态：",if(ok)"COMPLETED_FOR_REVIEW" else "FAILED","\n固定输出：\n",out,"\n",sep="")
    if(zip_ok)cat("请回传：\n",file.path(out,"Step5_feedback.zip"),"\n",sep="") else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
    if(!is.null(failed))cat("本次未完成：",failed,"\n")
    invisible(list(status=if(ok)"COMPLETED_FOR_REVIEW" else "FAILED",output_dir=out,zip_ok=zip_ok))
  }
  execute()
})
