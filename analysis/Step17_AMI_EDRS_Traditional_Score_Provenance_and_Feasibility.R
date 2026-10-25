# AMI-EDRS revision | Step17: traditional-score provenance and feasibility audit
# source(file.choose(), encoding="UTF-8")
# Fixed filenames; replaces owned results. Patient-level data and frozen references stay local_only.
local({
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MAKE_PNG=TRUE,MAKE_ZIP=TRUE,
    LEGACY_MIMIC_DIR=file.path(Sys.getenv("AMI_MIMIC_PROJECT", unset=file.path(getwd(), "work/MIMIC")), "13_step08B_traditional_scores_STAGED_FAST"),
    LEGACY_EICU_DIR=file.path(Sys.getenv("AMI_EICU_PROJECT", unset=file.path(getwd(), "work/eICU")), "03_feature_extraction/E2D_support_treatment_proxies"),
    RAW_MIMIC_DIR=Sys.getenv("AMI_MIMIC_RAW", unset=file.path(getwd(), "data/mimic-iv-3.1")),RAW_EICU_DIR=Sys.getenv("AMI_EICU_RAW", unset=file.path(getwd(), "data/eicu-crd-2.0")),FILES=list())
  cfg<-utils::modifyList(cfg,getOption("ami_step17.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step17")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step17输出目录。")
  if (is_link(out)) stop("Step17输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP17")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step17目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP17_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP17_MANAGED_OUTPUT")) {
    stop("Step17输出目录的归属标记不正确。")
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
      if (file.exists(p) && !startsWith(rel,"local_only/cache/")) {
        if (dir.exists(p)) stop("登记的输出应为文件，不能为目录。")
        if (unlink(p) != 0L) stop("不能覆盖旧Step17输出；请关闭正在使用它的程序。")
      }
    }
  }
  registered <- old[startsWith(old,"local_only/cache/") & file.exists(file.path(out,old))]
  writeLines(registered, manifest)
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
  logp <- reg("to_share/Step17_run_log.txt")
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
  warning_rows<-list()
  add_warning<-function(msg,scope){warning_rows[[length(warning_rows)+1L]]<<-data.frame(scope=scope,message=msg,stringsAsFactors=FALSE)}
  flush_warnings<-function(){
    z<-if(length(warning_rows))do.call(rbind,warning_rows)else data.frame(scope=character(),message=character())
    txt_out(if(nrow(z))unlist(Map(function(a,b)c(a,b,""),z$scope,z$message))else"No warnings in Step17.","local_only/Step17_warning_detail.txt")
    a<-if(nrow(z))aggregate(list(n=rep(1L,nrow(z))),list(scope=z$scope),sum)else data.frame(scope=character(),n=integer())
    csv_out(a,"to_share/09_warning_summary.csv")
  }
  plan<-list(version="Step17_v1",population="Full frozen T48 cohorts; no Step16 exclusions",
    task="Audit legacy score implementation, native APACHE versions, current component coverage and missing prerequisites",
    decision="No score is certified by its column name; no performance estimation or model fitting in this step",
    timing="0-24h cached measurement coverage is a screen, not certification of original score timing or completeness",
    native="Keep APACHE IV and IVa separate; hospital probability must be numeric in [0,1]; do not convert percentages by guesswork",
    duplicates="Identical numeric values may collapse within stay/version/field; discordant or mixed valid/missing rows remain unresolved")
  need<-function(x,n,label){if(!is.data.frame(x)||!all(n%in%names(x)))stop(label,"缺少字段：",paste(setdiff(n,names(x)),collapse=","))}
  keys<-function(x)paste(x$source,as.character(x$stay_id),sep=":")
  binary<-function(x){z<-as.character(x);if(anyNA(z)||any(!z%in%c("0","1")))stop("冻结结局必须为完整0/1。");as.integer(z)}
  hash_object<-function(x){p<-tempfile();on.exit(unlink(p));saveRDS(x,p,version=2);unname(tools::md5sum(p))}
  namekey<-function(x)tolower(gsub("[^A-Za-z0-9]","",x))
  colname<-function(x,aliases,required=TRUE){hit<-which(namekey(names(x))%in%namekey(aliases))
    if(length(hit)>1L)stop("字段别名不唯一：",paste(names(x)[hit],collapse=","))
    if(!length(hit)){if(required)stop("缺少字段：",paste(aliases,collapse="/"));return(NULL)};names(x)[hit]}
  id_values<-function(x){z<-trimws(as.character(x));z[is.na(z)|!grepl("^[0-9]+$",z)]<-NA_character_;z}
  parse_value<-function(x,kind="score"){
    raw<-trimws(as.character(x));blank<-is.na(raw)|toupper(raw)%in%c("","NA","N/A","NULL","NAN")
    validnum<-!blank&grepl("^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$",raw)
    val<-rep(NA_real_,length(raw));val[validnum]<-suppressWarnings(as.numeric(raw[validnum]));st<-rep("NON_NUMERIC",length(raw));st[blank]<-"MISSING"
    st[validnum&!is.finite(val)]<-"NON_FINITE";st[validnum&is.finite(val)]<-"VALID"
    st[validnum&is.finite(val)&val<0]<-"NEGATIVE_SENTINEL"
    if(kind=="probability")st[validnum&is.finite(val)&val>1]<-"OUT_OF_RANGE"
    if(kind=="native_score")st[st=="VALID"&abs(val-round(val))>1e-8]<-"NON_INTEGER"
    data.frame(raw=raw,value=val,status=st,stringsAsFactors=FALSE)
  }
  collapse_values<-function(ids,x,roster_ids,kind="score"){
    if(length(ids)!=length(x)||anyNA(roster_ids)||anyDuplicated(roster_ids))stop("重复核对输入不完整。")
    ids<-id_values(ids);p<-parse_value(x,kind);groups<-split(which(!is.na(ids)&ids%in%roster_ids),ids[!is.na(ids)&ids%in%roster_ids])
    out<-data.frame(stay_id=as.character(roster_ids),value=NA_real_,status="NO_ROW",n_rows=0L,stringsAsFactors=FALSE)
    for(k in names(groups)){ii<-groups[[k]];j<-match(k,roster_ids);a<-p[ii,,drop=FALSE];out$n_rows[j]<-length(ii)
      if(all(a$status=="VALID")&&length(unique(a$value))==1L){out$value[j]<-a$value[1];out$status[j]<-if(length(ii)>1)"VALID_DUPLICATE_IDENTICAL"else"VALID"}
      else if(length(ii)>1L&&length(unique(paste(a$status,ifelse(is.na(a$value),a$raw,format(a$value,digits=17)))))>1L)out$status[j]<-"CONFLICTING_ROWS"
      else out$status[j]<-a$status[1]
    };out
  }
  is_valid<-function(x)x%in%c("VALID","VALID_DUPLICATE_IDENTICAL")
  numeric_summary<-function(x){x<-x[is.finite(x)];if(!length(x))return(c(min=NA,q25=NA,median=NA,q75=NA,max=NA))
    setNames(as.numeric(quantile(x,c(0,.25,.5,.75,1),names=FALSE)),c("min","q25","median","q75","max"))}
  audit_native<-function(x,roster,pred){
    rr<-roster[roster$source=="eICU",,drop=FALSE];ids<-as.character(rr$stay_id)
    idc<-colname(x,c("patientunitstayid","stay_id"));vc<-colname(x,"apacheversion")
    vs<-toupper(trimws(as.character(x[[vc]])));vs[is.na(vs)|!nzchar(vs)]<-"MISSING_VERSION"
    # Preserve undocumented version labels, without selecting a version by performance.
    canon<-ifelse(vs%in%c("4","IV"),"IV",ifelse(vs%in%c("4A","IVA"),"IVa",paste0("UNRESOLVED:",vs)))
    pcols<-list(hospital_probability=colname(x,"predictedhospitalmortality",FALSE),apache_score=colname(x,"apachescore",FALSE),acute_physiology_score=colname(x,"acutephysiologyscore",FALSE))
    pm<-match(paste("eICU",ids,sep=":"),keys(pred$metadata));if(anyNA(pm))stop("Step13患者匹配失败。")
    model_ok<-apply(pred$prediction[pm,,drop=FALSE],1,function(z)all(is.finite(z)&z>=0&z<=1))
    rows<-patients<-statuses<-list()
    for(v in sort(unique(canon)))for(nm in names(pcols)){
      z<-x[canon==v,,drop=FALSE];field<-pcols[[nm]]
      cc<-if(is.null(field))data.frame(stay_id=ids,value=NA_real_,status="FIELD_ABSENT",n_rows=0L)else collapse_values(z[[idc]],z[[field]],ids,if(nm=="hospital_probability")"probability"else"native_score")
      keep<-is_valid(cc$status)&model_ok;q<-numeric_summary(cc$value[keep]);valid<-is_valid(cc$status)
      rows[[length(rows)+1L]]<-data.frame(version=v,field=nm,frozen_n=length(ids),patients_with_rows=sum(cc$n_rows>0),valid_n=sum(valid),joint_model_n=sum(keep),hospital_deaths=sum(rr$death_hospital[keep]),
        no_row_n=sum(cc$status=="NO_ROW"),conflict_n=sum(cc$status=="CONFLICTING_ROWS"),other_unusable_n=sum(!valid&!cc$status%in%c("NO_ROW","CONFLICTING_ROWS")),t(q),
        time_verified_by_T48=FALSE,role="NATIVE_ADMISSION_BENCHMARK_CANDIDATE",stringsAsFactors=FALSE)
      cc$version<-v;cc$field<-nm;cc$joint_model_available<-keep;patients[[length(patients)+1L]]<-cc
      tab<-as.data.frame(table(status=cc$status),stringsAsFactors=FALSE);tab$version<-v;tab$field<-nm;statuses[[length(statuses)+1L]]<-tab
    }
    list(summary=if(length(rows))do.call(rbind,rows)else data.frame(),patients=if(length(patients))do.call(rbind,patients)else data.frame(),statuses=if(length(statuses))do.call(rbind,statuses)else data.frame(),
      versions=as.data.frame(table(raw_version=vs),stringsAsFactors=FALSE),invalid_id_rows=sum(is.na(id_values(x[[idc]]))),outside_frozen_rows=sum(!is.na(id_values(x[[idc]]))&!id_values(x[[idc]])%in%ids))
  }
  audit_legacy<-function(x,roster,manifest=NULL){
    rr<-roster[roster$source=="MIMIC",,drop=FALSE];ids<-as.character(rr$stay_id);idc<-colname(x,c("stay_id","icustay_id"))
    cols<-names(x)[grepl("sofa|saps|oasis|apsiii|aps_iii|lods|grace",names(x),ignore.case=TRUE)]
    rows<-patients<-list()
    for(nm in cols){cc<-collapse_values(x[[idc]],x[[nm]],ids);ok<-is_valid(cc$status);qs<-numeric_summary(cc$value[ok])
      proxy<-grepl("proxy|like|adapt",nm,ignore.case=TRUE)
      # A legacy manifest is descriptive evidence; it never grants certification.
      evidence<-"";if(!is.null(manifest)&&nrow(manifest)){
        hits<-apply(manifest,1,function(r)any(namekey(r)==namekey(nm),na.rm=TRUE))
        if(any(hits))evidence<-paste(as.character(unlist(manifest[hits,,drop=FALSE])),collapse=" | ")
      }
      proxy<-proxy||grepl("proxy|like|adapt|approx",evidence,ignore.case=TRUE)
      rows[[length(rows)+1L]]<-data.frame(column=nm,frozen_n=length(ids),valid_numeric_n=sum(ok),hospital_deaths=sum(rr$death_hospital[ok]),day30_deaths=sum(rr$death_day30[ok]),
        no_row_n=sum(cc$status=="NO_ROW"),conflict_n=sum(cc$status=="CONFLICTING_ROWS"),other_unusable_n=sum(!ok&!cc$status%in%c("NO_ROW","CONFLICTING_ROWS")),t(qs),
        classification=if(proxy)"EXPLICIT_PROXY_OR_ADAPTED_LABEL"else"UNVERIFIED_LEGACY_IMPLEMENTATION",approved_for_primary_comparison=FALSE,stringsAsFactors=FALSE)
      cc$column<-nm;patients[[length(patients)+1L]]<-cc
    }
    list(summary=if(length(rows))do.call(rbind,rows)else data.frame(),patients=if(length(patients))do.call(rbind,patients)else data.frame(),
      invalid_id_rows=sum(is.na(id_values(x[[idc]]))),outside_frozen_rows=sum(!is.na(id_values(x[[idc]]))&!id_values(x[[idc]])%in%ids))
  }
  component_labels<-c(heart_rate="Heart rate",sbp="Systolic BP",map="Mean arterial pressure",resp_rate="Respiratory rate",temperature="Temperature",gcs_total="Complete GCS total",
    creatinine="Creatinine",bun="BUN",platelet="Platelets",total_bilirubin="Total bilirubin",sodium="Sodium",potassium="Potassium",wbc="White cell count",bicarbonate="Bicarbonate",albumin="Albumin",hematocrit="Hematocrit",glucose="Glucose",pt="Prothrombin time")
  feature_availability<-function(f,roster){
    need(f,c("source","stay_id","variable","unit","n_chart_times","min_value","max_value","first_hour","last_hour"),"Step4首24小时特征")
    if(any(!keys(f)%in%keys(roster))||anyDuplicated(paste(keys(f),f$variable)))stop("首24小时特征患者或组件重复。")
    n<-suppressWarnings(as.numeric(f$n_chart_times));if(any(!is.finite(n)|n<0))stop("观测次数无效。")
    observed<-n>0
    if(any(observed&(!is.finite(f$min_value)|!is.finite(f$max_value)|f$min_value>f$max_value|!is.finite(f$first_hour)|!is.finite(f$last_hour)|f$first_hour<0|f$last_hour>24|f$first_hour>f$last_hour)))stop("首24小时特征的数值或时间窗不符。")
    rows<-list()
    for(src in c("MIMIC","eICU"))for(v in names(component_labels)){
      z<-f[f$source==src&f$variable==v,,drop=FALSE];den<-sum(roster$source==src);present<-nrow(z)>0
      if(present&&nrow(z)!=den)stop("组件行数未覆盖全部冻结患者：",src,"/",v)
      cnt<-if(present)sum(as.numeric(z$n_chart_times)>0)else NA_integer_
      rows[[length(rows)+1L]]<-data.frame(source=src,variable=v,label=unname(component_labels[v]),unit=paste(unique(z$unit),collapse=";"),cohort_n=den,available_n=cnt,
        availability=cnt/den,window="0-24h",cache_status=if(present)"QUERIED_CURRENT_CACHE"else"NOT_IN_CURRENT_CACHE",score_component_certified=FALSE)
    };do.call(rbind,rows)
  }
  strict_csv_sql<-function(path,q)paste0("read_csv(",q(path),",header=true,all_varchar=true,auto_detect=true,delim=',',quote='\"',escape='\"',sample_size=-1,ignore_errors=false,strict_mode=true,null_padding=false,skip=0)")
  read_csv_local<-function(path){
    cc<-DBI::dbConnect(duckdb::duckdb(),dbdir=":memory:");on.exit(DBI::dbDisconnect(cc,shutdown=TRUE));q<-function(x)as.character(DBI::dbQuoteString(cc,x))
    DBI::dbGetQuery(cc,paste("SELECT * FROM",strict_csv_sql(path,q)))
  }
  load_features<-function(path){
    cc<-DBI::dbConnect(duckdb::duckdb(),dbdir=path,read_only=TRUE);on.exit(DBI::dbDisconnect(cc,shutdown=TRUE))
    sql<-"SELECT source,CAST(stay_id AS VARCHAR) AS stay_id,variable,unit,n_chart_times,min_value,max_value,first_hour,last_hour FROM feature_summary WHERE landmark='T48' AND \"window\"='0-24h'"
    DBI::dbGetQuery(cc,sql)
  }
  fingerprint<-function(p){i<-file.info(p);data.frame(path=norm(p),bytes=i$size,mtime=as.numeric(i$mtime),md5=if(i$size<=5e7)unname(tools::md5sum(p))else NA_character_,stringsAsFactors=FALSE)}
  choose_file<-function(key,candidates){
    explicit<-cfg$FILES[[key]]
    if(!is.null(explicit)){if(length(explicit)!=1L||!file.exists(explicit))stop("指定文件不存在：",key);return(norm(explicit))}
    p<-unique(vapply(candidates[file.exists(candidates)],norm,character(1)))
    if(!length(p))return(NULL)
    if(length(p)==1L)return(p)
    h<-tools::md5sum(p);if(length(unique(unname(h)))==1L){issue("NOTE",key,"发现内容相同的重复文件，按规范路径排序选取；未按修改日期选取。");return(sort(p)[1])}
    csv_out(data.frame(key=key,path=p,md5=unname(h)),paste0("to_share/01_ambiguous_",key,".csv"))
    issue("UNRESOLVED",key,paste0("多份内容不同的候选文件，未自动合并；可通过options(ami_step17.config=list(FILES=list(",key,"='完整路径')))指定。"));NULL
  }
  preflight<-function(){
    z<-parse_value(c("0","1","0.25","25%","25","-1","","NaN","Inf"),"probability")
    stopifnot(identical(z$status,c("VALID","VALID","VALID","NON_NUMERIC","OUT_OF_RANGE","NEGATIVE_SENTINEL","MISSING","MISSING","NON_NUMERIC")))
    d<-collapse_values(c("1","1","2","2","3","3","4","4"),c("0.2","0.20",".2",".3",".2","-1","-1","-1"),as.character(1:5),"probability")
    stopifnot(identical(d$status,c("VALID_DUPLICATE_IDENTICAL","CONFLICTING_ROWS","CONFLICTING_ROWS","NEGATIVE_SENTINEL","NO_ROW")),d$value[1]==.2)
    rr<-data.frame(source=c("MIMIC","MIMIC","eICU","eICU"),stay_id=c("1","2","1","2"),death_hospital=c(1,0,0,1),death_day30=c(1,0,NA,NA))
    pp<-list(metadata=rr[3:4,c("source","stay_id")],prediction=matrix(.1,2,4))
    x<-data.frame(patientunitstayid=c("1","1","2","2"),apacheVersion=c("IV","IVa","IV","IVa"),predictedHospitalMortality=c(".1",".2","-1","1"),apacheScore=c("20","20","30","30"),acutePhysiologyScore=c("10","10","20","20"))
    a<-audit_native(x,rr,pp);b<-a$summary[a$summary$field=="hospital_probability",]
    stopifnot(nrow(b)==2L,b$valid_n[b$version=="IV"]==1,b$valid_n[b$version=="IVa"]==2)
    ll<-audit_legacy(data.frame(stay_id=c("2","1"),SOFA=c("2","1"),grace_like_proxy=c("40","30")),rr)
    stopifnot(all(!ll$summary$approved_for_primary_comparison),ll$summary$classification[ll$summary$column=="SOFA"]=="UNVERIFIED_LEGACY_IMPLEMENTATION")
    no<-audit_native(x[FALSE,],rr,pp);stopifnot(nrow(no$summary)==0)
    stopifnot(inherits(try(colname(data.frame(stay_id=1,stayid=1),"stay_id"),silent=TRUE),"try-error"))
    TRUE
  }

  check_deps<-function(){for(p in c("DBI","duckdb"))if(!requireNamespace(p,quietly=TRUE))stop("缺少已有依赖包：",p,"；请先install.packages('",p,"')。")}
  reference_spec<-function()data.frame(
    score=c("SOFA","SAPS II","OASIS","APS III","LODS","GRACE (hospital)"),
    reference_context=c("First-day organ dysfunction","First-day ICU severity","First-day ICU severity","Admission acute physiology","First-day organ dysfunction","ACS presentation"),
    key_unverified_inputs=c("Matched arterial PaO2/FiO2 and support; vasoactive agent/dose; urine output; GCS conventions",
      "Oxygenation with ventilation; urine output; admission type; AIDS/hematologic/metastatic disease; full scoring rules",
      "Pre-ICU hospital duration; urine output; ventilation; elective surgery; full scoring thresholds",
      "Oxygenation/acid-base combinations; urine output; neurologic and chronic/dialysis conventions; complete rules",
      "Respiratory support/oxygenation; urine output; prothrombin definition; complete organ scoring rules",
      "Presentation Killip class, ST deviation, arrest, initial enzyme positivity and presentation-time vitals/creatinine"),
    existing_cache_role=c(rep("Partial component screen only",5),"ICU first-day values do not certify presentation-time inputs"),
    status="NOT_CERTIFIED_BY_THIS_AUDIT",stringsAsFactors=FALSE)
  main<-function(){
    current_module<<-"preflight";logmsg("Step17预检：原版与代理标签、APACHE版本、概率范围、同版本重复、缺失与空输入。");preflight();check_deps()
    txt_out("PASS: exact aliases; probability bounds; sentinel and missing values; no cross-version averaging; conflicting duplicates unresolved; legacy names cannot certify original scores; empty input.","to_share/02_preflight.txt")
    for(st in c("Step4","Step13","Step15","Step16")){
      p<-file.path(cfg$OUTPUT_PARENT,st,"to_share",paste0(st,"_STATUS.txt"));if(!file.exists(p)||!"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(p,warn=FALSE))stop("请先完成",st,"。")}
    paths<-file.path(cfg$OUTPUT_PARENT,c(roster="Step15/local_only/frozen_T48_roster.rds",input15="Step15/to_share/01_inputs.csv",db="Step4/local_only/Step4_harmonized_features.duckdb",av4="Step4/to_share/07_common_feature_availability.csv",pred="Step13/local_only/eICU_T48_hospital_frozen_predictions.rds"))
    names(paths)<-c("roster","input15","db","av4","pred");if(!all(file.exists(paths)))stop("缺少本地上游文件：",paste(names(paths)[!file.exists(paths)],collapse=","))
    inputs<-do.call(rbind,lapply(paths,fingerprint));rownames(inputs)<-NULL
    roster<-readRDS(paths["roster"]);need(roster,c("source","stay_id","death_hospital","death_day30"),"冻结名单")
    if(anyNA(keys(roster))||anyNA(roster$stay_id)||anyDuplicated(keys(roster))||nrow(roster)!=10417L||any(!roster$source%in%c("MIMIC","eICU")))stop("冻结患者键不完整或重复。")
    for(src in c("MIMIC","eICU")){rr<-roster[roster$source==src,];if(nrow(rr)!=c(MIMIC=5133,eICU=5284)[src]||sum(binary(rr$death_hospital))!=c(MIMIC=461,eICU=219)[src])stop("冻结队列人数或住院结局改变：",src)}
    if(sum(binary(roster$death_day30[roster$source=="MIMIC"]))!=630L||any(!is.na(roster$death_day30[roster$source=="eICU"])))stop("冻结30天结局异常。")
    old<-read_csv_local(paths["input15"]);if(nrow(old)!=1L||norm(old$file)!=norm(paths["db"])||as.numeric(old$bytes)!=file.info(paths["db"])$size||abs(as.numeric(old$mtime)-as.numeric(file.info(paths["db"])$mtime))>.01)stop("Step4数据库与Step15的输入记录不符。")
    pred<-readRDS(paths["pred"]);need(pred$metadata,c("source","stay_id","landmark"),"Step13");need(pred$outcomes,c("event_primary","endpoint"),"Step13")
    er<-roster[roster$source=="eICU",];idx<-match(keys(pred$metadata),keys(er))
    if(!identical(pred$version,"Step13_v1")||nrow(pred$metadata)!=5284L||anyNA(idx)||anyDuplicated(idx)||anyNA(pred$metadata$landmark)||any(pred$metadata$landmark!="T48")||!identical(binary(pred$outcomes$event_primary),binary(er$death_hospital[idx]))||anyNA(pred$outcomes$endpoint)||any(pred$outcomes$endpoint!="in_hospital_death_after_T48"))stop("Step13冻结预测与住院终点不匹配。")
    if(!is.matrix(pred$prediction)||nrow(pred$prediction)!=5284L||!identical(colnames(pred$prediction),c("A","B","C","D"))||any(!is.finite(pred$prediction))||any(pred$prediction<0|pred$prediction>1))stop("Step13 A-D概率不完整。")
    csv_out(data.frame(source=c("MIMIC","eICU"),patients=c(5133,5284),hospital_deaths=c(461,219),day30_deaths=c(630,NA)),"to_share/02_cohort.csv")
    txt_out(capture.output(dput(plan)),"to_share/02_analysis_plan.txt")
    current_module<<-"current cached component availability";logmsg("核对冻结T48队列的首24小时组件可用率；不把组件覆盖率当作完整评分可用率。")
    f<-load_features(paths["db"]);av<-feature_availability(f,roster)
    prior<-read_csv_local(paths["av4"]);prior<-prior[prior$landmark=="T48"&prior$window=="0-24h",]
    j<-match(paste(av$source,av$variable),paste(prior$source,prior$variable));good<-!is.na(av$available_n)
    if(anyNA(j[good])||any(av$available_n[good]!=as.numeric(prior$measured_patients[j[good]])))stop("首24小时组件可用率未复现Step4。")
    csv_out(av,"to_share/04_current_component_availability.csv");saveRDS(f,reg("local_only/current_first24h_component_screen.rds"),compress="gzip")
    # Candidate raw files are inventoried only. Large chart/lab/event files are not scanned.
    current_module<<-"raw prerequisites inventory"
    stems<-list(MIMIC=c("outputevents","inputevents","procedureevents","chartevents","labevents","admissions","services"),
      eICU=c("apachePatientResult","apacheApsVar","apachePredVar","intakeOutput","respiratoryCare","respiratoryCharting","infusionDrug"))
    ec<-if(dir.exists(cfg$RAW_EICU_DIR))list.files(cfg$RAW_EICU_DIR,pattern="[.]csv([.]gz)?$",recursive=TRUE,full.names=TRUE,ignore.case=TRUE)else character()
    raw_rows<-list();raw_native<-character()
    for(src in names(stems))for(stem in stems[[src]]){
      pp<-if(src=="eICU")ec[grepl(paste0("^",stem,"[.]csv([.]gz)?$"),basename(ec),ignore.case=TRUE)]else as.vector(outer(file.path(cfg$RAW_MIMIC_DIR,c("icu","hosp","")),paste0(stem,c(".csv.gz",".csv")),file.path))
      pp<-unique(vapply(pp[file.exists(pp)],norm,character(1)));if(src=="eICU"&&stem=="apachePatientResult")raw_native<-pp
      if(length(pp))for(p in pp)raw_rows[[length(raw_rows)+1L]]<-data.frame(source=src,table=stem,path=p,file_present=TRUE,bytes=file.info(p)$size,contents_validated=FALSE)
      else raw_rows[[length(raw_rows)+1L]]<-data.frame(source=src,table=stem,path="",file_present=FALSE,bytes=NA_real_,contents_validated=FALSE)
    };csv_out(do.call(rbind,raw_rows),"to_share/01_raw_prerequisite_inventory.csv")
    current_module<<-"legacy implementation audit"
    targets<-list(MIMIC_legacy_scores=file.path(cfg$LEGACY_MIMIC_DIR,"analysis_datasets/Step08B_reconstructed_traditional_scores.csv"),
      MIMIC_legacy_manifest=file.path(cfg$LEGACY_MIMIC_DIR,"tables/Step08B_score_manifest.csv"),
      MIMIC_legacy_components=file.path(cfg$LEGACY_MIMIC_DIR,"tables/Step08B_score_component_availability.csv"),
      MIMIC_legacy_code=file.path(cfg$LEGACY_MIMIC_DIR,"Step08B_STAGED_TraditionalScores_FAST.R"),
      eICU_legacy_apache=file.path(cfg$LEGACY_EICU_DIR,"StepE2D_APACHE_patient_result_for_T48_cohort.csv"))
    selected<-lapply(names(targets),function(k)choose_file(k,targets[[k]]));names(selected)<-names(targets)
    state<-data.frame(key=names(selected),path=vapply(selected,function(p)if(is.null(p))""else p,character(1)),found=!vapply(selected,is.null,logical(1)))
    csv_out(state,"to_share/01_legacy_file_availability.csv")
    manifests<-NULL
    for(k in c("MIMIC_legacy_manifest","MIMIC_legacy_components"))if(!is.null(selected[[k]])){
      z<-read_csv_local(selected[[k]]);inputs<-rbind(inputs,fingerprint(selected[[k]]));csv_out(data.frame(file=k,column=names(z)),paste0("to_share/03_",k,"_columns.csv"))
      # These two known files are aggregate manifests, not patient score datasets.
      if(any(namekey(names(z))%in%c("stayid","patientunitstayid","subjectid","hadmid")))stop(k,"意外包含患者标识，不写入反馈包。")
      csv_out(z,paste0("to_share/03_",k,".csv"));if(k=="MIMIC_legacy_manifest")manifests<-z
    }
    legacy<-list(summary=data.frame(),patients=data.frame())
    if(!is.null(selected$MIMIC_legacy_scores)){
      logmsg("读取旧Step08B评分；仅审计字段、实现标签和当前名单覆盖，不使用旧概率或旧结局。")
      z<-read_csv_local(selected$MIMIC_legacy_scores);legacy<-audit_legacy(z,roster,manifests);inputs<-rbind(inputs,fingerprint(selected$MIMIC_legacy_scores))
      csv_out(data.frame(column=names(z)),"to_share/03_legacy_score_columns.csv");csv_out(legacy$summary,"to_share/03_legacy_score_coverage.csv")
      csv_out(data.frame(invalid_id_rows=legacy$invalid_id_rows,outside_frozen_rows=legacy$outside_frozen_rows),"to_share/03_legacy_patient_key_audit.csv")
      saveRDS(legacy$patients,reg("local_only/legacy_score_alignment.rds"),compress="gzip")
      if(!nrow(legacy$summary))issue("UNRESOLVED","legacy scores","旧评分文件未找到六种评分的候选列；只回传字段名等待核对。")
    }else issue("UNRESOLVED","legacy scores","旧Step08B评分文件未找到；未构造替代评分。")
    if(!is.null(selected$MIMIC_legacy_code)){
      code<-readLines(selected$MIMIC_legacy_code,warn=FALSE,encoding="UTF-8");inputs<-rbind(inputs,fingerprint(selected$MIMIC_legacy_code))
      hit<-grep("sofa|saps|oasis|apsiii|aps_iii|lods|grace|proxy|killip|urine|vent|gcs|raw_.*24h",code,ignore.case=TRUE)
      ii<-sort(unique(unlist(lapply(hit,function(i)seq.int(max(1L,i-4L),min(length(code),i+5L))))));total<-length(ii);ii<-head(ii,1000L)
      txt_out(c(paste("Source:",selected$MIMIC_legacy_code),paste("Selected lines:",length(ii),"of",total,"matching-context lines; entire script was NOT executed."),sprintf("%05d  %s",ii,code[ii])),"to_share/03_legacy_scoring_code_evidence.txt")
    }else issue("UNRESOLVED","legacy implementation","旧Step08B实现脚本未找到；原版身份仍需核查。")
    current_module<<-"native APACHE version and duplicate audit"
    p<-choose_file("eICU_apachePatientResult",raw_native);origin<-"RAW_NATIVE_TABLE"
    if(is.null(p)){p<-selected$eICU_legacy_apache;origin<-"LEGACY_EXTRACT_PARTIAL_COVERAGE_POSSIBLE"}
    native<-list(summary=data.frame(),patients=data.frame())
    if(!is.null(p)){
      logmsg(paste0("读取eICU APACHE：",origin,"；IV与IVa分别核对，缺失及冲突不取平均。"))
      z<-read_csv_local(p);native<-audit_native(z,roster,pred);inputs<-rbind(inputs,fingerprint(p))
      csv_out(data.frame(origin=origin,file=p,column=names(z)),"to_share/05_native_source_columns.csv")
      csv_out(native$summary,"to_share/05_native_APACHE_coverage.csv");csv_out(native$statuses,"to_share/05_native_APACHE_value_status.csv");csv_out(native$versions,"to_share/05_native_APACHE_versions.csv")
      csv_out(data.frame(invalid_id_rows=native$invalid_id_rows,outside_frozen_rows=native$outside_frozen_rows),"to_share/05_native_patient_key_audit.csv")
      saveRDS(native$patients,reg("local_only/native_APACHE_alignment.rds"),compress="gzip")
      issue("LIMITATION","APACHE timing","APACHE原生结果没有逐条生成/录入时刻；本步不能证明T48实时可用。作为入院严重度背景对照的适用性另行确认，不并入动态模型。")
    }else issue("UNRESOLVED","native APACHE","未找到原始表或既有提取表；没有将APACHE缺失值当正常或死亡风险0。")
    spec<-reference_spec();csv_out(spec,"to_share/06_score_definition_and_remaining_requirements.csv")
    saveRDS(list(version=plan$version,plan=plan,roster_hash=hash_object(roster),legacy=legacy,native=native,component_availability=av,requirements=spec,input_fingerprints=inputs),reg("local_only/Step17_score_feasibility.rds"),compress="gzip")
    current_module<<-"large-font coverage figures and tables";make_visuals(av,native$summary)
    csv_out(unique(inputs),"to_share/01_inputs.csv")
    csv_out(data.frame(output=c("Figure_S_traditional_component_availability","Table_S_traditional_component_availability","Table_S_native_APACHE_coverage"),
      action="ADD supplementary feasibility audit; old score-performance figures await source validation",reviewer="R3 major7; R1 major5 and minor5"),"to_share/11_figure_table_replacement_map.csv")
    txt_out(c("Step17：传统评分的实现来源、版本与覆盖率核查。完成状态不代表已完成传统评分性能比较。",
      "使用完整冻结T48队列：MIMIC 5133人、eICU 5284人，不沿用Step16的排除名单。所有死亡标签来自修订后的冻结队列。",
      "旧Step08B文件仅用于查明实际评分实现和可用病例。列名或旧manifest声称Traditional/Original/Official，不足以认定为正式原版。",
      "首24小时缓存仅报告已提取组件的覆盖率，不计算任何proxy总分。首24小时ICU数值不等同GRACE就诊时输入；不能把单个GCS组件当总分。",
      "06表列出关键待核项目，不是完整评分公式。NOT_IN_CURRENT_CACHE不表示原始数据库没有该信息。文件存在也不表示其内容已验证。",
      "eICU APACHE优先使用唯一原始表；原始表缺失或多份不一致时保留问题清单，可用既有提取表作为明确标记的覆盖率审计来源。",
      "APACHE IV与IVa分别保留。同一患者/版本/字段中一致重复可以合并；不同概率或有效/缺失混杂时标记冲突，不取均值、不取最大值。",
      "死亡概率按官方文档的0-1小数验证，不把12或12%自动解释成0.12。负值、字符串、缺失和范围错误分开汇报。APACHE score及APS不被当作死亡概率。",
      "原生APACHE表无逐条录入时间，不能证明实时T48可得；其原始住院风险也不是已条件化于T48存活的风险。后续比较必须说明这一人群与预测时点差异。",
      "不读取旧训练/测试概率来替代修订后的OOF，不重新选模型、不改变分型、结局或患者纳排。性能比较待本次来源核对后再进行。",
      "本步不扫描大型chartevents/labevents。建议保持T7连接以定位原始APACHE及组件表；未连接时会标记来源缺口，不推断原始数据不存在。",
      "结果写入修改/Step17，患者级对齐表只留local_only。请回传Step17_feedback.zip。图基础18pt，表正文18pt。"),"to_share/00_READ_ME_FIRST.txt")
    txt_out(c("https://github.com/MIT-LCP/mimic-code/blob/main/mimic-iv/concepts/firstday/first_day_sofa.sql",
      "https://github.com/MIT-LCP/mimic-code/blob/main/mimic-iv/concepts/score/sapsii.sql",
      "https://github.com/MIT-LCP/mimic-code/blob/main/mimic-iv/concepts_postgres/score/oasis.sql",
      "https://pubmed.ncbi.nlm.nih.gov/14581255/","https://eicu.mit.edu/eicutables/apachepatientresult/","https://eicu.mit.edu/eicutables/apacheapsvar/"),"to_share/10_definition_sources.txt")
    for(i in seq_len(nrow(inputs))){a<-inputs[i,];b<-fingerprint(a$path);if(!identical(as.numeric(a$bytes),as.numeric(b$bytes))||abs(a$mtime-b$mtime)>.01||(!is.na(a$md5)&&!identical(a$md5,b$md5)))stop("运行期间输入改变：",a$path)}
    status<<-"COMPLETED_FOR_REVIEW"
  }

  make_visuals<-function(av,native){
    draw<-function(){
      par(mar=c(4.2,12,4.5,1.5),oma=c(2.8,0,0,0),cex=1,cex.axis=1,cex.lab=1,las=1,family="sans")
      yy<-rev(seq_along(component_labels));plot(NA,xlim=c(0,100),ylim=c(.5,length(yy)+.5),xlab="Patients with recorded component (%)",ylab="",yaxt="n",xaxs="i",bty="n")
      abline(v=seq(0,100,20),col="#E5E8EC",lwd=.8);abline(h=yy,col="#F0F1F3",lwd=.8)
      axis(2,at=yy,labels=unname(component_labels),tick=FALSE)
      colors<-c(MIMIC="#4F7FC4",eICU="#CB9353")
      for(src in c("MIMIC","eICU")){z<-av[av$source==src,];z<-z[match(names(component_labels),z$variable),]
        points(100*z$availability,yy+if(src=="MIMIC").13 else -.13,pch=if(src=="MIMIC")16 else 17,col=colors[src],cex=1.1)}
      mtext("First-24h component coverage in frozen T48 cohorts",side=3,line=2.8,cex=1.12,font=2)
      mtext("MIMIC n=5133",side=3,line=1.1,at=28,col=colors[1],cex=1)
      mtext("eICU n=5284",side=3,line=1.1,at=75,col=colors[2],cex=1)
      mtext("Circles: MIMIC; triangles: eICU. Missing cache components have no plotted mark.",side=1,outer=TRUE,line=.3,cex=1)
      mtext("Component coverage does not certify complete scores or original scoring windows.",side=1,outer=TRUE,line=1.6,cex=1)
    }
    grDevices::pdf(reg("to_share/Figure_S_traditional_component_availability.pdf"),width=14,height=12,family="Helvetica",pointsize=18,useDingbats=FALSE)
    tryCatch(draw(),finally=grDevices::dev.off())
    if(isTRUE(cfg$MAKE_PNG)){grDevices::png(reg("to_share/Figure_S_traditional_component_availability.png"),width=14,height=12,units="in",res=300,pointsize=18);tryCatch(draw(),finally=grDevices::dev.off())}
    pct<-function(n,den)ifelse(is.na(n),"Not in cache",sprintf("%d (%.1f%%)",n,100*n/den))
    m<-av[av$source=="MIMIC",];e<-av[av$source=="eICU",];e<-e[match(m$variable,e$variable),]
    tab<-data.frame(Component=m$label,`MIMIC: n (%)`=pct(m$available_n,m$cohort_n),`eICU: n (%)`=pct(e$available_n,e$cohort_n),check.names=FALSE)
    table_out<-function(tab,title,stem,notes){
      groups<-split(seq_len(nrow(tab)),ceiling(seq_len(nrow(tab))/18));if(!length(groups))return(invisible(NULL))
      grDevices::pdf(reg(paste0("to_share/",stem,".pdf")),width=14,height=11,family="Helvetica",pointsize=18,useDingbats=FALSE)
      tryCatch(for(k in seq_along(groups)){
        tt<-tab[groups[[k]],,drop=FALSE];grid::grid.newpage();tx<-function(s,x,y,just="left",bold=FALSE,size=18)grid::grid.text(s,x,y,just=just,gp=grid::gpar(fontsize=size,fontfamily="Helvetica",fontface=if(bold)"bold"else"plain"))
        xs<-if(ncol(tt)==3)c(.04,.65,.96)else c(.04,seq(.46,.96,length.out=ncol(tt)-1))
        tx(title,.04,.95,bold=TRUE,size=21);tx("Full frozen T48 cohorts; source and coverage audit",.04,.90)
        line<-function(y,w)grid::grid.lines(c(.04,.96),c(y,y),gp=grid::gpar(lwd=w));line(.855,1.4)
        for(j in seq_len(ncol(tt)))tx(names(tt)[j],xs[j],.83,if(j==1)"left"else"right",TRUE)
        line(.81,.8);ys<-.8-seq_len(nrow(tt))*.031
        for(i in seq_len(nrow(tt)))for(j in seq_len(ncol(tt)))tx(as.character(tt[i,j]),xs[j],ys[i],if(j==1)"left"else"right")
        line(min(ys)-.018,1.4);ny<-min(ys)-.065
        for(note in notes)for(s in strwrap(note,width=94)){tx(s,.04,ny);ny<-ny-.032}
        tx(paste(k,"/",length(groups)),.96,.035,"right")
      },finally=grDevices::dev.off())
      esc<-function(x){x<-gsub("\\","\\\\",x,fixed=TRUE);x<-gsub("{","\\{",x,fixed=TRUE);gsub("}","\\}",x,fixed=TRUE)}
      rt<-c("{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 Arial;}}\\landscape\\paperw20160\\paperh15840\\margl600\\margr600\\margt600\\margb600\\f0\\fs36")
      for(k in seq_along(groups)){
        if(k>1)rt<-c(rt,"\\page");tt<-tab[groups[[k]],,drop=FALSE];rt<-c(rt,paste0("\\pard\\b ",esc(title),"\\b0\\par\\par"));rows<-rbind(names(tt),as.matrix(tt))
        ends<-if(ncol(tt)==3)c(8000,13500,18900)else round(c(6000,seq(11000,18900,length.out=ncol(tt)-1)))
        for(i in seq_len(nrow(rows))){bd<-paste0(if(i==1)"\\clbrdrt\\brdrs\\brdrw15\\clbrdrb\\brdrs\\brdrw10"else"",if(i==nrow(rows))"\\clbrdrb\\brdrs\\brdrw15"else"")
          rt<-c(rt,paste0("\\trowd\\trgaph60\\trkeep",paste0(bd,"\\cellx",ends,collapse=""),paste(vapply(seq_len(ncol(tt)),function(j)paste0("\\pard\\intbl",if(j==1)"\\ql "else"\\qr ","\\fs36 ",if(i==1)"\\b "else"\\b0 ",esc(rows[i,j]),"\\cell"),character(1)),collapse=""),"\\row"))}
        rt<-c(rt,paste0("\\pard\\fs36 ",paste(esc(notes),collapse="\\par "),"\\par"))
      };txt_out(c(rt,"}"),paste0("to_share/",stem,".rtf"))
    }
    table_out(tab,"Traditional-score component availability","Table_S_traditional_component_availability",
      c("Records were measured and available within 0-24h under the existing Step4 rules.","MIMIC n=5133; eICU n=5284. Coverage is not proof of complete original scores."))
    if(nrow(native)){
      z<-native[native$field=="hospital_probability",,drop=FALSE]
      if(nrow(z))table_out(data.frame(Version=z$version,`Usable n (%)`=pct(z$joint_model_n,z$frozen_n),`Hospital deaths`=z$hospital_deaths,`Conflicts (n)`=z$conflict_n,check.names=FALSE),
        "Native APACHE hospital-probability coverage","Table_S_native_APACHE_coverage",
        c("Denominator: all 5284 frozen eICU patients; usable values intersect complete A-D predictions.","Versions remain separate. Conflicting records and invalid probabilities are unresolved.","Admission-score timing and T48 availability are not certified by this table."))
    }
  }

  if(isTRUE(getOption("ami_step17.functions_only",FALSE)))return(invisible(list(main=main,preflight=preflight,parse_value=parse_value,collapse_values=collapse_values,audit_native=audit_native,audit_legacy=audit_legacy,feature_availability=feature_availability,make_visuals=make_visuals)))
  status<-"FAILED";current_module<-"input verification";failure_message<-NULL
  tryCatch(withCallingHandlers(main(),warning=function(w){add_warning(conditionMessage(w),current_module);invokeRestart("muffleWarning")}),error=function(err){
    status<<-"FAILED";failure_message<<-conditionMessage(err);issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step17_error_detail.txt")})
  flush_warnings()
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step17",
    "COMPLETED_FOR_REVIEW表示来源和覆盖率审计完成；不表示传统评分已通过原定义验证或已完成性能比较。",
    "FAILED表示本步未完整完成；检查日志。上游队列和预测保持原样。"),"to_share/Step17_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step17_sessionInfo.txt");logmsg(paste0("Step17状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step17_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share)else{
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step17_feedback.zip"),"\n",sep="")else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
