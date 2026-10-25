# AMI-EDRS revision | Step6: shared organ-domain scoring specification and unfitted model inputs
# 运行：source(file.choose(), encoding="UTF-8")。无需连接T7，不扫描原始大表。
# 必须已成功完成Step4-1和Step5-2。本步不拟合全队列评分、聚类或风险模型。
# Step6-1：对齐Step2实际冻结字段；去除未输出且未使用的旧ICU入科后30天结局字段要求。
# 固定输出 修改/Step6；重跑覆盖本步骤登记输出，不增加时间戳文件。
local({
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MAKE_ZIP=TRUE,MAKE_PNG=TRUE)
  cfg<-utils::modifyList(cfg,getOption("ami_step6.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step6")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step6输出目录。")
  if (is_link(out)) stop("Step6输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP6")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step6目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP6_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP6_MANAGED_OUTPUT")) {
    stop("Step6输出目录的归属标记不正确。")
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
        if (unlink(p) != 0L) stop("不能覆盖旧Step6输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step6_run_log.txt")
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
  score_engine_factory<-function(){
    key<-function(v,w,s)paste(v,gsub("-","_",w,fixed=TRUE),s,sep="__")
    specification<-function(){
      domain<-c(rep("hemodynamic",6),rep("respiratory_neurologic",8),rep("renal_metabolic",8),rep("inflammatory_coagulopathic",8),rep("cardiac_hypoperfusion",3))
      variable<-c("heart_rate","heart_rate","sbp","sbp","map","map",
        "resp_rate","resp_rate","spo2","spo2","temperature","gcs_eye","gcs_verbal","gcs_motor",
        "creatinine","bun","sodium","potassium","chloride","bicarbonate","bicarbonate","glucose",
        "wbc","wbc","hemoglobin","platelet","platelet","inr","ptt","ptt",
        "lactate","lactate","troponin_t")
      stat<-c("max_value","abnormal_fraction","min_value","abnormal_fraction","min_value","abnormal_fraction",
        "max_value","abnormal_fraction","min_value","abnormal_fraction","abnormal_fraction","min_value","min_value","min_value",
        "max_value","max_value","abnormal_fraction","abnormal_fraction","abnormal_fraction","min_value","abnormal_fraction","abnormal_fraction",
        "max_value","abnormal_fraction","min_value","min_value","abnormal_fraction","max_value","max_value","abnormal_fraction",
        "max_value","abnormal_fraction","max_value")
      z<-data.frame(domain,variable,stat,direction=ifelse(stat=="min_value","low","high"),
        tier=ifelse(domain=="cardiac_hypoperfusion","exploratory","main"),stringsAsFactors=FALSE)
      z$component_id<-paste(z$variable,z$stat,sep="__")
      z$reference_window<-"0-6h";z$weight<-1
      z
    }
    windows_for<-function(landmark,type=c("all","cumulative","nonoverlap")){
      type<-match.arg(type);h<-as.numeric(sub("T","",landmark,fixed=TRUE))
      if(length(h)!=1||!h%in%c(6,24,48))stop("Unsupported landmark.")
      w<-c("0-6h","0-24h","0-48h","6-24h","24-48h");end<-c(6,24,48,24,48)
      keep<-end<=h
      if(type=="cumulative")keep<-keep&seq_along(w)<=3
      if(type=="nonoverlap")keep<-keep&seq_along(w)%in%c(1,4,5)
      w[keep]
    }
    checked_rows<-function(rows,n){
      if(!is.numeric(rows)||!length(rows)||anyNA(rows)||any(rows!=floor(rows))||any(rows<1|rows>n)||anyDuplicated(rows))
        stop("Explicit, unique training row indices are required.")
      as.integer(rows)
    }
    checked_numeric<-function(x){
      if(!is.numeric(x))stop("Input feature must be numeric; no implicit factor conversion.")
      if(any(is.infinite(x)|is.nan(x)))stop("Infinite/NaN values must be resolved upstream; NA is retained.")
      as.numeric(x)
    }
    fit_reference<-function(raw,train_rows,spec=specification(),min_observed=20L){
      rows<-checked_rows(train_rows,nrow(raw))
      if(anyDuplicated(spec$component_id)||any(!spec$direction%in%c("high","low")))stop("Invalid component specification.")
      refs<-vector("list",nrow(spec));names(refs)<-spec$component_id
      for(i in seq_len(nrow(spec))){
        nm<-key(spec$variable[i],spec$reference_window[i],spec$stat[i])
        if(!nm%in%names(raw))stop("Missing reference input: ",nm)
        x<-checked_numeric(raw[[nm]])[rows];x<-x[!is.na(x)]
        vals<-sort(unique(x));cnt<-tabulate(match(x,vals),nbins=length(vals))
        refs[[i]]<-list(values=vals,count=cnt,cumulative=cumsum(cnt),n=length(x),
          enabled=length(x)>=min_observed,direction=spec$direction[i],reference_column=nm)
      }
      list(version="Step6_v1",spec=spec,reference=refs,training_n=length(rows),
        min_observed=min_observed,reference_window="0-6h",tie_rule="mid-distribution percentile")
    }
    percentile<-function(x,r){
      x<-checked_numeric(x);ans<-rep(NA_real_,length(x));ok<-which(!is.na(x))
      if(!r$enabled||!length(ok))return(ans)
      xx<-x[ok];j<-findInterval(xx,r$values);less_equal<-c(0,r$cumulative)[j+1L]
      match_id<-match(xx,r$values);equal<-!is.na(match_id)
      less_equal[equal]<-less_equal[equal]-.5*r$count[match_id[equal]]
      v<-100*less_equal/r$n
      if(r$direction=="low")v<-100-v
      ans[ok]<-pmax(0,pmin(100,v));ans
    }
    apply_reference<-function(raw,reference,windows,min_fraction=.5){
      if(!identical(reference$version,"Step6_v1"))stop("Reference version mismatch.")
      if(length(min_fraction)!=1||!is.finite(min_fraction)||min_fraction<=0||min_fraction>1)stop("Invalid minimum component fraction.")
      spec<-reference$spec;domains<-unique(spec$domain);comp<-list();scores<-list();coverage<-list()
      for(w in windows){
        mm<-matrix(NA_real_,nrow(raw),nrow(spec));colnames(mm)<-spec$component_id
        for(i in seq_len(nrow(spec))){
          nm<-key(spec$variable[i],w,spec$stat[i]);if(!nm%in%names(raw))stop("Missing input column: ",nm)
          mm[,i]<-percentile(raw[[nm]],reference$reference[[i]])
          comp[[paste(spec$component_id[i],gsub("-","_",w,fixed=TRUE),sep="__")]]<-mm[,i]
        }
        for(d in domains){
          ii<-which(spec$domain==d);n<-rowSums(!is.na(mm[,ii,drop=FALSE]));required<-ceiling(length(ii)*min_fraction)
          v<-rowMeans(mm[,ii,drop=FALSE],na.rm=TRUE);v[n<required]<-NA_real_;v[is.nan(v)]<-NA_real_
          prefix<-paste(d,gsub("-","_",w,fixed=TRUE),sep="__")
          scores[[paste0(prefix,"__score")]]<-v
          coverage[[paste0(prefix,"__observed_components")]]<-n
        }
      }
      list(scores=as.data.frame(scores,check.names=FALSE),component_scores=as.data.frame(comp,check.names=FALSE),
        coverage=as.data.frame(coverage,check.names=FALSE))
    }
    fit_preprocessor<-function(x,train_rows,max_missing=.95,min_observed=20L,add_missing=TRUE){
      rows<-checked_rows(train_rows,nrow(x));keep<-character();med<-numeric()
      for(nm in names(x)){
        v<-checked_numeric(x[[nm]])[rows];obs<-v[!is.na(v)]
        if(length(obs)<min_observed||mean(is.na(v))>max_missing||length(unique(obs))<2L)next
        keep<-c(keep,nm);med[nm]<-stats::median(obs)
      }
      if(!length(keep))stop("No usable training features.")
      indicators<-if(add_missing)keep[vapply(x[rows,keep,drop=FALSE],function(v)anyNA(v),logical(1))] else character()
      design<-x[rows,keep,drop=FALSE]
      for(nm in keep){v<-checked_numeric(design[[nm]]);design[[nm]]<-ifelse(is.na(v),med[nm],v)}
      for(nm in indicators)design[[paste0(nm,"__NA")]]<-as.integer(is.na(x[[nm]][rows]))
      center<-vapply(design,mean,numeric(1));scale<-vapply(design,stats::sd,numeric(1));scale[!is.finite(scale)|scale==0]<-1
      list(version="Step6_v1",features=keep,median=med,indicators=indicators,center=center,scale=scale,
        design_columns=names(design),training_n=length(rows),max_missing=max_missing)
    }
    apply_preprocessor<-function(x,p){
      if(length(setdiff(p$features,names(x))))stop("Predictor schema mismatch.")
      z<-x[p$features]
      for(nm in p$features){v<-checked_numeric(z[[nm]]);z[[nm]]<-ifelse(is.na(v),p$median[nm],v)}
      for(nm in p$indicators)z[[paste0(nm,"__NA")]]<-as.integer(is.na(x[[nm]]))
      z<-z[p$design_columns]
      for(nm in names(z))z[[nm]]<-(z[[nm]]-p$center[nm])/p$scale[nm]
      as.matrix(z)
    }
    list(version="Step6_v1",key=key,specification=specification,windows_for=windows_for,
      fit_reference=fit_reference,apply_reference=apply_reference,percentile=percentile,
      fit_preprocessor=fit_preprocessor,apply_preprocessor=apply_preprocessor)
  }
  engine<-score_engine_factory()
  make_folds<-function(patient_key,y,k=5L,seed=2026L){
    if(anyNA(patient_key)||anyDuplicated(patient_key)||anyNA(y)||any(!y%in%c(0,1)))stop("分折输入存在重复患者或缺失结局。")
    if(any(table(factor(y,levels=0:1))<k))stop("事件/非事件不足以分折。")
    oldseed<-if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))get(".Random.seed",.GlobalEnv) else NULL
    on.exit(if(is.null(oldseed)){if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv)}else assign(".Random.seed",oldseed,.GlobalEnv))
    set.seed(seed);fold<-integer(length(y))
    for(v in 0:1){i<-which(y==v);i<-i[order(patient_key[i])];i<-i[sample.int(length(i))];fold[i]<-rep(seq_len(k),length.out=length(i))}
    fold
  }
  nested_folds<-function(patient_key,y,k=5L,seed=2026L){
    outer<-make_folds(patient_key,y,k,seed);inner<-list()
    for(o in seq_len(k)){tr<-which(outer!=o);inner[[as.character(o)]]<-data.frame(patient_key=patient_key[tr],
      inner_fold=make_folds(patient_key[tr],y[tr],k,seed+100L*o),stringsAsFactors=FALSE)
      if(any(inner[[as.character(o)]]$patient_key%in%patient_key[outer==o]))stop("内外折患者重叠。")}
    list(outer=data.frame(patient_key,outer_fold=outer,stringsAsFactors=FALSE),inner=inner,k=k,seed=seed)
  }
  preflight<-function(){
    sp<-engine$specification();sp<-sp[c(1,3),];sp$domain<-"fixture";sp$component_id<-paste(sp$variable,sp$stat,sep="__")
    raw<-data.frame(a=1:8);names(raw)<-engine$key(sp$variable[1],"0-6h",sp$stat[1])
    raw[[engine$key(sp$variable[2],"0-6h",sp$stat[2])]]<-c(1,2,3,4,NA,6,7,8)
    for(i in 1:2)raw[[engine$key(sp$variable[i],"0-24h",sp$stat[i])]]<-raw[[engine$key(sp$variable[i],"0-6h",sp$stat[i])]]
    ref<-engine$fit_reference(raw,1:4,sp,min_observed=2)
    z<-engine$apply_reference(raw,ref,c("0-6h","0-24h"))
    stopifnot(all.equal(z$component_scores[[1]][1:4],c(12.5,37.5,62.5,87.5)),
      all.equal(z$component_scores[[2]][1:4],c(87.5,62.5,37.5,12.5)),
      identical(z$scores[[1]],z$scores[[2]]))
    changed<-raw;changed[5:8,]<-10000
    stopifnot(identical(ref,engine$fit_reference(changed,1:4,sp,min_observed=2)))
    future<-raw;future[,3:4]<-90000
    stopifnot(identical(ref,engine$fit_reference(future,1:4,sp,min_observed=2)))
    one<-engine$apply_reference(raw[6,,drop=FALSE],ref,c("0-6h","0-24h"))
    stopifnot(isTRUE(all.equal(as.numeric(one$scores[1,]),as.numeric(z$scores[6,]))))
    absent<-raw[1,,drop=FALSE];absent[,]<-NA_real_
    stopifnot(all(is.na(engine$apply_reference(absent,ref,"0-6h")$scores)))
    tie<-ref$reference[[1]];tie$values<-c(1,2);tie$count<-c(3,1);tie$cumulative<-c(3,4);tie$n<-4
    stopifnot(identical(engine$percentile(c(0,1,1.5,2,3,NA),tie),c(0,37.5,75,87.5,100,NA)))
    x<-data.frame(a=c(1,2,NA,4,999),b=c(4,3,2,1,-999));p<-engine$fit_preprocessor(x,1:4,min_observed=2)
    x2<-x;x2[5,]<-0;stopifnot(identical(p,engine$fit_preprocessor(x2,1:4,min_observed=2)))
    stopifnot(isTRUE(all.equal(engine$apply_preprocessor(x,p)[5,,drop=FALSE],engine$apply_preprocessor(x[5,,drop=FALSE],p),check.attributes=FALSE)))
    ids<-sprintf("p%03d",1:100);y<-rep(0:1,each=50);f<-nested_folds(ids,y)
    stopifnot(identical(f,nested_folds(ids,y)),all(table(f$outer$outer_fold)==20))
    revfold<-make_folds(rev(ids),rev(y));stopifnot(identical(rev(revfold),f$outer$outer_fold))
    TRUE
  }
  domain_labels<-c(hemodynamic="Hemodynamic",respiratory_neurologic="Respiratory / neurologic",
    renal_metabolic="Renal / metabolic",inflammatory_coagulopathic="Inflammatory / coagulopathic",
    cardiac_hypoperfusion="Cardiac / hypoperfusion*")
  clinical_vars<-c("vasoactive","pci","cabg","iabp","impella","ecmo","diagnostic_cath","prior_cad",
    "heart_failure","diabetes","hypertension","chronic_kidney_disease","copd","stroke","atrial_fibrillation")
  require_cols<-function(x,nms,label){
    if(anyDuplicated(names(x)))stop(label,"存在重复列名。")
    if(length(setdiff(nms,names(x))))stop(label,"缺少字段：",paste(setdiff(nms,names(x)),collapse=", "))
  }
  numeric_checked<-function(x,label){
    if(inherits(x,"integer64"))x<-as.numeric(x)
    if(!is.numeric(x)||any(is.infinite(x)|is.nan(x)))stop(label,"含非数值或Inf/NaN；不自动置零。")
    as.numeric(x)
  }
  strict_bit<-function(x,label,allow_na=FALSE){
    z<-suppressWarnings(as.numeric(as.character(x)))
    if(any(!is.na(x)&is.na(z))||any(!is.na(z)&!z%in%0:1)||(!allow_na&&anyNA(z)))stop(label,"不是完整0/1字段。")
    z
  }
  feature_plan<-function(lm){
    sp<-engine$specification();core<-unique(sp$variable[sp$tier=="main"]);w<-engine$windows_for(lm)
    rows<-list();add<-function(nm,role,window="",tier="main")rows[[length(rows)+1L]]<<-data.frame(feature=nm,role=role,window=window,tier=tier)
    for(ww in w){
      typ<-if(ww%in%engine$windows_for(lm,"cumulative"))"main_continuous" else "nonoverlap_continuous_sensitivity"
      for(v in core)for(st in c("mean_value","sd_value","slope_per_hour","abnormal_fraction"))add(engine$key(v,ww,st),typ,ww)
      for(i in seq_len(nrow(sp)))add(engine$key(sp$variable[i],ww,sp$stat[i]),"score_component_input",ww,sp$tier[i])
      for(v in unique(c(core,"lactate","troponin_t","gcs_total")))for(st in c("n_chart_times","missing_flag"))
        add(engine$key(v,ww,st),"observation_process",ww,if(v%in%core)"main" else "supplemental")
      for(v in c("lactate","gcs_total"))add(engine$key(v,ww,"last_value"),"simple_clinical_comparator",ww,"supplemental")
      add(engine$key("troponin_t",ww,"last_value"),"exploratory_biomarker",ww,"exploratory")
    }
    unique(do.call(rbind,rows))
  }
  make_bundle<-function(raw,clinical,roster,src,lm){
    ids<-if(src=="MIMIC")"stay_id" else "patientunitstayid"
    person<-if(src=="MIMIC")"subject_id" else "uniquepid"
    endpoint<-if(src=="MIMIC")"death_30d_after_landmark" else "hospital_death_after_landmark"
    require_cols(raw,c("source","stay_id","landmark","cutoff_hour","in_index_icu"),"Step4宽表")
    require_cols(clinical,c("source","stay_id","landmark","age","sex","hospital_id","age_topcoded",
      as.vector(outer(clinical_vars,c("documented_positive","patient_source_covered"),paste,sep="__"))),"Step5宽表")
    require_cols(roster,c(ids,person,"landmark","analysis_eligible","in_index_icu_at_landmark",endpoint),"冻结名单")
    rr<-roster[as.character(roster$landmark)==lm&strict_bit(roster$analysis_eligible,"analysis_eligible",TRUE)%in%1,,drop=FALSE]
    key<-as.character(raw$stay_id);ck<-as.character(clinical$stay_id);rk<-as.character(rr[[ids]])
    for(k in list(key,ck,rk))if(anyNA(k)||any(!nzchar(k))||anyDuplicated(k))stop("患者连接键缺失或重复。")
    if(!setequal(key,ck)||!setequal(key,rk))stop("Step4/5与冻结名单患者集合不一致。")
    if(anyNA(raw$source)||any(raw$source!=src)||anyNA(raw$landmark)||any(raw$landmark!=lm)||
      anyNA(clinical$source)||any(clinical$source!=src)||anyNA(clinical$landmark)||any(clinical$landmark!=lm))stop("来源/landmark不一致。")
    ord<-order(key);raw<-raw[ord,,drop=FALSE];key<-key[ord]
    clinical<-clinical[match(key,ck),,drop=FALSE];rr<-rr[match(key,rk),,drop=FALSE]
    pkey<-paste(src,as.character(rr[[person]]),sep=":")
    if(anyNA(rr[[person]])||any(!nzchar(trimws(as.character(rr[[person]]))))||anyDuplicated(pkey))stop("同一队列的患者去重校验失败。")
    icu<-strict_bit(raw$in_index_icu,"in_index_icu")
    if(any(icu!=strict_bit(rr$in_index_icu_at_landmark,"roster ICU"))||anyNA(raw$cutoff_hour)||any(raw$cutoff_hour!=as.numeric(sub("T","",lm))))stop("landmark时点/ICU状态不一致。")
    fp<-feature_plan(lm);cols<-unique(fp$feature);require_cols(raw,cols,"Step4评分及建模输入")
    x<-raw[cols];for(nm in names(x))x[[nm]]<-numeric_checked(x[[nm]],nm)
    if(any(vapply(x[grepl("__abnormal_fraction$",names(x))],function(v)any(v<0|v>1,na.rm=TRUE),logical(1))))stop("异常比例超出0至1。")
    sp<-engine$specification();core<-unique(sp$variable[sp$tier=="main"])
    for(w in engine$windows_for(lm))for(v in unique(c(core,"lactate","troponin_t","gcs_total"))){
      n<-x[[engine$key(v,w,"n_chart_times")]];miss<-x[[engine$key(v,w,"missing_flag")]]
      if(anyNA(n)||any(n<0|n!=floor(n))||anyNA(miss)||any(miss!=as.integer(n==0)))stop("观测次数/缺失标记不一致：",v," / ",w)
      cn<-names(x)[startsWith(names(x),paste0(v,"__",gsub("-","_",w,fixed=TRUE),"__"))]
      cn<-cn[!grepl("__(n_chart_times|missing_flag)$",cn)]
      for(nm in cn)if(any(n==0&!is.na(x[[nm]])))stop("无观测却有数值：",nm)
      for(st in c("sd_value","slope_per_hour")){
        nm<-engine$key(v,w,st);if(nm%in%names(x)&&any(n<2&!is.na(x[[nm]])))stop("不足两次观测却有SD/斜率：",nm)
      }
    }
    if(any(!is.na(clinical$sex)&!clinical$sex%in%c("Male","Female")))stop("未知性别编码。")
    baseline<-data.frame(age=numeric_checked(clinical$age,"age"),sex_male=ifelse(is.na(clinical$sex),NA_real_,as.numeric(clinical$sex=="Male")),in_index_icu=icu)
    if(any(!is.na(baseline$age)&(baseline$age<18|baseline$age>120)))stop("年龄超出冻结范围。")
    clinic_cols<-as.vector(outer(clinical_vars,c("documented_positive","patient_source_covered"),paste,sep="__"))
    cx<-clinical[clinic_cols]
    for(v in clinical_vars){
      pos<-paste0(v,"__documented_positive");cov<-paste0(v,"__patient_source_covered")
      cx[[pos]]<-strict_bit(cx[[pos]],pos,TRUE);cx[[cov]]<-strict_bit(cx[[cov]],cov)
      if(any(cx[[cov]]==0&!is.na(cx[[pos]]))||any(cx[[cov]]==1&is.na(cx[[pos]])))stop("临床阳性与来源可用性不一致：",v)
    }
    y<-strict_bit(rr[[endpoint]],endpoint)
    outcomes<-data.frame(event_primary=y,endpoint=if(src=="MIMIC")"30_day_all_cause_death_after_landmark" else "in_hospital_death_after_T48",stringsAsFactors=FALSE)
    if(src=="MIMIC"){
      secondary<-c("death_in_index_hospital_after_landmark","analysis_eligible_hospital_endpoint",
        "death_precision","endpoint_boundary_ambiguous",
        "death_30d_lower_bound","death_30d_upper_bound","death_30d_midday_sensitivity",
        "followup_time_days_midpoint_if_date_only","event_time_lower_days","event_time_upper_days",
        "event_time_is_approximate","strict_sensitivity_eligible")
      require_cols(rr,secondary,"MIMIC结局精度字段")
      outcomes<-cbind(outcomes,rr[secondary])
    }else{
      require_cols(rr,c("hospitaldischargeoffset","landmark_hour"),"eICU随访")
      follow<-(as.numeric(rr$hospitaldischargeoffset)-as.numeric(rr$landmark_hour)*60)/1440
      if(anyNA(follow)||any(!is.finite(follow)|follow<=0))stop("eICU住院随访时长无效。")
      outcomes$hospital_followup_days_after_landmark<-follow
    }
    metadata<-data.frame(patient_key=pkey,source=src,stay_id=key,landmark=lm,
      hospital_id=as.character(clinical$hospital_id),age_topcoded=clinical$age_topcoded,stringsAsFactors=FALSE)
    for(z in c("x","baseline","cx","outcomes","metadata")){tmp<-get(z);rownames(tmp)<-NULL;assign(z,tmp)}
    list(version="Step6_v1",metadata=metadata,raw_numeric=x,baseline=baseline,clinical_documentation=cx,
      outcomes=outcomes,feature_plan=fp,score_specification=sp,
      note="Unfitted inputs: reference fitting, imputation, scaling and clustering must occur inside each training split.")
  }
  availability<-function(bundle){
    x<-bundle$raw_numeric;sp<-bundle$score_specification;src<-bundle$metadata$source[1];lm<-bundle$metadata$landmark[1];ans<-list();comp<-list()
    for(w in engine$windows_for(lm))for(d in unique(sp$domain)){
      s<-sp[sp$domain==d,,drop=FALSE];cols<-engine$key(s$variable,w,s$stat);m<-!is.na(x[cols]);n<-rowSums(m);req<-ceiling(nrow(s)/2)
      ans[[length(ans)+1L]]<-data.frame(source=src,landmark=lm,window=w,domain=d,tier=s$tier[1],
        specified_components=nrow(s),minimum_components=req,cohort_n=nrow(x),raw_available_n=sum(n>=req),
        raw_available_pct=100*mean(n>=req),all_components_n=sum(n==nrow(s)),median_components=stats::median(n))
      for(i in seq_len(nrow(s)))comp[[length(comp)+1L]]<-data.frame(source=src,landmark=lm,window=w,
        domain=d,component_id=s$component_id[i],cohort_n=nrow(x),observed_n=sum(m[,i]))
    }
    list(domain=do.call(rbind,ans),component=do.call(rbind,comp))
  }
  fold_summary<-function(f,y,src,lm){
    ans<-list();o<-f$outer$outer_fold
    for(k in 1:f$k){ans[[length(ans)+1L]]<-data.frame(source=src,landmark=lm,level="outer",outer_fold=k,inner_fold=NA_integer_,
      training_n=sum(o!=k),training_events=sum(y[o!=k]),assessment_n=sum(o==k),assessment_events=sum(y[o==k]))
      ii<-f$inner[[as.character(k)]];jj<-match(ii$patient_key,f$outer$patient_key)
      for(j in 1:f$k)ans[[length(ans)+1L]]<-data.frame(source=src,landmark=lm,level="inner",outer_fold=k,inner_fold=j,
        training_n=sum(ii$inner_fold!=j),training_events=sum(y[jj[ii$inner_fold!=j]]),
        assessment_n=sum(ii$inner_fold==j),assessment_events=sum(y[jj[ii$inner_fold==j]]))
    };do.call(rbind,ans)
  }
  make_visuals<-function(a){
    open_pdf<-function(path,width,height){if(isTRUE(capabilities("cairo")))grDevices::cairo_pdf(path,width=width,height=height,family="Arial")
      else grDevices::pdf(path,width=width,height=height,family="Helvetica",useDingbats=FALSE)}
    aa<-a[a$landmark=="T48",,drop=FALSE];core<-names(domain_labels)[1:4]
    palettes<-list(MIMIC=grDevices::colorRampPalette(c("#F3F6FC","#4F7FC4","#214B84"))(101),
      eICU=grDevices::colorRampPalette(c("#FCF6F1","#D98C57","#8C461B"))(101))
    ntext<-function(src)format(unique(aa$cohort_n[aa$source==src]),big.mark=",",trim=TRUE)
    draw<-function(){
      grid::grid.newpage();gt<-function(label,x,y,size=9,just="centre",...)grid::grid.text(label,x=x,y=y,just=just,gp=grid::gpar(fontsize=size,...))
      gt("Availability of organ-domain components",.02,.97,15,"left",fontface="bold")
      gt("Patients with at least half of the prespecified components observed; fixed T48 cohorts",.02,.931,10,"left",col="#555555")
      for(r in 1:2){w<-if(r==1)c("0-6h","0-24h","0-48h")else c("0-6h","6-24h","24-48h");top<-if(r==1).805 else .405;dy<-.057
        labels<-c("Hemodynamic","Respiratory /\nneurologic","Renal /\nmetabolic","Inflammatory /\ncoagulopathic")
        for(i in 1:4)gt(labels[i],.02,top-(i-.5)*dy,10,"left")
        for(s in 1:2){src<-c("MIMIC","eICU")[s];left<-c(.29,.65)[s];dx<-.10
          gt(paste0(LETTERS[(r-1)*2+s],"  ",src," | ",if(r==1)"cumulative"else"non-overlapping"),left,top+.069,11,"left",fontface="bold")
          gt(paste0("n = ",ntext(src)),left,top+.037,9,"left",col="#555555")
          for(j in 1:3)for(i in 1:4){z<-aa[aa$source==src&aa$domain==core[i]&aa$window==w[j],,drop=FALSE]
            if(nrow(z)!=1||!is.finite(z$raw_available_pct))stop("可用性绘图数据不完整。")
            pct<-z$raw_available_pct
            grid::grid.rect(x=left+(j-.5)*dx,y=top-(i-.5)*dy,width=dx,height=dy,
              gp=grid::gpar(fill=palettes[[src]][round(pct)+1],col="white",lwd=2))
            gt(sprintf("%.1f%%",pct),left+(j-.5)*dx,top-(i-.5)*dy,10,col=if(pct>65)"white"else"#1F2937")
          }
          for(j in 1:3)gt(sub("h"," h",w[j],fixed=TRUE),left+(j-.5)*dx,top-4*dy-.021,9)
        }
      }
      for(s in 1:2){src<-c("MIMIC","eICU")[s];left<-c(.29,.65)[s];ww<-.30
        for(j in 0:100)grid::grid.rect(left+(j+.5)*ww/101,.097,width=ww/101+.0001,height=.014,gp=grid::gpar(fill=palettes[[src]][j+1],col=NA))
        for(j in c(0,50,100))gt(paste0(j,"%"),left+j*ww/100,.073,8)
      }
      gt("Raw availability precedes training-fold reference checks; this is not a score or performance comparison.",.02,.022,8,"left",col="#555555")
    }
    open_pdf(reg("to_share/Figure_S_organ_domain_availability.pdf"),10,7.4);tryCatch(draw(),finally=grDevices::dev.off())
    if(isTRUE(cfg$MAKE_PNG)){grDevices::png(reg("to_share/Figure_S_organ_domain_availability.png"),width=10,height=7.4,units="in",res=400);tryCatch(draw(),finally=grDevices::dev.off())}
    tables<-list()
    for(typ in c("cumulative","nonoverlap")){
      w<-engine$windows_for("T48",typ);tab<-list()
      for(d in names(domain_labels))for(ww in w){m<-aa[aa$source=="MIMIC"&aa$domain==d&aa$window==ww,,drop=FALSE];e<-aa[aa$source=="eICU"&aa$domain==d&aa$window==ww,,drop=FALSE]
        if(nrow(m)!=1||nrow(e)!=1)stop("可用性三线表数据不完整。")
        tab[[length(tab)+1L]]<-data.frame(Domain=unname(domain_labels[d]),Minimum=paste0(m$minimum_components," / ",m$specified_components),
          Window=sub("h"," h",ww,fixed=TRUE),MIMIC=sprintf("%s (%.1f%%)",format(m$raw_available_n,big.mark=",",trim=TRUE),m$raw_available_pct),
          eICU=sprintf("%s (%.1f%%)",format(e$raw_available_n,big.mark=",",trim=TRUE),e$raw_available_pct))
      };tables[[typ]]<-do.call(rbind,tab)
    }
    csv_out(do.call(rbind,lapply(names(tables),function(t)cbind(window_type=t,tables[[t]]))),"to_share/Table_S_domain_component_availability.csv")
    open_pdf(reg("to_share/Table_S_domain_component_availability.pdf"),11.69,8.27)
    tryCatch({for(typ in names(tables)){
      tab<-tables[[typ]];grid::grid.newpage();gt<-function(label,x,y,size=9,just="left",...)grid::grid.text(label,x=x,y=y,just=just,gp=grid::gpar(fontsize=size,...))
      gt(paste0("Supplementary table. Domain component availability: ",if(typ=="cumulative")"cumulative windows"else"non-overlapping windows"),.04,.95,13,fontface="bold")
      gt(paste0("Fixed T48 cohorts: MIMIC-IV n = ",ntext("MIMIC"),"; eICU n = ",ntext("eICU"),". Values are n (% of full cohort)."),.04,.91,9)
      xs<-c(.04,.49,.64,.80,.96);top<-.87;dy<-.037
      line<-function(y,lwd)grid::grid.lines(x=c(.04,.97),y=c(y,y),gp=grid::gpar(lwd=lwd))
      line(top,1.3)
      for(j in 1:5)gt(names(tab)[j],xs[j],top-.022,9,if(j==1)"left"else"right",fontface="bold")
      line(top-.045,.8)
      for(i in 1:nrow(tab))for(j in 1:5)gt(as.character(tab[i,j]),xs[j],top-.035-i*dy,8.5,if(j==1)"left"else"right")
      bot<-top-.055-nrow(tab)*dy;line(bot,1.3)
      gt("Minimum: observed / specified components needed for raw availability (at least half; no imputation).",.04,bot-.03,8)
      gt("* Exploratory cardiac / hypoperfusion domain: lactate maximum, lactate abnormal fraction and troponin T maximum.",.04,bot-.055,8)
      gt("Training-fold reference availability may further reduce score coverage. No fitted scores are reported in this table.",.04,bot-.08,8)
    }},finally=grDevices::dev.off())
    esc<-function(x){x<-gsub("\\","\\\\",x,fixed=TRUE);x<-gsub("{","\\{",x,fixed=TRUE);gsub("}","\\}",x,fixed=TRUE)}
    rtf<-c("{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 Arial;}}\\landscape\\paperw16838\\paperh11906\\margl600\\margr600\\f0\\fs20")
    for(typ in names(tables)){
      if(typ=="nonoverlap")rtf<-c(rtf,"\\page")
      rtf<-c(rtf,paste0("\\pard\\b Supplementary table. Domain component availability: ",typ,"\\b0\\par Fixed T48 cohorts: MIMIC-IV n=",ntext("MIMIC"),"; eICU n=",ntext("eICU"),". Values are n (%).\\par\\par"))
      rows<-rbind(names(tables[[typ]]),as.matrix(tables[[typ]]));ends<-c(6500,8200,10000,12550,15100)
      for(i in 1:nrow(rows)){bd<-paste0(if(i==1)"\\clbrdrt\\brdrs\\brdrw15\\clbrdrb\\brdrs\\brdrw10"else"",if(i==nrow(rows))"\\clbrdrb\\brdrs\\brdrw15"else"")
        rtf<-c(rtf,paste0("\\trowd\\trgaph80\\trkeep",paste0(bd,"\\cellx",ends,collapse=""),
          paste(vapply(1:5,function(j)paste0("\\pard\\intbl",if(j==1)"\\ql "else"\\qr ","\\fs17 ",if(i==1)"\\b "else"\\b0 ",esc(rows[i,j]),"\\cell"),character(1)),collapse=""),"\\row"))
      }
      rtf<-c(rtf,"\\pard\\fs16 Minimum: observed / specified components; at least half, without imputation.\\par * Exploratory domain: lactate maximum, lactate abnormal fraction and troponin T maximum.\\par Raw availability may exceed final training-reference-based score coverage. No fitted scores are reported.\\par")
    };txt_out(c(rtf,"}"),"to_share/Table_S_domain_component_availability.rtf")
    csv_out(data.frame(output=c("Figure_S_organ_domain_availability","Table_S_domain_component_availability"),
      action="ADD",placement=c("New supplementary figure: raw component availability; final S number assigned on integration",
        "New supplementary three-line table: raw component availability; final S number assigned on integration"),
      relation_to_existing="Does not replace existing main Figure 2 trajectory panels or Step5 clinical-documentation figure/table"),"to_share/11_figure_table_replacement_map.csv")
  }
  if(isTRUE(getOption("ami_step6.functions_only",FALSE)))return(invisible(list(engine=engine,preflight=preflight,
    make_folds=make_folds,nested_folds=nested_folds,feature_plan=feature_plan,make_bundle=make_bundle,
    availability=availability,fold_summary=fold_summary,make_visuals=make_visuals)))
  status<-"FAILED";current_module<-"input verification"
  check_manifest<-function(p){
    z<-utils::read.csv(p,stringsAsFactors=FALSE,check.names=FALSE);require_cols(z,c("file","md5"),basename(p))
    for(i in seq_len(nrow(z))){
      if(!file.exists(z$file[i])||is.na(z$md5[i])||unname(tools::md5sum(z$file[i]))!=z$md5[i])stop("上游已变化，需先重跑对应步骤：",z$file[i])
    }
  }
  read_input_rds<-function(p){
    if(!file.exists(p))stop("缺少输入：",p)
    x<-readRDS(p);if(!is.data.frame(x))stop("RDS不是患者宽表：",p)
    emit("01_inputs",file=p,n_rows=nrow(x),n_columns=ncol(x),md5=unname(tools::md5sum(p)));x
  }
  main<-function(){
    logmsg("小型预检：训练折评分参考、同值同分、单例/批量等价、缺失处理及嵌套分折隔离。")
    stopifnot(preflight())
    txt_out("PASS: fixed-reference percentiles; ties; direction; held-out invariance; single/batch equality; missingness; deterministic patient-level nested folds.","to_share/02_preflight.txt")
    step4<-file.path(cfg$OUTPUT_PARENT,"Step4");step5<-file.path(cfg$OUTPUT_PARENT,"Step5")
    for(s in c(4,5)){
      p<-file.path(cfg$OUTPUT_PARENT,paste0("Step",s),"to_share",paste0("Step",s,"_STATUS.txt"))
      if(!file.exists(p)||!"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(p,warn=FALSE))stop("先成功完成Step",s,"。")
    }
    # Step5-2核对修正后的冠脉介入映射，不接受已知PCI/造影漏识别的旧版本。
    if(!any(grepl("SCRIPT_VERSION = Step5-2",readLines(file.path(step5,"to_share/Step5_STATUS.txt"),warn=FALSE),fixed=TRUE)))stop("需要已核对通过的Step5-2结果。")
    check_manifest(file.path(step5,"to_share/01_inputs.csv"));check_manifest(file.path(step4,"to_share/01_inputs.csv"))
    if(!requireNamespace("DBI",quietly=TRUE)||!requireNamespace("duckdb",quietly=TRUE))stop("需要DBI和duckdb；本步不自动安装包。")
    dbpath<-file.path(step4,"local_only/Step4_harmonized_features.duckdb")
    if(!file.exists(dbpath))stop("缺少Step4冻结名单数据库。")
    con<-DBI::dbConnect(duckdb::duckdb(),dbdir=dbpath,read_only=TRUE)
    on.exit(DBI::dbDisconnect(con,shutdown=TRUE),add=TRUE)
    DBI::dbExecute(con,"SET threads=1");DBI::dbExecute(con,"SET memory_limit='1GB'")
    mr<-DBI::dbGetQuery(con,"SELECT * FROM MIMIC_frozen_roster")
    er<-DBI::dbGetQuery(con,"SELECT * FROM eICU_frozen_roster")
    sp<-engine$specification();sp$minimum_reference_observations<-20L
    sp$minimum_domain_fraction<-.5;sp$aggregation<-"Equal-weight mean of observed enabled component scores"
    csv_out(sp,"to_share/03_score_specification.csv")
    rules_path<-file.path(step4,"to_share/03_common_component_rules.csv")
    if(!file.exists(rules_path))stop("缺少Step4单位/阈值规则。")
    rules<-utils::read.csv(rules_path,check.names=FALSE,stringsAsFactors=FALSE);require_cols(rules,"variable","Step4 rules")
    if(!all(unique(sp$variable)%in%rules$variable))stop("评分组件与Step4字典不匹配。")
    csv_out(rules[rules$variable%in%sp$variable,,drop=FALSE],"to_share/03_component_units_and_thresholds.csv")
    expected<-data.frame(source=c("MIMIC","MIMIC","MIMIC","eICU"),landmark=c("T6","T24","T48","T48"),
      n=c(5513,5411,5133,5284),events=c(825,738,630,219))
    allav<-list();allcomp<-list();folds<-list();allplans<-list()
    logmsg("读取Step4数值宽表和Step5临床记录；核对冻结患者、结局、窗口及缺失含义，不扫描原始数据库。")
    for(i in seq_len(nrow(expected))){
      src<-expected$source[i];lm<-expected$landmark[i];tag<-paste(src,lm,sep="_");current_module<<-paste("bundle",tag)
      raw<-read_input_rds(file.path(step4,"local_only",paste0(tag,"_numeric_features.rds")))
      clinical<-read_input_rds(file.path(step5,"local_only",paste0(tag,"_clinical_context.rds")))
      b<-make_bundle(raw,clinical,if(src=="MIMIC")mr else er,src,lm)
      n<-nrow(b$metadata);events<-sum(b$outcomes$event_primary)
      if(n!=expected$n[i]||events!=expected$events[i])stop(tag,"人数或事件未复现审核通过的冻结结果。")
      emit("02_cohort_checks",source=src,landmark=lm,n=n,events=events,in_index_icu=sum(b$baseline$in_index_icu),
        raw_numeric_columns=ncol(b$raw_numeric),baseline_columns=ncol(b$baseline),clinical_columns=ncol(b$clinical_documentation),
        fitted_score_references=0,endpoint=unique(b$outcomes$endpoint))
      for(nm in names(b$baseline))emit("05_baseline_availability",source=src,landmark=lm,variable=nm,n=n,observed_n=sum(!is.na(b$baseline[[nm]])))
      a<-availability(b);allav[[tag]]<-a$domain;allcomp[[tag]]<-a$component
      if(src=="MIMIC"){
        f<-nested_folds(b$metadata$patient_key,b$outcomes$event_primary,k=5L,seed=2026L)
        folds[[tag]]<-f;b$fold_key<-tag
        agg[["06_fold_counts"]]<-c(agg[["06_fold_counts"]],list(fold_summary(f,b$outcomes$event_primary,src,lm)))
      }else b$fold_key<-NA_character_
      allplans[[tag]]<-cbind(source=src,landmark=lm,b$feature_plan)
      saveRDS(b,reg(paste0("local_only/",tag,"_model_inputs.rds")),compress="gzip")
      logmsg(paste0(tag,"：",n,"人，",events,"个事件；输入及患者连接核对通过。"))
      rm(raw,clinical,b);gc(verbose=FALSE)
    }
    saveRDS(folds,reg("local_only/MIMIC_nested_folds.rds"),compress="gzip")
    av<-do.call(rbind,allav);csv_out(av,"to_share/04_domain_raw_availability.csv")
    csv_out(do.call(rbind,allcomp),"to_share/04_component_raw_availability.csv")
    csv_out(do.call(rbind,allplans),"to_share/05_raw_feature_roles.csv")
    factory_text<-c("# Shared score engine. Parameters must be fit within the current training split.",
      paste0("score_engine_factory <- ",deparse(score_engine_factory,width.cutoff=120L)[1]),
      deparse(score_engine_factory,width.cutoff=120L)[-1],"step6_score_engine <- score_engine_factory()")
    txt_out(factory_text,"local_only/Step6_score_engine.R")
    contract<-list(version="Step6_v1",main_landmark="T48",early_landmarks=c("T6","T24"),
      main_domains=names(domain_labels)[1:4],exploratory_domain="cardiac_hypoperfusion",
      reference_window="0-6h",reference_source="Current MIMIC training split only",reference_min_n=20L,
      percentile="100 * (N_training_less_than_x + 0.5*N_training_equal_x) / N_observed_training; reverse for low-worse",
      minimum_domain_fraction=.5,domain_fraction_sensitivity=.75,component_weights="equal",
      main_windows="cumulative through landmark",sensitivity_windows="nonoverlapping through landmark",
      clustering_features="Four main domain score levels across available windows; no redundant delta/slope",
      clustering_selection="Training-only; no outcome in choice of k, fitting or labels; final rules specified before Step7 fitting",
      preprocessing=list(max_missing=.95,min_observed=20L,drop_constant=TRUE,impute="training median",scale="training mean and SD",missing_indicators="training-missing retained features only"),
      baseline=c("age","sex_male","in_index_icu"),model_arms=c(A="baseline",B="baseline + phenotype",C="baseline + continuous dynamics",D="baseline + continuous dynamics + phenotype"),
      continuous_dynamics="21 core variables x mean, SD, slope per hour and abnormal fraction plus four domain scores over cumulative windows",
      clinical_documentation_role="Supplemental candidates only: documentation is not verified absence; do not encode NA as zero",
      observation_model="Observation counts and missing flags; compare separately",
      simple_clinical_model="Age, ICU state, vasoactive documentation, lactate and GCS total last observed values within 0-landmark; not validated named severity score",
      nested_cv=list(outer=5L,inner=5L,stratify="MIMIC primary event",seed=2026L,unit="unique patient",shared_across_arms=TRUE),
      fit_order=c("fit reference on inner training; apply to inner assessment; train-only impute/scale/cluster/model",
        "after tuning, refit all stages on outer training; evaluate outer assessment once",
        "after internal evaluation, refit fixed pipeline on all eligible MIMIC; freeze for eICU"),
      external_endpoint="eICU post-T48 hospital death; not 30-day death validation",external_model_selection=FALSE,
      status="SPECIFICATION_AND_UNFITTED_INPUTS_ONLY")
    saveRDS(contract,reg("local_only/Step6_analysis_contract.rds"),compress="gzip")
    csv_out(data.frame(model=names(contract$model_arms),definition=unname(contract$model_arms),same_folds=TRUE,
      fit_all_preprocessing_inside_training=TRUE),"to_share/07_model_arm_contract.csv")
    txt_out(capture.output(dput(contract)),"to_share/07_analysis_contract.txt")
    current_module<<-"availability figure and three-line tables"
    logmsg("输出器官域原始组件可用性图与三线表；本步不在全队列拟合评分参考、分型或风险模型。")
    make_visuals(av)
    txt_out(c("本步完成统一评分规则、未拟合建模输入和MIMIC患者级外5折/内5折分组。",
      "评分为相对于训练参考的经验百分位，不是已验证的临床严重度刻度；高分表示相对较差。",
      "同一组件所有窗口均使用相应训练患者0-6h参考。并列值使用中分布百分位；越界值映射为0/100。",
      "训练参考至少20个非缺失患者。参考不足的组件保持NA；器官域分母仍按原规定组件数，不为外部缺项缩小。",
      "至少一半规定组件可评分才输出该域均值；全缺失不置0。第五域仅探索，肌钙蛋白仅T，不混I，不构造未验证异常比例。",
      "四域分别6、8、8、8个组件；探索域3个组件。主连续特征来自21个核心变量的均值、SD、异常比例、斜率及四域评分。",
      "当前图表只显示原始组件至少半数可观测；训练参考不足还可能进一步降低真正评分可用率。",
      "累计窗口均值/SD/斜率仍来自Step4真实观测，不用多个窗口均值再次汇总。累计最大/最小和累积窗口分数不直接解释为即时恢复。",
      "时间窗口、单位、异常阈值沿用审核通过的Step4。填补、缩放、特征筛选、评分参考、分型必须在每一训练折重拟合。",
      "内折必须重新拟合评分与所有预处理，不能复用外训练全集的已拟合参考；外部仅应用最终MIMIC参数。",
      "分折按唯一患者及主要事件分层；三landmark各自建模，不能把多个landmark堆叠后把同一患者分到两侧。",
      "MIMIC主终点是landmark后30天死亡；eICU是T48后院内死亡。后续同终点模型另用保留的MIMIC院内结局。",
      "日期级死亡的区间、边界标签及中点随访均单独保留。中点只是近似，不能声称精确死亡时间。",
      "性别Male=1、Female=0、未知=NA；eICU年龄>89保留Step5的90代理及topcoded标记。",
      "病史及治疗代理只存为补充候选，不混入主四臂默认特征；缺乏记录不是证实没有治疗/疾病。",
      "简易临床模型的last_value来自0至landmark内最近记录，可能较早；不是GRACE/SOFA等原版评分。",
      "旧测试集参与开发的历史仍需披露；重新分折不产生完全未见过的独立队列。",
      "新增补充图和三线表，不替代旧主Figure2轨迹图；正式S编号待整合。Step5临床记录图表保持原位。",
      "固定Step6目录及同名输出；重跑只清理本步骤已登记的文件。患者输入、ID和分折仅保存local_only。",
      "请回传Step6_feedback.zip。待审核后运行训练折内分型与统一预测管线。"),"to_share/00_READ_ME_FIRST.txt")
    txt_out(c("https://www.tidymodels.org/learn/work/nested-resampling/index.html",
      "https://recipes.tidymodels.org/reference/prep.html"),"to_share/10_method_sources.txt")
    status<<-"COMPLETED_FOR_REVIEW"
  }
  failure_message<-NULL
  tryCatch(main(),error=function(err){status<<-"FAILED";failure_message<<-conditionMessage(err)
    issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step6_error_detail.txt")})
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step6-1",
    "COMPLETED_FOR_REVIEW表示评分规则、未拟合输入及分折已准备，尚未训练分型或风险模型。",
    "FAILED表示不完整，不能使用本次患者级结果建模。"),"to_share/Step6_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step6_sessionInfo.txt")
  logmsg(paste0("Step6状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step6_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share) else {
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step6_feedback.zip"),"\n",sep="") else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
