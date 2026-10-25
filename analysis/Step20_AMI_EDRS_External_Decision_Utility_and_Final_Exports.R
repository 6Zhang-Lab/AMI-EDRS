# AMI-EDRS revision | Step20: external fixed thresholds, decision utility and frozen coefficient exports
# source(file.choose(), encoding="UTF-8")
# Fixed filenames; replaces owned results. Patient-level data and frozen references stay local_only.
local({
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MAKE_PNG=TRUE,MAKE_ZIP=TRUE,RESUME=TRUE,PDF_FAMILY="Arial",BOOTSTRAP_N=2000L,SEED=2026L)
  cfg<-utils::modifyList(cfg,getOption("ami_step20.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step20")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step20输出目录。")
  if (is_link(out)) stop("Step20输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP20")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step20目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP20_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP20_MANAGED_OUTPUT")) {
    stop("Step20输出目录的归属标记不正确。")
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
        if (unlink(p) != 0L) stop("不能覆盖旧Step20输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step20_run_log.txt")
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
    txt_out(if(nrow(z))unlist(Map(function(a,b)c(a,b,""),z$scope,z$message))else"No warnings in Step20.","local_only/Step20_warning_detail.txt")
    a<-if(nrow(z))aggregate(list(n=rep(1L,nrow(z))),list(scope=z$scope),sum)else data.frame(scope=character(),n=integer())
    csv_out(a,"to_share/09_warning_summary.csv")
  }

  plan<-list(version="Step20_v1",endpoint="in_hospital_death_after_T48",thresholds=(1:50)/100,
    display_thresholds=c(.05,.15,.30),risk_cutpoints=c(.05,.15,.30),main_display_upper=.30,
    bootstrap_n=as.integer(cfg$BOOTSTRAP_N),seed=as.integer(cfg$SEED),minimum_valid_fraction=.90,
    primary_resampling="hospital_cluster",sensitivity_resampling="patient",
    threshold_origin="Step11 display cutpoints; no external optimization or treatment-threshold validation",
    update="Step14 hospital-blocked cross-fitted intercept update; secondary; no refitting",
    uncertainty="Pointwise percentile intervals conditional on fixed predictions; not simultaneous and not full updating-process uncertainty",
    action="Hypothetical additional clinical review / monitoring assessment at T48",
    coefficient_export="Frozen full-MIMIC penalized coefficients; descriptive model parameters, not SHAP or causal effects")
  if(length(plan$bootstrap_n)!=1L||is.na(plan$bootstrap_n)||plan$bootstrap_n<20L||length(plan$seed)!=1L||is.na(plan$seed))stop("BOOTSTRAP_N至少20；正式分析默认2000。SEED必须有效。")
  arms<-c("A","B","C","D")
  model_map<-data.frame(model=c(paste0(arms,"_raw"),"APACHE_IV","APACHE_IVa",paste0(arms,"_updated")),
    label=c("A: baseline","B: baseline + phenotype","C: continuous dynamics","D: dynamics + phenotype","APACHE IV","APACHE IVa","A: updated","B: updated","C: updated","D: updated"),
    role=c(rep("frozen_hospital_endpoint",4),rep("native_admission_benchmark",2),rep("hospital_crossfitted_intercept_update",4)),stringsAsFactors=FALSE)
  labels<-setNames(model_map$label,model_map$model);labels<-c(labels,All="Assess all",None="Assess none")
  colors<-c(A_raw="#7A8797",B_raw="#729473",C_raw="#477EBB",D_raw="#3D647F",APACHE_IV="#D98936",APACHE_IVa="#8D68AD",
    A_updated="#7A8797",B_updated="#729473",C_updated="#277F79",D_updated="#5B867E",All="#555555",None="#111111")
  fixed_metrics<-c("sensitivity","specificity","PPV","NPV","alert_fraction","net_benefit")
  band_labels<-c("<5%","5 to <15%","15 to <30%",">=30%")
  need<-function(x,fields,label){if(!is.data.frame(x)||!all(fields%in%names(x)))stop(label,"缺少字段：",paste(setdiff(fields,names(x)),collapse=", "))}
  binary<-function(x){if(is.factor(x))x<-as.character(x);if(is.logical(x))x<-as.integer(x);s<-as.character(x)
    if(anyNA(s)||any(!s%in%c("0","1")))stop("结局或标志不是完整0/1；不自动填补。");as.integer(s)}
  unique_key<-function(x,label){s<-as.character(x);if(!length(s)||anyNA(s)||any(!nzchar(trimws(s)))||anyDuplicated(s))stop(label,"为空、缺失或重复。");s}
  hash_object<-function(x){p<-tempfile();on.exit(unlink(p));saveRDS(x,p,version=2);unname(tools::md5sum(p))}
  cache_result<-function(rel,key,fun){p<-safe_path(rel)
    if(isTRUE(cfg$RESUME)&&file.exists(p)){z<-tryCatch(readRDS(p),error=function(e)NULL)
      if(is.list(z)&&identical(z$key,key)&&identical(z$hash,hash_object(z$value))){reg(rel);logmsg(paste0("复用本步缓存：",basename(rel)));return(z$value)}}
    value<-fun();tmp<-reg(paste0(rel,".pending"));saveRDS(list(key=key,hash=hash_object(value),value=value),tmp,compress="gzip")
    if(!file.rename(tmp,reg(rel)))stop("Step20缓存写入失败。");value}
  check_decision<-function(y,p,t){
    if(!length(y)||anyNA(y)||any(!y%in%0:1)||!is.matrix(p)||!is.numeric(p)||nrow(p)!=length(y)||!ncol(p)||
      is.null(colnames(p))||anyNA(colnames(p))||anyDuplicated(colnames(p))||any(colnames(p)%in%c("All","None"))||any(!is.finite(p))||any(p<0|p>1))stop("无效结局或冻结概率；不静默删除患者。")
    if(!length(t)||any(!is.finite(t)|t<=0|t>=1)||is.unsorted(t,strictly=TRUE))stop("阈值必须在(0,1)且严格递增。")
    invisible(TRUE)}
  divide<-function(a,b){z<-rep(NA_real_,length(a));ok<-b>0;z[ok]<-a[ok]/b[ok];z}
  suffix_counts<-function(bins,y,K){tail_sum<-function(x)rev(cumsum(rev(x)))[seq.int(2L,K+1L)]
    list(TP=tail_sum(tabulate(bins[y==1]+1L,K+1L)),FP=tail_sum(tabulate(bins[y==0]+1L,K+1L)))}
  count_arrays<-function(y,bins,K,models){
    n<-length(y);ev<-sum(y);tp<-fp<-matrix(0,K,length(models)+2L,dimnames=list(NULL,c(models,"All","None")))
    for(j in seq_along(models)){v<-suffix_counts(bins[,j],y,K);tp[,j]<-v$TP;fp[,j]<-v$FP}
    tp[,"All"]<-ev;fp[,"All"]<-n-ev
    list(TP=tp,FP=fp,FN=ev-tp,TN=n-ev-fp,n=n,events=ev)}
  metric_arrays<-function(a,t){list(sensitivity=if(a$events>0)a$TP/a$events else a$TP*NA_real_,
    specificity=if(a$n-a$events>0)a$TN/(a$n-a$events)else a$TN*NA_real_,
    PPV=matrix(divide(as.vector(a$TP),as.vector(a$TP+a$FP)),nrow(a$TP),dimnames=dimnames(a$TP)),
    NPV=matrix(divide(as.vector(a$TN),as.vector(a$TN+a$FN)),nrow(a$TP),dimnames=dimnames(a$TP)),
    alert_fraction=(a$TP+a$FP)/a$n,net_benefit=(a$TP-a$FP*t/(1-t))/a$n)}
  band_arrays<-function(a,indices,M){pos<-rbind(a$events,a$TP[indices,seq_len(M),drop=FALSE],0)
    neg<-rbind(a$n-a$events,a$FP[indices,seq_len(M),drop=FALSE],0)
    events<-pos[1:4,,drop=FALSE]-pos[2:5,,drop=FALSE];n<-events+neg[1:4,,drop=FALSE]-neg[2:5,,drop=FALSE]
    list(n=n,events=events,fraction=n/a$n,event_fraction=matrix(divide(as.vector(events),as.vector(n)),4,M))}
  decision_points<-function(y,p,t=plan$thresholds){
    check_decision(y,p,t);models<-colnames(p);K<-length(t);M<-ncol(p);bins<-matrix(findInterval(as.vector(p),t),nrow(p),M,dimnames=dimnames(p))
    a<-count_arrays(y,bins,K,models);met<-metric_arrays(a,t)
    points<-data.frame(model=rep(c(models,"All","None"),each=K),threshold=rep(t,M+2L),n=a$n,events=a$events,
      TP=as.vector(a$TP),FP=as.vector(a$FP),FN=as.vector(a$FN),TN=as.vector(a$TN),stringsAsFactors=FALSE)
    points$alerts<-points$TP+points$FP
    for(nm in fixed_metrics)points[[nm]]<-as.vector(met[[nm]])
    points$PPV_status<-ifelse(points$alerts==0,"NO_POSITIVE_PREDICTIONS","OK")
    points$NPV_status<-ifelse(points$TN+points$FN==0,"NO_NEGATIVE_PREDICTIONS","OK")
    points$sensitivity_status<-ifelse(points$events==0,"NO_EVENTS","OK")
    points$specificity_status<-ifelse(points$n==points$events,"NO_NONEVENTS","OK")
    id<-match(plan$risk_cutpoints,t);if(anyNA(id))stop("决策曲线网格必须包含冻结5%、15%、30%切点。")
    b<-band_arrays(a,id,M)
    bands<-data.frame(model=rep(models,each=4),band=rep(1:4,M),risk_band=rep(band_labels,M),n=as.vector(b$n),events=as.vector(b$events),
      fraction=as.vector(b$fraction),event_fraction=as.vector(b$event_fraction),mean_prediction=NA_real_,stringsAsFactors=FALSE)
    for(j in seq_len(M)){g<-findInterval(p[,j],plan$risk_cutpoints)+1L
      for(k in 1:4){ii<-which(g==k);if(length(ii))bands$mean_prediction[(j-1L)*4L+k]<-mean(p[ii,j])}}
    bands$status<-ifelse(bands$n==0,"EMPTY_BAND",ifelse(bands$events==0,"NO_EVENTS_OBSERVED",ifelse(bands$n==bands$events,"NO_NONEVENTS_OBSERVED","OK")))
    list(points=points,bands=bands,bins=bins)}
  decision_pairs<-function(models){
    rows<-list();put<-function(a,b,role){if(all(c(a,b)%in%models))rows[[length(rows)+1L]]<<-data.frame(first=a,second=b,comparison=paste(a,"minus",b),role=role)}
    for(s in c("raw","updated"))for(v in list(c("B","A"),c("C","A"),c("D","C")))put(paste0(v[1],"_",s),paste0(v[2],"_",s),"model_comparison")
    for(a in arms)put(paste0(a,"_updated"),paste0(a,"_raw"),"crossfitted_update_secondary")
    for(a in setdiff(models,c("All","None"))){put(a,"All","versus_assess_all");put(a,"None","versus_assess_none")}
    for(a in intersect(c(paste0(arms,"_raw"),paste0(arms,"_updated")),models))for(b in intersect(c("APACHE_IV","APACHE_IVa"),models))put(a,b,"native_benchmark_context")
    put("APACHE_IVa","APACHE_IV","version_context")
    if(!length(rows))return(data.frame(first=character(),second=character(),comparison=character(),role=character()))
    do.call(rbind,rows)}
  interval<-function(x,B){valid<-sum(is.finite(x));ci<-if(valid>=ceiling(plan$minimum_valid_fraction*B))as.numeric(stats::quantile(x[is.finite(x)],c(.025,.975),names=FALSE,type=7))else c(NA_real_,NA_real_)
    c(lower=ci[1],upper=ci[2],bootstrap_valid=valid)}
  evaluate_decisions<-function(y,p,hospital=NULL,B=plan$bootstrap_n,seed=plan$seed,progress=FALSE,keep_draws=FALSE){
    initial<-decision_points(y,p);t<-plan$thresholds;K<-length(t);M<-ncol(p);models<-c(colnames(p),"All","None");n<-length(y)
    if(length(B)!=1L||!is.finite(B)||B<2L||B!=as.integer(B))stop("Bootstrap次数无效。")
    mode<-if(is.null(hospital))"patient"else"hospital_cluster"
    if(!is.null(hospital)){
      if(length(hospital)!=n||anyNA(hospital)||any(!nzchar(trimws(as.character(hospital))))||length(unique(hospital))<2L)stop("医院重抽样要求完整医院ID和至少两家医院。")
      hi<-match(as.character(hospital),unique(as.character(hospital)));H<-max(hi);groups<-split(seq_len(n),factor(hi,levels=seq_len(H)))}
    ti<-match(plan$display_thresholds,t);bi<-match(plan$risk_cutpoints,t)
    nb<-array(NA_real_,c(B,K,M+2L),dimnames=list(NULL,NULL,models))
    fm<-array(NA_real_,c(B,length(ti),M+2L,length(fixed_metrics)),dimnames=list(NULL,NULL,models,fixed_metrics))
    ba<-array(NA_real_,c(B,4L,M,2L),dimnames=list(NULL,NULL,colnames(p),c("fraction","event_fraction")))
    draws<-matrix(0,B,2,dimnames=list(NULL,c("patients","events")));set.seed(seed)
    for(b in seq_len(B)){
      ii<-if(is.null(hospital))sample.int(n,n,replace=TRUE)else unlist(groups[sample.int(H,H,replace=TRUE)],use.names=FALSE)
      a<-count_arrays(y[ii],initial$bins[ii,,drop=FALSE],K,colnames(p));v<-metric_arrays(a,t);nb[b,,]<-v$net_benefit
      for(m in fixed_metrics)fm[b,,,m]<-v[[m]][ti,,drop=FALSE]
      band<-band_arrays(a,bi,M);ba[b,,,"fraction"]<-band$fraction;ba[b,,,"event_fraction"]<-band$event_fraction
      draws[b,]<-c(a$n,a$events)
      if(isTRUE(progress)&&(b%%500L==0L||b==B))logmsg(sprintf("%s 配对重抽样：%d/%d。",mode,b,B))
    }
    points<-initial$points
    cis<-t(vapply(seq_len(K*(M+2L)),function(j){a<-(j-1L)%/%K+1L;k<-(j-1L)%%K+1L;interval(nb[,k,a],B)},numeric(3)))
    points$nb_lower<-cis[,1];points$nb_upper<-cis[,2];points$nb_bootstrap_valid<-as.integer(cis[,3])
    fixed<-points[points$threshold%in%plan$display_thresholds,,drop=FALSE]
    for(m in fixed_metrics){ci<-t(vapply(seq_len(nrow(fixed)),function(j){a<-(j-1L)%/%length(ti)+1L;k<-(j-1L)%%length(ti)+1L;interval(fm[,k,a,m],B)},numeric(3)))
      fixed[[paste0(m,"_lower")]]<-ci[,1];fixed[[paste0(m,"_upper")]]<-ci[,2];fixed[[paste0(m,"_bootstrap_valid")]]<-as.integer(ci[,3])}
    pairs<-decision_pairs(models);diffs<-lapply(seq_len(nrow(pairs)),function(j){x<-pairs$first[j];z<-pairs$second[j];ci<-t(vapply(seq_len(K),function(k)interval(nb[,k,x]-nb[,k,z],B),numeric(3)))
      data.frame(pairs[j,],threshold=t,n=n,events=sum(y),delta_nb=points$net_benefit[points$model==x]-points$net_benefit[points$model==z],lower=ci[,1],upper=ci[,2],bootstrap_valid=as.integer(ci[,3]),row.names=NULL)})
    bands<-initial$bands
    for(m in c("fraction","event_fraction")){ci<-t(vapply(seq_len(nrow(bands)),function(j){a<-(j-1L)%/%4L+1L;k<-(j-1L)%%4L+1L;interval(ba[,k,a,m],B)},numeric(3)))
      bands[[paste0(m,"_lower")]]<-ci[,1];bands[[paste0(m,"_upper")]]<-ci[,2];bands[[paste0(m,"_bootstrap_valid")]]<-as.integer(ci[,3])}
    differences<-if(length(diffs))do.call(rbind,diffs)else data.frame()
    out<-list(points=points,fixed=fixed,bands=bands,differences=differences,
      audit=data.frame(resampling=mode,requested=B,seed=seed,hospitals=if(is.null(hospital))NA_integer_ else H,
        min_patients=min(draws[,1]),max_patients=max(draws[,1]),min_events=min(draws[,2]),max_events=max(draws[,2]),
        single_class_draws=sum(draws[,2]==0|draws[,2]==draws[,1]),min_NB_valid=min(points$nb_bootstrap_valid)),B=B)
    if(isTRUE(keep_draws)){out$bootstrap_nb<-nb;out$draw_counts<-draws}
    out}

  check_status<-function(st){p<-file.path(cfg$OUTPUT_PARENT,st,"to_share",paste0(st,"_STATUS.txt"))
    if(!file.exists(p)||!"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(p,warn=FALSE))stop("请先完成",st,"。")}
  check_lineage<-function(st){check_status(st);p<-file.path(cfg$OUTPUT_PARENT,st,"to_share/01_inputs.csv")
    if(!file.exists(p))stop(st,"缺少输入审计。")
    z<-utils::read.csv(p,stringsAsFactors=FALSE,colClasses="character");need(z,c("file","md5"),paste(st,"输入审计"))
    if(!nrow(z)||anyNA(z$file)||anyNA(z$md5)||any(!file.exists(z$file)))stop(st,"上游本地输入不可访问。")
    now<-unname(tools::md5sum(z$file));if(any(now!=z$md5))stop(st,"上游输入已改变，请先核对。")
    z}
  align_rows<-function(target,meta,label){need(meta,c("source","stay_id","patient_key","hospital_id"),label)
    k<-unique_key(meta$patient_key,label);j<-match(target$patient_key,k)
    if(length(k)!=nrow(target)||anyNA(j)||anyDuplicated(j)||
      !identical(as.character(meta$source[j]),as.character(target$source))||
      !identical(as.character(meta$stay_id[j]),as.character(target$stay_id))||
      !identical(as.character(meta$hospital_id[j]),as.character(target$hospital_id)))stop(label,"患者键或医院匹配失败。")
    j}
  load_inputs<-function(paths){
    lin<-list();for(st in c("Step7","Step11","Step13","Step14","Step18"))lin[[st]]<-check_lineage(st)
    z<-readRDS(paths["benchmark"]);r<-readRDS(paths["raw"]);u<-readRDS(paths["updated"])
    need(z$metadata,c("source","stay_id","patient_key","landmark","hospital_id"),"Step18元数据");need(z$outcomes,c("event_primary","endpoint"),"Step18结局")
    k<-unique_key(z$metadata$patient_key,"Step18 patient_key");unique_key(paste(z$metadata$source,z$metadata$stay_id),"Step18 stay_id")
    y<-binary(z$outcomes$event_primary);hospital<-as.character(z$metadata$hospital_id);p<-z$prediction
    if(!identical(z$version,"Step18_v1")||!identical(z$plan$endpoint,plan$endpoint)||nrow(z$metadata)!=5284L||length(y)!=5284L||sum(y)!=219L||
      anyNA(z$metadata$source)||any(z$metadata$source!="eICU")||anyNA(z$metadata$landmark)||any(z$metadata$landmark!="T48")||
      anyNA(z$outcomes$endpoint)||any(z$outcomes$endpoint!=plan$endpoint)||!identical(colnames(p),model_map$model)||
      anyNA(hospital)||any(!nzchar(trimws(hospital)))||length(unique(hospital))<5L)stop("Step18冻结队列、概率列、医院或终点不符。")
    if(!is.numeric(p)||!is.matrix(p)||nrow(p)!=length(y))stop("Step18概率矩阵类型异常。")
    basecols<-c(paste0(arms,"_raw"),paste0(arms,"_updated"));check_decision(y,p[,basecols,drop=FALSE],plan$thresholds)
    ap<-p[,c("APACHE_IV","APACHE_IVa"),drop=FALSE];valid<-is.finite(ap)&ap>=0&ap<=1
    if(any(!valid&!is.na(ap)))stop("APACHE缺失应为NA；发现非法值。")
    if(!is.matrix(z$APACHE_status)||!identical(dim(z$APACHE_status),dim(ap))||!identical(colnames(z$APACHE_status),colnames(ap))||anyNA(z$APACHE_status)||
      !identical(unname(z$APACHE_status%in%c("VALID","VALID_DUPLICATE_IDENTICAL")),as.vector(valid)))stop("APACHE状态与概率不符。")
    common<-valid[,1]&valid[,2]
    if(!is.logical(z$masks$common)||anyNA(z$masks$common)||!identical(z$masks$common,common)||!any(common))stop("Step18共同患者交集不符或为空。")
    check_decision(y[common],p[common,,drop=FALSE],plan$thresholds)
    l18<-lin$Step18;need(l18,"input","Step18输入审计")
    if(!identical(as.character(z$input_hashes[l18$input]),as.character(l18$md5)))stop("Step18对象与其输入审计不一致。")
    ri<-align_rows(z$metadata,r$metadata,"Step13");ui<-align_rows(z$metadata,u$metadata,"Step14")
    need(r$outcomes,c("event_primary","endpoint"),"Step13结局");need(u$outcomes,c("event_primary","endpoint"),"Step14结局")
    if(!identical(r$version,"Step13_v1")||!identical(r$prediction_endpoint,plan$endpoint)||!identical(r$evaluation_endpoint,plan$endpoint)||
      !identical(u$version,"Step14_v1")||!identical(u$source_model_hash,r$model_hash)||!identical(u$plan$endpoint,plan$endpoint)||
      !identical(u$plan$primary_scheme,"hospital_blocked")||!identical(u$plan$primary_update,"intercept")||
      !identical(binary(r$outcomes$event_primary[ri]),y)||!identical(binary(u$outcomes$event_primary[ui]),y)||
      anyNA(r$outcomes$endpoint)||anyNA(u$outcomes$endpoint)||any(r$outcomes$endpoint!=plan$endpoint)||any(u$outcomes$endpoint!=plan$endpoint))stop("Step13/14模型来源或院内死亡定义不一致。")
    if(!is.matrix(r$prediction)||!is.matrix(r$linear_predictor)||!identical(colnames(r$prediction),arms)||!identical(dim(r$prediction),dim(r$linear_predictor))||
      any(!is.finite(r$linear_predictor))||max(abs(stats::plogis(r$linear_predictor)-r$prediction))>1e-12)stop("Step13冻结概率无法由线性预测值复现。")
    cf<-u$results$hospital_blocked;cols<-c(paste0(arms,"_raw"),paste0(arms,"_intercept"))
    if(!is.list(cf)||!is.matrix(cf$prediction)||nrow(cf$prediction)!=length(y)||!all(cols%in%colnames(cf$prediction))||length(cf$fold)!=length(y))stop("Step14交叉拟合预测不完整。")
    fold<-cf$fold[ui];need(cf$audit,c("shared_patients","shared_hospitals"),"Step14分折审计")
    if(anyNA(fold)||!setequal(fold,1:5)||any(vapply(split(fold,hospital),function(v)length(unique(v))!=1L,logical(1)))||
      nrow(cf$audit)!=5L||anyNA(cf$audit$shared_patients)||anyNA(cf$audit$shared_hospitals)||any(cf$audit$shared_patients!=0|cf$audit$shared_hospitals!=0)||
      !identical(as.integer(z$hospital_blocked_update_fold),as.integer(fold)))stop("校准折、医院隔离或患者隔离审计失败。")
    old<-cf$prediction[ui,cols,drop=FALSE];colnames(old)<-basecols;check_decision(y,old,plan$thresholds)
    errors<-c(raw_Step13=max(abs(p[,paste0(arms,"_raw"),drop=FALSE]-r$prediction[ri,,drop=FALSE])),
      raw_Step14=max(abs(old[,paste0(arms,"_raw"),drop=FALSE]-p[,paste0(arms,"_raw"),drop=FALSE])),
      update_Step14=max(abs(old[,paste0(arms,"_updated"),drop=FALSE]-p[,paste0(arms,"_updated"),drop=FALSE])))
    if(any(!is.finite(errors)|errors>1e-12))stop("Step18保存概率不能复现Step13/14。")
    dca11<-utils::read.csv(paths["dca11"],stringsAsFactors=FALSE);cut11<-utils::read.csv(paths["cut11"],stringsAsFactors=FALSE)
    need(dca11,"threshold","Step11曲线");need(cut11,"threshold","Step11阈值")
    if(!identical(sort(unique(dca11$threshold)),plan$thresholds)||!identical(sort(unique(cut11$threshold)),plan$display_thresholds))stop("Step11阈值定义不一致；不自动重选外部阈值。")
    census<-utils::read.csv(paths["flow18"],stringsAsFactors=FALSE);need(census,c("cohort","n","hospital_deaths","hospitals"),"Step18队列");census$events<-census$hospital_deaths
    for(nm in c("full_frozen","common")){ii<-if(nm=="common")common else rep(TRUE,length(y));q<-census[census$cohort==nm,]
      if(nrow(q)!=1L||q$n!=sum(ii)||q$events!=sum(y[ii])||q$hospitals!=length(unique(hospital[ii])))stop("Step18队列规模/事件/医院数未复现。")}
    m18<-utils::read.csv(paths["metrics18"],stringsAsFactors=FALSE);need(m18,c("cohort","resampling","model","metric","estimate"),"Step18指标")
    checks<-list()
    for(a in colnames(p))for(m in c("Brier","Mean_prediction","Event_fraction")){
      v<-switch(m,Brier=mean((p[common,a]-y[common])^2),Mean_prediction=mean(p[common,a]),Event_fraction=mean(y[common]))
      prev<-m18$estimate[m18$cohort=="common"&m18$resampling=="hospital_cluster"&m18$model==a&m18$metric==m]
      if(length(prev)!=1L||!is.finite(prev)||abs(v-prev)>1e-10)stop("Step18共同队列指标不能复现：",a," / ",m)
      checks[[length(checks)+1L]]<-data.frame(model=a,metric=m,previous=prev,recomputed=v,absolute_difference=abs(v-prev))}
    model7<-readRDS(paths["model7"]);model13<-readRDS(paths["model13"])
    if(!identical(hash_object(model13),r$model_hash))stop("Step13模型对象与冻结预测的模型指纹不符。")
    list(y=y,p=p,hospital=hospital,common=common,census=census[census$cohort%in%c("full_frozen","common"),],
      roundtrip=data.frame(check=names(errors),max_absolute_error=as.numeric(errors)),metric_checks=do.call(rbind,checks),model7=model7,model13=model13)}
  coefficient_export<-function(obj,source,expected_version,endpoint){
    if(!identical(obj$version,expected_version)||!identical(obj$endpoint,endpoint)||obj$training_n!=5133L||!identical(names(obj$models),arms))stop(source,"冻结模型版本、终点或训练人数不符。")
    out<-lapply(arms,function(a){m<-obj$models[[a]];pp<-m$preprocessor;cf<-m$coefficients
      if(!is.numeric(cf)||any(!is.finite(cf))||!identical(names(cf),c("(Intercept)",pp$design_columns))||anyDuplicated(names(cf))||
        !identical(names(pp$center),pp$design_columns)||!identical(names(pp$scale),pp$design_columns)||any(!is.finite(pp$center))||any(!is.finite(pp$scale)|pp$scale<=0)||pp$training_n!=5133L)stop(source,"模型",a,"的系数与冻结标准化参数不一致。")
      raw<-c(cf[1]-sum(cf[-1]*pp$center/pp$scale),cf[-1]/pp$scale)
      data.frame(source=source,endpoint=endpoint,model=a,feature=names(cf),coefficient_standardized=as.numeric(cf),
        training_center=c(NA_real_,unname(pp$center)),training_scale=c(NA_real_,unname(pp$scale)),coefficient_original_design=as.numeric(raw),
        missingness_indicator=endsWith(names(cf),"__NA"),nonzero=cf!=0,alpha=as.numeric(m$alpha),lambda=as.numeric(m$lambda),
        interpretation="Frozen penalized parameter; no inferential P value or causal meaning",stringsAsFactors=FALSE)})
    do.call(rbind,out)}

  pdf_backend<-"uninitialized"
  open_pdf<-function(p,width,height,pointsize){
    ok<-FALSE
    if(isTRUE(capabilities("cairo")))ok<-tryCatch({grDevices::cairo_pdf(p,width=width,height=height,pointsize=pointsize,family=cfg$PDF_FAMILY,onefile=TRUE);TRUE},error=function(e){add_warning(conditionMessage(e),"PDF Cairo");FALSE})
    if(ok){pdf_backend<<-"cairo_pdf; font subsetting/embedding";return(invisible(TRUE))}
    grDevices::pdf(p,width=width,height=height,pointsize=pointsize,family="Helvetica",onefile=TRUE,useDingbats=FALSE)
    pdf_backend<<-"pdf Helvetica fallback; verify/embed fonts before publication"
  }
  save_pages<-function(stem,drawers,width=13,height=10){
    p<-reg(paste0("to_share/",stem,".pdf"));open_pdf(p,width,height,20)
    tryCatch(for(f in drawers)f(),finally=grDevices::dev.off())
    if(isTRUE(cfg$MAKE_PNG))for(i in seq_along(drawers)){
      suf<-if(length(drawers)>1)paste0("_",i)else""
      grDevices::png(reg(paste0("to_share/",stem,suf,".png")),width=width,height=height,units="in",res=300,pointsize=20)
      tryCatch(drawers[[i]](),finally=grDevices::dev.off())
    }
  }
  pformat<-function(x)ifelse(!is.finite(x),"NE",ifelse(x<.001,"<0.001",sprintf("%.3f",x)))
  ciformat<-function(x,l,u)ifelse(!is.finite(x)|!is.finite(l)|!is.finite(u),"NE",sprintf("%.2f (%.2f, %.2f)",x,l,u))
  three_line20<-function(tables,title,stem,notes){
    drawers<-lapply(seq_along(tables),function(k)local({kk<-k;function(){
      t<-tables[[kk]];nr<-nrow(t);nc<-ncol(t);if(nr>8||nc>5)stop("表格超过大字号页面容量。")
      grid::grid.newpage();tx<-function(s,x,y,just="left",bold=FALSE,size=18)grid::grid.text(s,x,y,just=just,gp=grid::gpar(fontfamily="sans",fontsize=size,fontface=if(bold)"bold"else"plain"))
      tx(title,.04,.95,bold=TRUE,size=24);tx(names(tables)[kk],.04,.89,bold=TRUE,size=20)
      xs<-switch(as.character(nc),`5`=c(.04,.44,.65,.81,.97),`4`=c(.04,.50,.73,.97),`3`=c(.04,.63,.97),`2`=c(.04,.97))
      line<-function(y,lwd)grid::grid.lines(c(.04,.97),c(y,y),gp=grid::gpar(lwd=lwd))
      line(.835,1.4);for(j in seq_len(nc))tx(names(t)[j],xs[j],.80,if(j==1)"left"else"right",TRUE);line(.77,.8)
      ys<-.745-seq_len(nr)*.052
      for(i in seq_len(nr))for(j in seq_len(nc))tx(as.character(t[i,j]),xs[j],ys[i],if(j==1)"left"else"right")
      bottom<-if(nr)min(ys)-.025 else .70;line(bottom,1.4);ny<-bottom-.05
      for(note in notes)for(s in strwrap(note,width=108)){if(ny<.05)stop("表格注释溢出。");tx(s,.04,ny);ny<-ny-.035}
      tx(paste(kk,"/",length(tables)),.97,.025,"right")
    }}))
    # Tables are PDF+editable RTF; PNG is reserved for figures.
    p<-reg(paste0("to_share/",stem,".pdf"));open_pdf(p,16,10,18);tryCatch(for(f in drawers)f(),finally=grDevices::dev.off())
    esc<-function(x){x<-gsub("\\","\\\\",x,fixed=TRUE);x<-gsub("{","\\{",x,fixed=TRUE);gsub("}","\\}",x,fixed=TRUE)}
    rt<-c("{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 Arial;}}\\landscape\\paperw23040\\paperh14400\\margl600\\margr600\\margt600\\margb600\\f0\\fs36")
    for(k in seq_along(tables)){
      if(k>1)rt<-c(rt,"\\page");t<-tables[[k]];rt<-c(rt,paste0("\\pard\\b\\fs44 ",esc(title),"\\par\\fs38 ",esc(names(tables)[k]),"\\b0\\par\\par"));rows<-rbind(names(t),as.matrix(t))
      ends<-switch(as.character(ncol(t)),`5`=c(7200,11000,14800,18100,21500),`4`=c(7200,12000,16800,21500),`3`=c(8500,15000,21500),`2`=c(11000,21500))
      for(i in seq_len(nrow(rows))){bd<-paste0(if(i==1)"\\clbrdrt\\brdrs\\brdrw15\\clbrdrb\\brdrs\\brdrw10"else"",if(i==nrow(rows))"\\clbrdrb\\brdrs\\brdrw15"else"")
        rt<-c(rt,paste0("\\trowd\\trgaph60\\trkeep",paste0(bd,"\\cellx",ends,collapse=""),paste(vapply(seq_len(ncol(t)),function(j)paste0("\\pard\\intbl",if(j==1)"\\ql "else"\\qr ","\\fs36 ",if(i==1)"\\b "else"\\b0 ",esc(rows[i,j]),"\\cell"),character(1)),collapse=""),"\\row"))}
      rt<-c(rt,paste0("\\pard\\fs36 ",paste(esc(notes),collapse="\\par "),"\\par"))
    };txt_out(c(rt,"}"),paste0("to_share/",stem,".rtf"))
  }
  fmt<-function(x,d=1)ifelse(is.finite(x),formatC(x,format="f",digits=d),"NE")
  fmt_ci<-function(x,l,u,scale=100,d=1)ifelse(is.finite(x),ifelse(is.finite(l)&is.finite(u),paste0(fmt(scale*x,d)," [",fmt(scale*l,d),", ",fmt(scale*u,d),"]"),paste0(fmt(scale*x,d)," [NE]")),"NE")
  scope_title<-function(x)if(x=="full_frozen")"Full frozen eICU cohort"else"APACHE common-patient cohort"
  short_label<-function(x){s<-gsub("_raw"," raw",x,fixed=TRUE);s<-gsub("_updated"," updated",s,fixed=TRUE);s<-gsub("APACHE_","APACHE ",s,fixed=TRUE);s[s=="All"]<-"Assess all";s[s=="None"]<-"Assess none";s}
  figure_shell<-function(title,subtitle,notes){
    par(mfrow=c(1,2),mar=c(4.2,4.7,3.0,1.0),oma=c(6.5,0,4.3,0),cex=1,cex.axis=1,cex.lab=1,las=1)
    function(legend_names,legend_colors,legend_lty=rep(1,length(legend_names))){
      mtext(title,outer=TRUE,side=3,line=2.5,font=2,cex=1.15);mtext(subtitle,outer=TRUE,side=3,line=.9,cex=1)
      count<-length(legend_names);xs<-seq(.12,.88,length.out=count)
      for(j in seq_len(count)){grid::grid.lines(c(xs[j]-.025,xs[j]+.015),c(.16,.16),gp=grid::gpar(col=legend_colors[j],lwd=2.6,lty=legend_lty[j]));grid::grid.text(legend_names[j],xs[j],.125,gp=grid::gpar(fontsize=18,fontfamily="sans"))}
      for(j in seq_along(notes))grid::grid.text(notes[j],.035,.075-(j-1)*.027,just="left",gp=grid::gpar(fontsize=18,fontfamily="sans"))
    }}
  draw_dca<-function(curves,scope,stage,xmax){
    mods<-if(scope=="full_frozen")if(stage=="raw")c("A_raw","C_raw")else c("C_raw","C_updated")else if(stage=="raw")c("C_raw","APACHE_IV","APACHE_IVa")else c("C_updated","APACHE_IV","APACHE_IVa")
    z<-curves[curves$cohort==scope&curves$resampling=="hospital_cluster"&curves$threshold<=xmax,,drop=FALSE]
    finish<-figure_shell(paste(scope_title(scope),if(stage=="raw")"- frozen raw"else"- cross-fitted update"),
      "Endpoint: hospital death after T48; fixed risks; no threshold optimization",
      c("Shading: pointwise 95% whole-hospital bootstrap intervals, conditional on fixed predictions.",
        "Right: model detail; assess-all is shown on the full-scale left panel. Hypothetical clinical review only."))
    for(detail in c(FALSE,TRUE)){
      chosen<-c(mods,if(!detail)"All","None");v<-z[z$model%in%chosen,,drop=FALSE]
      yy<-100*c(0,v$net_benefit,v$nb_lower[v$model%in%mods],v$nb_upper[v$model%in%mods]);yl<-range(yy[is.finite(yy)]);if(diff(yl)<.01)yl<-yl+c(-.01,.01);yl<-yl+c(-.04,.06)*diff(yl)
      plot(NA,xlim=c(.01,xmax)*100,ylim=yl,xlab="Risk threshold (%)",ylab="Net benefit per 100 patients",bty="l",xaxt="n",xaxs="i")
      axis(1,at=unique(c(1,seq(5,100*xmax,5))));abline(h=0,col="#CCCCCC",lty=3)
      for(m in mods){q<-v[v$model==m,];ok<-is.finite(q$nb_lower)&is.finite(q$nb_upper)
        if(all(ok))polygon(c(q$threshold,rev(q$threshold))*100,c(q$nb_lower,rev(q$nb_upper))*100,col=grDevices::adjustcolor(colors[m],alpha.f=.12),border=NA)}
      for(m in chosen){q<-v[v$model==m,];lines(q$threshold*100,q$net_benefit*100,col=colors[m],lwd=2.4,lty=if(m=="All")2 else if(m=="None")3 else 1)}
      title(if(detail)"Model detail"else"Includes assess-all, full y-scale",cex.main=1)
    }
    all<-c(mods,"All","None");finish(short_label(all),colors[all],ifelse(all=="All",2,ifelse(all=="None",3,1)))}
  draw_differences<-function(d,scope){
    ids<-if(scope=="full_frozen")c("C_raw minus A_raw","D_raw minus C_raw","C_updated minus C_raw")else c("C_raw minus APACHE_IV","C_raw minus APACHE_IVa","C_updated minus APACHE_IV","C_updated minus APACHE_IVa")
    labs<-if(scope=="full_frozen")c("C raw - A raw","D raw - C raw","C updated - raw")else c("C raw - IV","C raw - IVa","C updated - IV","C updated - IVa")
    cc<-c("#477EBB","#D98936","#277F79","#8D68AD")[seq_along(ids)]
    q<-d[d$cohort==scope&d$resampling=="hospital_cluster"&d$comparison%in%ids,]
    finish<-figure_shell(paste(scope_title(scope),"- paired net benefit"),"Positive values favor the first strategy; same patients in every contrast",
      c("Pointwise 95% paired hospital-bootstrap intervals; multiple thresholds are descriptive, not simultaneous tests.",
        "Updates are cross-fitted secondary analyses. APACHE uses a different information horizon."))
    for(xmax in c(.30,.50)){
      z<-q[q$threshold<=xmax,];v<-100*c(0,z$delta_nb,z$lower,z$upper);yl<-range(v[is.finite(v)]);if(diff(yl)<.01)yl<-yl+c(-.01,.01);yl<-yl+c(-.07,.07)*diff(yl)
      plot(NA,xlim=c(1,xmax*100),ylim=yl,xlab="Risk threshold (%)",ylab="Net-benefit difference per 100",bty="l",xaxt="n",xaxs="i");axis(1,at=unique(c(1,seq(5,xmax*100,5))));abline(h=0,col="#999999",lty=2)
      for(j in seq_along(ids)){a<-z[z$comparison==ids[j],];if(all(is.finite(a$lower)&is.finite(a$upper)))polygon(c(a$threshold,rev(a$threshold))*100,c(a$lower,rev(a$upper))*100,col=grDevices::adjustcolor(cc[j],alpha.f=.12),border=NA)
        lines(a$threshold*100,a$delta_nb*100,col=cc[j],lwd=2.3,lty=j)}
      title(paste0("1-",xmax*100,"% threshold range"),cex.main=1)
    };finish(labs,cc,seq_along(ids))}
  draw_bands<-function(bands,scope){
    mods<-c("C_raw","C_updated",if(scope=="common")c("APACHE_IV","APACHE_IVa"));z<-bands[bands$cohort==scope&bands$resampling=="hospital_cluster"&bands$model%in%mods,]
    finish<-figure_shell(paste(scope_title(scope),"- fixed risk bands"),"Bands inherited from Step11; probability equal to a cutpoint enters the higher band",
      c("Bars: pointwise hospital-bootstrap intervals. Empty bands remain NE; no patients are silently removed.",
        "Empirical bootstrap intervals can be degenerate when a band has no observed events."))
    for(metric in c("fraction","event_fraction")){
      v<-100*c(z[[metric]],z[[paste0(metric,"_upper")]]);top<-max(c(1,v[is.finite(v)]));yl<-c(0,min(100,top*1.12))
      plot(NA,xlim=c(.6,4.4),ylim=yl,xlab="Predicted-risk band",ylab=if(metric=="fraction")"Patients in band (%)"else"Observed hospital mortality (%)",bty="l",xaxt="n");axis(1,at=1:4,labels=c("<5%","5-<15%","15-<30%",">=30%"))
      for(j in seq_along(mods)){q<-z[z$model==mods[j],];q<-q[order(q$band),];x<-1:4+(j-(length(mods)+1)/2)*.12
        lines(x,100*q[[metric]],col=colors[mods[j]],lwd=2);points(x,100*q[[metric]],col=colors[mods[j]],pch=16,cex=1)
        good<-is.finite(q[[paste0(metric,"_lower")]])&is.finite(q[[paste0(metric,"_upper")]])
        segments(x[good],100*q[[paste0(metric,"_lower")]][good],x[good],100*q[[paste0(metric,"_upper")]][good],col=colors[mods[j]],lwd=2)}
      title(if(metric=="fraction")"Distribution"else"Observed event fraction",cex.main=1)
    };finish(short_label(mods),colors[mods])}
  pretty_feature<-function(s){s<-gsub("__NA$"," [missingness]",s);s<-gsub("__"," / ",s,fixed=TRUE);s<-gsub("_"," ",s,fixed=TRUE);s}
  draw_coefficients<-function(coef,source){
    z<-coef[coef$source==source&coef$model=="C"&coef$feature!="(Intercept)",];z<-z[order(-abs(z$coefficient_standardized),z$feature),];z<-head(z[z$nonzero,,drop=FALSE],12L)
    grid::grid.newpage();tx<-function(s,x,y,just="left",size=20,bold=FALSE)grid::grid.text(s,x,y,just=just,gp=grid::gpar(fontfamily="sans",fontsize=size,fontface=if(bold)"bold"else"plain"))
    tx("Frozen elastic-net model C: largest coefficients",.035,.96,size=25,bold=TRUE)
    tx(if(source=="Step7")"30-day mortality after T48"else"Hospital mortality after T48 (companion model)",.035,.91)
    tx("Feature",.035,.85,bold=TRUE);tx("Standardized coefficient",.96,.85,"right",bold=TRUE)
    if(!nrow(z)){tx("No nonzero predictor coefficients in the frozen model.",.035,.65);return(invisible(NULL))}
    mx<-max(abs(z$coefficient_standardized))*1.1;if(mx==0)mx<-1
    xx<-function(v).76+.18*v/mx;ys<-seq(.79,.25,length.out=nrow(z));grid::grid.lines(rep(xx(0),2),c(.21,.82),gp=grid::gpar(col="#888888",lty=2))
    for(j in seq_len(nrow(z))){lab<-paste(strwrap(pretty_feature(z$feature[j]),width=54),collapse="\n");tx(lab,.035,ys[j],size=18)
      v<-z$coefficient_standardized[j];col<-if(v<0)"#477EBB"else"#D98936"
      grid::grid.lines(c(xx(0),xx(v)),rep(ys[j],2),gp=grid::gpar(col=col,lwd=5));grid::grid.circle(xx(v),ys[j],r=grid::unit(1.5,"mm"),gp=grid::gpar(col=col,fill=col));tx(fmt(v,3),.985,ys[j],"right",size=18)}
    ticks<-pretty(c(-mx,mx),n=4);ticks<-ticks[abs(ticks)<=mx];grid::grid.lines(c(.58,.94),c(.20,.20))
    for(v in ticks){grid::grid.lines(rep(xx(v),2),c(.20,.19));tx(fmt(v,2),xx(v),.17,"centre",size=18)}
    tx("Top 12 by absolute penalized coefficient; all parameters, including zeros, are in the source CSV.",.035,.105,size=18)
    tx("One unit is one frozen training SD of the model-design column, including binary/missingness columns.",.035,.07,size=18)
    tx("Descriptive fitted parameters; not SHAP, causal effects, or confidence intervals.",.035,.035,size=18)}
  make_visuals<-function(curves,diffs,fixed,bands,coefficients){
    for(scope in c("full_frozen","common")){
      save_pages(paste0("Figure_S_Step20_DCA_",scope),lapply(c("raw","updated"),function(s)local({ss<-s;gg<-scope;function()draw_dca(curves,gg,ss,.30)})),16,11)
    }
    drawers<-list();for(scope in c("full_frozen","common"))for(stage in c("raw","updated"))drawers[[length(drawers)+1L]]<-local({gg<-scope;ss<-stage;function()draw_dca(curves,gg,ss,.50)})
    save_pages("Figure_S_Step20_DCA_full_range",drawers,16,11)
    save_pages("Figure_S_Step20_paired_net_benefit",lapply(c("full_frozen","common"),function(s)local({ss<-s;function()draw_differences(diffs,ss)})),16,11)
    save_pages("Figure_S_Step20_fixed_risk_bands",lapply(c("full_frozen","common"),function(s)local({ss<-s;function()draw_bands(bands,ss)})),16,11)
    save_pages("Figure_4G_frozen_elastic_net_coefficients",list(function()draw_coefficients(coefficients,"Step7")),16,12)
    save_pages("Figure_S_Step20_hospital_model_coefficients",list(function()draw_coefficients(coefficients,"Step13")),16,12)
    primary<-fixed[fixed$resampling=="hospital_cluster"&!fixed$model%in%c("All","None"),];tables<-list();counts<-list()
    for(scope in c("full_frozen","common")){
      z<-primary[primary$cohort==scope,];z<-z[order(match(z$model,model_map$model),z$threshold),]
      groups<-split(seq_len(nrow(z)),ceiling(seq_len(nrow(z))/6L))
      for(j in seq_along(groups)){q<-z[groups[[j]],];name<-paste(scope_title(scope),";",j)
        tab<-data.frame(`Model / threshold`=paste0(short_label(q$model),"; ",fmt(q$threshold*100,0),"%"),check.names=FALSE)
        for(m in c("sensitivity","specificity","PPV","NPV"))tab[[switch(m,sensitivity="Sensitivity %",specificity="Specificity %",PPV="PPV %",NPV="NPV %",m)]]<-fmt_ci(q[[m]],q[[paste0(m,"_lower")]],q[[paste0(m,"_upper")]])
        tables[[name]]<-tab
        counts[[name]]<-data.frame(`Model / threshold`=tab[[1]],`TP / FP`=paste(q$TP,q$FP,sep=" / "),`FN / TN`=paste(q$FN,q$TN,sep=" / "),`Alerts %`=fmt(100*q$alert_fraction),`NB / 100`=fmt(100*q$net_benefit,2),check.names=FALSE)
      }
    }
    notes<-c("Hospital mortality after T48. Raw models: frozen MIMIC fits; updated models: hospital-blocked cross-fitted intercept update.",
      "Trigger: risk >= threshold. Cutpoints are inherited display thresholds, not validated treatment recommendations.",
      "Brackets: pointwise 95% whole-hospital bootstrap CI conditional on fixed risks. NE: zero denominator or insufficient valid draws.",
      "APACHE comparison uses shared patients only; information horizons differ. Full cohort and common cohort are distinct.")
    three_line20(tables,"External fixed-threshold classification","Table_S_Step20_threshold_performance",notes)
    three_line20(counts,"External threshold counts and workload","Table_S_Step20_threshold_counts",c(notes[1:2],"TP/FP/FN/TN and alert fractions are observed counts. Net benefit is not a count of prevented deaths.","NB = TP/N - FP/N * threshold/(1-threshold). Full grid and paired uncertainty are in source CSV."))
    ids<-c("C_raw minus A_raw","D_raw minus C_raw","C_updated minus C_raw","C_raw minus APACHE_IV","C_raw minus APACHE_IVa","C_updated minus APACHE_IV","C_updated minus APACHE_IVa")
    tabs<-list()
    for(scope in c("full_frozen","common")){q<-diffs[diffs$cohort==scope&diffs$resampling=="hospital_cluster"&diffs$threshold%in%plan$display_thresholds&diffs$comparison%in%ids,]
      q<-q[order(match(q$comparison,ids),q$threshold),];groups<-split(seq_len(nrow(q)),ceiling(seq_len(nrow(q))/6L))
      for(j in seq_along(groups)){z<-q[groups[[j]],];tabs[[paste(scope_title(scope),";",j)]]<-data.frame(Comparison=gsub("_"," ",z$comparison),`Threshold %`=fmt(z$threshold*100,0),`Delta NB / 100 [95% CI]`=fmt_ci(z$delta_nb,z$lower,z$upper,100,2),check.names=FALSE)}}
    three_line20(tabs,"Paired differences in external net benefit","Table_S_Step20_paired_net_benefit",c("Positive values favor the first strategy; all differences use paired hospital resamples on the same cohort.",
      "Intervals are pointwise and conditional on frozen predictions; calibration models are not refitted in the bootstrap.",
      "The full set of model/assess-all/assess-none comparisons and the patient-bootstrap sensitivity are in CSV."))
    invisible(TRUE)}

  main<-function(){
    current_module<<-"preflight";logmsg("Step20预检：阈值同分、零分母、空风险层、医院整体配对重抽样、净获益及冻结系数。")
    preflight();txt_out("PASS: direct TP/FP/NB and risk-band oracles; exact cutpoint ties; zero denominators; empty bands; all-event/no-event draws; independent whole-hospital resampling oracle; zero paired contrasts; frozen coefficient unit conversion.","to_share/02_preflight.txt")
    paths<-c(benchmark="Step18/local_only/eICU_APACHE_paired_predictions.rds",raw="Step13/local_only/eICU_T48_hospital_frozen_predictions.rds",
      updated="Step14/local_only/eICU_crossfitted_recalibration.rds",model7="Step7/local_only/MIMIC_T48_frozen_pipeline.rds",
      model13="Step13/local_only/MIMIC_T48_hospital_frozen_pipeline.rds",flow18="Step18/to_share/03_cohort_flow.csv",
      metrics18="Step18/to_share/04_benchmark_metrics.csv",dca11="Step11/to_share/03_DCA_all_thresholds.csv",cut11="Step11/to_share/05_fixed_threshold_metrics.csv")
    paths<-setNames(file.path(cfg$OUTPUT_PARENT,paths),names(paths));if(!all(file.exists(paths)))stop("缺少上游本地文件：",paste(names(paths)[!file.exists(paths)],collapse=", "))
    h<-setNames(unname(tools::md5sum(paths)),names(paths));csv_out(data.frame(input=names(paths),file=unname(paths),md5=unname(h)),"to_share/01_inputs.csv")
    txt_out(capture.output(dput(plan)),"to_share/02_analysis_plan.txt");csv_out(model_map,"to_share/02_model_definitions.csv")
    current_module<<-"frozen inputs and paired cohorts";z<-load_inputs(paths)
    csv_out(z$roundtrip,"to_share/02_prediction_roundtrip.csv");csv_out(z$metric_checks,"to_share/02_Step18_metric_roundtrip.csv")
    csv_out(data.frame(threshold=plan$display_thresholds,source="Step11 display cutpoints",trigger="probability >= threshold",external_selection=FALSE),"to_share/02_threshold_origin.csv")
    census<-data.frame(cohort=c("full_frozen","common"),n=c(length(z$y),sum(z$common)),events=c(sum(z$y),sum(z$y[z$common])),
      hospitals=c(length(unique(z$hospital)),length(unique(z$hospital[z$common]))),endpoint=plan$endpoint)
    census$event_fraction<-census$events/census$n;csv_out(census,"to_share/03_cohort_flow.csv")
    coefficient<-rbind(coefficient_export(z$model7,"Step7","Step7_v1","30_day_all_cause_death_after_T48"),
      coefficient_export(z$model13,"Step13","Step13_v1",plan$endpoint))
    csv_out(coefficient,"to_share/07_frozen_model_coefficients.csv")
    csv_out(do.call(rbind,lapply(split(coefficient,paste(coefficient$source,coefficient$model)),function(v)data.frame(source=v$source[1],model=v$model[1],
      predictor_n=sum(v$feature!="(Intercept)"),nonzero_predictor_n=sum(v$nonzero&v$feature!="(Intercept)"),alpha=v$alpha[1],lambda=v$lambda[1]))),"to_share/07_coefficient_summary.csv")
    current_module<<-"paired external decision evaluation"
    codehash<-hash_object(lapply(c("decision_points","evaluate_decisions","decision_pairs","count_arrays","metric_arrays","band_arrays","suffix_counts","divide","interval"),function(nm)paste(deparse(body(get(nm))),collapse="\n")))
    cachekey<-hash_object(list(inputs=h,plan=plan,code=codehash,R=as.character(getRversion())))
    results<-list()
    for(scope in c("full_frozen","common")){
      keep<-if(scope=="common")z$common else rep(TRUE,length(z$y));cols<-if(scope=="common")model_map$model else setdiff(model_map$model,c("APACHE_IV","APACHE_IVa"))
      y<-z$y[keep];p<-z$p[keep,cols,drop=FALSE];hospital<-z$hospital[keep]
      logmsg(sprintf("%s：%d人，%d例院内死亡，%d家医院；固定阈值与固定预测。",scope,length(y),sum(y),length(unique(hospital))))
      for(mode in c("hospital_cluster","patient")){
        current_module<<-paste(scope,mode);key<-hash_object(list(cachekey=cachekey,cohort=scope,resampling=mode))
        ev<-cache_result(paste0("local_only/cache/",scope,"_",mode,".rds"),key,function()evaluate_decisions(y,p,
          if(mode=="hospital_cluster")hospital else NULL,B=plan$bootstrap_n,seed=plan$seed+20000L+100L*match(scope,c("full_frozen","common"))+match(mode,c("hospital_cluster","patient")),progress=TRUE))
        if(!identical(ev$B,plan$bootstrap_n)||any(ev$points$nb_bootstrap_valid!=plan$bootstrap_n)||any(ev$differences$bootstrap_valid!=plan$bootstrap_n))stop("净获益重抽样缓存不完整。")
        results[[paste(scope,mode)]]<-list(points=cbind(cohort=scope,resampling=mode,ev$points),fixed=cbind(cohort=scope,resampling=mode,ev$fixed),
          differences=cbind(cohort=scope,resampling=mode,ev$differences),bands=cbind(cohort=scope,resampling=mode,ev$bands),audit=cbind(cohort=scope,ev$audit))
      }
    }
    combine<-function(nm)do.call(rbind,lapply(results,`[[`,nm))
    curves<-combine("points");fixed<-combine("fixed");diffs<-combine("differences");bands<-combine("bands");audit<-combine("audit")
    csv_out(curves,"to_share/04_DCA_all_thresholds.csv");csv_out(diffs,"to_share/04_DCA_paired_differences.csv")
    csv_out(audit,"to_share/04_bootstrap_audit.csv");csv_out(fixed,"to_share/05_fixed_threshold_metrics.csv");csv_out(bands,"to_share/06_fixed_risk_bands.csv")
    checks<-list();for(scope in c("full_frozen","common")){
      q<-curves[curves$cohort==scope&curves$resampling=="hospital_cluster",];b<-bands[bands$cohort==scope&bands$resampling=="hospital_cluster",]
      direct<-(q$TP-q$FP*q$threshold/(1-q$threshold))/q$n
      if(any(q$TP+q$FP+q$FN+q$TN!=q$n)||max(abs(direct-q$net_benefit))>1e-12)stop("导出TP/FP与净获益不一致。")
      for(m in unique(b$model)){v<-b[b$model==m,];if(sum(v$n)!=unique(q$n)||sum(v$events)!=unique(q$events))stop("风险分层人数不守恒。")
        for(k in 1:3){a<-q[q$model==m&q$threshold==plan$risk_cutpoints[k],];if(nrow(a)!=1L||sum(v$n[v$band>k])!=a$alerts||sum(v$events[v$band>k])!=a$TP)stop("阈值触发和风险层边界不一致。")}}
      checks[[scope]]<-data.frame(cohort=scope,counts_conserved=TRUE,bands_conserved=TRUE,cutpoint_ties_verified=TRUE,max_NB_recalculation_error=max(abs(direct-q$net_benefit)))}
    csv_out(do.call(rbind,checks),"to_share/02_arithmetic_checks.csv")
    inadequate<-do.call(rbind,lapply(fixed_metrics,function(m){v<-fixed[is.finite(fixed[[m]])&fixed[[paste0(m,"_bootstrap_valid")]]<ceiling(plan$minimum_valid_fraction*plan$bootstrap_n),c("cohort","resampling","model","threshold")];if(nrow(v))cbind(metric=m,v)else NULL}))
    if(!is.null(inadequate)&&nrow(inadequate)){csv_out(inadequate,"to_share/09_insufficient_valid_intervals.csv");issue("REVIEW","classification uncertainty","部分分类指标有效重抽样不足90%；保留点估计，区间为NE。")}
    if(any(bands$status%in%c("NO_EVENTS_OBSERVED","NO_NONEVENTS_OBSERVED")))issue("NOTE","risk bands","部分风险层仅观察到一种结局；经验bootstrap区间可能退化，不能解释成总体风险确定为0或1。")
    if(plan$bootstrap_n<2000L)issue("REVIEW","bootstrap count","当前重抽样少于正式默认2000次，仅适合测试；正式稿请恢复默认运行。")
    current_module<<-"large-font figures and final source exports";make_visuals(curves,diffs,fixed,bands,coefficient)
    csv_out(data.frame(backend=pdf_backend,requested_family=cfg$PDF_FAMILY,figure_pointsize=20,table_pointsize=18),"to_share/08_graphics_audit.csv")
    if(!grepl("cairo",pdf_backend))issue("REVIEW","PDF fonts","当前为Helvetica回退；正式投稿前需嵌入字体，PNG可用于查看。本步数值不受影响。")
    mapping<-data.frame(output=c("Figure_4G_frozen_elastic_net_coefficients","Figure_S_Step20_DCA_full_frozen","Figure_S_Step20_DCA_common","Figure_S_Step20_DCA_full_range","Figure_S_Step20_paired_net_benefit","Figure_S_Step20_fixed_risk_bands","Figure_S_Step20_hospital_model_coefficients","Table_S_Step20_threshold_performance","Table_S_Step20_threshold_counts","Table_S_Step20_paired_net_benefit"),
      action=c("REPLACE original Figure 4G: frozen elastic-net coefficients replace unrelated XGBoost SHAP",rep("ADD supplementary; final numbering at manuscript assembly",9)),
      reviewer=c("Original Figure4G model mismatch; work package12; R3 minor3 full analysis code",rep("R1 major3/minor1; R2 minor1; R3 major4/8/9; external updating evaluation",9)))
    csv_out(mapping,"to_share/11_figure_table_replacement_map.csv")
    csv_out(data.frame(task=c("Step20 external fixed thresholds/DCA and coefficient exports","Web prediction consistency","Manuscript, figures and 45 reply units"),status=c("COMPLETED_FOR_REVIEW",rep("PENDING",2))),"to_share/12_revision_progress.csv")
    txt_out(c("Step20：外部固定阈值、净获益和正式模型系数导出。无重新训练、重新校准或外部阈值寻优。",
      "使用Step13院内死亡伴随模型；不把Step12的30天模型与eICU院内死亡概率混为一谈。",
      "完整eICU冻结队列报告原始与Step14医院分组交叉拟合截距更新的A-D；APACHE IV/IVa只在Step18共同可比较患者中比较。",
      "Step14全量eICU拟合的部署更新对象未用于本步评价；斜率更新及患者分折方案的既有结果仍在Step14，不按性能挑选更新方法。",
      "阈值沿用Step11：曲线1%-50%，重点展示1%-30%；固定报告5%、15%、30%；p>=阈值触发复核。它们是展示阈值，不是经临床验证的治疗切点。",
      "直接计算TP、FP、FN、TN、敏感度、特异度、PPV、NPV、提醒比例和净获益；不按患病率换算外部PPV。零分母为NE，空风险层仍保留。",
      "净获益=TP/N-FP/N*t/(1-t)；评估所有人为事件率减去非事件率乘阈值赔率，评估无人为0。负净获益完整保留。",
      "医院整体配对bootstrap为主，患者配对bootstrap为敏感性；每次所有模型、阈值及比较共用相同患者。医院被抽中时保留其全部患者，重复抽中按重复次数计入，分母随抽样变化。",
      "默认每种方式2000次，95%逐点百分位区间；分类指标要求至少90%有效次数。区间不含重新拟合MIMIC或校准模型的不确定性，不是同时置信带。",
      "相邻阈值的显著性不用于寻找最优切点。各风险层的患者比例、实测死亡率和均值预测均分别保留；零事件层的经验bootstrap区间可能退化。",
      "临床动作仅是假设增加病情复核/监测评估；DCA不证明减少死亡、不支持直接选择PCI/MCS，也不表示模型已经可部署。APACHE原生模型与T48工具信息时点不同。",
      "图的左面板显示包括assess-all的完整纵轴，右面板展示模型细节并注明assess-all位于左图；1%-50%全范围另存，避免隐藏负值。",
      "Figure4G改为Step7正式30天弹性网C模型冻结系数，另导出Step13院内死亡伴随模型；不再用旧XGBoost SHAP解释当前模型。",
      "图显示绝对值最大的12个非零系数；完整CSV保留所有A-D参数、零系数、原标准化参数及原设计单位系数。每单位为训练设计列1SD，包含二元/缺失指示列，不能当作临床单位OR或因果贡献。",
      "PDF图20pt，表18pt；PNG300dpi，三线表另存RTF。输出只写修改/Step20，保留有效本步缓存和未登记的用户文件；不会覆盖Step7-19。",
      "反馈仅汇总表和图；不含患者ID、逐人预测或参考分布。请回传Step20_feedback.zip。",
      "后续收尾为网页一致性、手稿/图注/审稿回复；Step19比例风险限制须写入HR图注，Step18旧PDF字体在最终排版时处理。"),"to_share/00_READ_ME_FIRST.txt")
    txt_out(c("Vickers AJ, Elkin EB. Decision curve analysis. Med Decis Making. 2006;26:565-574. doi:10.1177/0272989X06295361.",
      "https://www.danieldsjoberg.com/dcurves/reference/dca.html",
      "Vickers AJ, van Calster B, Steyerberg EW. Reporting and Interpreting Decision Curve Analysis: A Guide for Investigators. Eur Urol. 2018;74:796-804. doi:10.1016/j.eururo.2018.08.038."),"to_share/10_method_sources.txt")
    if(!identical(h,setNames(unname(tools::md5sum(paths)),names(paths))))stop("本步运行期间上游文件发生改变。")
    status<<-"COMPLETED_FOR_REVIEW"
    invisible(TRUE)}

  preflight<-function(){
    y<-c(1L,0L,1L,0L,1L,0L,0L,1L)
    v<-c(.05,.05,.15,.15,.30,.30,0,1);p<-cbind(A_raw=rep(.2,8),C_raw=v,D_raw=v,C_updated=v/2)
    a<-decision_points(y,p);q<-a$points
    for(m in colnames(p))for(t in plan$thresholds){z<-q[q$model==m&q$threshold==t,];hit<-p[,m]>=t;tp<-sum(y[hit]);fp<-sum(1-y[hit])
      stopifnot(nrow(z)==1L,z$TP==tp,z$FP==fp,z$FN==sum(y)-tp,z$TN==sum(1-y)-fp,abs(z$net_benefit-(tp-fp*t/(1-t))/length(y))<1e-14)}
    stopifnot(all(q$net_benefit[q$model=="None"]==0),all(is.na(q$PPV[q$model=="None"])),all(is.na(q$NPV[q$model=="All"])))
    b<-a$bands[a$bands$model=="C_raw",];stopifnot(identical(b$n,c(1,2,2,3)),identical(b$events,c(0,1,1,2)))
    edge<-cbind(C_raw=c(.05-.Machine$double.eps,.05,.15-.Machine$double.eps,.15,.30-.Machine$double.eps,.30))
    eb<-decision_points(rep(0:1,3),edge)$bands;stopifnot(identical(eb$n,c(1,2,2,1)))
    zero<-decision_points(y,cbind(C_raw=rep(0,8)));stopifnot(sum(zero$bands$status=="EMPTY_BAND")==3L,all(is.na(zero$points$PPV[zero$points$model=="C_raw"])))
    for(yy in list(rep(0L,8),rep(1L,8))){out<-evaluate_decisions(yy,p,B=7L,keep_draws=TRUE);stopifnot(all(is.finite(out$bootstrap_nb)))}
    h<-c("a","a","b","b","b","c","d","d");seed<-491L;fit<-evaluate_decisions(y,p,hospital=h,B=23L,seed=seed,keep_draws=TRUE)
    set.seed(seed);hosp<-unique(h);selected<-sample.int(length(hosp),length(hosp),replace=TRUE);ii<-unlist(lapply(selected,function(j)which(h==hosp[j])),use.names=FALSE)
    for(m in colnames(p))for(k in seq_along(plan$thresholds)){t<-plan$thresholds[k];hit<-p[ii,m]>=t;oracle<-(sum(y[ii][hit])-sum(1-y[ii][hit])*t/(1-t))/length(ii)
      stopifnot(abs(oracle-fit$bootstrap_nb[1,k,m])<1e-14)}
    dd<-fit$differences[fit$differences$comparison=="D_raw minus C_raw",];stopifnot(all(dd$delta_nb==0),all(dd$lower==0),all(dd$upper==0),fit$draw_counts[1,1]==length(ii))
    for(bad in list(p*NA_real_,p+1)){stopifnot(inherits(try(decision_points(y,bad),silent=TRUE),"try-error"))}
    stopifnot(inherits(try(decision_points(integer(),p[FALSE,,drop=FALSE]),silent=TRUE),"try-error"),inherits(try(evaluate_decisions(y,p,rep("a",8),B=5L),silent=TRUE),"try-error"))
    m<-list(preprocessor=list(design_columns=c("x","x__NA"),center=c(x=2,x__NA=.2),scale=c(x=3,x__NA=.4),training_n=5133L),
      coefficients=c("(Intercept)"=-2,x=.6,x__NA=-.2),alpha=.5,lambda=.01)
    obj<-list(version="Step7_v1",endpoint="30_day_all_cause_death_after_T48",training_n=5133L,models=setNames(rep(list(m),4),arms))
    co<-coefficient_export(obj,"Step7",obj$version,obj$endpoint);a<-co[co$model=="C",]
    x<-c(5,1);eta1<-m$coefficients[1]+sum(m$coefficients[-1]*(x-m$preprocessor$center)/m$preprocessor$scale);eta2<-a$coefficient_original_design[1]+sum(x*a$coefficient_original_design[-1])
    stopifnot(abs(eta1-eta2)<1e-14,all(a$missingness_indicator==c(FALSE,FALSE,TRUE)))
    invisible(TRUE)}

  if(isTRUE(getOption("ami_step20.functions_only",FALSE)))return(invisible(list(main=main,preflight=preflight,decision_points=decision_points,evaluate_decisions=evaluate_decisions,load_inputs=load_inputs,coefficient_export=coefficient_export,make_visuals=make_visuals)))
  status<-"FAILED";current_module<-"input verification";failure_message<-NULL
  tryCatch(withCallingHandlers(main(),warning=function(w){add_warning(conditionMessage(w),current_module);invokeRestart("muffleWarning")}),error=function(err){
    status<<-"FAILED";failure_message<<-conditionMessage(err);issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step20_error_detail.txt")})
  flush_warnings()
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step20",
    "COMPLETED_FOR_REVIEW表示外部阈值、决策曲线及冻结模型系数已导出；不表示已证明临床干预获益或模型可部署。",
    "FAILED表示本步未完整完成；检查日志。上游队列和预测保持原样。"),"to_share/Step20_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step20_sessionInfo.txt");logmsg(paste0("Step20状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step20_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share)else{
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step20_feedback.zip"),"\n",sep="")else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
