# AMI-EDRS revision | Step16: documented comfort-only restriction with locked predictions
# source(file.choose(), encoding="UTF-8")
# Fixed filenames; replaces owned results. Patient-level data and frozen references stay local_only.
local({
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MAKE_PNG=TRUE,MAKE_ZIP=TRUE,BOOTSTRAP_N=2000L,SEED=2026L)
  cfg<-utils::modifyList(cfg,getOption("ami_step16.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step16")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step16输出目录。")
  if (is_link(out)) stop("Step16输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP16")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step16目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP16_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP16_MANAGED_OUTPUT")) {
    stop("Step16输出目录的归属标记不正确。")
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
        if (unlink(p) != 0L) stop("不能覆盖旧Step16输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step16_run_log.txt")
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
    txt_out(if(nrow(z))unlist(Map(function(a,b)c(a,b,""),z$scope,z$message))else"No warnings in Step16.","local_only/Step16_warning_detail.txt")
    a<-if(nrow(z))aggregate(list(n=rep(1L,nrow(z))),list(scope=z$scope),sum)else data.frame(scope=character(),n=integer())
    csv_out(a,"to_share/09_warning_summary.csv")
  }
  plan<-list(version="Step16_v1",landmark="T48",bootstrap_n=as.integer(cfg$BOOTSTRAP_N),seed=as.integer(cfg$SEED),epsilon=1e-12,
    logloss_epsilon_by_dataset=c(MIMIC_30d=1e-15,MIMIC_hospital=1e-12,eICU_raw=1e-12,eICU_updated=1e-12),
    selection="Exclude any explicit comfort-only documentation with both chart and entry times in [0,48] hours",
    definition_timing="Rule fixed after documentation audit and before evaluating restricted-cohort performance",
    estimand="Performance of the existing locked predictions in a restricted evaluation population; no model redevelopment",
    uncertainty="Paired patient/whole-hospital bootstrap of fixed predictions; excludes model and calibration refitting uncertainty",
    exclusions="DNR/DNI alone, late-only records, unknown values and same-time conflicts do not trigger exclusion",
    caveat="Documented-ever CMO does not establish active CMO at T48; incomplete capture remains")
  if(!is.finite(plan$bootstrap_n)||plan$bootstrap_n<20L||!is.finite(plan$seed))stop("BOOTSTRAP_N应为至少20的整数，正式分析默认2000；SEED必须有效。")
  arms<-c("A","B","C","D");metric_names<-c("AUROC","AUPRC","Brier","LogLoss","Mean_prediction","Event_fraction")
  pairs<-data.frame(first=c("B","C","D"),second=c("A","A","C"),comparison=c("B-A","C-A","D-C"))
  dataset_labels<-c(MIMIC_30d="MIMIC: 30-day mortality / nested OOF",MIMIC_hospital="MIMIC: hospital mortality / nested OOF",
    eICU_raw="eICU: hospital mortality / frozen raw",eICU_updated="eICU: hospital mortality / cross-fitted update")
  binary<-function(x){if(is.factor(x))x<-as.character(x);if(is.logical(x))x<-as.integer(x);z<-as.character(x)
    if(anyNA(z)||any(!z%in%c("0","1")))stop("结局或入选标志不是完整0/1；不自动填补。");as.integer(z)}
  need<-function(x,cols,label){if(!is.data.frame(x)||!all(cols%in%names(x)))stop(label,"字段不完整：",paste(setdiff(cols,names(x)),collapse=","))}
  keys<-function(x)paste(x$source,as.character(x$stay_id),sep=":")
  check_keys<-function(x,label){need(x,c("source","stay_id"),label);if(anyNA(x$source)||anyNA(x$stay_id)||any(!nzchar(as.character(x$stay_id)))||anyDuplicated(keys(x)))stop(label,"患者标识缺失或重复。")}
  hash_object<-function(x){p<-tempfile();on.exit(unlink(p));saveRDS(x,p,version=2);unname(tools::md5sum(p))}
  cache_result<-function(rel,key,fun){
    p<-safe_path(rel)
    if(file.exists(p)){z<-tryCatch(readRDS(p),error=function(e)NULL)
      if(is.list(z)&&identical(z$key,key)&&identical(z$hash,hash_object(z$value))){reg(rel);logmsg(paste0("复用本步缓存：",basename(rel)));return(z$value)}}
    value<-fun();tmp<-reg(paste0(rel,".pending"));saveRDS(list(key=key,hash=hash_object(value),value=value),tmp,compress="gzip")
    if(!file.rename(tmp,reg(rel)))stop("Step16缓存写入失败。");value
  }
  metric_setup<-function(y,p,epsilon=plan$epsilon){
    if(length(epsilon)!=1L||!is.finite(epsilon)||epsilon<=0||epsilon>=.5)stop("LogLoss数值截断参数无效。")
    if(!length(y)||anyNA(y)||any(!y%in%0:1)||!is.matrix(p)||!is.numeric(p)||nrow(p)!=length(y)||!ncol(p)||any(!is.finite(p))||any(p<0|p>1))stop("无效结局或概率；不静默删除患者。")
    lapply(seq_len(ncol(p)),function(j){o<-order(p[,j]);pp<-p[o,j];g<-cumsum(c(TRUE,diff(pp)!=0));ends<-c(which(diff(g)!=0),length(o))
      pc<-pmax(epsilon,pmin(1-epsilon,p[,j]));list(order=o,ends=ends,error=(p[,j]-y)^2,loss=-(y*log(pc)+(1-y)*log1p(-pc)))})
  }
  weighted_metrics<-function(y,p,w,setup=metric_setup(y,p)){
    if(length(w)!=length(y)||any(!is.finite(w)|w<0))stop("无效重抽样权重。")
    ans<-matrix(NA_real_,ncol(p),6,dimnames=list(colnames(p),metric_names));nn<-sum(w)
    if(nn<=0)return(ans)
    pos<-sum(w*y);neg<-nn-pos
    for(j in seq_len(ncol(p))){z<-setup[[j]];o<-z$order;ep<-z$ends
      gp<-diff(c(0,cumsum(w[o]*y[o])[ep]));gn<-diff(c(0,cumsum(w[o]*(1-y[o]))[ep]))
      if(pos>0&&neg>0)ans[j,"AUROC"]<-sum(gp*(cumsum(gn)-.5*gn))/(pos*neg)
      if(pos>0){tp<-rev(cumsum(rev(gp)));tot<-rev(cumsum(rev(gp+gn)));ans[j,"AUPRC"]<-sum(ifelse(tot>0,gp*tp/pmax(tot,1),0))/pos}
      ans[j,3:6]<-c(sum(w*z$error)/nn,sum(w*z$loss)/nn,sum(w*p[,j])/nn,pos/nn)
    };ans
  }
  interval<-function(x,B){z<-x[is.finite(x)];if(length(z)<.9*B)return(c(NA_real_,NA_real_));as.numeric(quantile(z,c(.025,.975),names=FALSE))}
  evaluate_restriction<-function(y,p,keep,hospital=NULL,B=plan$bootstrap_n,seed=plan$seed,epsilon=plan$epsilon){
    if(length(keep)!=length(y)||anyNA(keep)||!is.logical(keep)||!any(keep))stop("限制队列为空或标志无效。")
    setup<-metric_setup(y,p,epsilon);subsets<-c("full","without_documented_CMO")
    point<-array(NA_real_,c(ncol(p),6,2),dimnames=list(colnames(p),metric_names,subsets))
    point[,,1]<-weighted_metrics(y,p,rep(1,length(y)),setup);point[,,2]<-weighted_metrics(y,p,as.integer(keep),setup)
    mode<-if(is.null(hospital))"patient"else"hospital_cluster"
    if(!is.null(hospital)){if(length(hospital)!=length(y)||anyNA(hospital)||any(!nzchar(as.character(hospital)))||length(unique(hospital))<2)stop("医院重抽样需要完整医院ID且至少两家医院。")
      hi<-match(hospital,unique(hospital));H<-max(hi)}
    arr<-array(NA_real_,c(B,ncol(p),6,2),dimnames=list(NULL,colnames(p),metric_names,subsets));set.seed(seed)
    draw_counts<-matrix(0,B,4,dimnames=list(NULL,c("full_n","full_events","restricted_n","restricted_events")))
    for(i in seq_len(B)){
      w<-if(is.null(hospital))tabulate(sample.int(length(y),length(y),replace=TRUE),length(y))else tabulate(sample.int(H,H,replace=TRUE),H)[hi]
      arr[i,,,1]<-weighted_metrics(y,p,w,setup);arr[i,,,2]<-weighted_metrics(y,p,w*keep,setup)
      draw_counts[i,]<-c(sum(w),sum(w*y),sum(w*keep),sum(w*keep*y))
      if(i%%500L==0L)logmsg(paste0(mode," 配对重抽样：",i,"/",B,"。"))
    }
    met<-changes<-inc<-incchanges<-list()
    row<-function(est,v,...){ci<-interval(v,B);data.frame(...,estimate=est,lower=ci[1],upper=ci[2],bootstrap_valid=sum(is.finite(v)),bootstrap_requested=B,check.names=FALSE)}
    for(a in colnames(p))for(m in metric_names){
      for(s in subsets)met[[length(met)+1L]]<-row(point[a,m,s],arr[,a,m,s],resampling=mode,subset=s,model=a,metric=m)
      changes[[length(changes)+1L]]<-row(point[a,m,2]-point[a,m,1],arr[,a,m,2]-arr[,a,m,1],resampling=mode,model=a,metric=m,comparison="restricted_minus_full")
    }
    for(j in seq_len(nrow(pairs)))for(m in metric_names[1:4]){
      a<-pairs$first[j];b<-pairs$second[j]
      for(s in subsets)inc[[length(inc)+1L]]<-row(point[a,m,s]-point[b,m,s],arr[,a,m,s]-arr[,b,m,s],resampling=mode,subset=s,comparison=pairs$comparison[j],metric=m)
      incchanges[[length(incchanges)+1L]]<-row((point[a,m,2]-point[b,m,2])-(point[a,m,1]-point[b,m,1]),
        (arr[,a,m,2]-arr[,b,m,2])-(arr[,a,m,1]-arr[,b,m,1]),resampling=mode,comparison=pairs$comparison[j],metric=m,contrast="restricted_increment_minus_full_increment")
    }
    list(metrics=do.call(rbind,met),changes=do.call(rbind,changes),increments=do.call(rbind,inc),increment_changes=do.call(rbind,incchanges),bootstrap=arr,draw_counts=draw_counts,seed=seed)
  }
  build_selection<-function(care,roster){
    check_keys(roster,"Step15冻结名单");if(!identical(care$version,"Step15_mapping_v2"))stop("需要Step15-2修正后的照护对象。")
    f<-care$patient_documentation;need(f,c("source","stay_id","concept","source_evaluated","documented_by_T48"),"照护标志")
    f<-f[f$concept=="comfort_only",,drop=FALSE];check_keys(f,"comfort-only标志")
    if(!setequal(keys(f),keys(roster)))stop("照护标志与冻结名单不匹配。")
    f<-f[match(keys(roster),keys(f)),,drop=FALSE]
    if(anyNA(f$source_evaluated)||!all(f$source_evaluated))stop("某队列没有可评估的舒适照护来源。")
    flag<-binary(f$documented_by_T48);e<-care$evidence
    need(e,c("source","stay_id","concept","chart_hour","entry_hour"),"照护证据")
    if(any(!keys(e)%in%keys(roster)))stop("照护证据含名单外患者。")
    time_ok<-is.finite(e$chart_hour)&is.finite(e$entry_hour)&e$chart_hour>=0&e$entry_hour>=0&e$chart_hour<=48&e$entry_hour<=48
    ids<-unique(keys(e)[e$concept=="comfort_only"&time_ok])
    if(!identical(flag,as.integer(keys(roster)%in%ids)))stop("舒适照护标志不能从原时间证据复现。")
    last<-care$last_code;check_keys(last,"最后状态");if(!setequal(keys(last),keys(roster)))stop("最后状态名单不匹配。")
    last<-last[match(keys(roster),keys(last)),,drop=FALSE]
    r<-care$records;need(r,c("source","stay_id","chart_hour","entry_hour","code_value"),"候选照护记录")
    tok<-is.finite(r$chart_hour)&is.finite(r$entry_hour)&r$chart_hour>=0&r$entry_hour>=0&r$chart_hour<=48&r$entry_hour<=48
    unknown<-unique(keys(r)[tok&!is.na(r$code_value)&r$code_value=="unmapped"])
    data.frame(source=roster$source,stay_id=as.character(roster$stay_id),exclude_documented_CMO=as.logical(flag),
      unresolved_code_documentation_by_T48=keys(roster)%in%unknown,latest_code_T48=last$latest_code_T48,stringsAsFactors=FALSE)
  }
  align_bundle<-function(z,src,endpoint,roster,selection,label){
    need(z$metadata,c("patient_key","source","stay_id","landmark"),label);check_keys(z$metadata,label)
    if(anyNA(z$metadata$patient_key)||anyDuplicated(z$metadata$patient_key)||any(!nzchar(as.character(z$metadata$patient_key)))||anyNA(z$metadata$landmark)||any(z$metadata$source!=src)||any(z$metadata$landmark!="T48"))stop(label,"来源或T48标志不符。")
    need(z$outcomes,c("event_primary","endpoint"),label)
    if(nrow(z$outcomes)!=nrow(z$metadata)||anyNA(z$outcomes$endpoint)||any(z$outcomes$endpoint!=endpoint))stop(label,"结局定义不符。")
    rr<-roster[roster$source==src,,drop=FALSE];ss<-selection[selection$source==src,,drop=FALSE]
    if(!setequal(keys(z$metadata),keys(rr)))stop(label,"预测患者与Step15名单不一致。")
    ii<-match(keys(z$metadata),keys(rr));jj<-match(keys(z$metadata),keys(ss));y<-binary(z$outcomes$event_primary)
    reference<-if(grepl("30_day",endpoint))rr$death_day30[ii]else rr$death_hospital[ii]
    if(!identical(y,binary(reference)))stop(label,"患者级结局与Step15不一致。")
    p<-z$prediction;if(!identical(colnames(p),arms))stop(label,"模型列应为A/B/C/D。")
    metric_setup(y,p)
    list(source=src,endpoint=endpoint,metadata=z$metadata,y=y,p=p,keep=!ss$exclude_documented_CMO[jj],selection=ss[jj,,drop=FALSE])
  }
  roundtrip<-function(dataset,d,reference){
    point<-weighted_metrics(d$y,d$p,rep(1,length(d$y)),metric_setup(d$y,d$p,d$epsilon));z<-reference
    if(dataset=="MIMIC_30d")z$metric[z$metric=="AUPRC_AP"]<-"AUPRC"
    if(dataset%in%c("MIMIC_hospital","eICU_raw"))z<-z[z$cohort==if(dataset=="MIMIC_hospital")"MIMIC_nested_OOF"else"eICU_frozen",,drop=FALSE]
    if(dataset%in%c("MIMIC_hospital","eICU_raw"))z<-z[z$resampling=="patient",,drop=FALSE]
    if(dataset=="eICU_updated")z<-z[z$scheme=="hospital_blocked"&z$resampling=="hospital_cluster"&z$method=="intercept",,drop=FALSE]
    rows<-list()
    for(a in arms)for(m in metric_names[1:4]){
      old<-z$estimate[z$model==a&z$metric==m]
      if(length(old)!=1L||!is.finite(old)||abs(old-point[a,m])>1e-10)stop(dataset,"无法复现既有全队列指标：",a,"/",m)
      rows[[length(rows)+1L]]<-data.frame(dataset,model=a,metric=m,previous=old,recomputed=point[a,m],absolute_difference=abs(old-point[a,m]))
    };do.call(rbind,rows)
  }

  preflight<-function(){
    stopifnot(identical(binary(factor(c("0","1"),levels=c("1","0"))),0:1),inherits(try(binary(c(0,NA)),silent=TRUE),"try-error"))
    y<-c(1L,0L,1L);p<-matrix(rep(c(.5,.5,.2),4),3,4,dimnames=list(NULL,arms))
    mm<-weighted_metrics(y,p,rep(1,3));stopifnot(abs(mm[1,"AUROC"]-.25)<1e-12,abs(mm[1,"AUPRC"]-7/12)<1e-12)
    yp<-c(1L,0L);px<-matrix(rep(c(0,1),4),2,4,dimnames=list(NULL,arms))
    for(ep in c(1e-15,1e-12)){
      mx<-weighted_metrics(yp,px,c(1,1),metric_setup(yp,px,ep));expected_loss<-( -log(ep)-log1p(-(1-ep)) )/2
      stopifnot(abs(mx[1,"LogLoss"]-expected_loss)<1e-12,mx[1,"Brier"]==1,mx[1,"AUROC"]==0)
    }
    # Independent pairwise AUC oracle and threshold-based AP oracle with integer weights.
    yy<-c(0L,1L,0L,1L,0L,1L);pp<-matrix(rep(c(.2,.2,.5,.8,.8,.9),4),6,4,dimnames=list(NULL,arms));w<-c(2,1,0,2,1,3)
    idx<-rep(seq_along(yy),w);a<-pp[idx,1][yy[idx]==1];b<-pp[idx,1][yy[idx]==0]
    auc<-mean(outer(a,b,">")+.5*outer(a,b,"=="));q<-sort(unique(pp[idx,1]),decreasing=TRUE);prev<-0;ap<-0
    for(t in q){sel<-pp[idx,1]>=t;rec<-sum(yy[idx][sel])/sum(yy[idx]);ap<-ap+(rec-prev)*mean(yy[idx][sel]);prev<-rec}
    z<-weighted_metrics(yy,pp,w);stopifnot(abs(z[1,"AUROC"]-auc)<1e-12,abs(z[1,"AUPRC"]-ap)<1e-12)
    k<-c(TRUE,TRUE,FALSE,TRUE,FALSE,TRUE);z1<-weighted_metrics(yy,pp,w*k);z2<-weighted_metrics(yy[k],pp[k,,drop=FALSE],w[k]);stopifnot(max(abs(z1-z2),na.rm=TRUE)<1e-12)
    same<-evaluate_restriction(yy,pp,rep(TRUE,6),B=20L,seed=1L)
    stopifnot(all(same$changes$estimate==0),all(same$changes$lower==0|is.na(same$changes$lower)),all(same$increments$estimate==0))
    rr<-data.frame(source="MIMIC",stay_id=as.character(1:4),death_hospital=c(1L,0L,1L,0L),death_day30=c(1L,0L,1L,0L),in_index_icu=1L)
    ee<-data.frame(source="MIMIC",stay_id=c("1","2","3","4"),concept=c("comfort_only","dnr","comfort_only","comfort_only"),chart_hour=c(48,4,49,10),entry_hour=c(48,4,49,50))
    records<-ee;records$code_value<-c("comfort_only","dnr","comfort_only","unmapped")
    ca<-list(version="Step15_mapping_v2",patient_documentation=data.frame(source="MIMIC",stay_id=as.character(1:4),concept="comfort_only",source_evaluated=TRUE,documented_by_T48=c(1L,0L,0L,0L)),
      evidence=ee,records=records,last_code=data.frame(source="MIMIC",stay_id=as.character(1:4),latest_code_T48=c("comfort_only","dnr","full_code","unmapped")))
    s<-build_selection(ca,rr);stopifnot(identical(s$exclude_documented_CMO,c(TRUE,FALSE,FALSE,FALSE)))
    rr$death_hospital<-1L-rr$death_hospital;rr$death_day30<-1L-rr$death_day30;stopifnot(identical(s,build_selection(ca,rr)))
    bad<-ca;bad$patient_documentation$stay_id[2]<-"1";stopifnot(inherits(try(build_selection(bad,rr),silent=TRUE),"try-error"))
    stopifnot(inherits(try(evaluate_restriction(yy,pp,rep(FALSE,6),B=20L),silent=TRUE),"try-error"))
    TRUE
  }

  check_status<-function(step){p<-file.path(cfg$OUTPUT_PARENT,step,"to_share",paste0(step,"_STATUS.txt"))
    if(!file.exists(p)||!"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(p,warn=FALSE))stop("请先完成",step,"。")}
  main<-function(){
    current_module<<-"preflight";logmsg("Step16预检：时间窗排除、患者键匹配、同分指标、配对队列重抽样、结局缺失与空限制集。")
    preflight();txt_out("PASS: exact T48 documentation mask; no DNR-only or late-only exclusion; outcome-blind selection; key alignment; tied metrics; paired subset and cluster bootstrap.","to_share/02_preflight.txt")
    for(st in c("Step7","Step13","Step14","Step15"))check_status(st)
    paths<-c(care="Step15/local_only/Step15_care_documentation.rds",roster="Step15/local_only/frozen_T48_roster.rds",
      care_summary="Step15/to_share/04_care_documentation_summary.csv",care_inputs="Step15/to_share/01_inputs.csv",
      m30="Step7/local_only/MIMIC_T48_nested_OOF.rds",mh="Step13/local_only/MIMIC_T48_hospital_nested_OOF.rds",
      eh="Step13/local_only/eICU_T48_hospital_frozen_predictions.rds",eu="Step14/local_only/eICU_crossfitted_recalibration.rds",
      metric7="Step7/to_share/05_OOF_metrics.csv",metric13="Step13/to_share/05_hospital_metrics.csv",metric14="Step14/to_share/04_crossfit_metrics.csv")
    paths<-setNames(file.path(cfg$OUTPUT_PARENT,paths),names(paths));if(!all(file.exists(paths)))stop("缺少上游本地结果：",paste(names(paths)[!file.exists(paths)],collapse=","))
    h<-setNames(unname(tools::md5sum(paths)),names(paths));csv_out(data.frame(file=unname(paths),md5=unname(h)),"to_share/01_inputs.csv")
    current_module<<-"freeze documentation-based selection"
    roster<-readRDS(paths["roster"]);check_keys(roster,"冻结名单")
    need(roster,c("death_hospital","death_day30","in_index_icu"),"冻结结局")
    for(src in c("MIMIC","eICU")){rr<-roster[roster$source==src,,drop=FALSE]
      if(nrow(rr)!=c(MIMIC=5133,eICU=5284)[src]||sum(binary(rr$death_hospital))!=c(MIMIC=461,eICU=219)[src])stop("冻结队列人数/住院结局不符：",src)}
    if(nrow(roster)!=10417L||sum(binary(roster$death_day30[roster$source=="MIMIC"]))!=630L||any(!is.na(roster$death_day30[roster$source=="eICU"])))stop("冻结30天终点不符；不能给eICU构造30天标签。")
    care<-readRDS(paths["care"]);input15<-read.csv(paths["care_inputs"],stringsAsFactors=FALSE)
    if(nrow(input15)!=1L||!identical(care$roster_hash,input15$object_md5[1]))stop("Step15名单指纹不符。")
    if(!file.exists(input15$file)||file.info(input15$file)$size!=input15$bytes||abs(as.numeric(file.info(input15$file)$mtime)-input15$mtime)>.01)stop("Step4冻结数据库已变化或缺失；请核对上游。")
    selection<-build_selection(care,roster)
    cs<-read.csv(paths["care_summary"],stringsAsFactors=FALSE)
    for(src in c("MIMIC","eICU")){
      z<-cs[cs$source==src&cs$concept=="comfort_only",,drop=FALSE]
      if(nrow(z)!=1L||!isTRUE(z$source_evaluated)||sum(selection$exclude_documented_CMO[selection$source==src])!=z$recorded_by_T48_n)stop("排除标志不能复现Step15汇总。")
    }
    # Selection is locked before reading model probabilities or assessing restricted performance.
    selection_hash<-hash_object(selection);saveRDS(list(version=plan$version,plan=plan,input_hash=h,selection=selection,selection_hash=selection_hash),reg("local_only/Step16_locked_selection.rds"),compress="gzip")
    txt_out(capture.output(dput(plan)),"to_share/02_analysis_plan.txt")
    csv_out(selection,"local_only/patient_selection.csv")
    current_module<<-"align locked prediction objects"
    m30<-readRDS(paths["m30"]);mh<-readRDS(paths["mh"]);eh<-readRDS(paths["eh"]);eu<-readRDS(paths["eu"])
    if(!identical(mh$version,"Step13_v1")||!identical(eh$version,"Step13_v1")||!identical(eu$version,"Step14_v1")||!identical(eh$model_hash,eu$source_model_hash))stop("住院模型或再校准对象来源不一致。")
    for(z in list(m30,mh))if(length(z$outer_fold)!=nrow(z$metadata)||anyNA(z$outer_fold)||!setequal(z$outer_fold,1:5))stop("内部评价必须使用完整嵌套OOF预测。")
    he<-"in_hospital_death_after_T48"
    data<-list(MIMIC_30d=align_bundle(m30,"MIMIC","30_day_all_cause_death_after_landmark",roster,selection,"Step7"),
      MIMIC_hospital=align_bundle(mh,"MIMIC",he,roster,selection,"Step13 MIMIC"),eICU_raw=align_bundle(eh,"eICU",he,roster,selection,"Step13 eICU"))
    need(eu$metadata,c("patient_key","source","stay_id","landmark","hospital_id"),"Step14");check_keys(eu$metadata,"Step14")
    if(!setequal(keys(eu$metadata),keys(eh$metadata)))stop("Step14与Step13患者集合不符。")
    eq<-eu$results$hospital_blocked
    if(!identical(eu$plan$primary_scheme,"hospital_blocked")||!identical(eu$plan$primary_update,"intercept")||is.null(eq))stop("Step14主方案不是按医院交叉拟合截距更新。")
    pp<-eq$prediction;expected<-paste(arms,"intercept",sep="_")
    if(!is.matrix(pp)||!all(c(expected,paste0(arms,"_raw"))%in%colnames(pp))||nrow(pp)!=nrow(eu$metadata))stop("缺少Step14折外更新预测。")
    need(eq$audit,c("shared_patients","shared_hospitals","fold"),"Step14分折审计")
    if(nrow(eq$audit)!=5L||anyNA(eq$audit$shared_patients)||anyNA(eq$audit$shared_hospitals)||any(eq$audit$shared_patients!=0|eq$audit$shared_hospitals!=0))stop("Step14校准折隔离核验不通过。")
    eidx<-match(keys(eu$metadata),keys(eh$metadata))
    for(a in arms)if(max(abs(pp[,paste0(a,"_raw")]-eh$prediction[eidx,a]))>1e-12)stop("Step14原始概率与Step13不匹配。")
    updated<-list(metadata=eu$metadata,outcomes=eu$outcomes,prediction=pp[,expected,drop=FALSE]);colnames(updated$prediction)<-arms
    data$eICU_updated<-align_bundle(updated,"eICU",he,roster,selection,"Step14截距更新")
    for(nm in c("eICU_raw","eICU_updated")){
      hid<-as.character(data[[nm]]$metadata$hospital_id)
      if(length(hid)!=5284L||anyNA(hid)||any(!nzchar(hid))||length(unique(hid))<5L)stop("eICU医院ID不完整。")
      data[[nm]]$hospital<-hid
    }
    if(!identical(as.character(eu$metadata$hospital_id),as.character(eh$metadata$hospital_id[eidx])))stop("Step14医院ID与Step13不符。")
    if(length(eq$fold)!=nrow(eu$metadata)||anyNA(eq$fold)||!setequal(eq$fold,1:5)||any(vapply(split(eq$fold,eu$metadata$hospital_id),function(x)length(unique(x))!=1L,logical(1))))stop("Step14折编号未按医院隔离。")
    for(nm in names(data))data[[nm]]$epsilon<-unname(plan$logloss_epsilon_by_dataset[nm])
    refs<-list(MIMIC_30d=read.csv(paths["metric7"]),MIMIC_hospital=read.csv(paths["metric13"]),eICU_raw=read.csv(paths["metric13"]),eICU_updated=read.csv(paths["metric14"]))
    check<-do.call(rbind,lapply(names(data),function(nm)roundtrip(nm,data[[nm]],refs[[nm]])));csv_out(check,"to_share/02_full_cohort_metric_roundtrip.csv")
    census<-do.call(rbind,lapply(names(data),function(nm){d<-data[[nm]];k<-d$keep
      data.frame(dataset=nm,source=d$source,endpoint=d$endpoint,full_n=length(k),full_events=sum(d$y),excluded_n=sum(!k),excluded_events=sum(d$y[!k]),
        retained_n=sum(k),retained_events=sum(d$y[k]),retained_unresolved_code_n=sum(d$selection$unresolved_code_documentation_by_T48[k]),
        retained_latest_conflict_n=sum(d$selection$latest_code_T48[k]=="same_time_conflict"),hospitals=if(d$source=="eICU")length(unique(d$hospital))else NA_integer_) }))
    csv_out(census,"to_share/03_cohort_restriction.csv")
    last<-as.data.frame(table(source=selection$source,excluded=selection$exclude_documented_CMO,latest_state=selection$latest_code_T48),stringsAsFactors=FALSE)
    csv_out(last[last$Freq>0,],"to_share/03_last_state_by_restriction.csv")
    current_module<<-"paired restriction performance";result<-list()
    for(nm in names(data)){
      d<-data[[nm]];logmsg(paste0(nm,"：排除",sum(!d$keep),"人，保留",sum(d$keep),"人；固定预测，",plan$bootstrap_n,"次配对重抽样。"))
      key<-hash_object(list(h,plan,nm,d$y,d$p,d$keep,d$hospital,functions=lapply(c("metric_setup","weighted_metrics","evaluate_restriction"),function(x)deparse(body(get(x))))))
      result[[nm]]<-cache_result(paste0("local_only/cache/",nm,"_bootstrap.rds"),key,function()evaluate_restriction(d$y,d$p,d$keep,d$hospital,seed=plan$seed+if(d$source=="MIMIC")100L else 200L,epsilon=d$epsilon))
    }
    tab<-function(field)do.call(rbind,lapply(names(result),function(nm)cbind(dataset=nm,endpoint=data[[nm]]$endpoint,result[[nm]][[field]])))
    metrics<-tab("metrics");changes<-tab("changes");increments<-tab("increments");increment_changes<-tab("increment_changes")
    csv_out(metrics,"to_share/04_performance_by_restriction.csv");csv_out(changes,"to_share/04_restriction_minus_full.csv")
    csv_out(increments,"to_share/05_model_increment_by_restriction.csv");csv_out(increment_changes,"to_share/05_change_in_model_increment.csv")
    if(any(!is.finite(metrics$lower))||any(!is.finite(changes$lower)))issue("NOTE","bootstrap","部分区间有效重复不足90%；保留NA，详见有效次数。")
    if(plan$bootstrap_n<1000L)issue("NOTE","bootstrap","本次重复次数少于1000；正式报告建议使用默认2000。")
    issue("LIMITATION","ascertainment","仅评价排除T48前明确comfort-only文档者；保留未确定状态及DNR/DNI-only患者，不能证明已排除全部姑息照护影响。")
    issue("LIMITATION","fixed prediction uncertainty","本步不重新训练模型或再校准。区间条件于既有预测，不覆盖模型开发及校准拟合的全部不确定性。")
    saveRDS(list(version=plan$version,plan=plan,input_hash=h,selection_hash=selection_hash,results=result,cohort=census),reg("local_only/Step16_sensitivity_results.rds"),compress="gzip")
    current_module<<-"large-font figures and tables";make_visuals(metrics,census)
    csv_out(data.frame(output=c("Figure_S_CMO_documentation_sensitivity","Table_S_CMO_documentation_sensitivity"),
      action="ADD supplementary sensitivity; retain full-cohort primary analyses",reviewer="R1 minor2; R2 minor2; R3 major11"),"to_share/11_figure_table_replacement_map.csv")
    txt_out(c("Step16：排除T48前明确舒适照护文档者后的固定预测敏感性分析。", 
      "入选规则在审阅Step15后、查看限制队列性能之前确定。不得称为研究开始前预注册的分析。",
      "排除依据是0-48小时内至少一次明确comfort-only记录，且记录和录入时间均在窗内；不等同于T48时仍在执行舒适照护。",
      "不因DNR/DNI、后续死亡、晚于T48的记录、未知值或同刻冲突直接排除患者。无法确认照护状态者保留，计数单列。",
      "MIMIC 30天结局使用Step7嵌套OOF；MIMIC住院结局使用Step13嵌套OOF；eICU住院结局使用Step13冻结预测及Step14按医院交叉拟合的截距更新预测。",
      "内部评价不用全量训练集拟合值，eICU更新评价不用全eICU拟合的部署对象。不构造eICU的30天死亡结局。",
      "A=基线；B=基线+分型；C=基线+连续动态；D=基线+连续动态+分型。所有A-D保留，不能按本次结果选模型。",
      "所有概率保持原样；本步仅改变评价患者集合，不删除上游数据，不重新开发模型或重新拟合校准。",
      "MIMIC按患者、eICU按整家医院重抽样。同一次抽样同时用于全队列、限制队列和所有模型；限制组以原样本权重乘入选标志计算。",
      "restricted_minus_full是评价人群变化对应的指标差，不是治疗效应。模型增量为B-A、C-A和D-C；另报限制前后增量之差。",
      "AUPRC为同分阈值合并后的average precision。AUROC/AP越高越好；Brier/LogLoss越低越好。指标使用相应评价人群的实际事件率。",
      "LogLoss保持原脚本数值约定：MIMIC 30天概率截断为[1e-15,1-1e-15]，住院结局为[1e-12,1-1e-12]；其他指标使用原概率。全队列指标逐项复核后才计算限制队列。",
      "95%区间是固定预测下的百分位bootstrap区间，不包含训练和校准重拟合不确定性。样本量改变亦影响AP及Brier，不作因果解释。",
      "明确记录者数量少，且来源不完整；结果稳定只能支持对这些已记录个案不敏感，不能写成已全面排除姑息照护或治疗限制偏倚。",
      "无需T7原始盘。使用已有本地RDS，输出在修改/Step16；不覆盖Step0-15。图基础18pt，表正文18pt。",
      "仅回传Step16_feedback.zip；患者标志与重抽样数组保留local_only。"),"to_share/00_READ_ME_FIRST.txt")
    txt_out(c("https://eicu.mit.edu/eicutables/careplangeneral/","https://mimic.mit.edu/docs/iv/modules/icu/chartevents.html"),"to_share/10_definition_sources.txt")
    if(!identical(selection_hash,hash_object(selection))||!identical(h,setNames(unname(tools::md5sum(paths)),names(paths))))stop("运行期间上游输入或排除名单发生变化。")
    status<<-"COMPLETED_FOR_REVIEW"
  }

  open_pdf<-function(path,w,h)grDevices::pdf(path,width=w,height=h,family="Helvetica",onefile=TRUE,useDingbats=FALSE,pointsize=18)
  check_pdf<-function(p,n){b<-as.integer(readBin(p,"raw",n=file.info(p)$size));b[b<32L|b>126L]<-32L
    m<-gregexpr("/Type[[:space:]]*/Page([^sA-Za-z]|$)",rawToChar(as.raw(b)),perl=TRUE)[[1]]
    actual<-if(m[1]<0)0L else length(m);if(actual!=n)stop("PDF页数不符：",basename(p),"。")}
  make_visuals<-function(metrics,census){
    subsets<-c("full","without_documented_CMO");cols<-c(full="#7A8797",without_documented_CMO="#4F7FC4")
    draw<-function(nm){
      z<-metrics[metrics$dataset==nm,,drop=FALSE];c<-census[census$dataset==nm,,drop=FALSE]
      par(mfrow=c(2,2),mar=c(4,3.8,2.5,1.5),oma=c(3,0,5,0),cex=1,cex.axis=1,cex.lab=1,las=1,family="sans")
      for(m in metric_names[1:4]){
        a<-z[z$metric==m,,drop=FALSE];v<-c(a$lower,a$upper,a$estimate);v<-v[is.finite(v)]
        rr<-if(length(v))range(v)else c(0,1);pad<-max(diff(rr)*.15,switch(m,AUROC=.015,AUPRC=.015,Brier=.003,LogLoss=.01))
        lim<-c(max(0,rr[1]-pad),rr[2]+pad);if(m%in%c("AUROC","AUPRC","Brier"))lim[2]<-min(1,lim[2])
        if(diff(lim)<=0)lim<-c(0,1)
        plot(NA,xlim=lim,ylim=c(.5,4.6),xlab=if(m=="AUPRC")"Average precision"else m,ylab="",yaxt="n",bty="n",xaxs="i")
        abline(h=1:4,col="#ECEFF2",lwd=.7);axis(2,at=4:1,labels=arms,tick=FALSE)
        for(s in subsets){b<-a[a$subset==s,,drop=FALSE];b<-b[match(arms,b$model),];y<-4:1+if(s=="full").12 else -.12
          ok<-is.finite(b$lower)&is.finite(b$upper);segments(b$lower[ok],y[ok],b$upper[ok],y[ok],col=cols[s],lwd=2)
          points(b$estimate,y,pch=if(s=="full")1 else 16,col=cols[s],cex=1.15,lwd=1.5)}
        title(if(m%in%c("AUROC","AUPRC"))"Higher is better"else"Lower is better",cex.main=1,font.main=1)
      }
      mtext(dataset_labels[nm],side=3,outer=TRUE,line=3.1,cex=1.15,font=2)
      mtext(sprintf("Full: n=%s, deaths=%s   |   Restricted: n=%s, deaths=%s",c$full_n,c$full_events,c$retained_n,c$retained_events),side=3,outer=TRUE,line=1.8,cex=1)
      mtext("Open: full cohort",side=3,outer=TRUE,line=.4,at=.30,col=cols[1],cex=1)
      mtext("Filled: without documented CMO",side=3,outer=TRUE,line=.4,at=.70,col=cols[2],cex=1)
      mtext("95% paired bootstrap intervals; locked predictions. Axes differ by metric and page.",side=1,outer=TRUE,line=.2,cex=1)
      mtext("CMO: explicit comfort-only documentation by T48; no record does not establish absence.",side=1,outer=TRUE,line=1.5,cex=1)
    }
    fig<-reg("to_share/Figure_S_CMO_documentation_sensitivity.pdf");open_pdf(fig,15,10)
    tryCatch(for(nm in names(dataset_labels))draw(nm),finally=grDevices::dev.off());check_pdf(fig,4L)
    if(isTRUE(cfg$MAKE_PNG))for(nm in names(dataset_labels)){
      grDevices::png(reg(paste0("to_share/Figure_S_CMO_",nm,".png")),width=15,height=10,units="in",res=300,pointsize=18)
      tryCatch(draw(nm),finally=grDevices::dev.off())}
    tabs<-list();ci<-function(x)ifelse(is.finite(x$lower)&is.finite(x$upper),sprintf("%.3f [%.3f, %.3f]",x$estimate,x$lower,x$upper),sprintf("%.3f [CI unavailable]",x$estimate))
    for(nm in names(dataset_labels)){
      rows<-list()
      for(m in metric_names[1:4])for(a in arms){z<-metrics[metrics$dataset==nm&metrics$metric==m&metrics$model==a,,drop=FALSE]
        rows[[length(rows)+1L]]<-data.frame(Model=a,Metric=if(m=="AUPRC")"AP"else m,Full=ci(z[z$subset=="full",]),Restricted=ci(z[z$subset=="without_documented_CMO",]),check.names=FALSE)}
      tabs[[nm]]<-do.call(rbind,rows)
    }
    table_path<-reg("to_share/Table_S_CMO_documentation_sensitivity.pdf");open_pdf(table_path,14,11)
    tryCatch(for(nm in names(tabs)){
      tt<-tabs[[nm]];c<-census[census$dataset==nm,,drop=FALSE];grid::grid.newpage()
      tx<-function(text,x,y,just="left",bold=FALSE,size=18)grid::grid.text(text,x,y,just=just,gp=grid::gpar(fontsize=size,fontface=if(bold)"bold"else"plain",fontfamily="Helvetica"))
      tx(dataset_labels[nm],.04,.95,bold=TRUE,size=21)
      tx(sprintf("Full n=%s (%s deaths); restricted n=%s (%s deaths)",c$full_n,c$full_events,c$retained_n,c$retained_events),.04,.905)
      xs<-c(.04,.15,.58,.96);ys<-.82-seq_len(nrow(tt))*.032
      line<-function(y,lwd)grid::grid.lines(c(.04,.96),c(y,y),gp=grid::gpar(lwd=lwd))
      line(.86,1.4);for(j in 1:4)tx(c("Model","Metric","Full cohort: estimate [95% CI]","Restricted: estimate [95% CI]")[j],xs[j],.835,if(j<=2)"left"else"right",TRUE)
      line(.813,.8)
      for(i in seq_len(nrow(tt)))for(j in 1:4)tx(as.character(tt[i,j]),xs[j],ys[i],if(j<=2)"left"else"right")
      line(min(ys)-.02,1.4)
      notes<-c("CMO restriction: any explicit comfort-only documentation available by T48.",
        "AP: average precision. Higher AUROC/AP and lower Brier/LogLoss are better.",
        "Paired percentile intervals condition on fixed predictions; no model refitting.",
        "A: baseline; B: +phenotype; C: +continuous dynamics; D: +dynamics +phenotype.",
        "Case ascertainment is incomplete; this does not exclude all care-limitation bias.")
      for(i in seq_along(notes))tx(notes[i],.04,min(ys)-.06-.032*(i-1))
      tx(paste(match(nm,names(tabs)),"/",length(tabs)),.96,.035,"right")
    },finally=grDevices::dev.off());check_pdf(table_path,4L)
    # Editable three-line Word-compatible tables; 18-point text in landscape pages.
    esc<-function(x){x<-gsub("\\","\\\\",x,fixed=TRUE);x<-gsub("{","\\{",x,fixed=TRUE);gsub("}","\\}",x,fixed=TRUE)}
    rt<-c("{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 Arial;}}\\landscape\\paperw20160\\paperh15840\\margl600\\margr600\\margt600\\margb600\\f0\\fs36")
    for(nm in names(tabs)){
      if(nm!=names(tabs)[1])rt<-c(rt,"\\page")
      rt<-c(rt,paste0("\\pard\\b ",esc(dataset_labels[nm]),"\\b0\\par\\par"));rows<-rbind(names(tabs[[nm]]),as.matrix(tabs[[nm]]))
      for(i in seq_len(nrow(rows))){bd<-paste0(if(i==1)"\\clbrdrt\\brdrs\\brdrw15\\clbrdrb\\brdrs\\brdrw10"else"",if(i==nrow(rows))"\\clbrdrb\\brdrs\\brdrw15"else"")
        rt<-c(rt,paste0("\\trowd\\trgaph60\\trkeep",paste0(bd,"\\cellx",c(1800,4200,11300,18800),collapse=""),
          paste(vapply(1:4,function(j)paste0("\\pard\\intbl",if(j<=2)"\\ql "else"\\qr ","\\fs36 ",if(i==1)"\\b "else"\\b0 ",esc(rows[i,j]),"\\cell"),character(1)),collapse=""),"\\row"))}
      rt<-c(rt,"\\pard\\fs36 CMO: explicit comfort-only documentation by T48. Fixed predictions; paired 95% bootstrap intervals. Ascertainment is incomplete.\\par")
    };txt_out(c(rt,"}"),"to_share/Table_S_CMO_documentation_sensitivity.rtf")
  }

  if(isTRUE(getOption("ami_step16.functions_only",FALSE)))return(invisible(list(main=main,preflight=preflight,build_selection=build_selection,align_bundle=align_bundle,weighted_metrics=weighted_metrics,evaluate_restriction=evaluate_restriction,make_visuals=make_visuals)))
  status<-"FAILED";current_module<-"input verification";failure_message<-NULL
  tryCatch(withCallingHandlers(main(),warning=function(w){add_warning(conditionMessage(w),current_module);invokeRestart("muffleWarning")}),error=function(err){
    status<<-"FAILED";failure_message<<-conditionMessage(err);issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step16_error_detail.txt")})
  flush_warnings()
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step16",
    "COMPLETED_FOR_REVIEW表示固定预测敏感性分析完成，等待结果核对；不表示已排除全部照护限制偏倚。",
    "FAILED表示本步未完整完成；检查日志。上游队列和预测保持原样。"),"to_share/Step16_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step16_sessionInfo.txt");logmsg(paste0("Step16状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step16_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share)else{
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step16_feedback.zip"),"\n",sep="")else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
