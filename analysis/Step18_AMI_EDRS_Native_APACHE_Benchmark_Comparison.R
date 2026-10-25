# AMI-EDRS revision | Step18: native APACHE IV/IVa benchmark comparison
# source(file.choose(), encoding="UTF-8")
# Fixed filenames; replaces owned results. Patient-level data and frozen references stay local_only.
local({
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MAKE_PNG=TRUE,MAKE_ZIP=TRUE,BOOTSTRAP_N=2000L,SEED=2026L)
  cfg<-utils::modifyList(cfg,getOption("ami_step18.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step18")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step18输出目录。")
  if (is_link(out)) stop("Step18输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP18")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step18目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP18_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP18_MANAGED_OUTPUT")) {
    stop("Step18输出目录的归属标记不正确。")
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
        if (unlink(p) != 0L) stop("不能覆盖旧Step18输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step18_run_log.txt")
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
    txt_out(if(nrow(z))unlist(Map(function(a,b)c(a,b,""),z$scope,z$message))else"No warnings in Step18.","local_only/Step18_warning_detail.txt")
    a<-if(nrow(z))aggregate(list(n=rep(1L,nrow(z))),list(scope=z$scope),sum)else data.frame(scope=character(),n=integer())
    csv_out(a,"to_share/09_warning_summary.csv")
  }

  plan<-list(version="Step18_v1",endpoint="in_hospital_death_after_T48",
    bootstrap_n=as.integer(cfg$BOOTSTRAP_N),seed=as.integer(cfg$SEED),epsilon=1e-12,
    population="Full frozen eICU T48 cohort; primary comparison uses intersection of valid IV and IVa probabilities",
    primary="Raw frozen MIMIC hospital-endpoint models versus native APACHE IV and IVa",
    secondary="Existing hospital-blocked cross-fitted intercept updates; no new updating in Step18",
    selection="APACHE availability only; never select a version, cohort or model by performance",
    uncertainty="Whole-hospital paired bootstrap primary; patient paired bootstrap sensitivity; fixed predictions",
    interpretation="Admission-severity contextual benchmarks in T48 survivors; different information horizons, not prospective equal-time evaluation",
    legacy="Six unverified/proxy MIMIC scores are excluded from standard-score comparisons; no score reconstruction in this script")
  if(length(plan$bootstrap_n)!=1L||!is.finite(plan$bootstrap_n)||plan$bootstrap_n<20L||length(plan$seed)!=1L||!is.finite(plan$seed))stop("BOOTSTRAP_N至少20，正式分析默认2000；SEED必须有效。")
  arms<-c("A","B","C","D")
  metric_names<-c("AUROC","AUPRC","Brier","LogLoss","Mean_prediction","Event_fraction")
  map<-data.frame(model=c(paste0(arms,"_raw"),"APACHE_IV","APACHE_IVa",paste0(arms,"_updated")),
    label=c("A: baseline","B: baseline + phenotype","C: continuous dynamics","D: dynamics + phenotype","APACHE IV","APACHE IVa",
      "A: updated","B: updated","C: updated","D: updated"),
    role=c(rep("frozen_raw",4),rep("native_admission_benchmark",2),rep("crossfitted_update_secondary",4)),stringsAsFactors=FALSE)
  primary_models<-map$model[1:6]
  model_colors<-c(A_raw="#7A8797",B_raw="#7D9C83",C_raw="#4F7FC4",D_raw="#3D647F",APACHE_IV="#CB9353",APACHE_IVa="#8B68A6",C_updated="#277F79")
  benchmark_pairs<-function(models){
    benches<-intersect(c("APACHE_IV","APACHE_IVa"),models); rows<-list()
    for(a in intersect(c(paste0(arms,"_raw"),paste0(arms,"_updated")),models))for(b in benches)
      rows[[length(rows)+1L]]<-data.frame(first=a,second=b,comparison=paste(a,"minus",b),role=if(grepl("updated$",a))"updated_secondary"else"raw_primary")
    for(ab in list(c("C_raw","A_raw"),c("D_raw","C_raw"),c("APACHE_IVa","APACHE_IV")))if(all(ab%in%models))
      rows[[length(rows)+1L]]<-data.frame(first=ab[1],second=ab[2],comparison=paste(ab[1],"minus",ab[2]),role="supporting_contrast")
    if(!length(rows))return(data.frame(first=character(),second=character(),comparison=character(),role=character()))
    do.call(rbind,rows)
  }
  check_status<-function(st){p<-file.path(cfg$OUTPUT_PARENT,st,"to_share",paste0(st,"_STATUS.txt"))
    if(!file.exists(p)||!"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(p,warn=FALSE))stop("请先完成",st,"。")}
  aligned_native<-function(native,ids){
    need(native,c("stay_id","value","status","version","field","joint_model_available"),"Step17原生APACHE")
    if(!length(ids)||anyNA(ids)||anyDuplicated(ids))stop("APACHE匹配名单为空、缺失或重复。")
    pp<-matrix(NA_real_,length(ids),2,dimnames=list(NULL,c("APACHE_IV","APACHE_IVa")))
    ss<-matrix("",length(ids),2,dimnames=dimnames(pp));valid<-matrix(FALSE,length(ids),2,dimnames=dimnames(pp))
    for(v in c("IV","IVa")){
      z<-native[!is.na(native$version)&native$version==v&!is.na(native$field)&native$field=="hospital_probability",,drop=FALSE]
      if(nrow(z)!=length(ids)||anyNA(z$stay_id)||anyDuplicated(as.character(z$stay_id))||!setequal(as.character(z$stay_id),ids))stop("APACHE ",v," 未覆盖唯一冻结患者名单。")
      z<-z[match(ids,as.character(z$stay_id)),,drop=FALSE];ok<-z$status%in%c("VALID","VALID_DUPLICATE_IDENTICAL")
      if(anyNA(z$status)||!is.numeric(z$value)||any(ok&(!is.finite(z$value)|z$value<0|z$value>1))||any(!ok&!is.na(z$value)))stop("APACHE有效性标志与概率不一致。")
      if(anyNA(z$joint_model_available)||!identical(as.logical(z$joint_model_available),ok))stop("Step17的完整模型交集标志异常。")
      nm<-paste0("APACHE_",v);pp[,nm]<-z$value;ss[,nm]<-z$status;valid[,nm]<-ok
    }
    list(probability=pp,status=ss,valid=valid,common=valid[,1]&valid[,2])
  }
  benchmark_cohorts<-function(a){
    masks<-list(common=a$common,IV_available=a$valid[,"APACHE_IV"],IVa_available=a$valid[,"APACHE_IVa"])
    rows<-list();active<-list(common=masks$common)
    for(nm in names(masks)){
      same<-if(nm!="common"&&identical(masks[[nm]],masks$common))"common"else nm
      rows[[length(rows)+1L]]<-data.frame(cohort=nm,analysis_key=same,n=sum(masks[[nm]]),role=if(nm=="common")"PRIMARY_SHARED_PATIENTS"else"VERSION_AVAILABILITY_SENSITIVITY")
      if(nm!="common"&&same!= "common")active[[nm]]<-masks[[nm]]
    }
    list(masks=masks,active=active,registry=do.call(rbind,rows))
  }
  evaluate_benchmark<-function(y,p,hospital=NULL,B=plan$bootstrap_n,seed=plan$seed){
    setup<-metric_setup(y,p);mode<-if(is.null(hospital))"patient"else"hospital_cluster"
    if(length(unique(y))<2L)stop("比较队列缺少事件或非事件，不能计算完整基准比较。")
    if(!is.null(hospital)){
      if(length(hospital)!=length(y)||anyNA(hospital)||any(!nzchar(trimws(as.character(hospital))))||length(unique(hospital))<2L)stop("医院配对重抽样需要完整医院ID且至少两家医院。")
      hi<-match(as.character(hospital),unique(as.character(hospital)));H<-max(hi)
    }
    point<-weighted_metrics(y,p,rep(1,length(y)),setup)
    arr<-array(NA_real_,c(B,ncol(p),length(metric_names)),dimnames=list(NULL,colnames(p),metric_names))
    draws<-matrix(NA_real_,B,2,dimnames=list(NULL,c("patients","events")));set.seed(seed)
    for(i in seq_len(B)){
      w<-if(is.null(hospital))tabulate(sample.int(length(y),length(y),replace=TRUE),length(y))else tabulate(sample.int(H,H,replace=TRUE),H)[hi]
      arr[i,,]<-weighted_metrics(y,p,w,setup);draws[i,]<-c(sum(w),sum(w*y))
      if(i%%500L==0L)logmsg(paste0(mode," 配对重抽样：",i,"/",B,"。"))
    }
    row<-function(est,v,...){ci<-interval(v,B);data.frame(...,estimate=est,lower=ci[1],upper=ci[2],bootstrap_valid=sum(is.finite(v)),bootstrap_requested=B,check.names=FALSE)}
    met<-dif<-list();pairs<-benchmark_pairs(colnames(p))
    for(a in colnames(p))for(m in metric_names)met[[length(met)+1L]]<-row(point[a,m],arr[,a,m],resampling=mode,model=a,metric=m)
    for(j in seq_len(nrow(pairs)))for(m in metric_names[1:4])dif[[length(dif)+1L]]<-row(point[pairs$first[j],m]-point[pairs$second[j],m],
      arr[,pairs$first[j],m]-arr[,pairs$second[j],m],resampling=mode,first=pairs$first[j],second=pairs$second[j],comparison=pairs$comparison[j],role=pairs$role[j],metric=m)
    list(metrics=do.call(rbind,met),differences=do.call(rbind,dif),bootstrap=arr,draw_counts=draws,seed=seed)
  }
  cohort_summary<-function(mask,name,y,hospital,icu){
    n<-sum(mask);k<-sum(y[mask]);ci<-wilson(k,n)
    data.frame(cohort=name,n=n,hospital_deaths=k,event_fraction=if(n)k/n else NA_real_,event_lower=ci[1],event_upper=ci[2],
      hospitals=length(unique(hospital[mask])),in_index_ICU_n=sum(icu[mask]),out_of_index_ICU_n=sum(!icu[mask]),stringsAsFactors=FALSE)
  }
  check_full_metrics<-function(y,raw,updated,previous13,previous14){
    rows<-list()
    for(type in c("raw","updated")){
      p<-if(type=="raw")raw else updated;point<-weighted_metrics(y,p,rep(1,length(y)))
      z<-if(type=="raw")previous13[previous13$cohort=="eICU_frozen"&previous13$resampling=="patient",]else
        previous14[previous14$scheme=="hospital_blocked"&previous14$resampling=="hospital_cluster"&previous14$method=="intercept",]
      for(a in arms)for(m in metric_names){old<-z$estimate[z$model==a&z$metric==m]
        if(length(old)!=1L||!is.finite(old)||abs(old-point[a,m])>1e-10)stop("全队列",type,"指标不能复现：",a,"/",m)
        rows[[length(rows)+1L]]<-data.frame(variant=type,model=a,metric=m,previous=old,recomputed=point[a,m],absolute_difference=abs(old-point[a,m]))
      }
    };do.call(rbind,rows)
  }
  preflight<-function(){
    stopifnot(identical(binary(factor(c("0","1"),levels=c("1","0"))),0:1),inherits(try(binary(c(0,NA)),silent=TRUE),"try-error"))
    yy<-c(0L,1L,0L,1L,0L,1L);pp<-cbind(C_raw=c(.2,.2,.5,.8,.8,.9),APACHE_IV=c(.2,.2,.5,.8,.8,.9));w<-c(2,1,0,2,1,3)
    ix<-rep(seq_along(yy),w);a<-pp[ix,1][yy[ix]==1];b<-pp[ix,1][yy[ix]==0]
    auc<-mean(outer(a,b,">")+.5*outer(a,b,"=="));q<-sort(unique(pp[ix,1]),decreasing=TRUE);prev<-ap<-0
    for(t in q){hit<-pp[ix,1]>=t;rec<-sum(yy[ix][hit])/sum(yy[ix]);ap<-ap+(rec-prev)*mean(yy[ix][hit]);prev<-rec}
    mm<-weighted_metrics(yy,pp,w);stopifnot(abs(mm[1,"AUROC"]-auc)<1e-12,abs(mm[1,"AUPRC"]-ap)<1e-12,
      abs(mm[1,"Brier"]-mean((pp[ix,1]-yy[ix])^2))<1e-12)
    tie<-weighted_metrics(c(1L,0L,1L),cbind(C_raw=c(.5,.5,.2),APACHE_IV=c(.5,.5,.2)),rep(1,3))
    stopifnot(abs(tie[1,"AUROC"]-.25)<1e-12,abs(tie[1,"AUPRC"]-7/12)<1e-12)
    na<-rbind(data.frame(stay_id=as.character(1:4),value=c(.1,.2,NA,.4),status=c("VALID","VALID","NO_ROW","VALID"),version="IV",field="hospital_probability",joint_model_available=c(TRUE,TRUE,FALSE,TRUE)),
      data.frame(stay_id=as.character(1:4),value=c(.1,.2,.3,NA),status=c("VALID","VALID","VALID","NEGATIVE_SENTINEL"),version="IVa",field="hospital_probability",joint_model_available=c(TRUE,TRUE,TRUE,FALSE)))
    aa<-aligned_native(na[c(8,3,6,1,4,7,2,5),],as.character(1:4));cc<-benchmark_cohorts(aa)
    stopifnot(sum(aa$common)==2L,all(colSums(aa$valid)==3L),length(cc$active)==3L,identical(aa$probability[1,],c(APACHE_IV=.1,APACHE_IVa=.1)))
    bad<-na;bad$value[1]<--1;stopifnot(inherits(try(aligned_native(bad,as.character(1:4)),silent=TRUE),"try-error"))
    stopifnot(inherits(try(aligned_native(rbind(na,na[1,]),as.character(1:4)),silent=TRUE),"try-error"))
    aa$valid[,2]<-aa$valid[,1];aa$common<-aa$valid[,1];stopifnot(length(benchmark_cohorts(aa)$active)==1L)
    same<-evaluate_benchmark(yy,pp,hospital=c("a","a","b","b","c","c"),B=20L,seed=19L)
    stopifnot(all(same$differences$estimate==0),all(same$differences$lower==0|is.na(same$differences$lower)))
    # Reproduce the first whole-hospital draw independently; repeated hospitals keep all their patients.
    set.seed(19L);hc<-tabulate(sample.int(3,3,replace=TRUE),3);expanded<-rep(seq_len(6),hc[c(1,1,2,2,3,3)])
    oracle<-weighted_metrics(yy[expanded],pp[expanded,,drop=FALSE],rep(1,length(expanded)))
    stopifnot(max(abs(same$bootstrap[1,,]-oracle),na.rm=TRUE)<1e-12)
    stopifnot(inherits(try(evaluate_benchmark(rep(0L,6),pp,B=20L),silent=TRUE),"try-error"))
    TRUE
  }

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
    if(!file.rename(tmp,reg(rel)))stop("Step18缓存写入失败。");value
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

  wilson<-function(k,n){if(n<=0)return(c(lower=NA_real_,upper=NA_real_));z<-qnorm(.975);v<-k/n;den<-1+z*z/n
    c(lower=(v+z*z/(2*n)-z*sqrt(v*(1-v)/n+z*z/(4*n*n)))/den,upper=(v+z*z/(2*n)+z*sqrt(v*(1-v)/n+z*z/(4*n*n)))/den)}
  calibration<-function(y,p,hospital=NULL){
    lp<-qlogis(pmax(plan$epsilon,pmin(1-plan$epsilon,p)));rows<-list()
    for(kind in c("intercept_only","joint")){
      x<-if(kind=="intercept_only")matrix(1,length(y),1,dimnames=list(NULL,"intercept_offset"))else cbind(joint_intercept=1,slope=lp)
      off<-if(kind=="intercept_only")lp else rep(0,length(y));warnings<-character()
      fit<-withCallingHandlers(tryCatch(stats::glm.fit(x,y,offset=off,family=stats::binomial(),control=stats::glm.control(maxit=100)),error=function(e)e),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")})
      valid<-!inherits(fit,"error")&&isTRUE(fit$converged)&&fit$rank==ncol(x)&&all(is.finite(fit$coefficients))&&!length(warnings)
      reason<-if(inherits(fit,"error"))conditionMessage(fit)else if(length(warnings))paste(warnings,collapse="; ")else if(!valid)"Nonconverged or rank deficient"else"OK"
      v<-if(valid)tryCatch(solve(crossprod(x,x*as.numeric(fit$fitted.values*(1-fit$fitted.values)))),error=function(e)NULL)else NULL
      if(is.null(v)){valid<-FALSE;if(reason=="OK")reason<-"Singular information matrix"}
      for(method in c("model_based",if(!is.null(hospital))"hospital_robust")){
        vv<-v
        if(valid&&method=="hospital_robust"){
          ss<-rowsum(x*as.numeric(y-fit$fitted.values),hospital,reorder=FALSE);H<-nrow(ss)
          vv<-v%*%crossprod(ss)%*%v*H/(H-1)*(length(y)-1)/(length(y)-ncol(x))
        }
        se<-if(valid)sqrt(diag(vv))else rep(NA_real_,ncol(x));coef<-if(valid)fit$coefficients else rep(NA_real_,ncol(x))
        rows[[length(rows)+1L]]<-data.frame(parameter=colnames(x),inference=method,estimate=as.numeric(coef),lower=as.numeric(coef-qnorm(.975)*se),upper=as.numeric(coef+qnorm(.975)*se),status=if(valid)"OK"else reason)
      }
    };do.call(rbind,rows)
  }
  curves<-function(y,p){o<-order(p,decreasing=TRUE);ps<-p[o];last<-c(which(diff(ps)!=0),length(y));tp<-cumsum(y[o])[last];fp<-cumsum(1-y[o])[last]
    data.frame(threshold=ps[last],TP=tp,FP=fp,recall=tp/sum(y),fpr=fp/sum(1-y),precision=tp/(tp+fp))}
  calibration_bins<-function(y,p){br<-unique(as.numeric(quantile(p,seq(0,1,.1))));g<-if(length(br)>1)cut(p,br,include.lowest=TRUE,labels=FALSE)else rep(1L,length(y))
    do.call(rbind,lapply(sort(unique(g)),function(j){ix<-which(g==j);ci<-wilson(sum(y[ix]),length(ix));data.frame(bin=j,n=length(ix),events=sum(y[ix]),predicted=mean(p[ix]),observed=mean(y[ix]),lower=ci[1],upper=ci[2])}))}

  main<-function(){
    current_module<<-"preflight";logmsg("Step18预检：APACHE版本交集、患者匹配、同分指标、医院整体配对重抽样和空输入。");preflight()
    txt_out("PASS: outcome parsing; tied AUROC/AP independent oracles; identical counts with different patient masks; version separation; conflicting probability rejection; whole-hospital weighting; zero paired difference; single-class rejection.","to_share/02_preflight.txt")
    for(st in c("Step13","Step14","Step15","Step17"))check_status(st)
    paths<-c(roster="Step15/local_only/frozen_T48_roster.rds",audit="Step17/local_only/Step17_score_feasibility.rds",
      native="Step17/local_only/native_APACHE_alignment.rds",native_source="Step17/to_share/05_native_source_columns.csv",
      native_summary="Step17/to_share/05_native_APACHE_coverage.csv",raw="Step13/local_only/eICU_T48_hospital_frozen_predictions.rds",
      updated="Step14/local_only/eICU_crossfitted_recalibration.rds",lineage14="Step14/to_share/01_inputs.csv",
      metrics13="Step13/to_share/05_hospital_metrics.csv",metrics14="Step14/to_share/04_crossfit_metrics.csv")
    paths<-setNames(file.path(cfg$OUTPUT_PARENT,paths),names(paths));if(!all(file.exists(paths)))stop("缺少已完成步骤的本地文件：",paste(names(paths)[!file.exists(paths)],collapse=","))
    hashes<-setNames(unname(tools::md5sum(paths)),names(paths));csv_out(data.frame(input=names(paths),file=unname(paths),md5=unname(hashes)),"to_share/01_inputs.csv")
    txt_out(capture.output(dput(plan)),"to_share/02_analysis_plan.txt");csv_out(map,"to_share/02_model_definitions.csv")
    current_module<<-"upstream lineage and frozen cohort"
    roster<-readRDS(paths["roster"]);check_keys(roster,"Step15名单");need(roster,c("death_hospital","death_day30","in_index_icu"),"Step15名单")
    if(nrow(roster)!=10417L||sum(roster$source=="eICU")!=5284L||sum(roster$source=="MIMIC")!=5133L)stop("冻结队列人数改变。")
    audit<-readRDS(paths["audit"]);native<-readRDS(paths["native"])
    if(!identical(audit$version,"Step17_v1")||!identical(audit$roster_hash,hash_object(roster))||!identical(native,audit$native$patients))stop("Step17版本、名单或APACHE缓存不一致。")
    source<-read.csv(paths["native_source"],stringsAsFactors=FALSE);need(source,c("origin","file","column"),"Step17来源")
    if(!nrow(source)||anyNA(source$origin)||any(source$origin!="RAW_NATIVE_TABLE"))stop("本步要求Step17已确认的原生APACHE表，不能静默使用旧的局部提取文件。")
    fi<-audit$input_fingerprints;need(fi,c("path","md5"),"Step17来源指纹")
    for(nm in c("raw","roster")){
      j<-which(vapply(fi$path,norm,character(1))==norm(paths[nm]))
      if(length(j)!=1L||is.na(fi$md5[j])||!identical(as.character(fi$md5[j]),unname(hashes[nm])))stop("Step17后冻结输入变化：",nm)
    }
    lin<-read.csv(paths["lineage14"],stringsAsFactors=FALSE);need(lin,c("file","md5"),"Step14来源")
    j<-which(vapply(lin$file,norm,character(1))==norm(paths["raw"]))
    if(length(j)!=1L||is.na(lin$md5[j])||!identical(as.character(lin$md5[j]),unname(hashes["raw"])))stop("Step14使用的原始预测与当前Step13不一致。")
    z<-readRDS(paths["raw"]);u<-readRDS(paths["updated"])
    need(z$metadata,c("source","stay_id","patient_key","landmark","hospital_id"),"Step13患者信息");check_keys(z$metadata,"Step13")
    need(z$outcomes,c("event_primary","endpoint"),"Step13结局")
    rr<-roster[roster$source=="eICU",,drop=FALSE];idx<-match(keys(z$metadata),keys(rr))
    if(!identical(z$version,"Step13_v1")||!identical(z$prediction_endpoint,plan$endpoint)||!identical(z$evaluation_endpoint,plan$endpoint)||nrow(z$metadata)!=5284L||anyNA(idx)||anyDuplicated(idx)||anyNA(z$metadata$landmark)||any(z$metadata$landmark!="T48")||nrow(z$outcomes)!=5284L||anyNA(z$outcomes$endpoint)||any(z$outcomes$endpoint!=plan$endpoint))stop("Step13版本、患者或终点不符。")
    y<-binary(z$outcomes$event_primary);raw<-z$prediction
    if(sum(y)!=219L||!identical(y,binary(rr$death_hospital[idx]))||!identical(colnames(raw),arms))stop("冻结院内死亡标签或A-D列不符。")
    metric_setup(y,raw);lp<-z$linear_predictor
    if(!is.matrix(lp)||!identical(dim(lp),dim(raw))||!identical(colnames(lp),arms)||any(!is.finite(lp))||max(abs(plogis(lp)-raw))>1e-12)stop("Step13线性预测值不能复现原始概率。")
    hospital<-as.character(z$metadata$hospital_id)
    if(anyNA(hospital)||any(!nzchar(trimws(hospital)))||length(unique(hospital))<5L)stop("需要完整医院ID及至少五家医院。")
    if(anyNA(z$metadata$patient_key)||anyDuplicated(z$metadata$patient_key))stop("Step13 patient_key不唯一。")
    need(u$metadata,c("source","stay_id","patient_key","hospital_id"),"Step14患者信息");check_keys(u$metadata,"Step14")
    need(u$outcomes,c("event_primary","endpoint"),"Step14结局")
    ui<-match(keys(z$metadata),keys(u$metadata))
    if(!identical(u$version,"Step14_v1")||!identical(u$source_model_hash,z$model_hash)||!identical(u$plan$endpoint,plan$endpoint)||nrow(u$metadata)!=length(y)||nrow(u$outcomes)!=length(y)||anyNA(ui)||anyDuplicated(ui)||
      !identical(as.character(u$metadata$patient_key[ui]),as.character(z$metadata$patient_key))||!identical(as.character(u$metadata$hospital_id[ui]),hospital)||!identical(binary(u$outcomes$event_primary[ui]),y)||anyNA(u$outcomes$endpoint)||any(u$outcomes$endpoint!=plan$endpoint))stop("Step14与Step13患者、模型或结局不一致。")
    cf<-u$results$hospital_blocked
    if(!is.list(cf)||!is.matrix(cf$prediction)||nrow(cf$prediction)!=length(y)||!all(c(paste0(arms,"_raw"),paste0(arms,"_intercept"))%in%colnames(cf$prediction))||length(cf$fold)!=length(y))stop("Step14医院分组交叉拟合对象不完整。")
    fold<-cf$fold[ui]
    if(anyNA(fold)||length(unique(fold))!=5L||any(vapply(split(fold,hospital),function(a)length(unique(a))!=1L,logical(1))))stop("医院跨校准折或折信息缺失。")
    need(cf$audit,c("shared_hospitals","shared_patients"),"Step14训练/评价审计")
    if(nrow(cf$audit)!=5L||anyNA(cf$audit$shared_hospitals)||anyNA(cf$audit$shared_patients)||any(cf$audit$shared_hospitals!=0|cf$audit$shared_patients!=0))stop("Step14训练/评价审计不完整或存在重叠。")
    oldraw<-cf$prediction[ui,paste0(arms,"_raw"),drop=FALSE];colnames(oldraw)<-arms
    updated<-cf$prediction[ui,paste0(arms,"_intercept"),drop=FALSE];colnames(updated)<-arms
    metric_setup(y,updated);if(max(abs(oldraw-raw))>1e-12)stop("Step14内保存的原始预测不一致。")
    round<-check_full_metrics(y,raw,updated,read.csv(paths["metrics13"],stringsAsFactors=FALSE),read.csv(paths["metrics14"],stringsAsFactors=FALSE))
    csv_out(round,"to_share/02_full_cohort_metric_roundtrip.csv")
    current_module<<-"APACHE availability and paired cohorts"
    a<-aligned_native(native,as.character(z$metadata$stay_id));coh<-benchmark_cohorts(a)
    if(!any(a$common))stop("IV与IVa没有共同可比较患者；未自动选择更有利版本。")
    ns<-read.csv(paths["native_summary"],stringsAsFactors=FALSE);need(ns,c("version","field","valid_n","joint_model_n","hospital_deaths"),"Step17覆盖率")
    checks<-list()
    for(v in c("IV","IVa")){
      mask<-a$valid[,paste0("APACHE_",v)];q<-ns[ns$version==v&ns$field=="hospital_probability",,drop=FALSE]
      if(nrow(q)!=1L||sum(mask)!=q$valid_n||sum(mask)!=q$joint_model_n||sum(y[mask])!=q$hospital_deaths)stop("APACHE覆盖率未复现Step17：",v)
      checks[[v]]<-data.frame(version=v,previous_n=q$valid_n,recomputed_n=sum(mask),previous_deaths=q$hospital_deaths,recomputed_deaths=sum(y[mask]))
    }
    csv_out(do.call(rbind,checks),"to_share/02_Step17_coverage_roundtrip.csv")
    colnames(raw)<-paste0(arms,"_raw");colnames(updated)<-paste0(arms,"_updated")
    allp<-cbind(raw,a$probability,updated);allp<-allp[,map$model,drop=FALSE]
    icu<-as.logical(binary(rr$in_index_icu[idx]));masks<-c(list(full_frozen=rep(TRUE,length(y))),coh$masks,list(outside_common=!a$common))
    census<-do.call(rbind,lapply(names(masks),function(nm)cohort_summary(masks[[nm]],nm,y,hospital,icu)))
    csv_out(census,"to_share/03_cohort_flow.csv");csv_out(coh$registry,"to_share/03_cohort_analysis_registry.csv")
    states<-do.call(rbind,lapply(colnames(a$status),function(nm){tt<-as.data.frame(table(status=a$status[,nm]),stringsAsFactors=FALSE);tt$model<-nm;tt}))
    csv_out(states,"to_share/03_APACHE_missingness.csv")
    csv_out(as.data.frame(table(IV_valid=a$valid[,1],IVa_valid=a$valid[,2])),"to_share/03_version_availability_overlap.csv")
    saveRDS(list(version=plan$version,metadata=z$metadata,outcomes=z$outcomes,prediction=allp,APACHE_status=a$status,
      masks=coh$masks,hospital_blocked_update_fold=fold,plan=plan,input_hashes=hashes),reg("local_only/eICU_APACHE_paired_predictions.rds"),compress="gzip")
    current_module<<-"paired performance"
    codehash<-hash_object(lapply(c("evaluate_benchmark","weighted_metrics","metric_setup","benchmark_pairs","interval"),function(nm)paste(deparse(body(get(nm))),collapse="\n")))
    cachekey<-hash_object(list(inputs=hashes,plan=plan,code=codehash,R=as.character(getRversion())))
    mets<-diffs<-cals<-bins<-rocpr<-refs<-bs_audit<-list()
    for(nm in names(coh$active)){
      keep<-coh$active[[nm]];cols<-if(nm=="common")map$model else setdiff(map$model,if(nm=="IV_available")"APACHE_IVa"else"APACHE_IV")
      yy<-y[keep];pp<-allp[keep,cols,drop=FALSE];hh<-hospital[keep]
      metric_setup(yy,pp);logmsg(paste0(nm,"：",sum(keep),"人，",sum(yy),"例院内死亡；固定预测配对比较。"))
      localkey<-hash_object(list(cachekey=cachekey,cohort=nm,y=yy,prediction=pp,hospital=hh))
      for(mode in c("hospital_cluster","patient")){
        current_module<<-paste(nm,mode)
        ev<-cache_result(paste0("local_only/cache/",nm,"_",mode,".rds"),localkey,
          function()evaluate_benchmark(yy,pp,if(mode=="hospital_cluster")hh else NULL,plan$bootstrap_n,plan$seed))
        tag<-paste(nm,mode);mets[[tag]]<-cbind(cohort=nm,ev$metrics);diffs[[tag]]<-cbind(cohort=nm,ev$differences)
        bs_audit[[tag]]<-data.frame(cohort=nm,resampling=mode,requested=plan$bootstrap_n,min_patients=min(ev$draw_counts[,1]),max_patients=max(ev$draw_counts[,1]),
          min_events=min(ev$draw_counts[,2]),max_events=max(ev$draw_counts[,2]),min_metric_valid=min(ev$metrics$bootstrap_valid),seed=ev$seed)
      }
      cals[[nm]]<-do.call(rbind,lapply(cols,function(m)cbind(cohort=nm,model=m,calibration(yy,pp[,m],hh))))
      bins[[nm]]<-do.call(rbind,lapply(cols,function(m)cbind(cohort=nm,model=m,calibration_bins(yy,pp[,m]))))
      rocpr[[nm]]<-do.call(rbind,lapply(cols,function(m)cbind(cohort=nm,model=m,curves(yy,pp[,m]))))
      rate<-mean(yy);refs[[nm]]<-data.frame(cohort=nm,n=length(yy),events=sum(yy),event_rate=rate,AP_reference=rate,
        Brier_event_rate_reference=rate*(1-rate),LogLoss_event_rate_reference=-(rate*log(rate)+(1-rate)*log1p(-rate)),
        role="Descriptive evaluation-cohort event-rate reference; not an independently evaluated predictor")
    }
    met<-do.call(rbind,mets);dif<-do.call(rbind,diffs);cal<-do.call(rbind,cals);bin<-do.call(rbind,bins);cur<-do.call(rbind,rocpr)
    csv_out(met,"to_share/04_benchmark_metrics.csv");csv_out(dif,"to_share/04_paired_benchmark_differences.csv")
    csv_out(do.call(rbind,bs_audit),"to_share/04_bootstrap_audit.csv");csv_out(do.call(rbind,refs),"to_share/04_event_rate_references.csv")
    csv_out(cal,"to_share/05_calibration_parameters.csv");csv_out(bin,"to_share/05_calibration_bins.csv")
    csv_out(cur[,setdiff(names(cur),"threshold"),drop=FALSE],"to_share/05_ROC_PR_source.csv")
    if(any(!is.finite(met$lower))||any(!is.finite(dif$lower)))issue("WARNING","uncertainty","部分指标有效重抽样不足90%；保留NA区间。")
    if(any(cal$status!="OK"))issue("NOTE","calibration","部分校准参数无法稳定估计；CSV保留状态，不生成替代参数。")
    if(plan$bootstrap_n!=2000L)issue("NOTE","bootstrap configuration","当前不是默认2000次；正式结果请使用默认配置。")
    current_module<<-"legacy score disposition and interpretation"
    legacy<-data.frame(score=c("SOFA reconstructed","OASIS reconstructed","SAPS II reconstructed","GRACE-like proxy","LODS-like proxy","APS III-like proxy"),
      disposition="NOT_USED_AS_ORIGINAL_SCORE",reason=c("Simplified vasopressor/respiratory scoring and incomplete components","Incomplete domains and unverified original thresholds","Approximate, incomplete component implementation","Explicit proxy; presentation-time GRACE inputs not established","Explicit proxy","Explicit proxy"),stringsAsFactors=FALSE)
    csv_out(legacy,"to_share/06_legacy_score_disposition.csv")
    issue("LIMITATION","benchmark interpretation","原生APACHE为入院严重度背景基准；T48动态模型拥有更长信息窗。原生记录无逐条生成时间，不能据此证明实时T48可用或等时点优越性。")
    issue("LIMITATION","external model update","更新模型沿用Step14医院分组折外预测，单列为补充；未对APACHE重新校准。与原生APACHE比较不能解释为模型架构本身的优势。")
    txt_out(c("Step18：eICU原生APACHE IV/IVa与冻结院内死亡模型的配对背景基准比较。",
      "对应原Word第9项。使用Step17缓存，不需要T7，不读取旧proxy评分性能，也不改变Step0-17。",
      "完整eICU冻结队列5284人/219例院内死亡。主比较采用IV与IVa均有效的实际患者交集；不能因两版本人数相同而假设患者相同。",
      "各版本单独可用队列作为敏感性；与主交集完全相同时只登记别名、不重复运行。缺失/负值/冲突均不填0，不按性能选版本。",
      "所有结局均来自修订后冻结名单的T48后本次住院死亡。不用APACHE表的actualHospitalMortality覆盖结局，不混入30天标签。",
      "主结果：原始冻结A-D与原生APACHE IV/IVa；补充：Step14医院分组交叉拟合的截距更新A-D。同一比较内所有模型使用相同患者与结局。",
      "保留APACHE原始概率。Step18没有训练、调参、分型、再校准或阈值筛选。校准回归仅诊断，不用诊断拟合值替换原预测。",
      "APACHE为入院/早期严重度背景基准，T48模型使用更长信息窗；比较不能证明相同信息条件下的算法优势。原始APACHE风险未条件化于T48存活。",
      "原生APACHE无逐条生成/录入时间，本比较是回顾性背景评价，不证明实时T48可用；结果不直接支持临床部署。",
      "主区间按医院整体配对重抽样2000次；患者级配对重抽样为敏感性。同次抽样对全部模型使用相同权重，重复医院保留其所有患者及重复权重。",
      "95%百分位区间条件于固定预测，不包含MIMIC重新开发或校准重新拟合的不确定性。差值方向固定为first减second；AUROC/AP高更好，Brier/LogLoss低更好。",
      "AUPRC统一为同分阈值合并的average precision；PR图用与AP一致的阶梯连接。LogLoss和校准的数值截断为1e-12，其他指标用原始概率。",
      "校准截距分别报告固定斜率1的offset截距和联合截距，另报斜率及模型/医院稳健Wald区间。分组校准图的95%Wilson区间仅为描述性患者级区间。",
      "事件率参照由当前评价队列计算，仅作为描述参照，不是经独立验证的预测模型。AP与误差受人群构成影响；主交集外人数和死亡数单独报告。",
      "旧MIMIC六项近似评分不进入标准评分对照，本步没有重建原版SOFA/OASIS/SAPSII/GRACE/LODS/APSIII。撤回对这些原版评分的既有优越性主张。",
      "本步新增原生APACHE背景基准补充图表，原代理评分性能图不再作为正式标准评分证据；最终正文与补充材料编号在整稿时统一。",
      "新图基本字号20pt，表正文18pt；PDF可编辑，图同时输出300dpi PNG，三线表另有RTF与CSV。",
      "ROC/PR分享源表只含累计计数与曲线坐标，不包含逐人概率或精确概率阈值。",
      "仅回传Step18_feedback.zip。患者ID、医院ID、个体预测、分折及重抽样数组保留local_only。"),"to_share/00_READ_ME_FIRST.txt")
    txt_out(c("https://eicu.mit.edu/eicutables/apachepatientresult/","https://eicu.mit.edu/eicutables/apachepredvar/",
      "APACHE probability/version definitions: original eICU documentation. T48 selection is this study's frozen cohort, not the original APACHE target population."),"to_share/10_definition_sources.txt")
    current_module<<-"large-font figures and three-line tables";make_visuals(met,dif,cal,bin,cur,census)
    csv_out(data.frame(output=c("Figure_S_eICU_APACHE_ROC","Figure_S_eICU_APACHE_PR","Figure_S_eICU_APACHE_calibration","Table_S_eICU_APACHE_performance","Table_S_eICU_APACHE_paired_differences","Table_S_eICU_APACHE_calibration"),
      action="ADD native benchmark supplementary evidence; retire legacy proxy-as-original claims; final panel numbers pending integration",
      original_plan_item=9L,reviewer="R3 major7 and major9; R1 major5",stringsAsFactors=FALSE),"to_share/11_figure_table_replacement_map.csv")
    csv_out(data.frame(task=c("Native APACHE contextual comparison","MIMIC original six-score reconstruction","Held-out categorical phenotype OR/HR","External thresholds/DCA and final statistical exports","Web prediction consistency","Manuscript and 45 reply units"),
      status=c("COMPLETED_FOR_REVIEW","NOT_PERFORMED; legacy proxy claims to be removed","PENDING","PENDING","PENDING","PENDING")),"to_share/12_revision_progress.csv")
    if(!identical(hashes,setNames(unname(tools::md5sum(paths)),names(paths))))stop("运行期间上游输入发生变化。")
    status<<-"COMPLETED_FOR_REVIEW"
  }

  check_pdf<-function(p,expected){b<-as.integer(readBin(p,"raw",n=file.info(p)$size));b[b<32L|b>126L]<-32L
    m<-gregexpr("/Type[[:space:]]*/Page([^sA-Za-z]|$)",rawToChar(as.raw(b)),perl=TRUE)[[1]]
    if((if(m[1]<0)0L else length(m))!=expected)stop("PDF页数不符：",basename(p))}
  save_figure<-function(stem,draw){
    p<-reg(paste0("to_share/",stem,".pdf"));grDevices::pdf(p,width=12,height=10,pointsize=20,family="Helvetica",onefile=TRUE,useDingbats=FALSE,useKerning=FALSE)
    tryCatch(draw(),finally=grDevices::dev.off());check_pdf(p,1L)
    if(isTRUE(cfg$MAKE_PNG)){grDevices::png(reg(paste0("to_share/",stem,".png")),width=12,height=10,units="in",res=300,pointsize=20)
      tryCatch(draw(),finally=grDevices::dev.off())}
  }
  three_line<-function(tables,title,stem,notes){
    if(!length(tables))stop("没有可输出的表格：",stem)
    # Large type is retained by placing each modest table on its own landscape page.
    p<-reg(paste0("to_share/",stem,".pdf"));grDevices::pdf(p,width=16,height=10,pointsize=18,family="Helvetica",onefile=TRUE,useDingbats=FALSE,useKerning=FALSE)
    tryCatch(for(k in seq_along(tables)){
      tab<-tables[[k]];nr<-nrow(tab);nc<-ncol(tab);if(nr>12L||nc>5L)stop("表格超过大字号单页容量。")
      grid::grid.newpage();tx<-function(s,x,y,just="left",bold=FALSE,size=18)grid::grid.text(s,x,y,just=just,gp=grid::gpar(fontfamily="Helvetica",fontsize=size,fontface=if(bold)"bold"else"plain"))
      tx(title,.04,.95,bold=TRUE,size=24);tx(names(tables)[k],.04,.895,bold=TRUE,size=20)
      xs<-if(nc==5)c(.04,.43,.61,.79,.97)else if(nc==4)c(.04,.48,.72,.97)else c(.04,seq(.60,.97,length.out=nc-1L))
      line<-function(y,w)grid::grid.lines(c(.04,.97),c(y,y),gp=grid::gpar(lwd=w));line(.84,1.4)
      for(j in seq_len(nc))tx(names(tab)[j],xs[j],.81,if(j==1)"left"else"right",TRUE)
      line(.785,.8);ys<-.77-seq_len(nr)*.045
      for(i in seq_len(nr))for(j in seq_len(nc))tx(as.character(tab[i,j]),xs[j],ys[i],if(j==1)"left"else"right")
      bottom<-if(nr)min(ys)-.025 else .74;line(bottom,1.4);ny<-bottom-.055
      for(note in notes)for(s in strwrap(note,width=110)){if(ny<.05)stop("表格说明溢出：",stem);tx(s,.04,ny,size=18);ny<-ny-.035}
      tx(paste(k,"/",length(tables)),.97,.025,"right",size=18)
    },finally=grDevices::dev.off());check_pdf(p,length(tables))
    esc<-function(x){x<-gsub("\\","\\\\",x,fixed=TRUE);x<-gsub("{","\\{",x,fixed=TRUE);gsub("}","\\}",x,fixed=TRUE)}
    rt<-c("{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 Arial;}}\\landscape\\paperw23040\\paperh14400\\margl600\\margr600\\margt600\\margb600\\f0\\fs36")
    for(k in seq_along(tables)){
      if(k>1)rt<-c(rt,"\\page");tt<-tables[[k]];rt<-c(rt,paste0("\\pard\\b\\fs44 ",esc(title),"\\par\\fs38 ",esc(names(tables)[k]),"\\b0\\par\\par"));rows<-rbind(names(tt),as.matrix(tt))
      ends<-if(ncol(tt)==5)c(6300,10100,13900,17700,21500)else if(ncol(tt)==4)c(6800,11700,16600,21500)else round(seq(7000,21500,length.out=ncol(tt)))
      for(i in seq_len(nrow(rows))){bd<-paste0(if(i==1)"\\clbrdrt\\brdrs\\brdrw15\\clbrdrb\\brdrs\\brdrw10"else"",if(i==nrow(rows))"\\clbrdrb\\brdrs\\brdrw15"else"")
        rt<-c(rt,paste0("\\trowd\\trgaph60\\trkeep",paste0(bd,"\\cellx",ends,collapse=""),
          paste(vapply(seq_len(ncol(tt)),function(j)paste0("\\pard\\intbl",if(j==1)"\\ql "else"\\qr ","\\fs36 ",if(i==1)"\\b "else"\\b0 ",esc(rows[i,j]),"\\cell"),character(1)),collapse=""),"\\row"))}
      rt<-c(rt,paste0("\\pard\\fs36 ",paste(esc(notes),collapse="\\par "),"\\par"))
    };txt_out(c(rt,"}"),paste0("to_share/",stem,".rtf"))
  }
  make_visuals<-function(met,dif,cal,bin,cur,census){
    n<-census$n[census$cohort=="common"];events<-census$hospital_deaths[census$cohort=="common"];rate<-events/n
    context<-sprintf("Shared eICU T48 cohort: n=%d; hospital deaths=%d (%.1f%%)",n,events,100*rate)
    shown<-c("A_raw","C_raw","APACHE_IV","APACHE_IVa");ltys<-c(3,1,2,4);labs<-c("A: baseline","C: continuous dynamics","APACHE IV","APACHE IVa")
    fmtci<-function(x,lo,hi,d=3){ifelse(!is.finite(x),"NA",ifelse(!is.finite(lo)|!is.finite(hi),sprintf(paste0("%.",d,"f (NA)"),x),
      sprintf(paste0("%.",d,"f (%.",d,"f, %.",d,"f)"),x,lo,hi)))}
    for(kind in c("ROC","PR"))save_figure(paste0("Figure_S_eICU_APACHE_",kind),local({kk<-kind;function(){
      par(mar=c(5,5.2,4.7,1.4),oma=c(2.6,0,0,0),cex=1,cex.axis=1,cex.lab=1,las=1,family="sans")
      plot(NA,xlim=c(0,1),ylim=c(0,1),xlab=if(kk=="ROC")"False-positive rate"else"Recall",ylab=if(kk=="ROC")"Sensitivity"else"Precision",xaxs="i",yaxs="i",bty="l")
      if(kk=="ROC")abline(0,1,col="#B2B7BD",lty=3)else abline(h=rate,col="#B2B7BD",lty=3)
      for(j in seq_along(shown)){
        z<-cur[cur$cohort=="common"&cur$model==shown[j],,drop=FALSE]
        if(kk=="ROC")lines(c(0,z$fpr),c(0,z$recall),lwd=2.6,col=model_colors[shown[j]],lty=ltys[j])else{
          # At each recall interval use precision after adding the entire tied-score group: AP convention.
          prev<-c(0,head(z$recall,-1L));segments(prev,z$precision,z$recall,z$precision,lwd=2.6,col=model_colors[shown[j]],lty=ltys[j])
          if(nrow(z)>1L)segments(head(z$recall,-1L),head(z$precision,-1L),head(z$recall,-1L),tail(z$precision,-1L),lwd=1.5,col=model_colors[shown[j]],lty=ltys[j])
        }
      }
      legend(if(kk=="ROC")"bottomright"else"topright",legend=labs,col=model_colors[shown],lty=ltys,lwd=2.6,bty="n",cex=1)
      mtext(if(kk=="ROC")"Native APACHE benchmarks: discrimination"else"Native APACHE benchmarks: precision-recall",3,line=3,cex=1.1,font=2)
      mtext(context,3,line=1.3,cex=.9)
      mtext("Raw frozen predictions; different admission and T48 information horizons",1,outer=TRUE,line=.3,cex=.9)
      mtext(if(kk=="PR")"Dashed horizontal line: event rate; AP uses tied-score steps"else"A-D results and paired confidence intervals are reported in the tables",1,outer=TRUE,line=1.5,cex=.9)
    }}))
    save_figure("Figure_S_eICU_APACHE_calibration",function(){
      mods<-c("C_raw","APACHE_IV","APACHE_IVa");b<-bin[bin$cohort=="common"&bin$model%in%mods,]
      lim<-min(1,max(.25,ceiling(max(c(b$predicted,b$upper),na.rm=TRUE)*10)/10))
      par(mar=c(5,5.2,4.7,1.4),oma=c(2.6,0,0,0),cex=1,cex.axis=1,cex.lab=1,las=1,family="sans")
      plot(NA,xlim=c(0,lim),ylim=c(0,lim),xlab="Mean predicted hospital risk",ylab="Observed hospital mortality",bty="l",xaxs="i",yaxs="i",axes=FALSE)
      ticks<-pretty(c(0,lim),n=3);ticks<-ticks[ticks>=0&ticks<=lim+1e-10];axis(1,at=ticks);axis(2,at=ticks);box(bty="l")
      abline(0,1,col="#B2B7BD",lty=3)
      for(j in seq_along(mods)){z<-b[b$model==mods[j],];segments(z$predicted,z$lower,z$predicted,z$upper,col=model_colors[mods[j]],lwd=1.5)
        lines(z$predicted,z$observed,col=model_colors[mods[j]],lty=j,lwd=2);points(z$predicted,z$observed,col=model_colors[mods[j]],pch=c(16,17,15)[j],cex=1.1)}
      legend("topleft",legend=c("C: continuous dynamics","APACHE IV","APACHE IVa"),col=model_colors[mods],lty=1:3,pch=c(16,17,15),bty="n",cex=1)
      mtext("Native APACHE and raw C: grouped calibration",3,line=3,cex=1.1,font=2);mtext(context,3,line=1.3,cex=.9)
      mtext("Prediction deciles, ties merged; patient-level 95% Wilson intervals",1,outer=TRUE,line=.3,cex=.9)
      mtext("Descriptive T48-selected population; no APACHE recalibration",1,outer=TRUE,line=1.5,cex=.9)
    })
    tt<-list();dt<-list()
    for(mode in c("hospital_cluster","patient"))for(stage in c("raw","updated")){
      models<-if(stage=="raw")primary_models else c(paste0(arms,"_updated"),"APACHE_IV","APACHE_IVa")
      tab<-data.frame(Model=map$label[match(models,map$model)],check.names=FALSE)
      for(m in metric_names[1:4]){z<-met[met$cohort=="common"&met$resampling==mode&met$metric==m,];z<-z[match(models,z$model),]
        tab[[if(m=="AUPRC")"AP"else m]]<-fmtci(z$estimate,z$lower,z$upper)}
      key<-paste(if(stage=="raw")"Primary: raw frozen"else"Secondary: cross-fitted update",if(mode=="hospital_cluster")"/ hospital bootstrap"else"/ patient bootstrap")
      tt[[key]]<-tab
      want<-c(paste0(c("C","D"),"_",stage));comparisons<-as.vector(t(outer(want,c("APACHE_IV","APACHE_IVa"),paste,sep=" minus ")))
      dtab<-data.frame(Comparison=gsub("APACHE_","",gsub("_raw"," raw",gsub("_updated"," updated",comparisons))),check.names=FALSE)
      for(m in metric_names[1:4]){z<-dif[dif$cohort=="common"&dif$resampling==mode&dif$metric==m,];z<-z[match(comparisons,z$comparison),]
        dtab[[if(m=="AUPRC")"AP"else m]]<-fmtci(z$estimate,z$lower,z$upper)}
      dt[[key]]<-dtab
    }
    three_line(tt,"Native APACHE benchmark performance","Table_S_eICU_APACHE_performance",c(context,
      "Estimates (95% paired bootstrap intervals). AP = average precision. All A-D variants are retained.",
      "Updated models use Step14 hospital-blocked intercept updates; APACHE probabilities remain native.",
      "Admission and T48 information horizons differ. Intervals condition on fixed predictions."))
    three_line(dt,"Paired differences versus native APACHE","Table_S_eICU_APACHE_paired_differences",c(context,
      "Difference = C or D minus APACHE IV/IVa. AUROC/AP: higher is better; Brier/LogLoss: lower is better.",
      "95% paired bootstrap intervals; no model or calibration refitting. A/B contrasts are in the CSV.",
      "Raw comparisons are primary; updated comparisons are secondary and do not establish algorithm superiority."))
    ct<-list()
    for(stage in c("raw","updated")){
      models<-if(stage=="raw")primary_models else paste0(arms,"_updated");tab<-data.frame(Model=map$label[match(models,map$model)],check.names=FALSE)
      q<-cal[cal$cohort=="common"&cal$inference=="hospital_robust",]
      for(param in c("intercept_offset","slope")){z<-q[q$parameter==param,];z<-z[match(models,z$model),];tab[[if(param=="slope")"Slope (95% CI)"else"Offset intercept (95% CI)"]]<-fmtci(z$estimate,z$lower,z$upper,2)}
      z<-met[met$cohort=="common"&met$resampling=="hospital_cluster"&met$metric=="Mean_prediction",];tab[["Mean risk (%)"]]<-sprintf("%.2f",100*z$estimate[match(models,z$model)])
      ct[[if(stage=="raw")"Raw frozen models and native APACHE"else"Secondary: existing cross-fitted intercept updates"]]<-tab
    }
    three_line(ct,"Calibration in the shared T48 cohort","Table_S_eICU_APACHE_calibration",c(context,
      "Hospital-cluster robust Wald intervals. Offset intercept fixes slope at 1; slope is jointly fitted.",
      "Calibration regressions diagnose predictions only; they do not replace model probabilities.",
      "NA denotes unstable estimation. Joint intercepts and model-based intervals are provided in the CSV.",
      "Native APACHE risks target admission populations; T48 conditioning changes the evaluation population."))
  }

  if(isTRUE(getOption("ami_step18.functions_only",FALSE)))return(invisible(list(main=main,preflight=preflight,aligned_native=aligned_native,benchmark_cohorts=benchmark_cohorts,weighted_metrics=weighted_metrics,evaluate_benchmark=evaluate_benchmark,calibration=calibration,make_visuals=make_visuals)))
  status<-"FAILED";current_module<-"input verification";failure_message<-NULL
  tryCatch(withCallingHandlers(main(),warning=function(w){add_warning(conditionMessage(w),current_module);invokeRestart("muffleWarning")}),error=function(err){
    status<<-"FAILED";failure_message<<-conditionMessage(err);issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step18_error_detail.txt")})
  flush_warnings()
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step18",
    "COMPLETED_FOR_REVIEW表示原生APACHE背景基准的配对比较完成；不表示已重建MIMIC六项原版评分或证实实时T48可用。",
    "FAILED表示本步未完整完成；检查日志。上游队列和预测保持原样。"),"to_share/Step18_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step18_sessionInfo.txt");logmsg(paste0("Step18状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step18_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share)else{
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step18_feedback.zip"),"\n",sep="")else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
