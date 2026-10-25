# AMI-EDRS revision | Step7: T48 outcome-free phenotyping and nested four-arm elastic-net validation
# 运行：source(file.choose(), encoding="UTF-8")。无需T7；使用Step6本地输入。
# 固定Step7输出，无时间戳；按输入/算法/包版本核对并复用完成的内外折缓存。
# 患者数据、模型与评分参考仅留local_only；反馈ZIP仅含聚合结果。
local({
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MAKE_PNG=TRUE,MAKE_ZIP=TRUE,RESUME=TRUE)
  cfg<-utils::modifyList(cfg,getOption("ami_step7.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step7")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step7输出目录。")
  if (is_link(out)) stop("Step7输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP7")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step7目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP7_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP7_MANAGED_OUTPUT")) {
    stop("Step7输出目录的归属标记不正确。")
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
        if (unlink(p) != 0L) stop("不能覆盖旧Step7输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step7_run_log.txt")
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
  engine<-NULL
  main_domains<-c("hemodynamic","respiratory_neurologic","renal_metabolic","inflammatory_coagulopathic")
  windows<-c("0-6h","0-24h","0-48h")
  model_names<-c(A="Baseline",B="Baseline + phenotype",C="Baseline + dynamics",D="Baseline + dynamics + phenotype")
  plan<-list(version="Step7_v1",landmark="T48",outer=5L,inner=5L,seed=2026L,
    alpha=c(.1,.5,1),lambda=10^seq(1,-4,length.out=31),criterion="pooled inner-assessment log loss",
    k=2:6,min_cluster_fraction=.05,min_cluster_n=25L,min_silhouette=.05,min_stability_ARI=.6,
    stability_repeats=10L,stability_fraction=.8,silhouette_max_n=1000L,nstart=20L,iter_max=100L,
    min_reference_n=20L,min_domain_fraction=.5,preprocess_min_n=20L,max_missing=.95,
    cluster_missing_indicators=FALSE,model_missing_indicators=TRUE,bootstrap_repeats=2000L)
  plan<-utils::modifyList(plan,getOption("ami_step7.plan",list()))
  with_seed<-function(seed,expr){
    old<-if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))get(".Random.seed",.GlobalEnv)else NULL
    on.exit(if(is.null(old)){if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv)}else assign(".Random.seed",old,.GlobalEnv))
    set.seed(seed);force(expr)
  }
  nearest<-function(x,centers){
    x<-as.matrix(x);centers<-as.matrix(centers)
    if(!identical(colnames(x),colnames(centers))||any(!is.finite(x))||any(!is.finite(centers)))stop("分型输入顺序或数值异常。")
    d<-outer(rowSums(x*x),rowSums(centers*centers),"+")-2*tcrossprod(x,centers)
    max.col(-d,ties.method="first")
  }
  adjusted_rand<-function(a,b){
    t<-table(a,b);choose2<-function(x)x*(x-1)/2;tot<-choose2(sum(t));if(tot==0)return(1)
    s<-sum(choose2(t));aa<-sum(choose2(rowSums(t)));bb<-sum(choose2(colSums(t)));expected<-aa*bb/tot
    denom<-(aa+bb)/2-expected
    if(abs(denom)<1e-12)return(if(identical(outer(a,a,"=="),outer(b,b,"==")))1 else 0)
    (s-expected)/denom
  }
  km_fit<-function(x,k,seed){
    ans<-with_seed(seed,stats::kmeans(x,centers=k,nstart=plan$nstart,iter.max=plan$iter_max,algorithm="Lloyd"))
    if(any(!is.finite(ans$centers))||length(unique(ans$cluster))!=k||(!is.null(ans$ifault)&&ans$ifault!=0))stop("k-means未收敛。")
    ans
  }
  fit_clusters<-function(x,seed){
    x<-as.matrix(x);n<-nrow(x);idx<-with_seed(seed+1L,sort(sample.int(n,min(n,plan$silhouette_max_n))))
    dd<-stats::dist(x[idx,,drop=FALSE]);fits<-list();audit<-list()
    for(k in plan$k){
      result<-tryCatch({
        fit<-km_fit(x,k,seed+k);cl<-nearest(x,fit$centers)
        sz<-tabulate(cl,nbins=k);sil<-if(length(unique(cl[idx]))>1L)mean(cluster::silhouette(cl[idx],dd)[,"sil_width"])else NA_real_
        ari<-vapply(seq_len(plan$stability_repeats),function(r){
          ii<-with_seed(seed+1000L*k+r,sort(sample.int(n,max(k+1L,floor(plan$stability_fraction*n)))))
          sub<-km_fit(x[ii,,drop=FALSE],k,seed+10000L*k+r)
          adjusted_rand(cl,nearest(x,sub$centers))
        },numeric(1))
        med<-stats::median(ari);eligible<-min(sz)>=max(plan$min_cluster_n,ceiling(plan$min_cluster_fraction*n))&&is.finite(sil)&&sil>=plan$min_silhouette&&med>=plan$min_stability_ARI
        fits[[as.character(k)]]<-fit
        data.frame(k=k,min_cluster_n=min(sz),min_cluster_fraction=min(sz)/n,silhouette=sil,
          median_stability_ARI=med,min_stability_ARI=min(ari),eligible=eligible,error="")
      },error=function(e)data.frame(k=k,min_cluster_n=NA_integer_,min_cluster_fraction=NA_real_,silhouette=NA_real_,
        median_stability_ARI=NA_real_,min_stability_ARI=NA_real_,eligible=FALSE,error=conditionMessage(e)))
      audit[[as.character(k)]]<-result
    }
    audit<-do.call(rbind,audit);ok<-audit[audit$eligible,,drop=FALSE]
    if(!nrow(ok)){
      # Prespecified no-supported-partition outcome, not a forced five-class solution.
      centers<-matrix(colMeans(x),nrow=1,dimnames=list("C1",colnames(x)));k<-1L
    }else{
      best<-max(ok$silhouette);k<-min(ok$k[ok$silhouette>=best-.01])
      centers<-fits[[as.character(k)]]$centers
      oo<-do.call(order,as.data.frame(centers));centers<-centers[oo,,drop=FALSE];rownames(centers)<-paste0("C",seq_len(k))
    }
    audit$selected<-audit$k==k
    list(k=k,centers=centers,audit=audit,training_n=n,rule="Eligible silhouette maximum; within 0.01 select smaller k; otherwise k=1/no supported partition")
  }
  score_columns<-function(){unlist(lapply(windows,function(w)paste(main_domains,gsub("-","_",w,fixed=TRUE),"score",sep="__")),use.names=FALSE)}
  fit_state<-function(raw,baseline,feature_plan,seed){
    # No outcome argument: each call receives only the current training patients.
    ref<-engine$fit_reference(raw,seq_len(nrow(raw)),min_observed=plan$min_reference_n)
    sc<-engine$apply_reference(raw,ref,windows,min_fraction=plan$min_domain_fraction)$scores[score_columns()]
    cp<-engine$fit_preprocessor(sc,seq_len(nrow(sc)),max_missing=plan$max_missing,min_observed=plan$preprocess_min_n,add_missing=FALSE)
    cx<-engine$apply_preprocessor(sc,cp)
    cl<-fit_clusters(cx,seed)
    dynamic<-unique(feature_plan$feature[feature_plan$role=="main_continuous"])
    if(length(dynamic)!=21L*4L*3L||!all(dynamic%in%names(raw)))stop("主连续特征与Step6约定不符。")
    list(version="Step7_v1",reference=ref,cluster_preprocessor=cp,cluster=cl,dynamic_columns=dynamic,
      baseline_columns=c("age","sex_male","in_index_icu"),score_columns=score_columns())
  }
  apply_state<-function(raw,baseline,state){
    sc<-engine$apply_reference(raw,state$reference,windows,min_fraction=plan$min_domain_fraction)$scores[state$score_columns]
    cluster_x<-engine$apply_preprocessor(sc,state$cluster_preprocessor);labels<-nearest(cluster_x,state$cluster$centers)
    base<-baseline[state$baseline_columns];rownames(base)<-NULL;rownames(sc)<-NULL
    dyn<-cbind(base,raw[state$dynamic_columns],sc);rownames(dyn)<-NULL
    dummy<-if(state$cluster$k>1L)as.data.frame(matrix(vapply(seq_len(state$cluster$k),function(k)as.numeric(labels==k),numeric(nrow(raw))),nrow=nrow(raw),ncol=state$cluster$k))else data.frame(row.names=seq_len(nrow(raw)))
    if(ncol(dummy))names(dummy)<-paste0("phenotype_C",seq_len(state$cluster$k))
    list(design=list(A=base,B=cbind(base,dummy),C=dyn,D=cbind(dyn,dummy)),scores=sc,cluster=labels)
  }
  fit_path<-function(x,y){
    if(anyNA(y)||!all(y%in%0:1)||length(unique(y))!=2||any(!is.finite(x)))stop("模型训练数据无效。")
    if(ncol(x)<2L)stop("弹性网训练特征不足两列。")
    beta<-list();grid<-list()
    for(a in plan$alpha){
      fit<-glmnet::glmnet(x=x,y=y,family="binomial",alpha=a,lambda=plan$lambda,standardize=FALSE,intercept=TRUE,maxit=100000L,thresh=1e-7)
      if(fit$jerr>0)stop("glmnet参数或训练数据错误：jerr=",fit$jerr)
      bb<-matrix(NA_real_,ncol(x)+1L,length(plan$lambda),dimnames=list(c("(Intercept)",colnames(x)),NULL))
      if(length(fit$lambda)){
        ii<-vapply(fit$lambda,function(l)which.min(abs(log(plan$lambda)-log(l))),integer(1))
        if(anyDuplicated(ii)||max(abs(log(fit$lambda)-log(plan$lambda[ii])))>1e-8)stop("glmnet返回未请求的lambda。")
        bb[,ii]<-as.matrix(stats::coef(fit))
      }
      beta[[length(beta)+1L]]<-bb
      grid[[length(grid)+1L]]<-data.frame(alpha=a,lambda=plan$lambda)
    }
    list(beta=do.call(cbind,beta),grid=do.call(rbind,grid))
  }
  path_predict<-function(path,x){
    if(!identical(rownames(path$beta),c("(Intercept)",colnames(x))))stop("预测矩阵与系数顺序不一致。")
    p<-plogis(cbind(1,x)%*%path$beta)
    if(any(is.infinite(p)|is.nan(p)))stop("预测概率无效。")
    p
  }
  logloss_sum<-function(y,p){
    p<-as.matrix(p);if(nrow(p)!=length(y))stop("损失计算的患者数不一致。")
    valid<-colSums(is.finite(p))==nrow(p);p[]<-pmin(1-1e-15,pmax(1e-15,p))
    ans<-colSums(-y*log(p)-(1-y)*log1p(-p));ans[!valid]<-Inf;ans
  }
  select_hyper<-function(loss,grid){
    if(!any(is.finite(loss)))stop("没有在全部内折收敛的候选模型。")
    best<-min(loss);ii<-which(loss<=best+1e-10)
    ii[order(-grid$lambda[ii],-grid$alpha[ii])][1]
  }
  fit_arm<-function(train,test,y){
    pp<-engine$fit_preprocessor(train,seq_len(nrow(train)),max_missing=plan$max_missing,
      min_observed=plan$preprocess_min_n,add_missing=TRUE)
    x<-engine$apply_preprocessor(train,pp);xx<-engine$apply_preprocessor(test,pp)
    path<-fit_path(x,y)
    list(preprocessor=pp,path=path,predictions=path_predict(path,xx))
  }
  curves<-function(y,p){
    oo<-order(p,decreasing=TRUE);ys<-y[oo];ps<-p[oo];ends<-which(!duplicated(ps,fromLast=TRUE))
    tp<-cumsum(ys)[ends];fp<-ends-tp
    data.frame(threshold=ps[ends],tpr=tp/sum(y),fpr=fp/sum(1-y),precision=tp/ends,recall=tp/sum(y))
  }
  metrics<-function(y,p){
    if(any(!is.finite(p))||any(p<0|p>1)||anyNA(y)||!all(y%in%0:1))stop("性能输入无效。")
    if(length(unique(y))<2)return(c(AUROC=NA_real_,AUPRC_AP=NA_real_,Brier=mean((p-y)^2),LogLoss=logloss_sum(y,matrix(p,ncol=1))/length(y)))
    n1<-sum(y);n0<-length(y)-n1;auc<-(sum(rank(p,ties.method="average")[y==1])-n1*(n1+1)/2)/(n1*n0)
    cv<-curves(y,p);ap<-sum(diff(c(0,cv$recall))*cv$precision)
    c(AUROC=auc,AUPRC_AP=ap,Brier=mean((p-y)^2),LogLoss=unname(logloss_sum(y,matrix(p,ncol=1))/length(y)))
  }
  calibration<-function(y,p){
    lp<-qlogis(pmin(1-1e-6,pmax(1e-6,p)))
    tryCatch({f0<-stats::glm(y~1+offset(lp),family=stats::binomial());f1<-stats::glm(y~lp,family=stats::binomial())
      if(!f0$converged||!f1$converged||any(!is.finite(c(coef(f0),coef(f1))))||isTRUE(f0$boundary)||isTRUE(f1$boundary))stop("Calibration fit failed")
      c(intercept_offset=unname(coef(f0)[1]),intercept_joint=unname(coef(f1)[1]),slope_joint=unname(coef(f1)[2]))
    },error=function(e)c(intercept_offset=NA_real_,intercept_joint=NA_real_,slope_joint=NA_real_))
  }
  evaluate_oof<-function(y,p){
    arms<-colnames(p);point<-sapply(arms,function(a)metrics(y,p[,a]));boot<-array(NA_real_,c(plan$bootstrap_repeats,nrow(point),ncol(point)),dimnames=list(NULL,rownames(point),arms))
    with_seed(plan$seed+9000L,for(i in seq_len(plan$bootstrap_repeats)){
      ii<-sample.int(length(y),replace=TRUE)
      for(a in arms)boot[i,,a]<-metrics(y[ii],p[ii,a])
    })
    out<-list();for(a in arms)for(m in rownames(point)){
      ci<-stats::quantile(boot[,m,a],c(.025,.975),na.rm=TRUE,names=FALSE)
      out[[length(out)+1L]]<-data.frame(model=a,metric=m,estimate=point[m,a],lower=ci[1],upper=ci[2],valid_bootstrap=sum(is.finite(boot[,m,a])))
    }
    diff<-list();for(pair in list(c("C","A"),c("D","C"),c("B","A")))for(m in rownames(point)){
      v<-boot[,m,pair[1]]-boot[,m,pair[2]];ci<-stats::quantile(v,c(.025,.975),na.rm=TRUE,names=FALSE)
      diff[[length(diff)+1L]]<-data.frame(comparison=paste(pair,collapse=" minus "),metric=m,estimate=point[m,pair[1]]-point[m,pair[2]],lower=ci[1],upper=ci[2],valid_bootstrap=sum(is.finite(v)))
    }
    list(metrics=do.call(rbind,out),differences=do.call(rbind,diff),calibration=do.call(rbind,lapply(arms,function(a)data.frame(model=a,t(calibration(y,p[,a]))))))
  }
  hash_object<-function(x){p<-tempfile(fileext=".rds");on.exit(unlink(p));saveRDS(x,p,version=2);unname(tools::md5sum(p))}
  cache_read<-function(rel,key){
    p<-safe_path(rel);if(!isTRUE(cfg$RESUME)||!file.exists(p))return(NULL)
    z<-tryCatch(readRDS(p),error=function(e)NULL)
    if(is.null(z)||!identical(z$key,key)||!identical(z$complete,TRUE))return(NULL)
    z$payload
  }
  cache_save<-function(value,rel,key){
    p<-reg(rel);tmp<-reg(paste0(rel,".pending"));saveRDS(list(complete=TRUE,key=key,payload=value),tmp,compress="gzip")
    if(!file.rename(tmp,p))stop("不能更新缓存；请关闭占用输出的程序。")
    invisible(value)
  }
  reference_audit<-function(state){sp<-state$reference$spec;sp$training_observed_n<-vapply(state$reference$reference,function(x)x$n,integer(1));sp$enabled<-vapply(state$reference$reference,function(x)x$enabled,logical(1));sp}
  score_audit<-function(sc,role){data.frame(role=role,feature=names(sc),n=nrow(sc),observed_n=vapply(sc,function(x)sum(!is.na(x)),integer(1)),stringsAsFactors=FALSE)}
  run_split<-function(b,tr,va,seed,chosen=NULL){
    state<-fit_state(b$raw_numeric[tr,,drop=FALSE],b$baseline[tr,,drop=FALSE],b$feature_plan,seed)
    train<-apply_state(b$raw_numeric[tr,,drop=FALSE],b$baseline[tr,,drop=FALSE],state)
    assess<-apply_state(b$raw_numeric[va,,drop=FALSE],b$baseline[va,,drop=FALSE],state)
    y<-b$outcomes$event_primary;loss<-list();pred<-list();models<-list();grid<-NULL
    for(a in names(model_names)){
      fit<-fit_arm(train$design[[a]],assess$design[[a]],y[tr])
      if(!is.null(grid)&&!identical(grid,fit$path$grid))stop("调参网格不一致。")
      grid<-fit$path$grid;loss[[a]]<-logloss_sum(y[va],fit$predictions)
      if(!is.null(chosen)){
        if(any(!is.finite(fit$predictions[,chosen[[a]]])))stop("内折选中参数在外训练折未收敛：",a)
        pred[[a]]<-fit$predictions;models[[a]]<-list(preprocessor=fit$preprocessor,
          coefficients=fit$path$beta[,chosen[[a]]],alpha=grid$alpha[chosen[[a]]],lambda=grid$lambda[chosen[[a]]])
      }
    }
    result<-list(training_n=length(tr),assessment_n=length(va),assessment_keys=b$metadata$patient_key[va],
      loss=loss,grid=grid,k=state$cluster$k,cluster_audit=state$cluster$audit,reference_audit=reference_audit(state),
      score_audit=rbind(score_audit(train$scores,"training"),score_audit(assess$scores,"assessment")))
    if(!is.null(chosen)){
      result$predictions_all<-pred;result$models<-models;result$state<-state
      result$predictions<-sapply(names(model_names),function(a)pred[[a]][,chosen[[a]]])
      result$assessment_cluster<-assess$cluster
    }
    result
  }
  predict_frozen<-function(raw,baseline,object){
    state<-object$state;applied<-apply_state(raw,baseline,state)
    p<-matrix(sapply(names(object$models),function(a){m<-object$models[[a]];xx<-engine$apply_preprocessor(applied$design[[a]],m$preprocessor)
      if(!identical(names(m$coefficients),c("(Intercept)",colnames(xx))))stop("冻结模型系数顺序不一致。")
      as.numeric(plogis(cbind(1,xx)%*%m$coefficients))}),nrow=nrow(raw),dimnames=list(NULL,names(object$models)))
    if(any(!is.finite(p)))stop("冻结模型未生成有效概率。")
    list(probability=p,phenotype=applied$cluster,scores=applied$scores)
  }
  fit_final<-function(b,chosen,seed){
    state<-fit_state(b$raw_numeric,b$baseline,b$feature_plan,seed)
    ap<-apply_state(b$raw_numeric,b$baseline,state);models<-list()
    for(a in names(model_names)){
      fit<-fit_arm(ap$design[[a]],ap$design[[a]][1,,drop=FALSE],b$outcomes$event_primary);j<-chosen[[a]]
      if(any(!is.finite(fit$path$beta[,j])))stop("最终选中参数未收敛：",a)
      models[[a]]<-list(preprocessor=fit$preprocessor,coefficients=fit$path$beta[,j],alpha=fit$path$grid$alpha[j],lambda=fit$path$grid$lambda[j])
    }
    list(version="Step7_v1",endpoint="30_day_all_cause_death_after_T48",landmark="T48",plan=plan,
      state=state,models=models,training_n=nrow(b$metadata),note="Full-MIMIC refit for later external evaluation; not an independently evaluated training performance")
  }
  check_bundle_folds<-function(b,f){
    if(!identical(b$version,"Step6_v1")||nrow(b$metadata)!=5133L||sum(b$outcomes$event_primary)!=630||
      any(b$metadata$source!="MIMIC")||any(b$metadata$landmark!="T48")||any(b$outcomes$endpoint!="30_day_all_cause_death_after_landmark"))stop("主T48队列或终点不符。")
    keys<-b$metadata$patient_key
    if(anyNA(keys)||anyDuplicated(keys)||anyDuplicated(f$outer$patient_key)||!setequal(keys,f$outer$patient_key)||f$k!=5L)stop("外折患者键不匹配。")
    outer<-f$outer$outer_fold[match(keys,f$outer$patient_key)]
    if(anyNA(outer)||!setequal(outer,1:5))stop("外折缺失。")
    for(o in 1:5){z<-f$inner[[as.character(o)]]
      if(anyDuplicated(z$patient_key)||!setequal(z$patient_key,keys[outer!=o])||anyNA(z$inner_fold)||!setequal(z$inner_fold,1:5))stop("内折与外训练患者不匹配。")
    }
    outer
  }
  main<-function(){
    missing_pkgs<-c("glmnet","cluster")[!vapply(c("glmnet","cluster"),requireNamespace,logical(1),quietly=TRUE)]
    if(length(missing_pkgs))stop("缺少R包：",paste(missing_pkgs,collapse=", "),"。请先运行 install.packages(c('glmnet','cluster'))，再运行本脚本。")
    step6<-file.path(cfg$OUTPUT_PARENT,"Step6");sp<-file.path(step6,"to_share/Step6_STATUS.txt")
    if(!file.exists(sp)||!"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(sp,warn=FALSE))stop("先成功完成Step6。")
    paths<-file.path(step6,"local_only",c("MIMIC_T48_model_inputs.rds","MIMIC_nested_folds.rds","Step6_score_engine.R","Step6_analysis_contract.rds"))
    if(!all(file.exists(paths)))stop("Step6本地输入不完整。")
    h<-unname(tools::md5sum(paths));csv_out(data.frame(file=paths,md5=h),"to_share/01_inputs.csv")
    lineage<-utils::read.csv(file.path(step6,"to_share/01_inputs.csv"),stringsAsFactors=FALSE)
    if(!all(c("file","md5")%in%names(lineage)))stop("Step6输入审计格式不符。")
    for(i in seq_len(nrow(lineage)))if(!file.exists(lineage$file[i])||unname(tools::md5sum(lineage$file[i]))!=lineage$md5[i])stop("Step6上游结果已变化，请先重新完成Step6。")
    ee<-new.env(parent=baseenv());sys.source(paths[3],envir=ee);engine<<-ee$step6_score_engine
    contract<-readRDS(paths[4]);if(!identical(contract$version,"Step6_v1")||!identical(contract$main_domains,main_domains))stop("Step6评分约定不一致。")
    logmsg("小型预检：折内拟合隔离、无结局分型、同分AUPRC、glmnet路径与单例/批量预测一致性。")
    preflight()
    txt_out("PASS: independent training-slice fitting; deterministic clustering; held-out invariance; tied AP; glmnet prediction equality; single/batch frozen inference.","to_share/02_preflight.txt")
    b<-readRDS(paths[1]);f<-readRDS(paths[2])[["MIMIC_T48"]]
    outer<-check_bundle_folds(b,f);y<-b$outcomes$event_primary;n<-length(y)
    funs<-c("nearest","adjusted_rand","km_fit","fit_clusters","fit_state","apply_state","fit_path","fit_arm","run_split","predict_frozen","fit_final","logloss_sum","select_hyper","score_columns","with_seed","path_predict")
    key<-hash_object(list(inputs=h,plan=plan,functions=lapply(funs,function(z)body(get(z))),
      packages=vapply(c("glmnet","cluster"),function(p)as.character(utils::packageVersion(p)),character(1)),R=as.character(getRversion())))
    txt_out(capture.output(dput(plan)),"to_share/02_frozen_analysis_plan.txt")
    csv_out(data.frame(n=n,events=sum(y),outer_folds=5,inner_folds=5,models=4,fit_scope="MIMIC T48 only; no eICU reads"),"to_share/02_cohort_check.csv")
    pp<-matrix(NA_real_,n,4,dimnames=list(NULL,names(model_names)));gridpred<-lapply(names(model_names),function(a)matrix(NA_real_,n,length(plan$alpha)*length(plan$lambda)));names(gridpred)<-names(model_names)
    audits<-list();refs<-list();cov<-list();tuning<-list();outerfits<-list();krows<-list();foldclass<-integer(n)
    collect<-function(z,scope){
      audits[[scope]]<<-cbind(scope=scope,z$cluster_audit)
      refs[[scope]]<<-cbind(scope=scope,z$reference_audit)
      cov[[scope]]<<-cbind(scope=scope,z$score_audit)
      krows[[scope]]<<-data.frame(scope=scope,k=z$k,training_n=z$training_n,assessment_n=z$assessment_n)
    }
    for(o in 1:5){
      tr<-which(outer!=o);va<-which(outer==o);inn<-f$inner[[as.character(o)]];il<-inn$inner_fold[match(b$metadata$patient_key[tr],inn$patient_key)]
      losses<-NULL
      for(j in 1:5){
        scope<-paste0("outer",o,"_inner",j);current_module<<-scope;rel<-paste0("local_only/cache/T48_",scope,".rds")
        z<-cache_read(rel,key)
        if(is.null(z)){
          logmsg(paste0("T48 外折 ",o,"/5，内折 ",j,"/5：重新拟合评分、预处理、分型及四模型。"))
          z<-run_split(b,tr[il!=j],tr[il==j],plan$seed+1000L*o+10L*j);cache_save(z,rel,key)
        }else logmsg(paste0("缓存通过：",scope,"。"))
        if(!identical(z$assessment_keys,b$metadata$patient_key[tr[il==j]]))stop("内折缓存患者顺序不一致。")
        collect(z,scope);if(is.null(losses))losses<-z$loss else for(a in names(model_names))losses[[a]]<-losses[[a]]+z$loss[[a]]
        grid<-z$grid
      }
      chosen<-lapply(losses,select_hyper,grid=grid)
      for(a in names(model_names))tuning[[paste(o,a)]]<-data.frame(scope=paste0("outer",o),model=a,grid,mean_logloss=losses[[a]]/length(tr),converged_all_folds=is.finite(losses[[a]]),selected=seq_len(nrow(grid))==chosen[[a]])
      scope<-paste0("outer",o);current_module<<-scope;rel<-paste0("local_only/cache/T48_",scope,".rds")
      z<-cache_read(rel,key)
      if(is.null(z)){
        logmsg(paste0("T48 外折 ",o,"/5：按内折选择参数，生成未参与拟合患者的预测。"))
        z<-run_split(b,tr,va,plan$seed+1000L*o+99L,chosen=chosen);cache_save(z,rel,key)
      }else logmsg(paste0("缓存通过：",scope,"。"))
      if(!identical(z$assessment_keys,b$metadata$patient_key[va]))stop("外折缓存患者顺序不一致。")
      collect(z,scope);pp[va,]<-z$predictions;foldclass[va]<-z$assessment_cluster
      for(a in names(model_names))gridpred[[a]][va,]<-z$predictions_all[[a]]
      outerfits[[scope]]<-list(state=z$state,models=z$models)
    }
    if(any(!is.finite(pp)))stop("折外预测未覆盖全部患者。")
    saveRDS(list(metadata=b$metadata,outcomes=b$outcomes,outer_fold=outer,fold_specific_phenotype=foldclass,prediction=pp),reg("local_only/MIMIC_T48_nested_OOF.rds"),compress="gzip")
    saveRDS(outerfits,reg("local_only/MIMIC_T48_outer_models.rds"),compress="gzip")
    # Final tuning is separate from the already locked nested OOF assessment.
    finalchosen<-lapply(gridpred,function(p)select_hyper(logloss_sum(y,p),grid))
    for(a in names(model_names))tuning[[paste0("final",a)]]<-data.frame(scope="full_MIMIC_tuning_only",model=a,grid,mean_logloss=logloss_sum(y,gridpred[[a]])/n,converged_all_folds=is.finite(logloss_sum(y,gridpred[[a]])),selected=seq_len(nrow(grid))==finalchosen[[a]])
    current_module<<-"full MIMIC refit"
    final<-cache_read("local_only/cache/T48_final.rds",key)
    if(is.null(final)){
      logmsg("五个外折预测已保存；在全部MIMIC T48重拟合冻结模型，供后续外部评价。")
      final<-fit_final(b,finalchosen,plan$seed+99999L);cache_save(final,"local_only/cache/T48_final.rds",key)
    }
    saveRDS(final,reg("local_only/MIMIC_T48_frozen_pipeline.rds"),compress="gzip")
    txt_out(readLines(paths[3],warn=FALSE),"local_only/Step7_score_engine.R")
    prediction_code<-c("# source Step7_score_engine.R first; then predict_step7(raw, baseline, object).",
      "predict_step7 <- local({", "engine <- step6_score_engine",paste0("plan <- ",paste(deparse(plan),collapse="\n")),
      'windows <- c("0-6h","0-24h","0-48h")',
      paste0("nearest <- ",paste(deparse(nearest),collapse="\n")),paste0("apply_state <- ",paste(deparse(apply_state),collapse="\n")),
      paste0("predict_frozen <- ",paste(deparse(predict_frozen),collapse="\n")),"predict_frozen", "})")
    txt_out(prediction_code,"local_only/Step7_predict.R")
    a<-predict_frozen(b$raw_numeric[1:3,,drop=FALSE],b$baseline[1:3,,drop=FALSE],final)
    one<-predict_frozen(b$raw_numeric[2,,drop=FALSE],b$baseline[2,,drop=FALSE],final)
    if(!isTRUE(all.equal(as.numeric(a$probability[2,]),as.numeric(one$probability[1,]),tolerance=1e-12)))stop("最终模型单例/批量不一致。")
    audits$full_MIMIC_final<-cbind(scope="full_MIMIC_final",final$state$cluster$audit)
    refs$full_MIMIC_final<-cbind(scope="full_MIMIC_final",reference_audit(final$state))
    krows$full_MIMIC_final<-data.frame(scope="full_MIMIC_final",k=final$state$cluster$k,training_n=n,assessment_n=0)
    csv_out(do.call(rbind,audits),"to_share/03_cluster_selection.csv");csv_out(do.call(rbind,krows),"to_share/03_selected_k.csv")
    csv_out(do.call(rbind,refs),"to_share/03_reference_availability.csv");csv_out(do.call(rbind,cov),"to_share/03_fold_score_coverage.csv")
    csv_out(do.call(rbind,tuning),"to_share/04_tuning_logloss.csv")
    current_module<<-"OOF uncertainty and figures"
    logmsg(paste0("计算四模型折外指标及配对差值；患者级bootstrap ",plan$bootstrap_repeats,"次。"))
    ev<-evaluate_oof(y,pp)
    csv_out(ev$metrics,"to_share/05_OOF_metrics.csv");csv_out(ev$differences,"to_share/05_paired_differences.csv");csv_out(ev$calibration,"to_share/05_calibration.csv")
    csv_out(do.call(rbind,lapply(1:5,function(o)do.call(rbind,lapply(names(model_names),function(a)data.frame(outer_fold=o,model=a,n=sum(outer==o),events=sum(y[outer==o]),t(metrics(y[outer==o],pp[outer==o,a]))))))),"to_share/05_metrics_by_outer_fold.csv")
    make_visuals(y,pp,ev,do.call(rbind,krows))
    txt_out(c("Step7完成MIMIC T48主分析：5513/5411早期队列留待后续步骤；本步T48为5133人、630个事件。",
      "A：年龄、性别、T48 ICU状态；B：A+分型；C：A+连续动态信息；D：C+分型。四臂均为同一弹性网管线、同一网格、同一患者与分折。",
      "所有评分参考、填补、筛特征、标准化、分型及回归在各内训练折重新拟合；外验证患者只用于评价。",
      "未收敛候选的合并损失记为Inf并排除选择，不用路径端点外推；选中参数在外训练或最终拟合未收敛则停止。",
      "调参按内折合并log loss最小值；正则化lambda与alpha网格运行前固定，不按外折AUROC筛选。",
      "分型仅使用四域累计窗口评分，不使用死亡结局、ICU状态或测量次数。缺失指示不进入聚类，但填补仍可能受观察过程影响。",
      "k=2至6；最低簇规模max(25人,5%)、平均轮廓系数>=0.05、中位稳定性ARI>=0.60。合格者选最大轮廓系数，差<=0.01选较小k。",
      "上述阈值是本次预设操作标准，不是公认生物分型阈值。均不合格时记录k=1/无支持分型，B等同A、D等同C。",
      "稳定性为固定该训练折评分和预处理后的80%患者子抽样、10次重复；只反映给定表示下聚类稳定性，不是整个评分管线的bootstrap。",
      "轮廓系数在至多1000名训练患者的固定随机子样本计算。C标签按质心字典序确定，无死亡排序或等级含义。",
      "各折C1不能合并解释为同一临床亚型；本步合并概率评价预测管线，分类OR/HR需后续独立的分型发现/评价设计。",
      "AUPRC采用同分阈值分组的average precision；曲线展示与指标名称明确，不混用梯形PR面积。",
      "95%CI为固定折外预测的患者级bootstrap，配对差值使用同一重抽样；未覆盖重新训练和开发流程的全部不确定性。",
      "校准截距同时报告固定斜率1的offset截距与联合拟合的截距/斜率。本步不重新校准OOF风险。",
      "嵌套OOF完成后，五折网格预测用于全MIMIC最终模型调参并重拟合；这些网格最优表现不是新的独立性能结果。",
      "eICU未参与本步训练、调参或分型。最终对象仅用于后续冻结应用，30天模型不能直接称eICU院内死亡的同终点验证。",
      "旧测试集参与开发的历史仍须披露；本步属于重新实施的内部验证。",
      "本步新增补充验证图和三线表；后续完成临床比较与DCA后再整合主Figure4。尚不替代Figure4F或Figure4G。",
      "缓存固定命名；输入、算法或包版本变化会使缓存失效并覆盖重算。中断后直接重跑同一脚本，不要删除Step7目录。",
      "患者预测、结局、分折、评分参考和模型仅留local_only；请回传Step7_feedback.zip。"),"to_share/00_READ_ME_FIRST.txt")
    txt_out(c("https://glmnet.stanford.edu/articles/glmnet.html","https://stat.ethz.ch/R-manual/R-devel/library/cluster/html/silhouette.html",
      "https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html"),"to_share/10_method_sources.txt")
    status<<-"COMPLETED_FOR_REVIEW"
  }
  synthetic_bundle<-function(n=160L){
    with_seed(71L,{
      sp<-engine$specification();core<-unique(sp$variable[sp$tier=="main"]);fp<-list()
      for(w in windows)for(v in core)for(s in c("mean_value","sd_value","slope_per_hour","abnormal_fraction"))fp[[length(fp)+1L]]<-data.frame(feature=engine$key(v,w,s),role="main_continuous")
      fp<-do.call(rbind,fp);cols<-unique(c(fp$feature,unlist(lapply(windows,function(w)engine$key(sp$variable,w,sp$stat)))))
      latent<-rep(c(-2,2),length.out=n)+rnorm(n,0,.25);raw<-as.data.frame(matrix(0,n,length(cols)));names(raw)<-cols
      for(nm in cols){v<-latent+rnorm(n,0,.2);if(grepl("__min_value$",nm))v<--v;if(grepl("__abnormal_fraction$",nm))v<-plogis(v);raw[[nm]]<-as.numeric(v)}
      raw[seq(7,n,17),seq(1,ncol(raw),9)]<-NA_real_
      base<-data.frame(age=runif(n,40,85),sex_male=as.numeric(seq_len(n)%%2),in_index_icu=as.numeric(seq_len(n)%%3!=0))
      y<-rbinom(n,1,plogis(-.7+.5*latent));list(version="Step6_v1",metadata=data.frame(patient_key=sprintf("fixture:%04d",1:n),source="MIMIC",landmark="T48"),
        raw_numeric=raw,baseline=base,feature_plan=fp,outcomes=data.frame(event_primary=y,endpoint="30_day_all_cause_death_after_landmark"))
    })
  }
  preflight<-function(){
    old<-plan;on.exit(plan<<-old,add=TRUE)
    plan<<-utils::modifyList(plan,list(k=2:3,min_cluster_n=3L,stability_repeats=2L,nstart=3L,
      min_reference_n=8L,preprocess_min_n=8L,alpha=c(.5,1),lambda=c(1,.3,.1,.03),bootstrap_repeats=20L,silhouette_max_n=120L))
    b<-synthetic_bundle();tr<-1:120;va<-121:160
    s<-fit_state(b$raw_numeric[tr,],b$baseline[tr,],b$feature_plan,102)
    changed<-b;changed$raw_numeric[va,]<-999;changed$outcomes$event_primary<-1-changed$outcomes$event_primary
    s2<-fit_state(changed$raw_numeric[tr,],changed$baseline[tr,],changed$feature_plan,102)
    stopifnot(identical(s,s2),s$cluster$k>=2,adjusted_rand(c(1,1,2,2),c(2,2,1,1))==1,
      abs(metrics(c(1,0,1),c(.5,.5,.2))["AUPRC_AP"]-7/12)<1e-12,
      abs(metrics(c(1,0,1),c(.5,.5,.2))["AUROC"]-.25)<1e-12)
    ap<-apply_state(b$raw_numeric[tr,],b$baseline[tr,],s);aa<-apply_state(b$raw_numeric[va,],b$baseline[va,],s)
    models<-list()
    for(a in names(model_names)){
      f<-fit_arm(ap$design[[a]],aa$design[[a]],b$outcomes$event_primary[tr]);j<-3L
      models[[a]]<-list(preprocessor=f$preprocessor,coefficients=f$path$beta[,j])
      x<-engine$apply_preprocessor(aa$design[[a]],f$preprocessor)
      stopifnot(max(abs(f$predictions[,j]-plogis(cbind(1,x)%*%models[[a]]$coefficients)))<1e-12)
    }
    obj<-list(state=s,models=models);p<-predict_frozen(b$raw_numeric[va,],b$baseline[va,],obj)
    one<-predict_frozen(b$raw_numeric[va[2],,drop=FALSE],b$baseline[va[2],,drop=FALSE],obj)
    stopifnot(isTRUE(all.equal(as.numeric(p$probability[2,]),as.numeric(one$probability[1,]),tolerance=1e-12)))
    # If no candidate meets the declared criteria, do not force a partition.
    plan$min_silhouette<<-1.1
    null<-fit_state(b$raw_numeric[tr,],b$baseline[tr,],b$feature_plan,102)
    nd<-apply_state(b$raw_numeric[va,],b$baseline[va,],null)$design
    stopifnot(null$cluster$k==1L,identical(nd$A,nd$B),identical(nd$C,nd$D))
    TRUE
  }
  open_pdf<-function(path,width,height){if(isTRUE(capabilities("cairo")))grDevices::cairo_pdf(path,width=width,height=height,family="Arial")else grDevices::pdf(path,width=width,height=height,family="Helvetica",useDingbats=FALSE)}
  three_line<-function(tables,title,stem,notes){
    open_pdf(reg(paste0("to_share/",stem,".pdf")),11.69,8.27)
    tryCatch(for(nm in names(tables)){
      tab<-tables[[nm]];nr<-nrow(tab);nc<-ncol(tab);grid::grid.newpage()
      grid::grid.text(title,x=.04,y=.95,just="left",gp=grid::gpar(fontsize=13,fontface="bold"))
      grid::grid.text(nm,x=.04,y=.90,just="left",gp=grid::gpar(fontsize=10))
      xs<-c(.04,seq(.42,.96,length.out=nc-1));top<-.85;dy<-min(.055,.57/max(1,nr))
      line<-function(y,lwd)grid::grid.lines(x=c(.04,.97),y=c(y,y),gp=grid::gpar(lwd=lwd))
      line(top,1.3)
      for(j in 1:nc)grid::grid.text(names(tab)[j],xs[j],top-.022,just=if(j==1)"left"else"right",gp=grid::gpar(fontsize=8.5,fontface="bold"))
      line(top-.045,.8)
      for(i in seq_len(nr))for(j in 1:nc)grid::grid.text(as.character(tab[i,j]),xs[j],top-.035-i*dy,just=if(j==1)"left"else"right",gp=grid::gpar(fontsize=8.5))
      bot<-top-.055-nr*dy;line(bot,1.3)
      for(i in seq_along(notes))grid::grid.text(notes[i],.04,bot-.03*i,just="left",gp=grid::gpar(fontsize=8))
    },finally=grDevices::dev.off())
    esc<-function(x){x<-gsub("\\","\\\\",x,fixed=TRUE);x<-gsub("{","\\{",x,fixed=TRUE);gsub("}","\\}",x,fixed=TRUE)}
    rt<-c("{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 Arial;}}\\landscape\\paperw16838\\paperh11906\\margl600\\margr600\\f0\\fs20")
    for(nm in names(tables)){
      if(nm!=names(tables)[1])rt<-c(rt,"\\page")
      rt<-c(rt,paste0("\\pard\\b ",esc(title),"\\b0\\par ",esc(nm),"\\par\\par"))
      tab<-tables[[nm]];rows<-rbind(names(tab),as.matrix(tab));nc<-ncol(tab);ends<-round(c(4400,seq(6900,15100,length.out=nc-1)))
      for(i in 1:nrow(rows)){bd<-paste0(if(i==1)"\\clbrdrt\\brdrs\\brdrw15\\clbrdrb\\brdrs\\brdrw10"else"",if(i==nrow(rows))"\\clbrdrb\\brdrs\\brdrw15"else"")
        rt<-c(rt,paste0("\\trowd\\trgaph60\\trkeep",paste0(bd,"\\cellx",ends,collapse=""),paste(vapply(1:nc,function(j)paste0("\\pard\\intbl",if(j==1)"\\ql "else"\\qr ","\\fs16 ",if(i==1)"\\b "else"\\b0 ",esc(rows[i,j]),"\\cell"),character(1)),collapse=""),"\\row"))}
      rt<-c(rt,paste0("\\pard\\fs16 ",paste(esc(notes),collapse="\\par "),"\\par"))
    };txt_out(c(rt,"}"),paste0("to_share/",stem,".rtf"))
  }
  make_visuals<-function(y,p,ev,krows){
    colors<-c(A="#7A8797",B="#4F7FC4",C="#D98C57",D="#8B68A6");cur<-lapply(names(colors),function(a)curves(y,p[,a]));names(cur)<-names(colors)
    bins<-list();rocdata<-list()
    for(a in names(colors)){
      br<-unique(as.numeric(stats::quantile(p[,a],seq(0,1,.1))))
      g<-if(length(br)>1)cut(p[,a],breaks=br,include.lowest=TRUE,labels=FALSE)else rep(1L,length(y))
      bins[[a]]<-do.call(rbind,lapply(sort(unique(g)),function(j)data.frame(model=a,bin=j,n=sum(g==j),events=sum(y[g==j]),predicted=mean(p[g==j,a]),observed=mean(y[g==j]))))
      rocdata[[a]]<-do.call(rbind,lapply(seq(0,1,length.out=501),function(t){pos<-p[,a]>=t;tp<-sum(y[pos]);fp<-sum(1-y[pos]);data.frame(model=a,threshold=t,tp=tp,fp=fp,n=length(y),tpr=tp/sum(y),fpr=fp/sum(1-y),precision=if(sum(pos))tp/sum(pos)else NA_real_)}))
    }
    csv_out(do.call(rbind,bins),"to_share/06_calibration_bins.csv");csv_out(do.call(rbind,rocdata),"to_share/06_fixed_threshold_curves.csv")
    draw<-function(){op<-par(no.readonly=TRUE);on.exit(par(op));par(mfrow=c(2,2),mar=c(4.2,4.3,2.5,1),oma=c(2,0,1,0),family="sans",las=1,cex=.85)
      plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),xlab="False-positive rate",ylab="True-positive rate",bty="l",main="A  Nested out-of-fold ROC",adj=0);abline(0,1,lty=2,col="#BBBBBB")
      for(a in names(colors))lines(c(0,cur[[a]]$fpr),c(0,cur[[a]]$tpr),col=colors[a],lwd=2)
      legend("bottomright",legend=paste0(names(colors),": ",sprintf("%.3f",vapply(names(colors),function(a)metrics(y,p[,a])["AUROC"],numeric(1)))),col=colors,lwd=2,bty="n",cex=.85)
      plot(0,1,type="n",xlim=c(0,1),ylim=c(0,1),xlab="Recall",ylab="Precision",bty="l",main="B  Precision-recall",adj=0);abline(h=mean(y),lty=2,col="#999999")
      for(a in names(colors))lines(c(0,cur[[a]]$recall),c(1,cur[[a]]$precision),col=colors[a],lwd=2,type="S")
      legend("topright",legend=paste0(names(colors),": AP ",sprintf("%.3f",vapply(names(colors),function(a)metrics(y,p[,a])["AUPRC_AP"],numeric(1)))),col=colors,lwd=2,bty="n",cex=.8)
      plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),xlab="Mean predicted probability",ylab="Observed event proportion",bty="l",main="C  Calibration (grouped OOF)",adj=0);abline(0,1,lty=2,col="#BBBBBB")
      for(a in names(colors))lines(bins[[a]]$predicted,bins[[a]]$observed,type="b",pch=16,col=colors[a],lwd=1.5,cex=.7)
      z<-ev$differences[ev$differences$metric=="AUROC",];rr<-range(c(0,z$lower,z$upper));pad<-max(.005,diff(rr)*.15)
      par(mar=c(4.2,6.5,2.5,1));plot(0,0,type="n",xlim=rr+c(-pad,pad),ylim=c(.5,3.5),yaxt="n",xlab="Paired AUROC difference (95% CI)",ylab="",bty="l",main="D  Incremental discrimination",adj=0)
      axis(2,at=3:1,labels=z$comparison,las=1,tick=FALSE);abline(v=0,lty=2,col="#AAAAAA")
      segments(z$lower,3:1,z$upper,3:1,col="#4F7FC4",lwd=2);points(z$estimate,3:1,pch=16,col="#4F7FC4")
      mtext("A: baseline; B: baseline + phenotype; C: baseline + dynamics; D: baseline + dynamics + phenotype",side=1,outer=TRUE,line=.5,cex=.75)
    }
    open_pdf(reg("to_share/Figure_S_T48_nested_validation.pdf"),10,8.2);tryCatch(draw(),finally=grDevices::dev.off())
    if(cfg$MAKE_PNG){grDevices::png(reg("to_share/Figure_S_T48_nested_validation.png"),width=10,height=8.2,units="in",res=400);tryCatch(draw(),finally=grDevices::dev.off())}
    format_ci<-function(z)sprintf("%.3f (%.3f, %.3f)",z$estimate,z$lower,z$upper)
    tab<-data.frame(Model=names(model_names));dif<-data.frame(Comparison=unique(ev$differences$comparison))
    for(m in c("AUROC","AUPRC_AP","Brier","LogLoss")){
      z<-ev$metrics[ev$metrics$metric==m,];tab[[m]]<-format_ci(z[match(tab$Model,z$model),])
      z<-ev$differences[ev$differences$metric==m,];dif[[m]]<-format_ci(z[match(dif$Comparison,z$comparison),])
    }
    csv_out(tab,"to_share/Table_S_T48_nested_performance.csv");csv_out(dif,"to_share/Table_S_T48_paired_increment.csv")
    three_line(list("MIMIC T48: nested OOF estimates and 95% CI"=tab,"Paired differences on the same OOF patients"=dif),
      "Supplementary table. T48 four-model internal validation","Table_S_T48_nested_validation",
      c("A: baseline; B: baseline + phenotype; C: baseline + dynamics; D: baseline + dynamics + phenotype.",
        "AP is average precision with tied scores grouped. Positive AUROC/AP differences and negative Brier/log-loss differences favor the first model.",
        "Intervals use patient bootstrap of fixed OOF predictions; they do not include full pipeline redevelopment uncertainty."))
    kt<-krows[!grepl("inner",krows$scope),];names(kt)<-c("Training scope","Selected k","Training n","Assessment n")
    csv_out(kt,"to_share/Table_S_T48_cluster_choices.csv")
    three_line(list("Separate fold-specific labels; k=1 means no supported partition"=kt),"Supplementary table. Outcome-free cluster selection",
      "Table_S_T48_cluster_choices",c("Candidate selection used only training features. The same numeric label across folds does not identify the same clinical phenotype.","The full-MIMIC refit is for subsequent frozen deployment, not an independently evaluated discovery result."))
    csv_out(data.frame(output=c("Figure_S_T48_nested_validation","Table_S_T48_nested_validation","Table_S_T48_cluster_choices"),
      action="ADD supplementary; final S number assigned on integration",relation="New nested-CV evidence. Main Figure 4 will be reorganized after clinical comparators and DCA; no replacement of Figure 4F/G in this step."),"to_share/11_figure_table_replacement_map.csv")
  }
  if(isTRUE(getOption("ami_step7.functions_only",FALSE)))return(invisible(list(preflight=preflight,synthetic_bundle=synthetic_bundle,
    fit_state=fit_state,apply_state=apply_state,fit_arm=fit_arm,run_split=run_split,fit_final=fit_final,predict_frozen=predict_frozen,
    fit_clusters=fit_clusters,metrics=metrics,curves=curves,evaluate_oof=evaluate_oof,make_visuals=make_visuals,
    check_bundle_folds=check_bundle_folds,main=main,cache_read=cache_read,cache_save=cache_save)))
  status<-"FAILED";current_module<-"input verification"
  failure_message<-NULL
  tryCatch(main(),error=function(err){status<<-"FAILED";failure_message<<-conditionMessage(err)
    issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step7_error_detail.txt")})
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step7",
    "COMPLETED_FOR_REVIEW表示T48嵌套内部验证及全MIMIC冻结对象已生成，等待结果核对；外部验证尚未开展。",
    "FAILED表示不完整，不能使用本次患者级结果建模。"),"to_share/Step7_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step7_sessionInfo.txt")
  logmsg(paste0("Step7状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step7_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share) else {
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step7_feedback.zip"),"\n",sep="") else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
