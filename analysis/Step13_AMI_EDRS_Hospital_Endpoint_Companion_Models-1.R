# AMI-EDRS revision | Step13-1: MIMIC hospital-endpoint companion models and frozen eICU evaluation
# source(file.choose(), encoding="UTF-8")
# Fixed filenames; replaces owned results. Patient-level data and frozen references stay local_only.
local({
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MAKE_PNG=TRUE,MAKE_ZIP=TRUE)
  cfg<-utils::modifyList(cfg,getOption("ami_step13.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step13")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step13输出目录。")
  if (is_link(out)) stop("Step13输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP13")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step13目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP13_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP13_MANAGED_OUTPUT")) {
    stop("Step13输出目录的归属标记不正确。")
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
        if (unlink(p) != 0L) stop("不能覆盖旧Step13输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step13_run_log.txt")
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
    txt_out(if(nrow(z))unlist(Map(function(a,b)c(a,b,""),z$scope,z$message))else"No warnings in Step13.","local_only/Step13_warning_detail.txt")
    a<-if(nrow(z))aggregate(list(n=rep(1L,nrow(z))),list(scope=z$scope),sum)else data.frame(scope=character(),n=integer())
    csv_out(a,"to_share/09_warning_summary.csv")
  }
  check_pdf<-function(p,n){actual<-pdf_pages(p);if(!is.finite(actual)||actual!=n)stop("PDF页数不符：",basename(p),"；期望",n,"页，实际",actual,"页。")
    invisible(actual)}
  pdf_pages<-function(p){
    bytes<-as.integer(readBin(p,"raw",n=file.info(p)$size));bytes[bytes<32L|bytes>126L]<-32L
    txt<-rawToChar(as.raw(bytes));m<-gregexpr("/Type[[:space:]]*/Page([^sA-Za-z]|$)",txt,perl=TRUE)[[1]]
    if(m[1]<0L)NA_integer_ else length(m)
  }
  open_pdf<-function(path,width,height){grDevices::pdf(path,width=width,height=height,family="Helvetica",onefile=TRUE,useDingbats=FALSE,pointsize=18)}
  save_figure<-function(stem,draw,w,h){
    p<-reg(paste0("to_share/",stem,".pdf"));open_pdf(p,w,h);tryCatch(draw(),finally=grDevices::dev.off());check_pdf(p,1L)
    if(cfg$MAKE_PNG){grDevices::png(reg(paste0("to_share/",stem,".png")),width=w,height=h,units="in",res=350,pointsize=18);tryCatch(draw(),finally=grDevices::dev.off())}
  }
  quant<-function(x){x<-x[is.finite(x)];if(!length(x))return(c(median=NA_real_,q25=NA_real_,q75=NA_real_));setNames(as.numeric(quantile(x,c(.5,.25,.75),names=FALSE)),c("median","q25","q75"))}
  fmt_q<-function(x){ifelse(is.finite(x$median),sprintf("%.1f [%.1f, %.1f]",x$median,x$q25,x$q75),"Unavailable")}
  draw_table_pdf<-function(tables,title,path,notes=character()){
    if(!length(tables)||any(vapply(tables,nrow,integer(1))>12L))stop("三线表每页最多12行。")
    open_pdf(path,11.69,8.27)
    tryCatch(for(nm in names(tables)){
      tab<-tables[[nm]];nr<-nrow(tab);nc<-ncol(tab);grid::grid.newpage()
      grid::grid.text(title,.04,.95,just="left",gp=grid::gpar(fontsize=17,fontface="bold"))
      grid::grid.text(nm,.04,.90,just="left",gp=grid::gpar(fontsize=13))
      xs<-c(.04,seq(.27,.96,length.out=nc-1));top<-.85;dy<-min(.052,.48/max(1,nr))
      line<-function(y,w)grid::grid.lines(x=c(.04,.97),y=c(y,y),gp=grid::gpar(lwd=w))
      line(top,1.3)
      for(j in seq_len(nc))grid::grid.text(names(tab)[j],xs[j],top-.022,just=if(j==1)"left"else"right",gp=grid::gpar(fontsize=12,fontface="bold"))
      line(top-.045,.8)
      for(i in seq_len(nr))for(j in seq_len(nc))grid::grid.text(as.character(tab[i,j]),xs[j],top-.035-i*dy,just=if(j==1)"left"else"right",gp=grid::gpar(fontsize=12))
      bot<-top-.055-nr*dy;line(bot,1.3)
      wrap<-unlist(lapply(notes,strwrap,width=112))
      for(i in seq_along(wrap))grid::grid.text(wrap[i],.04,bot-.029*i,just="left",gp=grid::gpar(fontsize=10.5))
      grid::grid.text(paste(match(nm,names(tables)),"/",length(tables)),.97,.035,just="right",gp=grid::gpar(fontsize=10.5))
    },finally=grDevices::dev.off())
    check_pdf(path,length(tables))
  }
  three_line<-function(tables,title,stem,notes){
    draw_table_pdf(tables,title,reg(paste0("to_share/",stem,".pdf")),notes)
    esc<-function(x){x<-gsub("\\","\\\\",x,fixed=TRUE);x<-gsub("{","\\{",x,fixed=TRUE);gsub("}","\\}",x,fixed=TRUE)}
    rt<-c("{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 Arial;}}\\landscape\\paperw16838\\paperh11906\\margl600\\margr600\\f0\\fs20")
    for(nm in names(tables)){
      if(nm!=names(tables)[1])rt<-c(rt,"\\page")
      rt<-c(rt,paste0("\\pard\\b ",esc(title),"\\b0\\par ",esc(nm),"\\par\\par"))
      rows<-rbind(names(tables[[nm]]),as.matrix(tables[[nm]]));nc<-ncol(rows);ends<-round(c(2200,seq(4200,15100,length.out=nc-1)))
      for(i in seq_len(nrow(rows))){bd<-paste0(if(i==1)"\\clbrdrt\\brdrs\\brdrw15\\clbrdrb\\brdrs\\brdrw10"else"",if(i==nrow(rows))"\\clbrdrb\\brdrs\\brdrw15"else"")
        rt<-c(rt,paste0("\\trowd\\trgaph60\\trkeep",paste0(bd,"\\cellx",ends,collapse=""),paste(vapply(seq_len(nc),function(j)paste0("\\pard\\intbl",if(j==1)"\\ql "else"\\qr ","\\fs24 ",if(i==1)"\\b "else"\\b0 ",esc(rows[i,j]),"\\cell"),character(1)),collapse=""),"\\row"))}
      rt<-c(rt,paste0("\\pard\\fs24 ",paste(esc(notes),collapse="\\par "),"\\par"))
    };txt_out(c(rt,"}"),paste0("to_share/",stem,".rtf"))
  }
  engine<-NULL
  main_domains<-c("hemodynamic","respiratory_neurologic","renal_metabolic","inflammatory_coagulopathic")
  windows<-c("0-6h","0-24h","0-48h")
  model_names<-c(A="Baseline",B="Baseline + phenotype",C="Baseline + dynamics",D="Baseline + dynamics + phenotype")
  plan<-list(version="Step13_v1",implementation_revision="Step13-1",landmark="T48",outer=5L,inner=5L,seed=2026L,
    alpha=c(.1,.5,1),lambda=10^seq(1,-4,length.out=31),criterion="pooled inner-assessment log loss",
    k=2:6,min_cluster_fraction=.05,min_cluster_n=25L,min_silhouette=.05,min_stability_ARI=.6,
    stability_repeats=10L,stability_fraction=.8,silhouette_max_n=1000L,nstart=20L,iter_max=100L,
    min_reference_n=20L,min_domain_fraction=.5,preprocess_min_n=20L,max_missing=.95,
    cluster_missing_indicators=FALSE,model_missing_indicators=TRUE,bootstrap_repeats=2000L,bootstrap_n=2000L,epsilon=1e-12,models=c("A","B","C","D"),
    prediction_endpoint="in_hospital_death_after_T48",evaluation_endpoint="in_hospital_death_after_T48")
  plan<-utils::modifyList(plan,getOption("ami_step13.plan",list()))
  labels<-c(A="A: Baseline",B="B: Baseline + phenotype",C="C: Continuous dynamics",D="D: Dynamics + phenotype")
  colors<-c(A="#7A8797",B="#4F7FC4",C="#D98C57",D="#8B68A6")
  pairs<-data.frame(first=c("B","C","D"),second=c("A","A","C"),comparison=c("B-A","C-A","D-C"))
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
    list(version="Step13_v1",reference=ref,cluster_preprocessor=cp,cluster=cl,dynamic_columns=dynamic,
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
    list(version="Step13_v1",endpoint="in_hospital_death_after_T48",landmark="T48",plan=plan,
      state=state,models=models,training_n=nrow(b$metadata),note="Full-MIMIC refit for later external evaluation; not an independently evaluated training performance")
  }
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
  hash_object<-function(x){f<-tempfile();on.exit(unlink(f));saveRDS(x,f,version=2);unname(tools::md5sum(f))}
  cache_result<-function(rel,key,fun){p<-safe_path(rel)
    if(file.exists(p)){z<-tryCatch(readRDS(p),error=function(e)NULL);if(is.list(z)&&isTRUE(z$complete)&&identical(z$key,key)){reg(rel);logmsg(paste0("复用缓存：",basename(rel)));if(length(z$warnings))for(w in z$warnings)add_warning(w,current_module);return(z$value)}}
    ww<-character();z<-withCallingHandlers(fun(),warning=function(w){ww<<-c(ww,conditionMessage(w));add_warning(conditionMessage(w),current_module);invokeRestart("muffleWarning")});pending<-reg(paste0(rel,".pending"));saveRDS(list(complete=TRUE,key=key,value=z,warnings=ww),pending,compress="gzip")
    p<-reg(rel);if(!file.rename(pending,p))stop("无法保存缓存。");z
  }
  metric_setup<-function(y,p){
    if(anyNA(y)||any(!y%in%0:1)||!is.matrix(p)||nrow(p)!=length(y)||!ncol(p)||any(!is.finite(p))||any(p<0|p>1))stop("无效结局或概率；不删除患者或生成替代风险。")
    lapply(seq_len(ncol(p)),function(j){o<-order(p[,j]);pp<-p[o,j];g<-cumsum(c(TRUE,diff(pp)!=0));ends<-c(which(diff(g)!=0),length(o))
      pc<-pmax(plan$epsilon,pmin(1-plan$epsilon,p[,j]));list(order=o,ends=ends,
        error=(p[,j]-y)^2,loss=-(y*log(pc)+(1-y)*log1p(-pc)))})
  }
  weighted_metrics<-function(y,p,w,setup=metric_setup(y,p)){
    if(length(w)!=length(y)||any(!is.finite(w)|w<0)||sum(w)<=0)stop("无效重抽样权重。")
    nn<-sum(w);pos<-sum(w*y);neg<-nn-pos;prev<-pos/nn
    ans<-matrix(NA_real_,ncol(p),6,dimnames=list(colnames(p),c("AUROC","AUPRC","Brier","LogLoss","Mean_prediction","Event_fraction")))
    for(j in seq_len(ncol(p))){z<-setup[[j]];o<-z$order;ep<-z$ends
      gp<-diff(c(0,cumsum(w[o]*y[o])[ep]));gn<-diff(c(0,cumsum(w[o]*(1-y[o]))[ep]))
      if(pos>0&&neg>0)ans[j,"AUROC"]<-sum(gp*(cumsum(gn)-.5*gn))/(pos*neg)
      if(pos>0){tp<-rev(cumsum(rev(gp)));tot<-rev(cumsum(rev(gp+gn)));ans[j,"AUPRC"]<-sum(ifelse(tot>0,gp*tp/pmax(tot,1),0))/pos}
      ans[j,c("Brier","LogLoss","Mean_prediction","Event_fraction")]<-c(sum(w*z$error)/nn,sum(w*z$loss)/nn,sum(w*p[,j])/nn,prev)
    };ans
  }
  evaluate_external<-function(y,p,hospital=NULL,B=plan$bootstrap_n,seed=plan$seed){
    setup<-metric_setup(y,p);point<-weighted_metrics(y,p,rep(1,length(y)),setup);metrics<-colnames(point)
    mode<-if(is.null(hospital))"patient"else"hospital_cluster"
    if(!is.null(hospital)){if(anyNA(hospital)||any(!nzchar(hospital))||length(unique(hospital))<2)stop("医院重抽样需要完整且至少两个医院。")
      hi<-match(hospital,unique(hospital));H<-max(hi)}
    set.seed(seed);arr<-array(NA_real_,c(B,ncol(p),length(metrics)),dimnames=list(NULL,colnames(p),metrics))
    for(i in seq_len(B)){
      w<-if(is.null(hospital))tabulate(sample.int(length(y),length(y),replace=TRUE),length(y))else tabulate(sample.int(H,H,replace=TRUE),H)[hi]
      arr[i,,]<-weighted_metrics(y,p,w,setup)
      if(i%%500L==0L)logmsg(paste0(mode," 重抽样：",i,"/",B,"。"))
    }
    interval<-function(x){v<-x[is.finite(x)];if(length(v)<.9*B)return(c(NA_real_,NA_real_));as.numeric(quantile(v,c(.025,.975)))}
    tab<-do.call(rbind,lapply(colnames(p),function(a)do.call(rbind,lapply(metrics,function(m){v<-arr[,a,m];ci<-interval(v)
      data.frame(resampling=mode,model=a,metric=m,estimate=point[a,m],lower=ci[1],upper=ci[2],bootstrap_valid=sum(is.finite(v)),bootstrap_requested=B)}))))
    dd<-do.call(rbind,lapply(seq_len(nrow(pairs)),function(j)do.call(rbind,lapply(metrics[1:4],function(m){v<-arr[,pairs$first[j],m]-arr[,pairs$second[j],m];ci<-interval(v)
      data.frame(resampling=mode,comparison=pairs$comparison[j],metric=m,estimate=point[pairs$first[j],m]-point[pairs$second[j],m],lower=ci[1],upper=ci[2],bootstrap_valid=sum(is.finite(v)),bootstrap_requested=B)}))))
    list(metrics=tab,differences=dd,bootstrap=arr,seed=seed)
  }
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
  inspect_prediction<-function(p,lp){
    do.call(rbind,lapply(colnames(p),function(a){q<-quantile(p[,a],c(0,.01,.25,.5,.75,.99,1));data.frame(model=a,n=nrow(p),unique_probabilities=length(unique(p[,a])),
      p_min=q[1],p01=q[2],p25=q[3],p50=q[4],p75=q[5],p99=q[6],p_max=q[7],mean=mean(p[,a]),sd=sd(p[,a]),
      fraction_near0=mean(p[,a]<1e-6),fraction_near1=mean(p[,a]>1-1e-6),lp_min=min(lp[,a]),lp_max=max(lp[,a]))}))
  }
  manual_predict<-function(b,object){
    s<-object$state;sc<-engine$apply_reference(b$raw_numeric,s$reference,windows,min_fraction=object$plan$min_domain_fraction)$scores[s$score_columns]
    xx<-engine$apply_preprocessor(sc,s$cluster_preprocessor);cen<-s$cluster$centers
    dist2<-sapply(seq_len(nrow(cen)),function(j)rowSums(sweep(xx,2,cen[j,],"-")^2));dist2<-matrix(dist2,nrow=nrow(xx),ncol=nrow(cen))
    cl<-max.col(-dist2,ties.method="first")
    base<-b$baseline[s$baseline_columns];dyn<-cbind(base,b$raw_numeric[s$dynamic_columns],sc)
    dummy<-as.data.frame(matrix(0,nrow(base),if(nrow(cen)>1)nrow(cen)else 0))
    if(ncol(dummy)){names(dummy)<-paste0("phenotype_C",seq_len(ncol(dummy)));for(j in seq_len(ncol(dummy)))dummy[,j]<-as.numeric(cl==j)}
    design<-list(A=base,B=cbind(base,dummy),C=dyn,D=cbind(dyn,dummy));lp<-matrix(NA_real_,nrow(base),4,dimnames=list(NULL,plan$models));audit<-list()
    for(a in plan$models){m<-object$models[[a]];xx<-engine$apply_preprocessor(design[[a]],m$preprocessor)
      if(!identical(names(m$coefficients),c("(Intercept)",colnames(xx))))stop("模型系数顺序不匹配。")
      lp[,a]<-as.numeric(cbind(1,xx)%*%m$coefficients)
      for(nm in m$preprocessor$features){v<-design[[a]][[nm]];obs<-v[!is.na(v)];q<-if(length(obs))quantile(obs,c(.25,.5,.75),names=FALSE)else rep(NA_real_,3)
        audit[[length(audit)+1L]]<-data.frame(model=a,feature=nm,n=length(v),observed_n=length(obs),missing_fraction=mean(is.na(v)),all_missing=all(is.na(v)),
          q25=q[1],median=q[2],q75=q[3],frozen_imputation=m$preprocessor$median[[nm]],has_frozen_missing_indicator=nm%in%m$preprocessor$indicators)}
    }
    list(probability=plogis(lp),lp=lp,scores=sc,phenotype=cl,distance=sqrt(dist2[cbind(seq_len(nrow(xx)),cl)]),audit=do.call(rbind,audit))
  }
  validate_bundle<-function(b,src,n,events){
    if(!identical(b$version,"Step6_v1")||nrow(b$metadata)!=n||anyNA(b$metadata$patient_key)||anyDuplicated(b$metadata$patient_key)||
       any(b$metadata$source!=src)||any(b$metadata$landmark!="T48"))stop("冻结队列版本、人数、患者键或时点不符：",src)
    y<-b$outcomes$event_primary;ep<-if(src=="MIMIC")"30_day_all_cause_death_after_landmark"else plan$evaluation_endpoint
    if(length(y)!=n||anyNA(y)||any(!y%in%0:1)||sum(y)!=events||any(b$outcomes$endpoint!=ep))stop("冻结结局不符：",src)
    for(z in c("raw_numeric","baseline","outcomes"))if(nrow(b[[z]])!=n||anyDuplicated(names(b[[z]])))stop("患者行数或重复列名不符。")
    for(d in list(b$raw_numeric,b$baseline))for(v in d)if(!is.numeric(v)||any(is.infinite(v)|is.nan(v)))stop("输入应为数值或NA，不接受无穷值/NaN。")
    if(anyNA(b$baseline$in_index_icu)||any(!b$baseline$in_index_icu%in%0:1))stop("ICU状态缺失或无效。")
    invisible(TRUE)
  }
  verify_lineage<-function(step){
    root<-file.path(cfg$OUTPUT_PARENT,step);sp<-file.path(root,"to_share",paste0(step,"_STATUS.txt"))
    if(!file.exists(sp)||!"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(sp,warn=FALSE))stop("请先完成",step,"。")
    lin<-read.csv(file.path(root,"to_share/01_inputs.csv"),stringsAsFactors=FALSE)
    if(!all(c("file","md5")%in%names(lin))||!nrow(lin)||anyNA(lin$file)||anyNA(lin$md5))stop("上游输入审计无效：",step)
    for(i in seq_len(nrow(lin)))if(!file.exists(lin$file[i])||unname(tools::md5sum(lin$file[i]))!=lin$md5[i])stop(step,"输入已变化，请先重跑该步骤。")
  }
  subset_bundle<-function(b,i){
    n<-nrow(b$metadata)
    for(nm in c("metadata","raw_numeric","baseline","clinical_documentation","outcomes")){
      if(is.null(b[[nm]]))next
      if(nrow(b[[nm]])!=n)stop("患者表行数不一致：",nm)
      b[[nm]]<-b[[nm]][i,,drop=FALSE];rownames(b[[nm]])<-NULL
    };b
  }
  hospital_bit<-function(x,label){
    # Step6 secondary outcomes can retain VARCHAR/factor storage. Convert values,
    # never factor level codes; reject invalid entries rather than turning them into NA.
    if(is.factor(x))x<-as.character(x)
    if(is.logical(x))x<-as.integer(x)
    if(!is.numeric(x)&&!is.character(x))stop(label,"应为数值、字符或因子的0/1/NA字段。")
    z<-suppressWarnings(as.numeric(x))
    if(any(!is.na(x)&is.na(z))||any(is.nan(z)|is.infinite(z))||any(!is.na(z)&!z%in%0:1))
      stop(label,"含无效二分类值；只接受0/1及原有NA，不自动置零或新增缺失。")
    as.integer(z)
  }
  hospital_bundle<-function(b){
    req<-c("death_in_index_hospital_after_landmark","analysis_eligible_hospital_endpoint")
    if(!all(req%in%names(b$outcomes)))stop("Step6未保存住院终点或其资格标记；不能由30天死亡推算。")
    types<-vapply(req,function(nm)paste(class(b$outcomes[[nm]]),collapse="/"),character(1))
    h<-hospital_bit(b$outcomes[[req[1]]],req[1]);eligible<-hospital_bit(b$outcomes[[req[2]]],req[2])
    if(length(h)!=nrow(b$metadata)||length(eligible)!=nrow(b$metadata))stop("住院终点字段与患者行数不一致。")
    b$outcomes[[req[1]]]<-h;b$outcomes[[req[2]]]<-eligible
    type_audit<-data.frame(field=req,input_class=unname(types),normalized_class="integer",
      n=c(length(h),length(eligible)),missing_n=c(sum(is.na(h)),sum(is.na(eligible))),
      zero_n=c(sum(h==0,na.rm=TRUE),sum(eligible==0,na.rm=TRUE)),one_n=c(sum(h==1,na.rm=TRUE),sum(eligible==1,na.rm=TRUE)))
    keep<-which(!is.na(eligible)&eligible==1&!is.na(h))
    if(length(keep)<50L||any(table(factor(h[keep],levels=0:1))<25L))stop("住院终点有效样本/事件不足以进行预设嵌套验证。")
    z<-subset_bundle(b,keep);z$outcomes$event_30d_original<-z$outcomes$event_primary
    z$outcomes$event_primary<-as.integer(h[keep]);z$outcomes$endpoint<-plan$prediction_endpoint
    list(bundle=z,index=keep,type_audit=type_audit,audit=data.frame(item=c("Frozen 30-day-eligible T48 cohort","Hospital eligibility flag missing","Hospital eligibility flag zero","Hospital outcome missing","Hospital companion included","Included hospital deaths"),
      n=c(nrow(b$metadata),sum(is.na(eligible)),sum(eligible==0,na.rm=TRUE),sum(is.na(h)),length(keep),sum(h[keep]))))
  }
  main<-function(){
    need<-c("glmnet","cluster");missing_pkgs<-need[!vapply(need,requireNamespace,logical(1),quietly=TRUE)]
    if(length(missing_pkgs))stop("缺少R包：",paste(missing_pkgs,collapse=", "),"。请先运行 install.packages(c('glmnet','cluster'))。")
    current_module<<-"frozen Step6 inputs";verify_lineage("Step6")
    paths<-c(mimic=file.path(cfg$OUTPUT_PARENT,"Step6/local_only/MIMIC_T48_model_inputs.rds"),
      eicu=file.path(cfg$OUTPUT_PARENT,"Step6/local_only/eICU_T48_model_inputs.rds"),
      engine=file.path(cfg$OUTPUT_PARENT,"Step6/local_only/Step6_score_engine.R"),
      contract=file.path(cfg$OUTPUT_PARENT,"Step6/local_only/Step6_analysis_contract.rds"))
    if(!all(file.exists(paths)))stop("Step6本地输入不完整。")
    h<-setNames(unname(tools::md5sum(paths)),names(paths));csv_out(data.frame(file=unname(paths),md5=unname(h)),"to_share/01_inputs.csv")
    env<-new.env(parent=baseenv());sys.source(paths["engine"],envir=env);engine<<-env$step6_score_engine
    contract<-readRDS(paths["contract"])
    if(!identical(contract$version,"Step6_v1")||!identical(contract$main_domains,main_domains))stop("Step6评分约定不一致。")
    if(plan$outer!=5L||plan$inner!=5L)stop("本步要求五乘五嵌套验证。")
    current_module<<-"synthetic preflight";logmsg("小型预检：折内拟合隔离、冻结推理、同分指标、配对重抽样和PDF。")
    endpoint_preflight();modeling_preflight();metric_preflight()
    txt_out("PASS: numeric/character/factor hospital endpoints with preserved missingness; training-only references/preprocessing/clustering; outcome-blind clusters; frozen single/batch prediction; tied metrics; paired and hospital bootstrap; multipage PDF.","to_share/02_preflight.txt")
    current_module<<-"hospital endpoint and fresh stratified folds"
    original<-readRDS(paths["mimic"]);validate_bundle(original,"MIMIC",5133L,630L)
    hb<-hospital_bundle(original);b<-hb$bundle;rm(original)
    csv_out(hb$audit,"to_share/02_hospital_endpoint_eligibility.csv")
    csv_out(hb$type_audit,"to_share/02_hospital_field_types.csv")
    xt<-as.data.frame(table(death_30day=factor(b$outcomes$event_30d_original,levels=0:1),death_hospital=factor(b$outcomes$event_primary,levels=0:1)))
    csv_out(xt,"to_share/02_MIMIC_endpoint_cross_tab.csv")
    y<-b$outcomes$event_primary;n<-length(y)
    f<-nested_folds(b$metadata$patient_key,y,k=5L,seed=plan$seed);outer<-f$outer$outer_fold
    saveRDS(f,reg("local_only/MIMIC_T48_hospital_nested_folds.rds"),compress="gzip")
    fa<-list()
    for(o in 1:5){tr<-which(outer!=o);va<-which(outer==o);inn<-f$inner[[as.character(o)]]
      if(anyDuplicated(inn$patient_key)||!setequal(inn$patient_key,b$metadata$patient_key[tr]))stop("内外折患者不匹配。")
      il<-inn$inner_fold[match(b$metadata$patient_key[tr],inn$patient_key)]
      fa[[paste0(o,"outer")]]<-data.frame(scope=paste0("outer",o),training_n=length(tr),training_events=sum(y[tr]),assessment_n=length(va),assessment_events=sum(y[va]),patient_overlap=length(intersect(tr,va)))
      for(j in 1:5){tt<-tr[il!=j];vv<-tr[il==j];fa[[paste(o,j)]]<-data.frame(scope=paste0("outer",o,"_inner",j),training_n=length(tt),training_events=sum(y[tt]),assessment_n=length(vv),assessment_events=sum(y[vv]),patient_overlap=length(intersect(tt,vv)))}
    }
    fa<-do.call(rbind,fa)
    if(any(fa$patient_overlap!=0)||any(fa$training_events==0|fa$training_events==fa$training_n)||any(fa$assessment_events==0|fa$assessment_events==fa$assessment_n))stop("分折存在重叠或单一结局。")
    csv_out(fa,"to_share/02_fold_audit.csv");txt_out(capture.output(dput(plan)),"to_share/02_analysis_plan.txt")
    funs<-c("with_seed","nearest","adjusted_rand","km_fit","fit_clusters","score_columns","fit_state","apply_state","fit_path","path_predict","logloss_sum","select_hyper","fit_arm","run_split","fit_final","predict_frozen","make_folds","nested_folds","hospital_bit","hospital_bundle")
    key<-hash_object(list(inputs=h[c("mimic","engine","contract")],plan=plan,folds=f,functions=lapply(funs,function(z)body(get(z))),
      packages=vapply(need,function(p)as.character(utils::packageVersion(p)),character(1)),R=as.character(getRversion())))
    pp<-matrix(NA_real_,n,4,dimnames=list(NULL,plan$models));gridpred<-setNames(lapply(plan$models,function(a)matrix(NA_real_,n,length(plan$alpha)*length(plan$lambda))),plan$models)
    audits<-refs<-cov<-tuning<-outerfits<-krows<-list();foldclass<-integer(n)
    collect<-function(z,scope){audits[[scope]]<<-cbind(scope=scope,z$cluster_audit);refs[[scope]]<<-cbind(scope=scope,z$reference_audit)
      cov[[scope]]<<-cbind(scope=scope,z$score_audit);krows[[scope]]<<-data.frame(scope=scope,k=z$k,training_n=z$training_n,assessment_n=z$assessment_n)}
    for(o in 1:5){
      tr<-which(outer!=o);va<-which(outer==o);inn<-f$inner[[as.character(o)]];il<-inn$inner_fold[match(b$metadata$patient_key[tr],inn$patient_key)];losses<-NULL
      for(j in 1:5){scope<-paste0("outer",o,"_inner",j);current_module<<-scope
        logmsg(paste0("住院终点：外折 ",o,"/5，内折 ",j,"/5。"))
        z<-cache_result(paste0("local_only/cache/hospital_",scope,".rds"),key,function()run_split(b,tr[il!=j],tr[il==j],plan$seed+1000L*o+10L*j))
        if(!identical(z$assessment_keys,b$metadata$patient_key[tr[il==j]]))stop("内折缓存患者顺序不一致。")
        collect(z,scope);if(is.null(losses))losses<-z$loss else for(a in plan$models)losses[[a]]<-losses[[a]]+z$loss[[a]];grid<-z$grid
      }
      chosen<-lapply(losses,select_hyper,grid=grid)
      for(a in plan$models)tuning[[paste(o,a)]]<-data.frame(scope=paste0("outer",o),model=a,grid,mean_logloss=losses[[a]]/length(tr),converged_all_folds=is.finite(losses[[a]]),selected=seq_len(nrow(grid))==chosen[[a]])
      scope<-paste0("outer",o);current_module<<-scope
      z<-cache_result(paste0("local_only/cache/hospital_",scope,".rds"),hash_object(list(key,chosen)),function()run_split(b,tr,va,plan$seed+1000L*o+99L,chosen))
      if(!identical(z$assessment_keys,b$metadata$patient_key[va]))stop("外折缓存患者顺序不一致。")
      collect(z,scope);pp[va,]<-z$predictions;foldclass[va]<-z$assessment_cluster
      for(a in plan$models)gridpred[[a]][va,]<-z$predictions_all[[a]]
      outerfits[[scope]]<-list(state=z$state,models=z$models)
    }
    metric_setup(y,pp)
    saveRDS(list(version=plan$version,metadata=b$metadata,outcomes=b$outcomes,outer_fold=outer,fold_specific_phenotype=foldclass,prediction=pp),reg("local_only/MIMIC_T48_hospital_nested_OOF.rds"),compress="gzip")
    saveRDS(outerfits,reg("local_only/MIMIC_T48_hospital_outer_models.rds"),compress="gzip")
    # The nested OOF probabilities above are locked. The following tuning serves only the final external model.
    finalchosen<-lapply(gridpred,function(p)select_hyper(logloss_sum(y,p),grid))
    for(a in plan$models)tuning[[paste0("final",a)]]<-data.frame(scope="full_MIMIC_tuning_only",model=a,grid,mean_logloss=logloss_sum(y,gridpred[[a]])/n,converged_all_folds=is.finite(logloss_sum(y,gridpred[[a]])),selected=seq_len(nrow(grid))==finalchosen[[a]])
    current_module<<-"full MIMIC hospital model"
    logmsg("嵌套OOF已锁定；拟合全MIMIC住院终点模型，随后才读取eICU患者数据。")
    final<-cache_result("local_only/cache/hospital_final.rds",hash_object(list(key,finalchosen)),function()fit_final(b,finalchosen,plan$seed+99999L))
    saveRDS(final,reg("local_only/MIMIC_T48_hospital_frozen_pipeline.rds"),compress="gzip");final_hash<-hash_object(final)
    txt_out(readLines(paths["engine"],warn=FALSE),"local_only/Step13_score_engine.R")
    code<-c("# source Step13_score_engine.R first. All references and coefficients are frozen MIMIC hospital-endpoint fits.",
      "predict_step13 <- local({","engine <- step6_score_engine","plogis <- stats::plogis",paste0("plan <- ",paste(deparse(plan),collapse="\n")),
      'windows <- c("0-6h","0-24h","0-48h")',paste0("nearest <- ",paste(deparse(nearest),collapse="\n")),
      paste0("apply_state <- ",paste(deparse(apply_state),collapse="\n")),paste0("predict_frozen <- ",paste(deparse(predict_frozen),collapse="\n")),"predict_frozen","})")
    txt_out(code,"local_only/Step13_predict.R")
    exported<-new.env(parent=baseenv());sys.source(safe_path("local_only/Step13_score_engine.R"),envir=exported);sys.source(safe_path("local_only/Step13_predict.R"),envir=exported)
    auditpred<-list()
    for(o in 1:5){i<-which(outer==o);z<-exported$predict_step13(b$raw_numeric[i,,drop=FALSE],b$baseline[i,,drop=FALSE],outerfits[[paste0("outer",o)]])
      err<-max(abs(z$probability-pp[i,,drop=FALSE]));bad<-sum(z$phenotype!=foldclass[i]);if(err>1e-10||bad)stop("导出函数无法复现住院终点OOF。")
      auditpred[[paste0("outer",o)]]<-data.frame(check=paste0("outer",o),n=length(i),max_probability_error=err,phenotype_mismatches=bad)}
    audits$full_MIMIC_final<-cbind(scope="full_MIMIC_final",final$state$cluster$audit)
    refs$full_MIMIC_final<-cbind(scope="full_MIMIC_final",reference_audit(final$state))
    krows$full_MIMIC_final<-data.frame(scope="full_MIMIC_final",k=final$state$cluster$k,training_n=n,assessment_n=0)
    csv_out(do.call(rbind,audits),"to_share/03_cluster_selection.csv");csv_out(do.call(rbind,krows),"to_share/03_selected_k.csv")
    csv_out(do.call(rbind,refs),"to_share/03_reference_availability.csv");csv_out(do.call(rbind,cov),"to_share/03_fold_score_coverage.csv");csv_out(do.call(rbind,tuning),"to_share/04_tuning_logloss.csv")
    csv_out(do.call(rbind,lapply(plan$models,function(a){z<-final$models[[a]];data.frame(model=a,alpha=z$alpha,lambda=z$lambda,retained_columns=length(z$coefficients)-1L,nonzero_predictor_coefficients=sum(z$coefficients[-1]!=0))})),"to_share/04_final_model_summary.csv")
    current_module<<-"frozen eICU hospital-endpoint inference"
    e<-readRDS(paths["eicu"]);validate_bundle(e,"eICU",5284L,219L)
    if(!identical(b$feature_plan,e$feature_plan)||!identical(b$score_specification,e$score_specification)||!identical(names(b$raw_numeric),names(e$raw_numeric))||!identical(names(b$baseline),names(e$baseline)))stop("两库特征约定或顺序不一致。")
    pred<-list()
    for(src in c("MIMIC","eICU")){bb<-if(src=="MIMIC")b else e;z<-exported$predict_step13(bb$raw_numeric,bb$baseline,final);man<-manual_predict(bb,final)
      err<-max(abs(z$probability-man$probability));bad<-sum(z$phenotype!=man$phenotype)
      for(j in unique(c(1L,round(nrow(bb$metadata)/2),nrow(bb$metadata)))){one<-exported$predict_step13(bb$raw_numeric[j,,drop=FALSE],bb$baseline[j,,drop=FALSE],final);err<-max(err,abs(one$probability[1,]-z$probability[j,]));bad<-bad+sum(one$phenotype!=z$phenotype[j])}
      raw2<-bb$raw_numeric[1:3,,drop=FALSE];raw2[2:3,]<-NA_real_;zz<-exported$predict_step13(raw2,bb$baseline[1:3,,drop=FALSE],final);err<-max(err,abs(zz$probability[1,]-z$probability[1,]));bad<-bad+sum(zz$phenotype[1]!=z$phenotype[1])
      if(!is.finite(err)||err>1e-10||bad)stop("冻结矩阵/单例/批量推理不一致。")
      auditpred[[src]]<-data.frame(check=paste0(src,"_full_frozen"),n=nrow(z$probability),max_probability_error=err,phenotype_mismatches=bad);pred[[src]]<-man
    }
    csv_out(do.call(rbind,auditpred),"to_share/02_prediction_roundtrip.csv")
    csv_out(rbind(cbind(source="MIMIC_full_fit",pred$MIMIC$audit),cbind(source="eICU_frozen",pred$eICU$audit)),"to_share/03_frozen_feature_availability.csv")
    if(any(pred$eICU$audit$all_missing))issue("WARNING","external feature availability","存在外部全缺失特征；按冻结MIMIC参数填补，详见可用性审计。")
    ey<-e$outcomes$event_primary;ep<-pred$eICU$probability;metric_setup(ey,ep)
    hid<-as.character(e$metadata$hospital_id);hok<-length(hid)==length(ey)&&!anyNA(hid)&&all(nzchar(trimws(hid)))&&length(unique(hid))>=2;hosp<-if(hok)hid else NULL
    if(!hok)issue("NOTE","hospital bootstrap","医院ID不完整或不足两个医院；仅报告患者bootstrap。")
    csv_out(data.frame(source=c("MIMIC_nested_OOF","eICU_frozen"),n=c(n,length(ey)),events=c(sum(y),sum(ey)),event_fraction=c(mean(y),mean(ey)),in_index_icu=c(sum(b$baseline$in_index_icu),sum(e$baseline$in_index_icu)),endpoint=plan$prediction_endpoint),"to_share/02_cohort_endpoints.csv")
    csv_out(data.frame(n=length(ey),hospital_ids_complete=length(hid)==length(ey)&&!anyNA(hid)&&all(nzchar(trimws(hid))),hospital_count=length(unique(hid[!is.na(hid)&nzchar(hid)])),cluster_bootstrap=hok),"to_share/02_hospital_availability.csv")
    saveRDS(list(version=plan$version,metadata=e$metadata,outcomes=e$outcomes,prediction=ep,linear_predictor=pred$eICU$lp,phenotype=pred$eICU$phenotype,
      prediction_endpoint=plan$prediction_endpoint,evaluation_endpoint=plan$evaluation_endpoint,input_md5=h,model_hash=final_hash,note="Frozen MIMIC hospital-endpoint model; no eICU fitting or recalibration"),reg("local_only/eICU_T48_hospital_frozen_predictions.rds"),compress="gzip")
    current_module<<-"locked prediction uncertainty"
    ek<-hash_object(list(key=key,external=h["eicu"],internal_prediction=pp,external_prediction=ep,weighted=body(weighted_metrics),setup=body(metric_setup),evaluate=body(evaluate_external)))
    rr<-list(MIMIC_nested_OOF=cache_result("local_only/cache/MIMIC_hospital_OOF_bootstrap.rds",ek,function()evaluate_external(y,pp,seed=plan$seed+9100L)),
      eICU_frozen=cache_result("local_only/cache/eICU_hospital_patient_bootstrap.rds",ek,function()evaluate_external(ey,ep,seed=plan$seed+9200L)))
    if(hok)rr$eICU_cluster<-cache_result("local_only/cache/eICU_hospital_cluster_bootstrap.rds",ek,function()evaluate_external(ey,ep,hosp,seed=plan$seed+9300L))
    met<-do.call(rbind,lapply(names(rr),function(nm)cbind(cohort=if(nm=="eICU_cluster")"eICU_frozen"else nm,rr[[nm]]$metrics)))
    dif<-do.call(rbind,lapply(names(rr),function(nm)cbind(cohort=if(nm=="eICU_cluster")"eICU_frozen"else nm,rr[[nm]]$differences)))
    csv_out(met,"to_share/05_hospital_metrics.csv");csv_out(dif,"to_share/05_hospital_paired_differences.csv")
    if(any(!is.finite(met$lower))||any(!is.finite(dif$lower)))issue("WARNING","bootstrap interval","部分区间缺失，检查有效bootstrap数；未用替代区间填充。")
    byfold<-do.call(rbind,lapply(1:5,function(o){i<-which(outer==o);z<-weighted_metrics(y[i],pp[i,,drop=FALSE],rep(1,length(i)));data.frame(outer_fold=o,model=rownames(z),n=length(i),events=sum(y[i]),z,row.names=NULL)}))
    csv_out(byfold,"to_share/05_MIMIC_metrics_by_outer_fold.csv")
    data<-list(MIMIC_nested_OOF=list(y=y,p=pp,hospital=NULL),eICU_frozen=list(y=ey,p=ep,hospital=hosp))
    cal<-bins<-diagnostics<-grids<-list()
    for(src in names(data)){d<-data[[src]]
      cal[[src]]<-do.call(rbind,lapply(plan$models,function(a)cbind(cohort=src,model=a,calibration(d$y,d$p[,a],d$hospital))))
      bins[[src]]<-do.call(rbind,lapply(plan$models,function(a)cbind(cohort=src,model=a,calibration_bins(d$y,d$p[,a]))))
      diagnostics[[src]]<-cbind(cohort=src,inspect_prediction(d$p,qlogis(pmin(pmax(d$p,plan$epsilon),1-plan$epsilon))))
      grids[[src]]<-do.call(rbind,lapply(plan$models,function(a)do.call(rbind,lapply(seq(0,1,length.out=201),function(t){pos<-d$p[,a]>=t;tp<-sum(pos&d$y==1);fp<-sum(pos&d$y==0);data.frame(cohort=src,model=a,threshold=t,TP=tp,FP=fp,FN=sum(d$y)-tp,TN=sum(1-d$y)-fp,n=length(d$y))}))))
    }
    cal<-do.call(rbind,cal);bins<-do.call(rbind,bins);diagnostics<-do.call(rbind,diagnostics)
    csv_out(cal,"to_share/06_calibration_parameters.csv");csv_out(bins,"to_share/06_calibration_bins.csv");csv_out(diagnostics,"to_share/06_prediction_diagnostics.csv");csv_out(do.call(rbind,grids),"to_share/06_fixed_grid_curve_counts.csv")
    if(any(cal$status!="OK"))issue("NOTE","calibration","部分校准拟合不稳定；参数保留NA，原因见校准CSV。")
    if(any(diagnostics$unique_probabilities<2))issue("WARNING","constant predictions","存在常数预测；如实保留所有模型结果。")
    sizes<-do.call(rbind,lapply(names(pred),function(src){ct<-tabulate(pred[[src]]$phenotype,final$state$cluster$k);data.frame(source=src,phenotype=paste0("C",seq_along(ct)),n=ct,fraction=ct/sum(ct))}))
    csv_out(sizes,"to_share/03_hospital_frozen_class_sizes.csv")
    current_module<<-"large-font figures and tables";make_visuals(data,met,dif,cal,bins)
    outputs<-c("Figure_S_hospital_ROC","Figure_S_hospital_PR","Figure_S_hospital_MIMIC_calibration","Figure_S_hospital_eICU_calibration","Table_S_hospital_performance","Table_S_hospital_calibration")
    csv_out(data.frame(output=outputs,action="ADD supplementary; retain Step7 30-day primary analysis and Step12 related-endpoint transport",endpoint="In-hospital death after T48",note="No original Figure4 panel is replaced by this step; final manuscript integration pending"),"to_share/11_figure_table_replacement_map.csv")
    txt_out(c("Step13-1：修正Step6住院终点/资格标记可能为字符或因子的兼容问题；先严格校验并转为整数，再统计和建模，原有NA保留，非法值停止。",
      "Step13：T48后本次住院死亡配套模型及冻结eICU外部评价，属于后续补充分析。",
      "MIMIC起点仍为Step6已冻结的30天终点合格T48队列5133人；只在其中筛选住院终点资格明确且结局已知者，不能称已扩展至所有住院终点合格患者。",
      "住院死亡直接读取Step6保存的death_in_index_hospital_after_landmark及analysis_eligible_hospital_endpoint，不由30天死亡反推。资格审计和两终点交叉表均输出。",
      "本次已看过Step12相关终点结果；须披露为修订阶段补充分析，不称预先注册或从未查看过的独立验证。",
      "A年龄/性别/T48 ICU状态；B=A+分型；C=A+连续动态及域评分；D=C+分型。四模型同一患者、分折和网格。",
      "按住院终点重新生成确定性的5乘5嵌套分折，seed=2026；与原30天终点分折不同。",
      "所有评分参考、填补、筛特征、标准化和无监督分型在相应训练折重新拟合；外评估患者不参与其训练或内折调参。",
      "分型规则沿用Step7：k=2:6，最低规模max(25人,5%)，轮廓>=0.05，中位子抽样稳定性ARI>=0.60；合格者选最高轮廓，差<=0.01选较小k；均不合格则k=1。",
      "上述分型标准为本项目操作规则；稳定性只在固定该训练折表示下评估，不能代表整个评分管线稳定性。各折C标签不能直接合并作临床亚型。",
      "模型调参为alpha={0.1,0.5,1}、lambda=10^seq(1,-4,length.out=31)，内折合并log loss最小；未收敛候选排除，选中模型失败则停止。",
      "先锁定嵌套OOF；再以五外折全网格预测选择最终全MIMIC模型参数。该网格最优值仅供调参，不作为新的独立内部性能。",
      "最终模型冻结后才读取eICU患者数据；eICU结局不参与评分参考、缺失处理、分型、特征选择或调参。本步不重新校准预测。",
      "MIMIC全量拟合预测只用于导出接口和描述性核验，不作为内部性能；内部性能只来自嵌套OOF。eICU使用全MIMIC冻结模型。",
      "现在两库均为本次住院死亡，但出院、转院、住院时长及患者选择仍可能不同；这是住院终点配套模型评价，不是原30天死亡模型的外部验证。",
      "AUROC和AUPRC按同分组处理；AUPRC为average precision。LogLoss及校准计算仅为数值稳定将极端0/1截至1e-12，原始风险、AUROC、AUPRC及Brier不改。",
      "每种患者bootstrap 2000次，四模型使用配对样本。eICU医院ID完整时另做2000次整医院重抽样。有效重复不足90%则区间NA。",
      "上述区间条件于已锁定预测，不覆盖模型重新开发的全部不确定性；内部OOF训练折有重叠，患者bootstrap区间应审慎解释。",
      "校准输出offset截距、联合截距/斜率及Wald区间，eICU额外给医院稳健区间；拟合仅用于诊断，不产生更新预测。图为分组点与Wilson区间。",
      "住院终点配套分析新增补充图表，保留Step7主30天终点及Step12相关终点结果；最终主文外部面板待后续分析完成再统一。",
      "新图使用18pt基础字号，坐标约17-19pt，说明约15pt；PDF可编辑，PNG为350dpi；三线表PDF/RTF附完整CSV。",
      "只读取Step6，无需T7；仅覆盖Step13登记文件；输入、算法或软件版本变化使缓存失效。中断直接重跑脚本，已完成模型块可复用。",
      "患者结局、预测、分折、冻结参考与模型只留local_only。只回传Step13_feedback.zip。"),"to_share/00_READ_ME_FIRST.txt")
    txt_out(c("https://glmnet.stanford.edu/articles/glmnet.html","https://stat.ethz.ch/R-manual/R-devel/library/cluster/html/silhouette.html","https://eicu.mit.edu/eicutables/patient/","https://www.bmj.com/content/384/bmj-2023-074820"),"to_share/10_method_sources.txt")
    if(!identical(h,setNames(unname(tools::md5sum(paths)),names(paths)))||hash_object(final)!=final_hash)stop("运行过程中上游输入或冻结模型发生变化。")
    status<<-"COMPLETED_FOR_REVIEW"
  }
  fmt<-function(x,d=3)ifelse(is.finite(x),formatC(x,format="f",digits=d),"NA")
  fmt_ci<-function(x,l,u,d=3)ifelse(is.finite(x)&is.finite(l)&is.finite(u),paste0(fmt(x,d)," [",fmt(l,d),", ",fmt(u,d),"]"),"NA")
  setup_plot<-function(mf=c(1,1),bottom=3.4){par(mfrow=mf,mar=c(4.3,4.4,2.8,1),oma=c(bottom,0,0,0),family="sans",las=1,cex=1,cex.axis=.95,cex.lab=1.05,cex.main=1.05,mgp=c(2.8,.8,0))}
  note<-function(x){for(i in seq_along(x))mtext(x[i],side=1,outer=TRUE,line=.8+(i-1)*1.15,cex=.82)}
  make_visuals<-function(data,met,dif,cal,bins){
    titles<-c(MIMIC_nested_OOF="MIMIC: nested OOF",eICU_frozen="eICU: frozen external")
    for(kind in c("ROC","PR"))save_figure(paste0("Figure_S_hospital_",kind),local({kk<-kind;function(){setup_plot(c(1,2),4.2)
      for(src in names(data)){d<-data[[src]];roc<-kk=="ROC"
        plot(0,0,type="n",xlim=c(0,100),ylim=c(0,100),xlab=if(roc)"False-positive rate (%)"else"Recall (%)",ylab=if(roc)"True-positive rate (%)"else"Precision (%)",main=titles[src],adj=0,bty="l")
        if(roc)abline(0,1,col="#BBBBBB",lty=3)else abline(h=mean(d$y)*100,col="#777777",lty=3)
        for(a in plan$models){z<-curves(d$y,d$p[,a]);if(roc)lines(100*c(0,z$fpr),100*c(0,z$recall),col=colors[a],lty=match(a,plan$models),lwd=2.6)else lines(100*c(0,z$recall),100*c(z$precision[1],z$precision),type="S",col=colors[a],lty=match(a,plan$models),lwd=2.6)}
        m<-met[met$cohort==src&met$resampling=="patient"&met$metric==if(roc)"AUROC"else"AUPRC",];m<-m[match(plan$models,m$model),]
        legend(if(roc)"bottomright"else"topright",legend=paste0(plan$models,": ",fmt(m$estimate)),col=colors,lty=1:4,lwd=2.6,bty="n",cex=.95,title=if(roc)"AUROC"else"Average precision")
        mtext(sprintf("n=%s; hospital deaths=%s",length(d$y),sum(d$y)),side=3,line=.2,adj=0,cex=.84)
      };note(c("Outcome in both cohorts: in-hospital death after T48",if(kk=="ROC")"MIMIC hospital-endpoint companion models; no eICU updating"else"PR baseline: cohort event rate; AUPRC = average precision"))
    }}),11.8,7.1)
    for(src in names(data))save_figure(paste0("Figure_S_hospital_",if(src=="MIMIC_nested_OOF")"MIMIC"else"eICU","_calibration"),local({ss<-src;function(){setup_plot(c(2,2),3.7)
      for(a in plan$models){z<-bins[bins$cohort==ss&bins$model==a,];plot(0,0,type="n",xlim=c(0,100),ylim=c(0,100),xlab="Predicted hospital death (%)",ylab="Observed hospital death (%)",main=labels[a],adj=0,bty="l")
        abline(0,1,col="#AAAAAA",lty=3);segments(z$predicted*100,z$lower*100,z$predicted*100,z$upper*100,col=colors[a],lwd=1.8)
        lines(z$predicted*100,z$observed*100,type="b",pch=16,col=colors[a],lwd=2.2)
      };note(c(paste0(titles[ss],"; in-hospital death after T48"),"Grouped diagnostic; bars: 95% Wilson intervals; no recalibration"))
    }}),11.8,10.6)
    tabs<-list()
    for(src in unique(met$cohort))for(mode in unique(met$resampling[met$cohort==src])){
      tt<-data.frame(Model=plan$models,check.names=FALSE)
      for(mm in c("AUROC","AUPRC","Brier")){z<-met[met$cohort==src&met$resampling==mode&met$metric==mm,];z<-z[match(plan$models,z$model),];tt[[paste0(mm," [95% CI]")]]<-fmt_ci(z$estimate,z$lower,z$upper)}
      tabs[[paste(titles[src],"/",mode)]]<-tt
      tt<-data.frame(Comparison=pairs$comparison,check.names=FALSE)
      for(mm in c("AUROC","AUPRC","Brier")){z<-dif[dif$cohort==src&dif$resampling==mode&dif$metric==mm,];z<-z[match(pairs$comparison,z$comparison),];tt[[paste0("Delta ",mm)]]<-fmt_ci(z$estimate,z$lower,z$upper)}
      tabs[[paste(titles[src],"/",mode,"paired differences")]]<-tt
    }
    three_line(tabs,"Hospital-endpoint companion models: performance","Table_S_hospital_performance",c(
      "A: baseline; B: baseline + phenotype; C: continuous dynamics; D: C + phenotype.",
      "Both outcomes: hospital death after T48. AUPRC is average precision; negative Brier differences favor the first model.",
      paste0("Paired bootstrap: ",plan$bootstrap_n," resamples per method, conditional on locked predictions; models are not refitted."),
      "MIMIC uses nested OOF predictions; eICU uses the frozen full-MIMIC model. Internal training folds overlap.",
      "This companion analysis does not replace the primary 30-day model. Hospital discharge practices may still differ."))
    tt<-list()
    for(src in unique(cal$cohort))for(mode in unique(cal$inference[cal$cohort==src])){
      z<-cal[cal$cohort==src&cal$inference==mode,];tab<-data.frame(Model=plan$models,check.names=FALSE)
      for(pm in c("intercept_offset","joint_intercept","slope")){v<-z[z$parameter==pm,];v<-v[match(plan$models,v$model),];tab[[switch(pm,intercept_offset="Offset intercept",joint_intercept="Joint intercept",slope="Slope")]]<-fmt_ci(v$estimate,v$lower,v$upper,2)}
      tt[[paste(titles[src],"/",mode,"95% Wald intervals")]]<-tab
    }
    three_line(tt,"Hospital-endpoint companion models: calibration","Table_S_hospital_calibration",c(
      "Offset intercept fixes slope at 1. Joint intercept and slope are estimated together.",
      "Fits diagnose calibration only: no updated probabilities are used in this step.",
      "Unstable, rank-deficient or nonconverged parameters are NA; full reasons are retained in CSV.",
      "Intervals are conditional on the fitted predictions. eICU also has hospital-robust Wald intervals when IDs permit."))
  }
  endpoint_preflight<-function(){
    b<-list(metadata=data.frame(patient_key=paste0("synthetic:",seq_len(100L))),outcomes=data.frame(
      event_primary=rep(0:1,50),death_in_index_hospital_after_landmark=rep(c(0L,0L,1L,1L),25),
      analysis_eligible_hospital_endpoint=rep(1L,100)))
    b$outcomes$death_in_index_hospital_after_landmark[2]<-NA_integer_
    b$outcomes$analysis_eligible_hospital_endpoint[c(1,3)]<-c(0L,NA_integer_)
    req<-c("death_in_index_hospital_after_landmark","analysis_eligible_hospital_endpoint")
    reference<-hospital_bundle(b)
    for(kind in c("character","factor","logical")){
      z<-b
      for(nm in req)z$outcomes[[nm]]<-switch(kind,character=as.character(z$outcomes[[nm]]),
        factor=factor(as.character(z$outcomes[[nm]]),levels=c("1","0")),logical=as.logical(z$outcomes[[nm]]))
      ans<-hospital_bundle(z)
      stopifnot(identical(reference$index,ans$index),identical(reference$audit,ans$audit),identical(reference$bundle,ans$bundle),
        identical(reference$type_audit$missing_n,ans$type_audit$missing_n))
    }
    for(bad in list(c("0","yes"),c("0",""),c("0","NA"),c(0,2),c(0,NaN),c(0,Inf)))
      stopifnot(inherits(try(hospital_bit(bad,"synthetic invalid endpoint"),silent=TRUE),"try-error"))
    TRUE
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
  modeling_preflight<-function(){
    old<-plan;on.exit(plan<<-old,add=TRUE)
    plan<<-utils::modifyList(plan,list(k=2:3,min_cluster_n=3L,stability_repeats=2L,nstart=3L,
      min_reference_n=8L,preprocess_min_n=8L,alpha=c(.5,1),lambda=c(1,.3,.1,.03),bootstrap_repeats=20L,silhouette_max_n=120L))
    b<-synthetic_bundle();tr<-1:120;va<-121:160
    s<-fit_state(b$raw_numeric[tr,],b$baseline[tr,],b$feature_plan,102)
    changed<-b;changed$raw_numeric[va,]<-999;changed$outcomes$event_primary<-1-changed$outcomes$event_primary
    s2<-fit_state(changed$raw_numeric[tr,],changed$baseline[tr,],changed$feature_plan,102)
    stopifnot(identical(s,s2),s$cluster$k>=2,adjusted_rand(c(1,1,2,2),c(2,2,1,1))==1,
      abs(weighted_metrics(c(1,0,1),cbind(A=c(.5,.5,.2)),rep(1,3))["A","AUPRC"]-7/12)<1e-12,
      abs(weighted_metrics(c(1,0,1),cbind(A=c(.5,.5,.2)),rep(1,3))["A","AUROC"]-.25)<1e-12)
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
  metric_preflight<-function(){
    y<-c(0,1,1,0,1,0,1,0);p<-cbind(A=c(.1,.2,.2,.8,.7,.3,.9,.2),B=rep(.2,8),C=c(0,1,1,0,1,0,1,0),D=c(0,1,1,0,1,0,1,0))
    oracle<-function(y,p){au<-mean(outer(p[y==1],p[y==0],">")+ .5*outer(p[y==1],p[y==0],"=="));th<-sort(unique(p),decreasing=TRUE)
      ap<-sum(vapply(th,function(t)sum(y[p==t])/sum(y)*mean(y[p>=t]),numeric(1)));c(AUROC=au,AUPRC=ap,Brier=mean((y-p)^2))}
    setup<-metric_setup(y,p);z<-weighted_metrics(y,p,rep(1,8),setup)
    for(a in colnames(p))stopifnot(max(abs(z[a,1:3]-oracle(y,p[,a])))<1e-12)
    stopifnot(z["B","AUROC"]==.5,z["B","AUPRC"]==mean(y),z["C","AUROC"]==1)
    ii<-c(1,2,2,4,5,6,6,8);w<-tabulate(ii,8);zz<-weighted_metrics(y,p,w,setup)
    for(a in colnames(p))stopifnot(max(abs(zz[a,1:3]-oracle(y[ii],p[ii,a])))<1e-12)
    B<-19L;bs<-evaluate_external(y,p,B=B,seed=7L);set.seed(7L)
    for(i in 1:B){ii<-sample.int(8,8,replace=TRUE);if(length(unique(y[ii]))==2)for(a in colnames(p))stopifnot(max(abs(bs$bootstrap[i,a,1:3]-oracle(y[ii],p[ii,a])))<1e-12)}
    h<-rep(letters[1:4],each=2);hb<-evaluate_external(y,p,h,B=B,seed=9L);set.seed(9L)
    for(i in 1:B){hs<-sample.int(4,4,replace=TRUE);ii<-unlist(lapply(hs,function(hh)which(h==letters[hh])))
      for(a in colnames(p))stopifnot(max(abs(hb$bootstrap[i,a,1:3]-oracle(y[ii],p[ii,a])))<1e-12)}
    d<-bs$differences[bs$differences$comparison=="D-C",];stopifnot(all(d$estimate==0),all(d$lower==0),all(d$upper==0))
    bad<-p;bad[1,1]<-NA;stopifnot(inherits(try(metric_setup(y,bad),silent=TRUE),"try-error"))
    # Constant risk has identifiable offset intercept, but not joint slope.
    cal<-calibration(y,rep(.2,8));stopifnot(abs(cal$estimate[cal$parameter=="intercept_offset"]-(qlogis(.5)-qlogis(.2)))<1e-6,
      is.na(cal$estimate[cal$parameter=="slope"]))
    tmp<-tempfile(fileext=".pdf");on.exit(unlink(tmp),add=TRUE)
    draw_table_pdf(list("one"=data.frame(Item="alpha",N=1),"two"=data.frame(Item="beta",N=2)),"Synthetic PDF preflight",tmp)
    stopifnot(pdf_pages(tmp)==2)
    invisible(TRUE)
  }
  if(isTRUE(getOption("ami_step13.functions_only",FALSE)))return(invisible(list(modeling_preflight=modeling_preflight,metric_preflight=metric_preflight,synthetic_bundle=synthetic_bundle,fit_state=fit_state,run_split=run_split,fit_final=fit_final,predict_frozen=predict_frozen,metric_setup=metric_setup,weighted_metrics=weighted_metrics,evaluate_external=evaluate_external,calibration=calibration,make_visuals=make_visuals,main=main,nested_folds=nested_folds,hospital_bundle=hospital_bundle,hospital_bit=hospital_bit,endpoint_preflight=endpoint_preflight)))
  status<-"FAILED";current_module<-"input verification"
  failure_message<-NULL
  tryCatch(withCallingHandlers(main(),warning=function(w){add_warning(conditionMessage(w),current_module);invokeRestart("muffleWarning")}),error=function(err){status<<-"FAILED";failure_message<<-conditionMessage(err)
    issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step13_error_detail.txt")})
  flush_warnings()
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step13-1",
    "COMPLETED_FOR_REVIEW表示MIMIC住院终点配套模型及其冻结eICU评价已生成，等待结果核对；未更新eICU预测。",
    "FAILED表示不完整，不能使用本次患者级结果建模。"),"to_share/Step13_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step13_sessionInfo.txt")
  logmsg(paste0("Step13状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step13_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share) else {
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step13_feedback.zip"),"\n",sep="") else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
