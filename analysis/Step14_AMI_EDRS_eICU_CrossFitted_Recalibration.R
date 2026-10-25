# AMI-EDRS revision | Step14: hospital-blocked cross-fitted eICU recalibration
# source(file.choose(), encoding="UTF-8")
# Fixed filenames; replaces owned results. Patient-level data and frozen references stay local_only.
local({
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MAKE_PNG=TRUE,MAKE_ZIP=TRUE)
  cfg<-utils::modifyList(cfg,getOption("ami_step14.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step14")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step14输出目录。")
  if (is_link(out)) stop("Step14输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP14")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step14目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP14_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP14_MANAGED_OUTPUT")) {
    stop("Step14输出目录的归属标记不正确。")
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
        if (unlink(p) != 0L) stop("不能覆盖旧Step14输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step14_run_log.txt")
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
    txt_out(if(nrow(z))unlist(Map(function(a,b)c(a,b,""),z$scope,z$message))else"No warnings in Step14.","local_only/Step14_warning_detail.txt")
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
  plan<-list(version="Step14_v1",models=c("A","B","C","D"),methods=c("raw","intercept","logistic"),
    primary_scheme="hospital_blocked",sensitivity_scheme="patient_stratified",folds=5L,seed=2026L,
    primary_update="intercept",secondary_update="logistic",bootstrap_n=2000L,epsilon=1e-12,
    endpoint="in_hospital_death_after_T48",minimum_training_events=10L,
    uncertainty="Paired bootstrap of fixed cross-fitted predictions; does not refit calibration or MIMIC models",
    full_eicu_fits="Separate deployment-only objects, not an independent performance assessment")
  plan<-utils::modifyList(plan,getOption("ami_step14.plan",list()))
  map<-do.call(rbind,lapply(plan$models,function(a)data.frame(variant=paste(a,plan$methods,sep="_"),model=a,method=plan$methods,stringsAsFactors=FALSE)))
  pairs<-do.call(rbind,lapply(plan$models,function(a)data.frame(first=paste(a,c("intercept","logistic","logistic"),sep="_"),second=paste(a,c("raw","raw","intercept"),sep="_"),comparison=paste(a,c("I-R","L-R","L-I")),stringsAsFactors=FALSE)))
  labels<-c(A="A: Baseline",B="B: Baseline + phenotype",C="C: Continuous dynamics",D="D: Dynamics + phenotype")
  method_labels<-c(raw="Raw",intercept="Intercept",logistic="Intercept + slope")
  colors<-c(raw="#7A8797",intercept="#D98C57",logistic="#8B68A6")
  scheme_labels<-c(hospital_blocked="Hospital-blocked five-fold",patient_stratified="Patient-stratified five-fold")
  hospital_folds<-function(hospital,k=5L,seed=2026L){
    h<-as.character(hospital)
    if(anyNA(h)||any(!nzchar(trimws(h)))||length(unique(h))<k)stop("按医院分折需要完整医院ID及至少5家医院。")
    ids<-sort(unique(h));size<-tabulate(match(h,ids),length(ids))
    assignment<-with_seed(seed,{
      priority<-runif(length(ids));orderh<-order(-size,priority,ids);load<-rep(0,k);result<-integer(length(ids));tie_order<-sample.int(k)
      for(j in orderh){free<-which(load==min(load));f<-tie_order[tie_order%in%free][1];result[j]<-f;load[f]<-load[f]+size[j]};result
    })
    assignment[match(h,ids)]
  }
  fit_update<-function(y,lp,method){
    y<-binary_outcome(y)
    if(length(y)!=length(lp)||anyNA(y)||any(!y%in%0:1)||any(!is.finite(lp)))stop("再校准输入无效。")
    if(any(table(factor(y,levels=0:1))<plan$minimum_training_events))stop("校准训练折的事件或非事件数不足。")
    if(!method%in%c("intercept","logistic"))stop("未知更新方式。")
    x<-if(method=="intercept")matrix(1,length(y),1,dimnames=list(NULL,"intercept"))else cbind(intercept=1,slope=lp)
    off<-if(method=="intercept")lp else rep(0,length(y));ww<-character()
    fit<-withCallingHandlers(stats::glm.fit(x=x,y=y,offset=off,family=stats::binomial(),control=stats::glm.control(maxit=100,epsilon=1e-10)),warning=function(w){ww<<-c(ww,conditionMessage(w));invokeRestart("muffleWarning")})
    if(!isTRUE(fit$converged)||isTRUE(fit$boundary)||fit$rank!=ncol(x)||any(!is.finite(fit$coefficients))||length(ww))
      stop("再校准拟合不稳定或不收敛；未使用替代风险。",if(length(ww))paste(ww,collapse="; ")else"")
    a<-unname(fit$coefficients[1]);b<-if(method=="intercept")1 else unname(fit$coefficients[2])
    list(method=method,intercept=a,slope=b,training_n=length(y),training_events=sum(y),iterations=fit$iter,
      converged=fit$converged,positive_slope=b>0)
  }
  apply_update<-function(lp,object){
    if(!is.numeric(lp)||any(!is.finite(lp))||!is.finite(object$intercept)||!is.finite(object$slope))stop("更新推理输入或参数无效。")
    eta<-object$intercept+object$slope*lp
    list(probability=stats::plogis(eta),linear_predictor=eta)
  }
  crossfit<-function(y,p,lp,fold,hospital,scheme){
    if(!identical(dim(p),dim(lp))||!identical(colnames(p),plan$models)||!identical(colnames(lp),plan$models)||length(fold)!=length(y)||!setequal(fold,seq_len(plan$folds))||anyNA(fold))stop("交叉拟合的列名、行数或折编号不符。")
    metric_setup(y,p)
    pp<-eta<-matrix(NA_real_,length(y),nrow(map),dimnames=list(NULL,map$variant));models<-list();audit<-coefrows<-within<-list()
    for(a in plan$models){pp[,paste0(a,"_raw")]<-p[,a];eta[,paste0(a,"_raw")]<-lp[,a]}
    for(f in seq_len(plan$folds)){
      tr<-which(fold!=f);va<-which(fold==f);overlap<-length(intersect(unique(hospital[tr]),unique(hospital[va])))
      if(length(intersect(tr,va))||scheme=="hospital_blocked"&&overlap>0)stop("校准训练与评估患者/医院重叠。")
      if(length(unique(y[va]))!=2)stop("某校准评估折仅有单一结局；按预设分折停止，不能挑选其他种子。")
      audit[[f]]<-data.frame(scheme=scheme,fold=f,training_n=length(tr),training_events=sum(y[tr]),assessment_n=length(va),assessment_events=sum(y[va]),
        training_hospitals=length(unique(hospital[tr])),assessment_hospitals=length(unique(hospital[va])),shared_hospitals=overlap,shared_patients=0L)
      models[[as.character(f)]]<-list()
      for(a in plan$models)for(method in c("intercept","logistic")){
        v<-paste(a,method,sep="_");obj<-fit_update(y[tr],lp[tr,a],method);pred<-apply_update(lp[va,a],obj)
        pp[va,v]<-pred$probability;eta[va,v]<-pred$linear_predictor;models[[as.character(f)]][[v]]<-obj
        coefrows[[length(coefrows)+1L]]<-data.frame(scheme=scheme,fold=f,model=a,method=method,intercept=obj$intercept,slope=obj$slope,training_n=obj$training_n,training_events=obj$training_events,iterations=obj$iterations,positive_slope=obj$positive_slope)
      }
      for(a in plan$models){cols<-paste(a,plan$methods,sep="_");mm<-weighted_metrics(y[va],pp[va,cols,drop=FALSE],rep(1,length(va)))
        within[[paste(f,a)]]<-data.frame(scheme=scheme,fold=f,model=a,method=plan$methods,n=length(va),events=sum(y[va]),mm,row.names=NULL)
      }
    }
    metric_setup(y,pp)
    if(any(!is.finite(eta)))stop("更新后的线性预测值非有限。")
    list(prediction=pp,linear_predictor=eta,fold=fold,models=models,audit=do.call(rbind,audit),coefficients=do.call(rbind,coefrows),within_fold_metrics=do.call(rbind,within))
  }
  warning_audit<-function(path){
    lines<-readLines(path,warn=FALSE,encoding="UTF-8");groups<-split(lines,cumsum(!nzchar(trimws(lines))));out<-list()
    for(g in groups){g<-g[nzchar(trimws(g))];if(!length(g)||grepl("^No warnings",g[1]))next
      if(length(g)<2L)stop("Step13警告明细格式不符。")
      msg<-paste(g[-1],collapse=" ")
      category<-if(grepl("glmnet|Convergence for.*lambda|error code -[0-9]",msg,ignore.case=TRUE))"glmnet_path_warning"else if(grepl("did not converge|k-means|kmeans",msg,ignore.case=TRUE))"clustering_convergence_warning"else"other_local_warning"
      out[[length(out)+1L]]<-data.frame(scope=g[1],category=category)
    }
    if(!length(out))return(data.frame(scope=character(),category=character(),n=integer()))
    z<-do.call(rbind,out);aggregate(list(n=rep(1L,nrow(z))),list(scope=z$scope,category=z$category),sum)
  }
  with_seed<-function(seed,expr){
    old<-if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))get(".Random.seed",.GlobalEnv)else NULL
    on.exit(if(is.null(old)){if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv)}else assign(".Random.seed",old,.GlobalEnv))
    set.seed(seed);force(expr)
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
  verify_lineage<-function(step){
    root<-file.path(cfg$OUTPUT_PARENT,step);sp<-file.path(root,"to_share",paste0(step,"_STATUS.txt"))
    if(!file.exists(sp)||!"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(sp,warn=FALSE))stop("请先完成",step,"。")
    lin<-read.csv(file.path(root,"to_share/01_inputs.csv"),stringsAsFactors=FALSE)
    if(!all(c("file","md5")%in%names(lin))||!nrow(lin)||anyNA(lin$file)||anyNA(lin$md5))stop("上游输入审计无效：",step)
    for(i in seq_len(nrow(lin)))if(!file.exists(lin$file[i])||unname(tools::md5sum(lin$file[i]))!=lin$md5[i])stop(step,"输入已变化，请先重跑该步骤。")
  }
  main<-function(){
    current_module<<-"preflight";logmsg("小型预检：医院隔离、字符/因子结局、校准训练隔离、offset解析核对和批量推理。")
    preflight();txt_out("PASS: deterministic hospital-blocked folds; held-out outcomes cannot change training fits; intercept root oracle; factor-safe outcome parsing; single/batch prediction; paired metric oracle.","to_share/02_preflight.txt")
    current_module<<-"Step13 frozen predictions and warning review";verify_lineage("Step13")
    root<-file.path(cfg$OUTPUT_PARENT,"Step13")
    paths<-c(prediction=file.path(root,"local_only/eICU_T48_hospital_frozen_predictions.rds"),
      model=file.path(root,"local_only/MIMIC_T48_hospital_frozen_pipeline.rds"),
      metrics=file.path(root,"to_share/05_hospital_metrics.csv"),tuning=file.path(root,"to_share/04_tuning_logloss.csv"),
      cluster=file.path(root,"to_share/03_cluster_selection.csv"),roundtrip=file.path(root,"to_share/02_prediction_roundtrip.csv"),
      warnings=file.path(root,"local_only/Step13_warning_detail.txt"),warning_summary=file.path(root,"to_share/09_warning_summary.csv"))
    if(!all(file.exists(paths)))stop("Step13本地预测、模型或审计不完整。")
    h<-setNames(unname(tools::md5sum(paths)),names(paths));csv_out(data.frame(file=unname(paths),md5=unname(h)),"to_share/01_inputs.csv")
    z<-readRDS(paths["prediction"]);obj<-readRDS(paths["model"])
    if(!identical(z$version,"Step13_v1")||!identical(obj$version,"Step13_v1")||obj$endpoint!=plan$endpoint||z$prediction_endpoint!=plan$endpoint||z$evaluation_endpoint!=plan$endpoint||obj$training_n!=5133L||!identical(names(obj$models),plan$models)||!identical(z$model_hash,hash_object(obj)))stop("Step13冻结对象版本、终点、训练人数或指纹不符。")
    y<-binary_outcome(z$outcomes$event_primary);p<-z$prediction;lp<-z$linear_predictor;keys<-as.character(z$metadata$patient_key);hospital<-as.character(z$metadata$hospital_id)
    if(length(y)!=5284L||sum(y)!=219L||nrow(z$metadata)!=length(y)||anyNA(keys)||any(!nzchar(keys))||anyDuplicated(keys)||any(z$metadata$source!="eICU")||any(z$metadata$landmark!="T48")||any(z$outcomes$endpoint!=plan$endpoint))stop("eICU冻结患者或结局不符。")
    if(!is.matrix(p)||!is.matrix(lp)||!identical(dim(p),dim(lp))||nrow(p)!=length(y)||!identical(colnames(p),plan$models)||!identical(colnames(lp),plan$models)||any(!is.finite(lp)))stop("冻结概率/线性预测值格式不符。")
    metric_setup(y,p);err<-max(abs(stats::plogis(lp)-p));if(err>1e-12)stop("冻结概率不能由保存的线性预测值复现。")
    if(length(hospital)!=length(y)||anyNA(hospital)||any(!nzchar(trimws(hospital)))||length(unique(hospital))<5L)stop("本步主分析要求完整医院ID及至少5家医院。")
    for(a in plan$models)if(any(!is.finite(obj$models[[a]]$coefficients)))stop("冻结MIMIC系数非有限。")
    previous<-read.csv(paths["metrics"],stringsAsFactors=FALSE);point<-weighted_metrics(y,p,rep(1,length(y)))
    check<-list()
    for(a in plan$models)for(m in colnames(point)){
      v<-previous[previous$cohort=="eICU_frozen"&previous$resampling=="patient"&previous$model==a&previous$metric==m,"estimate"]
      if(length(v)!=1L||!is.finite(v)||abs(v-point[a,m])>1e-10)stop("不能复现Step13原始外部指标。")
      check[[length(check)+1L]]<-data.frame(model=a,metric=m,Step13=v,recomputed=point[a,m],absolute_difference=abs(v-point[a,m]))
    }
    csv_out(do.call(rbind,check),"to_share/02_Step13_metric_roundtrip.csv")
    rt<-read.csv(paths["roundtrip"],stringsAsFactors=FALSE)
    if(nrow(rt)!=7L||any(!is.finite(rt$max_probability_error))||any(rt$max_probability_error>1e-10)||any(rt$phenotype_mismatches!=0))stop("Step13冻结接口核验未通过。")
    tune<-read.csv(paths["tuning"],stringsAsFactors=FALSE);selected<-tune[tune$selected,,drop=FALSE]
    if(nrow(selected)!=24L||any(!selected$converged_all_folds)||any(!is.finite(selected$mean_logloss)))stop("Step13已选参数未通过收敛核验。")
    for(a in plan$models){s<-selected[selected$scope=="full_MIMIC_tuning_only"&selected$model==a,];f<-obj$models[[a]]
      if(nrow(s)!=1L||abs(s$alpha-f$alpha)>1e-12||abs(s$lambda-f$lambda)>1e-12)stop("最终MIMIC参数与调参审计不一致。")}
    cls<-read.csv(paths["cluster"],stringsAsFactors=FALSE)
    if(any(cls$selected&(!cls$eligible|!is.na(cls$error)&nzchar(cls$error))))stop("Step13选中的分型候选无效。")
    wa<-warning_audit(paths["warnings"]);ws<-read.csv(paths["warning_summary"],stringsAsFactors=FALSE)
    if(sum(wa$n)!=sum(ws$n))stop("Step13警告明细与汇总数量不符。")
    csv_out(wa,"to_share/02_Step13_warning_categories.csv")
    csv_out(data.frame(check=c("Selected tuning rows","Selected rows finite and converged","Excluded nonconverged grid cells","Failed clustering candidates","Warnings classified","Other warning messages","Frozen coefficients finite","Frozen prediction roundtrip max error"),
      value=c(nrow(selected),sum(selected$converged_all_folds&is.finite(selected$mean_logloss)),sum(!tune$converged_all_folds),sum(!is.na(cls$error)&nzchar(cls$error)),sum(wa$n),sum(wa$n[wa$category=="other_local_warning"]),1,err)),"to_share/02_Step13_model_checks.csv")
    if(any(wa$category=="other_local_warning"))issue("NOTE","Step13 warning review","存在其他类型警告；原始明细仍在Step13/local_only，当前已选模型及冻结预测数值核验通过，不能据此宣称所有警告均已解释。")
    csv_out(data.frame(n=length(y),events=sum(y),event_fraction=mean(y),hospitals=length(unique(hospital)),endpoint=plan$endpoint),"to_share/02_cohort.csv")
    current_module<<-"frozen cross-fitting plan"
    fold<-list(hospital_blocked=hospital_folds(hospital,plan$folds,plan$seed),patient_stratified=make_folds(keys,y,plan$folds,plan$seed))
    saveRDS(list(metadata=z$metadata,outcome=y,folds=fold,plan=plan),reg("local_only/eICU_recalibration_folds.rds"),compress="gzip")
    txt_out(capture.output(dput(plan)),"to_share/02_analysis_plan.txt")
    key<-hash_object(list(inputs=h,plan=plan,folds=fold,code=lapply(c("crossfit","fit_update","apply_update","weighted_metrics","metric_setup","evaluate_external","hospital_folds","make_folds"),function(nm)paste(deparse(body(get(nm))),collapse="\n")),R=as.character(getRversion())))
    result<-list();raw_hash<-hash_object(list(p,lp,obj))
    for(scheme in names(fold)){
      current_module<<-paste0("cross-fitting ",scheme);logmsg(paste0(scheme,"：逐折拟合截距更新和截距+斜率更新，生成折外预测。"))
      result[[scheme]]<-cache_result(paste0("local_only/cache/",scheme,"_crossfit.rds"),key,function()crossfit(y,p,lp,fold[[scheme]],hospital,scheme))
      if(!identical(result[[scheme]]$fold,fold[[scheme]]))stop("交叉拟合缓存分折不符。")
      for(a in plan$models)if(max(abs(result[[scheme]]$prediction[,paste0(a,"_raw")]-p[,a]))>1e-12)stop("原始预测被更新覆盖。")
    }
    csv_out(do.call(rbind,lapply(result,function(x)x$audit)),"to_share/03_fold_audit.csv")
    cf<-do.call(rbind,lapply(result,function(x)x$coefficients));csv_out(cf,"to_share/03_crossfit_coefficients.csv")
    wf<-do.call(rbind,lapply(result,function(x)x$within_fold_metrics));csv_out(wf,"to_share/03_within_fold_metrics.csv")
    if(any(!cf$positive_slope))issue("WARNING","calibration slope","某校准折斜率非正；保留估计结果并单列参数，不据此声称校准改善。")
    saveRDS(list(version=plan$version,metadata=z$metadata,outcomes=z$outcomes,source_model_hash=z$model_hash,plan=plan,variants=map,results=result),reg("local_only/eICU_crossfitted_recalibration.rds"),compress="gzip")
    current_module<<-"paired cross-fitted performance"
    allmet<-alldif<-cal<-bins<-diag<-list()
    for(scheme in names(result)){
      q<-result[[scheme]]$prediction
      for(mode in c("patient","hospital_cluster")){
        current_module<<-paste(scheme,mode);logmsg(paste0(scheme," / ",mode,"：",plan$bootstrap_n,"次配对重抽样。"))
        ev<-cache_result(paste0("local_only/cache/",scheme,"_",mode,"_bootstrap.rds"),key,
          function()evaluate_external(y,q,if(mode=="patient")NULL else hospital,seed=plan$seed+if(mode=="patient")4000L else 5000L))
        tag<-paste(scheme,mode);allmet[[tag]]<-cbind(scheme=scheme,ev$metrics);alldif[[tag]]<-cbind(scheme=scheme,ev$differences)
      }
      cal[[scheme]]<-do.call(rbind,lapply(map$variant,function(v)cbind(scheme=scheme,variant=v,calibration(y,q[,v],hospital))))
      bins[[scheme]]<-do.call(rbind,lapply(map$variant,function(v)cbind(scheme=scheme,variant=v,calibration_bins(y,q[,v]))))
      diag[[scheme]]<-cbind(scheme=scheme,inspect_prediction(q,result[[scheme]]$linear_predictor))
    }
    met<-do.call(rbind,allmet);names(met)[names(met)=="model"]<-"variant";met$model<-map$model[match(met$variant,map$variant)];met$method<-map$method[match(met$variant,map$variant)]
    dif<-do.call(rbind,alldif);cal<-do.call(rbind,cal);bins<-do.call(rbind,bins)
    csv_out(met,"to_share/04_crossfit_metrics.csv");csv_out(dif,"to_share/04_paired_update_differences.csv")
    csv_out(cal,"to_share/05_calibration_parameters.csv");csv_out(bins,"to_share/05_calibration_bins.csv");csv_out(do.call(rbind,diag),"to_share/05_prediction_diagnostics.csv")
    if(any(!is.finite(met$lower))||any(!is.finite(dif$lower)))issue("WARNING","bootstrap intervals","部分区间有效重复不足90%；保留NA。")
    if(any(cal$status!="OK"))issue("NOTE","calibration diagnostics","部分校准诊断不稳定；保留NA及CSV原因。")
    current_module<<-"separate full-eICU update objects"
    updates<-list();fullrows<-list()
    for(a in plan$models)for(method in c("intercept","logistic")){
      v<-paste(a,method,sep="_");fit<-fit_update(y,lp[,a],method);updates[[v]]<-fit
      fullrows[[v]]<-data.frame(model=a,method=method,intercept=fit$intercept,slope=fit$slope,training_n=length(y),training_events=sum(y),purpose="deployment_only_not_independently_evaluated")
    }
    update_object<-list(version=plan$version,endpoint=plan$endpoint,source_model_hash=z$model_hash,plan=plan,updates=updates,
      note="Full eICU calibration fits for future independent evaluation only. Reported performance uses cross-fitting, not this object.")
    saveRDS(update_object,reg("local_only/eICU_full_update_for_future_validation.rds"),compress="gzip")
    csv_out(do.call(rbind,fullrows),"to_share/06_full_update_coefficients_NOT_performance.csv")
    export<-c("# Full-eICU calibration update; requires Step13 raw linear predictor. Not an independent evaluation.",
      paste0("apply_update <- ",paste(deparse(apply_update),collapse="\n")),
      'predict_step14 <- function(raw_linear_predictor, model, method, object) {',
      '  if (!identical(object$version,"Step14_v1")) stop("Wrong update version")',
      '  key <- paste(model,method,sep="_")',
      '  if (is.null(object$updates[[key]])) stop("Unknown model/update method")',
      '  apply_update(raw_linear_predictor,object$updates[[key]])$probability', '}' )
    txt_out(export,"local_only/Step14_predict_update.R")
    ex<-new.env(parent=baseenv());sys.source(safe_path("local_only/Step14_predict_update.R"),envir=ex)
    errs<-numeric()
    for(v in names(updates)){a<-strsplit(v,"_",fixed=TRUE)[[1]][1];method<-updates[[v]]$method;expected<-apply_update(lp[,a],updates[[v]])$probability
      got<-ex$predict_step14(lp[,a],a,method,update_object);errs<-c(errs,max(abs(got-expected)),abs(ex$predict_step14(lp[1,a],a,method,update_object)-expected[1]))}
    if(any(!is.finite(errs))||max(errs)>1e-12)stop("更新导出接口不一致。")
    csv_out(data.frame(check="full_update_export_single_batch",max_probability_error=max(errs),role="future_validation_only"),"to_share/06_update_export_check.csv")
    current_module<<-"large-font supplementary figures and tables";make_visuals(y,result,met,dif,cal,bins)
    csv_out(data.frame(output=c("Figure_S_eICU_hospital_blocked_recalibration","Figure_S_eICU_patient_stratified_recalibration","Table_S_eICU_crossfit_update_performance","Table_S_eICU_crossfit_update_calibration"),
      action="ADD supplementary; final external manuscript panels pending integration",endpoint=plan$endpoint),"to_share/11_figure_table_replacement_map.csv")
    txt_out(c("Step14：eICU住院死亡模型的折外再校准；只更新校准截距/斜率，MIMIC模型及评分参考冻结。",
      "本方案在查看Step13之后制定，属于修订阶段补充分析；不能称原先注册、完全未查看过的独立外部验证。",
      "主方案按医院分为5折：医院人数从大到小分配至当前人数最少的折，同规模/同负荷的顺序由seed=2026固定；不使用死亡结局平衡医院分折，不依据结果重新挑选种子。",
      "同一医院全部患者只能进入一折。每次用其余医院拟合校准，再评价当前医院折；反映已参与eICU医院之间的更新可迁移性，仍需未来独立验证。",
      "补充方案为患者级结局分层5折，用于反映同一外部总体内的更新表现。两种分折下的训练/评价患者都不重叠；患者分折允许同一医院跨折。",
      "A-D均报告Raw原始冻结预测、Intercept仅截距更新、Intercept+slope联合更新。截距更新为主要更新方式，联合更新为补充；不根据本次最优指标自动选择方法。",
      "每折直接使用Step13保存的原始线性预测值，截距更新固定斜率1，联合更新拟合截距及斜率。评估折结局不参与对应参数估计。未收敛或分离拟合会停止，不生成替代风险。",
      "两种更新的所有结果均保留，包括没有改善或变差的结果。非正斜率单独提示；不会人为翻转预测方向或强制改善。",
      "单折内的截距更新为单调变换；正斜率联合更新也保持排序。折与折参数不同，合并OOF的AUROC/AUPRC可能改变，不能把这当作新增预测信息。逐折指标另有CSV。",
      "性能主比较为更新后相对Raw的Brier/LogLoss变化，附AUROC、average precision、平均预测风险、事件率及校准参数。I-R=截距更新减Raw；L-R=联合更新减Raw；L-I=联合减截距。",
      "患者和医院整体配对bootstrap各2000次，所有模型/方法共享同一次重抽样；同一种重抽样也使用两分折方案共享的种子。有效重复不足90%则区间NA。",
      "上述区间条件于已锁定的折外预测，没有在每次bootstrap重拟合校准器或MIMIC模型；训练折重叠，区间不覆盖全部开发/更新不确定性。",
      "校准参数的Wald/医院稳健区间也条件于预测。校准图为预测十分位分组点和Wilson区间，同值合并；不能解释为平滑风险函数。",
      "完整eICU拟合的更新参数另存为future_validation对象；不使用其在同一eICU样本中的拟合后性能代替折外结果，也不在本步自动部署。",
      "本步复算Step13原始指标、检查已选参数/最终系数/导出接口，并将本地训练警告按类别汇总；不向反馈包复制患者级数据或警告原文。",
      "本步仍是T48后住院死亡；不能将这些更新参数用于原30天死亡模型。Step13原始外部结果必须与Step14更新结果同时报告。",
      "新增补充图表；新图18pt基础字号，坐标约17-19pt、说明约15pt。三线表PDF/RTF附CSV，保留固定命名和缓存续跑。",
      "无需重跑Step13、无需T7；输出修改/Step14。只回传Step14_feedback.zip，患者信息、分折和预测保留local_only。"),"to_share/00_READ_ME_FIRST.txt")
    txt_out(c("Riley et al. BMJ 2024;384:e074820. https://www.bmj.com/content/384/bmj-2023-074820",
      "Collins et al. BMJ 2024;384:e074819. https://www.bmj.com/content/384/bmj-2023-074819",
      "Efthimiou et al. BMJ 2024;386:e078276. https://www.bmj.com/content/386/bmj-2023-078276",
      "R stats::glm/offset documentation. https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html"),"to_share/10_method_sources.txt")
    if(!identical(h,setNames(unname(tools::md5sum(paths)),names(paths)))||hash_object(list(p,lp,obj))!=raw_hash)stop("运行中上游对象或原始预测变化。")
    status<<-"COMPLETED_FOR_REVIEW"
  }
  fmt<-function(x,d=3)ifelse(is.finite(x),formatC(x,format="f",digits=d),"NA")
  fmt_ci<-function(x,l,u,d=3)ifelse(is.finite(x)&is.finite(l)&is.finite(u),paste0(fmt(x,d)," [",fmt(l,d),", ",fmt(u,d),"]"),"NA")
  setup_plot<-function(mf=c(1,1),bottom=3.4){par(mfrow=mf,mar=c(4.3,4.4,2.8,1),oma=c(bottom,0,0,0),family="sans",las=1,cex=1,cex.axis=.95,cex.lab=1.05,cex.main=1.05,mgp=c(2.8,.8,0))}
  note<-function(x){for(i in seq_along(x))mtext(x[i],side=1,outer=TRUE,line=.8+(i-1)*1.15,cex=.82)}
  make_visuals<-function(y,result,met,dif,cal,bins){
    hi<-min(1,max(.3,ceiling((max(c(bins$predicted,bins$upper),na.rm=TRUE)+.02)*10)/10))
    for(scheme in names(result))save_figure(paste0("Figure_S_eICU_",scheme,"_recalibration"),local({ss<-scheme;function(){
      setup_plot(c(2,2),3.2);layout(matrix(c(1,2,3,4,5,5),nrow=3,byrow=TRUE),heights=c(1,1,.25));par(cex=1,cex.axis=.95,cex.lab=1.05,cex.main=1.05)
      for(a in plan$models){plot(0,0,type="n",xlim=c(0,hi*100),ylim=c(0,hi*100),xlab="Predicted hospital death (%)",ylab="Observed hospital death (%)",main=labels[a],adj=0,bty="l")
        abline(0,1,col="#AAAAAA",lty=3)
        for(j in seq_along(plan$methods)){method<-plan$methods[j];v<-paste(a,method,sep="_");z<-bins[bins$scheme==ss&bins$variant==v,];
          segments(100*z$predicted,100*z$lower,100*z$predicted,100*z$upper,col=adjustcolor(colors[method],.55),lwd=1.5)
          lines(100*z$predicted,100*z$observed,type="b",pch=c(16,17,15)[j],lty=j,lwd=2.3,col=colors[method],cex=1)
        }
      }
      par(mar=rep(0,4));plot.new();legend("center",legend=unname(method_labels),col=colors,lty=1:3,pch=c(16,17,15),lwd=2.3,ncol=3,bty="n",cex=.95)
      note(c(paste0(scheme_labels[ss],"; updated predictions are out of fold"),"Grouped calibration; 95% Wilson bars; same axes in all panels"))
    }}),11.8,11.7)
    tabs<-list();rlabels<-paste(map$model,c(R="R",I="I",L="L")[rep(1:3,4)],sep=" / ")
    for(scheme in names(result))for(mode in c("patient","hospital_cluster")){
      tt<-data.frame(`Model / update`=rlabels,check.names=FALSE)
      for(mm in c("AUROC","AUPRC","Brier")){v<-met[met$scheme==scheme&met$resampling==mode&met$metric==mm,];v<-v[match(map$variant,v$variant),];tt[[paste0(mm," [95% CI]")]]<-fmt_ci(v$estimate,v$lower,v$upper)}
      tabs[[paste(scheme_labels[scheme],"/",mode)]]<-tt
      tt<-data.frame(Comparison=pairs$comparison,check.names=FALSE)
      for(mm in c("Brier","LogLoss","AUROC")){v<-dif[dif$scheme==scheme&dif$resampling==mode&dif$metric==mm,];v<-v[match(pairs$comparison,v$comparison),];tt[[paste0("Delta ",mm)]]<-fmt_ci(v$estimate,v$lower,v$upper,if(mm=="AUROC")3 else 4)}
      tabs[[paste(scheme_labels[scheme],"/",mode,"paired changes")]]<-tt
    }
    three_line(tabs,"eICU hospital-risk updating: cross-fitted performance","Table_S_eICU_crossfit_update_performance",c(
      "R: raw frozen prediction; I: intercept update; L: intercept and slope update. A-D are the Step13 companion models.",
      "Differences are first minus second. Negative Brier or LogLoss changes favor the first method; AUPRC = average precision.",
      "Hospital-blocked folding is primary; patient-stratified folding is sensitivity. Every update uses other folds only.",
      paste0("Paired bootstrap: ",plan$bootstrap_n," resamples; fixed predictions, no refitting. Training-fold dependence is not fully covered."),
      "Pooled AUROC can change across fold-specific transformations without added predictor information."))
    tabs<-list()
    for(scheme in names(result))for(inf in c("model_based","hospital_robust")){
      tt<-data.frame(`Model / update`=rlabels,check.names=FALSE)
      v<-met[met$scheme==scheme&met$resampling=="patient"&met$metric=="Mean_prediction",];v<-v[match(map$variant,v$variant),];tt[["Mean risk %"]]<-fmt(100*v$estimate,2)
      for(pm in c("intercept_offset","slope")){v<-cal[cal$scheme==scheme&cal$inference==inf&cal$parameter==pm,];v<-v[match(map$variant,v$variant),];tt[[if(pm=="intercept_offset")"Offset intercept [95% CI]"else"Slope [95% CI]"]]<-fmt_ci(v$estimate,v$lower,v$upper,2)}
      tabs[[paste(scheme_labels[scheme],"/",inf)]]<-tt
    }
    three_line(tabs,"eICU hospital-risk updating: cross-fitted calibration","Table_S_eICU_crossfit_update_calibration",c(
      "R: raw; I: intercept update; L: intercept and slope update. Parameters here diagnose the resulting predictions.",
      sprintf("All methods use the same %s eICU patients, %s hospital deaths, observed event rate %.2f%%.",length(y),sum(y),100*mean(y)),
      "Offset intercept fixes slope at 1; the reported slope comes from a joint intercept/slope diagnostic fit.",
      "Wald intervals condition on the fitted predictions; rank-deficient or unstable fits are NA. Joint intercepts are in CSV.",
      "These tables assess cross-fitted updates, not apparent performance of full-eICU update objects."))
  }
  binary_outcome<-function(x){
    if(is.factor(x))x<-as.character(x)
    if(is.logical(x))x<-as.integer(x)
    if(!is.numeric(x)&&!is.character(x))stop("结局必须为0/1。")
    y<-suppressWarnings(as.numeric(x));if(anyNA(y)||any(!is.finite(y))||any(!y%in%0:1))stop("结局含缺失或非0/1值。")
    as.integer(y)
  }
  preflight<-function(){
    h<-rep(paste0("synthetic_hospital_",1:12),each=20);n<-length(h);keys<-paste0("synthetic_patient_",seq_len(n));fold<-hospital_folds(h)
    stopifnot(setequal(fold,1:5),all(vapply(split(fold,h),function(v)length(unique(v))==1,logical(1))),identical(rev(hospital_folds(rev(h))),fold))
    dat<-with_seed(171L,{lp<-rnorm(n);list(lp=lp,y=rbinom(n,1,plogis(-.4+.6*lp)))})
    y<-dat$y;lp<-dat$lp;tr<-which(fold!=1);va<-which(fold==1)
    a<-fit_update(y[tr],lp[tr],"intercept");root<-uniroot(function(a)sum(plogis(a+lp[tr]))-sum(y[tr]),c(-40,40),tol=1e-10)$root
    stopifnot(abs(a$intercept-root)<1e-7)
    joint<-fit_update(y[tr],lp[tr],"logistic");changed<-y;changed[va]<-1-changed[va]
    stopifnot(identical(joint,fit_update(changed[tr],lp[tr],"logistic")))
    q<-apply_update(lp[va],joint)$probability;stopifnot(abs(q[1]-apply_update(lp[va[1]],joint)$probability)<1e-12)
    stopifnot(identical(binary_outcome(factor(c("0","1"),levels=c("1","0"))),0:1),inherits(try(binary_outcome(c("0","missing")),silent=TRUE),"try-error"))
    yy<-c(0L,1L,1L,0L);p<-cbind(A_raw=c(.2,.6,.6,.8),A_intercept=rep(.5,4));m<-weighted_metrics(yy,p,rep(1,4))
    stopifnot(abs(m["A_raw","AUROC"]-.5)<1e-12,abs(m["A_intercept","AUROC"]-.5)<1e-12,abs(m["A_intercept","AUPRC"]-.5)<1e-12)
    ff<-make_folds(keys,y);stopifnot(identical(rev(make_folds(rev(keys),rev(y))),ff))
    TRUE
  }
  if(isTRUE(getOption("ami_step14.functions_only",FALSE)))return(invisible(list(main=main,preflight=preflight,hospital_folds=hospital_folds,make_folds=make_folds,fit_update=fit_update,apply_update=apply_update,crossfit=crossfit,weighted_metrics=weighted_metrics,evaluate_external=evaluate_external,make_visuals=make_visuals,warning_audit=warning_audit)))
  status<-"FAILED";current_module<-"input verification"
  failure_message<-NULL
  tryCatch(withCallingHandlers(main(),warning=function(w){add_warning(conditionMessage(w),current_module);invokeRestart("muffleWarning")}),error=function(err){status<<-"FAILED";failure_message<<-conditionMessage(err)
    issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step14_error_detail.txt")})
  flush_warnings()
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step14",
    "COMPLETED_FOR_REVIEW表示eICU折外再校准评价已生成，等待结果核对；全量更新对象仅供后续独立评价。",
    "FAILED表示不完整，不能使用本次患者级结果建模。"),"to_share/Step14_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step14_sessionInfo.txt")
  logmsg(paste0("Step14状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step14_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share) else {
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step14_feedback.zip"),"\n",sep="") else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
