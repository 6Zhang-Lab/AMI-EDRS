# AMI-EDRS revision | Step9: independent T6 / T24 landmark nested validation
# source(file.choose(), encoding="UTF-8")
# No timestamp filenames. Valid caches resume; script-owned results are overwritten.
# Captures available previous-session warnings immediately; full warning text stays local_only.
local({
  previous_session_warnings<-base::warnings()
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MAKE_PNG=TRUE,MAKE_ZIP=TRUE,RESUME=TRUE)
  cfg<-utils::modifyList(cfg,getOption("ami_step9.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step9")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step9输出目录。")
  if (is_link(out)) stop("Step9输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP9")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step9目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP9_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP9_MANAGED_OUTPUT")) {
    stop("Step9输出目录的归属标记不正确。")
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
        if (unlink(p) != 0L) stop("不能覆盖旧Step9输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step9_run_log.txt")
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
  model_names<-c(A="Baseline",C="Baseline + continuous physiology")
  expected<-data.frame(landmark=c("T6","T24","T48"),n=c(5513L,5411L,5133L),events=c(825L,738L,630L))
  plan<-list(version="Step9_v1",early_landmarks=c("T6","T24"),reference_landmark="T48",outer=5L,inner=5L,seed=2026L,
    alpha=c(.1,.5,1),lambda=10^seq(1,-4,length.out=31),criterion="pooled inner-assessment log loss",
    min_reference_n=20L,min_domain_fraction=.5,preprocess_min_n=20L,max_missing=.95,
    bootstrap_repeats=2000L,descriptive_thresholds=c(.05,.15,.30),
    endpoint="All-cause death within 30 days after each landmark; separate eligible populations",
    windows="Cumulative windows ending at or before the landmark; no T48 phenotype in early models")
  plan<-utils::modifyList(plan,getOption("ami_step9.plan",list()))
  score_columns9<-function(windows)unlist(lapply(windows,function(w)paste(main_domains,gsub("-","_",w,fixed=TRUE),"score",sep="__")),use.names=FALSE)
  with_seed<-function(seed,expr){
    old<-if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))get(".Random.seed",.GlobalEnv)else NULL
    on.exit(if(is.null(old)){if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv)}else assign(".Random.seed",old,.GlobalEnv))
    set.seed(seed);force(expr)
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
  evaluate8<-function(y,p,pairs,seed){
    arms<-colnames(p);point<-sapply(arms,function(a)metrics(y,p[,a]));boot<-array(NA_real_,c(plan$bootstrap_repeats,nrow(point),ncol(point)),dimnames=list(NULL,rownames(point),arms))
    with_seed(seed,for(i in seq_len(plan$bootstrap_repeats)){
      ii<-sample.int(length(y),replace=TRUE)
      for(a in arms)boot[i,,a]<-metrics(y[ii],p[ii,a])
    })
    ci<-function(x){v<-x[is.finite(x)];if(!length(v))return(c(NA_real_,NA_real_));as.numeric(stats::quantile(v,c(.025,.975)))}
    out<-list();for(a in arms)for(m in rownames(point)){
      z<-ci(boot[,m,a]);out[[length(out)+1L]]<-data.frame(model=a,metric=m,estimate=point[m,a],lower=z[1],upper=z[2],valid_bootstrap=sum(is.finite(boot[,m,a])))
    }
    dif<-list();for(pair in pairs)for(m in rownames(point)){
      v<-boot[,m,pair[1]]-boot[,m,pair[2]];z<-ci(v)
      dif[[length(dif)+1L]]<-data.frame(comparison=paste(pair,collapse=" minus "),metric=m,estimate=point[m,pair[1]]-point[m,pair[2]],lower=z[1],upper=z[2],valid_bootstrap=sum(is.finite(v)))
    }
    list(metrics=do.call(rbind,out),differences=if(length(dif))do.call(rbind,dif)else data.frame(),
      calibration=do.call(rbind,lapply(arms,function(a)data.frame(model=a,t(calibration(y,p[,a]))))))
  }
  calibration_bins<-function(y,p){
    do.call(rbind,lapply(colnames(p),function(a){
      br<-unique(as.numeric(stats::quantile(p[,a],seq(0,1,.1))))
      g<-if(length(br)>1L)cut(p[,a],breaks=br,include.lowest=TRUE,labels=FALSE)else rep(1L,length(y))
      do.call(rbind,lapply(sort(unique(g)),function(j)data.frame(model=a,bin=j,n=sum(g==j),events=sum(y[g==j]),predicted=mean(p[g==j,a]),observed=mean(y[g==j]))))
    }))
  }
  threshold_counts<-function(y,p){
    div<-function(a,b)if(b>0)a/b else NA_real_
    do.call(rbind,lapply(colnames(p),function(a)do.call(rbind,lapply(plan$descriptive_thresholds,function(t){
      pos<-p[,a]>=t;tp<-sum(y[pos]);fp<-sum(1-y[pos]);fn<-sum(y[!pos]);tn<-sum(1-y[!pos])
      data.frame(model=a,threshold=t,n=length(y),events=sum(y),tp=tp,fp=fp,fn=fn,tn=tn,
        sensitivity=div(tp,tp+fn),specificity=div(tn,tn+fp),ppv=div(tp,tp+fp),npv=div(tn,tn+fn),alerts_per_100=100*mean(pos))
    }))))
  }
  warning_rows<-list()
  warning_category<-function(x){
    if(grepl("Convergence for|convergence for|did not converge|not converged|convergence.*not reached",x,ignore.case=TRUE)&&grepl("lambda|glmnet|maxit|iterations",x,ignore.case=TRUE))return("glmnet path convergence warning")
    if(grepl("deprecated",x,ignore.case=TRUE))return("deprecated argument or API")
    if(grepl("glm.fit|fitted probabilities numerically 0 or 1",x,ignore.case=TRUE))return("GLM calibration warning")
    if(grepl("font|cairo|encoding|conversion failure",x,ignore.case=TRUE))return("font or graphics encoding warning")
    "Other warning: inspect local detail"
  }
  add_warnings<-function(messages,origin,scope){
    if(!length(messages))return(invisible(NULL))
    for(s in as.character(messages))warning_rows[[length(warning_rows)+1L]]<<-data.frame(origin=origin,scope=scope,
      category=warning_category(s),message=s,stringsAsFactors=FALSE)
    invisible(NULL)
  }
  capture_previous_warnings<-function(){
    txt_out(if(is.null(previous_session_warnings))"No warnings object available from the previous R session state. This does not establish that Step8 had no warnings."else capture.output(print(previous_session_warnings)),
      "local_only/previous_session_warnings.txt")
    msg<-if(is.null(previous_session_warnings))character()else names(previous_session_warnings)
    add_warnings(msg,"previous_R_session_unattributed","before_Step9")
    csv_out(data.frame(warnings_object_available=!is.null(previous_session_warnings),captured_n=length(msg),
      attribution="May be Step8 only if it is still the most recent warning-producing call; not verifiable automatically"),"to_share/00_previous_warning_capture.csv")
  }
  flush_warnings<-function(){
    if(!length(warning_rows)){
      csv_out(data.frame(origin=character(),scope=character(),category=character(),n=integer()),"to_share/09_warning_summary.csv")
      txt_out("No warnings were captured; cached split warnings are also retained when present.","local_only/Step9_warning_detail.txt")
    }else{
      z<-do.call(rbind,warning_rows);tab<-stats::aggregate(rep(1L,nrow(z)),z[c("origin","scope","category")],sum);names(tab)[4]<-"n"
      csv_out(tab,"to_share/09_warning_summary.csv")
      txt_out(unlist(lapply(seq_len(nrow(z)),function(i)c(paste0("[",z$origin[i]," | ",z$scope[i]," | ",z$category[i],"]"),z$message[i],""))),"local_only/Step9_warning_detail.txt")
      logmsg(paste0("警告已留存：",nrow(z),"条（含可恢复的上次会话及复用缓存警告）；分类计数进入反馈，完整原文在local_only。"))
    }
  }
  fit_path9<-function(x,y){
    if(anyNA(y)||!all(y%in%0:1)||length(unique(y))!=2||any(!is.finite(x))||ncol(x)<2L)stop("模型训练输入无效。")
    beta<-list();grid<-list();audit<-list()
    for(a in plan$alpha){
      fit<-glmnet::glmnet(x=x,y=y,family="binomial",alpha=a,lambda=plan$lambda,standardize=FALSE,intercept=TRUE,maxit=100000L,thresh=1e-7)
      if(fit$jerr>0)stop("glmnet参数或训练数据错误：jerr=",fit$jerr)
      bb<-matrix(NA_real_,ncol(x)+1L,length(plan$lambda),dimnames=list(c("(Intercept)",colnames(x)),NULL))
      if(length(fit$lambda)){
        ii<-vapply(fit$lambda,function(l)which.min(abs(log(plan$lambda)-log(l))),integer(1))
        if(anyDuplicated(ii)||max(abs(log(fit$lambda)-log(plan$lambda[ii])))>1e-8)stop("返回未请求的lambda。")
        bb[,ii]<-as.matrix(stats::coef(fit))
      }
      beta[[length(beta)+1L]]<-bb;grid[[length(grid)+1L]]<-data.frame(alpha=a,lambda=plan$lambda)
      audit[[length(audit)+1L]]<-data.frame(alpha=a,jerr=fit$jerr,requested_lambda_n=length(plan$lambda),returned_lambda_n=length(fit$lambda))
    }
    list(beta=do.call(cbind,beta),grid=do.call(rbind,grid),convergence=do.call(rbind,audit))
  }
  fit_state9<-function(b,tr){
    lm<-unique(as.character(b$metadata$landmark));if(length(lm)!=1L||!lm%in%c("T6","T24","T48"))stop("landmark不一致。")
    windows<-engine$windows_for(lm,"cumulative");fp<-b$feature_plan
    dynamic<-unique(fp$feature[fp$role=="main_continuous"])
    if(length(dynamic)!=84L*length(windows)||!all(dynamic%in%names(b$raw_numeric)))stop("早期连续特征定义不一致。")
    wanted<-unlist(lapply(windows,function(w){core<-unique(engine$specification()$variable[engine$specification()$tier=="main"])
      as.vector(t(outer(core,c("mean_value","sd_value","slope_per_hour","abnormal_fraction"),function(v,s)engine$key(v,w,s))))}),use.names=FALSE)
    if(!setequal(dynamic,wanted))stop("早期特征出现超过预测时间的窗口或错误统计量。")
    # Only training rows from this landmark determine the shared 0-6h reference.
    list(landmark=lm,windows=windows,reference=engine$fit_reference(b$raw_numeric,tr,min_observed=plan$min_reference_n),
      dynamic_columns=dynamic,score_columns=score_columns9(windows))
  }
  design9<-function(b,rows,state){
    raw<-b$raw_numeric[rows,,drop=FALSE];base<-b$baseline[rows,c("age","sex_male","in_index_icu"),drop=FALSE]
    sc<-engine$apply_reference(raw,state$reference,state$windows,min_fraction=plan$min_domain_fraction)$scores[state$score_columns]
    ans<-list(A=base,C=cbind(base,raw[state$dynamic_columns],sc));for(a in names(ans))rownames(ans[[a]])<-NULL
    ans
  }
  run_split9<-function(b,tr,va,chosen=NULL){
    captured<-character()
    out<-withCallingHandlers({
      state<-fit_state9(b,tr);train<-design9(b,tr,state);test<-design9(b,va,state)
      loss<-list();pred<-list();models<-list();audit<-list();convergence<-list();grid<-NULL
      for(a in names(model_names)){
        pp<-engine$fit_preprocessor(train[[a]],seq_along(tr),max_missing=plan$max_missing,min_observed=plan$preprocess_min_n,add_missing=TRUE)
        x<-engine$apply_preprocessor(train[[a]],pp);xx<-engine$apply_preprocessor(test[[a]],pp)
        path<-fit_path9(x,b$outcomes$event_primary[tr]);pr<-path_predict(path,xx)
        if(!is.null(grid)&&!identical(grid,path$grid))stop("调参网格不一致。")
        grid<-path$grid;loss[[a]]<-logloss_sum(b$outcomes$event_primary[va],pr)
        audit[[a]]<-data.frame(model=a,requested=ncol(train[[a]]),retained=length(pp$features),missing_indicators=length(pp$indicators),training_n=length(tr),assessment_n=length(va))
        convergence[[a]]<-cbind(model=a,path$convergence)
        if(!is.null(chosen)){
          j<-chosen[[a]];if(any(!is.finite(pr[,j])))stop("选中参数在外训练折未收敛：",a)
          pred[[a]]<-pr[,j];models[[a]]<-list(preprocessor=pp,coefficients=path$beta[,j],alpha=grid$alpha[j],lambda=grid$lambda[j])
        }
      }
      out<-list(grid=grid,loss=loss,assessment_keys=b$metadata$patient_key[va],design_audit=do.call(rbind,audit),convergence=do.call(rbind,convergence))
      if(!is.null(chosen)){out$prediction<-do.call(cbind,pred);out$state<-state;out$models<-models}
      out
    },warning=function(w){captured<<-c(captured,conditionMessage(w));invokeRestart("muffleWarning")})
    out$warnings<-captured;out
  }
  predict9<-function(b,rows,obj){
    d<-design9(b,rows,obj$state);p<-matrix(NA_real_,length(rows),2,dimnames=list(NULL,names(model_names)))
    for(a in names(model_names)){
      m<-obj$models[[a]];x<-engine$apply_preprocessor(d[[a]],m$preprocessor)
      if(!identical(names(m$coefficients),c("(Intercept)",colnames(x))))stop("冻结预测顺序不一致。")
      p[,a]<-as.numeric(stats::plogis(cbind(1,x)%*%m$coefficients))
    };if(any(!is.finite(p)))stop("早期预测不完整。")
    p
  }
  check_bundle9<-function(b,f,lm){
    ex<-expected[expected$landmark==lm,];keys<-b$metadata$patient_key;n<-length(keys)
    if(!identical(b$version,"Step6_v1")||n!=ex$n||sum(b$outcomes$event_primary)!=ex$events||anyNA(keys)||anyDuplicated(keys)||
      any(b$metadata$source!="MIMIC")||any(b$metadata$landmark!=lm)||any(b$outcomes$endpoint!="30_day_all_cause_death_after_landmark")||
      anyNA(b$outcomes$event_primary)||any(!b$outcomes$event_primary%in%0:1)||nrow(b$raw_numeric)!=n||nrow(b$baseline)!=n)stop("冻结队列或终点校验失败：",lm)
    if(f$k!=5L||anyDuplicated(f$outer$patient_key)||!setequal(keys,f$outer$patient_key))stop("患者分折不匹配：",lm)
    outer<-f$outer$outer_fold[match(keys,f$outer$patient_key)]
    if(anyNA(outer)||!setequal(outer,1:5))stop("外折无效。")
    for(o in 1:5){z<-f$inner[[as.character(o)]];if(anyDuplicated(z$patient_key)||!setequal(z$patient_key,keys[outer!=o])||anyNA(z$inner_fold)||!setequal(z$inner_fold,1:5))stop("内折越界或不完整。")}
    if(anyNA(b$baseline$in_index_icu)||any(!b$baseline$in_index_icu%in%0:1))stop("ICU状态无效。")
    outer
  }
  verify_lineage9<-function(step){
    root<-file.path(cfg$OUTPUT_PARENT,step);sp<-file.path(root,"to_share",paste0(step,"_STATUS.txt"))
    if(!file.exists(sp)||!"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(sp,warn=FALSE))stop("请先成功完成",step,"。")
    lin<-utils::read.csv(file.path(root,"to_share/01_inputs.csv"),stringsAsFactors=FALSE)
    if(!all(c("file","md5")%in%names(lin)))stop("输入审计缺少字段：",step)
    for(i in seq_len(nrow(lin)))if(!file.exists(lin$file[i])||unname(tools::md5sum(lin$file[i]))!=lin$md5[i])stop(step,"上游输入已变化，请先重新完成该步骤。")
  }
  main<-function(){
    capture_previous_warnings()
    if(!requireNamespace("glmnet",quietly=TRUE))stop("缺少glmnet；请先运行 install.packages('glmnet')。")
    for(step in c("Step6","Step7","Step8"))verify_lineage9(step)
    s6<-file.path(cfg$OUTPUT_PARENT,"Step6");s7<-file.path(cfg$OUTPUT_PARENT,"Step7");s2<-file.path(cfg$OUTPUT_PARENT,"Step2")
    paths<-c(file.path(s6,"local_only",c("MIMIC_T6_model_inputs.rds","MIMIC_T24_model_inputs.rds","MIMIC_T48_model_inputs.rds","MIMIC_nested_folds.rds","Step6_score_engine.R","Step6_analysis_contract.rds")),
      file.path(s7,"local_only",c("MIMIC_T48_nested_OOF.rds","MIMIC_T48_frozen_pipeline.rds")),
      file.path(s7,"to_share",c("05_OOF_metrics.csv","05_paired_differences.csv","05_calibration.csv")),
      file.path(s2,"to_share",c("03_frozen_cohort_summary.csv","04_exclusive_flow.csv")))
    if(!all(file.exists(paths)))stop("Step2/6/7的必要输出不完整。")
    h<-unname(tools::md5sum(paths));csv_out(data.frame(file=paths,md5=h),"to_share/01_inputs.csv")
    ee<-new.env(parent=baseenv());sys.source(paths[5],envir=ee);engine<<-ee$step6_score_engine
    contract<-readRDS(paths[6]);if(!identical(contract$version,"Step6_v1")||!identical(contract$main_domains,main_domains))stop("Step6评分约定不一致。")
    final7<-readRDS(paths[8]);for(nm in c("alpha","lambda","min_reference_n","min_domain_fraction","preprocess_min_n","max_missing"))if(!identical(final7$plan[[nm]],plan[[nm]]))stop("与Step7参数约定不一致：",nm)
    logmsg("小型预检：独立早期队列、仅使用预测时点前窗口、训练折评分隔离、未来列不影响预测及警告留存。")
    preflight();txt_out("PASS: T6/T24 input-window restrictions; training-only references; held-out/future invariance; single/batch inference; selected-path convergence; warning capture.","to_share/02_preflight.txt")
    bundles<-lapply(paths[1:3],readRDS);names(bundles)<-expected$landmark;folds<-readRDS(paths[4]);outers<-list()
    cohort<-utils::read.csv(paths[12],stringsAsFactors=FALSE);cohort<-cohort[cohort$source=="MIMIC"&cohort$landmark%in%expected$landmark,,drop=FALSE]
    flow<-utils::read.csv(paths[13],stringsAsFactors=FALSE);flow<-flow[flow$source=="MIMIC"&flow$landmark%in%expected$landmark,,drop=FALSE]
    if(nrow(cohort)!=3L||anyDuplicated(cohort$landmark)||!all(c("base_n","analyzable_n","events","inside_index_ICU_n")%in%names(cohort)))stop("Step2流程摘要不完整。")
    cohort<-cohort[match(expected$landmark,cohort$landmark),,drop=FALSE]
    for(lm in expected$landmark){
      b<-bundles[[lm]];outers[[lm]]<-check_bundle9(b,folds[[paste0("MIMIC_",lm)]],lm)
      ii<-which(cohort$landmark==lm)
      if(cohort$analyzable_n[ii]!=nrow(b$metadata)||cohort$events[ii]!=sum(b$outcomes$event_primary)||
        cohort$inside_index_ICU_n[ii]!=sum(b$baseline$in_index_icu)||sum(flow$n[flow$landmark==lm])!=cohort$base_n[ii]||
        sum(flow$n[flow$landmark==lm&flow$reason=="eligible"])!=nrow(b$metadata))stop("Step2流程与实际输入不一致：",lm)
      fit_state9(b,which(outers[[lm]]!=1L))
      logmsg(paste0(lm,"：",nrow(b$metadata),"人，",sum(b$outcomes$event_primary),"个事件；使用各自冻结队列和分折。"))
    }
    cohort$event_rate<-cohort$events/cohort$analyzable_n
    csv_out(cohort,"to_share/02_landmark_cohorts.csv");csv_out(flow,"to_share/02_exclusive_flow.csv")
    overlap<-do.call(rbind,lapply(list(c("T6","T24"),c("T6","T48"),c("T24","T48")),function(pair){
      a<-bundles[[pair[1]]]$metadata$patient_key;b<-bundles[[pair[2]]]$metadata$patient_key
      data.frame(first=pair[1],second=pair[2],first_n=length(a),second_n=length(b),shared_n=length(intersect(a,b)),first_only_n=length(setdiff(a,b)),second_only_n=length(setdiff(b,a)))
    }));csv_out(overlap,"to_share/02_cohort_overlap.csv")
    txt_out(capture.output(dput(plan)),"to_share/02_analysis_plan.txt")
    csv_out(do.call(rbind,lapply(expected$landmark,function(lm){w<-engine$windows_for(lm,"cumulative");data.frame(landmark=lm,
      model=c("A","C"),raw_dynamic_n=c(0,84*length(w)),domain_scores_n=c(0,4*length(w)),baseline_n=3,
      window=paste(w,collapse=";"),phenotype_used=FALSE)})),"to_share/03_feature_contract.csv")
    old<-readRDS(paths[7]);b48<-bundles$T48
    if(!identical(old$metadata$patient_key,b48$metadata$patient_key)||!identical(as.numeric(old$outcomes$event_primary),as.numeric(b48$outcomes$event_primary))||
      !identical(as.integer(old$outer_fold),as.integer(outers$T48))||!all(c("A","C")%in%colnames(old$prediction)))stop("T48锁定预测与Step6不一致。")
    key<-hash_object(list(inputs=h,plan=plan,functions=lapply(c("fit_state9","design9","fit_path9","run_split9","predict9","path_predict","logloss_sum","select_hyper","score_columns9"),function(z)body(get(z))),
      glmnet=as.character(utils::packageVersion("glmnet")),R=as.character(getRversion())))
    predictions<-list(T48=old$prediction[,c("A","C"),drop=FALSE]);tuning<-list();audits<-list();conv<-list();all_models<-list()
    for(lm in plan$early_landmarks){
      b<-bundles[[lm]];outer<-outers[[lm]];f<-folds[[paste0("MIMIC_",lm)]];keys<-b$metadata$patient_key
      p<-matrix(NA_real_,length(keys),2,dimnames=list(NULL,names(model_names)));models<-list()
      for(o in 1:5){
        current_module<<-paste(lm,"outer",o);tr<-which(outer!=o);va<-which(outer==o)
        zf<-f$inner[[as.character(o)]];inner<-zf$inner_fold[match(keys[tr],zf$patient_key)];loss<-NULL;grid<-NULL
        for(i in 1:5){
          scope<-paste0(lm,"_outer",o,"_inner",i);rel<-paste0("local_only/cache/",scope,".rds");z<-cache_read(rel,key)
          itr<-tr[inner!=i];iva<-tr[inner==i]
          if(is.null(z)){logmsg(paste0(lm," 外折 ",o,"/5，内折 ",i,"/5：拟合基础与连续动态模型。"));z<-run_split9(b,itr,iva);cache_save(z,rel,key)}else logmsg(paste0("缓存通过：",scope,"。"))
          if(!identical(z$assessment_keys,keys[iva]))stop("内折缓存患者不一致。")
          if(!is.null(grid)&&!identical(grid,z$grid))stop("调参网格不一致。")
          grid<-z$grid;loss<-if(is.null(loss))z$loss else Map(`+`,loss,z$loss)
          add_warnings(z$warnings,"split_fit_current_or_cached",scope);audits[[scope]]<-cbind(scope=scope,z$design_audit);conv[[scope]]<-cbind(scope=scope,z$convergence)
        }
        chosen<-lapply(loss,select_hyper,grid=grid)
        for(a in names(model_names))tuning[[paste(lm,o,a)]]<-data.frame(landmark=lm,outer_fold=o,model=a,grid,mean_logloss=loss[[a]]/length(tr),
          converged_all_folds=is.finite(loss[[a]]),selected=seq_len(nrow(grid))==chosen[[a]])
        scope<-paste0(lm,"_outer",o);rel<-paste0("local_only/cache/",scope,".rds");z<-cache_read(rel,key)
        if(is.null(z)){logmsg(paste0(lm," 外折 ",o,"/5：生成独立折外预测。"));z<-run_split9(b,tr,va,chosen);cache_save(z,rel,key)}else logmsg(paste0("缓存通过：",scope,"。"))
        if(!identical(z$assessment_keys,keys[va]))stop("外折缓存患者不一致。")
        add_warnings(z$warnings,"split_fit_current_or_cached",scope);audits[[scope]]<-cbind(scope=scope,z$design_audit);conv[[scope]]<-cbind(scope=scope,z$convergence)
        p[va,]<-z$prediction;obj<-z[c("state","models")]
        rp<-predict9(b,va,obj);one<-predict9(b,va[1],obj)
        if(max(abs(rp-z$prediction))>1e-12||max(abs(one[1,]-rp[1,]))>1e-12)stop("单例/批量或导出预测不一致。")
        models[[paste0("outer",o)]]<-obj
      }
      if(any(!is.finite(p)))stop("早期OOF预测未覆盖全部患者：",lm)
      saveRDS(list(metadata=b$metadata,outcomes=b$outcomes,outer_fold=outer,prediction=p,plan=plan),reg(paste0("local_only/MIMIC_",lm,"_nested_OOF.rds")),compress="gzip")
      predictions[[lm]]<-p;all_models[[lm]]<-models
    }
    saveRDS(list(models=all_models,plan=plan),reg("local_only/MIMIC_early_outer_models.rds"),compress="gzip")
    csv_out(do.call(rbind,tuning),"to_share/04_tuning_logloss.csv");csv_out(do.call(rbind,audits),"to_share/04_design_audit.csv");csv_out(do.call(rbind,conv),"to_share/04_convergence_audit.csv")
    current_module<<-"early OOF evaluation and figures";logmsg("计算T6/T24内部验证及配对增量；T48沿用Step7锁定结果，输出landmark比较图和三线表。")
    results<-list();bins<-list();thresholds<-list();foldmetrics<-list()
    for(lm in expected$landmark){
      y<-bundles[[lm]]$outcomes$event_primary;p<-predictions[[lm]]
      if(lm=="T48"){
        mm<-utils::read.csv(paths[9],stringsAsFactors=FALSE);dd<-utils::read.csv(paths[10],stringsAsFactors=FALSE);cc<-utils::read.csv(paths[11],stringsAsFactors=FALSE)
        ev<-list(metrics=mm[mm$model%in%c("A","C"),],differences=dd[dd$comparison=="C minus A",],calibration=cc[cc$model%in%c("A","C"),])
        if(nrow(ev$metrics)!=8L||nrow(ev$differences)!=4L||nrow(ev$calibration)!=2L)stop("T48锁定指标不完整。")
        for(a in names(model_names)){actual<-metrics(y,p[,a]);z<-ev$metrics[ev$metrics$model==a,];if(max(abs(z$estimate-actual[z$metric]))>1e-12)stop("T48锁定指标与患者预测不一致。")}
      }else ev<-evaluate8(y,p,list(c("C","A")),plan$seed+9000L+as.integer(sub("T","",lm)))
      ev$bins<-calibration_bins(y,p);results[[lm]]<-ev;bins[[lm]]<-cbind(landmark=lm,ev$bins);thresholds[[lm]]<-cbind(landmark=lm,threshold_counts(y,p))
      outer<-outers[[lm]];foldmetrics[[lm]]<-do.call(rbind,lapply(1:5,function(o)do.call(rbind,lapply(names(model_names),function(a)data.frame(landmark=lm,outer_fold=o,model=a,n=sum(outer==o),events=sum(y[outer==o]),t(metrics(y[outer==o],p[outer==o,a])))))))
    }
    for(type in c("metrics","differences","calibration"))csv_out(do.call(rbind,lapply(names(results),function(lm)cbind(landmark=lm,results[[lm]][[type]]))),paste0("to_share/05_landmark_",type,".csv"))
    csv_out(do.call(rbind,bins),"to_share/06_calibration_bins.csv");csv_out(do.call(rbind,thresholds),"to_share/06_threshold_counts.csv");csv_out(do.call(rbind,foldmetrics),"to_share/05_metrics_by_outer_fold.csv")
    make_visuals9(cohort,flow,results)
    txt_out(c(
      "Step9：T6/T24独立landmark队列的基础模型A与连续生理模型C；T48沿用Step7锁定A/C预测及其指标。",
      "T6为5513人/825事件，T24为5411人/738事件，T48为5133人/630事件；以输入与Step2冻结摘要双重核对。",
      "各时点纳入当时存活且仍在院、可判定其后30天结局的患者。早期队列不是从T48生存者中截取。",
      "队列重叠单独报告；死亡日期精度和结局边界可能影响资格，不强行要求不同landmark队列完全嵌套。",
      "A：年龄、性别和该时点是否仍在index ICU。C：A加21个核心变量的均值、SD、斜率和异常比例及四域评分。",
      "T6仅0-6小时（84项原始动态+4域分）；T24使用0-6和0-24小时（168+8）；T48使用0-6/0-24/0-48（252+12）。",
      "按Step6原冻结患者分折，外5内5；每个内训练折重新拟合0-6小时评分参考、缺失填补、变量保留及标准化。",
      "同一时点A/C共用患者、分折、弹性网网格和调参准则；按合并内折log loss最小选参。",
      "不使用T48分型或未来窗口，不在本步重新分型；没有测量次数作为直接输入，但保留训练中缺失特征的缺失指示。",
      "未收敛路径候选用NA系数/Inf损失排除；全部内折可用才可入选；外训练选中参数不能有效预测则失败。",
      "每条路径保存jerr及返回lambda数；完整警告保留local_only，反馈仅含类别与数量。缓存同时保留其拟合警告。",
      "启动时捕获仍可访问的上一轮R warnings；只有期间无其他警告调用时才可能对应Step8。重启R后无法恢复旧警告。",
      "T6/T24为2000次固定OOF患者bootstrap；配对C-A使用同一重抽样。T48完全沿用Step7的点估计与区间。",
      "不做跨landmark配对检验：人群、信息长度及30天预测区间不同，AUROC高低不能解释为等待更久的临床获益。",
      "Step2的预测前死亡数是互斥流程中death_at_or_before_landmark排除项；日期重叠、冲突和无效时点分别保留。",
      "校准展示及截距/斜率采用同一折外预测，不重新校准；5%/15%/30%仅固定展示阈值，不代表治疗指征。",
      "新增早期landmark比较补充图、校准图及三线表。后续整稿时确定S编号；不直接替换既有T48主结果图。",
      "本步不扫描原始大表、不使用eICU训练或评价；不重跑Step7/8，不生成早期模型的全样本部署对象。",
      "结果无时间戳；有效缓存可续跑，同名登记输出覆盖，用户另存文件保留。患者级结果与模型仅留local_only。"),"to_share/00_READ_ME_FIRST.txt")
    txt_out("https://glmnet.stanford.edu/articles/glmnet.html","to_share/10_method_sources.txt")
    status<<-"COMPLETED_FOR_REVIEW"
  }
  synthetic_bundle<-function(n=160L,lm="T6"){
    with_seed(71L,{
      sp<-engine$specification();core<-unique(sp$variable[sp$tier=="main"]);windows<-engine$windows_for(lm,"cumulative");fp<-list()
      for(w in windows)for(v in core)for(st in c("mean_value","sd_value","slope_per_hour","abnormal_fraction"))fp[[length(fp)+1L]]<-data.frame(feature=engine$key(v,w,st),role="main_continuous",tier="main",window=w)
      fp<-do.call(rbind,fp);cols<-unique(c(fp$feature,unlist(lapply(windows,function(w)engine$key(sp$variable,w,sp$stat)))))
      latent<-rep(c(-2,2),length.out=n)+stats::rnorm(n,0,.25);raw<-as.data.frame(matrix(0,n,length(cols)));names(raw)<-cols
      for(nm in cols){v<-latent+stats::rnorm(n,0,.4);if(grepl("__min_value$",nm))v<--v;if(grepl("__abnormal_fraction$",nm))v<-stats::plogis(v);raw[[nm]]<-as.numeric(v)}
      raw[seq(7,n,17),seq(1,ncol(raw),9)]<-NA_real_
      base<-data.frame(age=stats::runif(n,40,85),sex_male=as.numeric(seq_len(n)%%2),in_index_icu=as.numeric(seq_len(n)%%3!=0))
      y<-stats::rbinom(n,1,stats::plogis(-.7+.5*latent))
      list(version="Step6_v1",metadata=data.frame(patient_key=sprintf("fixture:%04d",1:n),source="MIMIC",landmark=lm),raw_numeric=raw,
        baseline=base,feature_plan=fp,outcomes=data.frame(event_primary=y,endpoint="30_day_all_cause_death_after_landmark"))
    })
  }
  preflight<-function(){
    oldplan<-plan;on.exit(plan<<-oldplan,add=TRUE)
    plan<<-utils::modifyList(plan,list(min_reference_n=8L,preprocess_min_n=8L,alpha=.5,lambda=c(1,.3,.1),bootstrap_repeats=10L))
    for(lm in c("T6","T24")){
      b<-synthetic_bundle(160L,lm);tr<-1:120;va<-121:160;st<-fit_state9(b,tr)
      changed<-b;changed$raw_numeric[va,]<-999;changed$outcomes$event_primary<-1-b$outcomes$event_primary
      stopifnot(identical(st,fit_state9(changed,tr)))
      future<-b;future$raw_numeric[[engine$key("heart_rate","0-48h","mean_value")]]<-99999
      stopifnot(identical(st,fit_state9(future,tr)),identical(design9(b,va,st),design9(future,va,st)))
      bad<-future;bad$feature_plan<-rbind(bad$feature_plan,data.frame(feature=engine$key("heart_rate","0-48h","mean_value"),role="main_continuous",tier="main",window="0-48h"))
      stopifnot(inherits(try(fit_state9(bad,tr),silent=TRUE),"try-error"))
      z<-run_split9(b,tr,va,list(A=3L,C=3L));p<-predict9(b,va,z);one<-predict9(b,va[2],z)
      stopifnot(max(abs(p-z$prediction))<1e-12,max(abs(one[1,]-p[2,]))<1e-12,
        length(st$dynamic_columns)==if(lm=="T6")84L else 168L,
        ncol(design9(b,va,st)$C)==if(lm=="T6")91L else 179L)
      add_warnings(z$warnings,"preflight","synthetic_checks")
    }
    caught<-character();withCallingHandlers(warning("fixture warning for capture",call.=FALSE),warning=function(w){caught<<-c(caught,conditionMessage(w));invokeRestart("muffleWarning")})
    stopifnot(length(caught)==1L,warning_category("Convergence for 31th lambda value not reached after maxit=100000 iterations; solutions for larger lambdas returned")=="glmnet path convergence warning",
      abs(metrics(c(1,0,1),c(.5,.5,.2))["AUPRC_AP"]-7/12)<1e-12)
    TRUE
  }
  make_visuals9<-function(cohort,flow,results){
    lms<-expected$landmark;colors<-c(A="#7A8797",C="#D98C57");hours<-as.integer(sub("T","",lms))
    get_rows<-function(type,metric,model=NULL){do.call(rbind,lapply(lms,function(lm){z<-results[[lm]][[type]];z<-z[z$metric==metric,,drop=FALSE];if(!is.null(model))z<-z[z$model==model,,drop=FALSE];cbind(landmark=lm,z)}))}
    save_fig<-function(stem,draw,w,h){open_pdf(reg(paste0("to_share/",stem,".pdf")),w,h);tryCatch(draw(),finally=grDevices::dev.off())
      if(cfg$MAKE_PNG){grDevices::png(reg(paste0("to_share/",stem,".png")),width=w,height=h,units="in",res=400);tryCatch(draw(),finally=grDevices::dev.off())}}
    draw<-function(){
      op<-par(no.readonly=TRUE);on.exit(par(op));par(mfrow=c(2,2),mar=c(4,4.5,2.8,1),oma=c(2.2,0,.5,0),family="sans",las=1,cex=.85)
      mat<-rbind(cohort$analyzable_n,cohort$events);colnames(mat)<-lms
      bp<-barplot(mat,beside=TRUE,col=c("#4F7FC4","#D98C57"),border=NA,ylim=c(0,max(mat)*1.2),ylab="Patients",xlab="Prediction landmark",main="A  Separate eligible populations",adj=0)
      text(bp,mat,labels=mat,pos=3,cex=.75);legend("topright",legend=c("Eligible","30-day events"),fill=c("#4F7FC4","#D98C57"),border=NA,bty="n",cex=.8)
      z<-get_rows("metrics","AUROC");lo<-max(0,min(.5,z$lower,na.rm=TRUE)-.04)
      plot(0,0,type="n",xlim=c(.5,3.5),ylim=c(lo,1),xaxt="n",xlab="Prediction landmark",ylab="AUROC (95% CI)",main="B  Discrimination",adj=0,bty="l")
      axis(1,1:3,lms);abline(h=.5,lty=2,col="#BBBBBB")
      for(a in names(colors)){v<-z[z$model==a,];xx<-seq_along(lms)+if(a=="A")-.08 else .08
        segments(xx,v$lower,xx,v$upper,col=colors[a],lwd=2);points(xx,v$estimate,pch=16,col=colors[a])}
      legend("bottomright",legend=c("A: baseline","C: baseline + physiology"),col=colors,lwd=2,pch=16,bty="n",cex=.8)
      z<-get_rows("metrics","AUPRC_AP");yl<-range(c(0,cohort$event_rate,z$lower,z$upper),na.rm=TRUE);yl[2]<-min(1,yl[2]+.10)
      plot(0,0,type="n",xlim=c(.5,3.5),ylim=yl,xaxt="n",xlab="Prediction landmark",ylab="Average precision (95% CI)",main="C  Precision-recall performance",adj=0,bty="l");axis(1,1:3,lms)
      for(a in names(colors)){v<-z[z$model==a,];xx<-1:3+if(a=="A")-.08 else .08;segments(xx,v$lower,xx,v$upper,col=colors[a],lwd=2);points(xx,v$estimate,pch=16,col=colors[a])}
      points(1:3,cohort$event_rate,pch=4,col="#619E91",lwd=2);legend("topleft",legend="Event-rate reference",pch=4,col="#619E91",bty="n",cex=.75)
      z<-get_rows("differences","AUROC");yr<-range(c(0,z$lower,z$upper),na.rm=TRUE);pad<-max(.01,diff(yr)*.12)
      plot(0,0,type="n",xlim=c(.5,3.5),ylim=yr+c(-pad,pad),xaxt="n",xlab="Prediction landmark",ylab="Paired AUROC difference (C - A)",main="D  Within-landmark increment",adj=0,bty="l");axis(1,1:3,lms);abline(h=0,lty=2,col="#BBBBBB")
      segments(1:3,z$lower,1:3,z$upper,lwd=2,col="#4F7FC4");points(1:3,z$estimate,pch=16,col="#4F7FC4")
      mtext("Each model predicts 30 days after its own landmark. Populations and prediction horizons differ; no paired comparison across landmarks.",side=1,outer=TRUE,line=.4,cex=.68)
      mtext("A: age, sex, ICU state. C: A + available cumulative-window physiology. T48 results are retained from Step7.",side=1,outer=TRUE,line=1.2,cex=.68)
    }
    save_fig("Figure_S_early_landmark_validation",draw,10.5,8.3)
    draw_cal<-function(){
      op<-par(no.readonly=TRUE);on.exit(par(op));par(mfrow=c(1,3),mar=c(4,4.2,2.5,.8),oma=c(1.7,0,.4,0),family="sans",las=1,cex=.8)
      for(i in seq_along(lms)){lm<-lms[i];plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),xlab="Mean predicted probability",ylab="Observed event proportion",main=paste0(LETTERS[i],"  ",lm," calibration"),adj=0,bty="l");abline(0,1,lty=2,col="#BBBBBB")
        for(a in names(colors)){z<-results[[lm]]$bins;z<-z[z$model==a,];lines(z$predicted,z$observed,type="b",pch=16,col=colors[a],lwd=1.5,cex=.7)}
        legend("topleft",legend=c("A: baseline","C: baseline + physiology"),col=colors,lwd=2,bty="n",cex=.65)
      };mtext("Grouped nested out-of-fold probabilities; no recalibration on the evaluation data.",side=1,outer=TRUE,line=.4,cex=.75)
    };save_fig("Figure_S_early_landmark_calibration",draw_cal,12,4.3)
    fmt<-function(z)sprintf("%.3f (%.3f, %.3f)",z$estimate,z$lower,z$upper)
    perf<-data.frame(Analysis=as.vector(t(outer(lms,c("A","C"),paste))));diff<-data.frame(Landmark=lms)
    mm<-do.call(rbind,lapply(lms,function(lm)cbind(landmark=lm,results[[lm]]$metrics)));mm$label<-paste(mm$landmark,mm$model)
    for(m in c("AUROC","AUPRC_AP","Brier","LogLoss")){
      z<-mm[mm$metric==m,];perf[[m]]<-fmt(z[match(perf$Analysis,z$label),])
      z<-get_rows("differences",m);diff[[m]]<-fmt(z[match(lms,z$landmark),])
    }
    cc<-data.frame(Landmark=lms,Eligible=cohort$analyzable_n,Events=cohort$events,`Event rate`=sprintf("%.2f%%",100*cohort$event_rate),`In index ICU`=cohort$inside_index_ICU_n,check.names=FALSE)
    csv_out(perf,"to_share/Table_S_early_landmark_performance.csv");csv_out(diff,"to_share/Table_S_early_landmark_increment.csv");csv_out(cc,"to_share/Table_S_early_landmark_cohorts.csv")
    notes<-c("A: age, sex, ICU state. C: A plus cumulative-window physiology and four-domain scores available at each landmark.",
      "AP: average precision. C-A differences are paired within the same landmark, not between different landmarks.",
      "95% intervals use fixed-OOF patient bootstrap; T48 estimates and intervals are retained from Step7.")
    three_line(list("Separate landmark populations"=cc,"Model performance and 95% CI"=perf,"Within-landmark paired C-A differences"=diff),
      "Supplementary table. Early landmark internal validation","Table_S_early_landmark_validation",notes)
    csv_out(data.frame(output=c("Figure_S_early_landmark_validation","Figure_S_early_landmark_calibration","Table_S_early_landmark_validation"),
      action="ADD supplementary; final S number assigned during manuscript integration",
      relation="New independent T6/T24 landmark evidence with locked T48 reference; no direct replacement of existing T48 main-figure panels"),"to_share/11_figure_table_replacement_map.csv")
  }
  if(isTRUE(getOption("ami_step9.functions_only",FALSE)))return(invisible(list(preflight=preflight,synthetic_bundle=synthetic_bundle,
    fit_state9=fit_state9,design9=design9,run_split9=run_split9,predict9=predict9,metrics=metrics,evaluate8=evaluate8,
    make_visuals9=make_visuals9,main=main,cache_read=cache_read,cache_save=cache_save,warning_category=warning_category)))
  status<-"FAILED";current_module<-"input verification"
  failure_message<-NULL
  tryCatch(withCallingHandlers(main(),warning=function(w){add_warnings(conditionMessage(w),"current_run",current_module);invokeRestart("muffleWarning")}),error=function(err){status<<-"FAILED";failure_message<<-conditionMessage(err)
    issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step9_error_detail.txt")})
  flush_warnings()
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step9",
    "COMPLETED_FOR_REVIEW表示T6/T24嵌套预测及T6/T24/T48比较图表已生成，等待结果核对；外部验证尚未开展。",
    "FAILED表示不完整，不能使用本次患者级结果建模。"),"to_share/Step9_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step9_sessionInfo.txt")
  logmsg(paste0("Step9状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step9_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share) else {
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step9_feedback.zip"),"\n",sep="") else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
