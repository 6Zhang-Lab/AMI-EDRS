# AMI-EDRS revision | Step8: T48 clinical and observation-process comparators
# source(file.choose(), encoding="UTF-8")
# Reuses Step6 inputs and Step7 OOF predictions. No raw-database scan or eICU modeling.
# Fixed filenames; resumable fold caches; only aggregate feedback leaves local_only.
local({
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MAKE_PNG=TRUE,MAKE_ZIP=TRUE,RESUME=TRUE)
  cfg<-utils::modifyList(cfg,getOption("ami_step8.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step8")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step8输出目录。")
  if (is_link(out)) stop("Step8输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP8")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step8目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP8_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP8_MANAGED_OUTPUT")) {
    stop("Step8输出目录的归属标记不正确。")
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
        if (unlink(p) != 0L) stop("不能覆盖旧Step8输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step8_run_log.txt")
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
  model_names<-c(I="ICU status",S="Simple clinical",C="Core dynamics",SD="Clinical + dynamics",
    O="Observation process",P="No ICU / NA flags",CT="Extended clinical")
  fit_names<-c("S","SD","O","P","CT")
  treatment_vars<-c("pci","cabg","iabp","impella","ecmo","prior_cad")
  plan<-list(version="Step8_v1",landmark="T48",outer=5L,inner=5L,seed=2026L,
    alpha=c(.1,.5,1),lambda=10^seq(1,-4,length.out=31),criterion="pooled inner-assessment log loss",
    min_reference_n=20L,min_domain_fraction=.5,preprocess_min_n=20L,max_missing=.95,
    bootstrap_repeats=2000L,descriptive_thresholds=c(.05,.15,.30),
    last_value_window="0-48h: latest eligible observation; not necessarily current at T48",
    support_window="Any positive-rate vasoactive documentation before T48; not necessarily active at T48")
  plan<-utils::modifyList(plan,getOption("ami_step8.plan",list()))
  score_columns<-function()unlist(lapply(windows,function(w)paste(main_domains,gsub("-","_",w,fixed=TRUE),"score",sep="__")),use.names=FALSE)
  with_seed<-function(seed,expr){
    old<-if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))get(".Random.seed",.GlobalEnv)else NULL
    on.exit(if(is.null(old)){if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv)}else assign(".Random.seed",old,.GlobalEnv))
    set.seed(seed);force(expr)
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
  fit_state8<-function(b,tr){
    dynamic<-unique(b$feature_plan$feature[b$feature_plan$role=="main_continuous"])
    if(length(dynamic)!=252L||!all(dynamic%in%names(b$raw_numeric)))stop("Step6连续特征定义不一致。")
    obs<-unique(b$feature_plan$feature[b$feature_plan$role=="observation_process"&b$feature_plan$tier=="main"&b$feature_plan$window%in%windows])
    if(length(obs)!=126L||!all(obs%in%names(b$raw_numeric))||any(!grepl("__(n_chart_times|missing_flag)$",obs)))stop("观察过程特征定义不一致。")
    list(reference=engine$fit_reference(b$raw_numeric,tr,min_observed=plan$min_reference_n),
      dynamic_columns=dynamic,observation_columns=obs,score_columns=score_columns())
  }
  design8<-function(b,rows,state){
    raw<-b$raw_numeric[rows,,drop=FALSE];base<-b$baseline[rows,c("age","sex_male","in_index_icu"),drop=FALSE]
    cx<-b$clinical_documentation[rows,,drop=FALSE]
    sc<-engine$apply_reference(raw,state$reference,windows,min_fraction=plan$min_domain_fraction)$scores[state$score_columns]
    dyn<-cbind(raw[state$dynamic_columns],sc)
    simple<-cbind(base,cx[c("vasoactive__documented_positive","vasoactive__patient_source_covered")],
      raw[c(engine$key("lactate","0-48h","last_value"),engine$key("gcs_total","0-48h","last_value"))])
    # Counts and missingness only. No observed physiological values, age, sex, ICU state or treatment values enter O.
    obs<-raw[state$observation_columns]
    flags<-as.data.frame(lapply(dyn,function(v)as.numeric(is.na(v))),check.names=FALSE)
    names(flags)<-paste0("missing__",names(dyn));obs<-cbind(obs,flags)
    clinic_cols<-as.vector(outer(treatment_vars,c("documented_positive","patient_source_covered"),paste,sep="__"))
    ans<-list(S=simple,SD=cbind(simple,dyn),O=obs,P=cbind(base[c("age","sex_male")],dyn),CT=cbind(simple,dyn,cx[clinic_cols]))
    for(nm in names(ans)){rownames(ans[[nm]])<-NULL;if(anyDuplicated(names(ans[[nm]])))stop("重复设计列：",nm)}
    ans
  }
  fit_arm8<-function(train,test,y,arm){
    pp<-engine$fit_preprocessor(train,seq_len(nrow(train)),max_missing=plan$max_missing,
      min_observed=plan$preprocess_min_n,add_missing=arm!="P")
    if(arm=="P"&&length(pp$indicators))stop("P模型不得加入缺失指示。")
    x<-engine$apply_preprocessor(train,pp);xx<-engine$apply_preprocessor(test,pp)
    path<-fit_path(x,y)
    list(preprocessor=pp,path=path,predictions=path_predict(path,xx))
  }
  run_split8<-function(b,tr,va,chosen=NULL){
    state<-fit_state8(b,tr);train<-design8(b,tr,state);test<-design8(b,va,state)
    loss<-list();pred<-list();models<-list();audit<-list();grid<-NULL
    for(a in fit_names){
      fit<-fit_arm8(train[[a]],test[[a]],b$outcomes$event_primary[tr],a)
      if(!is.null(grid)&&!identical(grid,fit$path$grid))stop("调参网格不一致。")
      grid<-fit$path$grid;loss[[a]]<-logloss_sum(b$outcomes$event_primary[va],fit$predictions)
      audit[[a]]<-data.frame(model=a,requested=ncol(train[[a]]),retained=length(fit$preprocessor$features),
        added_missing=length(fit$preprocessor$indicators),training_n=length(tr),assessment_n=length(va))
      if(!is.null(chosen)){
        j<-chosen[[a]];if(any(!is.finite(fit$predictions[,j])))stop("选中参数在外训练折未收敛：",a)
        pred[[a]]<-fit$predictions[,j];models[[a]]<-list(preprocessor=fit$preprocessor,
          coefficients=fit$path$beta[,j],alpha=grid$alpha[j],lambda=grid$lambda[j])
      }
    }
    out<-list(grid=grid,loss=loss,assessment_keys=b$metadata$patient_key[va],audit=do.call(rbind,audit))
    if(!is.null(chosen)){
      icu<-b$baseline$in_index_icu;rates<-vapply(0:1,function(g){ii<-tr[icu[tr]==g];if(!length(ii))stop("ICU单变量训练组为空。")
        mean(b$outcomes$event_primary[ii])},numeric(1))
      # Two-bin empirical risks are the binomial maximum-likelihood fit of an intercept + binary ICU predictor.
      out$prediction<-cbind(I=rates[icu[va]+1L],do.call(cbind,pred));colnames(out$prediction)<-c("I",fit_names)
      out$state<-state;out$models<-models;out$icu_rates<-rates
    };out
  }
  predict8<-function(b,rows,object){
    d<-design8(b,rows,object$state);ans<-matrix(NA_real_,length(rows),length(fit_names)+1L,dimnames=list(NULL,c("I",fit_names)))
    ans[,"I"]<-object$icu_rates[b$baseline$in_index_icu[rows]+1L]
    for(a in fit_names){m<-object$models[[a]];x<-engine$apply_preprocessor(d[[a]],m$preprocessor)
      if(!identical(names(m$coefficients),c("(Intercept)",colnames(x))))stop("预测列顺序不一致。")
      ans[,a]<-as.numeric(plogis(cbind(1,x)%*%m$coefficients))
    };if(any(!is.finite(ans)))stop("预测未成功。")
    ans
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
  report_eval<-function(y,p,scope,pairs,seed){
    ev<-evaluate8(y,p,pairs,seed)
    csv_out(ev$metrics,paste0("to_share/05_",scope,"_metrics.csv"));csv_out(ev$calibration,paste0("to_share/05_",scope,"_calibration.csv"))
    if(nrow(ev$differences))csv_out(ev$differences,paste0("to_share/05_",scope,"_paired_differences.csv"))
    ev$bins<-calibration_bins(y,p);csv_out(ev$bins,paste0("to_share/06_",scope,"_calibration_bins.csv"))
    csv_out(threshold_counts(y,p),paste0("to_share/06_",scope,"_threshold_counts.csv"));ev
  }
  input_check8<-function(b){
    n<-nrow(b$metadata)
    if(any(vapply(list(b$raw_numeric,b$baseline,b$clinical_documentation,b$outcomes),nrow,integer(1))!=n))stop("Step6各输入行数不一致。")
    if(anyNA(b$baseline$in_index_icu)||any(!b$baseline$in_index_icu%in%0:1))stop("ICU状态无效。")
    vars<-c("vasoactive",treatment_vars)
    for(v in vars){
      pos<-b$clinical_documentation[[paste0(v,"__documented_positive")]];cov<-b$clinical_documentation[[paste0(v,"__patient_source_covered")]]
      if(length(pos)!=n||length(cov)!=n||anyNA(cov)||any(!cov%in%0:1)||any(!is.na(pos)&!pos%in%0:1)||
        any(cov==0&!is.na(pos))||any(cov==1&is.na(pos)))stop("临床来源/阳性编码不一致：",v)
    }
    req<-c(engine$key("lactate","0-48h","last_value"),engine$key("gcs_total","0-48h","last_value"))
    if(!all(req%in%names(b$raw_numeric)))stop("缺少最近乳酸/GCS输入。")
    if(any(vapply(b$raw_numeric,function(x)!is.numeric(x)||any(is.infinite(x)|is.nan(x)),logical(1))))stop("数值输入类型或非有限值异常。")
    TRUE
  }
  availability8<-function(b){
    rows<-list()
    for(g in c("All","In ICU","Out of index ICU")){
      ii<-if(g=="All")seq_len(nrow(b$metadata))else which(b$baseline$in_index_icu==as.integer(g=="In ICU"))
      for(v in c("vasoactive",treatment_vars)){
        z<-b$clinical_documentation[[paste0(v,"__documented_positive")]][ii]
        rows[[length(rows)+1L]]<-data.frame(group=g,variable=v,n=length(ii),available_n=sum(!is.na(z)),positive_n=sum(z==1,na.rm=TRUE),
          no_positive_documentation_n=sum(z==0,na.rm=TRUE),unavailable_n=sum(is.na(z)),meaning="Documentation, not verified treatment/history absence")
      }
      for(v in c("lactate","gcs_total")){
        z<-b$raw_numeric[[engine$key(v,"0-48h","last_value")]][ii]
        rows[[length(rows)+1L]]<-data.frame(group=g,variable=paste0(v,"_last_0_48h"),n=length(ii),available_n=sum(!is.na(z)),positive_n=NA_integer_,
          no_positive_documentation_n=NA_integer_,unavailable_n=sum(is.na(z)),meaning="Latest eligible record in 0-48h; age of observation not assessed in this step")
      }
    };do.call(rbind,rows)
  }
  main<-function(){
    if(!requireNamespace("glmnet",quietly=TRUE))stop("缺少glmnet，请先运行 install.packages('glmnet')。")
    step6<-file.path(cfg$OUTPUT_PARENT,"Step6");step7<-file.path(cfg$OUTPUT_PARENT,"Step7")
    for(step in c("Step6","Step7")){
      sp<-file.path(cfg$OUTPUT_PARENT,step,"to_share",paste0(step,"_STATUS.txt"))
      if(!file.exists(sp)||!"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(sp,warn=FALSE))stop("请先成功完成",step,"。")
      lp<-file.path(cfg$OUTPUT_PARENT,step,"to_share/01_inputs.csv")
      if(!file.exists(lp))stop("缺少上游输入审计：",step)
      lin<-utils::read.csv(lp,stringsAsFactors=FALSE)
      if(!all(c("file","md5")%in%names(lin)))stop("上游输入审计缺少字段。")
      for(i in seq_len(nrow(lin)))if(!file.exists(lin$file[i])||unname(tools::md5sum(lin$file[i]))!=lin$md5[i])stop(step,"输入已改变，请先重跑该步骤。")
    }
    paths<-c(file.path(step6,"local_only",c("MIMIC_T48_model_inputs.rds","MIMIC_nested_folds.rds","Step6_score_engine.R","Step6_analysis_contract.rds")),
      file.path(step7,"local_only",c("MIMIC_T48_nested_OOF.rds","MIMIC_T48_outer_models.rds","MIMIC_T48_frozen_pipeline.rds","Step7_score_engine.R","Step7_predict.R")))
    if(!all(file.exists(paths)))stop("Step6/Step7本地输入不完整。")
    h<-unname(tools::md5sum(paths));csv_out(data.frame(file=paths,md5=h),"to_share/01_inputs.csv")
    ee<-new.env(parent=baseenv());sys.source(paths[3],envir=ee);engine<<-ee$step6_score_engine
    contract<-readRDS(paths[4]);if(!identical(contract$version,"Step6_v1")||!identical(contract$main_domains,main_domains))stop("评分约定不一致。")
    frozen7<-readRDS(paths[7]);if(!identical(frozen7$plan$alpha,plan$alpha)||!identical(frozen7$plan$lambda,plan$lambda))stop("Step8与Step7调参网格不一致。")
    for(nm in c("min_reference_n","min_domain_fraction","preprocess_min_n","max_missing"))if(!identical(frozen7$plan[[nm]],plan[[nm]]))stop("Step8与Step7预处理参数不一致：",nm)
    logmsg("小型预检：仅测量过程输入、临床来源缺失语义、训练折隔离、单例/批量预测及配对比较。")
    preflight();txt_out("PASS: training-only fitting; observation-only values; no missing indicators in P; clinical source semantics; single/batch inference; tied AP and bootstrap inputs.","to_share/02_preflight.txt")
    b<-readRDS(paths[1]);input_check8(b);f<-readRDS(paths[2])[["MIMIC_T48"]];outer<-check_bundle_folds(b,f)
    old<-readRDS(paths[5]);outer7<-readRDS(paths[6]);keys<-b$metadata$patient_key;y<-b$outcomes$event_primary;n<-length(y)
    if(!identical(old$metadata$patient_key,keys)||!identical(as.numeric(old$outcomes$event_primary),as.numeric(y))||
      !identical(as.integer(old$outer_fold),as.integer(outer))||!identical(colnames(old$prediction),c("A","B","C","D"))||any(!is.finite(old$prediction)))stop("Step7患者、结局、分折或预测不一致。")
    ep<-new.env(parent=globalenv());sys.source(paths[8],envir=ep);sys.source(paths[9],envir=ep)
    roundtrip<-list()
    for(o in 1:5){
      va<-which(outer==o);tr<-which(outer!=o);obj<-outer7[[paste0("outer",o)]]
      z<-ep$predict_step7(b$raw_numeric[va,,drop=FALSE],b$baseline[va,,drop=FALSE],obj)$probability
      if(!identical(dim(z),dim(old$prediction[va,,drop=FALSE]))||max(abs(z-old$prediction[va,,drop=FALSE]))>1e-10)stop("Step7模型导出与OOF预测不一致。")
      st<-fit_state8(b,tr);if(!identical(st$reference,obj$state$reference)||!identical(st$dynamic_columns,obj$state$dynamic_columns)||!identical(st$score_columns,obj$state$score_columns))stop("连续特征/评分参考与Step7不一致。")
      cc<-obj$models$D$coefficients;sel<-grepl("^phenotype_C[0-9]+$",names(cc))
      roundtrip[[o]]<-data.frame(outer_fold=o,n=length(va),max_export_error=max(abs(z-old$prediction[va,,drop=FALSE])),
        phenotype_terms=sum(sel),nonzero_phenotype_terms=sum(abs(cc[sel])>1e-12),max_abs_D_minus_C=max(abs(z[,"D"]-z[,"C"])),
        C_alpha=obj$models$C$alpha,D_alpha=obj$models$D$alpha,C_lambda=obj$models$C$lambda,D_lambda=obj$models$D$lambda)
    }
    csv_out(do.call(rbind,roundtrip),"to_share/02_Step7_prediction_roundtrip.csv")
    csv_out(availability8(b),"to_share/03_clinical_availability.csv")
    csv_out(data.frame(group=c("All","In ICU","Out of index ICU"),n=c(n,sum(b$baseline$in_index_icu==1),sum(b$baseline$in_index_icu==0)),
      events=c(sum(y),sum(y[b$baseline$in_index_icu==1]),sum(y[b$baseline$in_index_icu==0]))),"to_share/02_cohort_check.csv")
    txt_out(capture.output(dput(plan)),"to_share/02_frozen_analysis_plan.txt")
    csv_out(data.frame(model=names(model_names),name=unname(model_names),definition=c(
      "Outer-training event rate in each binary ICU-state group; no hyperparameters",
      "Age, sex, ICU state, pre-T48 vasoactive documentation/source, latest lactate and GCS in 0-48h",
      "Locked Step7 C nested OOF predictions",
      "S plus the same 252 dynamic summaries and 12 four-domain scores as C",
      "Core-variable cumulative-window counts and missing flags; missingness of all 252 raw dynamics and 12 fitted scores; no physiological values",
      "Age, sex, 252 raw dynamic summaries and 12 domain scores; no ICU state, treatment/count variables or added missing indicators",
      "SD plus PCI, CABG, IABP, Impella, ECMO and prior-CAD documentation/source indicators")),"to_share/02_model_definitions.csv")
    funcs<-c("fit_state8","design8","fit_arm8","run_split8","predict8","fit_path","path_predict","logloss_sum","select_hyper","score_columns")
    key<-hash_object(list(inputs=h,plan=plan,fit_names=fit_names,treatment_vars=treatment_vars,
      functions=lapply(funcs,function(z)body(get(z))),glmnet=as.character(utils::packageVersion("glmnet")),R=as.character(getRversion())))
    p<-matrix(NA_real_,n,length(model_names),dimnames=list(NULL,names(model_names)));p[,"C"]<-old$prediction[,"C"]
    tuning<-list();audits<-list();models<-list()
    for(o in 1:5){
      current_module<<-paste0("T48 outer fold ",o);tr<-which(outer!=o);va<-which(outer==o)
      zf<-f$inner[[as.character(o)]];inner<-zf$inner_fold[match(keys[tr],zf$patient_key)];loss<-NULL;grid<-NULL
      for(i in 1:5){
        scope<-paste0("outer",o,"_inner",i);rel<-paste0("local_only/cache/T48_",scope,".rds");z<-cache_read(rel,key)
        itr<-tr[inner!=i];iva<-tr[inner==i]
        if(is.null(z)){logmsg(paste0("T48 外折 ",o,"/5，内折 ",i,"/5：拟合临床、观察过程及敏感性模型。"));z<-run_split8(b,itr,iva);cache_save(z,rel,key)}else logmsg(paste0("缓存通过：",scope,"。"))
        if(!identical(z$assessment_keys,keys[iva]))stop("内折缓存患者顺序不一致。")
        if(!is.null(grid)&&!identical(grid,z$grid))stop("缓存网格不一致。")
        grid<-z$grid;loss<-if(is.null(loss))z$loss else Map(`+`,loss,z$loss);audits[[scope]]<-cbind(scope=scope,z$audit)
      }
      chosen<-lapply(loss,select_hyper,grid=grid)
      for(a in fit_names)tuning[[paste0(o,a)]]<-data.frame(outer_fold=o,model=a,grid,mean_logloss=loss[[a]]/length(tr),converged_all_folds=is.finite(loss[[a]]),selected=seq_len(nrow(grid))==chosen[[a]])
      scope<-paste0("outer",o);rel<-paste0("local_only/cache/T48_",scope,".rds");z<-cache_read(rel,key)
      if(is.null(z)){logmsg(paste0("T48 外折 ",o,"/5：生成独立折外预测。"));z<-run_split8(b,tr,va,chosen);cache_save(z,rel,key)}else logmsg(paste0("缓存通过：",scope,"。"))
      if(!identical(z$assessment_keys,keys[va]))stop("外折缓存患者顺序不一致。")
      p[va,colnames(z$prediction)]<-z$prediction;audits[[scope]]<-cbind(scope=scope,z$audit)
      obj<-z[c("state","models","icu_rates")];rp<-predict8(b,va,obj);single<-predict8(b,va[1],obj)
      if(max(abs(rp-z$prediction))>1e-12||max(abs(single[1,]-rp[1,]))>1e-12)stop("Step8模型单例/批量预测不一致。")
      models[[scope]]<-obj
    }
    if(any(!is.finite(p)))stop("患者折外预测未覆盖完整。")
    saveRDS(list(metadata=b$metadata,outcomes=b$outcomes,outer_fold=outer,ICU_state=b$baseline$in_index_icu,prediction=p,
      plan=plan,model_names=model_names),reg("local_only/MIMIC_T48_comparator_OOF.rds"),compress="gzip")
    saveRDS(list(models=models,plan=plan),reg("local_only/MIMIC_T48_comparator_outer_models.rds"),compress="gzip")
    csv_out(do.call(rbind,tuning),"to_share/04_tuning_logloss.csv");csv_out(do.call(rbind,audits),"to_share/04_design_audit.csv")
    csv_out(do.call(rbind,lapply(1:5,function(o)do.call(rbind,lapply(colnames(p),function(a)data.frame(outer_fold=o,model=a,n=sum(outer==o),events=sum(y[outer==o]),t(metrics(y[outer==o],p[outer==o,a]))))))),"to_share/05_metrics_by_outer_fold.csv")
    current_module<<-"paired OOF evaluation and figures"
    logmsg("计算总体及ICU状态分层指标、配对差值与校准；输出图和三线表。")
    ev<-report_eval(y,p,"all",list(c("C","I"),c("C","S"),c("SD","S"),c("C","O"),c("P","C"),c("CT","SD")),plan$seed+9000L)
    strata<-list()
    for(g in 0:1){ii<-which(b$baseline$in_index_icu==g);ps<-p[ii,c("S","C","SD","O","P","CT"),drop=FALSE]
      strata[[as.character(g)]]<-report_eval(y[ii],ps,paste0("ICU",g),list(c("SD","S"),c("C","O")),plan$seed+9100L+g)
    }
    make_visuals8(y,p,b$baseline$in_index_icu,ev,strata)
    txt_out(c(
      "Step8：T48临床与观察过程对照；与Step7相同5133人、630事件、固定外5内5患者分折及弹性网网格。",
      "C为Step7锁定折外预测，不重新选择主模型；重新核对导出预测、输入参考及C/D差异。",
      "I使用各外训练折ICU二分类组内事件率，即截距加二分类变量的二项最大似然估计；无调参。",
      "S为年龄、性别、ICU状态、预测前血管活性药文档及来源、0-48小时最近一次乳酸/GCS。不是已验证命名评分。",
      "最近一次值允许在0-48小时内；不声称是T48即时值。血管活性药阳性指预测前有正速率记录，不代表T48仍在用。",
      "SD在S上加入与C相同的252项连续动态和12项四域评分。CT在SD上加入PCI/CABG/IABP/Impella/ECMO/既往CAD文档及来源。",
      "临床0指可观测来源中没有阳性记录，NA指来源不可用；两者始终区别。源覆盖列和训练内缺失指示可进入临床模型。",
      "CT检验文档变量的预测增量，不用于推断治疗效果，也不能证明已排除治疗或心功能混杂。LVEF/RV等未可靠获取变量不补零。",
      "O仅包括21核心变量累计窗口的观测次数/缺失标记及动态输入/评分的缺失标记，无生理值、年龄、性别、ICU状态、治疗值。",
      "P保留年龄、性别与连续生理信息；移除ICU状态且不生成缺失指示；无直接支持强度/次数变量。",
      "P仍含训练中位数填补、记录时点和可测量性影响，不能解释为完全排除了观察过程偏倚。",
      "评分参考、筛选、填补和标准化均在当前训练折拟合；O缺失评分标记也使用训练折参考；不重新聚类。",
      "所有新弹性网模型按合并内折log loss选参，不按外折指标挑选；不收敛的候选损失设Inf，已选参数无法预测则失败。",
      "ICU分层评价使用原队列的同一折外预测，不在亚组重新训练。状态为是否仍在本次index ICU；转出ICU者仍在院。",
      "95%CI为固定OOF预测的2000次患者bootstrap；亚组在组内重抽样。未包含重新训练和开发全部不确定性。",
      "AUROC/AP越高、Brier/log loss越低越好。AUPRC采用同分阈值分组average precision。配对差使用相同重抽样。",
      "校准分别报告固定斜率1的截距以及联合估计的截距/斜率；不在OOF上重新校准风险。",
      "5%/15%/30%阈值仅作固定展示，不用评价结果选阈值、不视为临床治疗分级；输出逐阈值TP/FP/FN/TN。",
      "本步仅补充临床与观察过程比较；不扫描T7原始数据库，不使用eICU训练或评价，不开展T6/T24、非重叠窗口或DCA。",
      "新增临床增益补充图/表；ICU分层图用于替换原补充Figure S6对应分析，最终编号整合时确定。暂不替换Figure4F/G。",
      "输出无时间戳；保留有效折缓存，覆盖本脚本登记的旧结果，保留用户另存文件；中断后重跑同一脚本。",
      "患者级结果、模型、评分参考仅留local_only；请回传Step8_feedback.zip。"),"to_share/00_READ_ME_FIRST.txt")
    txt_out("https://glmnet.stanford.edu/articles/glmnet.html","to_share/10_method_sources.txt")
    status<<-"COMPLETED_FOR_REVIEW"
  }
  synthetic_bundle<-function(n=160L){
    with_seed(71L,{
      sp<-engine$specification();core<-unique(sp$variable[sp$tier=="main"]);fp<-list()
      for(w in windows){
        for(v in core)for(st in c("mean_value","sd_value","slope_per_hour","abnormal_fraction"))fp[[length(fp)+1L]]<-data.frame(feature=engine$key(v,w,st),role="main_continuous",tier="main",window=w)
        for(v in core)for(st in c("n_chart_times","missing_flag"))fp[[length(fp)+1L]]<-data.frame(feature=engine$key(v,w,st),role="observation_process",tier="main",window=w)
      }
      fp<-do.call(rbind,fp);cols<-unique(c(fp$feature,unlist(lapply(windows,function(w)engine$key(sp$variable,w,sp$stat))),engine$key(c("lactate","gcs_total"),"0-48h","last_value")))
      latent<-rep(c(-2,2),length.out=n)+stats::rnorm(n,0,.25);raw<-as.data.frame(matrix(0,n,length(cols)));names(raw)<-cols
      for(nm in cols){v<-latent+stats::rnorm(n,0,.2);if(grepl("__min_value$",nm))v<--v;if(grepl("__abnormal_fraction$",nm))v<-stats::plogis(v);raw[[nm]]<-as.numeric(v)}
      for(v in core)for(w in windows){
        cnt<-as.numeric(stats::rpois(n,3));raw[[engine$key(v,w,"n_chart_times")]]<-cnt;raw[[engine$key(v,w,"missing_flag")]]<-as.numeric(cnt==0)
        nms<-names(raw)[startsWith(names(raw),paste0(v,"__",gsub("-","_",w,fixed=TRUE),"__"))&!grepl("__(n_chart_times|missing_flag)$",names(raw))]
        for(nm in nms)raw[[nm]][cnt==0]<-NA_real_
      }
      raw[[engine$key("lactate","0-48h","last_value")]][seq(1,n,4)]<-NA_real_
      base<-data.frame(age=stats::runif(n,40,85),sex_male=as.numeric(seq_len(n)%%2),in_index_icu=as.numeric(seq_len(n)%%3!=0))
      cx<-data.frame(row.names=seq_len(n))
      for(v in c("vasoactive",treatment_vars)){
        cov<-as.numeric(seq_len(n)%%5!=0);pos<-as.numeric(stats::runif(n)<.3);pos[cov==0]<-NA_real_
        cx[[paste0(v,"__documented_positive")]]<-pos;cx[[paste0(v,"__patient_source_covered")]]<-cov
      }
      y<-stats::rbinom(n,1,stats::plogis(-.7+.5*latent))
      list(version="Step6_v1",metadata=data.frame(patient_key=sprintf("fixture:%04d",1:n),source="MIMIC",landmark="T48"),raw_numeric=raw,
        baseline=base,clinical_documentation=cx,feature_plan=fp,outcomes=data.frame(event_primary=y,endpoint="30_day_all_cause_death_after_landmark"))
    })
  }
  preflight<-function(){
    oldplan<-plan;on.exit(plan<<-oldplan,add=TRUE)
    plan<<-utils::modifyList(plan,list(min_reference_n=8L,preprocess_min_n=8L,alpha=c(.5,1),lambda=c(1,.3,.1,.03),bootstrap_repeats=10L))
    b<-synthetic_bundle();input_check8(b);tr<-1:120;va<-121:160
    st<-fit_state8(b,tr);changed<-b;changed$raw_numeric[va,]<-999;changed$outcomes$event_primary<-1-b$outcomes$event_primary
    stopifnot(identical(st,fit_state8(changed,tr)))
    d<-design8(b,va,st);changed<-b
    valcols<-names(b$raw_numeric)[!grepl("__(n_chart_times|missing_flag)$",names(b$raw_numeric))]
    for(nm in valcols){ii<-!is.na(changed$raw_numeric[[nm]]);changed$raw_numeric[[nm]][ii]<-changed$raw_numeric[[nm]][ii]*2+1}
    changed$baseline$age<-90;changed$baseline$in_index_icu<-1-b$baseline$in_index_icu
    stopifnot(identical(d$O,design8(changed,va,st)$O),!"in_index_icu"%in%names(d$P),
      !any(grepl("n_chart_times|missing_flag|^missing__|documented_positive",names(d$P))))
    stopifnot(all(is.na(d$S$vasoactive__documented_positive[d$S$vasoactive__patient_source_covered==0])),
      all(!is.na(d$S$vasoactive__documented_positive[d$S$vasoactive__patient_source_covered==1])))
    z<-run_split8(b,tr,va,setNames(as.list(rep(3L,length(fit_names))),fit_names));p<-predict8(b,va,z);one<-predict8(b,va[2],z)
    stopifnot(max(abs(p-z$prediction))<1e-12,max(abs(one[1,]-p[2,]))<1e-12,length(z$models$P$preprocessor$indicators)==0,
      abs(metrics(c(1,0,1),c(.5,.5,.2))["AUPRC_AP"]-7/12)<1e-12)
    th<-threshold_counts(b$outcomes$event_primary[va],p)
    stopifnot(all(th$tp+th$fp+th$fn+th$tn==length(va)),all(th$tp+th$fn==sum(b$outcomes$event_primary[va])))
    # Missing clinical source must never silently become a documented negative.
    bad<-b;bad$clinical_documentation$vasoactive__documented_positive[5]<-0
    stopifnot(inherits(try(input_check8(bad),silent=TRUE),"try-error"))
    TRUE
  }
  make_visuals8<-function(y,p,icu,ev,strata){
    colors<-c(I="#7A8797",S="#4F7FC4",C="#D98C57",SD="#8B68A6",O="#619E91",P="#B49C5B",CT="#B36F82")
    save_figure<-function(stem,draw,width=10,height=8){
      open_pdf(reg(paste0("to_share/",stem,".pdf")),width,height);tryCatch(draw(),finally=grDevices::dev.off())
      if(cfg$MAKE_PNG){grDevices::png(reg(paste0("to_share/",stem,".png")),width=width,height=height,units="in",res=400);tryCatch(draw(),finally=grDevices::dev.off())}
    }
    forest<-function(z,labels,title,xlab,col,xlim=NULL){
      nr<-nrow(z);yy<-rev(seq_len(nr));rr<-if(is.null(xlim))range(c(0,z$lower,z$upper),na.rm=TRUE)else range(c(xlim,z$lower,z$upper),na.rm=TRUE)
      pad<-max(.005,diff(rr)*.08);plot(0,0,type="n",xlim=rr+c(-pad,pad),ylim=c(.5,nr+.5),yaxt="n",xlab=xlab,ylab="",bty="l",main=title,adj=0)
      axis(2,at=yy,labels=labels,tick=FALSE,las=1,cex.axis=.85);segments(z$lower,yy,z$upper,yy,col=col,lwd=2);points(z$estimate,yy,col=col,pch=16)
    }
    plot_cal<-function(bins,arms,title){
      plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),xlab="Mean predicted probability",ylab="Observed event proportion",bty="l",main=title,adj=0);abline(0,1,lty=2,col="#BBBBBB")
      for(a in arms){z<-bins[bins$model==a,];lines(z$predicted,z$observed,type="b",pch=16,col=colors[a],lwd=1.5,cex=.65)}
      legend("topleft",legend=unname(model_names[arms]),col=colors[arms],lwd=2,bty="n",cex=.75)
    }
    draw<-function(){
      op<-par(no.readonly=TRUE);on.exit(par(op));par(mfrow=c(2,2),mar=c(4,8.4,2.5,.8),oma=c(1.8,0,.5,0),family="sans",las=1,cex=.8)
      z<-ev$metrics[ev$metrics$metric=="AUROC",];z<-z[match(names(model_names),z$model),]
      forest(z,unname(model_names[z$model]),"A  Discrimination","AUROC (95% CI)",colors[z$model],c(.45,1));abline(v=.5,lty=2,col="#BBBBBB")
      z<-ev$differences[ev$differences$metric=="AUROC",]
      forest(z,z$comparison,"B  Paired increments","AUROC difference (95% CI)",rep("#4F7FC4",nrow(z)));abline(v=0,lty=2,col="#BBBBBB")
      par(mar=c(4,4.5,2.5,.8))
      plot_cal(ev$bins,c("S","C","SD","CT"),"C  Clinical comparators")
      plot_cal(ev$bins,c("C","O","P"),"D  Observation-process checks")
      mtext("Same patients and outer folds; S/SD/O/P/CT tuned inside training folds. C: locked Step7 OOF predictions.",side=1,outer=TRUE,line=.35,cex=.7)
    }
    save_figure("Figure_S_T48_clinical_increment",draw,11,8.2)
    draw_strata<-function(){
      op<-par(no.readonly=TRUE);on.exit(par(op));par(mfrow=c(2,2),mar=c(4,4.5,3,.8),oma=c(1.5,0,.5,0),family="sans",las=1,cex=.85)
      for(g in 0:1){
        ii<-which(icu==g);arms<-c("S","C","SD");label<-if(g==1)"In index ICU"else"Out of index ICU"
        sub<-sprintf("%s: n=%s; events=%s",label,length(ii),sum(y[ii]))
        plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),xlab="False-positive rate",ylab="True-positive rate",bty="l",main=paste0(if(g==0)"A  "else"C  ",sub),adj=0,cex.main=.95);abline(0,1,lty=2,col="#BBBBBB")
        for(a in arms){cv<-curves(y[ii],p[ii,a]);lines(c(0,cv$fpr),c(0,cv$tpr),col=colors[a],lwd=2)}
        legend("bottomright",legend=paste0(arms,": ",sprintf("%.3f",vapply(arms,function(a)metrics(y[ii],p[ii,a])["AUROC"],numeric(1)))),col=colors[arms],lwd=2,bty="n",cex=.8)
        plot_cal(strata[[as.character(g)]]$bins,arms,paste0(if(g==0)"B  "else"D  ",label," calibration"))
      }
      mtext("All patients remained in hospital at T48. Subgroups use the original OOF predictions; models are not refitted by subgroup.",side=1,outer=TRUE,line=.3,cex=.65)
    }
    save_figure("Figure_S_T48_ICU_strata",draw_strata,10.5,8.2)
    fmt<-function(z)sprintf("%.3f (%.3f, %.3f)",z$estimate,z$lower,z$upper)
    table_metrics<-function(e){tab<-data.frame(Model=unique(e$metrics$model));for(m in c("AUROC","AUPRC_AP","Brier","LogLoss")){z<-e$metrics[e$metrics$metric==m,];tab[[m]]<-fmt(z[match(tab$Model,z$model),])};tab}
    tab<-table_metrics(ev);dif<-data.frame(Comparison=unique(ev$differences$comparison))
    for(m in c("AUROC","AUPRC_AP","Brier","LogLoss")){z<-ev$differences[ev$differences$metric==m,];dif[[m]]<-fmt(z[match(dif$Comparison,z$comparison),])}
    csv_out(tab,"to_share/Table_S_T48_clinical_performance.csv");csv_out(dif,"to_share/Table_S_T48_clinical_increment.csv")
    notes<-c("I: ICU state; S: simple clinical; C: Step7 core dynamics; SD: clinical + dynamics; O: observation process; P: reduced observation cues; CT: extended clinical.",
      "AP: average precision. Positive AUROC/AP and negative Brier/log-loss differences favor the first model.",
      "95% intervals: patient bootstrap of fixed OOF predictions, excluding complete model-training uncertainty.")
    three_line(list("MIMIC T48 nested internal validation"=tab,"Paired differences"=dif),"Supplementary table. Clinical and observation-process comparators","Table_S_T48_clinical_comparators",notes)
    tables<-list();for(g in 0:1){ii<-which(icu==g);name<-sprintf("%s: n=%s; events=%s (%.1f%%)",if(g==1)"In index ICU"else"Out of index ICU, still in hospital",length(ii),sum(y[ii]),100*mean(y[ii]))
      tables[[name]]<-table_metrics(strata[[as.character(g)]]);csv_out(tables[[name]],paste0("to_share/Table_S_T48_ICU",g,"_performance.csv"))}
    three_line(tables,"Supplementary table. T48 performance by ICU state","Table_S_T48_ICU_strata",notes)
    csv_out(data.frame(output=c("Figure_S_T48_clinical_increment","Figure_S_T48_ICU_strata","Table_S_T48_clinical_comparators","Table_S_T48_ICU_strata"),
      action=c("ADD supplementary","REPLACE previous Supplementary Figure S6 ICU-state analysis","ADD supplementary","ADD supplementary"),
      relation=c("New clinical / observation-process comparison; no Figure4F/G replacement yet","New eligibility, endpoint and OOF predictions; final S number assigned during integration",
        "Companion performance and paired-increment table","Companion ICU-state table; final S number assigned during integration")),"to_share/11_figure_table_replacement_map.csv")
  }
  if(isTRUE(getOption("ami_step8.functions_only",FALSE)))return(invisible(list(preflight=preflight,synthetic_bundle=synthetic_bundle,
    fit_state8=fit_state8,design8=design8,run_split8=run_split8,predict8=predict8,metrics=metrics,evaluate8=evaluate8,
    make_visuals8=make_visuals8,main=main,cache_read=cache_read,cache_save=cache_save)))
  status<-"FAILED";current_module<-"input verification"
  failure_message<-NULL
  tryCatch(main(),error=function(err){status<<-"FAILED";failure_message<<-conditionMessage(err)
    issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step8_error_detail.txt")})
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step8",
    "COMPLETED_FOR_REVIEW表示T48临床/观察过程比较及ICU分层评价已生成，等待结果核对；外部验证尚未开展。",
    "FAILED表示不完整，不能使用本次患者级结果建模。"),"to_share/Step8_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step8_sessionInfo.txt")
  logmsg(paste0("Step8状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step8_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share) else {
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step8_feedback.zip"),"\n",sep="") else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
