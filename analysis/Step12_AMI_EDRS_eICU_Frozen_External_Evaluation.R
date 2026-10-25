# AMI-EDRS revision | Step12: frozen eICU related-endpoint transport evaluation
# source(file.choose(), encoding="UTF-8")
# Fixed filenames; replaces owned results. Patient-level data and frozen references stay local_only.
local({
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MAKE_PNG=TRUE,MAKE_ZIP=TRUE)
  cfg<-utils::modifyList(cfg,getOption("ami_step12.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step12")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step12输出目录。")
  if (is_link(out)) stop("Step12输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP12")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step12目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP12_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP12_MANAGED_OUTPUT")) {
    stop("Step12输出目录的归属标记不正确。")
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
        if (unlink(p) != 0L) stop("不能覆盖旧Step12输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step12_run_log.txt")
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
    txt_out(if(nrow(z))unlist(Map(function(a,b)c(a,b,""),z$scope,z$message))else"No warnings in Step12.","local_only/Step12_warning_detail.txt")
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
  plan<-list(version="Step12_v1",models=c("A","B","C","D"),bootstrap_n=2000L,seed=2026L,
    prediction_endpoint="30_day_all_cause_death_after_T48",evaluation_endpoint="in_hospital_death_after_T48",
    intervals="Paired patient bootstrap; hospital cluster bootstrap as sensitivity; fixed frozen model",
    epsilon=1e-12,update_model=FALSE,refit_reference=FALSE)
  labels<-c(A="A: Baseline",B="B: Baseline + phenotype",C="C: Continuous dynamics",D="D: Dynamics + phenotype")
  colors<-c(A="#7A8797",B="#4F7FC4",C="#D98C57",D="#8B68A6")
  domains<-c(hemodynamic="Hemodynamic",respiratory_neurologic="Respiratory / neurologic",renal_metabolic="Renal / metabolic",inflammatory_coagulopathic="Inflammation / coagulation")
  windows<-c("0-6h","0-24h","0-48h")
  pairs<-data.frame(first=c("B","C","D"),second=c("A","A","C"),comparison=c("B-A","C-A","D-C"))
  engine<-NULL;predict_step7<-NULL
  hash_object<-function(x){f<-tempfile();on.exit(unlink(f));saveRDS(x,f,version=2);unname(tools::md5sum(f))}
  cache_result<-function(rel,key,fun){p<-safe_path(rel)
    if(file.exists(p)){z<-tryCatch(readRDS(p),error=function(e)NULL);if(is.list(z)&&isTRUE(z$complete)&&identical(z$key,key)){reg(rel);logmsg(paste0("复用缓存：",basename(rel)));return(z$value)}}
    z<-fun();pending<-reg(paste0(rel,".pending"));saveRDS(list(complete=TRUE,key=key,value=z),pending,compress="gzip")
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
  profile_summary<-function(ans,source,k){
    rows<-list()
    for(cl in seq_len(k))for(d in names(domains))for(w in windows){v<-ans$scores[[paste(d,gsub("-","_",w,fixed=TRUE),"score",sep="__")]][ans$phenotype==cl];obs<-v[!is.na(v)]
      q<-if(length(obs))quantile(obs,c(.25,.5,.75),names=FALSE)else rep(NA_real_,3)
      rows[[length(rows)+1L]]<-data.frame(source=source,phenotype=paste0("C",cl),domain=d,window=w,n=length(v),observed_n=length(obs),missing_n=sum(is.na(v)),q25=q[1],median=q[2],q75=q[3])
    };do.call(rbind,rows)
  }
  main<-function(){
    current_module<<-"small metric and PDF preflight";logmsg("小型预检：同值AUROC/AUPRC、患者配对与医院重抽样、概率退化、校准参数及多页PDF。")
    preflight();txt_out("PASS: weighted metrics vs direct pairwise/trapezoid-free AP; paired/hospital bootstrap; degenerate predictions; calibration; multipage PDF.","to_share/02_preflight.txt")
    current_module<<-"frozen inputs and exported inference"
    for(step in c("Step6","Step7"))verify_lineage(step)
    paths<-c(mimic=file.path(cfg$OUTPUT_PARENT,"Step6/local_only/MIMIC_T48_model_inputs.rds"),
      eicu=file.path(cfg$OUTPUT_PARENT,"Step6/local_only/eICU_T48_model_inputs.rds"),
      engine=file.path(cfg$OUTPUT_PARENT,"Step7/local_only/Step7_score_engine.R"),
      predict=file.path(cfg$OUTPUT_PARENT,"Step7/local_only/Step7_predict.R"),
      frozen=file.path(cfg$OUTPUT_PARENT,"Step7/local_only/MIMIC_T48_frozen_pipeline.rds"),
      outer=file.path(cfg$OUTPUT_PARENT,"Step7/local_only/MIMIC_T48_outer_models.rds"),
      oof=file.path(cfg$OUTPUT_PARENT,"Step7/local_only/MIMIC_T48_nested_OOF.rds"))
    if(!all(file.exists(paths)))stop("Step6/7本地输入不完整。")
    h<-unname(tools::md5sum(paths));csv_out(data.frame(file=unname(paths),md5=h),"to_share/01_inputs.csv")
    m<-readRDS(paths["mimic"]);e<-readRDS(paths["eicu"]);obj<-readRDS(paths["frozen"])
    validate_bundle(m,"MIMIC",5133L,630L);validate_bundle(e,"eICU",5284L,219L)
    if(!identical(obj$version,"Step7_v1")||obj$endpoint!=plan$prediction_endpoint||obj$landmark!="T48"||
       !identical(names(obj$models),plan$models)||obj$training_n!=nrow(m$metadata))stop("冻结模型版本或训练终点不符。")
    if(!identical(m$feature_plan,e$feature_plan)||!identical(m$score_specification,e$score_specification)||
       !identical(names(m$raw_numeric),names(e$raw_numeric))||!identical(names(m$baseline),names(e$baseline)))stop("两库特征定义或顺序不一致；不能隐式填补不存在的列。")
    env<-new.env(parent=baseenv());env$plogis<-stats::plogis;sys.source(paths["engine"],envir=env);sys.source(paths["predict"],envir=env)
    engine<<-env$step6_score_engine;predict_step7<<-env$predict_step7
    if(!is.function(predict_step7)||!is.list(engine))stop("冻结导出接口无效。")
    obj_hash<-hash_object(obj);outer<-readRDS(paths["outer"]);oof<-readRDS(paths["oof"])
    if(!identical(oof$metadata$patient_key,m$metadata$patient_key)||!identical(oof$outcomes,m$outcomes)||
      !setequal(oof$outer_fold,1:5)||!identical(colnames(oof$prediction),plan$models))stop("Step7折外结果与MIMIC输入不一致。")
    checks<-list()
    for(o in 1:5){ix<-which(oof$outer_fold==o);oo<-outer[[paste0("outer",o)]]
      if(is.null(oo))stop("缺少外折模型。")
      z<-predict_step7(m$raw_numeric[ix,,drop=FALSE],m$baseline[ix,,drop=FALSE],oo)
      delta<-max(abs(z$probability-oof$prediction[ix,,drop=FALSE]));if(!is.finite(delta)||delta>1e-10)stop("冻结接口不能复现MIMIC折外概率。")
      checks[[paste0("outer",o)]]<-data.frame(check=paste0("MIMIC_outer",o),n=length(ix),max_probability_error=delta,phenotype_mismatches=sum(z$phenotype!=oof$fold_specific_phenotype[ix]))
      if(any(z$phenotype!=oof$fold_specific_phenotype[ix]))stop("外折冻结赋类不一致。")
    }
    logmsg("MIMIC五个外折导出接口复核通过；直接应用冻结参考、填补、中心及A-D模型到eICU。")
    both<-list(MIMIC=m,eICU=e);pred<-list()
    for(src in names(both)){b<-both[[src]];z<-predict_step7(b$raw_numeric,b$baseline,obj);manual<-manual_predict(b,obj)
      err<-max(abs(z$probability-manual$probability));if(!is.finite(err)||err>1e-10||any(z$phenotype!=manual$phenotype))stop("冻结接口与独立矩阵推理不一致。")
      pick<-unique(as.integer(c(1,round(nrow(b$metadata)/2),nrow(b$metadata))));max_single<-0
      for(j in pick){single<-predict_step7(b$raw_numeric[j,,drop=FALSE],b$baseline[j,,drop=FALSE],obj)
        max_single<-max(max_single,max(abs(single$probability[1,]-z$probability[j,])))
        if(single$phenotype[1]!=z$phenotype[j])stop("单例/批量分型不一致。")}
      if(max_single>1e-10)stop("单例/批量概率不一致。")
      # Modify other assessment patients; the first patient's result must not change.
      raw2<-b$raw_numeric[1:3,,drop=FALSE];raw2[2:3,]<-NA_real_;base2<-b$baseline[1:3,,drop=FALSE]
      zz<-predict_step7(raw2,base2,obj);if(max(abs(zz$probability[1,]-z$probability[1,]))>1e-10||zz$phenotype[1]!=z$phenotype[1])stop("预测依赖同批其他患者。")
      checks[[src]]<-data.frame(check=paste0(src,"_full_frozen"),n=nrow(z$probability),max_probability_error=max(err,max_single),phenotype_mismatches=0)
      pred[[src]]<-manual
    }
    if(hash_object(obj)!=obj_hash)stop("冻结模型被修改。")
    csv_out(do.call(rbind,checks),"to_share/02_prediction_roundtrip.csv")
    y<-e$outcomes$event_primary;p<-pred$eICU$probability
    metric_setup(y,p)
    hid<-as.character(e$metadata$hospital_id);hospital_ok<-length(hid)==length(y)&&!anyNA(hid)&&all(nzchar(trimws(hid)))&&length(unique(hid))>=2
    hosp<-if(hospital_ok)hid else NULL
    csv_out(data.frame(source=c("MIMIC","eICU"),n=c(nrow(m$metadata),length(y)),events=c(sum(m$outcomes$event_primary),sum(y)),
      outcome=c("30-day death after T48","In-hospital death after T48"),in_index_ICU=c(sum(m$baseline$in_index_icu),sum(e$baseline$in_index_icu))),"to_share/02_cohort_endpoints.csv")
    csv_out(data.frame(hospital_ids_complete=!anyNA(hid)&&all(nzchar(trimws(hid))),hospital_count=length(unique(hid[!is.na(hid)&nzchar(hid)])),
      cluster_bootstrap=hospital_ok,n=length(y),events=sum(y)),"to_share/02_hospital_availability.csv")
    if(!hospital_ok)issue("NOTE","hospital uncertainty","医院ID不完整或不足两个医院；跳过医院重抽样，不人为填补医院。")
    diag<-inspect_prediction(p,pred$eICU$lp);csv_out(diag,"to_share/03_external_prediction_diagnostics.csv")
    for(a in plan$models)if(diag$unique_probabilities[diag$model==a]<2)issue("WARNING","constant predictions",paste0(a,"模型外部概率为常数；如实保留，不生成替代风险。"))
    audits<-rbind(cbind(source="MIMIC",pred$MIMIC$audit),cbind(source="eICU",pred$eICU$audit));csv_out(audits,"to_share/03_model_feature_availability.csv")
    missing_active<-audits[audits$source=="eICU"&audits$all_missing,]
    if(nrow(missing_active))issue("WARNING","external missingness",paste0(nrow(missing_active),"个模型-特征组合全缺失，已按冻结MIMIC参数填补并在可用性表明确列出；需要审核可迁移性。"))
    key<-hash_object(list(inputs=h,plan=plan,metric=body(weighted_metrics),evaluate=body(evaluate_external)))
    txt_out(capture.output(dput(plan)),"to_share/02_analysis_plan.txt")
    current_module<<-"external performance and paired uncertainty"
    logmsg("计算原始A-D外部区分能力与相关终点误差；每种重抽样2000次，所有模型使用相同重抽样患者/医院。")
    ev<-cache_result("local_only/cache/eICU_patient_bootstrap.rds",paste0(key,"patient"),function()evaluate_external(y,p))
    results<-list(ev);if(hospital_ok)results[[2]]<-cache_result("local_only/cache/eICU_hospital_bootstrap.rds",paste0(key,"hospital"),function()evaluate_external(y,p,hosp,seed=plan$seed+1L))
    met<-do.call(rbind,lapply(results,`[[`,"metrics"));diff<-do.call(rbind,lapply(results,`[[`,"differences"))
    if(any(met$bootstrap_valid<.9*met$bootstrap_requested))issue("WARNING","bootstrap validity","部分指标有效重抽样不足90%，对应区间保留NA，见指标表。");
    csv_out(met,"to_share/04_external_metrics.csv");csv_out(diff,"to_share/04_external_paired_differences.csv")
    cal<-do.call(rbind,lapply(plan$models,function(a)cbind(model=a,calibration(y,p[,a],hosp))))
    csv_out(cal,"to_share/05_related_endpoint_calibration.csv")
    bins<-do.call(rbind,lapply(plan$models,function(a)cbind(model=a,calibration_bins(y,p[,a]))));csv_out(bins,"to_share/05_calibration_bins.csv")
    csv_out(data.frame(n=length(y),events=sum(y),event_fraction=mean(y),event_fraction_Brier=mean(y)*(1-mean(y)),
      note="Descriptive external-prevalence reference, not a prospectively estimated baseline model"),"to_share/04_prevalence_reference.csv")
    # Fixed grid only: do not export a row for each patient's unique probability.
    curve_grid<-do.call(rbind,lapply(plan$models,function(a)do.call(rbind,lapply(seq(0,1,length.out=201),function(t){sel<-p[,a]>=t;tp<-sum(sel&y==1);fp<-sum(sel&y==0)
      data.frame(model=a,threshold=t,TP=tp,FP=fp,FN=sum(y)-tp,TN=sum(1-y)-fp,n=length(y))}))))
    csv_out(curve_grid,"to_share/05_fixed_grid_curve_counts.csv")
    current_module<<-"frozen phenotype transfer"
    k<-obj$state$cluster$k;prof<-rbind(profile_summary(pred$MIMIC,"MIMIC",k),profile_summary(pred$eICU,"eICU",k))
    sizes<-do.call(rbind,lapply(names(pred),function(src)do.call(rbind,lapply(seq_len(k),function(j){ix<-which(pred[[src]]$phenotype==j);data.frame(source=src,phenotype=paste0("C",j),n=length(ix),fraction=length(ix)/nrow(pred[[src]]$probability))}))))
    dst<-do.call(rbind,lapply(seq_len(k),function(j){ref<-pred$MIMIC$distance[pred$MIMIC$phenotype==j];thr<-if(length(ref))as.numeric(quantile(ref,.95))else NA_real_
      do.call(rbind,lapply(names(pred),function(src){v<-pred[[src]]$distance[pred[[src]]$phenotype==j];qq<-if(length(v))quantile(v,c(.25,.5,.75),names=FALSE)else rep(NA_real_,3)
        data.frame(source=src,phenotype=paste0("C",j),n=length(v),q25=qq[1],median=qq[2],q75=qq[3],MIMIC_distance_p95=thr,
          n_above_MIMIC_p95=if(is.finite(thr))sum(v>thr)else NA_real_,fraction_above_MIMIC_p95=if(length(v)&&is.finite(thr))mean(v>thr)else NA_real_)}))}))
    csv_out(prof,"to_share/06_frozen_phenotype_profiles.csv");csv_out(sizes,"to_share/06_frozen_phenotype_sizes.csv");csv_out(dst,"to_share/06_frozen_assignment_distances.csv")
    saveRDS(list(version=plan$version,metadata=e$metadata,outcomes=e$outcomes,prediction=p,linear_predictor=pred$eICU$lp,
      phenotype=pred$eICU$phenotype,distance=pred$eICU$distance,prediction_endpoint=plan$prediction_endpoint,evaluation_endpoint=plan$evaluation_endpoint,
      input_md5=setNames(h,names(paths)),note="Locked raw predictions; no update; not 30-day external absolute-risk validation"),reg("local_only/eICU_T48_frozen_predictions.rds"),compress="gzip")
    current_module<<-"large-font figures and three-line tables"
    logmsg("输出大字体ROC、PR、相关终点校准诊断、概率分布、冻结分型画像及三线表。")
    make_visuals(y,p,met,diff,cal,bins,diag,prof,sizes,dst)
    csv_out(data.frame(output=c("Figure_S_eICU_raw_ROC","Figure_S_eICU_raw_PR","Figure_S_eICU_related_endpoint_calibration","Figure_S_eICU_raw_probability_distribution","Figure_S_eICU_frozen_profiles","Figure_S_eICU_frozen_assignment","Table_S_eICU_frozen_performance","Table_S_eICU_calibration_diagnostics","Table_S_eICU_frozen_transfer"),
      action="ADD supplementary; final external panel mapping pending integration",
      detail=c("Unupdated A-D ROC","AUPRC uses grouped average precision; event-rate reference","30-day predictions vs hospital outcomes; no recalibration","All four models, including poorly transferred outputs","Same frozen MIMIC score references and centers; not recovery/deterioration labels","Class size and distance to frozen centers","Patient bootstrap and hospital-cluster sensitivity; paired differences","Calibration offset intercept separate from joint intercept/slope","Class counts, distances and component availability")),"to_share/11_figure_table_replacement_map.csv")
    txt_out(c("Step12：eICU相关终点可迁移性评价；MIMIC T48冻结A-D模型，不重训或校准。",
      "MIMIC终点是T48后30天全因死亡；eICU为T48后本次住院死亡。因此不称为原30天绝对风险的同终点外部验证。",
      "只应用既有评分参考、缺失填补、标准化、分型中心和模型系数；未在eICU重排名、重聚类、选特征或调参。",
      "原始A-D结果全部保留。A基础信息；B基础信息+分型；C基础信息+连续动态特征；D在C上加分型。",
      "缺失列停止；列存在但个体缺失时使用冻结MIMIC填补。全缺失的模型特征列单独报告，不当作正常值或未发生。",
      "AUROC/AUPRC按相同概率合并处理；AUPRC为average precision而非梯形插值。Brier和LogLoss描述对相关住院终点的误差。",
      "LogLoss与校准计算将0/1概率截断至1e-12及1-1e-12以保证数值有限；保存的原始预测、AUROC、AUPRC和Brier不作截断。",
      "患者配对bootstrap 2000次为主；医院ID完整且至少2家时，补充2000次医院整体重抽样，保留其全部患者及重复医院权重。",
      "区间条件于冻结模型，不包含MIMIC模型重建不确定性。缺单一结局的重抽样不计算AUROC；有效重复不足90%时区间NA。",
      "校准截距(offset固定斜率1)、联合截距及斜率分别输出。模型拟合仅用于诊断，不用其生成更新后的预测。",
      "校准参数同时给模型Wald区间与可用时的医院聚类稳健Wald区间。退化/分离拟合不输出伪造参数。",
      "校准图和参数是相关终点诊断，不证明30天风险失准完全由数据库差异造成。未在本步进行外部DCA或治疗阈值解释。",
      "分型标签来自同一MIMIC冻结中心；MIMIC全量画像为描述性，不是独立验证。画像使用未填补的域评分并保留缺失分母。",
      "距离为冻结预处理空间的欧氏距离；超过MIMIC组内95百分位是描述性外推标记，不是已验证的拒绝预测阈值。",
      "不合并不同外折的C标签；不按结局重命名两组；不称恢复型/恶化型。MIMIC-eICU之间没有同一患者标签ARI。",
      "本步新增外部原始评价补充图表。原外部图的正式面板替换，待同终点配套模型和后续更新分析完成后统一。",
      "全部新图使用大字体；单图或少量面板，可编辑PDF与350dpi PNG；三线表PDF/RTF及对应CSV。",
      "只读Step6/7已有结果，无需T7；命名无时间戳，覆盖登记输出，保留有效缓存和用户其他文件。",
      "患者级预测、医院标识及冻结对象仅保留local_only，反馈仅包含汇总。下一步准备MIMIC住院终点配套模型。"),"to_share/00_READ_ME_FIRST.txt")
    txt_out(c("eICU patient table: hospital discharge status and offset; https://eicu.mit.edu/eicutables/patient/",
      "Riley RD et al. Evaluation of clinical prediction models (part 2): how to undertake an external validation study. BMJ 2024;384:e074820. https://www.bmj.com/content/384/bmj-2023-074820"),"to_share/10_method_sources.txt")
    if(!identical(h,unname(tools::md5sum(paths)))||hash_object(obj)!=obj_hash)stop("运行中冻结上游文件变化。")
    status<<-"COMPLETED_FOR_REVIEW"
  }
  fmt<-function(x,d=3)ifelse(is.finite(x),formatC(x,format="f",digits=d),"NA")
  fmt_ci<-function(x,l,u,d=3)ifelse(is.finite(x)&is.finite(l)&is.finite(u),paste0(fmt(x,d)," [",fmt(l,d),", ",fmt(u,d),"]"),"NA")
  setup_plot<-function(mf=c(1,1),bottom=3.4){par(mfrow=mf,mar=c(4.3,4.4,2.8,1),oma=c(bottom,0,0,0),family="sans",las=1,cex=1,cex.axis=.95,cex.lab=1.05,cex.main=1.05,mgp=c(2.8,.8,0))}
  note<-function(x){for(i in seq_along(x))mtext(x[i],side=1,outer=TRUE,line=.8+(i-1)*1.15,cex=.82)}
  make_visuals<-function(y,p,met,dif,cal,bins,diag,prof,sizes,dst){
    cur<-lapply(plan$models,function(a)curves(y,p[,a]));names(cur)<-plan$models
    base_note<-c(sprintf("eICU: n=%s; in-hospital deaths=%s after T48",length(y),sum(y)),"Frozen 30-day models; related-endpoint evaluation")
    save_figure("Figure_S_eICU_raw_ROC",function(){setup_plot();plot(0,0,type="n",xlim=c(0,100),ylim=c(0,100),xlab="False-positive rate (%)",ylab="True-positive rate (%)",bty="l",main="Frozen models: external ROC",adj=0);abline(0,1,col="#BBBBBB",lty=3)
      for(a in plan$models)lines(100*c(0,cur[[a]]$fpr),100*c(0,cur[[a]]$recall),col=colors[a],lwd=2.6,lty=match(a,plan$models))
      v<-met[met$resampling=="patient"&met$metric=="AUROC",];v<-v[match(plan$models,v$model),]
      legend("bottomright",legend=paste0(plan$models,": ",fmt(v$estimate)),col=colors,lty=1:4,lwd=2.6,bty="n",cex=.95,title="AUROC")
      note(base_note)},7.4,7.2)
    save_figure("Figure_S_eICU_raw_PR",function(){setup_plot();plot(0,0,type="n",xlim=c(0,100),ylim=c(0,100),xlab="Recall (%)",ylab="Precision (%)",bty="l",main="Frozen models: precision-recall",adj=0)
      for(a in plan$models){z<-cur[[a]];lines(100*c(0,z$recall),100*c(z$precision[1],z$precision),col=colors[a],lwd=2.6,lty=match(a,plan$models),type="S")}
      abline(h=mean(y)*100,lty=3,col="#555555",lwd=1.8)
      v<-met[met$resampling=="patient"&met$metric=="AUPRC",];v<-v[match(plan$models,v$model),]
      legend("topright",legend=c(paste0(plan$models,": ",fmt(v$estimate)),paste0("Event rate: ",fmt(mean(y)))),col=c(colors,"#555555"),lty=c(1:4,3),lwd=2.6,bty="n",cex=.9,title="Average precision")
      note(base_note)},7.4,7.2)
    save_figure("Figure_S_eICU_related_endpoint_calibration",function(){setup_plot(c(2,2),3.7)
      for(a in plan$models){z<-bins[bins$model==a,];plot(0,0,type="n",xlim=c(0,100),ylim=c(0,100),xlab="Model prediction (%)",ylab="Hospital deaths (%)",bty="l",main=labels[a],adj=0)
        abline(0,1,col="#BBBBBB",lty=3);segments(z$predicted*100,z$lower*100,z$predicted*100,z$upper*100,col=colors[a],lwd=1.8)
        lines(z$predicted*100,z$observed*100,type="b",pch=16,col=colors[a],lwd=2.2,cex=1)
      };note(c("30-day model outputs compared with in-hospital outcomes", "Grouped diagnostic only; bars: 95% Wilson intervals"))},11.6,10.2)
    save_figure("Figure_S_eICU_raw_probability_distribution",function(){setup_plot(c(2,2),3.7)
      hh<-lapply(plan$models,function(a)hist(p[,a],breaks=seq(0,1,.05),plot=FALSE));hi<-min(100,max(vapply(hh,function(z)max(z$counts)/length(y)*100,numeric(1)))*1.18)
      for(j in seq_along(plan$models)){a<-plan$models[j];z<-hh[[j]];plot(0,0,type="n",xlim=c(0,100),ylim=c(0,hi),xlab="Model prediction (%)",ylab="Patients (%)",bty="l",main=labels[a],adj=0)
        rect(head(z$breaks,-1)*100,0,tail(z$breaks,-1)*100,z$counts/length(y)*100,col=adjustcolor(colors[a],.65),border="white")
        abline(v=mean(y)*100,lty=3,lwd=2,col="#333333")
        mtext(sprintf("Median %.1f%%",median(p[,a])*100),side=3,line=.15,adj=1,cex=.9)
      };note(c("Same axes and 5-percentage-point bins across models", "Dotted line: observed hospital death rate; no recalibration"))},11.6,10.2)
    k<-length(unique(sizes$phenotype));cc<-setNames(c("#4F7FC4","#D98C57","#8B68A6","#689A69","#BC6F8E","#8E7F57")[seq_len(k)],paste0("C",seq_len(k)))
    save_figure("Figure_S_eICU_frozen_profiles",function(){setup_plot(c(2,2),2);layout(matrix(c(1,2,3,4,5,5),nrow=3,byrow=TRUE),heights=c(1,1,.28));par(cex=1,cex.axis=.95,cex.lab=1.05,cex.main=1.05)
      for(d in names(domains)){plot(0,0,type="n",xlim=c(.9,3.1),ylim=c(0,100),xaxt="n",xlab="Cumulative window",ylab="Median domain score",main=domains[d],adj=0,bty="l");axis(1,at=1:3,labels=windows)
        for(cl in names(cc))for(src in c("MIMIC","eICU")){z<-prof[prof$domain==d&prof$phenotype==cl&prof$source==src,];z<-z[match(windows,z$window),]
          lines(1:3,z$median,col=cc[cl],lty=if(src=="MIMIC")1 else 2,lwd=2.5,type="b",pch=if(src=="MIMIC")16 else 17,cex=1.1)}
      }
      par(mar=rep(0,4));plot.new()
      ll<-as.vector(t(outer(names(cc),c("MIMIC","eICU"),paste)))
      legend("center",legend=ll,col=rep(cc,each=2),lty=rep(1:2,k),pch=rep(c(16,17),k),ncol=2,bty="n",cex=.92,lwd=2.5)
      mtext("Same MIMIC references and centers; available, unimputed scores",side=1,outer=TRUE,line=.3,cex=.82)
    },11.6,11.4)
    save_figure("Figure_S_eICU_frozen_assignment",function(){setup_plot(c(1,2),4)
      z<-matrix(0,2,k,dimnames=list(c("MIMIC","eICU"),names(cc)))
      for(src in rownames(z))for(cl in colnames(z))z[src,cl]<-sizes$fraction[sizes$source==src&sizes$phenotype==cl]*100
      bp<-barplot(z,beside=TRUE,col=c("#4F7FC4","#D98C57"),border=NA,ylim=c(0,100),ylab="Patients (%)",xlab="Frozen phenotype",main="A  Class proportions",cex.names=1.05)
      text(bp,z+3,sprintf("%.1f",z),cex=.85);legend("topright",rownames(z),fill=c("#4F7FC4","#D98C57"),bty="n",cex=.9)
      hi<-max(c(dst$q75,dst$MIMIC_distance_p95),na.rm=TRUE)*1.15
      plot(0,0,type="n",xlim=c(.55,k+.45),ylim=c(0,hi),xaxt="n",xlab="Frozen phenotype",ylab="Distance to frozen center",main="B  Assignment distances",adj=0,bty="l");axis(1,1:k,names(cc))
      for(j in 1:k)for(src in c("MIMIC","eICU")){d<-dst[dst$phenotype==paste0("C",j)&dst$source==src,];x<-j+if(src=="MIMIC")-.12 else .12;co<-if(src=="MIMIC")"#4F7FC4"else"#D98C57"
        segments(x,d$q25,x,d$q75,col=co,lwd=2.4);points(x,d$median,col=co,pch=16,cex=1.2)
        if(src=="MIMIC")segments(j-.25,d$MIMIC_distance_p95,j+.25,d$MIMIC_distance_p95,lty=3,lwd=2,col="#555555")}
      note(c("Distance: median and IQR; dotted: MIMIC within-class 95th percentile", "MIMIC full-fit profiles are descriptive; thresholds are not clinical rules"))},11.6,6.6)
    # Tables remain readable: at most 5 columns and 12 body rows per page.
    tabs<-list()
    for(mode in unique(met$resampling)){
      tt<-data.frame(Model=plan$models,check.names=FALSE)
      for(mm in c("AUROC","AUPRC","Brier")){z<-met[met$resampling==mode&met$metric==mm,];z<-z[match(plan$models,z$model),];tt[[paste0(mm," [95% CI]")]]<-fmt_ci(z$estimate,z$lower,z$upper)}
      tabs[[paste(mode,"uncertainty")]]<-tt
      z<-dif[dif$resampling==mode,];tt<-data.frame(Comparison=pairs$comparison,check.names=FALSE)
      for(mm in c("AUROC","AUPRC","Brier")){r<-z[z$metric==mm,];r<-r[match(pairs$comparison,r$comparison),];tt[[paste0("Delta ",mm)]]<-fmt_ci(r$estimate,r$lower,r$upper)}
      tabs[[paste(mode,"paired differences")]]<-tt
    }
    three_line(tabs,"Frozen eICU performance: related hospital endpoint","Table_S_eICU_frozen_performance",c(
      "A: baseline; B: baseline + phenotype; C: continuous dynamics; D: C + phenotype.",
      "95% paired bootstrap intervals condition on the frozen MIMIC models; 2000 resamples per method.",
      "AUPRC is average precision. Brier differences below zero favor the first model.",
      "This is not same-endpoint validation of 30-day absolute risks. Hospital clustering is a sensitivity analysis."))
    tt<-list()
    for(mode in unique(cal$inference)){
      z<-cal[cal$inference==mode,];tab<-data.frame(Model=plan$models,check.names=FALSE)
      for(pm in c("intercept_offset","joint_intercept","slope")){v<-z[z$parameter==pm,];v<-v[match(plan$models,v$model),];tab[[switch(pm,intercept_offset="Offset intercept",joint_intercept="Joint intercept",slope="Slope")]]<-fmt_ci(v$estimate,v$lower,v$upper,2)}
      tt[[paste(mode,"95% Wald intervals")]]<-tab
    }
    tt[["Unupdated output distribution"]]<-data.frame(Model=diag$model,`Median %`=fmt(diag$p50*100,1),`Range %`=paste0(fmt(diag$p_min*100,1)," - ",fmt(diag$p_max*100,1)),`Unique values`=diag$unique_probabilities,check.names=FALSE)
    three_line(tt,"Frozen model output and related-endpoint calibration","Table_S_eICU_calibration_diagnostics",c(
      "Offset intercept fixes the linear-predictor slope at 1; joint intercept and slope are fitted together.",
      "Fits diagnose agreement with hospital death only; no recalibrated probabilities are used in this step.",
      "Unstable or nonconverged parameters are NA. Full diagnostics and reasons are preserved in CSV.",
      "Patient probability distributions and all four model results are retained, including poor performance."))
    tt<-list();z<-sizes;tt[["Frozen class sizes"]]<-data.frame(Source=z$source,Class=z$phenotype,N=z$n,`Share %`=fmt(z$fraction*100,1),check.names=FALSE)
    for(src in c("MIMIC","eICU")){z<-dst[dst$source==src,];tt[[paste(src,"assignment distance")]]<-data.frame(Class=z$phenotype,N=z$n,`Median [IQR]`=paste0(fmt(z$median,2)," [",fmt(z$q25,2),", ",fmt(z$q75,2),"]"),`Above ref. %`=fmt(z$fraction_above_MIMIC_p95*100,1),check.names=FALSE)}
    for(src in c("MIMIC","eICU")){a<-aggregate(prof[c("observed_n","n")],list(domain=prof$domain,window=prof$window,source=prof$source),sum)
      a<-a[a$source==src,];a<-a[order(match(a$domain,names(domains)),match(a$window,windows)),];tt[[paste(src,"available domain scores")]]<-data.frame(Domain=unname(c(hemodynamic="Hemodynamic",respiratory_neurologic="Resp. / neuro.",renal_metabolic="Renal / metabolic",inflammatory_coagulopathic="Inflam. / coag.")[a$domain]),Window=a$window,N=a$n,`Available %`=fmt(a$observed_n/a$n*100,1),check.names=FALSE)}
    three_line(tt,"Frozen phenotype transfer and score availability","Table_S_eICU_frozen_transfer",c(
      "Both sources use the same MIMIC references, preprocessing and centers; no eICU clustering or relabeling.",
      "MIMIC full-fit descriptions are not independently validated profiles; no outcomes were used for class naming.",
      "Available-score percentages are before imputation. Distance thresholds are descriptive, not validated rules."))
  }
  preflight<-function(){
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
  if(isTRUE(getOption("ami_step12.functions_only",FALSE)))return(invisible(list(preflight=preflight,metric_setup=metric_setup,weighted_metrics=weighted_metrics,evaluate_external=evaluate_external,calibration=calibration,inspect_prediction=inspect_prediction,make_visuals=make_visuals,main=main,validate_bundle=validate_bundle,manual_predict=manual_predict)))
  status<-"FAILED";current_module<-"input verification"
  failure_message<-NULL
  tryCatch(withCallingHandlers(main(),warning=function(w){add_warning(conditionMessage(w),current_module);invokeRestart("muffleWarning")}),error=function(err){status<<-"FAILED";failure_message<<-conditionMessage(err)
    issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step12_error_detail.txt")})
  flush_warnings()
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step12",
    "COMPLETED_FOR_REVIEW表示冻结模型eICU相关终点评价与分型转移已生成，等待结果核对；未更新或重训预测模型。",
    "FAILED表示不完整，不能使用本次患者级结果建模。"),"to_share/Step12_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step12_sessionInfo.txt")
  logmsg(paste0("Step12状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step12_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share) else {
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step12_feedback.zip"),"\n",sep="") else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
