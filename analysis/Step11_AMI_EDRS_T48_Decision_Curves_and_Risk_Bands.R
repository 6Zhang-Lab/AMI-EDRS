# AMI-EDRS revision | Step11: locked OOF decision curves and fixed risk bands
# source(file.choose(), encoding="UTF-8")
# Fixed filenames; replaces owned results. Patient-level data and frozen references stay local_only.
local({
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MAKE_PNG=TRUE,MAKE_ZIP=TRUE)
  cfg<-utils::modifyList(cfg,getOption("ami_step11.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step11")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step11输出目录。")
  if (is_link(out)) stop("Step11输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP11")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step11目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP11_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP11_MANAGED_OUTPUT")) {
    stop("Step11输出目录的归属标记不正确。")
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
        if (unlink(p) != 0L) stop("不能覆盖旧Step11输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step11_run_log.txt")
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
    txt_out(if(nrow(z))unlist(Map(function(a,b)c(a,b,""),z$scope,z$message))else"No warnings in Step11.","local_only/Step11_warning_detail.txt")
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
  open_pdf<-function(path,width,height){grDevices::pdf(path,width=width,height=height,family="Helvetica",onefile=TRUE,useDingbats=FALSE)}
  save_figure<-function(stem,draw,w,h){
    p<-reg(paste0("to_share/",stem,".pdf"));open_pdf(p,w,h);tryCatch(draw(),finally=grDevices::dev.off());check_pdf(p,1L)
    if(cfg$MAKE_PNG){grDevices::png(reg(paste0("to_share/",stem,".png")),width=w,height=h,units="in",res=350);tryCatch(draw(),finally=grDevices::dev.off())}
  }
  quant<-function(x){x<-x[is.finite(x)];if(!length(x))return(c(median=NA_real_,q25=NA_real_,q75=NA_real_));setNames(as.numeric(quantile(x,c(.5,.25,.75),names=FALSE)),c("median","q25","q75"))}
  fmt_q<-function(x){ifelse(is.finite(x$median),sprintf("%.1f [%.1f, %.1f]",x$median,x$q25,x$q75),"Unavailable")}
  draw_table_pdf<-function(tables,title,path,notes=character()){
    if(!length(tables)||any(vapply(tables,nrow,integer(1))>18L))stop("三线表每页最多18行。")
    open_pdf(path,11.69,8.27)
    tryCatch(for(nm in names(tables)){
      tab<-tables[[nm]];nr<-nrow(tab);nc<-ncol(tab);grid::grid.newpage()
      grid::grid.text(title,.04,.95,just="left",gp=grid::gpar(fontsize=13,fontface="bold"))
      grid::grid.text(nm,.04,.90,just="left",gp=grid::gpar(fontsize=10))
      xs<-c(.04,seq(.27,.96,length.out=nc-1));top<-.85;dy<-min(.052,.54/max(1,nr))
      line<-function(y,w)grid::grid.lines(x=c(.04,.97),y=c(y,y),gp=grid::gpar(lwd=w))
      line(top,1.3)
      for(j in seq_len(nc))grid::grid.text(names(tab)[j],xs[j],top-.022,just=if(j==1)"left"else"right",gp=grid::gpar(fontsize=8.5,fontface="bold"))
      line(top-.045,.8)
      for(i in seq_len(nr))for(j in seq_len(nc))grid::grid.text(as.character(tab[i,j]),xs[j],top-.035-i*dy,just=if(j==1)"left"else"right",gp=grid::gpar(fontsize=8.5))
      bot<-top-.055-nr*dy;line(bot,1.3)
      wrap<-unlist(lapply(notes,strwrap,width=145))
      for(i in seq_along(wrap))grid::grid.text(wrap[i],.04,bot-.023*i,just="left",gp=grid::gpar(fontsize=8))
      grid::grid.text(paste(match(nm,names(tables)),"/",length(tables)),.97,.035,just="right",gp=grid::gpar(fontsize=8))
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
        rt<-c(rt,paste0("\\trowd\\trgaph60\\trkeep",paste0(bd,"\\cellx",ends,collapse=""),paste(vapply(seq_len(nc),function(j)paste0("\\pard\\intbl",if(j==1)"\\ql "else"\\qr ","\\fs16 ",if(i==1)"\\b "else"\\b0 ",esc(rows[i,j]),"\\cell"),character(1)),collapse=""),"\\row"))}
      rt<-c(rt,paste0("\\pard\\fs16 ",paste(esc(notes),collapse="\\par "),"\\par"))
    };txt_out(c(rt,"}"),paste0("to_share/",stem,".rtf"))
  }
  plan<-list(version="Step11_v1",landmark="T48",endpoint="All-cause death within 30 days after T48",
    thresholds=(1:50)/100,main_display_upper=.30,display_thresholds=c(.05,.15,.30),risk_cutpoints=c(.05,.15,.30),
    bootstrap_n=2000L,seed=2026L,models=c("I","S","C","SD","O","P","CT"),
    interval="95% pointwise percentile paired patient bootstrap conditional on fixed OOF predictions",
    action="Hypothetical additional clinical review / monitoring assessment at 48 hours",
    threshold_status="Display cutpoints carried forward; broad exploratory range, not validated treatment thresholds",
    no_refit=TRUE)
  model_labels<-c(I="ICU status",S="Simple clinical",C="Core dynamics",SD="Clinical + dynamics",
    O="Observation process",P="Reduced observation cues",CT="Extended clinical",All="Assess all",None="Assess none")
  group_labels<-c(All="All eligible patients",ICU_0="Out of index ICU, still in hospital",ICU_1="In index ICU")
  comparisons<-data.frame(first=c("C","SD","C","P","CT","C","C","SD","SD"),
    second=c("S","S","O","C","SD","All","None","All","None"),stringsAsFactors=FALSE)
  comparisons$comparison<-paste(comparisons$first,comparisons$second,sep="-")
  pct<-function(x,d=1)ifelse(is.finite(x),sprintf(paste0("%.",d,"f"),100*x),"NA")
  fmt_ci<-function(x,l,u,d=3)ifelse(is.finite(x)&is.finite(l)&is.finite(u),
    sprintf(paste0("%.",d,"f [%." ,d,"f, %.",d,"f]"),x,l,u),"NA")
  wilson<-function(k,n){
    if(length(k)!=length(n))stop("Wilson numerator and denominator lengths differ.")
    ans<-matrix(NA_real_,nrow=length(n),ncol=2,dimnames=list(NULL,c("lower","upper")))
    ok<-is.finite(n)&n>0&is.finite(k)&k>=0&k<=n
    if(any(ok)){z<-qnorm(.975);p<-k[ok]/n[ok];den<-1+z^2/n[ok]
      center<-(p+z^2/(2*n[ok]))/den;half<-z*sqrt(p*(1-p)/n[ok]+z^2/(4*n[ok]^2))/den
      ans[ok,]<-cbind(pmax(0,center-half),pmin(1,center+half))}
    ans
  }
  check_numeric<-function(y,p,t){
    if(!length(y)||anyNA(y)||any(!y%in%0:1)||!is.matrix(p)||nrow(p)!=length(y)||!ncol(p)||
      is.null(colnames(p))||anyDuplicated(colnames(p))||any(!is.finite(p))||any(p<0|p>1))stop("无效的结局或OOF概率；不静默删除患者。")
    if(!length(t)||any(!is.finite(t))||any(t<=0|t>=1)||is.unsorted(t,strictly=TRUE))stop("阈值必须在0和1之间且严格递增。")
    invisible(TRUE)
  }
  # findInterval is left-closed: risk exactly equal to threshold triggers assessment.
  suffix_counts<-function(bins,y,m){
    revsum<-function(x)rev(cumsum(rev(x)))[seq.int(2L,m+1L)]
    list(tp=revsum(tabulate(bins[y==1]+1L,nbins=m+1L)),fp=revsum(tabulate(bins[y==0]+1L,nbins=m+1L)))
  }
  threshold_counts<-function(y,p,t=plan$thresholds){
    check_numeric(y,p,t);n<-length(y);ev<-sum(y);rows<-list()
    for(model in colnames(p)){
      z<-suffix_counts(findInterval(p[,model],t),y,length(t))
      rows[[model]]<-data.frame(model=model,threshold=t,n=n,events=ev,TP=z$tp,FP=z$fp,FN=ev-z$tp,TN=n-ev-z$fp)
    }
    rows$All<-data.frame(model="All",threshold=t,n=n,events=ev,TP=ev,FP=n-ev,FN=0L,TN=0L)
    rows$None<-data.frame(model="None",threshold=t,n=n,events=ev,TP=0L,FP=0L,FN=ev,TN=n-ev)
    z<-do.call(rbind,rows);rownames(z)<-NULL
    z$alerts<-z$TP+z$FP;z$alert_fraction<-z$alerts/z$n
    z$net_benefit<-z$TP/z$n-z$FP/z$n*z$threshold/(1-z$threshold)
    for(nm in c("sensitivity","specificity","PPV","NPV")){
      numer<-switch(nm,sensitivity=z$TP,specificity=z$TN,PPV=z$TP,NPV=z$TN)
      denom<-switch(nm,sensitivity=z$events,specificity=z$n-z$events,PPV=z$alerts,NPV=z$TN+z$FN)
      ci<-wilson(numer,denom);z[[nm]]<-ifelse(denom>0,numer/denom,NA_real_)
      z[[paste0(nm,"_lower")]]<-ci[,1];z[[paste0(nm,"_upper")]]<-ci[,2]
    }
    z
  }
  bootstrap_dca<-function(y,p,t=plan$thresholds,B=plan$bootstrap_n,seed=plan$seed,progress=NULL){
    check_numeric(y,p,t);if(B<2L)stop("Bootstrap requires at least two resamples.")
    n<-length(y);m<-length(t);mods<-c(colnames(p),"All","None");w<-t/(1-t)
    bins<-lapply(seq_len(ncol(p)),function(j)findInterval(p[,j],t));names(bins)<-colnames(p)
    boot<-array(0,c(B,m,length(mods)),dimnames=list(NULL,NULL,mods))
    set.seed(seed)
    for(b in seq_len(B)){
      ii<-sample.int(n,n,replace=TRUE);yy<-y[ii]
      for(a in colnames(p)){z<-suffix_counts(bins[[a]][ii],yy,m);boot[b,,a]<-(z$tp-z$fp*w)/n}
      boot[b,,"All"]<-mean(yy)-(1-mean(yy))*w
      if(!is.null(progress)&&(b==1L||b%%250L==0L||b==B))progress(b,B)
    }
    points<-threshold_counts(y,p,t);qci<-function(mat)t(apply(mat,2,quantile,c(.025,.975),names=FALSE,type=7))
    for(a in mods){ci<-qci(boot[,,a,drop=FALSE][,,1]);ii<-points$model==a;points$nb_lower[ii]<-ci[,1];points$nb_upper[ii]<-ci[,2]}
    delta<-lapply(seq_len(nrow(comparisons)),function(j){a<-comparisons$first[j];b<-comparisons$second[j]
      if(!all(c(a,b)%in%mods))return(NULL)
      ci<-qci(boot[,,a]-boot[,,b]);za<-points[points$model==a,];zb<-points[points$model==b,]
      data.frame(comparison=comparisons$comparison[j],first=a,second=b,threshold=t,n=n,events=sum(y),
        delta_nb=za$net_benefit-zb$net_benefit,lower=ci[,1],upper=ci[,2],bootstrap_valid=B)
    })
    list(curves=points,differences=do.call(rbind,delta),bootstrap=boot,B=B,seed=seed)
  }
  risk_bands<-function(y,p){
    check_numeric(y,p,plan$risk_cutpoints)
    labels<-c("<5%","5 to <15%","15 to <30%",">=30%")
    rows<-list()
    for(a in colnames(p)){
      band<-findInterval(p[,a],plan$risk_cutpoints)+1L
      for(k in 1:4){ii<-which(band==k);n<-length(ii);ev<-sum(y[ii]);ci<-wilson(ev,n)
        rows[[length(rows)+1L]]<-data.frame(model=a,band=k,risk_band=labels[k],n=n,events=ev,
          fraction=n/length(y),observed_risk=if(n)ev/n else NA_real_,lower=ci[1,1],upper=ci[1,2],
          mean_predicted_risk=if(n)mean(p[ii,a])else NA_real_)
      }
    };do.call(rbind,rows)
  }
  validate_oof<-function(z,b,s7,expected_n=5133L,expected_events=630L){
    keys<-b$metadata$patient_key;n<-length(keys);y<-b$outcomes$event_primary
    if(n!=expected_n||sum(y)!=expected_events||anyNA(keys)||anyDuplicated(keys)||any(!nzchar(as.character(keys))))stop("冻结MIMIC_T48患者数、事件数或键不一致。")
    if(!identical(z$plan$version,"Step8_v1")||!identical(as.character(b$metadata$source),rep("MIMIC",n))||
      !identical(as.character(b$metadata$landmark),rep("T48",n)))stop("OOF来源/landmark不符合Step8 MIMIC T48约定。")
    for(obj in list(z,s7)){
      if(!identical(obj$metadata$patient_key,keys)||!identical(as.numeric(obj$outcomes$event_primary),as.numeric(y)))stop("患者顺序或结局与冻结Step6不一致。")
      if(length(obj$outer_fold)!=n||anyNA(obj$outer_fold)||!setequal(obj$outer_fold,1:5))stop("外折标记不完整。")
    }
    if(!identical(as.integer(z$outer_fold),as.integer(s7$outer_fold))||!identical(as.integer(z$ICU_state),as.integer(b$baseline$in_index_icu))||
      length(z$ICU_state)!=n||anyNA(z$ICU_state)||any(!z$ICU_state%in%0:1))stop("外折或ICU状态不一致。")
    if(!identical(colnames(z$prediction),plan$models))stop("Step8模型列不符合约定。")
    check_numeric(y,z$prediction,plan$thresholds)
    if(!is.matrix(s7$prediction)||nrow(s7$prediction)!=n||!"C"%in%colnames(s7$prediction)||any(!is.finite(s7$prediction[,"C"])))stop("Step7核心模型预测无效。")
    if(max(abs(z$prediction[,"C"]-s7$prediction[,"C"]))>1e-12)stop("Step8核心模型C未原样继承Step7。")
    invisible(TRUE)
  }
  hash_object<-function(x){p<-tempfile(fileext=".rds");on.exit(unlink(p));saveRDS(x,p,version=2);unname(tools::md5sum(p))}
  cache_result<-function(rel,key,compute){
    p<-reg(rel)
    if(file.exists(p)){z<-tryCatch(readRDS(p),error=function(e)NULL);if(is.list(z)&&isTRUE(z$complete)&&identical(z$key,key))return(z$result)}
    z<-compute();tmp<-reg(paste0(rel,".pending"));saveRDS(list(complete=TRUE,key=key,result=z),tmp,compress="gzip")
    if(!file.rename(tmp,p))stop("不能覆盖Step11缓存：",rel);z
  }
  palette<-c(I="#777777",S="#DA8C55",C="#4C7DBA",SD="#6B8E61",O="#A478A8",P="#B69B4C",CT="#7067A1",All="#555555",None="#111111")
  line_types<-c(I=3,S=2,C=1,SD=4,O=2,P=3,CT=5,All=5,None=3)
  difference_colors<-c("C-S"="#4C7DBA","SD-S"="#6B8E61","C-O"="#A478A8")
  plot_limits<-function(d,cols){z<-unlist(d[cols]);z<-z[is.finite(z)];r<-range(c(0,z));pad<-max(diff(r)*.08,.001);r+c(-pad,pad)}
  plot_dca<-function(d,models,title,xmax=.50,ylim=NULL,legend_cex=.8){
    z<-d[d$model%in%models&d$threshold<=xmax,];if(is.null(ylim))ylim<-100*plot_limits(z,c("net_benefit","nb_lower","nb_upper"))
    plot(NA,xlim=c(.01,xmax)*100,ylim=ylim,xlab="Risk threshold (%)",ylab="Net benefit per 100 patients",main=title,bty="l",las=1,cex.main=.98,xaxs="i",xaxt="n")
    axis(1,at=c(1,seq(5,xmax*100,by=5)))
    abline(h=pretty(ylim),col="#E8E8E8",lwd=.6);abline(v=c(5,15,30),col="#EEEEEE",lty=3,lwd=.7)
    for(a in models){zz<-z[z$model==a,];if(!nrow(zz))next
      if(!a%in%c("All","None"))polygon(c(zz$threshold,rev(zz$threshold))*100,c(zz$nb_lower,rev(zz$nb_upper))*100,
        col=adjustcolor(palette[a],alpha.f=.12),border=NA)
      lines(zz$threshold*100,zz$net_benefit*100,col=palette[a],lty=line_types[a],lwd=1.7)
    }
    legend("bottomleft",legend=unname(model_labels[models]),col=palette[models],lty=line_types[models],lwd=1.7,
      bty="n",cex=legend_cex,ncol=if(length(models)>4)2 else 1)
  }
  plot_delta<-function(d,title,xmax=.50,ylim=NULL){
    ids<-names(difference_colors);z<-d[d$comparison%in%ids&d$threshold<=xmax,]
    if(is.null(ylim))ylim<-100*plot_limits(z,c("delta_nb","lower","upper"))
    plot(NA,xlim=c(.01,xmax)*100,ylim=ylim,xlab="Risk threshold (%)",ylab="Net-benefit difference per 100 patients",main=title,bty="l",las=1,cex.main=.98,xaxs="i",xaxt="n")
    axis(1,at=c(1,seq(5,xmax*100,by=5)))
    abline(h=pretty(ylim),col="#E8E8E8",lwd=.6);abline(h=0,col="#222222",lty=2)
    for(j in seq_along(ids)){a<-ids[j];zz<-z[z$comparison==a,]
      polygon(c(zz$threshold,rev(zz$threshold))*100,c(zz$lower,rev(zz$upper))*100,col=adjustcolor(difference_colors[a],alpha.f=.12),border=NA)
      lines(zz$threshold*100,zz$delta_nb*100,col=difference_colors[a],lty=j,lwd=1.7)
    }
    legend("topleft",legend=c("Core dynamics - simple clinical","Clinical + dynamics - simple clinical","Core dynamics - observation process"),
      col=difference_colors,lty=1:3,lwd=1.7,bty="n",cex=.76)
  }
  make_visuals<-function(curves,differences,bands,cohorts){
    primary<-c("S","C","SD","All","None");groups<-c("All","ICU_0","ICU_1")
    subtitle<-function(g)paste0(group_labels[g]," (n=",cohorts$n[cohorts$stratum==g],"; events=",cohorts$events[cohorts$stratum==g],")")
    notes<-c("MIMIC T48; 30-day mortality after the landmark. Fixed nested out-of-fold predictions; no refitting.",
      "Bands: pointwise 95% paired patient bootstrap intervals conditional on fitted models; not simultaneous confidence bands.",
      "Assess all / none are hypothetical additional-review strategies. Thresholds are exploratory, not validated treatment triggers.")
    draw_notes<-function(lines=notes){for(j in seq_along(lines))mtext(lines[j],side=1,outer=TRUE,line=j+.4,cex=.72)}
    save_figure("Figure_4F_T48_decision_curves",function(){
      par(mfrow=c(1,2),mar=c(4.2,4.8,3.1,1.1),oma=c(5,0,3,0),mgp=c(2.8,.8,0),family="sans")
      plot_dca(curves[curves$stratum=="All",],primary,"A  Clinical comparison",xmax=.30)
      plot_delta(differences[differences$stratum=="All",],"B  Paired incremental net benefit",xmax=.30)
      mtext(paste0("48-hour decision curves: ",subtitle("All")),outer=TRUE,side=3,line=1,font=2,cex=1.1)
      draw_notes(c(notes[1:2],"Shown: 1-30% thresholds; the full 1-50% range is in the supplementary figure. Hypothetical review, not treatment triggers."))
    },12,6.2)
    save_figure("Figure_S_T48_DCA_full_range",function(){
      par(mfrow=c(1,3),mar=c(4.2,4.8,3.1,1),oma=c(5,0,3,0),mgp=c(2.8,.8,0))
      ylim<-100*plot_limits(curves[curves$model%in%primary,],c("net_benefit","nb_lower","nb_upper"))
      for(g in groups)plot_dca(curves[curves$stratum==g,],primary,paste0(g,"; n=",cohorts$n[cohorts$stratum==g]),ylim=ylim,legend_cex=.67)
      mtext("Clinical comparison: complete 1-50% threshold range",outer=TRUE,side=3,line=1,font=2,cex=1.1)
      draw_notes(c(notes[1:2],"ICU_0: out of index ICU, still hospitalized. ICU_1: in index ICU. Assess all / none: hypothetical additional review."))
    },14,6.2)
    save_figure("Figure_S_T48_DCA_ICU_strata",function(){
      par(mfrow=c(2,2),mar=c(4.2,4.8,3.1,1.1),oma=c(5,0,3,0),mgp=c(2.8,.8,0))
      z<-curves[curves$stratum!="All"&curves$threshold<=.30&curves$model%in%primary,]
      yl<-100*plot_limits(z,c("net_benefit","nb_lower","nb_upper"))
      zd<-differences[differences$stratum!="All"&differences$threshold<=.30&differences$comparison%in%names(difference_colors),]
      yd<-100*plot_limits(zd,c("delta_nb","lower","upper"))
      for(g in groups[-1])plot_dca(curves[curves$stratum==g,],primary,subtitle(g),xmax=.30,ylim=yl,legend_cex=.73)
      for(g in groups[-1])plot_delta(differences[differences$stratum==g,],paste0(g,": paired increments"),xmax=.30,ylim=yd)
      mtext("48-hour decision curves by ICU status",outer=TRUE,side=3,line=1,font=2,cex=1.1);draw_notes(notes)
    },12,10)
    save_figure("Figure_S_T48_DCA_observation_process",function(){
      par(mfrow=c(1,3),mar=c(4.2,4.8,3.1,1),oma=c(5,0,3,0),mgp=c(2.8,.8,0))
      mods<-c("C","O","P","All","None");z<-curves[curves$threshold<=.30&curves$model%in%mods,]
      yl<-100*plot_limits(z,c("net_benefit","nb_lower","nb_upper"))
      for(g in groups)plot_dca(curves[curves$stratum==g,],mods,paste0(g,"; n=",cohorts$n[cohorts$stratum==g]),xmax=.30,ylim=yl,legend_cex=.65)
      mtext("Physiology and observation-process comparison",outer=TRUE,side=3,line=1,font=2,cex=1.1)
      draw_notes(c(notes[1:2],"P reduces selected observation cues; it does not eliminate measurement-process effects. Hypothetical additional review."))
    },14,6.2)
    save_figure("Figure_S_T48_fixed_risk_bands",function(){
      par(mfrow=c(2,3),mar=c(4.5,4.8,3.1,1),oma=c(4,0,3,0),mgp=c(2.8,.8,0))
      mods<-c("S","C","SD");z<-bands[bands$stratum=="All"&bands$model%in%mods,]
      yl<-c(0,max(c(z$upper,z$mean_predicted_risk),na.rm=TRUE)*100*1.1)
      for(a in mods){q<-z[z$model==a,];x<-1:4;plot(x,100*q$observed_risk,type="n",ylim=yl,xaxt="n",xlab="Fixed predicted-risk band",ylab="30-day event risk (%)",bty="l",las=1,main=model_labels[a],cex.main=1)
        abline(h=pretty(yl),col="#E8E8E8",lwd=.6);axis(1,x,labels=q$risk_band,cex.axis=.8)
        segments(x-.06,100*q$lower,x-.06,100*q$upper,col=palette[a],lwd=1.2)
        points(x-.06,100*q$observed_risk,pch=16,col=palette[a]);points(x+.06,100*q$mean_predicted_risk,pch=1,col="#222222")
        legend("topleft",c("Observed (95% Wilson CI)","Mean predicted"),pch=c(16,1),col=c(palette[a],"#222222"),bty="n",cex=.75)
      }
      for(a in mods){q<-z[z$model==a,];bp<-barplot(100*q$fraction,ylim=c(0,100),col=palette[a],border=NA,names.arg=q$risk_band,
        cex.names=.8,ylab="Patients in band (%)",xlab="Fixed predicted-risk band",las=1)
        text(bp,100*q$fraction+5,labels=paste0("n=",q$n),cex=.78)}
      mtext("Fixed risk bands: all eligible MIMIC T48 patients",outer=TRUE,side=3,line=1,font=2,cex=1.1)
      draw_notes(c("Bands: <5%, 5 to <15%, 15 to <30%, >=30%; exact cutpoints enter the higher band. Empty bands are retained.",
        "Descriptive display cutpoints, not validated clinical action thresholds. Outcomes are not used to select cutpoints."))
    },13,9)
    tables<-list();perf<-list();risk<-list();inc<-list()
    for(g in groups){
      z<-curves[curves$stratum==g&curves$model%in%c("S","C","SD")&curves$threshold%in%plan$display_thresholds,]
      z<-z[order(z$threshold,match(z$model,c("S","C","SD"))),]
      tables[[subtitle(g)]]<-data.frame(Model=z$model,`Threshold %`=pct(z$threshold),TP=z$TP,FP=z$FP,FN=z$FN,TN=z$TN,
        `NB [95% CI]`=fmt_ci(z$net_benefit,z$nb_lower,z$nb_upper),check.names=FALSE)
      perf[[subtitle(g)]]<-data.frame(Model=z$model,`Threshold %`=pct(z$threshold),`Alert %`=pct(z$alert_fraction),
        `Sens. %`=pct(z$sensitivity),`Spec. %`=pct(z$specificity),`PPV %`=pct(z$PPV),`NPV %`=pct(z$NPV),check.names=FALSE)
      q<-bands[bands$stratum==g&bands$model%in%c("S","C","SD"),]
      risk[[subtitle(g)]]<-data.frame(Model=q$model,Band=q$risk_band,N=q$n,Events=q$events,`Share %`=pct(q$fraction),
        `Observed % [95% CI]`=fmt_ci(100*q$observed_risk,100*q$lower,100*q$upper,1),`Predicted %`=pct(q$mean_predicted_risk),check.names=FALSE)
      for(scope in c("Model comparisons","Default-strategy comparisons")){
        q<-differences[differences$stratum==g&differences$threshold%in%plan$display_thresholds&
          if(scope=="Model comparisons")!differences$second%in%c("All","None")else differences$second%in%c("All","None"),]
        inc[[paste(g,scope,sep="; ")]]<-data.frame(Comparison=q$comparison,`Threshold %`=pct(q$threshold),
          `Delta NB [95% CI]`=fmt_ci(q$delta_nb,q$lower,q$upper),`Valid bootstrap`=q$bootstrap_valid,check.names=FALSE)
      }
    }
    # Every supplementary table is multipage, explicitly onefile=TRUE and page-count checked.
    all_thresholds<-list();for(g in names(tables)){all_thresholds[[paste(g,"counts / net benefit",sep="; ")]]<-tables[[g]];all_thresholds[[paste(g,"classification metrics",sep="; ")]]<-perf[[g]]}
    three_line(all_thresholds,"Supplementary table. T48 fixed assessment thresholds","Table_S_T48_decision_thresholds",
      c("S: simple clinical; C: core dynamics; SD: clinical + dynamics. All use independent outer-fold predictions; no subgroup refitting.",
        "Trigger: predicted risk >= threshold. NB = TP/n - FP/n * threshold/(1-threshold), in per-patient units.",
        "NB intervals: 2000 paired patient bootstrap samples, conditional on fixed models. Classification Wilson intervals are in source CSV.",
        "ICU_0: out of index ICU but still hospitalized. Thresholds are display choices, not validated treatment triggers."))
    three_line(inc,"Supplementary table. Paired incremental net benefit","Table_S_T48_DCA_paired_increment",
      c("Positive differences favor the first strategy. C: core dynamics; S: simple clinical; SD: clinical + dynamics; O: observation process.",
        "P: reduced observation cues; CT: extended clinical. All / None: hypothetical additional-review strategies, not treatment regimens.",
        "Pointwise 95% percentile intervals from 2000 shared patient resamples. No refitting, threshold selection or simultaneous inference."))
    three_line(risk,"Supplementary table. Fixed predicted-risk bands","Table_S_T48_fixed_risk_bands",
      c("Events: all-cause death within 30 days after T48. Observed proportions use 95% Wilson intervals; not model-training uncertainty.",
        "Exact cutpoints enter the higher band. Empty bands retain n=0 and undefined risks. No outcome-derived band boundaries.",
        "S: simple clinical; C: core dynamics; SD: clinical + dynamics. Predicted % is the mean fixed OOF risk within each band."))
  }
  verify_lineage<-function(step){
    root<-file.path(cfg$OUTPUT_PARENT,step);sp<-file.path(root,"to_share",paste0(step,"_STATUS.txt"))
    if(!file.exists(sp)||!"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(sp,warn=FALSE))stop("请先完成",step,"。")
    ap<-file.path(root,"to_share/01_inputs.csv");if(!file.exists(ap))stop("缺少上游输入审计：",step)
    lin<-utils::read.csv(ap,stringsAsFactors=FALSE)
    if(!all(c("file","md5")%in%names(lin))||!nrow(lin)||anyNA(lin$file)||anyNA(lin$md5))stop("上游输入审计无效。")
    for(i in seq_len(nrow(lin)))if(!file.exists(lin$file[i])||unname(tools::md5sum(lin$file[i]))!=lin$md5[i])stop(step,"输入已改变，请先重跑该步骤。")
    invisible(TRUE)
  }
  reconcile_step8<-function(curves,step8){
    audit<-list()
    for(g in c("All","ICU_0","ICU_1")){
      oldscope<-switch(g,All="all",ICU_0="ICU0",ICU_1="ICU1")
      path<-file.path(step8,"to_share",paste0("06_",oldscope,"_threshold_counts.csv"))
      if(!file.exists(path))stop("缺少Step8阈值汇总：",basename(path))
      expected_models<-if(g=="All")plan$models else setdiff(plan$models,"I")
      a<-read.csv(path,stringsAsFactors=FALSE);b<-curves[curves$stratum==g&curves$model%in%expected_models&curves$threshold%in%plan$display_thresholds,]
      if(nrow(a)!=nrow(b)||anyDuplicated(paste(a$model,a$threshold)))stop("Step8阈值行数不一致。")
      ix<-match(paste(a$model,a$threshold),paste(b$model,b$threshold));if(anyNA(ix))stop("Step8阈值集合不一致。")
      b<-b[ix,];oldcols<-c("n","events","tp","fp","fn","tn");newcols<-c("n","events","TP","FP","FN","TN")
      for(j in seq_along(oldcols))if(anyNA(a[[oldcols[j]]])||!identical(as.numeric(a[[oldcols[j]]]),as.numeric(b[[newcols[j]]])))stop("Step8计数重现不一致：",g,"/",oldcols[j])
      audit[[g]]<-data.frame(stratum=g,matched_rows=nrow(a),mismatch_n=0L)
    };do.call(rbind,audit)
  }
  main<-function(){
    logmsg("小型预检：阈值同值边界、净获益公式、配对重抽样、空风险档、无预测阳性及多页PDF页数。")
    current_module<<-"small synthetic preflight";preflight()
    txt_out("PASS: direct vs binned TP/FP; ties at threshold; paired bootstrap vs independent brute-force implementation; null strategies; absent-class resamples; empty risk bands; OOF identity guards; multi-page PDF.","to_share/02_preflight.txt")
    current_module<<-"locked input and lineage verification"
    logmsg("读取Step8锁定T48折外预测，核对Step6患者/结局和Step7核心模型；不扫描原始数据库，不拟合模型。")
    for(step in c("Step6","Step7","Step8"))verify_lineage(step)
    paths<-c(bundle=file.path(cfg$OUTPUT_PARENT,"Step6/local_only/MIMIC_T48_model_inputs.rds"),
      step7_oof=file.path(cfg$OUTPUT_PARENT,"Step7/local_only/MIMIC_T48_nested_OOF.rds"),
      step8_oof=file.path(cfg$OUTPUT_PARENT,"Step8/local_only/MIMIC_T48_comparator_OOF.rds"),
      model_definitions=file.path(cfg$OUTPUT_PARENT,"Step8/to_share/02_model_definitions.csv"))
    oldtables<-file.path(cfg$OUTPUT_PARENT,"Step8/to_share",paste0("06_",c("all","ICU0","ICU1"),"_threshold_counts.csv"))
    if(!all(file.exists(c(paths,oldtables))))stop("Step6/7/8本地输入不完整。")
    input_files<-c(unname(paths),oldtables);input_hash<-unname(tools::md5sum(input_files))
    csv_out(data.frame(file=input_files,md5=input_hash),"to_share/01_inputs.csv")
    b<-readRDS(paths["bundle"]);s7<-readRDS(paths["step7_oof"]);z<-readRDS(paths["step8_oof"])
    validate_oof(z,b,s7);defs<-read.csv(paths["model_definitions"],stringsAsFactors=FALSE)
    if(!identical(defs$model,plan$models))stop("Step8模型定义顺序不一致。")
    csv_out(defs,"to_share/02_model_definitions.csv")
    y<-as.integer(z$outcomes$event_primary);p<-z$prediction;icu<-z$ICU_state
    groups<-list(All=seq_along(y),ICU_0=which(icu==0L),ICU_1=which(icu==1L))
    cohorts<-do.call(rbind,lapply(names(groups),function(g){ii<-groups[[g]];data.frame(stratum=g,n=length(ii),events=sum(y[ii]),event_fraction=mean(y[ii]))}))
    if(!identical(cohorts$n,c(5133L,2246L,2887L))||!identical(as.numeric(cohorts$events),c(630,128,502)))stop("MIMIC T48 ICU分层人数/事件数与Step8不一致。")
    csv_out(cohorts,"to_share/02_cohort_check.csv")
    csv_out(do.call(rbind,lapply(1:5,function(o)data.frame(outer_fold=o,n=sum(z$outer_fold==o),events=sum(y[z$outer_fold==o]),
      max_C_difference=max(abs(z$prediction[z$outer_fold==o,"C"]-s7$prediction[z$outer_fold==o,"C"]))))),"to_share/02_OOF_roundtrip.csv")
    txt_out(capture.output(dput(plan)),"to_share/02_analysis_plan.txt")
    curves<-differences<-bands<-list();key<-hash_object(list(plan=plan,comparisons=comparisons,input_hash=input_hash))
    current_module<<-"paired fixed-OOF bootstrap"
    for(g in names(groups)){
      ii<-groups[[g]];logmsg(paste0(g,"：",length(ii),"人，",sum(y[ii]),"个事件；计算或复用已核对的配对净获益缓存。"))
      ans<-cache_result(paste0("local_only/cache/",g,"_DCA.rds"),paste0(key,"_",g),function()
        bootstrap_dca(y[ii],p[ii,,drop=FALSE],seed=plan$seed+match(g,names(groups)),progress=function(k,B)logmsg(paste0(g," 配对重抽样：",k,"/",B,"。"))))
      if(ans$B!=plan$bootstrap_n||any(!is.finite(ans$bootstrap)))stop("Bootstrap缓存不完整。")
      curves[[g]]<-cbind(stratum=g,ans$curves);differences[[g]]<-cbind(stratum=g,ans$differences)
      bands[[g]]<-cbind(stratum=g,risk_bands(y[ii],p[ii,,drop=FALSE]))
    }
    curves<-do.call(rbind,curves);differences<-do.call(rbind,differences);bands<-do.call(rbind,bands)
    csv_out(reconcile_step8(curves,file.path(cfg$OUTPUT_PARENT,"Step8")),"to_share/02_Step8_threshold_reconciliation.csv")
    csv_out(curves,"to_share/03_DCA_all_thresholds.csv")
    csv_out(differences,"to_share/04_DCA_paired_differences.csv")
    csv_out(curves[curves$threshold%in%plan$display_thresholds,],"to_share/05_fixed_threshold_metrics.csv")
    csv_out(bands,"to_share/06_fixed_risk_bands.csv")
    csv_out(data.frame(stratum=names(groups),bootstrap_n=plan$bootstrap_n,valid_n=plan$bootstrap_n,
      seed=plan$seed+seq_along(groups),method=plan$interval),"to_share/04_bootstrap_audit.csv")
    current_module<<-"figures and three-line tables"
    logmsg("核对Step8既有阈值计数；输出决策曲线、配对增量、固定风险档及多页三线表。")
    make_visuals(curves,differences,bands,cohorts)
    csv_out(data.frame(output=c("Figure_4F_T48_decision_curves","Figure_S_T48_DCA_full_range","Figure_S_T48_DCA_ICU_strata", "Figure_S_T48_DCA_observation_process","Figure_S_T48_fixed_risk_bands","Table_S_T48_decision_thresholds","Table_S_T48_DCA_paired_increment","Table_S_T48_fixed_risk_bands"),
      action=c("REPLACE Figure 4F",rep("ADD supplementary; final S number pending",7)),
      detail=c("Two panels: clinical DCA and paired increments; 1-30% display; new nested OOF analysis",
        "Complete exploratory 1-50% range, same scale and all negative values retained","Same locked OOF risks split by ICU status; no subgroup refitting",
        "Core physiology / observation process / reduced observation cues", "Four fixed display bands; no optimal cutpoint search",
        "Threshold counts, net benefit and classification; full machine-readable intervals in CSV", "Paired model and default-strategy differences", "Band counts, events, mean risk and observed Wilson intervals")),"to_share/11_figure_table_replacement_map.csv")
    txt_out(c("Vickers AJ, Elkin EB. Decision curve analysis: a novel method for evaluating prediction models. Med Decis Making. 2006;26:565-574. doi:10.1177/0272989X06295361.",
      "https://pmc.ncbi.nlm.nih.gov/articles/PMC2577036/", "Van Calster B, et al. Reporting and Interpreting Decision Curve Analysis: A Guide for Investigators. Eur Urol. 2018;74:796-804. doi:10.1016/j.eururo.2018.08.038.",
      "https://pmc.ncbi.nlm.nih.gov/articles/PMC6261531/"),"to_share/10_method_sources.txt")
    txt_out(c("Step11：MIMIC T48锁定折外预测的决策曲线、固定阈值和风险分层。",
      "复用Step8 I/S/C/SD/O/P/CT预测；C逐患者核对Step7。Step6/7/8来源MD5、患者顺序、结局、外折和ICU状态一致。",
      "冻结总体5133人/630事件；ICU_0为已离开index ICU但仍住院2246人/128事件；ICU_1为仍在index ICU2887人/502事件。",
      "本步不重新拟合模型、重校准、重新分折、扫描原始大表或使用eICU。T6/T24不在本步混入T48决策分析。",
      "NB = TP/n - FP/n * t/(1-t)。风险>=t触发额外评估；None为0，All为事件率-(1-事件率)*t/(1-t)。",
      "临床场景是48小时病情再评估时，假设按预测风险触发额外临床复核/监测评估；并非模型决定PCI/MCS或治疗。",
      "1%-50%为本次固定的探索范围，不声称已有临床认可；Figure4F显示1%-30%，补充图完整显示1%-50%。",
      "5%、15%、30%沿用既有展示切点；不将其重新包装为前瞻预设或已经验证的治疗阈值。不扫描寻找最优阈值。",
      "每个阈值均导出TP/FP/FN/TN/人数/事件数/NB/告警比例/敏感度/特异度/PPV/NPV及区间。无分母时为NA而不是0。",
      "每个ICU层内独立重抽患者2000次；同一重复对全部模型、阈值和默认策略使用相同患者。事件率随抽样变化，不固定病例/非病例数量。",
      "95%区间采用百分位法；为条件于已拟合模型的点态不确定性，不是全流程重训区间或同时置信带；不据多个阈值宣称显著获益范围。",
      "分类指标及风险档观察风险区间使用Wilson法。风险档<5%、[5,15)%、[15,30)%、>=30%；边界归较高档，空档保留。",
      "阴性/阳性预测价值分母为实际触发/未触发人数；没有触发者时PPV不可估。各模型分层人群不同，不把同名风险档当作配对患者。",
      "不把净获益/100翻译成已避免的死亡。未测量实施成本、评估负担或临床干预效果，不能据此宣称改善结局或可部署。",
      "P仅减少部分观察过程线索，不能证明消除了选择性测量；CT在MIMIC中不含可估计的PCI/CABG调整效果。",
      "Figure_4F替换原Figure4F；其余图表新增补充材料，最终S编号待整稿。Figure4G另行处理。",
      "图表来自同一CSV；三线表显式多页且检查页数。命名不加时间戳，重跑覆盖登记输出，保留有效bootstrap缓存及用户其他文件。",
      "反馈包仅汇总结果；患者级预测留在既有local_only，bootstrap缓存亦在local_only。"),"to_share/00_READ_ME_FIRST.txt")
    # Read-only upstream contract: a successful run must leave all direct inputs unchanged.
    if(!identical(input_hash,unname(tools::md5sum(input_files))))stop("运行中上游输入发生改变，本次结果不能使用。")
    status<<-"COMPLETED_FOR_REVIEW"
  }
  preflight<-function(){
    y<-c(1,0,1,0,0,1,0,1);pr<-c(0,.05,.15,.30,1,.05,.15,.30)
    p<-cbind(S=pr,C=rev(pr),SD=pr,O=rep(.1,8),P=rep(0,8),CT=rep(1,8));t<-c(.05,.15,.30,.50)
    z<-threshold_counts(y,p,t)
    for(a in colnames(p))for(tt in t){q<-z[z$model==a&z$threshold==tt,];pos<-p[,a]>=tt
      stopifnot(q$TP==sum(pos&y==1),q$FP==sum(pos&y==0),q$FN==sum(!pos&y==1),q$TN==sum(!pos&y==0),
        abs(q$net_benefit-(sum(pos&y==1)-sum(pos&y==0)*tt/(1-tt))/length(y))<1e-14)}
    stopifnot(all(z$TP+z$FP+z$FN+z$TN==length(y)),all(z$net_benefit[z$model=="None"]==0),
      all(is.na(z$PPV[z$model=="P"])),all(is.na(z$NPV[z$model=="CT"])))
    # Independently enumerate patients, never use the suffix-count helper in the oracle.
    B<-31L;seed<-77L;bs<-bootstrap_dca(y,p,t,B=B,seed=seed);set.seed(seed)
    ref<-array(0,dim(bs$bootstrap),dimnames=dimnames(bs$bootstrap))
    for(b in seq_len(B)){ii<-sample.int(length(y),length(y),replace=TRUE)
      for(a in colnames(p))for(j in seq_along(t)){
        pos<-p[ii,a]>=t[j];ref[b,j,a]<-mean(pos&y[ii]==1)-mean(pos&y[ii]==0)*t[j]/(1-t[j])}
      ref[b,,"All"]<-mean(y[ii])-(1-mean(y[ii]))*t/(1-t)
    }
    stopifnot(max(abs(ref-bs$bootstrap))<1e-14,all(bs$differences$delta_nb[bs$differences$comparison=="SD-S"]==0),
      all(bs$differences$lower[bs$differences$comparison=="SD-S"]==0),all(bs$differences$upper[bs$differences$comparison=="SD-S"]==0))
    rb<-risk_bands(y,p);stopifnot(identical(rb$n[rb$model=="S"],c(1L,2L,2L,3L)),
      sum(rb$n[rb$model=="P"]==0L)==3L,all(is.na(rb$observed_risk[rb$n==0L])))
    for(yy in list(rep(0L,8),rep(1L,8)))stopifnot(all(is.finite(bootstrap_dca(yy,p,t,B=5L)$bootstrap)))
    bad<-p;bad[1,1]<-NA_real_;stopifnot(inherits(try(threshold_counts(y,bad,t),silent=TRUE),"try-error"))
    # Patient identity guards must reject shuffled OOF keys and a changed C prediction.
    pp<-cbind(I=rep(.5,8),p)[,plan$models];meta<-data.frame(patient_key=paste0("test",1:8),source="MIMIC",landmark="T48")
    b<-list(metadata=meta,outcomes=data.frame(event_primary=y),baseline=data.frame(in_index_icu=rep(0:1,4)))
    zz<-list(metadata=meta,outcomes=b$outcomes,outer_fold=c(1:5,1:3),ICU_state=b$baseline$in_index_icu,prediction=pp,plan=list(version="Step8_v1"))
    s7<-zz;s7$prediction<-pp[,"C",drop=FALSE];validate_oof(zz,b,s7,8L,4L)
    bad<-zz;bad$metadata$patient_key<-rev(bad$metadata$patient_key)
    stopifnot(inherits(try(validate_oof(bad,b,s7,8L,4L),silent=TRUE),"try-error"))
    bad<-zz;bad$prediction[1,"C"]<-.99;stopifnot(inherits(try(validate_oof(bad,b,s7,8L,4L),silent=TRUE),"try-error"))
    pp<-tempfile(fileext=".pdf");on.exit(unlink(pp),add=TRUE)
    draw_table_pdf(list("Page one"=data.frame(Item="alpha",N=1),"Page two"=data.frame(Item="beta",N=2)),"Synthetic multipage preflight",pp)
    stopifnot(pdf_pages(pp)==2L)
    invisible(TRUE)
  }
  if(isTRUE(getOption("ami_step11.functions_only",FALSE)))return(invisible(list(preflight=preflight,counts=threshold_counts,bootstrap=bootstrap_dca,risk_bands=risk_bands,validate_oof=validate_oof,make_visuals=make_visuals,main=main,pdf_pages=pdf_pages,three_line=three_line)))
  status<-"FAILED";current_module<-"input verification"
  failure_message<-NULL
  tryCatch(withCallingHandlers(main(),warning=function(w){add_warning(conditionMessage(w),current_module);invokeRestart("muffleWarning")}),error=function(err){status<<-"FAILED";failure_message<<-conditionMessage(err)
    issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step11_error_detail.txt")})
  flush_warnings()
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step11",
    "COMPLETED_FOR_REVIEW表示锁定OOF决策曲线、固定阈值及风险分层已生成，等待结果核对；未重新拟合模型。",
    "FAILED表示不完整，不能使用本次患者级结果建模。"),"to_share/Step11_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step11_sessionInfo.txt")
  logmsg(paste0("Step11状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step11_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share) else {
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step11_feedback.zip"),"\n",sep="") else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
