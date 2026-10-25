# AMI-EDRS revision | Step10: frozen phenotype profiles and biomarker availability
# source(file.choose(), encoding="UTF-8")
# Fixed filenames; replaces owned results. Patient-level data and frozen references stay local_only.
local({
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MAKE_PNG=TRUE,MAKE_ZIP=TRUE,REPAIR_PRIOR_TABLES=TRUE)
  cfg<-utils::modifyList(cfg,getOption("ami_step10.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step10")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step10输出目录。")
  if (is_link(out)) stop("Step10输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP10")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step10目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP10_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP10_MANAGED_OUTPUT")) {
    stop("Step10输出目录的归属标记不正确。")
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
        if (unlink(p) != 0L) stop("不能覆盖旧Step10输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step10_run_log.txt")
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
  engine<-NULL;predict_step7<-NULL
  domains<-c("hemodynamic","respiratory_neurologic","renal_metabolic","inflammatory_coagulopathic")
  labels<-c(hemodynamic="Hemodynamic",respiratory_neurologic="Respiratory / neurologic",renal_metabolic="Renal / metabolic",inflammatory_coagulopathic="Inflammatory / coagulopathic",cardiac_hypoperfusion="Cardiac / hypoperfusion (exploratory)")
  expected<-data.frame(tag=c("MIMIC_T6","MIMIC_T24","MIMIC_T48","eICU_T48"),n=c(5513L,5411L,5133L,5284L),events=c(825L,738L,630L,219L))
  plan<-list(version="Step10_v1",profile_scope="full MIMIC frozen fit, descriptive only",main_domains=domains,
    reference="Step7 frozen 0-6h reference",cumulative=c("0-6h","0-24h","0-48h"),nonoverlap=c("0-6h","6-24h","24-48h"),
    paired_change="24-48h minus 0-6h, patients observed in both windows",statistics="median and IQR; no confidence interval or causal trajectory claim",
    fold_labels="separate per outer fold, no pooled labels or outcome ordering",biomarkers=c("lactate","troponin_t"))
  warning_rows<-list()
  add_warning<-function(msg,scope){warning_rows[[length(warning_rows)+1L]]<<-data.frame(scope=scope,message=msg,stringsAsFactors=FALSE)}
  flush_warnings<-function(){
    z<-if(length(warning_rows))do.call(rbind,warning_rows)else data.frame(scope=character(),message=character())
    txt_out(if(nrow(z))unlist(Map(function(a,b)c(a,b,""),z$scope,z$message))else"No warnings in Step10.","local_only/Step10_warning_detail.txt")
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
      xs<-c(.04,seq(.44,.96,length.out=nc-1));top<-.85;dy<-min(.052,.54/max(1,nr))
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
      rows<-rbind(names(tables[[nm]]),as.matrix(tables[[nm]]));nc<-ncol(rows);ends<-round(c(4400,seq(6900,15100,length.out=nc-1)))
      for(i in seq_len(nrow(rows))){bd<-paste0(if(i==1)"\\clbrdrt\\brdrs\\brdrw15\\clbrdrb\\brdrs\\brdrw10"else"",if(i==nrow(rows))"\\clbrdrb\\brdrs\\brdrw15"else"")
        rt<-c(rt,paste0("\\trowd\\trgaph60\\trkeep",paste0(bd,"\\cellx",ends,collapse=""),paste(vapply(seq_len(nc),function(j)paste0("\\pard\\intbl",if(j==1)"\\ql "else"\\qr ","\\fs16 ",if(i==1)"\\b "else"\\b0 ",esc(rows[i,j]),"\\cell"),character(1)),collapse=""),"\\row"))}
      rt<-c(rt,paste0("\\pard\\fs16 ",paste(esc(notes),collapse="\\par "),"\\par"))
    };txt_out(c(rt,"}"),paste0("to_share/",stem,".rtf"))
  }
  prior_table_specs<-function(){list(
    list(step="Step6",stem="Table_S_domain_component_availability",csv="Table_S_domain_component_availability.csv",title="Supplementary table. Raw domain component availability",sub=c("Cumulative windows","Non-overlapping windows"),
      notes=c("T48 cohorts: MIMIC n=5133; eICU n=5284. Values are n (% of the full cohort). Minimum is observed / specified components.","At least half the specified components must be observed. No imputation. Raw availability may exceed fitted score availability.")),
    list(step="Step7",stem="Table_S_T48_nested_validation",csv=c("Table_S_T48_nested_performance.csv","Table_S_T48_paired_increment.csv"),title="Supplementary table. T48 four-model internal validation",sub=c("Nested OOF performance and 95% CI","Paired differences on the same patients"),
      notes=c("A: baseline; B: baseline + phenotype; C: baseline + dynamics; D: baseline + dynamics + phenotype.","AP: average precision. Intervals use fixed-OOF patient bootstrap, not full pipeline redevelopment.")),
    list(step="Step8",stem="Table_S_T48_clinical_comparators",csv=c("Table_S_T48_clinical_performance.csv","Table_S_T48_clinical_increment.csv"),title="Supplementary table. Clinical and observation-process comparators",sub=c("Nested OOF performance and 95% CI","Paired differences on the same patients"),
      notes=c("I: ICU state; S: simple clinical; C: core dynamics; SD: S + dynamics; O: observation process; P: reduced observation cues; CT: extended clinical.","AP: average precision. Fixed-OOF patient bootstrap. PCI/CABG were unavailable in MIMIC; CT does not adjust for revascularization.")),
    list(step="Step8",stem="Table_S_T48_ICU_strata",csv=c("Table_S_T48_ICU0_performance.csv","Table_S_T48_ICU1_performance.csv"),title="Supplementary table. T48 performance by ICU state",sub=c("Out of index ICU, still in hospital: n=2246; events=128","In index ICU: n=2887; events=502"),
      notes=c("All patients remained in hospital at T48. Original OOF predictions were used without subgroup refitting.","AP: average precision. 95% intervals condition on the fitted outer-fold models.")),
    list(step="Step9",stem="Table_S_early_landmark_validation",csv=c("Table_S_early_landmark_cohorts.csv","Table_S_early_landmark_performance.csv","Table_S_early_landmark_increment.csv"),title="Supplementary table. Early landmark internal validation",sub=c("Separate landmark populations","Model performance and 95% CI","Within-landmark paired C-A differences"),
      notes=c("A: age, sex and ICU state. C: A plus physiology available at the landmark. AP: average precision.","95% intervals use fixed-OOF patient bootstrap. Different landmarks have different populations and 30-day intervals; no cross-landmark paired test."))
  )}
  prior_owned<-function(step,rel){
    root<-file.path(cfg$OUTPUT_PARENT,step);p<-file.path(root,rel)
    if(is_link(root)||is_link(file.path(root,dirname(rel)))||is_link(p))stop("旧输出路径含符号链接。")
    mark<-file.path(root,paste0(".AMI_EDRS_",toupper(step)));manifest<-file.path(root,".generated_files.txt")
    if(!file.exists(mark)||!identical(readLines(mark,warn=FALSE),paste0("AMI_EDRS_",toupper(step),"_MANAGED_OUTPUT"))||!file.exists(manifest)||!rel%in%readLines(manifest,warn=FALSE))stop("不能覆盖未登记的旧输出：",step,"/",rel)
    p
  }
  repair_prior_tables<-function(){
    specs<-prior_table_specs();prepared<-list();audit<-list()
    # Verify and stage every repair before replacing any previous table.
    for(s in specs){
      target<-prior_owned(s$step,paste0("to_share/",s$stem,".pdf"));paths<-vapply(s$csv,function(n)prior_owned(s$step,paste0("to_share/",n)),character(1))
      if(!all(file.exists(c(target,paths))))stop("修复三线表的汇总来源缺失：",s$step)
      tabs<-lapply(paths,utils::read.csv,stringsAsFactors=FALSE,check.names=FALSE)
      if(s$step=="Step6"){z<-tabs[[1]];tabs<-lapply(c("cumulative","nonoverlap"),function(t)z[z$window_type==t,setdiff(names(z),"window_type"),drop=FALSE])}
      names(tabs)<-s$sub;pending<-reg(paste0("local_only/",s$step,"_",s$stem,".pending.pdf"));draw_table_pdf(tabs,s$title,pending,s$notes)
      prepared[[length(prepared)+1L]]<-list(target=target,pending=pending)
      audit[[length(audit)+1L]]<-data.frame(step=s$step,file=paste0(s$stem,".pdf"),old_pages=pdf_pages(target),old_pages_note="NA means compressed old PDF page objects were not countable without an external parser",expected_pages=length(tabs),new_pages=pdf_pages(pending),source_csv_md5=paste(unname(tools::md5sum(paths)),collapse=";"))
    }
    for(z in prepared)if(!file.rename(z$pending,z$target))stop("旧PDF覆盖失败，请关闭占用文件的程序：",basename(z$target))
    if(cfg$MAKE_ZIP)for(step in unique(vapply(specs,`[[`,character(1),"step"))){
      root<-file.path(cfg$OUTPUT_PARENT,step);target<-prior_owned(step,paste0(step,"_feedback.zip"));pending<-reg(paste0("local_only/",step,"_feedback.pending.zip"))
      rel<-readLines(file.path(root,".generated_files.txt"),warn=FALSE);files<-basename(rel[startsWith(rel,"to_share/")&file.exists(file.path(root,rel))]);share<-file.path(root,"to_share")
      if(requireNamespace("zip",quietly=TRUE))zip::zipr(pending,files=files,root=share)else{wd<-getwd();tryCatch({setwd(share);utils::zip(pending,files=files,flags="-q")},finally=setwd(wd))}
      if(!file.exists(pending)||file.info(pending)$size==0||!file.rename(pending,target))stop("旧反馈包更新失败：",step)
    }
    ans<-do.call(rbind,audit);csv_out(ans,"to_share/00_repaired_pdf_pages.csv");ans
  }
  assign_profile<-function(raw,state){
    if(!identical(state$version,"Step7_v1")||!setequal(state$score_columns,unlist(lapply(plan$cumulative,function(w)paste(domains,gsub("-","_",w,fixed=TRUE),"score",sep="__")))))stop("冻结分型不是约定的四域累计窗口。")
    sc<-engine$apply_reference(raw,state$reference,engine$windows_for("T48"),min_fraction=.5)$scores
    xx<-engine$apply_preprocessor(sc[state$score_columns],state$cluster_preprocessor);centers<-state$cluster$centers
    if(!identical(colnames(xx),colnames(centers))||any(!is.finite(xx))||any(!is.finite(centers)))stop("冻结中心输入不一致。")
    dist2<-outer(rowSums(xx*xx),rowSums(centers*centers),"+")-2*tcrossprod(xx,centers);dist2[dist2<0]<-0
    lab<-max.col(-dist2,ties.method="first");distance<-sqrt(dist2[cbind(seq_len(nrow(xx)),lab)])
    list(scores=sc,phenotype=lab,distance=distance,k=nrow(centers))
  }
  summarize_profile<-function(a,scope){
    rows<-list()
    for(typ in c("cumulative","nonoverlap"))for(w in plan[[typ]])for(d in domains)for(k in seq_len(a$k)){
      ii<-which(a$phenotype==k);x<-a$scores[[paste(d,gsub("-","_",w,fixed=TRUE),"score",sep="__")]][ii]
      rows[[length(rows)+1L]]<-data.frame(scope=scope,phenotype=paste0("C",k),window_type=typ,window=w,domain=d,
        group_n=length(ii),observed_n=sum(is.finite(x)),missing_n=sum(!is.finite(x)),as.list(quant(x)))
    };do.call(rbind,rows)
  }
  paired_change<-function(a,scope){
    rows<-list()
    for(d in domains)for(k in seq_len(a$k)){
      ix<-a$phenotype==k;x<-a$scores[[paste(d,"0_6h","score",sep="__")]];y<-a$scores[[paste(d,"24_48h","score",sep="__")]]
      ok<-ix&is.finite(x)&is.finite(y);delta<-y[ok]-x[ok]
      rows[[length(rows)+1L]]<-data.frame(scope=scope,phenotype=paste0("C",k),domain=d,group_n=sum(ix),paired_n=sum(ok),
        missing_either_n=sum(ix)-sum(ok),as.list(quant(delta)),lower_n=sum(delta<0),unchanged_n=sum(delta==0),higher_n=sum(delta>0))
    };do.call(rbind,rows)
  }
  raw_profile<-function(b,a,units){
    core<-unique(engine$specification()$variable[engine$specification()$tier=="main"]);rows<-list()
    if(anyDuplicated(units$variable)||!all(core%in%units$variable))stop("核心变量单位字典不完整。")
    for(w in plan$nonoverlap)for(v in core)for(k in seq_len(a$k)){
      ii<-which(a$phenotype==k);x<-b$raw_numeric[[engine$key(v,w,"mean_value")]][ii]
      rows[[length(rows)+1L]]<-data.frame(phenotype=paste0("C",k),window=w,variable=v,unit=units$unit[match(v,units$variable)],statistic="within-window observed mean",group_n=length(ii),observed_n=sum(is.finite(x)),as.list(quant(x)))
    };do.call(rbind,rows)
  }
  availability<-function(b){
    src<-unique(b$metadata$source);lm<-unique(b$metadata$landmark);n<-nrow(b$metadata);y<-b$outcomes$event_primary;icu<-b$baseline$in_index_icu
    groups<-list(All=seq_len(n),ICU_0=which(icu==0),ICU_1=which(icu==1),Outcome_0=which(y==0),Outcome_1=which(y==1))
    x<-b$raw_numeric;sp<-engine$specification();rows<-list()
    for(w in engine$windows_for(lm)){
      flags<-list();full<-list();required<-integer();totals<-integer()
      for(v in plan$biomarkers){
        cnt<-x[[engine$key(v,w,"n_chart_times")]];miss<-x[[engine$key(v,w,"missing_flag")]];val<-x[[engine$key(v,w,"max_value")]]
        if(length(cnt)!=n||anyNA(cnt)||any(cnt<0|cnt!=floor(cnt))||anyNA(miss)||any(miss!=(cnt==0))||any(is.finite(val)!=(cnt>0)))stop("标志物次数、缺失与数值不一致：",src,"/",lm,"/",v,"/",w)
        flags[[v]]<-cnt>0;full[[v]]<-flags[[v]];required[v]<-1L;totals[v]<-1L
      }
      for(d in unique(sp$domain)){
        ss<-sp[sp$domain==d,];z<-vapply(seq_len(nrow(ss)),function(i)is.finite(x[[engine$key(ss$variable[i],w,ss$stat[i])]]),logical(n))
        observed<-rowSums(z);required[d]<-ceiling(nrow(ss)*.5);totals[d]<-nrow(ss);flags[[d]]<-observed>=required[d];full[[d]]<-observed==nrow(ss)
      }
      for(g in names(groups))for(v in names(flags)){
        ii<-groups[[g]];den<-length(ii)
        rows[[length(rows)+1L]]<-data.frame(source=src,landmark=lm,window=w,stratum=g,item=v,
          kind=if(v%in%plan$biomarkers)"usable biomarker record"else"raw domain component availability",n=den,events=sum(y[ii]),available_n=sum(flags[[v]][ii]),
          unavailable_n=den-sum(flags[[v]][ii]),available_pct=if(den)100*mean(flags[[v]][ii])else NA_real_,
          complete_n=sum(full[[v]][ii]),minimum_components=required[v],specified_components=totals[v])
      }
    };do.call(rbind,rows)
  }
  check_bundle<-function(b,tag){
    i<-match(tag,expected$tag);n<-nrow(b$metadata)
    if(!identical(b$version,"Step6_v1")||n!=expected$n[i]||sum(b$outcomes$event_primary)!=expected$events[i]||anyNA(b$metadata$patient_key)||anyDuplicated(b$metadata$patient_key))stop("冻结队列不一致：",tag)
    if(!identical(paste(unique(b$metadata$source),unique(b$metadata$landmark),sep="_"),tag)||anyNA(b$outcomes$event_primary)||any(!b$outcomes$event_primary%in%0:1)||anyNA(b$baseline$in_index_icu)||any(!b$baseline$in_index_icu%in%0:1))stop("来源、结局或ICU状态异常：",tag)
    if(any(vapply(b[c("raw_numeric","baseline","outcomes")],nrow,integer(1))!=n))stop("患者级行数不一致。")
    endpoint<-if(startsWith(tag,"MIMIC"))"30_day_all_cause_death_after_landmark"else"in_hospital_death_after_T48"
    if(any(b$outcomes$endpoint!=endpoint))stop("结局定义不符。")
    invisible(TRUE)
  }
  biomarker_exclusion_audit<-function(objects,b){
    rows<-list();bad<-"lactate|troponin|cardiac_hypoperfusion"
    if(any(grepl(bad,b$feature_plan$feature[b$feature_plan$role=="main_continuous"])))stop("主连续特征含探索性标志物。")
    for(scope in names(objects)){
      o<-objects[[scope]];s<-o$state
      if(any(grepl(bad,c(s$dynamic_columns,s$score_columns,s$cluster_preprocessor$features))))stop("核心模型或分型包含标志物。")
      for(arm in names(o$models)){
        m<-o$models[[arm]];feature<-unique(c(m$preprocessor$features,m$preprocessor$indicators,names(m$coefficients)))
        hit<-feature[grepl(bad,feature)]
        if(length(hit))stop("冻结模型含不应存在的标志物输入：",scope,"/",arm)
        rows[[length(rows)+1L]]<-data.frame(scope=scope,model=arm,feature_n=length(m$preprocessor$features),biomarker_related_n=length(hit),
          interpretation="Already excluded from A-D; no new biomarker-drop model is needed")
      }
    };do.call(rbind,rows)
  }
  verify_lineage<-function(step){
    root<-file.path(cfg$OUTPUT_PARENT,step);p<-file.path(root,"to_share",paste0(step,"_STATUS.txt"))
    if(!file.exists(p)||!"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(p,warn=FALSE))stop("请先完成",step,"。")
    z<-utils::read.csv(file.path(root,"to_share/01_inputs.csv"),stringsAsFactors=FALSE)
    if(!all(c("file","md5")%in%names(z)))stop("上游审计缺字段：",step)
    for(i in seq_len(nrow(z)))if(!file.exists(z$file[i])||unname(tools::md5sum(z$file[i]))!=z$md5[i])stop(step,"输入发生变化，请先重新完成上游步骤。")
  }
  main<-function(){
    for(step in c("Step6","Step7","Step8","Step9"))verify_lineage(step)
    s6<-file.path(cfg$OUTPUT_PARENT,"Step6/local_only");s7<-file.path(cfg$OUTPUT_PARENT,"Step7/local_only")
    paths<-c(file.path(s6,paste0(expected$tag,"_model_inputs.rds")),file.path(s6,"Step6_score_engine.R"),
      file.path(s7,c("MIMIC_T48_frozen_pipeline.rds","MIMIC_T48_outer_models.rds","MIMIC_T48_nested_OOF.rds","Step7_predict.R")),file.path(cfg$OUTPUT_PARENT,"Step6/to_share/03_component_units_and_thresholds.csv"))
    if(!all(file.exists(paths)))stop("Step6/7必要输出缺失。")
    csvs<-unlist(lapply(prior_table_specs(),function(s)file.path(cfg$OUTPUT_PARENT,s$step,"to_share",s$csv)),use.names=FALSE)
    allin<-unique(c(paths,csvs));if(!all(file.exists(allin)))stop("旧三线表汇总CSV缺失。")
    hashes<-unname(tools::md5sum(allin));csv_out(data.frame(file=allin,md5=hashes),"to_share/01_inputs.csv")
    ee<-new.env(parent=globalenv());sys.source(paths[5],envir=ee);engine<<-ee$step6_score_engine;sys.source(paths[9],envir=ee);predict_step7<<-ee$predict_step7
    logmsg("小型预检：冻结赋类、单例/批量一致、非重叠窗口不参与赋类、同患者变化、缺失分母及多页PDF页数。")
    preflight();txt_out("PASS: frozen assignment; single/batch equality; non-overlap assignment invariance; paired changes; missingness denominators; explicit multipage PDF and page-count check.","to_share/02_preflight.txt")
    current_module<<-"prior table PDF repair"
    if(isTRUE(cfg$REPAIR_PRIOR_TABLES)){
      logmsg("从现有汇总CSV修复Step6-9多页三线表，覆盖同名PDF与反馈包；不重新拟合模型。")
      repair_prior_tables()
    }
    bundles<-lapply(paths[1:4],readRDS);names(bundles)<-expected$tag
    for(tag in names(bundles))check_bundle(bundles[[tag]],tag)
    b<-bundles$MIMIC_T48;final<-readRDS(paths[6]);outer<-readRDS(paths[7]);oof<-readRDS(paths[8])
    if(!identical(final$version,"Step7_v1")||final$training_n!=nrow(b$metadata)||!identical(final$endpoint,"30_day_all_cause_death_after_T48")||final$plan$min_domain_fraction!=.5)stop("最终冻结模型约定不一致。")
    if(!identical(oof$metadata$patient_key,b$metadata$patient_key)||!identical(as.numeric(oof$outcomes$event_primary),as.numeric(b$outcomes$event_primary))||anyNA(oof$outer_fold)||!setequal(oof$outer_fold,1:5)||!setequal(names(outer),paste0("outer",1:5)))stop("折外患者连接或分折不一致。")
    current_module<<-"frozen MIMIC profiles"
    logmsg("应用Step7冻结评分与中心：输出全MIMIC描述性画像；五个外折的评价画像分别保留，不合并C标签。")
    a<-assign_profile(b$raw_numeric,final$state);rt<-predict_step7(b$raw_numeric,b$baseline,final)
    if(!identical(a$phenotype,rt$phenotype)||max(abs(as.matrix(a$scores[final$state$score_columns])-as.matrix(rt$scores)),na.rm=TRUE)>1e-12)stop("冻结评分/赋类接口不一致。")
    one<-assign_profile(b$raw_numeric[1,,drop=FALSE],final$state);if(one$phenotype!=a$phenotype[1])stop("单例赋类不一致。")
    profile<-summarize_profile(a,"full_MIMIC_descriptive");delta<-paired_change(a,"full_MIMIC_descriptive");foldprofiles<-list();roundtrip<-list()
    for(o in 1:5){
      ii<-which(oof$outer_fold==o);obj<-outer[[paste0("outer",o)]]
      if(obj$state$reference$training_n!=nrow(b$metadata)-length(ii)||obj$state$cluster$training_n!=nrow(b$metadata)-length(ii))stop("外折训练样本数不符。")
      fa<-assign_profile(b$raw_numeric[ii,,drop=FALSE],obj$state);pr<-predict_step7(b$raw_numeric[ii,,drop=FALSE],b$baseline[ii,,drop=FALSE],obj)
      if(any(fa$phenotype!=oof$fold_specific_phenotype[ii])||any(!is.finite(pr$probability))||max(abs(pr$probability-oof$prediction[ii,colnames(pr$probability),drop=FALSE]))>1e-12)stop("外折冻结预测或赋类不一致。")
      foldprofiles[[paste0("outer",o)]]<-summarize_profile(fa,paste0("outer",o,"_assessment"))
      roundtrip[[o]]<-data.frame(outer_fold=o,training_n=nrow(b$metadata)-length(ii),assessment_n=length(ii),selected_k=fa$k,assignment_mismatch_n=sum(fa$phenotype!=oof$fold_specific_phenotype[ii]),max_probability_error=max(abs(pr$probability-oof$prediction[ii,colnames(pr$probability),drop=FALSE])))
    }
    sizes<-do.call(rbind,lapply(seq_len(a$k),function(k){ii<-which(a$phenotype==k);data.frame(phenotype=paste0("C",k),n=length(ii),pct=100*length(ii)/nrow(b$metadata),as.list(quant(a$distance[ii])))}))
    csv_out(sizes,"to_share/03_frozen_phenotype_sizes.csv");csv_out(profile,"to_share/03_domain_profiles.csv");csv_out(delta,"to_share/03_paired_nonoverlap_change.csv")
    csv_out(do.call(rbind,foldprofiles),"to_share/03_separate_fold_profiles.csv");csv_out(do.call(rbind,roundtrip),"to_share/02_frozen_roundtrip.csv")
    units<-utils::read.csv(paths[10],stringsAsFactors=FALSE);csv_out(raw_profile(b,a,units),"to_share/03_raw_nonoverlap_profiles.csv")
    saveRDS(list(metadata=b$metadata,phenotype=a$phenotype,domain_scores=a$scores,distance=a$distance,plan=plan),reg("local_only/MIMIC_T48_frozen_profiles.rds"),compress="gzip")
    current_module<<-"biomarker and domain availability"
    logmsg("按各landmark独立队列、ICU状态与结局计算乳酸、肌钙蛋白T及器官域原始组件可用率。")
    av<-do.call(rbind,lapply(bundles,availability));csv_out(av,"to_share/04_stratified_availability.csv")
    audit<-biomarker_exclusion_audit(c(list(full_MIMIC_final=final),outer),b);csv_out(audit,"to_share/04_biomarker_model_exclusion.csv")
    current_module<<-"figures and three-line tables";make_visuals(profile,delta,sizes,foldprofiles,av)
    txt_out(capture.output(dput(plan)),"to_share/02_analysis_plan.txt")
    txt_out(c(
      "Step10：复用Step7冻结分型；累计与非重叠窗口画像、同患者变化及分层标志物可用性。",
      "全MIMIC分型画像用于描述已拟合的分型，不是独立验证。保留中性C标签，不按死亡率排序，不强制恢复原五型。",
      "五个外折仅应用各自训练对象；评分、赋类及概率与Step7折外输出复核。每折单独展示，不将不同折的C1合并。",
      "累计窗口0-6/0-24/0-48h与非重叠0-6/6-24/24-48h共用各自对象冻结的0-6h百分位参考。非重叠窗口不重新聚类。",
      "画像用未填补的可计算评分的中位数和IQR；每格提供可用分母。赋类仍使用Step7冻结填补、标准化和中心。",
      "分型输入为四个主要器官域；第五心肌损伤/低灌注域仅可用性审计，未纳入主分型及A-D预测。",
      "24-48减0-6变化在同一患者两窗口评分均可得者计算；负值为参考百分位负担下降，不等同于已证实临床恢复。",
      "非重叠时段长度不同，最值和评分仍受测量频度、选择性测量及可用组件变化影响。附21个核心变量原始均值画像CSV供核查。",
      "标志物可用定义为预测截止前至少一条保留的有效观测；没有有效记录不能区分未检测、录入晚到、映射或质量排除，不能称正常。",
      "域原始可用需至少半数指定组件；complete_n另报全部组件齐全数。原始可用不保证冻结参考启用后一定可计算评分。",
      "MIMIC T6/T24/T48为各自独立landmark队列；eICU仅T48。结局分层仅描述缺失，不参与训练或变量选择。",
      "MIMIC结局为landmark后30天死亡；eICU为T48后院内死亡。两库结局分层不可当作同一终点的直接比较。",
      "逐个检查Step7最终及五个外折A-D输入，乳酸、肌钙蛋白T及第五域本已排除；不把重复拟合相同模型伪称新的去标志物敏感性分析。",
      "Step8简易临床S/SD/CT含乳酸，仍需按其定义解读；本步结论仅涉及Step7 A-D核心模型。",
      "本步不读取原始大表、不重训模型、不在eICU赋类或预测，也不估计分型死亡OR/HR。照护限制来源核查与外部评价仍待后续。",
      "Figure_2C替换原Figure2C四域画像；Figure_2D替换原Figure2D热图。原Figure2A/B仍需整合无结局k选择及嵌入重绘。",
      "新增非重叠窗口、逐折画像、标志物可用性补充图及三线表，最终S编号待整稿统一。",
      "修复Step6-9已登记多页表PDF：由原汇总CSV重绘，显式onefile=TRUE，实际页数核对后覆盖同名PDF并更新同名反馈包。",
      "原CSV、RTF、图、模型、缓存和患者结果不受表格修复影响；修复页数与来源MD5列入00_repaired_pdf_pages.csv。",
      "固定文件名，无时间戳；覆盖本脚本登记结果，其他用户文件保留。患者级评分与赋类仅存local_only。"),"to_share/00_READ_ME_FIRST.txt")
    txt_out("https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/pdf.html","to_share/10_method_sources.txt")
    if(!identical(hashes,unname(tools::md5sum(allin))))stop("运行期间源文件发生变化。")
    status<<-"COMPLETED_FOR_REVIEW"
  }
  synthetic_bundle<-function(n=120L,src="MIMIC",lm="T48"){
    set.seed(52026L);sp<-engine$specification();core<-unique(sp$variable[sp$tier=="main"]);x<-list();fp<-list()
    for(w in engine$windows_for(lm)){
      for(v in unique(c(core,"lactate","troponin_t"))){
        observed<-runif(n)>.12;cnt<-ifelse(observed,3,0)
        x[[engine$key(v,w,"n_chart_times")]]<-cnt;x[[engine$key(v,w,"missing_flag")]]<-as.integer(cnt==0)
        sts<-unique(c("mean_value","sd_value","slope_per_hour","abnormal_fraction",sp$stat[sp$variable==v]))
        for(st in sts){z<-rnorm(n)+rep(c(-1,1),length.out=n);if(st=="abnormal_fraction")z<-plogis(z);if(st=="sd_value")z<-abs(z);z[!observed]<-NA_real_;nm<-engine$key(v,w,st);x[[nm]]<-z
          role<-if(v%in%core&&st%in%c("mean_value","sd_value","slope_per_hour","abnormal_fraction"))if(w%in%engine$windows_for(lm,"cumulative"))"main_continuous"else"nonoverlap_continuous_sensitivity"else"score_component_input"
          fp[[length(fp)+1L]]<-data.frame(feature=nm,role=role,window=w)
        }
      }
    }
    list(version="Step6_v1",metadata=data.frame(patient_key=paste0(src,":",seq_len(n)),source=src,landmark=lm,stay_id=as.character(seq_len(n))),
      raw_numeric=as.data.frame(x,check.names=FALSE),baseline=data.frame(age=runif(n,40,90),sex_male=rep(0:1,length.out=n),in_index_icu=rep(c(0,0,1),length.out=n)),
      outcomes=data.frame(event_primary=rep(c(0,0,0,1),length.out=n),endpoint=if(src=="MIMIC")"30_day_all_cause_death_after_landmark"else"in_hospital_death_after_T48"),feature_plan=do.call(rbind,fp))
  }
  preflight<-function(){
    b<-synthetic_bundle();raw<-b$raw_numeric;rr<-engine$fit_reference(raw,1:80,min_observed=10L)
    cols<-unlist(lapply(plan$cumulative,function(w)paste(domains,gsub("-","_",w,fixed=TRUE),"score",sep="__")))
    sc<-engine$apply_reference(raw,rr,plan$cumulative)$scores[cols];pp<-engine$fit_preprocessor(sc,1:80,add_missing=FALSE,min_observed=10L)
    xx<-engine$apply_preprocessor(sc,pp);cc<-rbind(colMeans(xx[seq(1,79,2),,drop=FALSE]),colMeans(xx[seq(2,80,2),,drop=FALSE]));rownames(cc)<-c("C1","C2")
    state<-list(version="Step7_v1",reference=rr,score_columns=cols,cluster_preprocessor=pp,cluster=list(centers=cc,k=2L))
    a<-assign_profile(raw,state);one<-assign_profile(raw[81,,drop=FALSE],state)
    stopifnot(one$phenotype==a$phenotype[81],isTRUE(all.equal(one$scores,a$scores[81,,drop=FALSE],check.attributes=FALSE)))
    changed<-raw;non<-grepl("__6_24h__|__24_48h__",names(changed));changed[non]<-lapply(changed[non],function(v)ifelse(is.na(v),NA_real_,v*10+10))
    stopifnot(identical(assign_profile(changed,state)$phenotype,a$phenotype))
    d<-paired_change(a,"fixture");stopifnot(all(d$lower_n+d$unchanged_n+d$higher_n==d$paired_n),all(d$paired_n+d$missing_either_n==d$group_n))
    # Hand-computed paired sample: only rows 1 and 4 have both values; deltas 2 and -1.
    aa<-a;aa$phenotype<-rep(1L,4);aa$k<-1L;aa$scores<-aa$scores[1:4,,drop=FALSE]
    for(dom in domains){aa$scores[[paste(dom,"0_6h","score",sep="__")]]<-c(1,NA,8,5);aa$scores[[paste(dom,"24_48h","score",sep="__")]]<-c(3,9,NA,4)}
    dd<-paired_change(aa,"manual");stopifnot(all(dd$paired_n==2),all(dd$median==.5),all(dd$q25==-.25),all(dd$q75==1.25))
    av<-availability(b);stopifnot(all(av$available_n+av$unavailable_n==av$n),all(av$complete_n<=av$available_n))
    nk<-engine$key("lactate","0-6h","n_chart_times");mk<-engine$key("lactate","0-6h","missing_flag");vk<-engine$key("lactate","0-6h","max_value")
    bb<-b;bb$raw_numeric[[nk]][1]<-0;bb$raw_numeric[[mk]][1]<-1;bb$raw_numeric[[vk]][1]<-1
    stopifnot(inherits(try(availability(bb),silent=TRUE),"try-error"))
    f<-tempfile(fileext=".pdf");on.exit(unlink(f));draw_table_pdf(list(First=data.frame(A="one",B="1"),Second=data.frame(A="two",B="2")),"Preflight",f);stopifnot(pdf_pages(f)==2L)
    invisible(TRUE)
  }
  profile_draw<-function(z,sizes,title,footnote){
    pal<-c("#4F7FC4","#D98C57","#619E91","#9A78AB","#C9AA4D","#657786");cls<-sizes$phenotype;w<-unique(z$window)
    op<-par(no.readonly=TRUE);on.exit(par(op));par(mfrow=c(2,2),mar=c(4.1,4.3,2.6,.8),oma=c(3.1,0,2.4,0),family="sans",las=1,cex=.9)
    for(j in seq_along(domains)){
      d<-domains[j];plot(0,0,type="n",xlim=c(.75,3.25),ylim=c(0,100),xaxt="n",xlab="Observation window",ylab="Frozen reference score (0-100)",main=paste0(LETTERS[j],"  ",labels[d]),adj=0,bty="l",cex.main=.95);axis(1,1:3,sub("h"," h",w,fixed=TRUE));abline(h=c(25,50,75),col="#EAEAEA",lty=3)
      for(i in seq_along(cls)){
        g<-z[z$domain==d&z$phenotype==cls[i],];g<-g[match(w,g$window),];xx<-1:3+(i-(length(cls)+1)/2)*.07
        segments(xx,g$q25,xx,g$q75,col=adjustcolor(pal[i],alpha.f=.6),lwd=3);lines(xx,g$median,col=pal[i],lty=i,lwd=1.7,type="b",pch=15+i)
      }
    }
    mtext(title,side=3,outer=TRUE,line=1.1,font=2,cex=1.05)
    mtext(footnote,side=1,outer=TRUE,line=.65,cex=.73)
    mtext("Points: observed-score median; bars: IQR, not confidence intervals. Missing scores are not filled for these summaries.",side=1,outer=TRUE,line=1.65,cex=.73)
    par(oma=rep(0,4),fig=c(0,1,0,1),new=TRUE,mar=rep(0,4));plot.new();legend(.5,.955,legend=paste0(cls," (n=",sizes$n,")"),col=pal[seq_along(cls)],lwd=2,pch=15+seq_along(cls),lty=seq_along(cls),horiz=TRUE,xjust=.5,bty="n",cex=.85)
  }
  make_visuals<-function(profile,delta,sizes,foldprofiles,av){
    z<-profile[profile$window_type=="cumulative",]
    save_figure("Figure_2C_frozen_domain_profiles",function()profile_draw(z,sizes,"Frozen MIMIC phenotypes: cumulative windows","Full-MIMIC fitted partition: descriptive profiles, not independent validation; neutral C labels."),10.8,8.2)
    z2<-profile[profile$window_type=="nonoverlap",]
    save_figure("Figure_S_nonoverlap_domain_profiles",function()profile_draw(z2,sizes,"Same frozen phenotypes: non-overlapping windows","Assignments remain based on cumulative windows. Window lengths and available components differ."),10.8,8.2)
    draw_heat<-function(){
      op<-par(no.readonly=TRUE);on.exit(par(op));par(mfrow=c(1,4),mar=c(4,3,3,1),oma=c(3.5,0,2,0),family="sans",cex=.85)
      colors<-colorRampPalette(c("#F7FBFF","#C5D9EB","#79A7CD","#245781"))(101);k<-nrow(sizes)
      for(j in seq_along(domains)){
        d<-domains[j];plot(0,0,type="n",xlim=c(.5,3.5),ylim=c(.5,k+.5),axes=FALSE,xlab="Cumulative window",ylab="",main=labels[d],cex.main=.85)
        axis(1,1:3,c("0-6 h","0-24 h","0-48 h"));axis(2,k:1,sizes$phenotype,las=1)
        for(i in seq_len(k))for(t in 1:3){q<-z[z$domain==d&z$phenotype==sizes$phenotype[i]&z$window==plan$cumulative[t],];v<-q$median;yy<-k-i+1
          rect(t-.49,yy-.49,t+.49,yy+.49,col=if(is.finite(v))colors[round(v)+1L]else"#EEEEEE",border="white")
          text(t,yy,if(is.finite(v))sprintf("%.1f",v)else"NA",col=if(is.finite(v)&&v>65)"white"else"#222222",cex=1.05)
        }
      }
      mtext("Frozen MIMIC phenotypes: median score profiles",side=3,outer=TRUE,line=.6,font=2,cex=1.05)
      mtext("Same 0-100 scale in all cells; darker blue indicates higher reference percentile burden. Grey = unavailable.",side=1,outer=TRUE,line=.5,cex=.75)
      mtext("Full-MIMIC fitted partition, descriptive only. Per-cell observed n is provided in 03_domain_profiles.csv.",side=1,outer=TRUE,line=1.5,cex=.75)
    };save_figure("Figure_2D_frozen_domain_heatmap",draw_heat,12,4.6)
    fp<-reg("to_share/Figure_S_separate_fold_profiles.pdf");open_pdf(fp,10.8,8.2)
    tryCatch(for(scope in names(foldprofiles)){
      v<-foldprofiles[[scope]];v<-v[v$window_type=="cumulative",];ss<-unique(v[c("phenotype","group_n")]);names(ss)[2]<-"n"
      profile_draw(v,ss,paste0(scope,": held-out cumulative profiles"),"Each page uses its own training-fold reference and centers. C labels are not aligned or pooled across pages.")
    },finally=grDevices::dev.off());check_pdf(fp,length(foldprofiles))
    overall<-av[av$stratum=="All"&av$item%in%plan$biomarkers,]
    draw_av<-function(){
      op<-par(no.readonly=TRUE);on.exit(par(op));par(mfrow=c(1,2),mar=c(4.5,4.6,3,1),oma=c(3.4,0,1.7,0),family="sans",las=1,cex=.9)
      for(j in 1:2){v<-plan$biomarkers[j];rows<-overall[overall$item==v,];pick<-rows[(rows$source=="MIMIC"&rows$window==paste0("0-",sub("T","",rows$landmark),"h"))|(rows$source=="eICU"&rows$window=="0-48h"),];pick<-pick[match(expected$tag,paste(pick$source,pick$landmark,sep="_")),]
        yy<-barplot(pick$available_pct,names.arg=c("MIMIC T6","MIMIC T24","MIMIC T48","eICU T48"),col=c(rep("#4F7FC4",3),"#D98C57"),border=NA,ylim=c(0,115),ylab="Patients with a usable record (%)",main=if(j==1)"A  Lactate"else"B  Troponin T",adj=0,cex.names=.8)
        text(yy,pick$available_pct,labels=sprintf("%.1f%%\n%s/%s",pick$available_pct,pick$available_n,pick$n),pos=3,cex=.8)
      }
      mtext("Biomarker availability by each prediction landmark",side=3,outer=TRUE,line=.25,font=2,cex=1.05)
      mtext("At least one retained valid record within 0-landmark hours. Absence of a usable record does not mean a normal result.",side=1,outer=TRUE,line=.5,cex=.75)
      mtext("Separate eligible populations. No biomarker values or fifth-domain scores enter the Step7 core A-D models.",side=1,outer=TRUE,line=1.5,cex=.75)
    };save_figure("Figure_S_biomarker_availability",draw_av,11,4.8)
    tabs<-list()
    for(typ in c("cumulative","nonoverlap"))for(cl in sizes$phenotype){
      q<-profile[profile$window_type==typ&profile$phenotype==cl,];tab<-data.frame(Domain=unname(labels[domains]),check.names=FALSE)
      for(w in plan[[typ]]){a<-q[q$window==w,];a<-a[match(domains,a$domain),];tab[[w]]<-paste0(fmt_q(a),"; n=",a$observed_n)}
      tabs[[paste(cl,typ,"windows; group n =",sizes$n[match(cl,sizes$phenotype)])]]<-tab
    }
    notes<-c("Values are median [Q1, Q3]; n is the number with a calculable, unfilled score. IQR describes spread, not confidence.","Full-MIMIC fitted clusters are descriptive. Reference is frozen from 0-6h. Non-overlapping windows do not redefine cluster membership.")
    three_line(tabs,"Supplementary table. Frozen domain profiles","Table_S_frozen_domain_profiles",notes)
    dtabs<-list()
    for(cl in sizes$phenotype){q<-delta[delta$phenotype==cl,];q<-q[match(domains,q$domain),]
      dtabs[[paste(cl,"paired change: 24-48 h minus 0-6 h")]]<-data.frame(Domain=unname(labels[domains]),`Paired / group n`=paste0(q$paired_n," / ",q$group_n),`Median [IQR]`=fmt_q(q),Lower=q$lower_n,Equal=q$unchanged_n,Higher=q$higher_n,check.names=FALSE)
    };three_line(dtabs,"Supplementary table. Within-patient non-overlapping change","Table_S_paired_nonoverlap_change",c("Only patients with both scores observed are included; paired_n is the actual denominator.","Lower indicates a lower reference percentile score, not proven recovery. Measurement intensity, interval lengths and component mix can affect change."))
    atabs<-list()
    for(tag in expected$tag){parts<-strsplit(tag,"_",fixed=TRUE)[[1]];src<-parts[1];lm<-parts[2];ww<-paste0("0-",sub("T","",lm),"h")
      for(v in plan$biomarkers){q<-av[av$source==src&av$landmark==lm&av$window==ww&av$item==v,];q<-q[match(c("All","ICU_0","ICU_1","Outcome_0","Outcome_1"),q$stratum),]
        atabs[[paste(tag,v,"within",ww)]]<-data.frame(Stratum=q$stratum,N=q$n,Available=q$available_n,`Available %`=sprintf("%.1f",q$available_pct),`No usable record`=q$unavailable_n,check.names=FALSE)
      }
    };three_line(atabs,"Supplementary table. Stratified biomarker availability","Table_S_biomarker_availability",c("ICU_0: out of index ICU but still hospitalized; ICU_1: in index ICU. Outcome_1 denotes an event; these strata overlap.","MIMIC: 30-day death after the landmark. eICU: in-hospital death after T48. No usable record does not establish that testing was never performed."))
    csv_out(data.frame(output=c("Figure_2C_frozen_domain_profiles","Figure_2D_frozen_domain_heatmap","Figure_S_nonoverlap_domain_profiles","Figure_S_separate_fold_profiles","Figure_S_biomarker_availability","Table_S_frozen_domain_profiles","Table_S_paired_nonoverlap_change","Table_S_biomarker_availability"),
      action=c("REPLACE Figure 2C","REPLACE Figure 2D",rep("ADD supplementary; final S number pending",6)),
      detail=c("Four main domains; fitted MIMIC neutral labels; replaces outcome-ordered five-class cumulative profiles","Four-domain median heatmap; unavailable scores not imputed for display","Same frozen labels; non-overlapping windows","Five pages; separate held-out outer-fold labels, no pooling","Actual landmark populations; lactate and troponin T","Median/IQR and observed denominators","Paired subjects only, no causal recovery claim","ICU and outcome strata; source definitions remain distinct")),"to_share/11_figure_table_replacement_map.csv")
  }

  if(isTRUE(getOption("ami_step10.functions_only",FALSE)))return(invisible(list(main=main,preflight=preflight,synthetic_bundle=synthetic_bundle,assign_profile=assign_profile,
    summarize_profile=summarize_profile,availability=availability,paired_change=paired_change,repair_prior_tables=repair_prior_tables,
    make_visuals=make_visuals,pdf_pages=pdf_pages,draw_table_pdf=draw_table_pdf)))
  status<-"FAILED";current_module<-"input verification"
  failure_message<-NULL
  tryCatch(withCallingHandlers(main(),warning=function(w){add_warning(conditionMessage(w),current_module);invokeRestart("muffleWarning")}),error=function(err){status<<-"FAILED";failure_message<<-conditionMessage(err)
    issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step10_error_detail.txt")})
  flush_warnings()
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step10",
    "COMPLETED_FOR_REVIEW表示冻结分型画像、非重叠窗口与标志物可用性图表已生成，等待结果核对；外部验证尚未开展。",
    "FAILED表示不完整，不能使用本次患者级结果建模。"),"to_share/Step10_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step10_sessionInfo.txt")
  logmsg(paste0("Step10状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step10_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share) else {
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step10_feedback.zip"),"\n",sep="") else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
