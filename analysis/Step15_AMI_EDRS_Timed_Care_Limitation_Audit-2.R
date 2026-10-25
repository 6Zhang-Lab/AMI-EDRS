# AMI-EDRS revision | Step15-2: time-stamped care-limitation documentation audit
# source(file.choose(), encoding="UTF-8")
# Step15-2 unifies code fields, handles intubation separately, and audits recognized nonpositive codes.
# Step15-1 canonical-path fix retained. Raw extraction and cache keys are unchanged.
# Fixed filenames; replaces owned results. Patient-level data and frozen references stay local_only.
local({
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    RAW_MIMIC_DIR=Sys.getenv("AMI_MIMIC_RAW", unset=file.path(getwd(), "data/mimic-iv-3.1")),RAW_EICU_DIR=Sys.getenv("AMI_EICU_RAW", unset=file.path(getwd(), "data/eicu-crd-2.0")),
    RAW_FILES=list(),MEMORY_LIMIT="2GB",REFRESH_CACHE=FALSE,MAKE_PNG=TRUE,MAKE_ZIP=TRUE)
  cfg<-utils::modifyList(cfg,getOption("ami_step15.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step15")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step15输出目录。")
  if (is_link(out)) stop("Step15输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP15")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step15目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP15_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP15_MANAGED_OUTPUT")) {
    stop("Step15输出目录的归属标记不正确。")
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
  # Keep only aggregate counts in memory before replacing owned reports.
  previous_run<-list(version="not_available",summary=NULL)
  previous_status<-file.path(out,"to_share/Step15_STATUS.txt")
  if(file.exists(previous_status)){
    pv<-readLines(previous_status,warn=FALSE);pv<-pv[startsWith(pv,"SCRIPT_VERSION = ")]
    if(length(pv)==1L)previous_run$version<-sub("^SCRIPT_VERSION = ","",pv)
  }
  previous_summary<-file.path(out,"to_share/04_care_documentation_summary.csv")
  if(file.exists(previous_summary))previous_run$summary<-tryCatch(utils::read.csv(previous_summary,stringsAsFactors=FALSE),error=function(e)NULL)
  # 登记过的文件才会删除。不会遍历清空修改目录、旧项目或用户另存的文件。
  old <- character()
  if (file.exists(manifest)) {
    old <- unique(readLines(manifest, warn = FALSE))
    for (rel in old[nzchar(old)]) {
      p <- safe_path(rel)
      if (file.exists(p) && !startsWith(rel,"local_only/cache/")) {
        if (dir.exists(p)) stop("登记的输出应为文件，不能为目录。")
        if (unlink(p) != 0L) stop("不能覆盖旧Step15输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step15_run_log.txt")
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
    txt_out(if(nrow(z))unlist(Map(function(a,b)c(a,b,""),z$scope,z$message))else"No warnings in Step15.","local_only/Step15_warning_detail.txt")
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
      xs<-if(nc==4)c(.04,.56,.77,.96)else c(.04,seq(.4,.96,length.out=nc-1));top<-.85;dy<-min(.052,.48/max(1,nr))
      line<-function(y,w)grid::grid.lines(x=c(.04,.97),y=c(y,y),gp=grid::gpar(lwd=w))
      line(top,1.3)
      for(j in seq_len(nc))grid::grid.text(names(tab)[j],xs[j],top-.022,just=if(j==1)"left"else"right",gp=grid::gpar(fontsize=14,fontface="bold"))
      line(top-.045,.8)
      for(i in seq_len(nr))for(j in seq_len(nc))grid::grid.text(as.character(tab[i,j]),xs[j],top-.035-i*dy,just=if(j==1)"left"else"right",gp=grid::gpar(fontsize=14))
      bot<-top-.055-nr*dy;line(bot,1.3)
      wrap<-unlist(lapply(notes,strwrap,width=100))
      for(i in seq_along(wrap))grid::grid.text(wrap[i],.04,bot-.029*i,just="left",gp=grid::gpar(fontsize=12))
      grid::grid.text(paste(match(nm,names(tables)),"/",length(tables)),.97,.035,just="right",gp=grid::gpar(fontsize=12))
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
      rows<-rbind(names(tables[[nm]]),as.matrix(tables[[nm]]));nc<-ncol(rows);ends<-round(c(5100,seq(8500,15100,length.out=nc-1)))
      for(i in seq_len(nrow(rows))){bd<-paste0(if(i==1)"\\clbrdrt\\brdrs\\brdrw15\\clbrdrb\\brdrs\\brdrw10"else"",if(i==nrow(rows))"\\clbrdrb\\brdrs\\brdrw15"else"")
        rt<-c(rt,paste0("\\trowd\\trgaph60\\trkeep",paste0(bd,"\\cellx",ends,collapse=""),paste(vapply(seq_len(nc),function(j)paste0("\\pard\\intbl",if(j==1)"\\ql "else"\\qr ","\\fs24 ",if(i==1)"\\b "else"\\b0 ",esc(rows[i,j]),"\\cell"),character(1)),collapse=""),"\\row"))}
      rt<-c(rt,paste0("\\pard\\fs24 ",paste(esc(notes),collapse="\\par "),"\\par"))
    };txt_out(c(rt,"}"),paste0("to_share/",stem,".rtf"))
  }
  plan<-list(version="Step15_v1",landmark_hour=48,unit="index ICU stay",
    primary="Explicit structured documentation with both record and entry times available by T48",
    purpose="Ascertainment audit; no outcome deletion or model refitting",
    rule="Conservative exact values; unmatched text remains unresolved, not negative")
  concepts<-c("comfort_only","dnr","dni","palliative_consult","other_limit","eol_discussion")
  labels<-c(comfort_only="Comfort-only care",dnr="DNR",dni="DNI",palliative_consult="Palliative consult record",
    other_limit="Other care limits",eol_discussion="EOL discussion record")
  clean<-function(x){z<-tolower(trimws(as.character(x)));z[is.na(z)]<-"";z<-gsub("[[:space:]]+"," ",z);gsub("[[:space:]]*\\|[[:space:]]*","|",z)}
  hash_object<-function(x){p<-tempfile();on.exit(unlink(p));saveRDS(x,p,version=2);unname(tools::md5sum(p))}
  binary<-function(x){if(is.factor(x))x<-as.character(x);if(is.logical(x))x<-as.integer(x);z<-as.character(x)
    if(anyNA(z)||any(!z%in%c("0","1")))stop("结局或资格字段含缺失/非0/1，不自动转换为存活。");as.integer(z)}
  empty_events<-function()data.frame(source=character(),stay_id=character(),stream=character(),field=character(),value=character(),
    chart_hour=double(),entry_hour=double(),status=character(),stringsAsFactors=FALSE)
  empty_mapping<-function()data.frame(row=integer(),concept=character(),rule=character(),stringsAsFactors=FALSE)
  candidate_pattern<-"code[ -]?status|resusc|\\bdnr\\b|\\bdni\\b|comfort|palliat|hospice|end.of.life|care.limit|no augmentation|withhold|withdraw|full (code|therapy)|terminal|consult"
  is_candidate<-function(x)grepl(candidate_pattern,clean(x),perl=TRUE)
  # These functions affect analysis only. Raw extraction/cache keys remain Step15_v1.
  analysis_version<-"Step15_mapping_v2"
  code_field<-function(x){
    z<-sub("[.]$","",clean(x))
    grepl("(^|\\|)(code status|resuscitation status|care limitation|care limitations|code status/care limitation)($|\\|)",z,perl=TRUE)
  }
  intubation_field<-function(x)sub("[.]$","",clean(x))=="code status (intubation)"
  record_allowed<-function(x)!clean(x)%in%c("rewritten","cancelled","canceled","not performed","discontinued")
  decode_intubation<-function(x){
    z<-clean(x);ans<-rep("unmapped",length(z))
    ans[decode_status(z)%in%c("dni","dnr_dni")]<-"dni"
    ans[z=="intubate"]<-"intubation_permitted"
    ans
  }
  latest_state<-function(e,tm,roster,field_match,values,missing_label){
    ans<-rep(missing_label,nrow(roster));keys<-paste(roster$source,roster$stay_id)
    idx<-which(field_match&record_allowed(e$status)&tm$by_T48)
    grouped<-split(idx,paste(e$source[idx],e$stay_id[idx]))
    for(key in names(grouped)){
      j<-grouped[[key]];j<-j[e$chart_hour[j]==max(e$chart_hour[j])]
      j<-j[e$entry_hour[j]==max(e$entry_hour[j])];v<-unique(values[j])
      ans[match(key,keys)]<-if(length(v)==1L)v else "same_time_conflict"
    }
    ans
  }
  code_field_group<-function(x){
    z<-clean(x);leaf<-sub("^.*\\|","",z);ans<-rep("other_candidate_field",length(z))
    ans[code_field(z)]<-"other_supported_code_field"
    known<-c("code status","code status.","resuscitation status","care limitation","care limitations","code status/care limitation")
    for(k in known)ans[leaf==k]<-k
    ans[intubation_field(z)]<-"code status (intubation)"
    ans
  }
  code_audit<-function(e){
    empty<-data.frame(source=character(),stream=character(),field_group=character(),standardized_state=character(),
      rows=integer(),patients=integer(),by_T48_rows=integer(),by_T48_patients=integer(),
      positive_concept_rows=integer(),excluded_status_rows=integer())
    if(!nrow(e))return(empty)
    cf<-code_field(e$field);inf<-intubation_field(e$field);ix<-which(cf|inf)
    if(!length(ix))return(empty)
    # Only fixed category labels are shared; raw field values and patient identifiers stay local.
    state<-ifelse(cf,decode_status(e$value),decode_intubation(e$value))
    state[!record_allowed(e$status)]<-"excluded_record_status"
    groups<-split(ix,paste(e$source[ix],e$stream[ix],code_field_group(e$field[ix]),state[ix],sep="\r"))
    ans<-lapply(groups,function(i){
      ok<-e$by_T48[i]&record_allowed(e$status[i])
      data.frame(source=e$source[i[1]],stream=e$stream[i[1]],field_group=code_field_group(e$field[i[1]]),
        standardized_state=state[i[1]],rows=length(i),patients=length(unique(e$stay_id[i])),
        by_T48_rows=sum(ok),by_T48_patients=length(unique(e$stay_id[i][ok])),
        positive_concept_rows=sum(e$positive_concept_mapped[i]),excluded_status_rows=sum(!record_allowed(e$status[i])))
    })
    ans<-do.call(rbind,ans);rownames(ans)<-NULL;ans
  }
  revision_comparison<-function(summary,previous){
    ans<-summary[c("source","concept","recorded_by_T48_n")];names(ans)[3]<-"current_by_T48_n"
    ans$previous_script_version<-previous$version;ans$previous_by_T48_n<-NA_integer_
    p<-previous$summary
    if(!is.null(p)&&all(c("source","concept","recorded_by_T48_n")%in%names(p))&&!anyDuplicated(paste(p$source,p$concept))){
      i<-match(paste(ans$source,ans$concept),paste(p$source,p$concept))
      ans$previous_by_T48_n<-p$recorded_by_T48_n[i]
    }
    ans$change_n<-ans$current_by_T48_n-ans$previous_by_T48_n
    ans
  }
  decode_status<-function(x){
    z<-clean(x);out<-rep("unmapped",length(z))
    out[z%in%c("full code","full therapy","full treatment","full resuscitation")]<-"full_code"
    out[z%in%c("dnr","do not resuscitate","do not attempt resuscitation","dnar","no cpr","dnr (do not resuscitate)")]<-"dnr"
    out[z%in%c("dni","do not intubate","dni (do not intubate)")]<-"dni"
    out[z%in%c("dnr/dni","dnr / dni","dnr/dni (do not resuscitate/do not intubate)","dnr/dni (do not resuscitate / do not intubate)",
      "do not resuscitate/do not intubate","do not resuscitate / do not intubate","do not resuscitate, do not intubate")]<-"dnr_dni"
    out[z%in%c("comfort measures only","comfort measures","comfort care only","comfort care","cmo","cmo (comfort measures only)")]<-"comfort_only"
    out[z%in%c("no augmentation of care","limited therapy","limited code","limited treatment")]<-"other_limit"
    out
  }
  classify_events<-function(e){
    if(!nrow(e))return(empty_mapping())
    f<-clean(e$field);v<-clean(e$value);s<-clean(e$status);ans<-list()
    add<-function(i,concept,rule){if(length(i))ans[[length(ans)+1L]]<<-data.frame(row=as.integer(i),concept=rep(concept,length(i)),rule=rep(rule,length(i)))}
    allowed<-record_allowed(s)
    cf<-code_field(f)
    st<-decode_status(v)
    add(which(allowed&cf&st=="comfort_only"),"comfort_only","exact_code_value")
    add(which(allowed&cf&st%in%c("dnr","dnr_dni")),"dnr","exact_code_value")
    add(which(allowed&cf&st%in%c("dni","dnr_dni")),"dni","exact_code_value")
    add(which(allowed&cf&st=="other_limit"),"other_limit","exact_code_value")
    add(which(allowed&intubation_field(f)&decode_intubation(v)=="dni"),"dni","explicit_intubation_code_value")
    fields<-list(comfort_only=c("comfort measures only","comfort care only","comfort care"),
      dnr=c("do not resuscitate","dnr"),dni=c("do not intubate","dni"),
      palliative_consult=c("palliative care consult","palliative care consultation","palliative consult","palliative consultation"),
      eol_discussion=c("end of life discussion","end-of-life discussion","eol discussion"))
    yes<-v%in%c("yes","true","1","active","documented","performed","completed","done")
    for(a in names(fields))add(which(allowed&f%in%fields[[a]]&yes),a,"explicit_field_affirmative")
    add(which(allowed&f%in%c("consults","consultation","consult type")&v%in%c("palliative care","palliative medicine","palliative care consultation")),"palliative_consult","explicit_consult_specialty")
    # A treatment row documents an active treatment entry, not adjudicated consultation completion.
    tx<-which(e$stream=="eICU_treatment"&allowed)
    if(length(tx)){
      text<-v[tx];leaf<-sub("^.*\\|","",text)
      bad<-grepl("(^|[^a-z])(no|not|without|denied|declined|refused|planned|pending|consider|considered|history|previous|prior|requested|ordered)($|[^a-z])",text,perl=TRUE)
      # 'do not resuscitate' is a positive code-status phrase, so it is handled separately in code fields.
      for(a in c("comfort_only","palliative_consult")){
        exact<-if(a=="comfort_only")c("comfort measures only","comfort care only","comfort care")else fields[[a]]
        add(tx[!bad&leaf%in%exact],a,"exact_active_treatment_leaf")
      }
    }
    add(which(e$stream=="eICU_carePlanEOL"&allowed),"eol_discussion","discussion_record_not_comfort_care")
    if(!length(ans))return(empty_mapping())
    z<-do.call(rbind,ans);z[!duplicated(z[c("row","concept")]),,drop=FALSE]
  }
  timing<-function(e){
    valid<-is.finite(e$chart_hour)&is.finite(e$entry_hour)
    eligible<-valid&e$chart_hour>=0&e$entry_hour>=0&e$chart_hour<=48&e$entry_hour<=48
    data.frame(time_known=valid,by_T48=eligible,
      positive_offset=eligible&!(e$source=="eICU"&(e$chart_hour==0|e$entry_hour==0)),
      chart_only_by_T48=is.finite(e$chart_hour)&e$chart_hour>=0&e$chart_hour<=48,
      late_entry=valid&e$chart_hour>=0&e$chart_hour<=48&e$entry_hour>48,
      recorded_after_T48=valid&e$chart_hour>=0&e$entry_hour>=0&pmax(e$chart_hour,e$entry_hour)>48,
      zero_offset=e$source=="eICU"&((is.finite(e$chart_hour)&e$chart_hour==0)|(is.finite(e$entry_hour)&e$entry_hour==0)),
      negative_time=(is.finite(e$chart_hour)&e$chart_hour<0)|(is.finite(e$entry_hour)&e$entry_hour<0))
  }
  raw_csv_sql<-function(p,q)paste0("read_csv(",q(p),",delim=',',quote='\"',escape='\"',header=true,skip=0,all_varchar=true,strict_mode=true,null_padding=false,ignore_errors=false)")
  stream_columns<-list(MIMIC_chartevents=c("subject_id","hadm_id","stay_id","itemid","charttime","storetime","value"),
    MIMIC_procedureevents=c("subject_id","hadm_id","stay_id","itemid","starttime","storetime","statusdescription"),
    eICU_carePlanGeneral=c("patientunitstayid","cplitemoffset","cplgroup","cplitemvalue"),
    eICU_carePlanEOL=c("patientunitstayid","cpleolsaveoffset","cpleoldiscussionoffset"),
    eICU_treatment=c("patientunitstayid","treatmentoffset","treatmentstring"))
  query_for<-function(stream,view="rawdata"){
    if(stream%in%c("MIMIC_chartevents","MIMIC_procedureevents")){
      chart<-if(stream=="MIMIC_chartevents")"charttime"else"starttime"
      value<-if(stream=="MIMIC_chartevents")"x.value"else"'documented'"
      status<-if(stream=="MIMIC_chartevents")"''"else"x.statusdescription"
      return(paste0("SELECT 'MIMIC' AS source,b.stay_id,'",stream,"' AS stream,d.label AS field,",value," AS value,",
        "epoch(TRY_CAST(x.",chart," AS TIMESTAMP)-CAST(b.intime AS TIMESTAMP))/3600.0 AS chart_hour,",
        "epoch(TRY_CAST(x.storetime AS TIMESTAMP)-CAST(b.intime AS TIMESTAMP))/3600.0 AS entry_hour,",status," AS status FROM ",view,
        " x JOIN mimic_ids b ON x.stay_id=b.stay_id AND x.subject_id=b.subject_id AND x.hadm_id=b.hadm_id",
        " JOIN item_map d ON x.itemid=d.itemid AND d.linksto='",sub("MIMIC_","",stream),"'"))
    }
    part<-switch(stream,
      eICU_carePlanGeneral="x.cplgroup AS field,x.cplitemvalue AS value,TRY_CAST(x.cplitemoffset AS DOUBLE)/60.0 AS chart_hour,TRY_CAST(x.cplitemoffset AS DOUBLE)/60.0 AS entry_hour",
      eICU_carePlanEOL="'EOL discussion' AS field,'documented' AS value,TRY_CAST(x.cpleoldiscussionoffset AS DOUBLE)/60.0 AS chart_hour,TRY_CAST(x.cpleolsaveoffset AS DOUBLE)/60.0 AS entry_hour",
      eICU_treatment="'Active treatment documentation' AS field,x.treatmentstring AS value,TRY_CAST(x.treatmentoffset AS DOUBLE)/60.0 AS chart_hour,TRY_CAST(x.treatmentoffset AS DOUBLE)/60.0 AS entry_hour",
      stop("未知来源表。"))
    paste0("SELECT 'eICU' AS source,b.stay_id,'",stream,"' AS stream,",part,",'' AS status FROM ",view,
      " x JOIN eicu_ids b ON x.patientunitstayid=b.stay_id")
  }
  analyze_events<-function(e,roster,streams){
    e$source<-as.character(e$source);e$stay_id<-as.character(e$stay_id)
    if(anyNA(match(paste(e$source,e$stay_id),paste(roster$source,roster$stay_id))))stop("提取记录超出冻结患者名单。")
    e<-e[!duplicated(e),,drop=FALSE];rownames(e)<-NULL
    tm<-timing(e);mp<-classify_events(e)
    evidence<-if(nrow(mp))cbind(e[mp$row,,drop=FALSE],mp[c("concept","rule")],tm[mp$row,,drop=FALSE])else cbind(e[FALSE,],data.frame(concept=character(),rule=character()),tm[FALSE,])
    private<-cbind(e,tm);private$positive_concept_mapped<-seq_len(nrow(e))%in%mp$row
    statuses<-decode_status(e$value);iv<-decode_intubation(e$value)
    private$code_value<-ifelse(code_field(e$field),statuses,NA_character_)
    private$intubation_value<-ifelse(intubation_field(e$field),iv,NA_character_)
    private$recognized_nonpositive_code<-record_allowed(e$status)&
      ((code_field(e$field)&statuses=="full_code")|(intubation_field(e$field)&iv=="intubation_permitted"))
    private$mapped<-private$positive_concept_mapped|private$recognized_nonpositive_code
    latest<-roster[c("source","stay_id")]
    latest$latest_code_T48<-latest_state(e,tm,roster,code_field(e$field),statuses,"no_time_eligible_code_record")
    latest$latest_intubation_T48<-latest_state(e,tm,roster,intubation_field(e$field),iv,"no_time_eligible_intubation_record")
    # Intubation-only documentation never replaces the overall resuscitation/care-limitation state.
    summ<-list();bygroup<-list();patients<-list()
    for(src in unique(roster$source))for(a in concepts){
      rr<-roster[roster$source==src,,drop=FALSE];ee<-evidence[evidence$source==src&evidence$concept==a,,drop=FALSE]
      sources<-streams$stream[streams$source==src&streams$available&grepl(paste0("(^|;)",a,"(;|$)"),streams$concept_scope)]
      enabled<-length(sources)>0L
      ids<-unique(ee$stay_id[ee$by_T48]);nozero<-unique(ee$stay_id[ee$positive_offset]);later<-setdiff(unique(ee$stay_id[ee$recorded_after_T48]),ids)
      chartids<-unique(ee$stay_id[ee$chart_only_by_T48]);unknown<-unique(ee$stay_id[!ee$time_known]);neg<-unique(ee$stay_id[ee$negative_time])
      z<-data.frame(source=src,stay_id=rr$stay_id,concept=a,source_evaluated=enabled,
        documented_by_T48=if(enabled)as.integer(rr$stay_id%in%ids)else NA_integer_,
        documented_by_T48_excluding_zero=if(enabled)as.integer(rr$stay_id%in%nozero)else NA_integer_,
        positive_only_recorded_after_T48=if(enabled)as.integer(rr$stay_id%in%later)else NA_integer_)
      patients[[paste(src,a)]]<-z
      summ[[paste(src,a)]]<-data.frame(source=src,concept=a,cohort_n=nrow(rr),source_evaluated=enabled,
        recorded_by_T48_n=if(enabled)length(ids)else NA_integer_,
        excluding_zero_offset_n=if(enabled)length(nozero)else NA_integer_,
        recorded_only_after_T48_n=if(enabled)length(later)else NA_integer_,
        chart_time_only_n=if(enabled)length(chartids)else NA_integer_,
        positive_time_unknown_n=if(enabled)length(unknown)else NA_integer_,
        positive_negative_time_n=if(enabled)length(neg)else NA_integer_,
        sources=paste(sources,collapse=";"))
      for(group in c("All","Still_in_ICU","Transferred_from_index_ICU")){
        keep<-if(group=="All")rep(TRUE,nrow(rr))else if(group=="Still_in_ICU")rr$in_index_icu==1 else rr$in_index_icu==0
        ix<-which(keep);pos<-rr$stay_id[ix]%in%ids
        for(endpoint in c("hospital","day30")){
          if(src=="eICU"&&endpoint=="day30")next
          yy<-rr[[paste0("death_",endpoint)]][ix];ok<-!is.na(yy)
          bygroup[[length(bygroup)+1L]]<-data.frame(source=src,concept=a,group=group,endpoint=endpoint,
            cohort_n=length(ix),endpoint_known_n=sum(ok),total_events=if(any(ok))sum(yy,na.rm=TRUE)else NA_integer_,
            positive_documented_n=if(enabled)sum(pos)else NA_integer_,
            positive_endpoint_known_n=if(enabled)sum(pos&ok)else NA_integer_,
            positive_events=if(enabled)sum(yy[pos],na.rm=TRUE)else NA_integer_)
        }
      }
    }
    list(records=private,evidence=evidence,latest=latest,patients=do.call(rbind,patients),
      summary=do.call(rbind,summ),outcomes=do.call(rbind,bygroup))
  }
  preflight<-function(){
    tx<-c("DNR","DNI","DNR / DNI","Full code","Comfort measures only","consider comfort care","not DNR","No augmentation of care")
    e<-data.frame(source="MIMIC",stay_id=as.character(seq_along(tx)),stream="MIMIC_chartevents",field="Code Status",value=tx,chart_hour=12,entry_hour=13,status="")
    z<-classify_events(e)
    stopifnot(identical(z$row[z$concept=="comfort_only"],5L),identical(z$row[z$concept=="dnr"],c(1L,3L)),identical(z$row[z$concept=="dni"],c(2L,3L)),!any(z$row%in%c(4L,6L,7L)))
    e$chart_hour<-c(48,48,10,0,-1,NA,49,8);e$entry_hour<-c(48,49,NA,0,1,1,49,9);e$source[4]<-"eICU"
    t<-timing(e);stopifnot(identical(which(t$by_T48),c(1L,4L,8L)),!t$positive_offset[4],t$late_entry[2],!t$time_known[3],t$negative_time[5])
    q<-data.frame(source="eICU",stay_id="x",stream=c("eICU_treatment","eICU_treatment","eICU_carePlanEOL","eICU_treatment"),
      field=c("Active treatment documentation","Active treatment documentation","EOL discussion","Active treatment documentation"),
      value=c("other|palliative care consultation","other|planned comfort care","documented","comfort / function (pain)"),chart_hour=5,entry_hour=6,status="")
    z<-classify_events(q);stopifnot(sum(z$concept=="palliative_consult")==1L,sum(z$concept=="eol_discussion")==1L,!any(z$concept=="comfort_only"))
    stopifnot(nrow(classify_events(empty_events()))==0L,identical(binary(factor(c("0","1"),levels=c("1","0"))),0:1))
    # Exercise the actual field variants seen in the returned public item dictionary.
    fields<-c("Code Status.","Care Limitation","Code Status (Intubation)","Code Status (Intubation)","Status and Comfort")
    qe<-data.frame(source=c("MIMIC","eICU","MIMIC","MIMIC","MIMIC"),stay_id=as.character(1:5),
      stream=c("MIMIC_chartevents","eICU_carePlanGeneral",rep("MIMIC_chartevents",3)),field=fields,
      value=c("DNR / DNI","Do not resuscitate","DNI","Intubate","Comfort care"),chart_hour=12,entry_hour=13,status="")
    qm<-classify_events(qe)
    stopifnot(identical(qm$row[qm$concept=="dnr"],c(1L,2L)),identical(qm$row[qm$concept=="dni"],c(1L,3L)),
      !any(qm$concept=="comfort_only"),!code_field("Code Status (Intubation)"),code_field("Care Limitation"))
    qr<-qe[c("source","stay_id")];qs<-latest_state(qe,timing(qe),qr,code_field(qe$field),decode_status(qe$value),"none")
    stopifnot(qs[1]=="dnr_dni",qs[2]=="dnr",qs[3]=="none",qs[4]=="none")
    qi<-latest_state(qe,timing(qe),qr,intubation_field(qe$field),decode_intubation(qe$value),"none")
    stopifnot(qi[3]=="dni",qi[4]=="intubation_permitted")
    TRUE
  }

  main<-function(){
    current_module<<-"preflight";logmsg("Step15-2预检：字段别名、Care Limitation最后状态、独立插管状态、Full code、时间边界和空输入。")
    stopifnot(preflight());txt_out("PASS: strict code-value mappings; DNR/DNI distinct from comfort-only care; timing boundaries and missing times; no replacement of missing outcome labels.","to_share/02_preflight.txt")
    if(!requireNamespace("DBI",quietly=TRUE)||!requireNamespace("duckdb",quietly=TRUE))stop("需要已用于Step4/5的DBI和duckdb包。")
    for(step in c(4,13,14)){
      p<-file.path(cfg$OUTPUT_PARENT,paste0("Step",step),"to_share",paste0("Step",step,"_STATUS.txt"))
      if(!file.exists(p)||!"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(p,warn=FALSE))stop("请先完成Step",step,"。")
    }
    dbpath<-file.path(cfg$OUTPUT_PARENT,"Step4/local_only/Step4_harmonized_features.duckdb")
    if(!file.exists(dbpath))stop("缺少Step4冻结数据库。")
    con<-DBI::dbConnect(duckdb::duckdb(),dbdir=":memory:")
    spill<-tempfile("AMI_EDRS_step15_");dir.create(spill)
    on.exit({try(DBI::dbDisconnect(con,shutdown=TRUE),silent=TRUE);unlink(spill,recursive=TRUE)},add=TRUE)
    q<-function(x)as.character(DBI::dbQuoteString(con,as.character(x)))
    get<-function(sql)DBI::dbGetQuery(con,sql);run<-function(sql)DBI::dbExecute(con,sql)
    run("SET threads=1");run("SET preserve_insertion_order=false");run(paste("SET memory_limit =",q(cfg$MEMORY_LIMIT)));run(paste("SET temp_directory =",q(spill)))
    # Reject broken CSVs instead of silently skipping records or changing quoting rules.
    csvcheck<-file.path(spill,"csv_preflight.csv");writeLines(c('id,text','1,"quoted, comma"','2,"two ""quotes"""'),csvcheck)
    checked<-get(paste("SELECT * FROM",raw_csv_sql(csvcheck,q)))
    if(nrow(checked)!=2L||checked$text[1]!="quoted, comma"||checked$text[2]!='two "quotes"')stop("CSV读取规则预检失败。")
    writeLines(c('id,text','1,a,extra'),csvcheck)
    rejected<-tryCatch({get(paste("SELECT * FROM",raw_csv_sql(csvcheck,q)));FALSE},error=function(err)TRUE)
    if(!rejected)stop("CSV坏行未被严格拦截。")
    run(paste("ATTACH",q(dbpath),"AS frozen (READ_ONLY)"))
    m<-get("SELECT * FROM frozen.MIMIC_frozen_roster WHERE landmark='T48'")
    e<-get("SELECT * FROM frozen.eICU_frozen_roster WHERE landmark='T48'")
    req<-function(x,n,label){if(!all(n%in%names(x)))stop(label,"缺少字段：",paste(setdiff(n,names(x)),collapse=","))}
    req(m,c("subject_id","hadm_id","stay_id","intime","analysis_eligible","in_index_icu_at_landmark","death_30d_after_landmark","death_in_index_hospital_after_landmark"),"MIMIC名单")
    req(e,c("patientunitstayid","analysis_eligible","in_index_icu_at_landmark","hospital_death_after_landmark"),"eICU名单")
    roster<-rbind(data.frame(source="MIMIC",stay_id=as.character(m$stay_id),in_index_icu=binary(m$in_index_icu_at_landmark),
      death_hospital=binary(m$death_in_index_hospital_after_landmark),death_day30=binary(m$death_30d_after_landmark)),
      data.frame(source="eICU",stay_id=as.character(e$patientunitstayid),in_index_icu=binary(e$in_index_icu_at_landmark),
      death_hospital=binary(e$hospital_death_after_landmark),death_day30=NA_integer_))
    if(nrow(m)!=5133L||nrow(e)!=5284L||sum(roster$death_hospital[roster$source=="MIMIC"])!=461L||sum(roster$death_hospital[roster$source=="eICU"])!=219L)stop("冻结T48人数或住院死亡数与Step13不一致。")
    if(any(binary(m$analysis_eligible)!=1L)||any(binary(e$analysis_eligible)!=1L)||anyNA(roster$stay_id)||anyDuplicated(paste(roster$source,roster$stay_id)))stop("冻结名单资格、ID或唯一性异常。")
    ids<-m[c("subject_id","hadm_id","stay_id","intime")];ids[]<-lapply(ids,as.character)
    if(anyNA(as.POSIXct(ids$intime,tz="UTC")))stop("MIMIC ICU起点时间无法解析。")
    DBI::dbWriteTable(con,"mimic_ids",ids);DBI::dbWriteTable(con,"eicu_ids",data.frame(stay_id=as.character(e$patientunitstayid)))
    before<-hash_object(list(roster,ids));csv_out(aggregate(cbind(n=rep(1L,nrow(roster)),deaths=roster$death_hospital),roster["source"],sum),"to_share/02_cohort.csv")
    saveRDS(roster,reg("local_only/frozen_T48_roster.rds"),compress="gzip")
    txt_out(capture.output(dput(c(plan,list(analysis_version=analysis_version)))),"to_share/02_analysis_plan.txt")
    input_state<-data.frame(file=dbpath,bytes=file.info(dbpath)$size,mtime=as.numeric(file.info(dbpath)$mtime),contents_checked="Frozen roster and time anchors",object_md5=before)
    csv_out(input_state,"to_share/01_inputs.csv")
    # Raw large files use path/size/mtime to avoid a second full disk scan; extracted records use an object MD5.
    fingerprint<-function(p){if(is.null(p))return(NULL);fi<-file.info(p);list(path=norm(p),bytes=fi$size,mtime=as.numeric(fi$mtime),
      md5=if(fi$size<=5e7)unname(tools::md5sum(p))else NA_character_)}
    locate<-function(src,stem,subdir=""){
      key<-paste(src,stem,sep=".");explicit<-cfg$RAW_FILES[[key]]
      if(!is.null(explicit)){if(length(explicit)!=1L||!nzchar(explicit))stop("RAW_FILES路径无效：",key);p<-norm(explicit);if(!file.exists(p))return(NULL);return(p)}
      root<-if(src=="MIMIC")cfg$RAW_MIMIC_DIR else cfg$RAW_EICU_DIR
      cand<-unique(c(file.path(root,subdir,paste0(stem,c(".csv.gz",".csv"))),file.path(root,paste0(stem,c(".csv.gz",".csv")))))
      p<-cand[file.exists(cand)]
      if(!length(p)&&dir.exists(root))p<-list.files(root,pattern=paste0("^",stem,"[.]csv([.]gz)?$"),recursive=TRUE,full.names=TRUE,ignore.case=TRUE)
      # Step15-1: empty subdir can yield both root//file and root/file.
      # Canonicalize existing candidates before counting; also covers recursive aliases.
      if(length(p))p<-unique(vapply(p,norm,character(1),USE.NAMES=FALSE))
      if(length(p)>1L)stop("找到多份不同的",key,"文件：\n",paste(p,collapse="\n"),
        "\n请通过options(ami_step15.config=list(RAW_FILES=list(...)))明确路径；键名为 ",key,"。")
      if(length(p))p[[1L]]else NULL
    }
    cache_read<-function(name,p,fn,required=FALSE,extra=NULL){
      key<-hash_object(list(plan=plan,roster=before,name=name,extra=extra,raw_configuration=cfg[c("RAW_MIMIC_DIR","RAW_EICU_DIR","RAW_FILES")]))
      rel<-paste0("local_only/cache/",name,".rds");cp<-safe_path(rel);fp<-fingerprint(p)
      if(file.exists(cp)&&!isTRUE(cfg$REFRESH_CACHE)){
        z<-readRDS(cp)
        if(!identical(z$key,key)||!identical(z$data_md5,hash_object(z$data)))stop("缓存版本/名单/完整性不符：",name,"；需要REFRESH_CACHE=TRUE后重新提取。")
        if(!is.null(fp)&&!identical(fp,z$source))stop("原始文件已变化：",name,"；需要REFRESH_CACHE=TRUE后重新提取。")
        logmsg(paste0("复用缓存：",name,if(is.null(fp))"（原始盘未挂载，使用已冻结提取内容）"else""))
        emit("01_raw_sources",stream=name,path=z$source$path,bytes=z$source$bytes,raw_mtime=z$source$mtime,
          raw_md5=z$source$md5,extraction_md5=z$data_md5,status=if(is.null(fp))"CACHED_SOURCE_NOT_MOUNTED"else"CACHED_SOURCE_CHECKED")
        return(z$data)
      }
      if(is.null(p)){
        emit("01_raw_sources",stream=name,path="",bytes=NA_real_,raw_mtime=NA_real_,raw_md5=NA_character_,extraction_md5=NA_character_,status="UNAVAILABLE")
        if(required)stop("缺少",name,"的原始文件和本步缓存。首次运行请接上T7；默认路径见脚本开头。")
        issue("SOURCE_GAP",name,"来源未找到，相关记录无法充分核查，不表示没有患者接受该照护。")
        return(NULL)
      }
      logmsg(paste0("读取 ",name,"；只保存冻结患者的候选照护记录。"))
      value<-fn(p)
      if(!identical(fp,fingerprint(p)))stop("提取期间原始文件发生变化：",name)
      z<-list(key=key,source=fp,data_md5=hash_object(value),data=value)
      target<-reg(rel);tmp<-paste0(target,".tmp");on.exit(unlink(tmp),add=TRUE);saveRDS(z,tmp,compress="gzip")
      if(!file.rename(tmp,target))stop("缓存写入失败：",name)
      emit("01_raw_sources",stream=name,path=fp$path,bytes=fp$bytes,raw_mtime=fp$mtime,raw_md5=fp$md5,extraction_md5=z$data_md5,status="EXTRACTED")
      value
    }
    read_query<-function(p,columns,query){
      run(paste0("CREATE OR REPLACE TEMP VIEW rawdata AS SELECT * FROM ",raw_csv_sql(p,q)))
      schema<-tolower(get("DESCRIBE rawdata")$column_name)
      if(!all(columns%in%schema))stop("原始表缺少字段：",paste(setdiff(columns,schema),collapse=","))
      get(query)
    }
    current_module<<-"MIMIC item dictionary"
    di<-cache_read("MIMIC_d_items",locate("MIMIC","d_items","icu"),function(p)read_query(p,c("itemid","label","linksto"),"SELECT itemid,label,linksto FROM rawdata"),TRUE)
    di$itemid<-as.character(di$itemid);di$linksto<-clean(di$linksto);di<-di[di$linksto%in%c("chartevents","procedureevents")&is_candidate(di$label),,drop=FALSE]
    if(anyNA(di$itemid)||anyDuplicated(di$itemid))stop("候选d_items标识不唯一。")
    DBI::dbWriteTable(con,"item_map",di);csv_out(di,"to_share/03_MIMIC_candidate_item_dictionary.csv")
    # Item labels are public dictionary entries. Patient text values are never copied to the feedback package.
    records<-list();availability<-list()
    get_stream<-function(stream,src,stem,subdir="",required=FALSE){
      current_module<<-stream
      rows<-cache_read(stream,locate(src,stem,subdir),function(p){
        z<-read_query(p,stream_columns[[stream]],query_for(stream));z$stay_id<-as.character(z$stay_id)
        for(k in c("field","value","status"))z[[k]]<-as.character(z[[k]])
        keep<-if(stream=="eICU_carePlanEOL")rep(TRUE,nrow(z))else is_candidate(paste(z$field,z$value))
        list(events=z[keep,,drop=FALSE],all_source_rows=nrow(z),all_source_patients=length(unique(z$stay_id)))
      },required,extra=if(src=="MIMIC")hash_object(di)else NULL)
      if(is.null(rows))return(list(events=empty_events(),available=FALSE,all_source_rows=NA_integer_,all_source_patients=NA_integer_))
      rows$available<-TRUE;rows
    }
    scope_for<-function(stream,fields){
      if(stream=="eICU_carePlanEOL")return("eol_discussion")
      if(stream=="eICU_treatment")return("comfort_only;palliative_consult")
      f<-clean(fields);ans<-character()
      if(any(code_field(f)|f%in%c("care limitation","care limitations"))||stream=="eICU_carePlanGeneral")ans<-c(ans,"comfort_only","dnr","dni","other_limit")
      if(any(f%in%c("comfort measures only","comfort care only","comfort care")))ans<-c(ans,"comfort_only")
      if(any(f%in%c("dnr","do not resuscitate")))ans<-c(ans,"dnr")
      if(any(f%in%c("dni","do not intubate"))||any(intubation_field(f)))ans<-c(ans,"dni")
      if(any(f%in%c("palliative care consult","palliative care consultation","palliative consult","palliative consultation","consults","consultation","consult type")))ans<-c(ans,"palliative_consult")
      if(any(f%in%c("end of life discussion","end-of-life discussion","eol discussion")))ans<-c(ans,"eol_discussion")
      paste(unique(ans),collapse=";")
    }
    specs<-list(c("MIMIC_chartevents","MIMIC","chartevents","icu"),c("MIMIC_procedureevents","MIMIC","procedureevents","icu"),
      c("eICU_carePlanGeneral","eICU","carePlanGeneral",""),c("eICU_carePlanEOL","eICU","carePlanEOL",""),c("eICU_treatment","eICU","treatment",""))
    for(sp in specs){
      st<-sp[1];required<-st%in%c("MIMIC_chartevents","eICU_carePlanGeneral")
      if(st=="MIMIC_procedureevents"&&!any(di$linksto=="procedureevents")){
        z<-list(events=empty_events(),available=FALSE,all_source_rows=NA_integer_,all_source_patients=NA_integer_)
        issue("SOURCE_GAP",st,"公开项目字典中未发现候选项目，未扫描该表。")
      }else z<-get_stream(st,sp[2],sp[3],sp[4],required)
      records[[st]]<-z$events
      fields<-if(sp[2]=="MIMIC")di$label[di$linksto==sp[3]]else unique(z$events$field)
      availability[[st]]<-data.frame(source=sp[2],stream=st,available=z$available,all_source_rows=z$all_source_rows,
        all_source_patients=z$all_source_patients,candidate_rows=nrow(z$events),candidate_patients=length(unique(z$events$stay_id)),concept_scope=scope_for(st,fields))
    }
    streams<-do.call(rbind,availability);all<-do.call(rbind,records);rownames(all)<-NULL
    current_module<<-"time-aware documentation analysis";logmsg("统计T48前明确记录、晚录入、缺失时间及最后一条代码状态；主队列和死亡结局保持完整。")
    result<-analyze_events(all,roster,streams)
    csv_out(streams,"to_share/03_source_coverage.csv")
    csv_out(result$summary,"to_share/04_care_documentation_summary.csv")
    csv_out(result$outcomes,"to_share/05_documentation_by_ICU_status_and_outcome.csv")
    latesttab<-as.data.frame(table(source=result$latest$source,latest_code_T48=result$latest$latest_code_T48),stringsAsFactors=FALSE)
    csv_out(latesttab,"to_share/04_last_documented_code_status.csv")
    csv_out(as.data.frame(table(source=result$latest$source,latest_intubation_T48=result$latest$latest_intubation_T48),stringsAsFactors=FALSE),"to_share/04_last_documented_intubation_status.csv")
    csv_out(code_audit(result$records),"to_share/03_code_field_mapping_audit.csv")
    csv_out(revision_comparison(result$summary,previous_run),"to_share/04_revision_count_comparison.csv")
    saveRDS(list(version=analysis_version,extraction_version=plan$version,roster_hash=before,plan=c(plan,list(analysis_version=analysis_version)),streams=streams,records=result$records,evidence=result$evidence,
      patient_documentation=result$patients,last_code=result$latest),reg("local_only/Step15_care_documentation.rds"),compress="gzip")
    csv_out(result$patients,"local_only/patient_documentation_flags.csv")
    csv_out(result$latest,"local_only/last_documented_code_status.csv")
    dd<-unique(result$records[c("source","stream","field","value","status","positive_concept_mapped","recognized_nonpositive_code","mapped","code_value","intubation_value")])
    csv_out(dd,"local_only/candidate_value_dictionary_FOR_LOCAL_REVIEW.csv")
    audits<-lapply(unique(result$records$stream),function(st){z<-result$records[result$records$stream==st,,drop=FALSE];data.frame(stream=st,
      retained_unique_rows=nrow(z),mapped_rows=sum(z$mapped),positive_concept_rows=sum(z$positive_concept_mapped),
      recognized_nonpositive_code_rows=sum(z$recognized_nonpositive_code),unmapped_rows=sum(!z$mapped),
      by_T48_rows=sum(z$by_T48),late_entry_rows=sum(z$late_entry),unknown_time_rows=sum(!z$time_known),zero_offset_rows=sum(z$zero_offset),negative_time_rows=sum(z$negative_time))})
    audit<-if(length(audits))do.call(rbind,audits)else data.frame(stream=character(),retained_unique_rows=integer(),mapped_rows=integer(),positive_concept_rows=integer(),recognized_nonpositive_code_rows=integer(),unmapped_rows=integer(),by_T48_rows=integer(),late_entry_rows=integer(),unknown_time_rows=integer(),zero_offset_rows=integer(),negative_time_rows=integer())
    csv_out(audit,"to_share/03_timing_and_mapping_audit.csv")
    if(any(audit$unmapped_rows>0))issue("REVIEW","structured value dictionary","部分宽筛候选记录未归入本步概念；其中可能包含非目标护理项目或待核对代码值。Full code已作为已识别非阳性单列；原文留在local_only。")
    if(any(audit$unknown_time_rows>0))issue("NOTE","time availability","时间不完整记录不纳入严格T48前计数，另报chart-time-only和时间未知计数。")
    # A complete sensitivity analysis requires judging whether CMO capture is adequate; this audit never chooses exclusions by performance.
    gate<-result$summary[result$summary$concept=="comfort_only",c("source","source_evaluated","recorded_by_T48_n","positive_time_unknown_n")]
    gate$next_action<-ifelse(!gate$source_evaluated,"Cannot assess without usable source",ifelse(gate$recorded_by_T48_n==0,"No explicit records found; ascertainment limits remain","Review mapping and capture, then consider a prespecified restricted-cohort sensitivity"))
    gate$automatic_exclusion_applied<-FALSE;csv_out(gate,"to_share/06_comfort_care_sensitivity_feasibility.csv")
    current_module<<-"large-font figures and three-line table";make_visuals(result$summary,roster)
    csv_out(data.frame(output=c("Figure_S_care_documentation_T48","Table_S_care_documentation_T48"),action="ADD supplementary; no existing result replaced",reviewer="R1 minor2; R2 minor2; R3 major11"),"to_share/11_figure_table_replacement_map.csv")
    txt_out(c("Step15-2：冻结T48患者的结构化照护限制记录审计。MIMIC仍为5133人，eICU仍为5284人；不删除或重标死亡，不拟合任何预测模型。",
      "严格计数要求记录时间及录入时间均有效，均位于ICU入科后0至48小时（含边界）；时间缺失、晚录入、负offset分开统计，绝不回填为入院时已知。",
      "MIMIC chartevents使用charttime和storetime；procedureevents使用starttime和storetime。eICU carePlanGeneral和treatment的offset是文档录入时间，不能推断真实治疗启动时刻。",
      "eICU carePlanEOL分别使用discussion和save offset；EOL讨论不等于舒适照护、DNR/DNI或姑息会诊。activeUponDischarge属于出院信息，未用于T48识别。",
      "eICU offset=0按原始记载纳入主计数，同时提供排除0的保守计数；不能把所有0解释为确切入科时刻。负offset不并入本次0-48小时分析。",
      "DNR、DNI、comfort-only、其他照护限制、姑息会诊文档、EOL讨论分别识别；一个记录可以同时支持DNR和DNI，但不自动推断comfort-only。",
      "只识别明确字段和值。计划、考虑、否定、历史和不明确值不自动作为阳性。姑息会诊文档不等于已验证会诊完成；单纯palliative specialty/疼痛comfort记录不作为comfort-only。",
      "Step15-2修复Code Status.尾部标点及Care Limitation字段规则不一致；阳性识别和最后状态使用同一组整体代码字段。",
      "Code Status (Intubation)单独识别DNI，独立汇报最后插管状态；Intubate只表示插管许可，不覆盖整体复苏状态，也不推断为Full code。其他未识别值保持未确定。",
      "mapped_rows包括阳性概念及明确Full code/Intubate非阳性记录；positive_concept_rows单列阳性。其余宽筛候选可能是非目标项目，不能把unmapped_rows解释为漏诊率。",
      "03_code_field_mapping_audit.csv只包含固定字段类别、标准化状态和计数；原始值和患者ID不进入反馈包。04_revision_count_comparison.csv对照本次运行前的汇总。",
      "本版只重算缓存内记录的分类和汇总。原始提取缓存版本保持Step15_v1，分析版本升级Step15_mapping_v2。",
      "记录过一次的ever-by-T48与最后一条code-status不同；后续未知值/同刻冲突会使最后状态保持未确定，不延续旧值。两者均保留，尚未自动据此定义排除人群。",
      "0表示在已检查来源内未发现本规则识别的阳性文档，不表示临床确定未接受照护；NA表示没有可评估来源。未分类文本保留local_only字典，不自动转阴性。",
      "来源限于指数ICU stay中的chartevents/候选procedureevents、carePlanGeneral、carePlanEOL和treatment。未全面审阅病房医嘱、自由文本会诊记录、其他ICU stay或护理大表；无法证明完整捕获姑息照护。",
      "carePlanGeneral可能跨相邻ICU stay携带既往信息；本步统计文档可获得性，不将其解释为本次首次决定。",
      "结局分层仅描述同一冻结队列：MIMIC分别列T48后30天和住院死亡，eICU仅住院死亡。未构造eICU的30天死亡标签。",
      "首次运行接上T7；候选chartevents需要一次原始大表扫描。提取结果缓存后可复用；缓存和冻结名单均校验，原始大文件仅核对路径/大小/修改时间，小文件另核MD5，避免第二次全盘扫描。",
      "若原始文件或名单改变，明确设置options(ami_step15.config=list(REFRESH_CACHE=TRUE))后重跑本步；不会触碰Step0-14。",
      "本步产出照护记录和可用性证据，尚不宣称已排除临终照护对预测表现的影响。先核对记录映射及可用率，再决定是否做排除T48前明确comfort-only记录者的敏感性；不排除所有DNR/DNI患者。",
      "新增补充图：18pt基础字号、PDF可编辑、PNG350dpi；三线表PDF/RTF和完整CSV。只回传Step15_feedback.zip，患者记录与原文保留local_only。"),"to_share/00_READ_ME_FIRST.txt")
    txt_out(c("https://mimic.mit.edu/docs/iv/modules/icu/chartevents.html","https://mimic.mit.edu/docs/iv/modules/icu/procedureevents.html",
      "https://eicu.mit.edu/eicutables/careplangeneral/","https://eicu.mit.edu/eicutables/careplaneol/","https://eicu.mit.edu/eicutables/treatment/",
      "https://github.com/MIT-LCP/eicu-code/issues/49","https://www.duckdb.org/docs/lts/data/csv/overview"),"to_share/10_definition_sources.txt")
    if(!identical(before,hash_object(list(roster,ids))))stop("运行中冻结名单被改动。")
    if(file.info(dbpath)$size!=input_state$bytes||as.numeric(file.info(dbpath)$mtime)!=input_state$mtime)stop("运行中Step4上游数据库发生变化。")
    status<<-"COMPLETED_FOR_REVIEW"
  }

  make_visuals<-function(summary,roster){
    draw<-function(){
      cols<-c(MIMIC="#4F7FC4",eICU="#D98C57");par(mar=c(5.8,15,3.2,2),oma=c(3.4,0,0,0),las=1,family="sans",cex=1,cex.axis=1,cex.lab=1.05)
      pct<-100*summary$recorded_by_T48_n/summary$cohort_n;hi<-max(10,ceiling(max(c(0,pct),na.rm=TRUE)/10)*10)
      plot(NA,xlim=c(0,hi*1.22),ylim=c(.3,6.9),xlab="Patients with explicit documentation (%)",ylab="",yaxt="n",xaxt="n",bty="n",xaxs="i")
      ticks<-pretty(c(0,hi),n=5);ticks<-ticks[ticks>=0&ticks<=hi];abline(v=ticks,col="#E5E8EC",lwd=.8)
      axis(1,at=ticks);axis(2,at=6:1,labels=unname(labels[concepts]),tick=FALSE,las=1,cex.axis=1)
      for(src in names(cols))for(j in seq_along(concepts)){
        z<-summary[summary$source==src&summary$concept==concepts[j],,drop=FALSE];yy<-7-j+if(src=="MIMIC").16 else -.16
        if(nrow(z)!=1L)stop("照护绘图汇总不完整。")
        if(!z$source_evaluated){text(0,yy,"Source not established",adj=0,col=cols[src],cex=.85)}else{
          x<-100*z$recorded_by_T48_n/z$cohort_n;segments(0,yy,x,yy,lwd=3,col=cols[src]);points(x,yy,pch=16,cex=1.1,col=cols[src])
          text(x+hi*.025,yy,sprintf("%s (%.1f%%)",z$recorded_by_T48_n,x),adj=0,cex=.86,col=cols[src])
        }
      }
      title("Care-related documentation available by T48",adj=0,cex.main=1.1)
      legend("topright",legend=names(cols),col=cols,pch=16,bty="n",cex=.95)
      mtext("Counts describe records in checked sources; no record does not establish clinical absence.",side=1,outer=TRUE,line=.3,cex=.83)
      mtext("DNR, DNI, comfort-only care, consultation and EOL discussion are separate concepts.",side=1,outer=TRUE,line=1.5,cex=.83)
    }
    save_figure("Figure_S_care_documentation_T48",draw,13,8.8)
    tabs<-list()
    for(src in c("MIMIC","eICU")){
      z<-summary[summary$source==src,,drop=FALSE];z<-z[match(concepts,z$concept),]
      nn<-function(n)ifelse(is.na(n),"Not established",sprintf("%s (%.1f%%)",n,100*n/z$cohort_n))
      tabs[[paste0(src,": n = ",z$cohort_n[1])]]<-data.frame(Concept=unname(labels[concepts]),
        `By T48, n (%)`=nn(z$recorded_by_T48_n),`Without zero offsets`=nn(z$excluding_zero_offset_n),
        `Only after T48, n`=ifelse(is.na(z$recorded_only_after_T48_n),"Not established",z$recorded_only_after_T48_n),check.names=FALSE)
    }
    three_line(tabs,"Time-stamped care-related documentation","Table_S_care_documentation_T48",c(
      "By T48 requires both recorded and entry times in 0-48 hours. Columns can overlap across concepts.",
      "No documentation is not confirmed absence. Without zero offsets additionally excludes eICU time=0.",
      "Only after T48: later timed positive records without a qualifying positive record by T48.",
      "Palliative consultation entries do not prove completed consultation. EOL discussion is not comfort care.",
      "The main cohort and all deaths are retained. Source availability and unclassified values are in CSV."))
  }

  if(isTRUE(getOption("ami_step15.functions_only",FALSE)))return(invisible(list(main=main,preflight=preflight,classify_events=classify_events,
    timing=timing,decode_status=decode_status,analyze_events=analyze_events,query_for=query_for,raw_csv_sql=raw_csv_sql,make_visuals=make_visuals)))
  status<-"FAILED";current_module<-"input verification";failure_message<-NULL
  tryCatch(withCallingHandlers(main(),warning=function(w){add_warning(conditionMessage(w),current_module);invokeRestart("muffleWarning")}),error=function(err){
    status<<-"FAILED";failure_message<<-conditionMessage(err);issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step15_error_detail.txt")})
  flush_warnings()
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step15-2",
    "COMPLETED_FOR_REVIEW表示照护文档审计已完成并等待映射及来源核对，不表示完整识别全部姑息照护。",
    "FAILED表示本次提取不完整；检查日志，不据此作排除患者的决定。"),"to_share/Step15_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step15_sessionInfo.txt");logmsg(paste0("Step15状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step15_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share)else{
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step15_feedback.zip"),"\n",sep="")else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
