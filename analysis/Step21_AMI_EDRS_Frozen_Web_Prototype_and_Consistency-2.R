# AMI-EDRS revision | Step21: frozen research web prototype and prediction consistency
# source(file.choose(), encoding="UTF-8")
# Fixed filenames; replaces owned results. Patient-level data and frozen references stay local_only.
local({
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MAKE_ZIP=TRUE,INSTALL_SHINY=TRUE,RUN_SHINY_TESTS=TRUE,EXPORT_PRIVATE_CSV=FALSE)
  cfg<-utils::modifyList(cfg,getOption("ami_step21.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step21")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step21输出目录。")
  if (is_link(out)) stop("Step21输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP21")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step21目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP21_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP21_MANAGED_OUTPUT")) {
    stop("Step21输出目录的归属标记不正确。")
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
        if (unlink(p) != 0L) stop("不能覆盖旧Step21输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step21_run_log.txt")
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
    txt_out(if(nrow(z))unlist(Map(function(a,b)c(a,b,""),z$scope,z$message))else"No warnings in Step21.","local_only/Step21_warning_detail.txt")
    a<-if(nrow(z))aggregate(list(n=rep(1L,nrow(z))),list(scope=z$scope),sum)else data.frame(scope=character(),n=integer())
    csv_out(a,"to_share/09_warning_summary.csv")
  }

  PREDICTOR_CODE <- r"----(  ami_api <- local({
    fail<-function(s)stop(s,call.=FALSE)
    need<-function(x,nm,label){miss<-setdiff(nm,names(x));if(length(miss))fail(paste0(label,': missing ',paste(utils::head(miss,5),collapse=', ')))}
    num<-function(x,label,allow_na=TRUE){
      if(is.factor(x))x<-as.character(x)
      if(is.character(x)){x<-trimws(x);x[x%in%c('','NA')]<-NA_character_;v<-suppressWarnings(as.numeric(x));if(any(!is.na(x)&is.na(v)))fail(paste('Invalid numeric field:',label))}else if(is.numeric(x)||is.logical(x))v<-as.numeric(x)else fail(paste('Unsupported field type:',label))
      if(any(is.infinite(v)|is.nan(v))||(!allow_na&&anyNA(v)))fail(paste('Missing or nonfinite field:',label));v
    }
    bit<-function(x,label,allow_na=FALSE){v<-num(x,label,allow_na);if(any(!is.na(v)&!v%in%0:1))fail(paste('Expected 0 or 1:',label));v}
    sex<-function(x){
      if(is.factor(x))x<-as.character(x)
      if(is.character(x)){z<-tolower(trimws(x));z[z%in%c('','na','missing','unknown')]<-NA_character_;z[z%in%c('m','male')]<-'1';z[z%in%c('f','female')]<-'0';x<-z}
      bit(x,'sex_male',TRUE)
    }
    plain<-function(x,path='model'){
      if(is.environment(x)||is.function(x)||isS4(x)||typeof(x)%in%c('externalptr','weakref','language','symbol','expression','pairlist'))fail(paste('Non-parameter object:',path))
      at<-attributes(x);if(!is.null(at))for(nm in names(at))if(!nm%in%c('names','dim','dimnames','row.names','class'))fail(paste('Unexpected attribute:',path,nm))
      if(inherits(x,'formula'))fail('Formula environments must not be exported.')
      if(is.list(x))for(j in seq_along(x))plain(x[[j]],paste0(path,'/',if(is.null(names(x)))j else names(x)[j]))
      invisible(TRUE)
    }
    prep<-function(x,p){
      need(x,p$features,'Predictor table');z<-x[p$features]
      for(nm in p$features){v<-num(z[[nm]],nm);z[[nm]]<-ifelse(is.na(v),p$median[[nm]],v)}
      for(nm in p$indicators)z[[paste0(nm,'__NA')]]<-as.integer(is.na(x[[nm]]))
      z<-z[p$design_columns]
      for(nm in names(z))z[[nm]]<-(z[[nm]]-p$center[[nm]])/p$scale[[nm]]
      ans<-as.matrix(z);if(any(!is.finite(ans)))fail('Nonfinite standardized design.');ans
    }
    percentile<-function(x,r){
      ans<-rep(NA_real_,length(x));ok<-which(!is.na(x));if(!r$enabled||!length(ok))return(ans)
      j<-findInterval(x[ok],r$values);v<-c(0,r$cumulative)[j+1L];k<-match(x[ok],r$values);eq<-!is.na(k)
      v[eq]<-v[eq]-.5*r$count[k[eq]];v<-100*v/r$n;if(r$direction=='low')v<-100-v
      ans[ok]<-pmax(0,pmin(100,v));ans
    }
    scores<-function(raw,o){
      ref<-o$state$reference;s<-ref$spec;ans<-list()
      for(w in c('0-6h','0-24h','0-48h')){
        m<-matrix(NA_real_,nrow(raw),nrow(s))
        for(j in seq_len(nrow(s))){nm<-paste(s$variable[j],gsub('-','_',w,fixed=TRUE),s$stat[j],sep='__');m[,j]<-percentile(raw[[nm]],ref$reference[[j]])}
        for(d in unique(s$domain)){ii<-which(s$domain==d);mm<-m[,ii,drop=FALSE];n<-rowSums(!is.na(mm));v<-rowMeans(mm,na.rm=TRUE);v[n<ceiling(length(ii)*o$min_domain_fraction)]<-NA_real_;v[is.nan(v)]<-NA_real_;ans[[paste(d,gsub('-','_',w,fixed=TRUE),'score',sep='__')]]<-v}
      }
      as.data.frame(ans,check.names=FALSE)[o$state$score_columns]
    }
    validate_model<-function(o){
      plain(o);need(o,c('version','model_id','endpoint','source_md5','state','model','contract','min_domain_fraction'),'Model')
      if(!identical(o$version,'AMI_WEB21_v1')||!o$endpoint%in%c('30_day_all_cause_death_after_T48','in_hospital_death_after_T48'))fail('Unsupported model version or endpoint.')
      if(!is.numeric(o$min_domain_fraction)||length(o$min_domain_fraction)!=1L||!is.finite(o$min_domain_fraction)||o$min_domain_fraction<=0||o$min_domain_fraction>1)fail('Invalid score coverage threshold.')
      for(p in list(o$model$preprocessor,o$state$cluster_preprocessor)){
        need(p,c('features','median','indicators','center','scale','design_columns'),'Preprocessor')
        if(!length(p$features)||anyDuplicated(p$features)||!identical(names(p$median),p$features)||any(!is.finite(p$median))||
          !identical(p$design_columns,c(p$features,if(length(p$indicators))paste0(p$indicators,'__NA')else character()))||
          !all(p$indicators%in%p$features)||!identical(names(p$center),p$design_columns)||!identical(names(p$scale),p$design_columns)||
          any(!is.finite(p$center))||any(!is.finite(p$scale)|p$scale<=0))fail('Invalid frozen preprocessor.')
      }
      cf<-o$model$coefficients;if(!is.numeric(cf)||any(!is.finite(cf))||!identical(names(cf),c('(Intercept)',o$model$preprocessor$design_columns)))fail('Coefficient order mismatch.')
      ce<-o$state$cluster$centers;if(!is.matrix(ce)||!is.numeric(ce)||nrow(ce)!=o$state$cluster$k||any(!is.finite(ce))||!identical(colnames(ce),o$state$cluster_preprocessor$design_columns))fail('Invalid frozen cluster centers.')
      r<-o$state$reference;s<-r$spec;if(!is.data.frame(s)||length(r$reference)!=nrow(s)||!identical(names(r$reference),s$component_id))fail('Invalid score reference schema.')
      for(i in seq_len(nrow(s))){v<-r$reference[[i]];if(length(v$values)!=length(v$count)||length(v$count)!=length(v$cumulative)||any(!is.finite(v$values))||any(diff(v$values)<=0)||any(!is.finite(v$count)|v$count<1|v$count!=floor(v$count))||!isTRUE(all.equal(v$cumulative,cumsum(v$count),check.attributes=FALSE))||sum(v$count)!=v$n||!identical(v$direction,as.character(s$direction[i]))||length(v$enabled)!=1L||is.na(v$enabled)||!v$direction%in%c('low','high')||(v$enabled&&v$n<1))fail('Invalid frozen score reference.')}
      if(anyDuplicated(o$contract$columns)||!all(c('age','sex_male','in_index_icu',o$contract$raw_columns)%in%o$contract$columns)||!all(o$state$dynamic_columns%in%o$contract$raw_columns))fail('Invalid feature contract.')
      invisible(TRUE)
    }
    same_number<-function(a,b){
      # Numeric equality plus signed-zero preservation; missingness is checked separately.
      ok<-!is.na(a)&!is.na(b);ans<-is.na(a)&is.na(b)
      ans[ok]<-a[ok]==b[ok];zz<-which(ok&a==0&b==0)
      if(length(zz))ans[zz]<-(1/a[zz])==(1/b[zz]);ans
    }
    write_csv<-function(x,path){
      if(!is.data.frame(x)||!ncol(x)||anyDuplicated(names(x)))fail('CSV export requires a data frame with unique columns.')
      z<-x
      for(nm in names(z)){
        v<-z[[nm]]
        if(is.factor(v))v<-as.character(v)
        if(is.numeric(v)||is.logical(v)){
          if(is.object(v))fail(paste('CSV export requires plain numeric fields:',nm))
          v<-num(v,nm)
          # C99 hexadecimal floats preserve the binary significand, avoiding the
          # decimal-accumulation rounding in R 4.3 on 64-bit-long-double platforms.
          s<-sprintf('%a',v)
          dp<-Sys.localeconv()[['decimal_point']];if(nzchar(dp)&&dp!='.')s<-sub(dp,'.',s,fixed=TRUE)
          # Keep exact small integers human-readable, with no decimal exponent.
          whole<-which(!is.na(v)&v==trunc(v)&abs(v)<=2^53)
          if(length(whole))s[whole]<-sprintf('%.0f',v[whole])
          # Old R hex readers can overflow their scale for exponents below -1022.
          # Canonical subnormals use 0x0.<13 fraction hex digits>p-1022 instead.
          tiny<-which(!is.na(v)&abs(v)>0&abs(v)<.Machine$double.xmin)
          if(length(tiny)){
            if(.Machine$double.base!=2L||.Machine$double.digits!=53L)fail('CSV encoding requires IEEE binary64 doubles.')
            bytes<-matrix(as.character(writeBin(v[tiny],raw(),size=8L,endian='big')),nrow=8L)
            bits<-do.call(paste0,lapply(seq_len(8L),function(j)bytes[j,]))
            s[tiny]<-paste0(ifelse(v[tiny]<0,'-',''),'0x0.',substring(bits,4L),'p-1022')
          }
          s[is.na(v)]<-NA_character_
          if(!all(same_number(v,num(s,nm))))fail(paste('CSV numeric encoding is not exact on this R platform:',nm))
          z[[nm]]<-s
        }else if(is.character(v))z[[nm]]<-v else fail(paste('Unsupported CSV export field:',nm))
      }
      # Exact integer/hex text is read by the existing num() path; inference is unchanged.
      utils::write.csv(z,path,row.names=FALSE,na='',fileEncoding='UTF-8')
      invisible(path)
    }
    csv_audit<-function(before,after){
      if(!is.data.frame(before)||!is.data.frame(after)||!identical(dim(before),dim(after))||!identical(names(before),names(after)))fail('CSV row count or column order changed.')
      rows<-lapply(names(before),function(nm){
        a<-before[[nm]];b<-after[[nm]];numeric_field<-is.numeric(a)||is.logical(a)
        if(numeric_field){a<-num(a,nm);b<-num(b,nm);same<-same_number(a,b)}else{
          a<-as.character(a);b<-as.character(b);same<-(is.na(a)&is.na(b))|(!is.na(a)&!is.na(b)&a==b)
        }
        both<-!is.na(a)&!is.na(b);missing_bad<-sum(is.na(a)!=is.na(b))
        data.frame(field=nm,n=length(a),numeric_field=numeric_field,missing_before=sum(is.na(a)),missing_after=sum(is.na(b)),
          missingness_mismatches=missing_bad,value_mismatches=sum(!same&both),
          max_abs_error=if(numeric_field&&any(both))max(abs(a[both]-b[both]))else if(numeric_field)0 else NA_real_,
          exact_match=all(same),stringsAsFactors=FALSE)
      });do.call(rbind,rows)
    }
    read_csv<-function(path){
      if(!file.exists(path))fail('CSV file is unavailable.')
      if(file.info(path)$size>100*1024^2)fail('CSV exceeds 100 MB.')
      x<-utils::read.csv(path,check.names=FALSE,stringsAsFactors=FALSE,colClasses='character',na.strings=c('','NA'),fileEncoding='UTF-8-BOM',blank.lines.skip=FALSE)
      if(!nrow(x)||nrow(x)>10000L)fail('CSV must contain 1-10000 rows.');x
    }
    validate_input<-function(x,o){
      if(!is.data.frame(x)||!nrow(x)||nrow(x)>10000L||anyDuplicated(names(x)))fail('Input must have 1-10000 rows and unique column names.')
      ct<-o$contract;need(x,ct$columns,'Feature CSV');extra<-setdiff(names(x),ct$columns);if(length(extra))fail(paste('Unexpected columns:',paste(utils::head(extra,5),collapse=', ')))
      if(anyNA(x$contract_id)||any(x$contract_id!=ct$id))fail('Input contract version does not match the frozen model.')
      if(any(num(x$landmark_hours,'landmark_hours',FALSE)!=48)||any(bit(x$icu_admitted_AMI,'icu_admitted_AMI')!=1)||any(bit(x$alive_at_T48,'alive_at_T48')!=1))fail('This model requires ICU-admitted AMI and survival to the 48-hour landmark.')
      ih<-bit(x$in_index_hospital_at_T48,'in_index_hospital_at_T48',allow_na=o$endpoint!='in_hospital_death_after_T48');if(o$endpoint=='in_hospital_death_after_T48'&&any(ih!=1))fail('Hospital endpoint requires presence in the index hospital at T48.')
      raw<-x[ct$raw_columns];for(nm in names(raw))raw[[nm]]<-num(raw[[nm]],nm)
      for(i in seq_len(nrow(ct$dictionary))){d<-ct$dictionary[i,];if(!d$field%in%names(raw))next;v<-raw[[d$field]];if(any(v<d$minimum|v>d$maximum,na.rm=TRUE))fail(paste('Feature outside declared bounds:',d$field))}
      base<-x[c('age','sex_male','in_index_icu')];base$age<-num(base$age,'age');base$sex_male<-sex(base$sex_male);base$in_index_icu<-bit(base$in_index_icu,'in_index_icu')
      if(any(base$age<18|base$age>120,na.rm=TRUE)||any(base$in_index_icu>ih,na.rm=TRUE))fail('Age or ICU/hospital landmark state is invalid.')
      # A wholly empty row is a blank template, not a feature-engineered patient.
      if(any(rowSums(!is.na(raw))==0L))fail('All physiology fields are missing in at least one row; no risk is produced.')
      list(raw=raw,baseline=base)
    }
    infer<-function(x,o){
      validate_model(o);d<-validate_input(x,o);sc<-scores(d$raw,o);cx<-prep(sc,o$state$cluster_preprocessor);ce<-o$state$cluster$centers
      ds<-outer(rowSums(cx*cx),rowSums(ce*ce),'+')-2*tcrossprod(cx,ce);cl<-max.col(-ds,ties.method='first')
      design<-cbind(d$baseline,d$raw[o$state$dynamic_columns],sc);rownames(design)<-NULL
      xx<-prep(design,o$model$preprocessor);lp<-as.numeric(cbind(1,xx)%*%o$model$coefficients);pr<-stats::plogis(lp)
      if(any(!is.finite(pr))||any(pr<0|pr>1))fail('Model did not produce finite probabilities.')
      needed<-intersect(o$model$preprocessor$features,names(design));miss<-rowSums(is.na(design[needed]));fraction<-miss/length(needed)
      data.frame(row_number=seq_len(nrow(x)),risk_probability=pr,endpoint=o$endpoint,model_id=o$model_id,
        model_source_md5=o$source_md5,calibration='Frozen MIMIC; no local recalibration',
        frozen_cluster=paste0('C',cl),imputed_input_count=miss,imputed_input_fraction=fraction,
        score_domains_observed=rowSums(!is.na(sc)),score_domains_total=ncol(sc),stringsAsFactors=FALSE)
    }
    load_bundle<-function(root,endpoint){
      map<-c(mortality30='model_30day.rds',hospital='model_hospital.rds');if(length(endpoint)!=1L||is.na(endpoint)||!endpoint%in%names(map))fail('Select a supported endpoint.')
      mf<-file.path(root,'model_manifest.csv');if(!file.exists(mf))fail('Model manifest is unavailable; no risk is produced.')
      m<-utils::read.csv(mf,stringsAsFactors=FALSE);need(m,c('file','md5','model_id','endpoint'),'Model manifest');j<-which(m$file==unname(map[endpoint]));if(length(j)!=1L)fail('Model manifest is ambiguous.')
      p<-file.path(root,unname(map[endpoint]));if(!file.exists(p)||!identical(unname(tools::md5sum(p)),as.character(m$md5[j])))fail('Frozen model is missing or changed; no risk is produced.')
      o<-readRDS(p);validate_model(o);expected<-if(endpoint=='mortality30')'30_day_all_cause_death_after_T48' else 'in_hospital_death_after_T48'
      if(!identical(o$endpoint,expected)||!identical(o$model_id,as.character(m$model_id[j]))||!identical(o$endpoint,as.character(m$endpoint[j])))fail('Endpoint and frozen model do not match.');o
    }
    request<-function(root,endpoint,x,confirmed=FALSE){
      if(!isTRUE(confirmed))fail('Confirm feature engineering, units, and population eligibility before prediction.')
      o<-load_bundle(root,endpoint);infer(x,o)
    }
    controller<-function(root){
      current<-NULL;error<-NULL
      list(clear=function(){current<<-NULL;error<<-NULL;invisible(NULL)},result=function()current,error=function()error,
        run=function(endpoint,x,confirmed){current<<-NULL;error<<-NULL;tryCatch({current<<-request(root,endpoint,x,confirmed);TRUE},error=function(e){error<<-conditionMessage(e);FALSE})})
    }
    list(validate_model=validate_model,validate_input=validate_input,read_csv=read_csv,write_csv=write_csv,csv_audit=csv_audit,predict=infer,
      load_bundle=load_bundle,request=request,controller=controller,plain=plain,sex=sex,prep=prep,scores=scores,percentile=percentile)
  })
)----"
  eval(parse(text=PREDICTOR_CODE))

  need<-function(x,nm,label) { if(!all(nm%in%names(x))) stop(label,"缺少字段：",paste(setdiff(nm,names(x)),collapse=", ")) }
  hash_object<-function(x){f<-tempfile();on.exit(unlink(f));saveRDS(x,f,version=2);unname(tools::md5sum(f))}
  check_status<-function(st){p<-file.path(cfg$OUTPUT_PARENT,st,'to_share',paste0(st,'_STATUS.txt'));if(!file.exists(p)||!'STATUS = COMPLETED_FOR_REVIEW'%in%readLines(p,warn=FALSE))stop('请先完成',st,'。')}
  check_lineage<-function(st){check_status(st);p<-file.path(cfg$OUTPUT_PARENT,st,'to_share/01_inputs.csv');if(!file.exists(p))stop(st,'缺少输入审计。');z<-utils::read.csv(p,stringsAsFactors=FALSE,colClasses='character');need(z,c('file','md5'),st)
    if(!nrow(z)||anyNA(z$file)||anyNA(z$md5)||any(!file.exists(z$file))||any(unname(tools::md5sum(z$file))!=z$md5))stop(st,'原输入已改变或不可访问。');invisible(z)}
  contract_for<-function(o,rules){
    s<-o$state$reference$spec
    cols<-unique(c(o$state$dynamic_columns,unlist(lapply(c('0_6h','0_24h','0_48h'),function(w)paste(s$variable,w,s$stat,sep='__')),use.names=FALSE)))
    need(rules,c('variable','unit','lower','upper'),'单位约定')
    dd<-lapply(cols,function(nm){a<-strsplit(nm,'__',fixed=TRUE)[[1]];j<-match(a[1],rules$variable);if(is.na(j))stop('缺少单位约定：',a[1]);lo<-as.numeric(rules$lower[j]);hi<-as.numeric(rules$upper[j]);unit<-as.character(rules$unit[j])
      if(a[3]=='abnormal_fraction'){lo<-0;hi<-1;unit<-'fraction (0-1)'}else if(a[3]=='sd_value'){lo<-0;hi<-Inf}else if(a[3]=='slope_per_hour'){lo<--Inf;hi<-Inf;unit<-paste0(unit,' per hour')}
      data.frame(field=nm,variable=a[1],window=gsub('_','-',a[2],fixed=TRUE),statistic=a[3],unit=unit,minimum=lo,maximum=hi,missing_allowed=TRUE,stringsAsFactors=FALSE)})
    dictionary<-do.call(rbind,dd);meta<-c('contract_id','landmark_hours','icu_admitted_AMI','alive_at_T48','in_index_hospital_at_T48','age','sex_male','in_index_icu')
    list(id=paste0('AMI_T48_FEATURES_V1_',substr(hash_object(list(meta,cols,dictionary)),1,12)),columns=c(meta,cols),raw_columns=cols,dictionary=dictionary,
      note='Unscaled cumulative-window features from the frozen Step4/6 feature engineering; not three representative vital-sign measurements.')
  }
  minimize_model<-function(x,source,md5,rules){
    expected<-if(source=='Step7')'30_day_all_cause_death_after_T48' else 'in_hospital_death_after_T48';version<-if(source=='Step7')'Step7_v1' else 'Step13_v1'
    if(!identical(x$version,version)||!identical(x$endpoint,expected)||!identical(x$landmark,'T48')||!all(c('A','B','C','D')%in%names(x$models)))stop(source,'模型定义不符。')
    pfields<-c('version','features','median','indicators','center','scale','design_columns')
    pre<-function(p){need(p,pfields,'冻结预处理');p[pfields]}
    st<-x$state;ref<-st$reference;s<-ref$spec;keep<-s$tier=='main';if(anyNA(keep)||!any(keep))stop('主评分域定义异常。')
    rfields<-c('values','count','cumulative','n','enabled','direction','reference_column')
    rr<-lapply(ref$reference[keep],function(z){need(z,rfields,'冻结评分参考');z[rfields]})
    state<-list(reference=list(version=ref$version,spec=s[keep,,drop=FALSE],reference=rr),cluster_preprocessor=pre(st$cluster_preprocessor),
      cluster=list(k=st$cluster$k,centers=st$cluster$centers),dynamic_columns=st$dynamic_columns,baseline_columns=st$baseline_columns,score_columns=st$score_columns)
    o<-list(version='AMI_WEB21_v1',model_id=paste0('AMI_EDRS_',source,'_T48_C_',substr(md5,1,12)),endpoint=expected,source_md5=unname(md5),
      min_domain_fraction=x$plan$min_domain_fraction,state=state,model=list(preprocessor=pre(x$models$C$preprocessor),coefficients=x$models$C$coefficients,alpha=x$models$C$alpha,lambda=x$models$C$lambda))
    o$contract<-contract_for(o,rules);ami_api$validate_model(o);o
  }
  frame_from_bundle<-function(b,o){
    n<-nrow(b$raw_numeric);if(!n||n!=nrow(b$metadata)||n!=nrow(b$baseline)||anyDuplicated(b$metadata$patient_key)||anyNA(b$metadata$patient_key)||any(b$metadata$landmark!='T48'))stop('冻结输入患者键或T48不符。')
    ih<-if(all(b$metadata$source=='MIMIC'))as.numeric(as.character(b$outcomes$analysis_eligible_hospital_endpoint)) else rep(1,n)
    if(length(ih)!=n||any(!is.na(ih)&!ih%in%0:1))stop('住院终点资格字段异常。')
    d<-data.frame(contract_id=o$contract$id,landmark_hours=48,icu_admitted_AMI=1,alive_at_T48=1,in_index_hospital_at_T48=ih,stringsAsFactors=FALSE)
    d<-cbind(d,b$baseline[c('age','sex_male','in_index_icu')],b$raw_numeric[o$contract$raw_columns]);rownames(d)<-NULL;d
  }
  make_demo<-function(o){
    # Entirely synthetic values; no sampled patients or empirical training quantiles.
    means<-c(heart_rate=85,sbp=120,map=80,resp_rate=18,spo2=97,temperature=37,gcs_eye=4,gcs_verbal=5,gcs_motor=6,creatinine=1,bun=18,sodium=140,potassium=4,chloride=103,bicarbonate=24,glucose=110,wbc=8,hemoglobin=13,platelet=220,inr=1.1,ptt=30)
    d<-as.data.frame(setNames(replicate(length(o$contract$columns),rep(NA_real_,3),simplify=FALSE),o$contract$columns),check.names=FALSE)
    d$contract_id<-o$contract$id;d$landmark_hours<-48;d$icu_admitted_AMI<-1;d$alive_at_T48<-1;d$in_index_hospital_at_T48<-1
    d$age<-c(55,70,65);d$sex_male<-c(0,1,NA);d$in_index_icu<-c(0,1,1)
    for(nm in o$contract$raw_columns){sp<-strsplit(nm,'__',fixed=TRUE)[[1]];v<-means[[sp[1]]];if(is.null(v))stop('模拟示例缺少固定值：',sp[1]);st<-sp[3]
      if(st=='abnormal_fraction')z<-c(0,.25,.1)else if(st=='sd_value')z<-rep(if(startsWith(sp[1],'gcs_'))0 else abs(v)*.03,3)else if(st=='slope_per_hour')z<-c(0,0,0)else z<-rep(v,3)
      d[[nm]]<-z
    }
    # A few explicit missing cells exercise frozen imputation without an empty row.
    pick<-head(o$state$dynamic_columns,3);d[3,pick]<-NA_real_;ami_api$validate_input(d,o);d
  }
  compare_prediction<-function(a,b,label){
    if(!identical(dim(a),dim(b))||any(!is.finite(a))||any(!is.finite(b)))stop(label,'预测形状或数值异常。')
    z<-max(abs(a-b));if(z>1e-10)stop(label,'预测不一致：',format(z,digits=12));z
  }
  load_original<-function(engine_file,predict_file,predict_name){e<-new.env(parent=baseenv());e$plogis<-stats::plogis;sys.source(engine_file,envir=e);sys.source(predict_file,envir=e);if(!is.function(e[[predict_name]]))stop('冻结推理函数缺失。');e[[predict_name]]}
  ensure_shiny<-function(){
    packages<-c('shiny','testthat')
    missing<-packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
    if(length(missing)&&isTRUE(cfg$INSTALL_SHINY)){
      logmsg(paste0('安装网页及测试依赖：',paste(missing,collapse=', '),'；不涉及模型训练。'))
      utils::install.packages(missing,repos='https://cloud.r-project.org',dependencies=NA)
    }
    if(any(!vapply(packages,requireNamespace,logical(1),quietly=TRUE)))stop('需要shiny和testthat完成网页测试。请安装缺少的软件包后重跑本脚本。')
    if(utils::packageVersion('shiny')<'1.6.0')stop('请更新shiny至1.6.0或以上。')
    invisible(TRUE)
  }
  export_predictor<-function()PREDICTOR_CODE

  main<-function(){
    current_module<<-'upstream lineage'
    for(st in c('Step6','Step7','Step12','Step13','Step20'))check_lineage(st)
    rel<-c(mimic='Step6/local_only/MIMIC_T48_model_inputs.rds',eicu='Step6/local_only/eICU_T48_model_inputs.rds',
      model7='Step7/local_only/MIMIC_T48_frozen_pipeline.rds',engine7='Step7/local_only/Step7_score_engine.R',predict7='Step7/local_only/Step7_predict.R',
      model13='Step13/local_only/MIMIC_T48_hospital_frozen_pipeline.rds',engine13='Step13/local_only/Step13_score_engine.R',predict13='Step13/local_only/Step13_predict.R',
      saved12='Step12/local_only/eICU_T48_frozen_predictions.rds',saved13='Step13/local_only/eICU_T48_hospital_frozen_predictions.rds',
      units='Step6/to_share/03_component_units_and_thresholds.csv',flow20='Step20/to_share/03_cohort_flow.csv')
    paths<-setNames(file.path(cfg$OUTPUT_PARENT,rel),names(rel));if(any(!file.exists(paths)))stop('本地冻结输入缺失：',paste(names(paths)[!file.exists(paths)],collapse=', '))
    hashes<-unname(tools::md5sum(paths));names(hashes)<-names(paths);csv_out(data.frame(input=names(paths),file=unname(paths),md5=unname(hashes)),'to_share/01_inputs.csv')
    b<-readRDS(paths['mimic']);e<-readRDS(paths['eicu']);m7<-readRDS(paths['model7']);m13<-readRDS(paths['model13']);saved12<-readRDS(paths['saved12']);saved13<-readRDS(paths['saved13'])
    rules<-utils::read.csv(paths['units'],stringsAsFactors=FALSE);flow<-utils::read.csv(paths['flow20'],stringsAsFactors=FALSE)
    need(flow,c('cohort','n','events'),'Step20队列');ff<-flow[flow$cohort=='full_frozen',,drop=FALSE]
    if(!identical(b$version,'Step6_v1')||!identical(e$version,'Step6_v1')||!all(b$metadata$source=='MIMIC')||!all(e$metadata$source=='eICU')||
      !identical(b$feature_plan,e$feature_plan)||!identical(names(b$raw_numeric),names(e$raw_numeric))||m7$training_n!=nrow(b$metadata)||
      nrow(ff)!=1L||ff$n!=nrow(e$metadata)||ff$events!=sum(as.numeric(as.character(e$outcomes$event_primary))))stop('冻结队列/特征约定不能与Step20对齐。')
    hp<-as.numeric(as.character(b$outcomes$analysis_eligible_hospital_endpoint));hy<-as.numeric(as.character(b$outcomes$death_in_index_hospital_after_landmark));if(any(!is.na(hp)&!hp%in%0:1)||m13$training_n!=sum(hp%in%1 & !is.na(hy)))stop('住院模型训练人数不符。')
    if(!identical(hash_object(m13),saved13$model_hash))stop('Step13模型与保存预测的指纹不一致。')
    current_module<<-'minimal model export'
    logmsg('Step21-2：导出冻结模型C；使用无损数值编码并核验逐列往返，不重新训练。')
    o7<-minimize_model(m7,'Step7',hashes['model7'],rules);o13<-minimize_model(m13,'Step13',hashes['model13'],rules)
    if(!identical(o7$contract,o13$contract))stop('两个终点的输入约定不同，不能共用模板。')
    root<-dirname(reg('local_only/webapp/predictor.R'))
    txt_out(export_predictor(),'local_only/webapp/predictor.R');txt_out(SERVER_CODE,'local_only/webapp/server.R');txt_out(APP_CODE,'local_only/webapp/app.R');txt_out(SHINY_TEST_CODE,'local_only/webapp/tests_server.R')
    saveRDS(o7,reg('local_only/webapp/model_30day.rds'),version=2,compress='gzip');saveRDS(o13,reg('local_only/webapp/model_hospital.rds'),version=2,compress='gzip')
    mf<-c('model_30day.rds','model_hospital.rds');manifest21<-data.frame(file=mf,md5=unname(tools::md5sum(file.path(root,mf))),model_id=c(o7$model_id,o13$model_id),endpoint=c(o7$endpoint,o13$endpoint),source_model_file_md5=c(o7$source_md5,o13$source_md5),stringsAsFactors=FALSE)
    csv_out(manifest21,'local_only/webapp/model_manifest.csv');csv_out(manifest21,'to_share/03_model_manifest.csv')
    demo<-make_demo(o7);template<-demo[1,,drop=FALSE];template[1,o7$contract$raw_columns]<-NA_real_;template$age<-NA;template$sex_male<-NA;template$in_index_icu<-NA;template$in_index_hospital_at_T48<-NA
    ami_api$write_csv(demo,reg('local_only/webapp/synthetic_example.csv'));ami_api$write_csv(template,reg('local_only/webapp/input_template.csv'))
    dd<-o7$contract$dictionary
    dd$definition<-ifelse(dd$statistic=='abnormal_fraction','Fraction of valid observations outside the frozen abnormal bounds (0-1).',ifelse(dd$statistic=='slope_per_hour','Frozen upstream within-window time slope; use the Step4/6 value.',ifelse(dd$statistic=='sd_value','Within-window SD; blank when insufficient observations.',paste('Within-window',dd$statistic))))
    csv_out(dd,'local_only/webapp/feature_dictionary.csv');csv_out(rules,'local_only/webapp/component_units_and_bounds.csv')
    current_module<<-'inference preflight';logmsg('Step21-2预检：CSV二进制精度、旧版R兼容、分数同分回归、非恒定风险、性别编码、单例/批量、终点切换和旧结果清空。')
    tests<-preflight(o7,root);csv_out(tests,'to_share/02_preflight.csv')
    ep<-new.env(parent=baseenv());sys.source(file.path(root,'predictor.R'),envir=ep);api<-ep$ami_api
    orig7<-load_original(paths['engine7'],paths['predict7'],'predict_step7');orig13<-load_original(paths['engine13'],paths['predict13'],'predict_step13')
    current_module<<-'all-patient frozen roundtrip';audit<-list();input_audit<-list();privacy<-list();schemas<-list()
    for(source in c('MIMIC','eICU'))for(endpoint in c('mortality30','hospital')){
      bb<-if(source=='MIMIC')b else e;obj<-if(endpoint=='mortality30')m7 else m13;oo<-if(endpoint=='mortality30')o7 else o13;fun<-if(endpoint=='mortality30')orig7 else orig13
      x<-frame_from_bundle(bb,oo);keep<-rowSums(!is.na(bb$raw_numeric[oo$contract$raw_columns]))>0
      if(endpoint=='hospital')keep<-keep&x$in_index_hospital_at_T48%in%1
      if(!any(keep))stop('没有可核验的冻结输入。')
      xx<-x[keep,,drop=FALSE];oldp<-fun(bb$raw_numeric[keep,,drop=FALSE],bb$baseline[keep,,drop=FALSE],obj);z<-api$request(root,endpoint,xx,TRUE)
      err<-compare_prediction(matrix(z$risk_probability,ncol=1),matrix(oldp$probability[,'C'],ncol=1),'原分析/网页')
      clbad<-sum(z$frozen_cluster!=paste0('C',oldp$phenotype));sc<-api$scores(xx[oo$contract$raw_columns],oo)
      if(clbad||!isTRUE(all.equal(sc,oldp$scores,check.attributes=FALSE,tolerance=1e-10)))stop('冻结分型或评分不能复现。')
      # Every row is compared above; single/batch is additionally checked at deterministic positions.
      ii<-unique(as.integer(round(seq(1,nrow(xx),length.out=min(11,nrow(xx))))));serr<-0
      for(i in ii){one<-api$request(root,endpoint,xx[i,,drop=FALSE],TRUE);serr<-max(serr,abs(one$risk_probability-z$risk_probability[i]));if(one$frozen_cluster!=z$frozen_cluster[i])stop('单例/批量分型不一致。')}
      if(serr>1e-10)stop('单例/批量概率不一致。')
      f<-reg(paste0('local_only/validation_',source,'_',endpoint,'.csv'));api$write_csv(xx,f)
      parsed<-api$read_csv(f);ia<-api$csv_audit(xx,parsed);ia$source<-source;ia$endpoint<-oo$endpoint
      input_audit[[length(input_audit)+1L]]<-ia;csv_out(do.call(rbind,input_audit),'to_share/04_csv_input_roundtrip.csv')
      if(!all(ia$exact_match))stop('CSV读回的输入不完全一致；见04_csv_input_roundtrip.csv（仅字段级汇总）。')
      round<-api$request(root,endpoint,parsed,TRUE);csv_error<-compare_prediction(matrix(round$risk_probability,ncol=1),matrix(z$risk_probability,ncol=1),'CSV/离线')
      parsed_input<-api$validate_input(parsed,oo);csv_sc<-api$scores(parsed_input$raw,oo)
      csv_score_match<-identical(unname(as.matrix(csv_sc)),unname(as.matrix(sc)))
      csv_clbad<-sum(round$frozen_cluster!=z$frozen_cluster)
      if(!csv_score_match||csv_clbad)stop('CSV往返后评分或分型不一致。')
      if(!isTRUE(cfg$EXPORT_PRIVATE_CSV))unlink(f)
      saved_error<-NA_real_
      if(source=='eICU'){
        ss<-if(endpoint=='mortality30')saved12 else saved13;k<-as.character(ss$metadata$patient_key);target<-as.character(bb$metadata$patient_key)
        if(anyDuplicated(k)||anyNA(k)||!setequal(k,target)||!identical(ss$prediction_endpoint,oo$endpoint))stop('eICU保存概率的患者键/终点不符。')
        ind<-match(target,k)[keep];saved_error<-compare_prediction(matrix(z$risk_probability,ncol=1),matrix(ss$prediction[ind,'C'],ncol=1),'保存的eICU预测/网页')
      }
      audit[[length(audit)+1L]]<-data.frame(source=source,endpoint=oo$endpoint,input_n=nrow(x),verified_n=nrow(xx),blocked_by_input_contract=sum(!keep),
        original_probability_max_error=err,single_batch_max_error=serr,csv_max_error=csv_error,saved_external_max_error=saved_error,phenotype_mismatches=clbad,score_match=TRUE,
        csv_input_exact_match=all(ia$exact_match),csv_numeric_value_mismatches=sum(ia$value_mismatches[ia$numeric_field]),csv_missingness_mismatches=sum(ia$missingness_mismatches),
        csv_score_exact_match=csv_score_match,csv_phenotype_mismatches=csv_clbad,source_model_unchanged=TRUE)
      logmsg(paste0(source,' / ',endpoint,'：核验',nrow(xx),'行；原分析、CSV及网页入口一致。'))
    }
    csv_out(do.call(rbind,audit),'to_share/04_prediction_consistency.csv')
    for(oo in list(o7,o13)){
      ami_api$plain(oo);privacy[[length(privacy)+1L]]<-data.frame(model_id=oo$model_id,parameter_only=TRUE,contains_functions_or_environments=FALSE,contains_patient_rows_or_identifiers=FALSE,
        contains_empirical_score_reference=TRUE,automatically_public_released=FALSE)
      schemas[[length(schemas)+1L]]<-data.frame(model_id=oo$model_id,raw_statistic_columns=length(oo$contract$raw_columns),baseline_predictor_columns=3,eligibility_and_contract_columns=5,
        total_csv_columns=length(oo$contract$columns),retained_design_columns=length(oo$model$preprocessor$design_columns),nonzero_coefficients=sum(oo$model$coefficients[-1]!=0),frozen_clusters=oo$state$cluster$k)
    }
    csv_out(do.call(rbind,privacy),'to_share/05_export_privacy_audit.csv')
    csv_out(do.call(rbind,schemas),'to_share/05_input_schema_summary.csv')
    current_module<<-'Shiny server integration'
    if(isTRUE(cfg$RUN_SHINY_TESTS)){
      ensure_shiny();se<-new.env(parent=baseenv());sys.source(file.path(root,'tests_server.R'),envir=se);sh<-se$run_ami_shiny_tests(root);csv_out(sh,'to_share/06_Shiny_server_tests.csv')
      logmsg('Shiny核验通过：批量、单例、示例、终点切换、非法输入、缺失模型及旧结果清空。')
    }else{
      csv_out(data.frame(check='Shiny server integration',status='NOT_RUN'),'to_share/06_Shiny_server_tests.csv')
      issue('INCOMPLETE','Shiny','仅导出，未运行Shiny服务器测试；不能标记完成。')
    }
    if(any(unname(tools::md5sum(paths))!=unname(hashes)))stop('上游输入在本次运行中发生变化。')
    csv_out(data.frame(file=unname(paths),before=unname(hashes),after=unname(tools::md5sum(paths)),unchanged=TRUE),'to_share/02_upstream_unchanged.csv')
    current_module<<-'documentation and fixed outputs'
    description<-c('AMI-EDRS Step21 | Frozen research prototype',
      '本步不增加统计分析、不重新训练、不重复bootstrap。只核验冻结推理并生成替换用本地Shiny原型。',
      'Step21-2修复：整数以十进制、其他有限数值以C99十六进制浮点文本写入CSV；写出前及读回后均核对精度。冻结参数、评分同分规则和1e-10概率核验阈值保持不变。',
      'CSV中的0x...p...是保留原值的合法数值表示，由同一R预测接口自动解析；网页仍显示常规数值，不需要手工修改编码。',
      '输入CSV请用生成的ami_api$write_csv导出；不要使用普通write.csv或经电子表格四舍五入后另存，否则可能改变精确同分。既有低精度CSV必须从原始冻结RDS重新导出。',
      paste0('核心CSV共有',length(o7$contract$raw_columns),'个未缩放生理统计特征、3个基础预测变量和5个资格/版本字段。'),
      '使用由既有Step4/6工程生成的累计0-6h、0-24h、0-48h特征；禁止用三个代表值近似完整统计量。',
      'age=岁；sex_male支持0/1、F/M、Female/Male；空白/NA/Missing/Unknown为缺失；in_index_icu必须0/1。',
      'icu_admitted_AMI=1，alive_at_T48=1，landmark_hours=48。院内终点还要求in_index_hospital_at_T48=1。',
      '列名必须完整，可调整列顺序。真实缺失保留空白，按冻结填补规则处理；整行生理字段全空拒绝。单位/范围在feature_dictionary.csv，异常比例为0-1。',
      'CSV版本字段不能自动验证特征工程来源；使用者仍需保证时间窗和单位正确。本原型没有EHR自动接口。',
      '默认Step7模型C输出T48后30天全因死亡风险；切换至Step13模型C输出T48后院内死亡风险。二者不混用。',
      '两个模型均为冻结MIMIC原始模型。Step14的医院分组交叉拟合更新用于外部评价，不随意为新患者分配校准折，也不作为现成通用部署校准。',
      'eICU仅有院内结局；不能把它称为原30天模型绝对风险的外部验证。',
      '示例是独立人工构造的输入，不是抽取/扰动真实患者；示例、单例、批量和离线调用ami_api$request同一路径。',
      '新模型/评分/分型加载失败时停止输出风险；输入/终点改变及计算失败时清空旧结果。没有规则分型、手写概率或固定12%替代值。',
      'frozen_cluster来自全MIMIC冻结分型，不能与Step19发现/评价集的P1/P2标签互换。模型C不使用分型哑变量。',
      'local_only/webapp含必要系数、预处理参数和经验评分参考分布；虽然没有患者表或环境残留，这些仍为数据库衍生对象，保持本地，未自动上传到公共仓库或服务器。',
      '反馈包只含审计、代码、字段定义和人工模拟示例；没有真实患者逐行特征/概率，也没有模型RDS。',
      '如需在本地网页核对现有患者，可在运行前设置 options(ami_step21.config=list(EXPORT_PRIVATE_CSV=TRUE))。validation_*.csv仅保存在local_only，不进入反馈包或公网。',
      '网页启动：运行Step21/Launch_Step21_WebApp.R。需要shiny；本脚本默认在缺少时安装并运行服务器测试。',
      '本步生成替换用网页，未修改或下线旧的线上站点。旧线上结果不能据本步声称已修复；正式替换和Fig8截图在最终稿收尾时进行。',
      '验证覆盖全部合规冻结患者的评分/类别/概率、完整CSV往返及确定位置单例；Shiny测试使用人工模拟示例。没有进行真实临床工作流验证。',
      '下一项：统一全文、图表、TRIPOD+AI及45个审稿回复单元；不为获得显著结果再加分析。')
    txt_out(description,'to_share/00_READ_ME_FIRST.txt');txt_out(description,'local_only/webapp/README.txt')
    txt_out(c('# source this file to launch the local replacement prototype.',
      "if(!requireNamespace('shiny',quietly=TRUE))stop('Please install.packages(\"shiny\") first.')",
      paste0('shiny::runApp(',paste(deparse(root),collapse=''),' , host="127.0.0.1", launch.browser=TRUE)')),'Launch_Step21_WebApp.R')
    txt_out(c('# Offline entry point; set app_dir to the generated local_only/webapp directory.',
      'source(file.path(app_dir,"predictor.R"),encoding="UTF-8")',
      '# Export engineered_features directly from the original full-precision Step4/6 R object:',
      '# ami_api$write_csv(engineered_features,"your_engineered_features.csv")',
      '# Decimal integers and exact hexadecimal floats (0x...p...) are parsed by ami_api.',
      '# Do not round or resave the CSV in a spreadsheet application.',
      'x <- ami_api$read_csv("your_engineered_features.csv")',
      'pred <- ami_api$request(app_dir,"mortality30",x,confirmed=TRUE)',
      '# For hospital mortality use endpoint="hospital"; outcome is explicitly included in pred.',
      'ami_api$write_csv(pred,"frozen_predictions.csv")'),'to_share/07_offline_usage.R')
    for(nm in c('predictor.R','server.R','app.R','tests_server.R','input_template.csv','synthetic_example.csv','feature_dictionary.csv','component_units_and_bounds.csv')){
      dest<-reg(paste0('to_share/web_source/',nm));if(!file.copy(file.path(root,nm),dest,overwrite=TRUE))stop('不能导出网页代码：',nm)
    }
    csv_out(data.frame(target=c('Figure 8','Web prototype','Methods/code availability','Remaining analyses'),action=c('Replace screenshot after launching and reviewing the generated app','Replace original inference implementation with generated local prototype; online publication pending','Describe shared frozen functions, engineered CSV input, conditional endpoints and no fallback','No new statistical modeling in Step21'),stringsAsFactors=FALSE),'to_share/11_figure_table_replacement_map.csv')
    csv_out(data.frame(task=c('Statistical results through Step20','Local offline/web prediction consistency','Old online website replacement','Manuscript, figures and 45 reply units'),status=c('REVIEWED',if(isTRUE(cfg$RUN_SHINY_TESTS))'COMPLETED_FOR_REVIEW' else 'PENDING_SHINY_TESTS','NOT_PERFORMED','PENDING')),'to_share/12_revision_progress.csv')
    txt_out(c('https://shiny.posit.co/r/reference/shiny/latest/testserver.html','https://rstudio.github.io/shiny/reference/observeEvent.html','https://rstudio.github.io/shiny/reference/runApp.html',
      'https://stat.ethz.ch/R-manual/R-devel/library/utils/html/write.table.html','https://stat.ethz.ch/R-manual/R-devel/library/base/html/sprintf.html','https://stat.ethz.ch/R-manual/R-devel/library/base/html/double.html'),'to_share/10_software_sources.txt')
    files<-registered[startsWith(registered,'to_share/')&file.exists(file.path(out,registered))]
    if(any(grepl('[.]rds$',files))||any(grepl('validation_',files)))stop('反馈包不得包含患者表或冻结模型。')
    if(isTRUE(cfg$RUN_SHINY_TESTS))txt_out('Step21 automated consistency and Shiny server checks passed; local research review only.','local_only/webapp/VALIDATED_LOCAL_PROTOTYPE.txt')
    status<<-if(isTRUE(cfg$RUN_SHINY_TESTS))'COMPLETED_FOR_REVIEW' else 'EXPORTED_PENDING_SHINY_TESTS'
    invisible(TRUE)
  }

  preflight<-function(o,root){
    x<-make_demo(o);p<-ami_api$predict(x,o);checks<-list()
    test<-function(name,expr){if(!isTRUE(force(expr)))stop('预检失败：',name);checks[[length(checks)+1L]]<<-data.frame(check=name,status='PASS')}
    rejects<-function(expr)inherits(tryCatch({force(expr);NULL},error=function(e)e),'error')
    test('single and batch',max(abs(vapply(1:3,function(i)ami_api$predict(x[i,,drop=FALSE],o)$risk_probability,numeric(1))-p$risk_probability))<1e-12)
    z<-x[,rev(names(x)),drop=FALSE];test('permuted columns',isTRUE(all.equal(ami_api$predict(z,o),p,tolerance=1e-12)))
    z<-x;z$sex_male<-c('Female','M','Missing');test('sex aliases',isTRUE(all.equal(ami_api$predict(z,o),p,tolerance=1e-12)))
    z$sex_male[1]<-'unsupported';test('unsupported sex rejected',rejects(ami_api$predict(z,o)))
    z<-x;z$contract_id<-'wrong';test('wrong contract rejected',rejects(ami_api$predict(z,o)))
    z<-x;z[[o$contract$raw_columns[1]]]<-NULL;test('missing column rejected',rejects(ami_api$predict(z,o)))
    z<-x;z$death_30d<-0;test('extra outcome column rejected',rejects(ami_api$predict(z,o)))
    z<-x;names(z)[2]<-names(z)[1];test('duplicate column rejected',rejects(ami_api$predict(z,o)))
    z<-x;z[1,o$contract$raw_columns]<-NA_real_;test('blank physiology rejected',rejects(ami_api$predict(z,o)))
    z<-x;z[[o$contract$raw_columns[1]]][1]<-Inf;test('infinite numeric rejected',rejects(ami_api$predict(z,o)))
    z<-x;z[[o$contract$raw_columns[1]]]<-'not a number';test('malformed numeric rejected',rejects(ami_api$predict(z,o)))
    af<-grep('__abnormal_fraction$',names(x),value=TRUE)[1];z<-x;z[[af]][1]<-20;test('percent vs fraction rejected',rejects(ami_api$predict(z,o)))
    z<-x;z$landmark_hours<-24;test('wrong landmark rejected',rejects(ami_api$predict(z,o)))
    z<-x;z$alive_at_T48[1]<-0;test('prelandmark death rejected',rejects(ami_api$predict(z,o)))
    z<-x;z$icu_admitted_AMI[1]<-0;test('wrong population rejected',rejects(ami_api$predict(z,o)))
    test('empty input rejected',rejects(ami_api$predict(x[FALSE,,drop=FALSE],o)))
    f<-tempfile(fileext='.csv');on.exit(unlink(f),add=TRUE);ami_api$write_csv(x,f);parsed<-ami_api$read_csv(f)
    test('CSV input exact roundtrip',all(ami_api$csv_audit(x,parsed)$exact_match))
    test('CSV read roundtrip',max(abs(ami_api$predict(parsed,o)$risk_probability-p$risk_probability))<1e-10)
    # Deterministic precision stress, including adjacent doubles, missingness and signed zero.
    v<-c(120*.03,1/3,2/3,1/7,pi,sqrt(2),1+.Machine$double.eps,1-.Machine$double.eps,0,-0,NA_real_,
      .Machine$double.xmin,.Machine$double.xmax,5e-324,-5e-324,1e-300,2^53,-2^53)
    stress<-data.frame(id=seq_along(v),value=v);ami_api$write_csv(stress,f);parsed<-ami_api$read_csv(f)
    test('CSV fractions adjacent doubles extremes and signed zero',identical(as.numeric(parsed$value),v,num.eq=FALSE)&&all(ami_api$csv_audit(stress,parsed)$exact_match))
    test('CSV failing SBP SD value exact binary roundtrip',identical(as.numeric(parsed$value[1]),120*.03,num.eq=FALSE)&&grepl('^0x',parsed$value[1]))
    test('CSV subnormals use old R compatible exponent',identical(parsed$value[14:15],c('0x0.0000000000001p-1022','-0x0.0000000000001p-1022')))
    test('CSV integer metadata stays readable',identical(parsed$id,as.character(seq_along(v)))&&identical(parsed$value[17:18],c('9007199254740992','-9007199254740992')))
    bad<-stress;bad$value[1]<-Inf;test('CSV writer rejects infinite values',rejects(ami_api$write_csv(bad,f)))
    bad$value[1]<-NaN;test('CSV writer rejects NaN',rejects(ami_api$write_csv(bad,f)))
    bad<-parsed;bad$value[1]<-NA_character_;test('CSV audit detects changed missingness',sum(ami_api$csv_audit(stress,bad)$missingness_mismatches)==1L)
    test('CSV audit rejects changed row count',rejects(ami_api$csv_audit(stress,parsed[-1,,drop=FALSE])))
    # Independent toy model: repeated fractional references and a NONZERO score coefficient.
    # This never changes o or either exported frozen model.
    rc<-paste('probe',c('0_6h','0_24h','0_48h'),'abnormal_fraction',sep='__')
    sn<-paste('probe',c('0_6h','0_24h','0_48h'),'score',sep='__');target<-sn[3]
    rr<-list(values=c(0,1/3,2/3,1),count=c(10,40,40,10),cumulative=c(10,50,90,100),n=100,enabled=TRUE,direction='high')
    pp0<-list(features=target,median=setNames(50,target),indicators=character(),center=setNames(0,target),scale=setNames(1,target),design_columns=target)
    toy<-list(version='AMI_WEB21_v1',model_id='SYNTHETIC_FRACTION_TIES',endpoint='30_day_all_cause_death_after_T48',source_md5='synthetic',min_domain_fraction=1,
      state=list(reference=list(spec=data.frame(variable='probe',stat='abnormal_fraction',domain='probe',component_id='probe_fraction',direction='high'),reference=list(probe_fraction=rr)),
        cluster_preprocessor=pp0,cluster=list(k=2L,centers=matrix(c(30,70),2,1,dimnames=list(NULL,target))),dynamic_columns=rc,score_columns=sn),
      model=list(preprocessor=pp0,coefficients=setNames(c(-2,.02),c('(Intercept)',target))))
    tx<-data.frame(contract_id='SYNTHETIC',landmark_hours=rep(48,3),icu_admitted_AMI=1,alive_at_T48=1,in_index_hospital_at_T48=1,age=60,sex_male=0,in_index_icu=1)
    for(nm in rc)tx[[nm]]<-c(1/3,2/3,1/3)
    toy$contract<-list(id='SYNTHETIC',columns=names(tx),raw_columns=rc,dictionary=data.frame(field=rc,minimum=0,maximum=1))
    expected<-stats::plogis(-2+.02*c(30,70,30));tp<-ami_api$predict(tx,toy)
    test('fractional tie nonconstant independent probability oracle',max(abs(tp$risk_probability-expected))<1e-12&&identical(tp$frozen_cluster,c('C1','C2','C1')))
    utils::write.csv(tx,f,row.names=FALSE,na='');legacy<-ami_api$read_csv(f);legacy_p<-ami_api$predict(legacy,toy)
    test('regression reproduces ordinary CSV tie drift',any(!ami_api$csv_audit(tx,legacy)$exact_match)&&max(abs(legacy_p$risk_probability-expected))>1e-5)
    ami_api$write_csv(tx,f);fixed<-ami_api$read_csv(f);fp<-ami_api$predict(fixed,toy)
    test('fractional tie CSV fixed inputs scores clusters probabilities',all(ami_api$csv_audit(tx,fixed)$exact_match)&&identical(fp,tp)&&
      identical(ami_api$scores(ami_api$validate_input(fixed,toy)$raw,toy),ami_api$scores(tx[rc],toy)))
    # Force a simple nonconstant synthetic logistic oracle without changing any saved model.
    if('age'%in%o$model$preprocessor$features){
      toy<-o;toy$model$coefficients[]<-0;toy$model$coefficients['(Intercept)']<--1.2;toy$model$coefficients['age']<-.8
      v<-x$age;v[is.na(v)]<-o$model$preprocessor$median['age'];expected<-1/(1+exp(-(-1.2+.8*(v-o$model$preprocessor$center['age'])/o$model$preprocessor$scale['age'])))
      test('independent nonconstant logistic oracle',max(abs(ami_api$predict(x,toy)$risk_probability-expected))<1e-12)
    }
    # Independent percentile and linear-algebra oracles.
    r<-list(values=c(1,2),count=c(3,1),cumulative=c(3,4),n=4,enabled=TRUE,direction='high')
    test('mid-percentile ties',identical(ami_api$percentile(c(0,1,1.5,2,3,NA),r),c(0,37.5,75,87.5,100,NA_real_)))
    r$direction<-'low';test('low-direction applied once',identical(ami_api$percentile(c(0,1,2,3),r),c(100,62.5,12.5,0)))
    pp<-list(features=c('a','b'),median=c(a=2,b=5),indicators='a',center=c(a=2,b=4,a__NA=.25),scale=c(a=2,b=4,a__NA=.5),design_columns=c('a','b','a__NA'))
    test('frozen imputation and scaling oracle',max(abs(ami_api$prep(data.frame(a=c(NA,4),b=c(8,4)),pp)-matrix(c(0,1,1,0,1.5,-.5),2,3)))<1e-12)
    z<-o;z$model$coefficients[1]<-NA;test('invalid model rejected',rejects(ami_api$predict(x,z)))
    z<-o;z$leaked_environment<-new.env();test('environment leak rejected',rejects(ami_api$validate_model(z)))
    test('parameter-only object',isTRUE(ami_api$plain(o)))
    ctrl<-ami_api$controller(root);test('controller success',ctrl$run('mortality30',x,TRUE)&&!is.null(ctrl$result()))
    test('controller error clears result',!ctrl$run('unsupported',x,TRUE)&&is.null(ctrl$result())&&!is.null(ctrl$error()))
    temp<-tempfile();dir.create(temp);on.exit(unlink(temp,recursive=TRUE),add=TRUE);file.copy(file.path(root,'model_manifest.csv'),temp)
    test('missing model no fallback',rejects(ami_api$request(temp,'mortality30',x,TRUE)))
    file.copy(file.path(root,'model_30day.rds'),temp);cat('altered',file=file.path(temp,'model_30day.rds'),append=TRUE)
    test('changed model no fallback',rejects(ami_api$request(temp,'mortality30',x,TRUE)))
    hh<-ami_api$load_bundle(root,'hospital');z<-make_demo(hh);z$in_index_hospital_at_T48[1]<-0;z$in_index_icu[1]<-0
    test('hospital endpoint eligibility enforced',rejects(ami_api$predict(z,hh)))
    do.call(rbind,checks)
  }

  APP_CODE <- r"----(# AMI-EDRS frozen research prototype. Launch with shiny::runApp(this_directory).
if(!requireNamespace('shiny',quietly=TRUE))stop('Install shiny before launching this prototype.')
options(shiny.maxRequestSize=100*1024^2)
app_root<-normalizePath(getwd(),winslash='/',mustWork=TRUE)
if(!file.exists(file.path(app_root,'VALIDATED_LOCAL_PROTOTYPE.txt')))stop('Run Step21 successfully before launching this app.')
source(file.path(app_root,'predictor.R'),local=TRUE,encoding='UTF-8')
source(file.path(app_root,'server.R'),local=TRUE,encoding='UTF-8')
ui<-shiny::fluidPage(
  shiny::tags$head(shiny::tags$style(shiny::HTML('
    body {font-family:Arial,sans-serif;font-size:20px;color:#22313e;background:#f5f7fa;}
    .container-fluid {max-width:1280px;margin:28px auto;}
    h1 {font-size:34px;font-weight:700;} h2 {font-size:27px;} h3 {font-size:23px;}
    label,.form-control,.btn,.help-block,.shiny-text-output {font-size:19px;}
    .well,.result-card {background:white;border:1px solid #dce3ea;border-radius:10px;padding:24px;}
    .btn-primary {background:#176b85;border-color:#176b85;}
    .btn {white-space:normal;margin:5px 5px 7px 0;padding:9px 14px;}
    .table {font-size:20px;border-top:2px solid #354c60;border-bottom:2px solid #354c60;}
    .table th {border-bottom:1px solid #354c60!important;} .table td {border-top:0!important;}
    .scope {border-left:5px solid #176b85;padding:12px 20px;background:#eaf3f6;margin:18px 0 25px;}
    .small-note {font-size:18px;line-height:1.5;color:#485b69;} #status {font-size:20px;margin:12px 0;}
    #metadata {overflow-wrap:anywhere;font-size:18px;} .shiny-output-error {font-size:20px;}
  ')),shiny::tags$script(shiny::HTML("document.addEventListener('change',function(e){if(e.target&&e.target.id==='csv'&&window.Shiny){Shiny.setInputValue('invalidate_nonce',Date.now(),{priority:'event'});}});"))),
  shiny::tags$h1('AMI-EDRS | Frozen research prototype'),
  shiny::div(class='scope','For adults admitted to an ICU with AMI who survive to the 48-hour landmark. Research demonstration; clinical implementation and treatment effects have not been established.'),
  shiny::sidebarLayout(
    shiny::sidebarPanel(width=4,
      shiny::selectInput('endpoint','Prediction outcome',choices=c('30-day mortality after T48'='mortality30','Hospital mortality after T48'='hospital'),selected='mortality30'),
      shiny::textOutput('endpoint_label'),
      shiny::tags$hr(),shiny::fileInput('csv','Upload engineered features (.csv)',accept='.csv'),
      shiny::checkboxInput('confirm','I confirm the feature definitions, units, cumulative time windows, and population eligibility.',FALSE),
      shiny::numericInput('row_index','Row for a single prediction',value=1,min=1,step=1),
      shiny::actionButton('predict_single','Predict selected row',class='btn-primary'),
      shiny::actionButton('predict_batch','Predict all rows',class='btn-primary'),
      shiny::actionButton('clear','Clear results'),shiny::tags$hr(),
      shiny::actionButton('run_demo','Run synthetic example'),
      shiny::downloadButton('download_template','Feature template'),
      shiny::downloadButton('download_dictionary','Definitions and units'),
      shiny::downloadButton('download_demo','Synthetic example CSV')
    ),
    shiny::mainPanel(width=8,
      shiny::div(class='result-card',shiny::tags$h2(shiny::textOutput('result_title')),shiny::textOutput('status'),shiny::textOutput('metadata'),
        shiny::tableOutput('results'),shiny::uiOutput('download_controls'),shiny::tags$p(class='small-note','Up to 20 rows are displayed; the download contains all results. Missing cells use the frozen imputation rules and are counted above.')),
      shiny::tags$h3('Input requirements'),
      shiny::tags$p('Use the unscaled statistical features produced by the study feature-engineering workflow. The windows are cumulative: 0-6, 0-24 and 0-48 hours after ICU admission.'),
      shiny::tags$p('Three representative measurements cannot reproduce means, variability, slopes or abnormal fractions. This prototype has no automatic EHR connection.'),
      shiny::tags$p(class='small-note','Model C uses continuous dynamics. Both endpoints use frozen MIMIC models without eICU cross-fitted calibration updates. The 30-day model has no external validation of 30-day absolute risk in eICU, where only hospital outcomes are available.'),
      shiny::tags$p(class='small-note','The cluster label in the downloaded CSV comes from the frozen full-MIMIC model. It is not the discovery/evaluation P1/P2 label used in Step19. No treatment, monitoring-intensity or intervention-benefit recommendation is generated.')
    )
  )
)
shiny::shinyApp(ui=ui,server=make_ami_server(app_root,ami_api))
)----"

  SERVER_CODE <- r"----(make_ami_server <- function(root, api) {
  force(root);force(api)
  function(input, output, session) {
    result <- shiny::reactiveVal(NULL)
    message <- shiny::reactiveVal('Upload a feature CSV, or run the synthetic example.')
    origin <- shiny::reactiveVal('')
    clear <- function(text='Inputs changed. Run prediction again.') {result(NULL);origin('');message(text)}
    shiny::observeEvent(list(input$endpoint,input$csv,input$row_index,input$confirm,input$invalidate_nonce),{
      clear()
    },ignoreInit=FALSE,priority=100)
    execute <- function(kind) {
      clear('Checking the frozen model and feature contract...')
      tryCatch({
        endpoint<-input$endpoint
        if(is.null(endpoint)||!endpoint%in%c('mortality30','hospital'))stop('Select an endpoint.',call.=FALSE)
        if(kind=='demo'){
          x<-api$read_csv(file.path(root,'synthetic_example.csv'));confirmed<-TRUE;label<-'SYNTHETIC EXAMPLE - not a real patient'
        }else{
          if(is.null(input$csv)||!nrow(input$csv)||!file.exists(input$csv$datapath))stop('Upload a feature CSV first.',call.=FALSE)
          if(!isTRUE(input$confirm))stop('Confirm feature engineering, units, and eligibility.',call.=FALSE)
          x<-api$read_csv(input$csv$datapath);confirmed<-TRUE;label<-'Uploaded feature CSV'
          if(kind=='single'){
            i<-as.numeric(input$row_index)
            if(length(i)!=1L||!is.finite(i)||i!=floor(i)||i<1||i>nrow(x))stop('Selected row is outside the uploaded CSV.',call.=FALSE)
            x<-x[i,,drop=FALSE]
          }
        }
        # The exact same entry point is used by offline, batch, single and demo paths.
        z<-api$request(root,endpoint,x,confirmed)
        if(kind=='single')z$row_number<-as.integer(input$row_index)
        result(z);origin(label);message(paste(nrow(z),'row(s) predicted with the frozen model.'))
      },error=function(e){result(NULL);origin('');message(paste('No risk output:',conditionMessage(e)))})
    }
    shiny::observeEvent(input$predict_single,execute('single'),ignoreInit=TRUE)
    shiny::observeEvent(input$predict_batch,execute('batch'),ignoreInit=TRUE)
    shiny::observeEvent(input$run_demo,execute('demo'),ignoreInit=TRUE)
    shiny::observeEvent(input$clear,clear('Results cleared.'),ignoreInit=TRUE)
    # A disappearing/replaced model must also clear already displayed risks.
    model_check <- shiny::reactive({
      shiny::invalidateLater(2000,session)
      if(is.null(input$endpoint))return(NULL)
      tryCatch({o<-api$load_bundle(root,input$endpoint);list(ok=TRUE,id=o$model_id)},error=function(e)list(ok=FALSE,error=conditionMessage(e)))
    })
    shiny::observe({m<-model_check();z<-shiny::isolate(result());if(!is.null(m)&&!m$ok)clear(paste('No risk output:',m$error))else if(!is.null(m)&&!is.null(z)&&!identical(z$model_id[1],m$id))clear('Frozen model changed. Run prediction again.')})
    output$status<-shiny::renderText(message())
    output$result_title<-shiny::renderText({if(is.null(result()))return('No prediction');origin()})
    output$metadata<-shiny::renderText({z<-result();if(is.null(z))return('');paste('Model:',z$model_id[1],'|',z$calibration[1])})
    output$endpoint_label<-shiny::renderText({if(identical(input$endpoint,'hospital'))'Outcome: in-hospital death after T48; requires presence in the index hospital at T48.' else 'Outcome: all-cause death within 30 days after T48.'})
    output$results<-shiny::renderTable({
      z<-result();if(is.null(z))return(NULL);z<-utils::head(z,20)
      data.frame(Row=z$row_number,`Risk (%)`=sprintf('%.2f',100*z$risk_probability),`Imputed inputs`=z$imputed_input_count,
        `Observed domain scores`=paste0(z$score_domains_observed,'/',z$score_domains_total),check.names=FALSE)
    },striped=FALSE,bordered=FALSE,spacing='m',rownames=FALSE)
    output$download_controls<-shiny::renderUI({if(is.null(result()))return(NULL);shiny::downloadButton('download_results','Download all results (CSV)')})
    output$download_results<-shiny::downloadHandler(filename=function()'AMI_EDRS_frozen_predictions.csv',content=function(file){
      z<-shiny::isolate(result());if(is.null(z))stop('No valid prediction to download.')
      # Validate the selected model again before exporting any existing results.
      tryCatch(api$load_bundle(root,shiny::isolate(input$endpoint)),error=function(e){clear(paste('No risk output:',conditionMessage(e)));stop(e)})
      api$write_csv(z,file)
    })
    output$download_template<-shiny::downloadHandler(filename=function()'AMI_EDRS_feature_template.csv',content=function(file)file.copy(file.path(root,'input_template.csv'),file,overwrite=TRUE))
    output$download_dictionary<-shiny::downloadHandler(filename=function()'AMI_EDRS_feature_dictionary.csv',content=function(file)file.copy(file.path(root,'feature_dictionary.csv'),file,overwrite=TRUE))
    output$download_demo<-shiny::downloadHandler(filename=function()'AMI_EDRS_synthetic_example.csv',content=function(file)file.copy(file.path(root,'synthetic_example.csv'),file,overwrite=TRUE))
  }
}
)----"

  SHINY_TEST_CODE <- r"----(run_ami_shiny_tests <- function(root) {
  e<-new.env(parent=baseenv());sys.source(file.path(root,'predictor.R'),e);sys.source(file.path(root,'server.R'),e)
  api<-e$ami_api;server<-e$make_ami_server(root,api)
  good<-file.path(root,'synthetic_example.csv');x<-api$read_csv(good)
  f<-tempfile(fileext='.csv');bad<-x;bad$sex_male[1]<-'unmapped';utils::write.csv(bad,f,row.names=FALSE,na='')
  on.exit(unlink(f),add=TRUE)
  good_upload<-data.frame(name='synthetic_example.csv',size=file.info(good)$size,type='text/csv',datapath=good,stringsAsFactors=FALSE)
  bad_upload<-data.frame(name='bad.csv',size=file.info(f)$size,type='text/csv',datapath=f,stringsAsFactors=FALSE)
  checks<-character()
  shiny::testServer(server,{
    session$setInputs(endpoint='mortality30',confirm=FALSE,row_index=1,csv=good_upload,predict_single=0,predict_batch=0,run_demo=0,clear=0)
    session$flushReact();stopifnot(is.null(result()))
    session$setInputs(predict_batch=1);stopifnot(is.null(result()));checks<<-c(checks,'confirmation_required')
    session$setInputs(confirm=TRUE);session$setInputs(predict_batch=2)
    expected<-api$request(root,'mortality30',x,TRUE);stopifnot(!is.null(result()),isTRUE(all.equal(result(),expected,tolerance=1e-12)));checks<<-c(checks,'batch_offline_equality')
    stopifnot(grepl('Risk',output$results,fixed=TRUE),grepl(expected$model_id[1],output$metadata,fixed=TRUE));checks<<-c(checks,'result_rendering')
    session$setInputs(row_index=2);stopifnot(is.null(result()));session$setInputs(predict_single=1)
    stopifnot(nrow(result())==1L,abs(result()$risk_probability-expected$risk_probability[2])<1e-12,result()$row_number==2L);checks<<-c(checks,'single_batch_equality')
    session$setInputs(endpoint='hospital');stopifnot(is.null(result()));session$setInputs(predict_batch=3)
    h<-api$request(root,'hospital',x,TRUE);stopifnot(isTRUE(all.equal(result(),h,tolerance=1e-12)),all(result()$endpoint=='in_hospital_death_after_T48'));checks<<-c(checks,'endpoint_change_clears_and_relabels')
    session$setInputs(csv=bad_upload);stopifnot(is.null(result()));session$setInputs(predict_batch=4);stopifnot(is.null(result()),grepl('No risk output',message(),fixed=TRUE));checks<<-c(checks,'invalid_input_no_old_risk')
    session$setInputs(run_demo=1);stopifnot(isTRUE(all.equal(result(),h,tolerance=1e-12)),grepl('SYNTHETIC',origin(),fixed=TRUE));checks<<-c(checks,'demo_formal_path')
    session$setInputs(confirm=FALSE);stopifnot(is.null(result()));checks<<-c(checks,'withdraw_confirmation_clears')
    session$setInputs(endpoint='invalid',predict_batch=5);stopifnot(is.null(result()));checks<<-c(checks,'unsupported_endpoint_no_risk')
    session$setInputs(endpoint='mortality30',csv=good_upload,confirm=TRUE,row_index=999);session$setInputs(predict_single=2);stopifnot(is.null(result()));checks<<-c(checks,'invalid_row_no_risk')
    session$setInputs(row_index=1);session$setInputs(run_demo=2);stopifnot(!is.null(result()))
    session$setInputs(clear=1);stopifnot(is.null(result()),identical(output$metadata,''));checks<<-c(checks,'clear_no_old_risk')
    # Corrupt a private test copy, never the production model directory.
  })
  broken<-tempfile();dir.create(broken);on.exit(unlink(broken,recursive=TRUE),add=TRUE)
  file.copy(file.path(root,c('synthetic_example.csv','model_manifest.csv')),broken)
  broken_server<-e$make_ami_server(broken,api)
  shiny::testServer(broken_server,{
    session$setInputs(endpoint='mortality30',run_demo=0);session$flushReact();session$setInputs(run_demo=1)
    stopifnot(is.null(result()),grepl('No risk output',message(),fixed=TRUE));checks<<-c(checks,'missing_model_no_risk')
  })
  file.copy(file.path(root,c('model_30day.rds','model_hospital.rds')),broken)
  shiny::testServer(broken_server,{
    session$setInputs(endpoint='mortality30',run_demo=0);session$flushReact();session$setInputs(run_demo=1)
    stopifnot(!is.null(result()))
    unlink(file.path(broken,'model_30day.rds'))
    session$setInputs(run_demo=2);stopifnot(is.null(result()),grepl('No risk output',message(),fixed=TRUE))
    checks<<-c(checks,'model_removed_after_success_clears_old_risk')
  })
  stopifnot(length(checks)==13L)
  data.frame(check=checks,status='PASS',stringsAsFactors=FALSE)
}
)----"

  if(isTRUE(getOption("ami_step21.functions_only",FALSE)))return(list(main=main,preflight=preflight,minimize_model=minimize_model,contract_for=contract_for,frame_from_bundle=frame_from_bundle,make_demo=make_demo,api=ami_api,export_predictor=export_predictor))

  status<-"FAILED";current_module<-"input verification";failure_message<-NULL
  tryCatch(withCallingHandlers(main(),warning=function(w){add_warning(conditionMessage(w),current_module);invokeRestart("muffleWarning")}),error=function(err){
    status<<-"FAILED";failure_message<<-conditionMessage(err);issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step21_error_detail.txt")})
  flush_warnings()
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step21-2",
    "COMPLETED_FOR_REVIEW表示本地网页推理、输入校验和Shiny服务器测试通过；线上旧站尚未替换，也不表示临床部署已验证。",
    "FAILED表示本步未完整完成；检查日志。上游队列和预测保持原样。"),"to_share/Step21_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step21_sessionInfo.txt");logmsg(paste0("Step21状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step21_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(substring(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))],nchar("to_share/")+1L))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share,mode="mirror")else{
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step21_feedback.zip"),"\n",sep="")else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
