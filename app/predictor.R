  ami_api <- local({
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

