# AMI-EDRS revision | Step19-1: held-out categorical phenotype associations
# source(file.choose(), encoding="UTF-8")
# Step19-1: strictly parse numeric day strings; preserve frozen inputs and discovery cache.
# Fixed filenames; replaces owned results. Patient-level data and frozen references stay local_only.
local({
  cfg<-list(OUTPUT_PARENT=Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")),
    MAKE_PNG=TRUE,MAKE_ZIP=TRUE,RESUME=TRUE,
    LEGACY_SPLIT_FILE=file.path(Sys.getenv("AMI_MIMIC_PROJECT", unset=file.path(getwd(), "work/MIMIC")), "09_step05_T48_dynamic_prediction_model_with_C1C5/analysis_datasets/Step05_train_test_split.csv"),
    LEGACY_SPLIT_MD5="9a412679bac0f7672b7b4b5a98822243",PDF_FAMILY="Arial")
  cfg<-utils::modifyList(cfg,getOption("ami_step19.config",list()))
  norm <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  for (k in "OUTPUT_PARENT") cfg[[k]] <- norm(cfg[[k]])
  out <- file.path(cfg$OUTPUT_PARENT, "Step19")
  is_link <- function(p) { z <- Sys.readlink(p); any(!is.na(z) & nzchar(z)) }
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out) || file.access(out, 2) != 0L) stop("无法写入Step19输出目录。")
  if (is_link(out)) stop("Step19输出目录不能为符号链接。")
  owner <- file.path(out, ".AMI_EDRS_STEP19")
  manifest <- file.path(out, ".generated_files.txt")
  if (!file.exists(owner)) {
    existing <- list.files(out, all.files = TRUE, no.. = TRUE)
    if (length(existing)) stop("Step19目录已有非本脚本创建的文件；请先移走这些文件或更正OUTPUT_PARENT。")
    writeLines("AMI_EDRS_STEP19_MANAGED_OUTPUT", owner)
  } else if (!identical(readLines(owner, warn = FALSE), "AMI_EDRS_STEP19_MANAGED_OUTPUT")) {
    stop("Step19输出目录的归属标记不正确。")
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
  previous_run <- NULL
  if (file.exists(manifest)) {
    old <- unique(readLines(manifest, warn = FALSE))
    # Preserve prior provenance in memory before replacing generated reports.
    prior_files <- c("to_share/01_inputs.csv","local_only/Step19_frozen_phenotyping.rds")
    if(all(prior_files %in% old))previous_run <- tryCatch(list(
      inputs=utils::read.csv(safe_path(prior_files[1]),stringsAsFactors=FALSE,colClasses="character"),
      frozen=readRDS(safe_path(prior_files[2]))),error=function(e)NULL)
    for (rel in old[nzchar(old)]) {
      p <- safe_path(rel)
      if (file.exists(p) && !startsWith(rel,"local_only/cache/")) {
        if (dir.exists(p)) stop("登记的输出应为文件，不能为目录。")
        if (unlink(p) != 0L) stop("不能覆盖旧Step19输出；请关闭正在使用它的程序。")
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
  logp <- reg("to_share/Step19_run_log.txt")
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
    txt_out(if(nrow(z))unlist(Map(function(a,b)c(a,b,""),z$scope,z$message))else"No warnings in Step19.","local_only/Step19_warning_detail.txt")
    a<-if(nrow(z))aggregate(list(n=rep(1L,nrow(z))),list(scope=z$scope),sum)else data.frame(scope=character(),n=integer())
    csv_out(a,"to_share/09_warning_summary.csv")
  }

  engine<-NULL
  main_domains<-c("hemodynamic","respiratory_neurologic","renal_metabolic","inflammatory_coagulopathic")
  windows<-c("0-6h","0-24h","0-48h")
  plan<-list(version="Step19_v1",seed=2026L,split="Frozen original Train/Test; no new random split",
    k=2:6,min_cluster_fraction=.05,min_cluster_n=25L,min_silhouette=.05,min_stability_ARI=.6,
    stability_repeats=10L,stability_fraction=.8,silhouette_max_n=1000L,nstart=20L,iter_max=100L,
    min_reference_n=20L,min_domain_fraction=.5,preprocess_min_n=20L,max_missing=.95,
    primary="Evaluation-set categorical logistic OR: age, sex, ICU state, eight documented histories",
    secondary="Cox HR with approximate midpoint times; early 0-6h severity complete-case sensitivity",
    reference="P1 by discovery centroid lexicographic order; no outcome ordering",
    uncertainty="95% model-based Wald intervals conditional on the fixed discovery phenotyping",
    no_ordinal_trend=TRUE,no_outcome_based_k_selection=TRUE)
  history_vars<-c("prior_cad","heart_failure","diabetes","hypertension","chronic_kidney_disease","copd","stroke","atrial_fibrillation")
  model_labels<-c(M1="Unadjusted",M2="Age, sex, ICU state",M3="M2 + documented history",M3_CC="M3 on early-score complete cases",M4="M3 + early organ scores")
  domain_labels<-c("Hemodynamic","Respiratory / neurologic","Renal / metabolic","Inflammatory / coagulation")
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

  hash_object<-function(x){p<-tempfile(fileext=".rds");on.exit(unlink(p));saveRDS(x,p,version=2);unname(tools::md5sum(p))}
  fit_discovery<-function(raw){
    if(!nrow(raw))stop("发现集为空。")
    ref<-engine$fit_reference(raw,seq_len(nrow(raw)),min_observed=plan$min_reference_n)
    sc<-engine$apply_reference(raw,ref,windows,min_fraction=plan$min_domain_fraction)$scores[score_columns()]
    pp<-engine$fit_preprocessor(sc,seq_len(nrow(sc)),max_missing=plan$max_missing,min_observed=plan$preprocess_min_n,add_missing=FALSE)
    cl<-fit_clusters(engine$apply_preprocessor(sc,pp),plan$seed+19000L)
    rownames(cl$centers)<-paste0("P",seq_len(cl$k))
    list(version="Step19_v1",reference=ref,preprocessor=pp,cluster=cl,columns=score_columns())
  }
  assign_frozen<-function(raw,state){
    if(!nrow(raw))return(list(phenotype=character(),scores=as.data.frame(matrix(numeric(),0,length(state$columns),dimnames=list(NULL,state$columns)))))
    sc<-engine$apply_reference(raw,state$reference,windows,min_fraction=plan$min_domain_fraction)$scores[state$columns]
    x<-engine$apply_preprocessor(sc,state$preprocessor);a<-nearest(x,state$cluster$centers)
    list(phenotype=paste0("P",a),scores=sc)
  }
  profile_summary<-function(a,scope,levels){
    do.call(rbind,lapply(levels,function(g)do.call(rbind,lapply(main_domains,function(d)do.call(rbind,lapply(windows,function(w){
      x<-a$scores[[paste(d,gsub("-","_",w,fixed=TRUE),"score",sep="__")]][a$phenotype==g];z<-x[is.finite(x)]
      q<-if(length(z))stats::quantile(z,c(.25,.5,.75),names=FALSE)else rep(NA_real_,3)
      data.frame(scope=scope,phenotype=g,domain=d,window=w,n=length(x),observed_n=length(z),q25=q[1],median=q[2],q75=q[3])
    }))))))
  }

  required<-function(x,nms,label){if(!all(nms%in%names(x)))stop(label,"缺少字段：",paste(setdiff(nms,names(x)),collapse=", "))}
  checked_key<-function(x,label){x<-as.character(x);if(anyNA(x)||any(!nzchar(x))||anyDuplicated(x))stop(label,"缺失或重复。");x}
  bit<-function(x,label,allow_na=FALSE){if(is.factor(x))x<-as.character(x);z<-suppressWarnings(as.numeric(x));if(any(!is.na(x)&is.na(z))||any(!is.na(z)&!z%in%0:1)||(!allow_na&&anyNA(z)))stop(label,"不是合法0/1。");z}
  align_split<-function(b,old){
    required(old,c("stay_id","split"),"历史70/30分组");key<-checked_key(old$stay_id,"历史stay_id")
    lab<-tolower(trimws(as.character(old$split)));if(anyNA(lab)||any(!lab%in%c("train","test")))stop("历史split必须为Train/Test；不会重新随机分组。")
    m<-match(as.character(b$metadata$stay_id),key)
    if(anyNA(m))stop("冻结队列有",sum(is.na(m)),"人不在原70/30划分中；停止，需核对患者名单，不能默默排除或重分组。")
    role<-ifelse(lab[m]=="train","discovery","evaluation")
    if(!setequal(role,c("discovery","evaluation")))stop("发现集或评价集为空。")
    if(anyDuplicated(b$metadata$patient_key))stop("同一患者可能跨组。")
    list(role=role,excluded_legacy_n=sum(!key%in%as.character(b$metadata$stay_id)),matched_n=length(m))
  }
  numeric_days<-function(x,label){
    # Step4 reads roster CSV fields as character; Step6 retains secondary outcome types.
    # Convert decimal/scientific day values, never factor codes, dates, or strings containing units.
    if(inherits(x,c("Date","POSIXt","difftime","integer64"))||is.list(x)||!is.null(dim(x)))stop("随访天数字段类型不支持：",label)
    if(is.factor(x))x<-as.character(x)
    if(is.logical(x)){
      if(any(!is.na(x)))stop("随访天数不能是TRUE/FALSE：",label)
      return(rep(NA_real_,length(x)))
    }
    if(is.numeric(x)&&!is.complex(x)){
      if(any(is.infinite(x)|is.nan(x)))stop("随访天数含非有限数值：",label)
      return(as.numeric(x))
    }
    if(!is.character(x))stop("随访天数字段类型不支持：",label)
    t<-trimws(x);missing<-is.na(t)|t%in%c("","NA")
    pattern<-"^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$"
    ok<-missing|grepl(pattern,t,perl=TRUE)
    if(any(!ok))stop("随访天数字段含",sum(!ok),"个非数值内容：",label,"；不会转换成NA或删除患者。")
    z<-rep(NA_real_,length(t));z[!missing]<-suppressWarnings(as.numeric(t[!missing]))
    if(any(!missing&!is.finite(z)))stop("随访天数转换溢出或失败：",label)
    z
  }
  normalize_followup<-function(outcomes){
    fields<-c("followup_time_days_midpoint_if_date_only","event_time_lower_days","event_time_upper_days")
    required(outcomes,fields,"随访时间")
    values<-lapply(fields,function(v)numeric_days(outcomes[[v]],v));names(values)<-fields
    audit<-do.call(rbind,lapply(fields,function(v){
      original<-outcomes[[v]];converted<-values[[v]]
      data.frame(field=v,input_class=paste(class(original),collapse="/"),output_class="numeric_days",n=length(original),input_na_n=sum(is.na(original)),
        declared_blank_or_NA_n=if(is.character(original)||is.factor(original))sum(!is.na(original)&trimws(as.character(original))%in%c("","NA"))else 0L,
        output_missing_n=sum(is.na(converted)),finite_value_n=sum(is.finite(converted)),invalid_values_replaced_n=0L,
        conversion=if(is.numeric(original))"validated numeric"else"strict numeric text parsing",stringsAsFactors=FALSE)
    }))
    list(values=values,audit=audit)
  }
  prepare_association<-function(b,a){
    bs<-b$baseline;required(bs,c("age","sex_male","in_index_icu"),"基线数据")
    for(nm in names(bs)){if(!is.numeric(bs[[nm]]))stop("基线字段必须是数值：",nm)}
    y<-bit(b$outcomes$event_primary,"死亡结局");k<-sort(unique(a$phenotype))
    d<-data.frame(event=y,phenotype=factor(a$phenotype,levels=k),age10=(bs$age-60)/10,
      sex_male=bit(bs$sex_male,"sex_male",TRUE),in_index_icu=bit(bs$in_index_icu,"in_index_icu",TRUE))
    for(v in history_vars){pn<-paste0(v,"__documented_positive");cn<-paste0(v,"__patient_source_covered");required(b$clinical_documentation,c(pn,cn),"既往病史记录")
      p<-bit(b$clinical_documentation[[pn]],pn,TRUE);c<-bit(b$clinical_documentation[[cn]],cn)
      if(any(c==1&is.na(p))||any(c==0&!is.na(p)))stop("病史阳性/来源覆盖字段矛盾：",v)
      # Two design indicators preserve all three documentation states; unknown is never interpreted as absence.
      d[[paste0("history_",v,"_documented")]]<-as.numeric(c==1 & !is.na(p) & p==1)
      d[[paste0("history_",v,"_unknown")]]<-as.numeric(c==0)
    }
    for(v in main_domains)d[[paste0("early_",v)]]<-a$scores[[paste0(v,"__0_6h__score")]]/10
    required(b$outcomes,c("followup_time_days_midpoint_if_date_only","event_time_lower_days","event_time_upper_days","event_time_is_approximate"),"随访时间")
    time_input<-normalize_followup(b$outcomes)
    d$time<-time_input$values$followup_time_days_midpoint_if_date_only;d$approximate<-bit(b$outcomes$event_time_is_approximate,"event_time_is_approximate")
    d$time_lower<-d$time;d$time_upper<-d$time;ev<-d$event==1
    d$time_lower[ev]<-time_input$values$event_time_lower_days[ev];d$time_upper[ev]<-time_input$values$event_time_upper_days[ev]
    if(any(!is.finite(d$time)|d$time<=0|d$time>30)||any(!ev&abs(d$time-30)>1e-8))stop("T48后30天随访时间不符合冻结定义。")
    if(any(!is.finite(d$time_lower[ev]))||any(!is.finite(d$time_upper[ev]))||any(d$time_lower[ev]>d$time[ev]+1e-8)||any(d$time_upper[ev]<d$time[ev]-1e-8))stop("死亡时间区间与中点不一致。")
    if(any(!ev&d$approximate==1))stop("未死亡者不能标为近似死亡时间。")
    attr(d,"followup_type_audit")<-time_input$audit
    d
  }
  fit_association<-function(d,adjust,kind,model_id,cohort_name="evaluation",time_col="time",expected_levels=levels(d$phenotype)){
    req<-c("event","phenotype",adjust,if(kind=="HR")time_col)
    eligible<-stats::complete.cases(d[req])
    if(kind=="HR")eligible<-eligible&is.finite(d[[time_col]])&d[[time_col]]>0&d[[time_col]]<=30
    z<-d[eligible,,drop=FALSE];z$time_used<-z[[time_col]]
    n<-nrow(z);ev<-sum(z$event);ref<-expected_levels[1]
    contrast_template<-data.frame(cohort=cohort_name,model=model_id,measure=kind,time_definition=time_col,
      phenotype=if(length(expected_levels)>1)expected_levels[-1]else ref,reference=ref,n=n,events=ev,
      estimate=NA_real_,lower=NA_real_,upper=NA_real_,p_value=NA_real_,status="NOT_ESTIMABLE",stringsAsFactors=FALSE)
    overall<-data.frame(cohort=cohort_name,model=model_id,measure=kind,time_definition=time_col,n_input=nrow(d),n=n,events=ev,
      excluded_n=sum(!eligible),phenotype_df=NA_integer_,lrt_statistic=NA_real_,p_value=NA_real_,status="NOT_ESTIMABLE",reason="",covariates="",omitted_constant_or_duplicate="",events_per_parameter=NA_real_,stringsAsFactors=FALSE)
    fail<-function(reason){overall$reason<-reason;contrast_template$status<-reason;list(contrasts=contrast_template,overall=overall,coefficients=NULL,ph=NULL,fit=NULL,rows=which(eligible))}
    if(!n)return(fail("EMPTY_ANALYSIS_SET"))
    if(length(expected_levels)<2)return(fail("NO_SUPPORTED_PARTITION"))
    if(length(unique(z$event))<2)return(fail("SINGLE_OUTCOME_CLASS"))
    counts<-table(factor(z$phenotype,levels=expected_levels),factor(z$event,levels=0:1))
    if(any(rowSums(counts)==0))return(fail("ABSENT_PHENOTYPE_IN_ANALYSIS_SET"))
    if(any(counts[,2]==0)|| (kind=="OR"&&any(counts[,1]==0)))return(fail("ZERO_EVENT_OR_NONEVENT_CELL"))
    # All four prespecified models treat phenotype as a nominal factor.
    z$phenotype<-factor(z$phenotype,levels=expected_levels);contrasts(z$phenotype)<-stats::contr.treatment(length(expected_levels))
    keep<-adjust[vapply(z[adjust],function(v)length(unique(v[!is.na(v)]))>1,logical(1))]
    omitted<-setdiff(adjust,keep)
    # Prior-record coverage is often identical across history variables. Keep one copy of each identical
    # nuisance indicator, without looking at outcomes; otherwise eight unknown flags create exact aliasing.
    unique_keep<-character()
    for(v in keep){dup<-length(unique_keep)&&any(vapply(unique_keep,function(u)identical(z[[u]],z[[v]]),logical(1)))
      if(dup)omitted<-c(omitted,v)else unique_keep<-c(unique_keep,v)}
    keep<-unique_keep
    for(v in keep)if(is.factor(z[[v]])){z[[v]]<-droplevels(z[[v]]);contrasts(z[[v]])<-stats::contr.treatment(nlevels(z[[v]]))}
    overall$covariates<-paste(keep,collapse=" + ");overall$omitted_constant_or_duplicate<-paste(omitted,collapse="; ")
    rhs<-paste(c("phenotype",keep),collapse=" + ");reduced<-if(length(keep))paste(keep,collapse=" + ")else"1"
    response<-if(kind=="OR")"event"else"survival::Surv(time_used,event)"
    f1<-stats::as.formula(paste(response,"~",rhs));f0<-stats::as.formula(paste(response,"~",reduced))
    mm<-stats::model.matrix(stats::as.formula(paste("~",rhs)),data=z)
    if(qr(mm)$rank<ncol(mm))return(fail("RANK_DEFICIENT_DESIGN"))
    overall$events_per_parameter<-ev/(ncol(mm)-1L)
    warn<-character();err<-NULL
    fits<-tryCatch(withCallingHandlers({
      if(kind=="OR")list(full=stats::glm(f1,data=z,family=stats::binomial(),na.action=stats::na.fail,control=stats::glm.control(maxit=100)),
        reduced=stats::glm(f0,data=z,family=stats::binomial(),na.action=stats::na.fail,control=stats::glm.control(maxit=100)))else
        list(full=survival::coxph(f1,data=z,ties="efron",x=TRUE,y=TRUE,model=TRUE,singular.ok=FALSE,na.action=stats::na.fail,control=survival::coxph.control(iter.max=100)),
          reduced=survival::coxph(f0,data=z,ties="efron",x=TRUE,y=TRUE,model=TRUE,singular.ok=FALSE,na.action=stats::na.fail,control=survival::coxph.control(iter.max=100)))
    },warning=function(w){warn<<-c(warn,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e){err<<-conditionMessage(e);NULL})
    if(length(warn))for(w in warn)add_warning(w,paste(model_id,kind,time_col))
    if(is.null(fits)){add_warning(err,paste(model_id,kind,time_col));return(fail("FIT_ERROR"))}
    if(length(warn)&&any(grepl("converg|infinite|0 or 1|singular|iterations",warn,ignore.case=TRUE)))return(fail("UNSTABLE_FIT_WARNING"))
    if(kind=="OR"&&(!fits$full$converged||!fits$reduced$converged||isTRUE(fits$full$boundary)))return(fail("NONCONVERGENCE_OR_BOUNDARY"))
    cf<-stats::coef(fits$full);vc<-stats::vcov(fits$full);se<-sqrt(diag(vc))
    if(any(!is.finite(cf))||any(!is.finite(se)|se<=0)||any(abs(cf)>25|se>100))return(fail("UNSTABLE_COEFFICIENTS"))
    ll1<-stats::logLik(fits$full);ll0<-stats::logLik(fits$reduced);df<-as.integer(attr(ll1,"df")-attr(ll0,"df"));lr<-2*as.numeric(ll1-ll0)
    if(df!=length(expected_levels)-1L||!is.finite(lr)||lr< -1e-7)return(fail("NESTED_MODEL_COMPARISON_FAILED"))
    coefout<-data.frame(cohort=cohort_name,model=model_id,measure=kind,time_definition=time_col,term=names(cf),log_estimate=as.numeric(cf),standard_error=as.numeric(se),estimate=exp(cf),lower=exp(cf-1.95996398454*se),upper=exp(cf+1.95996398454*se),p_value=2*stats::pnorm(-abs(cf/se)),row.names=NULL)
    # model.matrix assign identifies exactly the factor's k-1 treatment columns, independent of term names.
    phcols<-colnames(mm)[attr(mm,"assign")==1L];jj<-match(phcols,coefout$term)
    if(anyNA(jj)||length(jj)!=length(expected_levels)-1L)return(fail("PHENOTYPE_COEFFICIENT_MAPPING_FAILED"))
    contrast_template[,c("estimate","lower","upper","p_value")]<-coefout[jj,c("estimate","lower","upper","p_value")]
    contrast_template$status<-"OK";overall$status<-"OK";overall$reason<-"";overall$phenotype_df<-df;overall$lrt_statistic<-max(0,lr);overall$p_value<-stats::pchisq(max(0,lr),df,lower.tail=FALSE)
    ph<-NULL
    if(kind=="HR"){
      ph<-tryCatch({zz<-survival::cox.zph(fits$full,transform="km",terms=TRUE,global=TRUE)$table
        data.frame(cohort=cohort_name,model=model_id,time_definition=time_col,term=rownames(zz),chisq=zz[,1],df=zz[,2],p_value=zz[,3],status="OK",row.names=NULL)},error=function(e){add_warning(conditionMessage(e),paste(model_id,"PH"));data.frame(cohort=cohort_name,model=model_id,time_definition=time_col,term="GLOBAL",chisq=NA_real_,df=NA_real_,p_value=NA_real_,status="PH_TEST_FAILED")})
    }
    list(contrasts=contrast_template,overall=overall,coefficients=coefout,ph=ph,fit=fits$full,rows=which(eligible))
  }
  association_suite<-function(d,levels){
    basic<-c("age10","sex_male","in_index_icu");hist<-as.vector(t(outer(paste0("history_",history_vars),c("_documented","_unknown"),paste0)));early<-paste0("early_",main_domains)
    adjustments<-list(M1=character(),M2=basic,M3=c(basic,hist),M3_CC=c(basic,hist),M4=c(basic,hist,early))
    common<-stats::complete.cases(d[c(basic,hist,early)])
    fitlist<-list()
    for(model in names(adjustments))for(kind in c("OR","HR")){
      dd<-if(model%in%c("M3_CC","M4"))d[common,,drop=FALSE]else d
      fitlist[[paste(model,kind,sep="_")]]<-fit_association(dd,adjustments[[model]],kind,model,expected_levels=levels)
    }
    # Whole eligible date interval must fit in (0,30] for lower/upper time sensitivity. No clipping.
    time_ok<-is.finite(d$time_lower)&is.finite(d$time_upper)&d$time_lower>0&d$time_upper<=30
    for(model in c("M3","M4"))for(time in c("time","time_lower","time_upper")){
      use<-time_ok;if(model=="M4")use<-use&common
      name<-paste(model,"HR",time,"bounds",sep="_")
      fitlist[[name]]<-fit_association(d[use,,drop=FALSE],adjustments[[model]],"HR",model,cohort_name="evaluation_time_interval_eligible",time_col=time,expected_levels=levels)
    }
    bind<-function(nm){z<-lapply(fitlist,`[[`,nm);z<-z[!vapply(z,is.null,logical(1))];if(length(z))do.call(rbind,z)else data.frame()}
    list(contrasts=bind("contrasts"),overall=bind("overall"),coefficients=bind("coefficients"),ph=bind("ph"),models=fitlist,
      time_audit=data.frame(evaluation_n=nrow(d),events=sum(d$event),approximate_event_n=sum(d$approximate==1),interval_eligible_n=sum(time_ok),interval_excluded_n=sum(!time_ok),early_complete_n=sum(common),early_complete_events=sum(d$event[common])))
  }

  pdf_backend<-"uninitialized"
  open_pdf<-function(p,width,height,pointsize){
    ok<-FALSE
    if(isTRUE(capabilities("cairo")))ok<-tryCatch({grDevices::cairo_pdf(p,width=width,height=height,pointsize=pointsize,family=cfg$PDF_FAMILY,onefile=TRUE);TRUE},error=function(e){add_warning(conditionMessage(e),"PDF Cairo");FALSE})
    if(ok){pdf_backend<<-"cairo_pdf; font subsetting/embedding";return(invisible(TRUE))}
    grDevices::pdf(p,width=width,height=height,pointsize=pointsize,family="Helvetica",onefile=TRUE,useDingbats=FALSE)
    pdf_backend<<-"pdf Helvetica fallback; verify/embed fonts before publication"
  }
  save_pages<-function(stem,drawers,width=13,height=10){
    p<-reg(paste0("to_share/",stem,".pdf"));open_pdf(p,width,height,20)
    tryCatch(for(f in drawers)f(),finally=grDevices::dev.off())
    if(isTRUE(cfg$MAKE_PNG))for(i in seq_along(drawers)){
      suf<-if(length(drawers)>1)paste0("_",i)else""
      grDevices::png(reg(paste0("to_share/",stem,suf,".png")),width=width,height=height,units="in",res=300,pointsize=20)
      tryCatch(drawers[[i]](),finally=grDevices::dev.off())
    }
  }
  pformat<-function(x)ifelse(!is.finite(x),"NE",ifelse(x<.001,"<0.001",sprintf("%.3f",x)))
  ciformat<-function(x,l,u)ifelse(!is.finite(x)|!is.finite(l)|!is.finite(u),"NE",sprintf("%.2f (%.2f, %.2f)",x,l,u))
  three_line19<-function(tables,title,stem,notes){
    drawers<-lapply(seq_along(tables),function(k)local({kk<-k;function(){
      t<-tables[[kk]];nr<-nrow(t);nc<-ncol(t);if(nr>8||nc>5)stop("表格超过大字号页面容量。")
      grid::grid.newpage();tx<-function(s,x,y,just="left",bold=FALSE,size=18)grid::grid.text(s,x,y,just=just,gp=grid::gpar(fontfamily="sans",fontsize=size,fontface=if(bold)"bold"else"plain"))
      tx(title,.04,.95,bold=TRUE,size=24);tx(names(tables)[kk],.04,.89,bold=TRUE,size=20)
      xs<-switch(as.character(nc),`5`=c(.04,.44,.65,.81,.97),`4`=c(.04,.50,.73,.97),`3`=c(.04,.63,.97),`2`=c(.04,.97))
      line<-function(y,lwd)grid::grid.lines(c(.04,.97),c(y,y),gp=grid::gpar(lwd=lwd))
      line(.835,1.4);for(j in seq_len(nc))tx(names(t)[j],xs[j],.80,if(j==1)"left"else"right",TRUE);line(.77,.8)
      ys<-.745-seq_len(nr)*.052
      for(i in seq_len(nr))for(j in seq_len(nc))tx(as.character(t[i,j]),xs[j],ys[i],if(j==1)"left"else"right")
      bottom<-if(nr)min(ys)-.025 else .70;line(bottom,1.4);ny<-bottom-.05
      for(note in notes)for(s in strwrap(note,width=108)){if(ny<.05)stop("表格注释溢出。");tx(s,.04,ny);ny<-ny-.035}
      tx(paste(kk,"/",length(tables)),.97,.025,"right")
    }}))
    # Tables are PDF+editable RTF; PNG is reserved for figures.
    p<-reg(paste0("to_share/",stem,".pdf"));open_pdf(p,16,10,18);tryCatch(for(f in drawers)f(),finally=grDevices::dev.off())
    esc<-function(x){x<-gsub("\\","\\\\",x,fixed=TRUE);x<-gsub("{","\\{",x,fixed=TRUE);gsub("}","\\}",x,fixed=TRUE)}
    rt<-c("{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 Arial;}}\\landscape\\paperw23040\\paperh14400\\margl600\\margr600\\margt600\\margb600\\f0\\fs36")
    for(k in seq_along(tables)){
      if(k>1)rt<-c(rt,"\\page");t<-tables[[k]];rt<-c(rt,paste0("\\pard\\b\\fs44 ",esc(title),"\\par\\fs38 ",esc(names(tables)[k]),"\\b0\\par\\par"));rows<-rbind(names(t),as.matrix(t))
      ends<-switch(as.character(ncol(t)),`5`=c(7200,11000,14800,18100,21500),`4`=c(7200,12000,16800,21500),`3`=c(8500,15000,21500),`2`=c(11000,21500))
      for(i in seq_len(nrow(rows))){bd<-paste0(if(i==1)"\\clbrdrt\\brdrs\\brdrw15\\clbrdrb\\brdrs\\brdrw10"else"",if(i==nrow(rows))"\\clbrdrb\\brdrs\\brdrw15"else"")
        rt<-c(rt,paste0("\\trowd\\trgaph60\\trkeep",paste0(bd,"\\cellx",ends,collapse=""),paste(vapply(seq_len(ncol(t)),function(j)paste0("\\pard\\intbl",if(j==1)"\\ql "else"\\qr ","\\fs36 ",if(i==1)"\\b "else"\\b0 ",esc(rows[i,j]),"\\cell"),character(1)),collapse=""),"\\row"))}
      rt<-c(rt,paste0("\\pard\\fs36 ",paste(esc(notes),collapse="\\par "),"\\par"))
    };txt_out(c(rt,"}"),paste0("to_share/",stem,".rtf"))
  }
  make_visuals<-function(profile,assoc,census,levels){
    colors<-setNames(c("#477EBB","#D98936","#8D68AD","#499782","#C76768","#757575")[seq_along(levels)],levels)
    for(role in c("discovery","evaluation"))save_pages(paste0("Figure_S_Step19_",role,"_profiles"),list(local({rr<-role;function(){
      par(mfrow=c(2,2),mar=c(4.8,4.5,2.7,1.1),oma=c(3.8,0,4.5,0),cex=1,cex.axis=1,cex.lab=1,las=1)
      for(j in seq_along(main_domains)){
        plot(NA,xlim=c(.8,3.2),ylim=c(0,100),xaxt="n",xlab="Cumulative window",ylab="Reference percentile score",bty="l")
        axis(1,at=1:3,labels=c("0-6 h","0-24 h","0-48 h"))
        for(g in levels){z<-profile[profile$scope==rr&profile$domain==main_domains[j]&profile$phenotype==g,];z<-z[match(windows,z$window),]
          lines(1:3,z$median,lwd=2.8,col=colors[g]);points(1:3,z$median,pch=16,col=colors[g],cex=1.1)
          segments(1:3,z$q25,1:3,z$q75,col=grDevices::adjustcolor(colors[g],alpha.f=.55),lwd=2)}
        title(domain_labels[j],cex.main=1.05)
      }
      mtext(paste0(if(rr=="discovery")"Discovery"else"Held-out evaluation",": frozen phenotype profiles"),3,outer=TRUE,line=2.5,font=2,cex=1.15)
      mtext("Median and IQR; observed scores only",3,outer=TRUE,line=.8,cex=1)
      for(k in seq_along(levels)){cx<-.5+(k-(length(levels)+1)/2)*.11
        grid::grid.lines(c(cx-.038,cx-.008),c(.038,.038),gp=grid::gpar(col=colors[k],lwd=3))
        grid::grid.circle(cx-.023,.038,r=grid::unit(1.8,"mm"),gp=grid::gpar(col=colors[k],fill=colors[k]))
        grid::grid.text(levels[k],cx+.01,.038,just="left",gp=grid::gpar(fontsize=22,fontfamily="sans"))}
    }})),width=16,height=12)
    core<-assoc$contrasts[assoc$contrasts$cohort=="evaluation"&assoc$contrasts$model%in%c("M1","M2","M3","M4"),]
    for(kind in c("OR","HR")){
      z<-core[core$measure==kind,];z<-z[order(match(z$model,c("M1","M2","M3","M4")),match(z$phenotype,levels)),]
      pages<-split(seq_len(nrow(z)),ceiling(seq_len(nrow(z))/5));if(!length(pages))pages<-list(integer())
      drawers<-lapply(pages,function(ii)local({tab<-z[ii,,drop=FALSE];function(){
        grid::grid.newpage();tx<-function(s,x,y,just="left",bold=FALSE,size=20)grid::grid.text(s,x,y,just=just,gp=grid::gpar(fontfamily="sans",fontsize=size,fontface=if(bold)"bold"else"plain"))
        tx(paste("Held-out phenotype association:",kind),.04,.95,bold=TRUE,size=26)
        tx(if(kind=="OR")"Primary endpoint: death within 30 days after T48"else"Secondary Cox analysis; date-only times use midpoints",.04,.89,size=20)
        tx("Model / contrast",.04,.82,bold=TRUE);tx(paste0(kind," (95% CI)"),.97,.82,"right",TRUE)
        good<-tab$status=="OK"&is.finite(tab$lower)&tab$lower>0&is.finite(tab$upper)
        vals<-c(1,tab$lower[good],tab$upper[good]);lims<-range(log10(vals));if(diff(lims)<.5)lims<-lims+c(-.3,.3);lims<-lims+c(-.1,.1)*diff(lims)
        xx<-function(v).43+.29*(log10(v)-lims[1])/diff(lims)
        ys<-.74-(seq_len(nrow(tab))-1)*.073;bottom<-if(length(ys))min(ys)-.055 else .3
        grid::grid.lines(c(xx(1),xx(1)),c(bottom,.78),gp=grid::gpar(lty=2,col="#888888"))
        for(i in seq_len(nrow(tab))){bold<-tab$model[i]=="M3";lab<-paste0(tab$model[i],": ",tab$phenotype[i]," vs ",tab$reference[i],"  (n=",tab$n[i],")")
          tx(lab,.04,ys[i],bold=bold)
          if(good[i]){grid::grid.lines(xx(c(tab$lower[i],tab$upper[i])),rep(ys[i],2),gp=grid::gpar(col=colors[tab$phenotype[i]],lwd=2.6));grid::grid.circle(xx(tab$estimate[i]),ys[i],r=grid::unit(2,"mm"),gp=grid::gpar(col=colors[tab$phenotype[i]],fill=colors[tab$phenotype[i]]))}
          tx(ciformat(tab$estimate[i],tab$lower[i],tab$upper[i]),.97,ys[i],"right",bold)
        }
        ticks<-10^pretty(lims,n=3);ticks<-ticks[log10(ticks)>=lims[1]&log10(ticks)<=lims[2]]
        grid::grid.lines(c(.43,.72),rep(bottom,2),gp=grid::gpar(lwd=1))
        for(v in ticks){grid::grid.lines(rep(xx(v),2),c(bottom,bottom-.012));tx(format(signif(v,2),trim=TRUE),xx(v),bottom-.04,"centre",size=20)}
        tx(paste0(kind,"; logarithmic scale"),.575,bottom-.09,"centre",size=20)
        foot<-c("M1: unadjusted; M2: age, sex and ICU state at T48.","M3: M2 + documented prior-history categories (primary OR model).","M4: M3 + four early 0-6 h scores, complete cases.","NE = not estimable. Intervals condition on the frozen discovery partition.")
        for(j in seq_along(foot))tx(foot[j],.04,.17-(j-1)*.035,size=18)
      }}))
      save_pages(paste0(if(kind=="OR")"Figure_3E_heldout_"else"Figure_3F_heldout_",kind),drawers,width=14,height=11)
    }
    tables<-list()
    for(g in setdiff(levels,levels[1])){
      tab<-data.frame(Model=names(model_labels),`N / deaths`="",`OR (95% CI)`="",`HR (95% CI)`="",check.names=FALSE)
      for(i in seq_len(nrow(tab))){o<-assoc$contrasts[assoc$contrasts$cohort=="evaluation"&assoc$contrasts$model==tab$Model[i]&assoc$contrasts$phenotype==g,]
        or<-o[o$measure=="OR",];hr<-o[o$measure=="HR",];tab[i,2]<-paste(or$n,or$events,sep=" / ");tab[i,3]<-ciformat(or$estimate,or$lower,or$upper);tab[i,4]<-ciformat(hr$estimate,hr$lower,hr$upper)}
      tables[[paste(g,"versus",levels[1])]]<-tab
    }
    if(!length(tables))tables<-list("No supported partition"=data.frame(Result="No categorical OR or HR is estimable",Reason="Only one phenotype"))
    notes<-c("M1: unadjusted; M2: age, sex and ICU state at T48; M3: M2 + eight documented prior histories.",
      "M4: M3 + four early 0-6 h scores (complete cases). M3_CC uses the same patients as M4.",
      "OR is primary; midpoint-time HR is secondary. 95% Wald intervals condition on discovery phenotyping.",
      "Unknown history is a separate record category. NE: not estimable; see diagnostics CSV.")
    three_line19(tables,"Held-out categorical phenotype associations","Table_S_Step19_OR_HR",notes)
    o<-assoc$overall[assoc$overall$cohort=="evaluation",];tab<-data.frame(Model=names(model_labels),`N / deaths`="",`OR overall P`="",`HR overall P`="",check.names=FALSE)
    for(i in seq_len(nrow(tab))){a<-o[o$model==tab$Model[i]&o$measure=="OR",];h<-o[o$model==tab$Model[i]&o$measure=="HR",];tab[i,2]<-paste(a$n,a$events,sep=" / ");tab[i,3]<-pformat(a$p_value);tab[i,4]<-pformat(h$p_value)}
    three_line19(list("Categorical phenotype term"=tab),"Overall likelihood-ratio tests","Table_S_Step19_overall_tests",c("Full and reduced models use exactly the same patients and adjustment variables.","Tests have k-1 degrees of freedom; no ordinal phenotype trend test is performed.","P values are nominal. M3 OR is the primary association; other analyses are secondary.","Model definitions follow the categorical association table. NE: not estimable."))
    ht<-assoc$contrasts[assoc$contrasts$cohort=="evaluation_time_interval_eligible",];tt<-list()
    for(g in setdiff(levels,levels[1])){
      tab<-data.frame(Model=rep(c("M3","M4"),each=3),Time=rep(c("time","time_lower","time_upper"),2),`N / deaths`="",`HR (95% CI)`="",check.names=FALSE)
      for(i in seq_len(nrow(tab))){z<-ht[ht$model==tab$Model[i]&ht$time_definition==tab$Time[i]&ht$phenotype==g,];tab[i,3]<-paste(z$n,z$events,sep=" / ");tab[i,4]<-ciformat(z$estimate,z$lower,z$upper)}
      tab$Time<-rep(c("Midpoint","Lower bound","Upper bound"),2);tt[[paste(g,"versus",levels[1])]]<-tab
    }
    if(!length(tt))tt<-list("No supported partition"=data.frame(Result="No categorical HR is estimable",Reason="Only one phenotype"))
    three_line19(tt,"Death-time precision sensitivity","Table_S_Step19_time_sensitivity",c("All three time assignments use the same interval-eligible patients within each model.","Date-only deaths use midpoint, lower or upper interval values; event labels remain frozen.","Intervals not wholly inside (0,30] days are excluded here, without clipping event times.","These are sensitivity scenarios, not interval-censored Cox estimates or exact death times."))
    csv_out(data.frame(backend=pdf_backend,requested_family=cfg$PDF_FAMILY,figure_pointsize=20,table_pointsize=18),"to_share/08_graphics_audit.csv")
    if(grepl("fallback",pdf_backend))issue("LIMITATION","PDF fonts","Cairo不可用，已输出Helvetica PDF；投稿前需检查/嵌入字体。PNG不受此限制。")
  }

  synthetic_raw<-function(n=300L){
    with_seed(719L,{
      sp<-engine$specification();cols<-unique(unlist(lapply(windows,function(w)engine$key(sp$variable,w,sp$stat))))
      latent<-rep(c(-1.6,1.6),length.out=n)+stats::rnorm(n,0,.5)
      raw<-as.data.frame(matrix(0,n,length(cols)));names(raw)<-cols
      for(nm in cols){x<-latent+stats::rnorm(n,0,.65);if(grepl("__min_value$",nm))x<--x;if(grepl("__abnormal_fraction$",nm))x<-stats::plogis(x);raw[[nm]]<-x}
      raw[seq(9,n,23),seq(1,ncol(raw),9)]<-NA_real_;raw
    })
  }
  preflight<-function(){
    stopifnot(identical(numeric_days(c("30","1.25","2e-1",NA_character_,"","NA"),"test"),c(30,1.25,.2,NA_real_,NA_real_,NA_real_)),
      identical(numeric_days(factor(c("2.5","30"),levels=c("30","2.5")),"factor"),c(2.5,30)),
      identical(numeric_days(c(NA,NA),"all-missing"),c(NA_real_,NA_real_)))
    for(x in list(c("30","bad"),c("1,000","30"),"2 days","Inf","NaN","1e999",TRUE,as.Date("2026-01-01")))
      stopifnot(inherits(try(numeric_days(x,"invalid-test"),silent=TRUE),"try-error"))
    oldplan<-plan;on.exit(plan<<-oldplan,add=TRUE)
    plan<<-utils::modifyList(plan,list(k=2:3,min_cluster_n=5L,stability_repeats=2L,nstart=4L,silhouette_max_n=240L))
    raw<-synthetic_raw();tr<-1:210;va<-211:300
    state<-fit_discovery(raw[tr,,drop=FALSE]);modified<-raw;modified[va,]<-10000
    other<-fit_discovery(modified[tr,,drop=FALSE]);stopifnot(identical(state,other),state$reference$training_n==210L)
    a<-assign_frozen(raw[va,,drop=FALSE],state);single<-assign_frozen(raw[va[2],,drop=FALSE],state)
    stopifnot(identical(single$phenotype,a$phenotype[2]),isTRUE(all.equal(as.numeric(single$scores[1,]),as.numeric(a$scores[2,]),tolerance=1e-12)),length(assign_frozen(raw[FALSE,,drop=FALSE],state)$phenotype)==0L)
    b<-list(metadata=data.frame(patient_key=paste0("m:",1:4),stay_id=as.character(1:4)))
    s<-data.frame(stay_id=c("4","2","1","3","9"),split=c("Test","Train","Train","Test","Train"))
    al<-align_split(b,s);stopifnot(identical(al$role,c("discovery","discovery","evaluation","evaluation")),al$excluded_legacy_n==1)
    stopifnot(inherits(try(align_split(b,rbind(s,s[1,])),silent=TRUE),"try-error"))
    d<-data.frame(event=c(rep(1,20),rep(0,80),rep(1,40),rep(0,60)),phenotype=factor(rep(c("P1","P2"),each=100)),time=rep(30,200))
    d$time[d$event==1]<-seq(1,29,length.out=60)
    z<-fit_association(d,character(),"OR","test");expected<-40*80/(60*20);se<-sqrt(1/40+1/80+1/60+1/20)
    stopifnot(z$overall$status=="OK",abs(z$contrasts$estimate-expected)<1e-7,abs(log(z$contrasts$upper)-log(expected)-1.95996398454*se)<1e-6,z$overall$phenotype_df==1L)
    observed<-matrix(c(80,20,60,40),2,byrow=TRUE);expected_counts<-outer(rowSums(observed),colSums(observed))/sum(observed)
    deviance<-2*sum(observed*log(observed/expected_counts));stopifnot(abs(z$overall$lrt_statistic-deviance)<1e-7)
    d$age10<-with_seed(192L,stats::rnorm(200));d$age10[1]<-NA_real_
    z2<-fit_association(d,"age10","OR","test2");stopifnot(z2$overall$status=="OK",z2$overall$n==199,z2$overall$excluded_n==1)
    empty<-fit_association(d[FALSE,],character(),"OR","empty");stopifnot(empty$overall$status=="NOT_ESTIMABLE")
    zero<-d;zero$event[zero$phenotype=="P2"]<-0;stopifnot(fit_association(zero,character(),"OR","zero")$overall$reason=="ZERO_EVENT_OR_NONEVENT_CELL")
    z3<-d;z3$phenotype<-factor(rep(c("P1","P2","P3","P2"),length.out=nrow(z3)));z3$age10<-NULL
    f3<-fit_association(z3,character(),"OR","three");stopifnot(f3$overall$phenotype_df==2L,nrow(f3$contrasts)==2L)
    cox<-fit_association(d,character(),"HR","cox");stopifnot(cox$overall$status=="OK",nrow(cox$ph)>=2L)
    invisible(TRUE)
  }

  legacy_cache_valid<-function(cache,hashes,discovery_keys,evaluation_keys){
    # Original keys serialized source references, so a renamed correction invalidated them.
    # Reuse only if independently saved inputs, plan, patient keys and full fitted state agree.
    isTRUE(tryCatch({
      p<-previous_run
      !is.null(p)&&!is.null(cache)&&is.null(cache$key_format)&&isTRUE(cache$complete)&&
        identical(as.character(p$inputs$md5),as.character(hashes))&&
        identical(p$frozen$version,"Step19_v1")&&identical(p$frozen$plan,plan)&&
        identical(p$frozen$split_md5,hashes[4])&&
        identical(p$frozen$discovery_keys,discovery_keys)&&identical(p$frozen$evaluation_keys,evaluation_keys)&&
        identical(p$frozen$state,cache$state)
    },error=function(e)FALSE))
  }
  verify_lineage<-function(step){
    root<-file.path(cfg$OUTPUT_PARENT,step);p<-file.path(root,"to_share",paste0(step,"_STATUS.txt"))
    if(!file.exists(p)||!"STATUS = COMPLETED_FOR_REVIEW"%in%readLines(p,warn=FALSE))stop("请先完成",step,"。")
    z<-utils::read.csv(file.path(root,"to_share/01_inputs.csv"),stringsAsFactors=FALSE)
    required(z,c("file","md5"),paste(step,"输入审计"))
    for(i in seq_len(nrow(z)))if(!file.exists(z$file[i])||unname(tools::md5sum(z$file[i]))!=z$md5[i])stop(step,"上游输入发生变化或不可访问；先核对上游结果。")
  }
  main<-function(){
    missing<-c("cluster","survival")[!vapply(c("cluster","survival"),requireNamespace,logical(1),quietly=TRUE)]
    if(length(missing))stop("缺少R包：",paste(missing,collapse=", "),"。请运行 install.packages(c('cluster','survival')) 后重试。")
    for(st in c("Step6","Step7"))verify_lineage(st)
    s6<-file.path(cfg$OUTPUT_PARENT,"Step6/local_only");s7<-file.path(cfg$OUTPUT_PARENT,"Step7/local_only")
    split_path<-norm(cfg$LEGACY_SPLIT_FILE)
    paths<-c(bundle=file.path(s6,"MIMIC_T48_model_inputs.rds"),engine=file.path(s6,"Step6_score_engine.R"),previous_plan=file.path(s7,"MIMIC_T48_frozen_pipeline.rds"),legacy_split=split_path)
    if(!all(file.exists(paths)))stop("必要文件缺失：",paste(basename(paths[!file.exists(paths)]),collapse=", "),"。历史分组可用 options(ami_step19.config=list(LEGACY_SPLIT_FILE='完整路径')) 指定。")
    hashes<-unname(tools::md5sum(paths));csv_out(data.frame(file=unname(paths),md5=hashes),"to_share/01_inputs.csv")
    if(length(cfg$LEGACY_SPLIT_MD5)!=1L||is.na(cfg$LEGACY_SPLIT_MD5)||!nzchar(cfg$LEGACY_SPLIT_MD5)||hashes[4]!=cfg$LEGACY_SPLIT_MD5)stop("历史70/30分组文件与Step0已审计版本不一致；不会重新分组。请使用原Step05_train_test_split.csv。")
    ee<-new.env(parent=baseenv());sys.source(paths[2],envir=ee);engine<<-ee$step6_score_engine
    previous<-readRDS(paths[3]);if(previous$version!="Step7_v1"||previous$endpoint!="30_day_all_cause_death_after_T48")stop("Step7分型规则来源不符。")
    same<-c("k","min_cluster_fraction","min_cluster_n","min_silhouette","min_stability_ARI","stability_repeats","stability_fraction","silhouette_max_n","nstart","iter_max","min_reference_n","min_domain_fraction","preprocess_min_n","max_missing")
    for(v in same)if(!isTRUE(all.equal(plan[[v]],previous$plan[[v]])))stop("Step7与Step19分型规则不一致：",v)
    txt_out(capture.output(dput(plan)),"to_share/02_analysis_plan.txt")
    logmsg("Step19-1预检：随访数字/字符类型、历史分组键、发现/评价隔离、冻结赋类、分类OR、总体检验、近似死亡时间和空组。")
    preflight();txt_out("PASS: strict numeric day parsing; key alignment; discovery-only scoring/clustering; single/batch assignment; independent 2x2 OR oracle; nested likelihood-ratio test; sparse/empty group handling; date precision checks.","to_share/02_preflight.txt")
    b<-readRDS(paths[1]);required(b,c("metadata","raw_numeric","baseline","clinical_documentation","outcomes"),"Step6数据")
    required(b$metadata,c("patient_key","stay_id","source","landmark"),"患者元数据")
    checked_key(b$metadata$patient_key,"patient_key");checked_key(b$metadata$stay_id,"stay_id")
    if(b$version!="Step6_v1"||nrow(b$metadata)!=5133L||sum(bit(b$outcomes$event_primary,"结局"))!=630L||any(b$metadata$source!="MIMIC")||any(b$metadata$landmark!="T48")||any(b$outcomes$endpoint!="30_day_all_cause_death_after_landmark"))stop("MIMIC T48冻结队列或终点不符。")
    for(v in c("raw_numeric","baseline","clinical_documentation","outcomes"))if(nrow(b[[v]])!=nrow(b$metadata))stop("Step6患者表行数不一致：",v)
    old<-utils::read.csv(split_path,stringsAsFactors=FALSE,colClasses="character",check.names=FALSE)
    split<-align_split(b,old);tr<-which(split$role=="discovery");va<-which(split$role=="evaluation")
    if(length(intersect(b$metadata$patient_key[tr],b$metadata$patient_key[va])))stop("发现/评价患者重叠。")
    csv_out(data.frame(legacy_total_n=nrow(old),frozen_matched_n=split$matched_n,excluded_legacy_n=split$excluded_legacy_n,discovery_n=length(tr),evaluation_n=length(va),overlap_n=0L,split_md5=hashes[4]),"to_share/02_split_audit.csv")
    current_module<<-"discovery-only phenotype fitting"
    logmsg(sprintf("沿用历史划分：发现集%d人，评价集%d人；仅发现集拟合参考、填补、缩放和分型。",length(tr),length(va)))
    key<-hash_object(list(inputs=hashes,plan=plan,R=as.character(getRversion()),cluster=as.character(utils::packageVersion("cluster")),functions=lapply(c("fit_discovery","fit_clusters","km_fit","nearest"),function(f)deparse(body(get(f)),width.cutoff=500L))))
    cp<-safe_path("local_only/cache/discovery_phenotyping.rds");cache<-if(isTRUE(cfg$RESUME)&&file.exists(cp))tryCatch(readRDS(cp),error=function(e)NULL)else NULL
    if(!is.null(cache)&&identical(cache$key,key)&&identical(cache$complete,TRUE)){
      state<-cache$state;logmsg("复用缓存：发现集冻结分型。")
    }else if(isTRUE(cfg$RESUME)&&legacy_cache_valid(cache,hashes,b$metadata$patient_key[tr],b$metadata$patient_key[va])){
      state<-cache$state
      saveRDS(list(key=key,key_format="Step19-1-source-independent",complete=TRUE,state=state),reg("local_only/cache/discovery_phenotyping.rds"),compress="gzip")
      logmsg("复用缓存：已核对原输入、分组、分析方案和完整分型对象；仅更新缓存兼容标记。")
    }else{
      state<-fit_discovery(b$raw_numeric[tr,,drop=FALSE]);saveRDS(list(key=key,key_format="Step19-1-source-independent",complete=TRUE,state=state),reg("local_only/cache/discovery_phenotyping.rds"),compress="gzip")
    }
    if(state$reference$training_n!=length(tr)||state$preprocessor$training_n!=length(tr)||state$cluster$training_n!=length(tr))stop("发现集拟合人数不一致。")
    labels<-paste0("P",seq_len(state$cluster$k));a<-assign_frozen(b$raw_numeric,state)
    one<-assign_frozen(b$raw_numeric[va[1],,drop=FALSE],state)
    if(!identical(one$phenotype,a$phenotype[va[1]])||!isTRUE(all.equal(as.numeric(one$scores[1,]),as.numeric(a$scores[va[1],]),tolerance=1e-12)))stop("单例/批量赋类不一致。")
    saveRDS(list(version="Step19_v1",plan=plan,state=state,discovery_keys=b$metadata$patient_key[tr],evaluation_keys=b$metadata$patient_key[va],split_md5=hashes[4]),reg("local_only/Step19_frozen_phenotyping.rds"),compress="gzip")
    csv_out(state$cluster$audit,"to_share/03_discovery_k_selection.csv")
    csv_out(data.frame(selected_k=state$cluster$k,discovery_n=length(tr),evaluation_n=length(va),reference=labels[1],labels="Discovery centroid order; no outcome ordering"),"to_share/03_selected_partition.csv")
    sp<-state$reference$spec;sp$training_observed_n<-vapply(state$reference$reference,function(r)r$n,integer(1));sp$enabled<-vapply(state$reference$reference,function(r)r$enabled,logical(1));csv_out(sp,"to_share/03_reference_availability.csv")
    csv_out(data.frame(feature=colnames(state$cluster$centers),t(state$cluster$centers),check.names=FALSE),"to_share/03_standardized_centroids.csv")
    profile<-do.call(rbind,lapply(c("discovery","evaluation"),function(role){ii<-which(split$role==role);profile_summary(list(phenotype=a$phenotype[ii],scores=a$scores[ii,,drop=FALSE]),role,labels)}));csv_out(profile,"to_share/03_domain_profiles.csv")
    current_module<<-"held-out categorical associations"
    d<-prepare_association(b,a);d$phenotype<-factor(a$phenotype,levels=labels)
    csv_out(attr(d,"followup_type_audit"),"to_share/02_followup_type_audit.csv")
    census<-do.call(rbind,lapply(c("discovery","evaluation"),function(role)do.call(rbind,lapply(labels,function(g){ii<-which(split$role==role&a$phenotype==g);data.frame(role=role,phenotype=g,n=length(ii),events=sum(d$event[ii]),event_fraction=if(length(ii))mean(d$event[ii])else NA_real_,approximate_event_n=sum(d$approximate[ii]))}))))
    csv_out(census,"to_share/04_split_phenotype_counts.csv")
    missingness<-do.call(rbind,lapply(c("discovery","evaluation"),function(role)do.call(rbind,lapply(setdiff(names(d),c("event","phenotype")),function(v){ii<-which(split$role==role);data.frame(role=role,variable=v,n=length(ii),missing_n=sum(is.na(d[[v]][ii])),unique_observed_n=length(unique(stats::na.omit(d[[v]][ii]))))}))))
    csv_out(missingness,"to_share/04_covariate_availability.csv")
    csv_out(do.call(rbind,lapply(history_vars,function(v)do.call(rbind,lapply(c("discovery","evaluation"),function(role){ii<-which(split$role==role);u<-d[[paste0("history_",v,"_unknown")]][ii];p<-d[[paste0("history_",v,"_documented")]][ii];data.frame(role=role,history=v,n=length(ii),documented_n=sum(p==1),not_documented_n=sum(p==0&u==0),unknown_n=sum(u==1))})))),"to_share/04_history_categories.csv")
    logmsg(sprintf("冻结分型选定k=%d；评价集%d人、%d例30天死亡，计算分类OR/HR及总体检验。",state$cluster$k,length(va),sum(d$event[va])))
    result<-association_suite(d[va,,drop=FALSE],labels)
    csv_out(result$contrasts,"to_share/05_categorical_OR_HR.csv");csv_out(result$overall,"to_share/05_overall_tests_and_diagnostics.csv")
    csv_out(result$coefficients,"to_share/05_all_coefficients.csv");csv_out(result$ph,"to_share/06_proportional_hazards_tests.csv");csv_out(result$time_audit,"to_share/06_death_time_audit.csv")
    bad<-result$overall[result$overall$status!="OK",]
    if(nrow(bad))issue("REVIEW","nonestimable associations",paste(nrow(bad),"个模型/场景未能估计；详见05_overall_tests_and_diagnostics.csv。不替换类别、不筛选有利结果。"))
    if(nrow(result$ph)&&any(result$ph$term%in%c("phenotype","GLOBAL")&result$ph$p_value<.05,na.rm=TRUE))issue("REVIEW","proportional hazards","至少一个表型/总体比例风险检验P<0.05；HR不能不加说明地解释为恒定风险比，主要解读固定30天OR。")
    if(any(result$overall$events_per_parameter<10,na.rm=TRUE))issue("REVIEW","sparse adjustment","部分调整模型事件数/参数数小于10；结合CI及模型稳定性解读，不视为自动合格/不合格阈值。")
    if(any(census$n[census$role=="evaluation"]<25))issue("REVIEW","small phenotype","评价集至少一个表型少于25人，需结合事件数与CI解读。")
    saveRDS(list(metadata=b$metadata,split_role=split$role,phenotype=a$phenotype,domain_scores=a$scores,association_data=d,results=result,plan=plan),reg("local_only/Step19_association_results.rds"),compress="gzip")
    current_module<<-"large-font figures and editable tables";make_visuals(profile,result,census,labels)
    csv_out(data.frame(output=c("Figure_3E_heldout_OR","Figure_3F_heldout_HR","Figure_S_Step19_discovery_profiles","Figure_S_Step19_evaluation_profiles","Table_S_Step19_OR_HR","Table_S_Step19_overall_tests","Table_S_Step19_time_sensitivity"),
      action=c("REPLACE Figure 3E: held-out categorical OR; remove old whole-cohort association claim","REPLACE Figure 3F: held-out categorical HR; label HR rather than OR",rep("ADD supplementary",2),"REPLACE Table S4 categorical OR/HR sections","REPLACE Table S4 phenotype overall-test section","ADD supplementary time-precision sensitivity"),
      reviewer=c("R1 major1a/4; R2 major10; R3 major10","R1 major1a/4; R3 major10","R2 major9/10; R3 major1/3","R2 major9/10; R3 major1/3","R1 major1a/4; R3 major10","R1 major4; R3 major10","Endpoint timing precision")),"to_share/11_figure_table_replacement_map.csv")
    csv_out(data.frame(task=c("Held-out categorical phenotype OR/HR","External threshold performance/DCA and final statistical exports","Web prediction consistency","Manuscript and 45 reply units"),status=c("COMPLETED_FOR_REVIEW; examine nonestimable models and diagnostics",rep("PENDING",3))),"to_share/12_revision_progress.csv")
    txt_out(c(
      "Step19-1修复：上游随访天数可为字符型数字；严格转换三个时间字段，保留原缺失与数值，不修改上游RDS，不改变队列/结局/分组/分型。非法字符串、Inf/NaN或带单位内容仍停止；转换审计见02_followup_type_audit.csv。",
      "Step19：历史70/30划分上的发现/评价分型关联分析。不是新的未触碰测试集或外部验证。",
      "本步是原修订计划第4工作包的收尾；脚本编号不等于新增工作包。",
      "使用Step6冻结MIMIC T48队列（5133人、630例30天死亡），按stay_id匹配Step0已审计历史Train/Test分组；不重新抽签，不按结果选择分组。",
      "仅发现集拟合0-6h参考百分位、域评分填补/缩放、k选择和质心。四域、三个累计窗口；k=2至6；门槛与Step7相同。无支持分型时报告k=1，不强制五类。",
      "P1/P2等为本次发现集质心字典序中性标签，不按死亡率排序，不等同于Step7全队列C1/C2或原稿C1-C5；不跨折混合标签。",
      "评价集只应用冻结分型；死亡结局不参与评分拟合、聚类、k选择和标签排序。关联回归在评价集估计，不能据此称回归本身的外部验证。",
      "主结局为T48后30天全因死亡；二分类Logistic OR为主要关联，Cox HR为次要。均按分类变量而非每升高一级分析。",
      "M1未调整；M2调整年龄（每10岁）、性别、T48是否仍在指数ICU；M3另调整8项既往病史记录代理，为主要OR模型。",
      "病史项为既往CAD/MI、心衰、糖尿病、高血压、慢性肾病、COPD、卒中、房颤；MIMIC来自本次住院前已出院的既往编码，不代表完整临床病史。",
      "每项病史保留记录阳性、来源覆盖但未记录、来源未知三态，以阳性/未知两个设计指标表示；完全相同未知指示只保留一份，省略项记录在诊断表。不将未知解释成没有疾病。",
      "M4在M3基础上调整四域0-6h早期评分（每10分）；这是分型输入的一部分，属于条件关联敏感性，不是因果独立作用证明，也不是入院瞬时T0评分。",
      "M4采用早期评分完整病例；M3_CC在同一批患者上重估M3，便于区分样本改变与调整改变。协变量不根据P值筛选，不为关联回归填补早期评分。",
      "总体检验为含/不含完整表型分类项的嵌套似然比检验；同一次比较使用完全相同患者。P值为名义值，M3 OR为主要分析，其余为次要。",
      "95%CI为模型Wald区间，条件于本次冻结分型；不覆盖发现集分型重新拟合的不确定性。不把这些区间写成2000次bootstrap区间。",
      "日期级死亡以既有冻结中点时间作近似HR；另在整个事件区间均处于(0,30]天者中使用中点/下界/上界三种时间，使用相同患者和固定事件标签，不裁剪到边界。",
      "日期上下界为时间精度情景，不是区间删失Cox估计。比例风险检验输出于06表；违反假设时不将HR解释为恒定风险比。",
      "空组、零事件/非事件、秩亏、非收敛和不稳定系数明确标为NE；不会更换参考组、合并类别或挑选较显著的OR/HR。",
      "本步Figure_3E_heldout_OR替换原Figure3E；Figure_3F_heldout_HR替换原Figure3F，明确区分OR与HR；OR/HR和总体检验表替换原Table S4相应关联部分，其余为新增补充。",
      "本步不读取原始大表，不改变Step6-18模型、风险概率、队列或已有结果。",
      "PDF优先使用Cairo嵌入字体；图20pt、表18pt，PNG300dpi，表另有可编辑RTF。若Cairo不可用，08表会明确提示字体检查。",
      "表型患者分组、模型对象与详细错误仅保存在local_only；反馈ZIP仅含汇总表和图，不包含患者标识或逐人数据。",
      "固定文件名；仅覆盖本脚本已登记文件，保留其他用户文件。回传Step19_feedback.zip。"),"to_share/00_READ_ME_FIRST.txt")
    txt_out(c("https://stat.ethz.ch/R-manual/R-devel/library/survival/html/coxph.html","https://stat.ethz.ch/R-manual/R-devel/library/survival/html/cox.zph.html","https://stat.ethz.ch/R-manual/R-devel/library/stats/html/anova.glm.html","https://stat.ethz.ch/R-manual/R-devel/library/cluster/html/silhouette.html"),"to_share/10_method_sources.txt")
    if(!identical(hashes,unname(tools::md5sum(paths))))stop("运行期间输入文件发生变化。")
    status<<-"COMPLETED_FOR_REVIEW"
  }

  if(isTRUE(getOption("ami_step19.functions_only",FALSE)))return(invisible(list(main=main,preflight=preflight,fit_discovery=fit_discovery,assign_frozen=assign_frozen,align_split=align_split,prepare_association=prepare_association,fit_association=fit_association,association_suite=association_suite,make_visuals=make_visuals,synthetic_raw=synthetic_raw)))
  status<-"FAILED";current_module<-"input verification";failure_message<-NULL
  tryCatch(withCallingHandlers(main(),warning=function(w){add_warning(conditionMessage(w),current_module);invokeRestart("muffleWarning")}),error=function(err){
    status<<-"FAILED";failure_message<<-conditionMessage(err);issue("ERROR",current_module,"此模块未完成；完整错误留在local_only。")
    txt_out(c(paste("Module:",current_module),conditionMessage(err)),"local_only/Step19_error_detail.txt")})
  flush_warnings()
  for(nm in ls(agg))csv_out(do.call(rbind,agg[[nm]]),paste0("to_share/",nm,".csv"))
  txt_out(c(paste0("STATUS = ",status),"SCRIPT_VERSION = Step19-1",
    "COMPLETED_FOR_REVIEW表示留出样本分类关联分析及诊断导出完成；NE/警告须审阅，不代表所有表型显著或独立验证成立。",
    "FAILED表示本步未完整完成；检查日志。上游队列和预测保持原样。"),"to_share/Step19_STATUS.txt")
  txt_out(capture.output(sessionInfo()),"to_share/Step19_sessionInfo.txt");logmsg(paste0("Step19状态：",status))
  zip_ok<-FALSE
  if(isTRUE(cfg$MAKE_ZIP)){
    zp<-reg("Step19_feedback.zip");share<-file.path(out,"to_share")
    files<-sort(basename(registered[startsWith(registered,"to_share/")&file.exists(file.path(out,registered))]))
    zip_ok<-tryCatch({if(requireNamespace("zip",quietly=TRUE))zip::zipr(zipfile=zp,files=files,root=share)else{
      wd<-getwd();tryCatch({setwd(share);utils::zip(zipfile=zp,files=files,flags="-q")},finally=setwd(wd))};file.exists(zp)&&file.info(zp)$size>0},error=function(err)FALSE)
    if(!zip_ok&&file.exists(zp))unlink(zp)
  }
  cat("\n固定输出：\n",out,"\n",sep="")
  if(zip_ok)cat("请回传：\n",file.path(out,"Step19_feedback.zip"),"\n",sep="")else cat("请压缩并回传：\n",file.path(out,"to_share"),"\n",sep="")
  if(!is.null(failure_message))cat("本次未完成：",failure_message,"\n")
  invisible(list(status=status,output_dir=out,zip_ok=zip_ok))
})
