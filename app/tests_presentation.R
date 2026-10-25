run_ami_presentation_tests<-function(root){
  e<-new.env(parent=baseenv())
  for(nm in c('predictor.R','presentation.R','app_server.R'))sys.source(file.path(root,nm),e)
  api<-e$ami_api;checks<-character();x<-api$read_csv(file.path(root,'synthetic_example.csv'))
  o<-api$load_bundle(root,'mortality30')
  values<-list(manual_age=x$age[1],manual_sex=x$sex_male[1],manual_icu=x$in_index_icu[1],
    manual_hospital=x$in_index_hospital_at_T48[1],manual_eligible=TRUE,manual_confirm=TRUE)
  for(nm in o$contract$raw_columns)values[[e$ami_field_id(nm)]]<-if(is.na(x[[nm]][1]))'' else x[[nm]][1]
  manual<-e$ami_manual_frame(values,o)
  for(ep in c('mortality30','hospital'))stopifnot(identical(api$request(root,ep,manual,TRUE)$risk_probability,
    api$request(root,ep,x[1,,drop=FALSE],TRUE)$risk_probability))
  checks<-c(checks,'manual_and_csv_both_endpoints_exact')
  for(nm in c('manual_age','manual_sex','manual_icu','manual_hospital','manual_eligible','manual_confirm')){
    bad<-values;bad[[nm]]<-if(is.logical(values[[nm]]))FALSE else ''
    stopifnot(inherits(try(e$ami_manual_frame(bad,o),silent=TRUE),'try-error'))
  }
  checks<-c(checks,'manual_required_profile_and_confirmations')
  empty<-values;for(nm in o$contract$raw_columns)empty[[e$ami_field_id(nm)]]<-''
  stopifnot(inherits(try(api$request(root,'mortality30',e$ami_manual_frame(empty,o),TRUE),silent=TRUE),'try-error'))
  checks<-c(checks,'empty_manual_physiology_rejected')
  bad<-values;bad$manual_hospital<-'0';bad$manual_icu<-'0'
  stopifnot(inherits(try(api$request(root,'hospital',e$ami_manual_frame(bad,o),TRUE),silent=TRUE),'try-error'))
  checks<-c(checks,'manual_hospital_eligibility_enforced')
  shiny::testServer(e$make_ami_server(root,api),{
    session$setInputs(endpoint='mortality30',predict_manual=0,run_demo=0,input_mode='manual')
    do.call(session$setInputs,values);session$flushReact()
    session$setInputs(predict_manual=1)
    expected<-api$request(root,'mortality30',x[1,,drop=FALSE],TRUE)
    stopifnot(!is.null(result()),identical(result()$risk_probability,expected$risk_probability),origin()=='Manual window summaries')
    card<-output$risk_cards$html
    stopifnot(grepl(sprintf('%.2f',100*expected$risk_probability),card,fixed=TRUE),!grepl('Step[0-9]',card))
    checks<<-c(checks,'manual_server_calls_frozen_engine')
    ex<-presentation_export();stopifnot(identical(ex[['Risk (%)']],sprintf('%.2f',100*expected$risk_probability)),
      all(ex[['Input source']]=='Single-patient entry'),!any(c('model_id','model_source_md5','frozen_cluster')%in%names(ex)))
    f<-tempfile(fileext='.csv');e$ami_write_display_csv(ex,f);txt<-readLines(f);unlink(f)
    stopifnot(!any(grepl('Step[0-9]|0x[0-9a-f]|[a-f0-9]{12}',txt)))
    checks<<-c(checks,'readable_report_no_workflow_codes_or_hex')
    session$setInputs(manual_age='61');stopifnot(is.null(result()),inherits(try(presentation_export(),silent=TRUE),'try-error'))
    checks<<-c(checks,'manual_profile_change_clears_risk_and_download')
    do.call(session$setInputs,values);session$setInputs(predict_manual=2);stopifnot(!is.null(result()))
    field<-e$ami_field_id(o$contract$raw_columns[1]);do.call(session$setInputs,stats::setNames(list('not-a-number'),field))
    stopifnot(is.null(result()));session$setInputs(predict_manual=3);stopifnot(is.null(result()))
    checks<<-c(checks,'manual_feature_change_clears_and_invalid_value_rejected')
    do.call(session$setInputs,values);session$setInputs(predict_manual=4);stopifnot(!is.null(result()))
    session$setInputs(input_mode='example');stopifnot(is.null(result()))
    checks<<-c(checks,'input_method_change_clears_risk')
    session$setInputs(run_demo=1);ex<-presentation_export()
    stopifnot(nrow(ex)==nrow(x),all(ex[['Input source']]=='Synthetic example'))
    checks<<-c(checks,'readable_example_report_retains_synthetic_label')
  })
  tmp<-tempfile('ami_entry_disabled_');dir.create(tmp);on.exit(unlink(tmp,recursive=TRUE),add=TRUE)
  writeLines("web_config <- list(allow_upload=FALSE,allow_manual=FALSE,code_url='')",file.path(tmp,'web_config.R'))
  shiny::testServer(e$make_ami_server(tmp,api),{
    session$setInputs(endpoint='mortality30',predict_manual=0);do.call(session$setInputs,values)
    session$flushReact();session$setInputs(predict_manual=1)
    stopifnot(is.null(result()),grepl('Manual input is disabled',message(),fixed=TRUE))
    checks<<-c(checks,'restricted_mode_blocks_manual_requests_on_server')
  })
  d<-e$ami_dictionary(root)
  if(nrow(d)){
    html<-as.character(e$ami_manual_ui(root))
    stopifnot(all(vapply(e$ami_field_id(d$field),function(id)grepl(paste0('id="',id,'"'),html,fixed=TRUE),logical(1))))
    stopifnot(!grepl('Step[0-9]',html))
    checks<-c(checks,'manual_form_contains_every_defined_field')
  }
  data.frame(check=checks,status='PASS',stringsAsFactors=FALSE)
}
