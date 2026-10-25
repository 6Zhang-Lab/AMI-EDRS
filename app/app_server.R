ami_web_settings <- function(root) {
  ans <- list(allow_upload = TRUE, code_url = '')
  p <- file.path(root, 'web_config.R')
  if (file.exists(p)) {
    e <- new.env(parent = baseenv()); sys.source(p, envir=e)
    if (exists('web_config', e, inherits=FALSE)) ans <- utils::modifyList(ans, e$web_config)
  }
  if (is.null(ans$allow_manual)) ans$allow_manual <- ans$allow_upload
  if (!is.logical(ans$allow_manual) || length(ans$allow_manual)!=1L || is.na(ans$allow_manual)) stop('Invalid manual-input setting.')
  if (!is.logical(ans$allow_upload) || length(ans$allow_upload)!=1L || is.na(ans$allow_upload)) stop('Invalid upload setting.')
  if (!is.character(ans$code_url) || length(ans$code_url)!=1L || is.na(ans$code_url) ||
      (nzchar(ans$code_url) && !grepl('^https://', ans$code_url))) stop('Code URL must be empty or an HTTPS URL.')
  ans
}
ami_outcome_label <- function(endpoint) {
  switch(endpoint,
    mortality30='30-day mortality after 48 hours',
    hospital='In-hospital mortality after 48 hours',
    'Unsupported outcome')
}
make_ami_server <- function(root, api) {
  force(root);force(api); settings <- ami_web_settings(root)
  function(input, output, session) {
    result <- shiny::reactiveVal(NULL)
    message <- shiny::reactiveVal('Choose an input method to begin.')
    origin <- shiny::reactiveVal('')
    clear <- function(text='Inputs changed. Calculate risk again.') {result(NULL);origin('');message(text)}
    shiny::observeEvent(list(input$endpoint,input$csv,input$row_index,input$confirm,input$invalidate_nonce),{
      clear(if(is.null(shiny::isolate(result())))'Ready when you are.' else 'Inputs changed. Calculate risk again.')
    },ignoreInit=FALSE,priority=100)
    manual_ids <- c('manual_age','manual_sex','manual_hospital','manual_icu','manual_eligible','manual_confirm',
      ami_field_id(unique(c(ami_dictionary(root)$field,tryCatch(api$load_bundle(root,'mortality30')$contract$raw_columns,error=function(e)character())))))
    shiny::observeEvent(lapply(manual_ids,function(id)input[[id]]),{
      if(!is.null(shiny::isolate(result())))clear()
    },ignoreInit=TRUE,priority=100)
    shiny::observeEvent(input$input_mode,{
      if(!is.null(shiny::isolate(result())))clear('Input method changed. Calculate risk again.')
    },ignoreInit=TRUE,priority=100)
    execute <- function(kind) {
      clear('Calculating risk…')
      tryCatch({
        endpoint<-input$endpoint
        if(is.null(endpoint)||!endpoint%in%c('mortality30','hospital'))stop('Select an endpoint.',call.=FALSE)
        if(kind=='manual'){
          if(!settings$allow_manual)stop('Manual input is disabled in this demonstration.',call.=FALSE)
          o<-api$load_bundle(root,endpoint)
          x<-ami_manual_frame(shiny::reactiveValuesToList(input),o);confirmed<-TRUE;label<-'Manual window summaries'
        }else if(kind=='demo'){
          x<-api$read_csv(file.path(root,'synthetic_example.csv'));confirmed<-TRUE;label<-'SYNTHETIC EXAMPLE - not a real patient'
        }else{
          if(!settings$allow_upload)stop('Patient-file uploads are disabled in this public demonstration.',call.=FALSE)
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
        result(z);origin(label);message(if(nrow(z)==1L)'Calculation complete.' else paste(nrow(z),'records calculated.'))
      },error=function(e){result(NULL);origin('');message(paste('No risk output:',ami_public_error(e,root)))})
    }
    shiny::observeEvent(input$predict_manual,execute('manual'),ignoreInit=TRUE)
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
    output$result_title<-shiny::renderText({if(is.null(result()))return('Your risk estimate');ami_source_label(origin())})
    output$result_context <- shiny::renderText({z<-result();if(is.null(z))return('');paste(ami_outcome_label(if(z$endpoint[1]=='in_hospital_death_after_T48')'hospital' else 'mortality30'),'')})
    output$metadata<-shiny::renderText({z<-result();if(is.null(z))return('');paste('AMI-EDRS ·',ami_source_label(origin()))})
    output$endpoint_label<-shiny::renderText({if(identical(input$endpoint,'hospital'))'Death during the remaining index hospital stay, after 48 hours.' else 'Death within the 30 days following the 48-hour landmark.'})
    output$results<-shiny::renderTable({
      z<-result();if(is.null(z))return(NULL);z<-utils::head(z,20)
      data.frame(Row=z$row_number,`Risk (%)`=sprintf('%.2f',100*z$risk_probability),`Imputed inputs`=z$imputed_input_count,
        `Observed domain scores`=paste0(z$score_domains_observed,'/',z$score_domains_total),check.names=FALSE)
    },striped=FALSE,bordered=FALSE,spacing='m',rownames=FALSE)
    output$risk_cards <- shiny::renderUI({
      z<-result()
      if(is.null(z))return(shiny::div(class='result-empty',shiny::div(class='empty-mark',shiny::HTML('&ndash;')),
        shiny::h3('Ready to calculate'),shiny::p('Complete a single-patient entry, import a CSV, or explore the example.')))
      if(nrow(z)>3L)return(shiny::tagList(shiny::div(class='batch-count',nrow(z)),shiny::p('records calculated'),output_table()))
      shiny::tagList(lapply(seq_len(nrow(z)),function(i)shiny::div(class='risk-row',
        shiny::div(class='risk-row-heading',paste('Record',z$row_number[i])),
        shiny::div(class='risk-value',sprintf('%.2f',100*z$risk_probability[i]),shiny::span('%')),
        shiny::div(class='risk-caption','Estimated mortality risk'),
        if(z$imputed_input_count[i]>0)shiny::p(class='imputed-note',paste(z$imputed_input_count[i],'missing model inputs imputed.')))))
    })
    output_table <- function()shiny::div(class='result-scroll',shiny::tableOutput('results'))
    output$result_details<-shiny::renderUI({
      z<-result();if(is.null(z))return(NULL)
      shiny::tags$details(class='quality-details',shiny::tags$summary('Input completeness'),
        shiny::div(class='result-scroll',shiny::tags$table(class='table',
          shiny::tags$thead(shiny::tags$tr(shiny::tags$th('Record'),shiny::tags$th('Imputed inputs'),shiny::tags$th('Domain scores'))),
          shiny::tags$tbody(lapply(seq_len(min(nrow(z),20L)),function(i)shiny::tags$tr(
            shiny::tags$td(z$row_number[i]),shiny::tags$td(z$imputed_input_count[i]),
            shiny::tags$td(paste0(z$score_domains_observed[i],'/',z$score_domains_total[i]))))))),
        shiny::p('Unavailable inputs are filled using the model reference values.'))
    })
    output$download_controls<-shiny::renderUI({if(is.null(result()))return(NULL);shiny::downloadButton('download_results','Download results')})
    export_results <- function() {
      z <- shiny::isolate(result())
      if(is.null(z))stop('No valid prediction to download.',call.=FALSE)
      ep <- shiny::isolate(input$endpoint)
      current <- tryCatch(api$load_bundle(root,ep),error=function(e){clear(paste('No risk output:',conditionMessage(e)));stop(e)})
      if(!identical(current$model_id,z$model_id[1]) || !identical(current$endpoint,z$endpoint[1])){
        clear('Model or outcome changed. Run prediction again.');stop('No current result to download.',call.=FALSE)
      }
      z$result_source <- shiny::isolate(origin())
      z$synthetic_example <- grepl('SYNTHETIC',z$result_source,fixed=TRUE)
      z$risk_percent_display <- sprintf('%.2f',100*z$risk_probability)
      z
    }
    presentation_export <- function(){z<-export_results();ami_display_results(z,shiny::isolate(origin()))}
    output$download_results<-shiny::downloadHandler(filename=function()'AMI_EDRS_results.csv',content=function(file){
      ami_write_display_csv(presentation_export(),file)
    })
    output$download_template<-shiny::downloadHandler(filename=function()'AMI_EDRS_feature_template.csv',content=function(file)file.copy(file.path(root,'input_template.csv'),file,overwrite=TRUE))
    output$download_dictionary<-shiny::downloadHandler(filename=function()'AMI_EDRS_input_definitions.csv',content=function(file){
      d<-ami_dictionary(root);d$definition<-gsub('Frozen upstream within-window time slope; use the Step4/6 value.','Within-window linear slope against elapsed hours; unavailable for fewer than two distinct times or a span under one hour.',d$definition,fixed=TRUE)
      utils::write.csv(d,file,row.names=FALSE,na='',fileEncoding='UTF-8')
    })
    output$download_demo<-shiny::downloadHandler(filename=function()'AMI_EDRS_synthetic_example.csv',content=function(file)file.copy(file.path(root,'synthetic_example.csv'),file,overwrite=TRUE))
  }
}

