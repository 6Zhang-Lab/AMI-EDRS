# Presentation and manual-entry helpers. The frozen predictor is unchanged.
ami_variable_labels <- c(heart_rate='Heart rate',sbp='Systolic blood pressure',map='Mean arterial pressure',
  resp_rate='Respiratory rate',spo2='Oxygen saturation',temperature='Temperature',gcs_eye='GCS: eye response',
  gcs_verbal='GCS: verbal response',gcs_motor='GCS: motor response',creatinine='Creatinine',bun='Blood urea nitrogen',
  sodium='Sodium',potassium='Potassium',chloride='Chloride',bicarbonate='Bicarbonate',glucose='Glucose',
  wbc='White blood cells',hemoglobin='Hemoglobin',platelet='Platelets',inr='INR',ptt='Partial thromboplastin time')
ami_stat_labels <- c(mean_value='Mean',sd_value='SD',slope_per_hour='Slope / h',abnormal_fraction='Abnormal fraction',
  min_value='Minimum',max_value='Maximum')
ami_variable_label <- function(x) {v<-unname(ami_variable_labels[x]);v[is.na(v)]<-gsub('_',' ',x[is.na(v)],fixed=TRUE);v}
ami_field_id <- function(x) paste0('manual__',x)
ami_dictionary <- function(root) {
  f<-file.path(root,'feature_dictionary.csv')
  if(!file.exists(f))return(data.frame())
  utils::read.csv(f,stringsAsFactors=FALSE,check.names=FALSE)
}
ami_source_label <- function(x) {
  if(grepl('SYNTHETIC',x,fixed=TRUE))'Synthetic example' else if(identical(x,'Manual window summaries'))'Single-patient entry' else 'CSV import'
}
ami_public_error <- function(e,root) {
  s<-conditionMessage(e);d<-ami_dictionary(root)
  if(nrow(d))for(i in seq_len(nrow(d)))if(grepl(d$field[i],s,fixed=TRUE))s<-gsub(d$field[i],
    paste(ami_variable_label(d$variable[i]),d$window[i],unname(ami_stat_labels[d$statistic[i]])),s,fixed=TRUE)
  s<-gsub('Step[0-9]+([/-][0-9]+)?','input specification',s)
  s
}
ami_manual_frame <- function(values,o) {
  scalar <- function(id,required=FALSE) {
    v<-values[[id]]
    if(length(v)>1L)stop('Each input must contain one value.',call.=FALSE)
    if(is.null(v)||!length(v)||is.na(v)||!nzchar(trimws(as.character(v)))){
      if(required)stop('Complete age, sex and location at 48 hours.',call.=FALSE)
      return(NA_character_)
    }
    trimws(as.character(v))
  }
  if(!isTRUE(values$manual_eligible))stop('Confirm AMI admission and survival to 48 hours.',call.=FALSE)
  if(!isTRUE(values$manual_confirm))stop('Confirm the time-window summaries and missing values.',call.=FALSE)
  x<-as.data.frame(stats::setNames(rep(list(NA_character_),length(o$contract$columns)),o$contract$columns),check.names=FALSE)
  x$contract_id<-o$contract$id;x$landmark_hours<-'48';x$icu_admitted_AMI<-'1';x$alive_at_T48<-'1'
  x$age<-scalar('manual_age',TRUE);x$sex_male<-scalar('manual_sex',TRUE)
  x$in_index_icu<-scalar('manual_icu',TRUE);x$in_index_hospital_at_T48<-scalar('manual_hospital',TRUE)
  for(nm in o$contract$raw_columns)x[[nm]]<-scalar(ami_field_id(nm))
  x[o$contract$columns]
}
ami_display_results <- function(z,origin) {
  data.frame(Record=z$row_number,
    Outcome=ifelse(z$endpoint=='in_hospital_death_after_T48','In-hospital mortality after 48 hours','30-day mortality after 48 hours'),
    `Risk (%)`=sprintf('%.2f',100*z$risk_probability),
    `Input source`=ami_source_label(origin),
    `Imputed model inputs`=z$imputed_input_count,
    `Available domain scores`=paste0(z$score_domains_observed,'/',z$score_domains_total),check.names=FALSE)
}
ami_write_display_csv <- function(z,file) {
  # Readable report only: this CSV is never used as a prediction input.
  utils::write.csv(z,file,row.names=FALSE,na='',fileEncoding='UTF-8')
}
ami_manual_ui <- function(root) {
  d<-ami_dictionary(root)
  if(!nrow(d))return(shiny::p('Input definitions are unavailable.'))
  units<-c(per_min='breaths/min',degC='°C',`10^9/L`='×10⁹/L')
  variable_ui<-function(v){
    dd<-d[d$variable==v,,drop=FALSE]
    st<-names(ami_stat_labels)[names(ami_stat_labels)%in%dd$statistic]
    baseunit<-dd$unit[match('mean_value',dd$statistic)]
    if(is.na(baseunit))baseunit<-dd$unit[1]
    if(baseunit%in%names(units))baseunit<-unname(units[baseunit])
    shiny::tags$details(class='measurement-group',
      shiny::tags$summary(shiny::span(ami_variable_label(v)),shiny::span(class='unit',baseunit)),
      shiny::div(class='summary-scroll',shiny::tags$table(class='summary-grid',
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th('Window'),lapply(st,function(s)shiny::tags$th(ami_stat_labels[[s]])))),
        shiny::tags$tbody(lapply(c('0-6h','0-24h','0-48h'),function(w){
          shiny::tags$tr(shiny::tags$th(gsub('h',' h',w,fixed=TRUE)),lapply(st,function(s){
            row<-dd[dd$window==w&dd$statistic==s,,drop=FALSE]
            if(!nrow(row))return(shiny::tags$td('—'))
            id<-ami_field_id(row$field[1]);label<-paste(ami_variable_label(v),w,ami_stat_labels[[s]])
            shiny::tags$td(shiny::tags$label(class='sr-only',`for`=id,label),
              shiny::tags$input(id=id,type='text',class='form-control manual-value',value='',placeholder='—',
                inputmode='decimal',autocomplete='off',`aria-label`=label))
          }))
        })))))
  }
  groups<-list('Vital signs'=c('heart_rate','sbp','map','resp_rate','spo2','temperature'),
    'Neurology'=c('gcs_eye','gcs_verbal','gcs_motor'),
    'Kidney & electrolytes'=c('creatinine','bun','sodium','potassium','chloride','bicarbonate'),
    'Blood & metabolism'=c('glucose','wbc','hemoglobin','platelet','inr','ptt'))
  other<-setdiff(unique(d$variable),unlist(groups));if(length(other))groups[['Other']]<-other
  tabs<-lapply(names(groups),function(g)shiny::tabPanel(g,value=g,
    lapply(intersect(groups[[g]],unique(d$variable)),variable_ui)))
  shiny::tagList(
    shiny::div(class='section-heading',shiny::h3('Patient profile'),shiny::span(class='muted','At 48 hours after ICU admission')),
    shiny::div(class='profile-grid',
      shiny::textInput('manual_age','Age (years)',value='',placeholder='e.g. 65'),
      shiny::selectInput('manual_sex','Sex',choices=c('Select…'='','Female'='0','Male'='1'),selectize=FALSE),
      shiny::selectInput('manual_hospital','Still in the index hospital?',choices=c('Select…'='','Yes'='1','No'='0'),selectize=FALSE),
      shiny::selectInput('manual_icu','Still in the index ICU?',choices=c('Select…'='','Yes'='1','No'='0'),selectize=FALSE)),
    shiny::checkboxInput('manual_eligible','AMI admission; alive at 48 hours after ICU admission.',FALSE),
    shiny::div(class='section-heading',shiny::h3('Time-window summaries'),shiny::span(class='muted','0–6 h · 0–24 h · 0–48 h')),
    shiny::p(class='input-note','Enter the available summaries for each cumulative window. Leave unavailable values blank.'),
    shiny::tags$details(class='field-guide',shiny::tags$summary('What should I enter?'),
      shiny::p('These are summaries of measurements collected within each window, rather than individual readings at 6, 24 or 48 hours. SD is the sample standard deviation; slope is change per hour. Abnormal fraction ranges from 0 to 1.'),
      shiny::downloadLink('download_dictionary','View input definitions')),
    do.call(shiny::tabsetPanel,c(tabs,list(id='measure_group',type='tabs'))),
    shiny::div(class='manual-actions',
      shiny::checkboxInput('manual_confirm','All available window summaries are entered; blank fields are unavailable.',FALSE),
      shiny::actionButton('predict_manual','Calculate risk',class='btn-primary'),
      shiny::span(class='input-note','Missing model inputs use the reference imputation values.')))
}
