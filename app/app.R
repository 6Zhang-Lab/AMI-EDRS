library(shiny)
options(shiny.maxRequestSize=100*1024^2,shiny.sanitize.errors=TRUE)
app_root<-normalizePath(getwd(),winslash='/',mustWork=TRUE)
if(!file.exists(file.path(app_root,'VALIDATED_LOCAL_PROTOTYPE.txt')))
  stop('Application setup is incomplete. Please contact the administrator.')
source(file.path(app_root,'predictor.R'),local=TRUE,encoding='UTF-8')
source(file.path(app_root,'presentation.R'),local=TRUE,encoding='UTF-8')
source(file.path(app_root,'app_server.R'),local=TRUE,encoding='UTF-8')
settings<-ami_web_settings(app_root)
entry_tabs<-list()
if(settings$allow_manual)entry_tabs<-c(entry_tabs,list(tabPanel('Single patient',value='manual',ami_manual_ui(app_root))))
if(settings$allow_upload)entry_tabs<-c(entry_tabs,list(tabPanel('Batch upload',value='upload',
  div(class='tab-intro',h3('Import prepared inputs'),p('Upload one or more records with the required time-window summaries.')),
  div(class='upload-panel',fileInput('csv','Feature file',accept='.csv',buttonLabel='Choose CSV',placeholder='No file selected')),
  checkboxInput('confirm','I confirm the input definitions, units and eligibility.',FALSE),
  div(class='batch-actions',numericInput('row_index','Selected row',value=1,min=1,step=1),
    actionButton('predict_single','Calculate selected row',class='btn-secondary'),
    actionButton('predict_batch','Calculate all rows',class='btn-primary')),
  div(class='resource-links',downloadLink('download_template','Blank template'),downloadLink('download_demo','Example input file')),
  p(class='input-note','Use the exact exported feature values. The result report is a readable summary, not an input template.'))))
entry_tabs<-c(entry_tabs,list(tabPanel('Example',value='example',
  div(class='example-panel',span(class='pill','SYNTHETIC DATA'),h3('Explore three example records'),
    p('See how the calculator presents risk estimates. These records are simulated and contain no patient data.'),
    div(class='example-records',lapply(c('01','02','03'),function(i)div(class='example-record',span('Record'),strong(i)))),
    actionButton('run_demo','Calculate example risks',class='btn-primary')))))
ui<-fluidPage(
  tags$head(tags$title('AMI-EDRS | Mortality risk calculator'),
    tags$meta(name='viewport',content='width=device-width, initial-scale=1'),
    tags$link(rel='stylesheet',type='text/css',href='app.css'),tags$script(src='app.js')),
  div(class='page-shell',
    tags$header(class='topbar',div(class='brand',span(class='brand-symbol',HTML('&#43;')),span('AMI-EDRS')),
      div(class='header-links',tags$a(href='#calculator','Calculator'),tags$a(href='#about','About'),span(class='research-tag','RESEARCH USE'))),
    div(class='hero',div(class='eyebrow','ACUTE MYOCARDIAL INFARCTION'),h1('Mortality risk after 48 hours'),
      p('For adults admitted to intensive care with acute myocardial infarction.'),
      div(class='scope-line',span(class='scope-dot'),span('Assessment begins 48 hours after index ICU admission, among survivors.'))),
    div(id='calculator',class='calculator-grid',
      div(class='entry-column',
        div(class='outcome-card',div(class='section-heading',h2('Choose an outcome'),span(class='muted','Prediction horizon')),
          radioButtons('endpoint',label=NULL,choices=c('30-day mortality'='mortality30','In-hospital mortality'='hospital'),inline=TRUE),
          div(class='outcome-description',textOutput('endpoint_label'))),
        div(class='entry-card',do.call(tabsetPanel,c(entry_tabs,list(id='input_mode',type='tabs'))))),
      tags$aside(class='result-column',
        div(class='result-card',div(class='result-topline',span(class='eyebrow','RISK ESTIMATE'),actionLink('clear','Clear')),
          h2(textOutput('result_title')),div(class='result-context',textOutput('result_context')),
          div(class='result-status',textOutput('status')),uiOutput('risk_cards'),uiOutput('result_details'),uiOutput('download_controls'),
          div(class='result-footnote','Predicted probability, not a treatment recommendation.')),
        div(class='side-note',h3('About the outcomes'),p('The 30-day outcome includes deaths after discharge. The hospital outcome ends with the index admission.')))),
    div(id='about',class='about-panel',h2('About the calculator'),
      div(class='about-grid',div(h3('Population & outcomes'),
        p('Adults with ICU-admitted AMI who survive to 48 hours. The 30-day horizon starts at that landmark; the hospital outcome concerns the remaining index admission.'),
        p('In-hospital estimates require the patient to remain in the index hospital at 48 hours.')),
        div(h3('Input & interpretation'),
          p('The model uses summaries from the first 6, 24 and 48 hours. Available values are evaluated with fixed reference and preprocessing parameters.'),
          p('This research calculator has not been evaluated for routine clinical decision-making. Estimates are displayed to two decimal places.'))),
      tags$details(class='methods-details',tags$summary('Model information'),
        p('Separate models estimate 30-day and in-hospital mortality. Both were developed in MIMIC. In-hospital evaluation in eICU does not establish external validation of the 30-day outcome.'),
        p('These estimates use the original models without local recalibration. There is no automatic connection to electronic health records.'),
        if(nzchar(settings$code_url))tags$a('Study code',href=settings$code_url,target='_blank',rel='noopener noreferrer'))),
    tags$footer(class='page-footer',span('AMI-EDRS'),span('Research calculator · Post-48-hour risk'))))
shinyApp(ui=ui,server=make_ami_server(app_root,ami_api))
