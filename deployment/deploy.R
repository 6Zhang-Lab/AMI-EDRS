# Explicit deployment command; never sourced by installation or testing.
source(file.path('scripts','bootstrap.R'),encoding='UTF-8')
ami_need(c('shiny','testthat','rsconnect'))
args<-commandArgs(trailingOnly=TRUE)
if(length(args)<3L)stop('Usage: Rscript deployment/deploy.R ACCOUNT SERVER APP_NAME [APP_ID]')
source(file.path(ami_root,'deployment/release_helpers.R'))
root<-file.path(ami_root,'work/webapp')
if(!file.exists(file.path(root,'VALIDATED_LOCAL_PROTOTYPE.txt')))stop('Prepare the actual frozen-model application first.')
ami_check_release(root,file.path(ami_root,'deployment'));ami_check_web_entry(root)
acc<-rsconnect::accounts();acc<-acc[acc$name==args[1]&acc$server==args[2],,drop=FALSE]
if(nrow(acc)!=1L)stop('Connect the specified hosting account locally before deployment.')
par<-list(appDir=root,appFiles=ami_app_files(),appPrimaryDoc='app.R',appMode='shiny',account=args[1],server=args[2],appName=args[3],appTitle='AMI-EDRS | Post-48-hour risk',launch.browser=TRUE)
if(length(args)>3L)par$appId<-args[4]
do.call(rsconnect::deployApp,par)
