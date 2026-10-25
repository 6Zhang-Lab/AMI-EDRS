source(file.path('scripts','bootstrap.R'),encoding='UTF-8')
args<-commandArgs(trailingOnly=TRUE)
cfg<-ami_read_config(if(length(args))args[1]else file.path(ami_root,'config/local.R'))
requested<-ami_select_steps(if(length(args)>1)args[2]else '1:21',0:21)
order<-utils::read.csv(file.path(ami_root,'analysis/run_order.csv'),stringsAsFactors=FALSE)
stage<-as.integer(sub('^Step([0-9]+).*','\\1',order$file))
order<-order[stage%in%requested,,drop=FALSE]
for(f in order$file) {
  message('Running ',f)
  result<-source(file.path(ami_root,'analysis',f),local=new.env(parent=globalenv()),encoding='UTF-8')$value
  n<-as.integer(sub('^Step([0-9]+).*','\\1',f))
  if(is.list(result)&&identical(result$status,'FAILED'))stop('Analysis failed in ',f,'. Inspect the local error report.')
  if(n>0) {
    status<-file.path(cfg$OUTPUT_PARENT,paste0('Step',n),'to_share',paste0('Step',n,'_STATUS.txt'))
    if(!file.exists(status)||!any(grepl('^STATUS = COMPLETED',readLines(status,warn=FALSE))))stop('No successful completion status for ',f,'. Subsequent stages were not run.')
  }
}
cat('Requested stages completed. Review their aggregate outputs before interpreting results.\n')
