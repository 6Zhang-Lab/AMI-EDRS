source(file.path('scripts','bootstrap.R'),encoding='UTF-8')
args<-commandArgs(trailingOnly=TRUE)
cfg<-ami_read_config(if(length(args))args[1]else file.path(ami_root,'config/local.R'))
order<-utils::read.csv(file.path(ami_root,'upstream/run_order.csv'),stringsAsFactors=FALSE)
requested<-ami_select_steps(if(length(args)>1)args[2]else 'all',order$order)
for(f in order$file[order$order%in%requested]) {
  message('Running prerequisite: ',f)
  # Isolate legacy rm(list=ls()) from the caller's R environment.
  source(file.path(ami_root,'upstream',f),local=new.env(parent=globalenv()),encoding='UTF-8')
}
cat('Prerequisite extraction finished. Revised endpoints and feature definitions are applied by analysis/ stages.\n')
