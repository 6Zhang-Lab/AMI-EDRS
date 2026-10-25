args<-commandArgs(trailingOnly=TRUE)
source(file.path('scripts','bootstrap.R'),encoding='UTF-8')
profile<-if(length(args))args[1]else 'web'
if(!profile%in%c('web','analysis','legacy','deploy','all'))stop('Profile must be web, analysis, legacy, deploy, or all.')
d<-utils::read.csv(file.path(ami_root,'dependencies.csv'),stringsAsFactors=FALSE)
want<-if(profile=='all')rep(TRUE,nrow(d))else vapply(strsplit(d$profiles,';',fixed=TRUE),function(x)profile%in%x,logical(1))
pkgs<-d$package[want]
missing<-pkgs[!vapply(pkgs,requireNamespace,logical(1),quietly=TRUE)]
options(repos=c(CRAN='https://cloud.r-project.org'))
if(length(missing))utils::install.packages(missing)
ami_need(pkgs)
cat('Dependencies available for profile:',profile,'\n')
cat('Recorded analysis versions are in dependencies.csv and provenance/environments.\n')
