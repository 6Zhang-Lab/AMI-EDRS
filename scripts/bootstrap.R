# Locate the repository from Rscript or source(), without changing user paths.
ami_find_root <- function() {
  fs <- unlist(lapply(sys.frames(), function(f) if (!is.null(f$ofile)) as.character(f$ofile) else NULL), use.names=FALSE)
  args <- grep('^--file=', commandArgs(), value=TRUE)
  candidates <- c(if(length(fs)) dirname(fs), if(length(args)) dirname(sub('^--file=','',args)), getwd())
  for (d in candidates) {
    d <- normalizePath(d, winslash='/', mustWork=FALSE)
    for (i in 1:4) {
      if(file.exists(file.path(d,'README.md')) && dir.exists(file.path(d,'analysis')) && dir.exists(file.path(d,'app'))) return(d)
      d <- dirname(d)
    }
  }
  stop('Run this script from the AMI-EDRS repository.',call.=FALSE)
}
ami_root <- ami_find_root()
ami_need <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs,requireNamespace,logical(1),quietly=TRUE)]
  if(length(missing)) stop('Missing R packages: ',paste(missing,collapse=', '),'. Run scripts/install_dependencies.R first.',call.=FALSE)
}
ami_read_config <- function(path) {
  if(!file.exists(path)) stop('Copy config/local.example.R to config/local.R and edit the paths first.',call.=FALSE)
  e <- new.env(parent=globalenv());source(path,local=e,encoding='UTF-8')
  if(!exists('ami_config',e,inherits=FALSE)||!is.list(e$ami_config)) stop('Configuration must define ami_config as a list.')
  cfg <- e$ami_config
  req <- c('MIMIC_PROJECT','EICU_PROJECT','MIMIC_RAW','EICU_RAW','OUTPUT_PARENT')
  if(!all(req%in%names(cfg))) stop('Configuration lacks: ',paste(setdiff(req,names(cfg)),collapse=', '))
  for(nm in req) {
    if(!is.character(cfg[[nm]])||length(cfg[[nm]])!=1||!nzchar(cfg[[nm]]))stop('Invalid path: ',nm)
    cfg[[nm]] <- normalizePath(path.expand(cfg[[nm]]),winslash='/',mustWork=FALSE)
  }
  ev <- setNames(cfg[req],paste0('AMI_',req));do.call(Sys.setenv,ev)
  for(i in 0:21) {
    v <- list(MIMIC_PROJECT=cfg$MIMIC_PROJECT,EICU_PROJECT=cfg$EICU_PROJECT,
      OUTPUT_PARENT=cfg$OUTPUT_PARENT,MIMIC_RAW=cfg$MIMIC_RAW,EICU_RAW=cfg$EICU_RAW,
      RAW_MIMIC_DIR=cfg$MIMIC_RAW,RAW_EICU_DIR=cfg$EICU_RAW,ALLOW_FILE_CHOOSE=FALSE)
    if(i==19) {
      v$LEGACY_SPLIT_FILE <- if(!is.null(cfg$LEGACY_SPLIT_FILE))cfg$LEGACY_SPLIT_FILE else file.path(cfg$MIMIC_PROJECT,'09_step05_T48_dynamic_prediction_model_with_C1C5/analysis_datasets/Step05_train_test_split.csv')
      # The recorded split hash is intentionally not relaxed here.
    }
    override <- cfg$step_options[[as.character(i)]]
    if(!is.null(override))v<-utils::modifyList(v,override)
    options(setNames(list(v),paste0('ami_step',i,'.config')))
  }
  cfg
}
ami_select_steps <- function(spec,valid) {
  if(identical(spec,'all'))return(valid)
  parts <- strsplit(spec,',',fixed=TRUE)[[1]];ans<-integer()
  for(z in parts) {
    if(!grepl('^[0-9]+(:[0-9]+)?$',z))stop('Use all, 1:21, or a comma-separated list of step numbers.')
    k<-as.integer(strsplit(z,':',fixed=TRUE)[[1]])
    ans<-c(ans,if(length(k)==2L)seq.int(k[1],k[2])else k)
  }
  if(!all(ans%in%valid))stop('Unknown step number.')
  unique(ans)
}
