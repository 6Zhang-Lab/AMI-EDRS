ami_kit_dir <- function() {
  fs <- unlist(lapply(sys.frames(), function(f) if(!is.null(f$ofile)) as.character(f$ofile) else NULL),use.names=FALSE)
  if(length(fs)) return(dirname(normalizePath(tail(fs,1),winslash='/',mustWork=TRUE)))
  a <- grep('^--file=',commandArgs(),value=TRUE)
  if(length(a)) return(dirname(normalizePath(sub('^--file=','',a[1]),winslash='/',mustWork=TRUE)))
  getwd()
}
ami_release_config <- function() {
  x <- utils::modifyList(list(
    STEP21_DIR=file.path(Sys.getenv("AMI_OUTPUT_PARENT", unset=file.path(getwd(), "work/results")), "Step21"),
    OUTPUT_DIR=NULL, LAUNCH=TRUE, INSTALL_PACKAGES=TRUE,
    ACCOUNT=NULL, SERVER=NULL, APP_ID=NULL, APP_NAME='ami-edrs-research', CODE_URL='', PUBLIC_DEMO_ONLY=FALSE
  ),getOption('ami_web.release',list()))
  x$STEP21_DIR <- normalizePath(path.expand(x$STEP21_DIR),winslash='/',mustWork=FALSE)
  if(is.null(x$OUTPUT_DIR)) x$OUTPUT_DIR <- file.path(dirname(x$STEP21_DIR),'Step21_Web_Run')
  x$OUTPUT_DIR <- normalizePath(path.expand(x$OUTPUT_DIR),winslash='/',mustWork=FALSE)
  if(!is.logical(x$PUBLIC_DEMO_ONLY)||length(x$PUBLIC_DEMO_ONLY)!=1L||is.na(x$PUBLIC_DEMO_ONLY))stop('PUBLIC_DEMO_ONLY必须为TRUE或FALSE。')
  x
}
ami_packages <- function(packages,install=TRUE) {
  missing <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
  if(length(missing) && install) utils::install.packages(missing,repos='https://cloud.r-project.org')
  missing <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
  if(length(missing)) stop('缺少R包：',paste(missing,collapse=', '),call.=FALSE)
}
ami_copy_files <- function(src,dst,files) {
  absent <- files[!file.exists(file.path(src,files))]
  if(length(absent))stop('缺少文件：',paste(absent,collapse=', '),'\n目录：',src,call.=FALSE)
  dir.create(dst,recursive=TRUE,showWarnings=FALSE)
  for(f in files){
    target<-file.path(dst,f);dir.create(dirname(target),recursive=TRUE,showWarnings=FALSE)
    if(!file.copy(file.path(src,f),target,overwrite=TRUE,copy.mode=FALSE))stop('复制失败：',f)
  }
}
ami_check_release <- function(root,kit) {
  expected <- utils::read.csv(file.path(kit,'expected_model_manifest.csv'),stringsAsFactors=FALSE)
  expected_predictor <- trimws(readLines(file.path(kit,'expected_predictor_md5.txt'),warn=FALSE)[1])
  actual_predictor <- unname(tools::md5sum(file.path(root,'predictor.R')))
  if(!identical(actual_predictor,expected_predictor))stop('predictor.R与已核验Step21版本不一致；停止发布。')
  for(i in seq_len(nrow(expected))){
    f<-file.path(root,expected$file[i])
    if(!file.exists(f)||!identical(unname(tools::md5sum(f)),expected$md5[i]))stop('模型文件与本次Step21反馈不一致：',expected$file[i])
  }
  env<-new.env(parent=baseenv());sys.source(file.path(root,'predictor.R'),env)
  for(ep in c('mortality30','hospital'))env$ami_api$load_bundle(root,ep)
  invisible(TRUE)
}
ami_app_files <- function() c('app.R','app_server.R','presentation.R','predictor.R','web_config.R','www/app.css','www/app.js',
  'model_30day.rds','model_hospital.rds','model_manifest.csv',
  'input_template.csv','synthetic_example.csv','feature_dictionary.csv',
  'component_units_and_bounds.csv','VALIDATED_LOCAL_PROTOTYPE.txt')

# server.R is a reserved Shiny entry point, not a safe name for an app.R helper.
ami_migrate_entrypoint <- function(root) {
  old <- list.files(root,full.names=TRUE)
  old <- old[tolower(basename(old))=='server.r']
  if(length(old)>1L)stop('发现多个server.R文件；停止自动修复。')
  target<-file.path(root,'app_server.R')
  if(length(old)){
    txt<-readLines(old,warn=FALSE,encoding='UTF-8')
    if(!any(grepl('make_ami_server',txt,fixed=TRUE)))stop('server.R不是本程序的辅助模块，未修改。')
    if(!file.exists(target)&&!file.copy(old,target,overwrite=FALSE))stop('不能创建app_server.R。')
  }
  if(!file.exists(target))stop('缺少app_server.R。')
  for(nm in c('app.R','tests_server.R','tests_release.R')){
    f<-file.path(root,nm);if(!file.exists(f))next
    before<-readLines(f,warn=FALSE,encoding='UTF-8')
    after<-gsub("'server.R'","'app_server.R'",before,fixed=TRUE)
    after<-gsub('"server.R"','"app_server.R"',after,fixed=TRUE)
    if(!identical(before,after)){
      bak<-paste0(f,'.before_entry_fix')
      if(!file.exists(bak)&&!file.copy(f,bak))stop('不能备份：',nm)
      writeLines(enc2utf8(after),f,useBytes=TRUE)
    }
  }
  if(length(old)){
    bak<-file.path(root,'server.R.before_entry_fix')
    if(file.exists(bak)){
      if(!identical(unname(tools::md5sum(old)),unname(tools::md5sum(bak))))stop('server.R备份与当前文件不同；未覆盖备份。')
      if(unlink(old)!=0L)stop('不能移除已备份的旧入口。')
    }else if(!file.rename(old,bak))stop('不能重命名旧server.R。')
  }
  invisible(TRUE)
}

ami_check_web_entry <- function(root) {
  root<-normalizePath(root,winslash='/',mustWork=TRUE)
  if(any(tolower(list.files(root))=='server.r'))stop('根目录存在server.R，会遮蔽app.R；请先修复入口。')
  app<-shiny::shinyAppDir(root)
  oldwd<-getwd()
  on.exit(setwd(oldwd),add=TRUE)
  app$onStart()
  on.exit({try(app$onStop(),silent=TRUE);setwd(oldwd)},add=FALSE)
  reply<-app$httpHandler(list(PATH_INFO='/',REQUEST_METHOD='GET',QUERY_STRING='',
    SCRIPT_NAME='',rook.url_scheme='http',HTTP_HOST='127.0.0.1'))
  html<-if(is.raw(reply$content))rawToChar(reply$content)else paste(reply$content,collapse='')
  if(!identical(as.integer(reply$status),200L)||grepl('No UI defined',html,fixed=TRUE)||
     !grepl('AMI-EDRS',html,fixed=TRUE)||!grepl('run_demo',html,fixed=TRUE))stop('网页首页加载失败。')
  if(!is.function(app$serverFuncSource()))stop('网页服务器入口无效。')
  data.frame(check='directory_entry_serves_AMI_UI_and_server',status='PASS')
}
