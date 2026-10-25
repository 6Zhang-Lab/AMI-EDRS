# Artificial coefficients and observations for deterministic software testing only.
# This function is never called by the deployed app or its model loader.
make_synthetic_fixture<-function(repo) {
  root<-tempfile('ami_software_fixture_');dir.create(root)
  if(!all(file.copy(list.files(file.path(repo,'app'),full.names=TRUE),root,recursive=TRUE)))stop('Cannot copy test fixture.')
  env<-new.env(parent=baseenv());sys.source(file.path(root,'predictor.R'),env);api<-env$ami_api
  rc<-paste('probe',c('0_6h','0_24h','0_48h'),'abnormal_fraction',sep='__')
  sn<-paste('probe',c('0_6h','0_24h','0_48h'),'score',sep='__');target<-sn[3]
  rr<-list(values=c(0,1/3,2/3,1),count=c(10,40,40,10),cumulative=c(10,50,90,100),n=100,enabled=TRUE,direction='high')
  pp<-list(features=target,median=setNames(50,target),indicators=character(),center=setNames(0,target),scale=setNames(1,target),design_columns=target)
  toy<-list(version='AMI_WEB21_v1',model_id='SYNTHETIC_SOFTWARE_TEST_30DAY',endpoint='30_day_all_cause_death_after_T48',source_md5='synthetic',min_domain_fraction=1,
    state=list(reference=list(spec=data.frame(variable='probe',stat='abnormal_fraction',domain='probe',component_id='probe_fraction',direction='high'),reference=list(probe_fraction=rr)),
    cluster_preprocessor=pp,cluster=list(k=2L,centers=matrix(c(30,70),2,1,dimnames=list(NULL,target))),dynamic_columns=rc,score_columns=sn),
    model=list(preprocessor=pp,coefficients=setNames(c(-2,.02),c('(Intercept)',target))))
  tx<-data.frame(contract_id='SYNTHETIC',landmark_hours=rep(48,3),icu_admitted_AMI=1,alive_at_T48=1,in_index_hospital_at_T48=1,age=60,sex_male=0,in_index_icu=1)
  for(nm in rc)tx[[nm]]<-c(1/3,2/3,1/3)
  toy$contract<-list(id='SYNTHETIC',columns=names(tx),raw_columns=rc,dictionary=data.frame(field=rc,minimum=0,maximum=1))
  api$validate_model(toy);saveRDS(toy,file.path(root,'model_30day.rds'))
  h<-toy;h$endpoint<-'in_hospital_death_after_T48';h$model_id<-'SYNTHETIC_SOFTWARE_TEST_HOSPITAL';h$model$coefficients[1]<--2.5
  saveRDS(h,file.path(root,'model_hospital.rds'))
  manifest<-data.frame(file=c('model_30day.rds','model_hospital.rds'),md5=unname(tools::md5sum(file.path(root,c('model_30day.rds','model_hospital.rds')))),model_id=c(toy$model_id,h$model_id),endpoint=c(toy$endpoint,h$endpoint),source_model_file_md5='synthetic')
  utils::write.csv(manifest,file.path(root,'model_manifest.csv'),row.names=FALSE)
  api$write_csv(tx,file.path(root,'synthetic_example.csv'))
  writeLines('SYNTHETIC SOFTWARE FIXTURE; NOT A CLINICAL MODEL',file.path(root,'VALIDATED_LOCAL_PROTOTYPE.txt'))
  list(root=root,api=api,input=tx,expected30=plogis(-2+.02*c(30,70,30)),expected_hospital=plogis(-2.5+.02*c(30,70,30)))
}
