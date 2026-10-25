source(file.path('scripts','bootstrap.R'),encoding='UTF-8')
source(file.path(ami_root,'tests/synthetic_fixture.R'))
local({
  t0<-proc.time()[['elapsed']]
  fx<-make_synthetic_fixture(ami_root);on.exit(unlink(fx$root,recursive=TRUE))
  a<-fx$api$request(fx$root,'mortality30',fx$input,TRUE)
  b<-fx$api$request(fx$root,'hospital',fx$input,TRUE)
  stopifnot(max(abs(a$risk_probability-fx$expected30))<1e-12,max(abs(b$risk_probability-fx$expected_hospital))<1e-12)
  out<-file.path(ami_root,'work/demo');dir.create(out,recursive=TRUE,showWarnings=FALSE)
  z<-data.frame(record=1:3,software_test_only=TRUE,p30=a$risk_probability,p_hospital=b$risk_probability)
  utils::write.csv(z,file.path(out,'synthetic_software_demo.csv'),row.names=FALSE)
  print(z);cat('PASS: artificial coefficients and observations only; not AMI risk estimates.\n')
  cat('Elapsed seconds:',proc.time()[['elapsed']]-t0,'\n')
})
