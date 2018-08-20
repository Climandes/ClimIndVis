context("forecast_helpers")

#test get_cats
testthat::test_that("category calculation works for terciles",{

  dat=replicate(2,array(rep(1:15,each=3),dim=c(3,15)))
  cd=1
  thresholds=array(c(5.5,10.5,10.5,14.5,0,20),dim=c(2,3))

  res<-array(NA,dim=c(3,2,3))
  res[1,,]<-array(rep(c(5,5,5),each=2),dim=c(2,3))
  res[2,,]<-array(rep(c(10,4,1),each=2),dim=c(2,3))
  res[3,,]<-array(rep(c(0,15,0),each=2),dim=c(2,3))

  testthat::expect_equal(ClimIndVis:::get_cats(dat,cd,thresholds,ydim=3,prob=FALSE),res)})

testthat::test_that("tercile calculation works for multiple categories",{

  dat=t(replicate(2,array(1:15)))
  cd=1
  thresholds=replicate(2,c(1.5,5.5,10.5,14.5))
  res<-array(rep(c(1,4,5,4,1),each=2),dim=c(2,5))

  testthat::expect_equal(ClimIndVis:::get_cats(dat,cd,thresholds,ydim=0,prob=FALSE),res)})


testthat::test_that("select maximum category and value",{
  v1=c(10,50,40,20)
  v2=c(40,40,20)
  v3=c(NA,NA,NA)
  v4=c(0,0,0)
  testthat::expect_equal(2,ClimIndVis:::sel_maxcat(v1))
  testthat::expect_equal(50,ClimIndVis:::sel_maxcat_value(v1))
  testthat::expect_equal(-999,ClimIndVis:::sel_maxcat(v2))
  testthat::expect_equal(-999,ClimIndVis:::sel_maxcat_value(v2))
  testthat::expect_equal(NA,ClimIndVis:::sel_maxcat(v3))
  testthat::expect_equal(NA,ClimIndVis:::sel_maxcat_value(v3))
  testthat::expect_equal(0,ClimIndVis:::sel_maxcat(v4))
  testthat::expect_equal(NA,ClimIndVis:::sel_maxcat_value(v3))
})



#test get_cats_fc----------------
data("object_fc_grid","object_fc_st","object_hc_grid","object_hc_st","object_st","object_grid",package="ClimIndVis")
testthat::test_that("calculate forecast categories",{

  dat=list(hc_p=object_hc_st,fc_p=object_fc_st,hc_grid=object_hc_grid,fc_grid=object_fc_grid)
  ind_dat=lapply(dat, function(dd) do.call("calc_index",c(list(dd,index="dd"),list(aggt="seasonal"))))

  qth=apply(ind_dat$hc_p$index,c(1,3),quantile,probs=c(1:2/3), type=8)
  res=ClimIndVis:::get_cats(ind_dat$fc_p$index,cd=c(1,3),qth,ydim=4,prob=TRUE)
  testthat::expect_equal(ClimIndVis:::get_cats_fc(ind_dat,grid=0,points=1,probs=c(1:2/3),prob=TRUE)$p$val,res)

  qth=apply(ind_dat$hc_grid$index,c(1,2,4),quantile,probs=c(1:2/3),na.rm = TRUE , type=8)
  res=ClimIndVis:::get_cats(ind_dat$fc_grid$index,cd=c(1,2,4),qth,ydim=5,prob=TRUE)
  testthat::expect_equal(ClimIndVis:::get_cats_fc(ind_dat,grid=1,points=0,probs=c(1:2/3),prob=TRUE)$grid$val,res)})



#get_skill_maxcat-------------

testthat::test_that("calculate forecast categories for station data",{
  dat=list(hc_p=object_hc_st,fc_p=object_fc_st,hc_grid=object_hc_grid,fc_grid=object_fc_grid,obs_p=object_st,obs_grid=object_grid)
  ind_dat=lapply(dat, function(dd) do.call("calc_index",c(list(dd,index="dd"),list(aggt="seasonal"))))
  veri_dat=list(p=verify_climindvis_index(ind_dat$obs_p,ind_dat$hc_p,"EnsRocss",hc_ref=NULL),
    grid=verify_climindvis_index(ind_dat$obs_grid,ind_dat$hc_grid,"EnsRocss",hc_ref=NULL))
  maxcat=list(p=array(rep(2,4),dim=c(4,1,1)),grid=array(rep(2,12),dim=c(3,4,1,1)))
  testthat::expect_equal(array(ClimIndVis:::get_skill_maxcat(veri_dat,maxcat,"EnsRocss")$p),array(veri_dat$p$verification$EnsRocss$cat2))
  testthat::expect_equal(array(ClimIndVis:::get_skill_maxcat(veri_dat,maxcat,"EnsRocss")$grid),array(veri_dat$grid$verification$EnsRocss$cat2))
  })

