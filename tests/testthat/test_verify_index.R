context("verify_climindvis_index")


testthat::test_that("verify_climindvis_index works",{


  data("object_fc_grid","object_fc_st","object_hc_grid","object_hc_st","object_st","object_grid",package="ClimIndVis")
  dat=list(hc_p=object_hc_st,fc_p=object_fc_st,obs_p=object_st)
  ind_dat=lapply(dat, function(dd) do.call("calc_index",c(list(dd,index="dd"),list(aggt="monthly"))))
  veri_dat=list(p=verify_climindvis_index(ind_dat$obs_p,ind_dat$hc_p,c("EnsRocss","EnsCorr","EnsRps"),hc_ref=NULL))

  EnsCorr=easyVerification::veriApply("EnsCorr",fcst=ind_dat$hc_p$index,obs=ind_dat$obs_p$index[,1:7,],tdim=4,ensdim=2,na.rm=TRUE)
  testthat::expect_equal(array(EnsCorr),array(veri_dat$p$verification$EnsCorr))
  EnsRps=easyVerification::veriApply("EnsRps",fcst=ind_dat$hc_p$index,obs=ind_dat$obs_p$index[,1:7,],tdim=4,ensdim=2,na.rm=TRUE,prob=c(1:2/3))
  testthat::expect_equal(array(EnsRps),array(veri_dat$p$verification$EnsRps))
  EnsRocss=easyVerification::veriApply("EnsRocss",fcst=ind_dat$hc_p$index,obs=ind_dat$obs_p$index[,1:7,],tdim=4,ensdim=2,na.rm=TRUE,prob=c(1:2/3))
  testthat::expect_equal(array(EnsRocss$cat2),array(veri_dat$p$verification$EnsRocss$cat2))

  dat=list(hc_grid=object_hc_grid,fc_grid=object_fc_grid,obs_grid=object_grid)
  ind_dat=lapply(dat, function(dd) do.call("calc_index",c(list(dd,index="dd"),list(aggt="dates",start_days="0000-02-18",end_days="0000-04-19"))))
  veri_dat=list(grid=verify_climindvis_index(ind_dat$obs_grid,ind_dat$hc_grid,c("FairRpss","EnsRmse","Ens2AFC"),hc_ref=NULL))

  EnsRmse=easyVerification::veriApply("EnsRmse",fcst=ind_dat$hc_grid$index,obs=ind_dat$obs_grid$index,tdim=4,ensdim=3,na.rm=TRUE)
  testthat::expect_equal(array(EnsRmse),array(veri_dat$grid$verification$EnsRmse))
  Ens2AFC=easyVerification::veriApply("Ens2AFC",fcst=ind_dat$hc_grid$index,obs=ind_dat$obs_grid$index,tdim=4,ensdim=3,na.rm=TRUE)
  testthat::expect_equal(array(Ens2AFC),array(veri_dat$grid$verification$Ens2AFC))
  FairRpss=easyVerification::veriApply("FairRpss",fcst=ind_dat$hc_grid$index,obs=ind_dat$obs_grid$index,tdim=4,ensdim=3,na.rm=TRUE,prob=c(1:2/3))
  testthat::expect_equal(array(FairRpss$skillscore),array(veri_dat$grid$verification$FairRpss))

})



testthat::test_that("verify_index works",{

  dat=list(hc_grid=object_hc_grid,obs_grid=object_grid)
  dat<-cut_to_same_dates(dat)
  ind_dat=lapply(dat, function(dd) do.call("calc_index",c(list(dd,index="dd"),list(aggt="monthly"))))
  res1<-ClimIndVis:::verify_climindvis_index(ind_dat$obs_grid,ind_dat$hc_grid,c("FairRpss","EnsRmse","Ens2AFC"),hc_ref=NULL, prob=c(1:2/3))
  res2<-verify_index(obs=object_grid, hc = object_hc_grid , index = "dd" ,index_args = list(aggt="monthly"), veri_metrics = c("FairRpss","EnsRmse","Ens2AFC"), veri_args=list(prob=c(1:2/3)))

  testthat::expect_equal(res1$verification$Ens2AFC,res2$verification$Ens2AFC)
})





