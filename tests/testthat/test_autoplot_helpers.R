
context("autoplot_helpers")

#generate test data
source("generate_testdata_object.R")


test_that("cut_to_same_dates returns correct output",{

  # to different time lengths
  test_object_point <- make_object(tmin=data_point, tmax=data_point,prec=data_point,tavg=data_point,dates_tmax=time,dates_tmin=time,dates_prec = time,dates_tavg = time,lon=lon,lat=lat,data_info=point_data_info)
  test_object_grid2 <- make_object(tmin=data_grid2, tavg=data_grid2,dates_tmin=time2,dates_tavg = time2,lon=lon,lat=lat,data_info=grid_data_info)
  test_object_grid <- make_object(tmin=data_grid,tmax=data_grid,prec=data_grid,tavg=data_grid,dates_tmin=time,dates_tmax=time,dates_prec = time,dates_tavg = time,lon=lon,lat=lat,data_info=grid_data_info)

  dat <- list(dat_p= test_object_point, dat_grid=test_object_grid2)

  cdat <- cut_to_same_dates(dat,selyears=NULL)
  expect_equal(test_object_grid2$time, cdat$dat_p$time)

  dat <- list(dat_p= test_object_point, dat_grid=test_object_grid)
  cdat <- cut_to_same_dates(dat,selyears=c(1981)) # returns selected years
  expect_equal(substr(cdat$dat_p$time,1,4), rep("1981",365))

})

test_that("cut_to_same_dates_index returns correct output",{

  # to different time lengths
  test_object_point <- make_object(tmin=data_point, tmax=data_point,prec=data_point,tavg=data_point,dates_tmax=time,dates_tmin=time,dates_prec = time,dates_tavg = time,lon=lon,lat=lat,data_info=point_data_info)
  test_object_grid2 <- make_object(tmin=data_grid2, tavg=data_grid2,dates_tmin=time2,dates_tavg = time2,lon=lon,lat=lat,data_info=grid_data_info)
  test_object_grid <- make_object(tmin=data_grid,tmax=data_grid,prec=data_grid,tavg=data_grid,dates_tmin=time,dates_tmax=time,dates_prec = time,dates_tavg = time,lon=lon,lat=lat,data_info=grid_data_info)

  tnn_obs <- calc_index( test_object_point, index="tnn",aggt = "monthly")
  tnn_grid_short <- calc_index( test_object_grid2, index="tnn",aggt = "monthly",trend=FALSE)

  dat <- list(dat_p= tnn_obs, dat_grid=tnn_grid_short)
  cdat <- ClimIndVis:::cut_to_same_dates_index(dat,selyears=1981)
  expect_equal(dim(cdat$dat_p$index),c(8,12,1)) # this is only needed with selyears


})

#get_plot_args + get_plot_title not testet

#calc_index_special_autoplot
#xxx?


test_that("get_skill_data returns correct output",{

  data("object_fc_grid","object_fc_st","object_hc_grid","object_hc_st","object_st","object_grid",package="ClimIndVis")
  dat=list(hc_p=object_hc_st,fc_p=object_fc_st,hc_grid=object_hc_grid,fc_grid=object_fc_grid,obs_p=object_st,obs_grid=object_grid)
  ind_dat=lapply(dat, function(dd) do.call("calc_index",c(list(dd,index="dd"),list(aggt="monthly"))))

  veri_dat=list(p=verify_climindvis_index(ind_dat$obs_p,ind_dat$hc_p,c("EnsRocss","EnsCorr","EnsRps"),hc_ref=NULL,prob=c(1:2/3)),
    grid=verify_climindvis_index(ind_dat$obs_grid,ind_dat$hc_grid,c("EnsRocss","EnsCorr","EnsRps"),hc_ref=NULL,prob=c(1:2/3)))

  res<-ClimIndVis:::get_skill_data(veri_dat,vv=1,aa=2,cc=2,aggl=TRUE,nout=3,tdims=1)
  testthat::expect_equal(res$p,veri_dat$p$verification[[1]][[2]][,2])
  testthat::expect_equal(res$grid,veri_dat$grid$verification[[1]][[2]][,,2])


  testthat::expect_error(ClimIndVis:::get_skill_data(veri_dat,vv=3,aa=5,tt=10,cc=2,aggl=TRUE,nout=4,tdims=30))
  res<-ClimIndVis:::get_skill_data(veri_dat,vv=3,aa=5,tt=10,cc=1,aggl=TRUE,nout=1,tdims=30)
  veri_dat$p$verification$EnsRps[,,10][veri_dat$p$same]=NA
  testthat::expect_equal(res$p,veri_dat$p$verification$EnsRps[,5,10])
  veri_dat$grid$verification$EnsRps[,,,10][veri_dat$grid$same]=NA
  testthat::expect_equal(res$grid,veri_dat$grid$verification$EnsRps[,,5,10])
})
