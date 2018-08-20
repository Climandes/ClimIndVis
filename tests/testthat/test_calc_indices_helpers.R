
context("calc_indices_helpers")
source("generate_testdata_object.R")

tp <- make_object(prec=data_point,dates_prec=time,lon=lon,lat=lat,data_info=point_data_info)
tp_grid <- make_object(prec=data_grid,dates_prec=time,lon=lon,lat=lat,data_info=grid_data_info)


test_that("get_date_factors return correct output",{

  sel_time<-ClimIndVis:::get_date_factors(tp,"seasonal")
  expect_is(sel_time, class="list")
  expect_equal(sel_time$aggnames, c("DJF","JJA","MAM","SON"))

  sel_time<-ClimIndVis:::get_date_factors(tp,"other",aggmons= c(1:2)) ## was brauchts hier?
  expect_is(as.numeric(substr(sel_time$tfactor,1,4)[1]), "numeric")
  expect_equal(sel_time$aggnames, "JF")

  sel_time<-ClimIndVis:::get_date_factors(tp,"dates",start_days="0000-02-15",end_days="0000-04-19") ## was brauchts hier?
  expect_equal(substr(sel_time$tfactor,1,4)[46], "1981")
  expect_equal(sel_time$aggnames, "0215-0419")

  sel_time<-ClimIndVis:::get_date_factors(tp,"dates",start_days="1982-02-15",end_days="1984-04-19")
  ref_time=seq(as.Date("1982-02-15"),as.Date("1984-04-19"),by="days")
  testthat::expect_equal(tp$time[!is.na(sel_time$tfactor)],ref_time)

  sel_time<-ClimIndVis:::get_date_factors(tp,"dates",start_days=c("1982-02-15","1984-05-01"),end_days=c("1983-04-19","1984-10-12"))
  ref_time=c(seq(as.Date("1982-02-15"),as.Date("1983-04-19"),by="days"),seq(as.Date("1984-05-01"),as.Date("1984-10-12"),by="days"))
  testthat::expect_equal(tp$time[!is.na(sel_time$tfactor)],ref_time)

  #errors
  data("object_hc_st",package="ClimIndVis")
  testthat::expect_error(ClimIndVis:::get_date_factors(object_hc_st,"annual"))

  t_miss<-data_hc_st$time
  t_miss[["1982"]]<-seq(as.Date("1982-02-15"),as.Date("1982-02-15")+213,by="days")
  object_miss<-testthat::expect_error(make_object(prec=data_hc_st$prec[],dates_prec=t_miss,lon=data_hc_st$lon,lat=data_hc_st$lat,data_info=data_hc_st$data_info))
 }
)


test_that("if rearange_by_year rearanges correctly",{

  len <- 12*5
  dat <- 20
  iout <- matrix(rnorm(len*dat), ncol=dat, nrow=len) # generate iout
  dates <- as.Date(1:(365*5), origin="1981-01-01")
  date_factors <- as.factor(substr(as.character(dates),1,7))
  dimnames(iout) <- list(levels(date_factors),NULL)
  ireturn <- ClimIndVis:::rearange_by_year(iout,levels(date_factors))

  expect_equal(dim(ireturn),c(dat, 12,5)) # test dimensions
  expect_equal(ireturn[2,3,1],as.vector(iout[3,2])) # test values

}
)


test_that("get_quants return correct output ",{

  q_threshold <- 90
  baseperiod <- c(1981,1991)
  dates <- ClimIndVis:::get_date_factors(tp,"seasonal")

  qargs <- list(qth=q_threshold/100,baseperiod = baseperiod, th_quantiles=NULL,
             NAmaxbasep=20,qens_all=TRUE)

  quants <- ClimIndVis:::get_quants(climindvis=tp,var="prec",qargs=qargs,sel_time=dates)

  expect_equal(dim(quants$quants), c(4,dim(tp$data$prec)[1])) # test dimensions
  expect_equal(dim(quants$quants_help), dim(tp$data$prec)) # test dimensions
  expect_equal(as.vector(quants$quants["DJF",3]), quants$quants_help[3, which(dates$tfactor=="1985-DJF")[1]]) # test quants_helps

  ## test values
  winter <- which(substr(dates$tfactor,6,8)=="DJF")
  quants_test <- climdex.pcic:::climdex.quantile(tp$data$prec[3,winter],q=qargs$qth)
  expect_equal(as.vector(quants$quants[1,3]),quants_test)
})

test_that("get_quants return correct output for forecast data ",{

  data("object_hc_grid","object_fc_grid","object_fc_st","object_hc_st")
  q_threshold <- 90
  qargs <- list(qth=q_threshold/100,baseperiod = NULL, th_quantiles=NULL,
    NAmaxbasep=20,qens_all=TRUE)
  dates= ClimIndVis:::get_date_factors(object_hc_grid,"seasonal")

  quants_hc_grid=ClimIndVis:::get_quants(object_hc_grid,var="tmin",qargs=qargs,sel_time=dates)
  quants_hc_p=ClimIndVis:::get_quants(object_hc_st,var="tmin",qargs=qargs,sel_time=dates)

  #test values
  MAM <- which(substr(dates$tfactor,6,8)=="MAM")
  quants_test<-climdex.pcic:::climdex.quantile(array(object_hc_st$data$tmin[2,,MAM]),q=qargs$qth)
  testthat::expect_equal(quants_test,as.numeric(quants_hc_p$quants[1,2,1]))

  #test if correct quants are given to forecasts
  dates= ClimIndVis:::get_date_factors(object_fc_grid,"seasonal")
  qargs$th_quantiles=quants_hc_grid$quants
  quants_fc_grid=ClimIndVis:::get_quants(object_fc_grid,var="tmin",qargs=qargs,sel_time=dates)
  qargs$th_quantiles=quants_hc_p$quants
  quants_fc_p=ClimIndVis:::get_quants(object_fc_st,var="tmin",qargs=qargs,sel_time=dates)
  testthat::expect_equal(quants_fc_grid$quants,quants_hc_grid$quants)
  testthat::expect_equal(quants_fc_p$quants,quants_hc_p$quants)
})
