context("plot_helpers")

#generate test data
source("generate_testdata_object.R")

tp <- make_object(prec=data_point,dates_prec=time,lon=lon,lat=lat,data_info=point_data_info)
tp_grid <- make_object(prec=data_grid,dates_prec=time,lon=lon,lat=lat,data_info=grid_data_info)


#test that function gives correct values
test_that("get_lims returns lims", {

  #some data
  dat <- runif(2000,-10,10)
  lims <- ClimIndVis:::get_lims(dat)
  lims_test <- list(lim=range(dat, na.rm=TRUE), diffs= NULL)
  expect_equal(lims, lims_test) # with runif should always be diffs=NULL

})


#test that function gives correct values
test_that("get_breaks returns breaks", {
  #some data
  dat <- runif(2000,-9,9)
  br <- ClimIndVis:::get_breaks(dat, zlims = NULL, nlev = 8,center = TRUE)
  expect_is(br, "list") # with runif should always be diffs=NULL
  expect_is(br$outliers, "logical") # with runif should always be diffs=NULL
  expect_is(br$breaks, "numeric") # with runif should always be diffs=NULL

})

