context("make_object")

# load the needed data: all the data is now as well in climindvis-2/src/climindvis/tests/testdata
source("generate_testdata_object.R")

test_that("make climindvis object return an object with correct structure and data",{

  ## generate different objects
  test_object_point <- make_object(tmin=data_point, tmax=data_point,prec=data_point,tavg=data_point,dates_tmax=time,dates_tmin=time,dates_prec = time,dates_tavg = time,lon=lon,lat=lat,data_info=point_data_info)
  test_object_grid <- make_object(tmin=data_grid,tmax=data_grid,prec=data_grid,tavg=data_grid,dates_tmin=time,dates_tmax=time,dates_prec = time,dates_tavg = time,lon=lon,lat=lat,data_info=grid_data_info)
  test_object_grid_t2d_hc <- make_object(tmin=data_grid_t2d_hc,tmax=data_grid_t2d_hc,dates_tmin=time_t2d_hc,dates_tmax=time_t2d_hc,lon=lon,lat=lat,data_info=grid_data_info_t2d_hc)

  # to different time lengths
  test_object_point2 <- make_object(tmin=data_point2, tavg=data_point,dates_tmin=time2,dates_tavg = time,lon=lon,lat=lat,data_info=point_data_info)
  test_object_grid2 <- make_object(tmin=data_grid2, tavg=data_grid,dates_tmin=time2,dates_tavg = time,lon=lon,lat=lat,data_info=grid_data_info)

  # to range
  test_object_point_range <- make_object(tmin=data_point2, tavg=data_point,dates_tmin=time2,dates_tavg = time,lon=lon,lat=lat,data_info=point_data_info, data_range = testrange)
  test_object_grid_range <- make_object(tmin=data_grid2, tavg=data_grid,dates_tmin=time2,dates_tavg = time,lon=lon,lat=lat,data_info=grid_data_info, data_range = testrange)

  # to t1d
  test_object_point_t2d <- make_object(tmin=data_point_t2d, tavg=data_point_t2d,dates_tmin=time_t2d,dates_tavg = time_t2d,lon=lon,lat=lat,data_info=point_data_info_t2d)


## is "climindvis" object
expect_equal(class(test_object_point),"climindvis")
expect_equal(class(test_object_grid),"climindvis")
expect_equal(class(test_object_grid_t2d_hc),"climindvis")

## test if there are seven positions in list and all occupied
expect_equal(length(test_object_grid),7)
help_test <- lapply(test_object_grid, is.null)
names(help_test) <- NULL
expect_equal(help_test,list(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE))

## errors
# time missing
expect_error(make_object(tmin=data_point, tmax=data_point,prec=data_point,tavg=data_point,dates_tmax=time,dates_prec = time,dates_tavg = time,lon=lon,lat=lat,data_info=point_data_info))
# time wrong
expect_error(make_object(tmin=data_point, tmax=data_point,prec=data_point,tavg=data_point,dates_tmax=time2,dates_prec = time,dates_tavg = time,dates_tmin = time,lon=lon,lat=lat,data_info=point_data_info))
# time of t2d not equal
expect_error(make_object(tmin=data_grid_t2d_hc,tmax=data_grid_t2d_hc,dates_tmin=time_t2d_hc,dates_tmax=time_t2d,lon=lon,lat=lat,data_info=grid_data_info_t2d_hc))
# lat wrong
expect_error(make_object(tmin=data_point, tavg=data_point,dates_tavg = time,dates_tmin = time,lon=lon,lat=lat2,data_info=point_data_info))
data("data_hc_st","data_st")
# hc and t1d
expect_error(make_object(tmin=data_hc_st$tmin, tavg=data_hc_st$tmax,dates_tavg = data_hc_st$time, dates_tmin =data_hc_st$time,lon=data_hc_st$lon,lat=data_hc_st$lat,data_info=list(type="p_hc",data_name="test",date_format="t1d")))
expect_error(make_object(tmin=data_hc_st$tmin, tavg=data_hc_st$tmax,dates_tavg = data_st$time, dates_tmin =data_hc_st$time,lon=data_hc_st$lon,lat=data_hc_st$lat,data_info=list(type="p_hc",data_name="test",date_format="t2d")))

expect_error(make_object(tmin=data_point, tavg=data_point,dates_tavg = time,dates_tmin = time,lon=lon,lat=lat2,data_info=point_data_info))

### cut time to same lengths
dl <- length(test_object_point2$data) # point data
testlist <- as.list(rep(length(test_object_point2$time),dl))
objectlist <- lapply(test_object_point2$data,function(x) {length(x[1,])})
names(objectlist) <- names(testlist)
expect_equal(objectlist,testlist )

dl <- length(test_object_grid_t2d_hc$data) # grid data
testlist <- as.list(rep(length(test_object_grid_t2d_hc$time),dl))
objectlist <- lapply(test_object_grid_t2d_hc$data,function(x) {length(x[1,1,])})
names(objectlist) <- names(testlist)
expect_equal(objectlist,testlist )

## time range
new_time <- as.Date(format(seq(as.Date(paste0(testrange[1],"-01-01")), as.Date(paste0(testrange[2],"-12-31")),by="1 day")))
testlist <- as.list(rep(length(new_time),dl)) # point data
objectlist <- lapply(test_object_point_range$data,function(x) {length(x[1,])})
names(objectlist) <- names(testlist)
expect_equal(objectlist,testlist )

new_time <- as.Date(format(seq(as.Date(paste0(testrange[1],"-01-01")), as.Date(paste0(testrange[2],"-12-31")),by="1 day")))
testlist <- as.list(rep(length(new_time),dl)) # grid data
objectlist <- lapply(test_object_grid_range$data,function(x) {length(x[1,1,])})
names(objectlist) <- names(testlist)
expect_equal(objectlist,testlist )

## t2d conversion
expect_equal(length(lapply(test_object_grid_t2d_hc$data, dim)[[1]]), 3)
expect_equal(length(lapply(test_object_point_t2d$data, dim)[[1]]), 2)

# ## cut at end of 7th fc month
# expect_equal(tail(test_object_grid_t2d_hc$time, n=1), as.Date("1990-07-31", origin="1970-01-01"))
# grid_test <- lapply(test_object_grid_t2d_hc$data, function(x){ return(length(x[1,1,]))})
# testlist <- list(2120, 2120)
# names(grid_test) <- names(testlist)
# expect_equal(grid_test, testlist)

a <- data_grid_t2d_hc[,,2,1]
b <- test_object_grid_t2d_hc$data$tmin[,,2]
expect_equal(a,b)


}
)

