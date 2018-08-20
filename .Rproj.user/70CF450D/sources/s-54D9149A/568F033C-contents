#generate_test_data
point_data_info <- list(type="p",date_format="t1d",data_name="test_point")
grid_data_info  <- list(type="grid",date_format="t1d",data_name="test_grid")

point_data_info_t2d <- list(type="p",date_format="t2d",data_name="test_point")
grid_data_info_t2d  <- list(type="grid",date_format="t2d",data_name="test_grid")
grid_data_info_t2d_hc  <- list(type="grid_hc",date_format="t2d",data_name="test_grid_hc")


time <- as.Date(format(seq(as.Date(paste0(1981,"-01-01")), as.Date(paste0(1990,"-12-31")),by="1 day")))
time2 <- time[1:400]
time_t2d <- lapply(1981:1990,function(x){
  y <- as.Date(format(seq(as.Date(paste0(x,"-04-01"),origin="1970-01-01"), as.Date(paste0(x,"-12-31"),origin="1970-01-01"),by="1 day")))
  return(y)
  })

time_t2d_hc <- lapply(1981:1990,function(x){
  y <- as.Date(format(seq(as.Date(paste0(x,"-01-01"),origin="1970-01-01"), as.Date(paste0(x,"-01-01"),origin="1970-01-01")+213,by="1 day")))
  return(y)
})



lon <- 1:8
lat <- 1:8
lat2 <- 1:2

data_point<-array(rgamma(length(lon)*length(time), shape= 8.3),dim=c(length(lon),length(time)))
data_point2<-array(rgamma(length(lon)*length(time2), shape= 8.3),dim=c(length(lon),length(time2)))

years <- unique(substr(time,1,4))
dl_t2d <- length(years)*12* length(lon)
data_point_t2d<-array(rgamma(dl_t2d,shape= 8.3),dim=c(length(lon),275,length(years)))

data_grid <- aperm(replicate(8,data_point),c(1,3,2))
data_grid2 <- aperm(replicate(8,data_point2),c(1,3,2))
dl_gt2d <- length(lon)*length(lat)*length(years)*214
data_grid_t2d_hc <- array(rgamma(dl_gt2d, shape= 8.3),dim=c(length(lon),length(lat),214,length(years)))

testrange <- c(1981,1982)
