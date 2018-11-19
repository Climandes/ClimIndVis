context("calc_index")

#generate test data
source("generate_testdata_object.R")
#source("/prod/zue/climate/comm_serv/CLIMANDES_Data/src_C2/Rclimandes_2/imn_code/climandes-2/src/ClimIndVis/tests/testthat/generate_testdata_object.R")

tp <- make_object(prec=data_point,dates_prec=time,lon=lon,lat=lat,data_info=point_data_info)
tp_grid <- make_object(prec=data_grid,dates_prec=time,lon=lon,lat=lat,data_info=grid_data_info)


#test that function gives correct values
test_that("test function values with test data", {

  expect_equal(calc_index(tp,"rainy_season_start",rs_method="stern",aggt="dates",start_days="0000-01-10",end_days="0000-12-31", trend=FALSE)$index[2,1:3],
               c("1981"=1,"1982"=1,"1983"=1))
  })

#test gridded data
test_that("test function values with test data, gridded data", {
  expect_equal(
    calc_index(tp_grid,"rainy_season_start",rs_method="stern",  aggt="dates",  start_days="0000-01-10", end_days="0000-12-31",trend=FALSE)$index[,1,],

    calc_index(tp_grid,"rainy_season_start", rs_method="stern", aggt="dates", start_days="0000-01-10",end_days="0000-12-31",trend=FALSE)$index[,2,])})

##### test calc_indices_functions ######

#test that method gives the same result as basic function call
for (rs_method in c("gurgiser","stern","garcia")){
test_that("generic function versus manual calculation using base function", {
  expect_equal(calc_index(tp,"rainy_season_start",rs_method=rs_method  ,aggt="dates",   start_days="0000-01-10",
                                                                    end_days="0000-12-31", trend=FALSE)$index[2,],
  ClimIndVis:::rainy_season_start(data_point[2,],factor(ClimIndVis:::select_date_factors(time,"0000-01-10","0000-12-31")),rs_method=rs_method,nval=NA))})
}


test_that("index functions yield correct index", {

  # prepare manual test data
  len <-365*5
  temp_tmin <- c(runif(len, max=18, min=2))
  tt <- c(rnorm(len, mean=0, sd=30))
  tt[which(tt<0)] <- abs(tt[which(tt<0)])
  tt[sample(1:len, size = 587)] <- 0
  temp_prec <- tt

  dates <- as.Date(1:len, origin="1981-01-01")
  date_factors <- as.factor(substr(as.character(dates),1,7))
  dat_tab <- data.frame(temp_tmin=temp_tmin,temp_prec=temp_prec, date_factors=date_factors)

  # aggregate_var
  aggreg <- as.vector(ClimIndVis:::aggregate_var(temp_prec, date_factors, NAmaxAgg = 20, aggfun=mean))
  aggreg_test <- as.vector(aggregate(dat_tab["temp_prec"], by= dat_tab["date_factors"],
                                      function(x){
                                        mm <- mean(x)
                                        return(mm)
                                      })[["temp_prec"]])
  expect_equal(aggreg, aggreg_test)

  #ndays_op_threshold
  nd_opth<- as.vector(ClimIndVis:::ndays_op_threshold(temp_tmin,q_temp=NULL,q_temp_inbase = NULL,date_factor=date_factors, threshold=10, op = ">",iformat="perc",NAmaxAgg=20))
  nd_opth_test <- as.vector(aggregate(dat_tab["temp_tmin"], by= dat_tab["date_factors"],
                              function(x){
                                so_fun <- match.fun(">")(x,10)
                                ndays <- sum(so_fun==TRUE)
                                len <- length(x)
                                perc <- ndays/len*100
                                return(perc)
                              })[["temp_tmin"]])
  expect_equal(nd_opth, round(nd_opth_test,digits=1))

  #ndays_in_range
  nd_irange <- as.vector(ClimIndVis:::ndays_in_range(temp_tmin, date_factor = date_factors,threshold=10, op = ">",threshold2=12,
                                                     op2="<",iformat="days",NAmaxAgg=20 ))
  nd_irange_test <- aggregate(dat_tab["temp_tmin"], by= dat_tab["date_factors"],
                              function(x){
                                so_fun <- match.fun(">")(x,10) & match.fun("<")(x,12)
                                ndays <- sum(so_fun==TRUE)
                                return(ndays)
                                })[["temp_tmin"]]
  expect_equal(nd_irange,nd_irange_test)

  #total_precip_op_threshold
  totprecip <- as.vector(ClimIndVis:::total_precip_op_threshold(temp_prec,q_temp=NULL, date_factor = date_factors,threshold=1, op = ">",NAmaxAgg=20 ))
  totprecip_test <- aggregate(dat_tab["temp_prec"], by= dat_tab["date_factors"],
                              function(x){
                                mm <- sum(x[match.fun(">")(x,1)])
                                return(mm)
                              })[["temp_prec"]]
  expect_equal(totprecip,totprecip_test)

  #dailymean_precip_op_threshold
  dmean_precip <- as.vector(ClimIndVis:::dailymean_precip_op_threshold(temp=temp_prec, date_factor = date_factors, threshold=0, op= ">", NAmaxAgg=20))
  dmean_precip_test <- aggregate(dat_tab["temp_prec"], by= dat_tab["date_factors"],
                              function(x){
                                mm <- mean(x[match.fun(">")(x,0)])
                                return(mm)
                              })[["temp_prec"]]
  expect_equal(dmean_precip,dmean_precip_test)

  #spell_length_max
  spell <- as.vector(ClimIndVis:::spell_length_max(temp_prec, date_factors, threshold=0, op= ">",spells_span_agg = FALSE, NAmaxAgg=20))
  spell_test <- aggregate(dat_tab["temp_prec"], by= dat_tab["date_factors"],
                                 function(x){
                                   days_op <- match.fun(">")(x,0)
                                   maxspell<- max(ClimIndVis:::get_series_lengths_at_ends(days_op))
                                   return(maxspell)
                                 })[["temp_prec"]]
  expect_equal(spell,spell_test)

  #get_series_length_at_ends----
  days_op <- c(0.3,4.5,0,0,0,0,0,0,42,0)> 0
  series_lengths <- as.vector(ClimIndVis:::get_series_lengths_at_ends(days_op))
  expect_equal(series_lengths,c(0,2,0,0,0,0,0,0,1,0))

  #select_block_gt_length-----
  q_temp <- climdex.pcic::climdex.quantile(temp_tmin, q= 0.02)
  sel_blo<- ClimIndVis:::select_block_gt_length(d= match.fun("<")(temp_tmin,q_temp),n=5, na_value=FALSE)
  expect_is(sel_blo, "logical")
  val <- (temp_tmin[7]<q_temp & all(temp_tmin[2:7]< q_temp))
  expect_equal(sel_blo[7], val)


  #minmax value-----
  rx3 <- as.vector(ClimIndVis:::minmax_value(temp_prec, date_factors, func = max, NAmaxAgg=20, rx=3))
  jan <- max(caTools::runmean(temp_prec[1:31], k=3,endrule="NA"), na.rm=TRUE)*3
  feb <- max(caTools::runmean(temp_prec[31:(31+28)], k=3,endrule="NA"), na.rm=TRUE)*3 # start bei doy31, da runmean ueber 3 Tage geht
  expect_equal(rx3[1:2],c(jan,feb))



  # #spell_duration----
  # q_temp <- climdex.pcic::climdex.quantile(temp_tmin, q= 0.02)
  # new_facts <- agg_factor <- as.factor(gsub(".*-","",date_factors))
  # q_temp <- climdex.pcic:::tapply.fast(temp_tmin, new_facts, function(x) {
  #   qs <-climdex.pcic::climdex.quantile(x, q= 0.02)
  #   return(qs)
  #   })
  # q_temp_all <- c()
  # monthi <- function(i) {ifelse(i %% 12 == 0,12,i %% 12)}
  # for(i in 1:12){
  #   q_temp_all[which(monthi(1:length(temp_tmin))==i)] <- q_temp[i]
  # }
  # dat_tab[,"q_temp"] <- q_temp_all
  # f <- match.fun("<")
  # periods <- ClimIndVis:::select_block_gt_length(f(temp_tmin, q_temp_all),5)
  #
  # spell_duration <- as.vector(ClimIndVis:::spell_duration(temp_tmin,q_temp = q_temp_all, date_factor=date_factors, op= "<",min_length = 6, spells_span_agg = TRUE, NAmaxAgg=20))
  # spell_duration_test <- as.vector(aggregate(periods, by= dat_tab["date_factors"],sum)[,"x"])
  # expect_equal(spell_duration,spell_duration_test)




  }
     )

#
# #test quantile calculation
# test_that("quantile function in C yields correct output",{
#
#   bp <- substr(c(dates[1], tail(dates,1)),1,4 )
#   r_df <- as.Date(c(paste0(bp,c("-01-01","-12-31"))))
#   q_temp <- ClimIndVis:::zhang_running_qtile(temp_tmin, dates, qtiles=0.1, n=5,bootstrap_range=r_df, get_bootstrap_data = FALSE, min_fraction = 0.1)
#
#
#   nd_opth<- as.vector(ClimIndVis:::ndays_op_threshold(temp_tmin,q_temp=NULL,q_temp_inbase = NULL,date_factors, threshold=10, op = ">",iformat="perc",NAmaxAgg=20))
#   nd_opth_test <- as.vector(aggregate(dat_tab["temp_tmin"], by= dat_tab["date_factors"],
#                                       function(x){
#                                         so_fun <- match.fun(">")(x,10)
#                                         ndays <- sum(so_fun==TRUE)
#                                         len <- length(x)
#                                         perc <- ndays/len*100
#                                         return(perc)
#                                       })[["temp_tmin"]])
#   expect_equal(nd_opth, round(nd_opth_test,digits=1))
#
#
#   #expect_equal(as.vector(tpg$index[1,1,,]),spi_vals)
#
# }
# )



#test spi calculations for point data
test_that("spi is calculated correctly for grid data",{
## generate different objects
  test_obj_tpg <- make_object(prec= data_grid, dates_prec=time, lon=lon,lat=lat, data_info= grid_data_info)
  tpg <- calc_index(test_obj_tpg, index = "spi",aggt="monthly", timescale= 3, distribution="gamma")

  dates <- ClimIndVis:::get_date_factors(test_obj_tpg,"monthly")
  na <- climdex.pcic:::tapply.fast(is.na(data_grid[1,1,]), dates$tfactor,sum)
  dlength <- climdex.pcic:::tapply.fast(data_grid[1,1,], dates$tfactor,function(x) length(x))
  percna <- na/dlength > 20
  tp_agg_month <- climdex.pcic:::tapply.fast(data_grid[1,1,], dates$tfactor, sum, na.rm=TRUE)
  tp_agg_month[percna] <- NA

  spi_params<- SCI::fitSCI(tp_agg_month, first.mon=as.numeric(substr(time,6,7)[1]),time.scale=3, distr="gamma",p0=TRUE )
  spi_vals <- SCI::transformSCI(tp_agg_month, first.mon= as.numeric(substr(time,6,7)[1]), obj= spi_params, sci.limit= 4)
  expect_equal(as.vector(tpg$index[1,1,,]),spi_vals)

}
)

#test spi for grid
test_that("spi is calculated correctly for station data",{

  test_obj_tpo <- make_object(prec= data_point, dates_prec=time, lon=lon,lat=lat, data_info= point_data_info)
  tpo <- calc_index(test_obj_tpo, index = "spi",aggt="monthly", timescale= 3, distribution="gamma")

  dates <- ClimIndVis:::get_date_factors(test_obj_tpo,"monthly")
  na <- climdex.pcic:::tapply.fast(is.na(data_point[2,]), dates$tfactor,sum)
  dlength <- climdex.pcic:::tapply.fast(data_point[2,], dates$tfactor,function(x) length(x))
  percna <- na/dlength > 20
  tp_agg_month <- climdex.pcic:::tapply.fast(data_point[2,], dates$tfactor, sum, na.rm=TRUE)
  tp_agg_month[percna] <- NA

  spi_params<- SCI::fitSCI(tp_agg_month, first.mon=as.numeric(substr(time,6,7)[1]),time.scale=3, distr="gamma",p0=TRUE )
  spi_vals <- SCI::transformSCI(tp_agg_month, first.mon= as.numeric(substr(time,6,7)[1]), obj= spi_params, sci.limit= 4)
  expect_equal(as.vector(tpo$index[2,,]),spi_vals)

}
)






