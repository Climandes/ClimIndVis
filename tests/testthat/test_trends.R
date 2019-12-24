context("calc_index_trend")

source("generate_testdata_object.R")
data(data_st,data_grid)
test_that("trend is calculated",{

  ## generate different objects
  test_obj_tpo <- make_object(prec= data_st$prec, dates_prec=data_st$time, lon=data_st$lon,lat=data_st$lat, data_info= data_st$data_info)
  test_obj_tpg <- make_object(prec= data_grid$prec, dates_prec=data_grid$time, lon=data_grid$lon,lat=data_grid$lat, data_info= data_grid$data_info)

  tpo <- calc_index(test_obj_tpo, index = "dd",aggt="seasonal",thvar="prec",iformat="perc",trend=TRUE)
  tpo_rs <- calc_index(test_obj_tpo, index = "rainy_season_start",rs_method="jd",aggt="dates",start_days="0000-01-10",end_days="0000-12-31",trend=TRUE)
  tpg <- calc_index(test_obj_tpg, index = "dd",aggt="seasonal",thvar="prec",iformat="perc",trend=TRUE)

  tpo_spi <- calc_index(test_obj_tpo, index = "spi",aggt="monthly", timescale= 3, distribution="gamma", trend = TRUE)

  # df from index
  df_ind <- data.frame(V1=as.numeric(names(tpo$index[1,1,])),V2=tpo$index[1,1,]/100)
  trd <- ClimIndVis:::trend.logit(df_ind[,1:2], type = "probs")
  fit1 <- predict(trd$mod, data.frame(year=df_ind[,1]), interval = "confidence", se.fit = T, level = 0.95)$fit
  fit <- exp(fit1)/(1+exp(fit1))*100
  expect_equal(as.vector(tpo$index_trend[1,1,1:29,1]),as.vector(fit)[1:29])

  df_ind <- data.frame(V1=as.numeric(names(tpg$index[1,1,1,])),V2=tpg$index[1,1,1,]/100)
  expect_equal(as.vector(tpg$index_trend[1,1,1,1:3,1]),
               ClimIndVis:::trend.logit(df_ind, type = "probs")$fitted[1:3,2]*100)

  df_ind <- data.frame(V1=as.numeric(names(tpo_spi$index[1,1,])),V2=as.vector(tpo_spi$index[1,1,]))
  trd1 <- ClimIndVis:::trend.linreg(df_ind)
  fit <- as.vector(predict(trd1$mod, data.frame(year=df_ind[,1]), interval = "confidence", se.fit = T, level = 0.95)$fit[,1])
  expect_equal(as.vector(tpo_spi$index_trend[1,1,2,1]),fit[2])

}
)


