## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE----------------------------------------------------------

knitr::opts_chunk$set(fig.path='Figs/',
                      echo=TRUE, warning=FALSE, message=FALSE,evaluate=TRUE)

library(ClimIndVis)
data ("object_grid","object_st")

## ----ts_single, fig.width=3.2, fig.height=2.5,fig.show='hold'------------
data(object_st)
index_args = list(aggt = "monthly",selagg = 3)
autoplot_ts_stations(dat_p= object_st,index = "dd",index_args = index_args,ts_type = "single_ts",
  selpoints = 1,text_cex = 0.6) #this line can be omitted if you are using the default parameters and want to plot the results for all stations.
autoplot_ts_stations(dat_p = object_st,index = "dd",index_args = index_args,ts_type = "single_ts",trendplots = TRUE,
    selpoints = 1,text_cex = 0.6) #this line can be omitted if you are using the default parameters and want to plot the results for all stations.

## ----ts_single_hc, fig.width=3.2, fig.height=2.5,fig.align='center'------
data(object_hc_st)
index_args = list(aggt = "monthly",selagg = 3)
autoplot_ts_stations(dat_p= object_hc_st,index = "dd",index_args = index_args,ts_type = "single_ts",
  selpoints = 1,text_cex = 0.6) #this line can be omitted if you are using the default parameters and want to plot the results for all stations.


## ----ts_single write, eval=FALSE,echo=TRUE-------------------------------
#   data(object_st)
#  index_args = list(aggt = "monthly",selagg = 3)
#  autoplot_ts_stations(dat_p= object_st,index = "dd",index_args = index_args,ts_type = "single_ts",
#    output="png",plotdir=name_of_directory_to_save_plots_in,plotname="test")
#  autoplot_ts_stations(dat_p = object_st,index = "dd",index_args = index_args,ts_type = "single_ts",trendplots = TRUE,
#    output="png",plotdir=name_of_directory_to_save_plots_in,plotname="test_with_trend")

## ----ts_multind, fig.width=3.2, fig.height=2.5,fig.show='hold'-----------
data(object_st)
index=c("tnn","tnx")
aggt="seasonal"
selagg="JJA"
index_args = list(tnn=list(aggt = aggt, selagg = selagg),
                  tnx=list(aggt = aggt, selagg = selagg))
autoplot_ts_stations(dat_p= object_st,index = index,index_args = index_args,ts_type = "multi_ind",trendplots=TRUE,
  selpoints = 1,text_cex = 0.6) #this line can be omitted if you are using the default parameters and want to plot the results for all stations.
autoplot_ts_stations(dat_p= object_st,index = index,index_args = index_args,ts_type = "multi_ind", selyears=c(1990:2000),trendplots=TRUE,
  selpoints = 1,text_cex = 0.6) #this line can be omitted if you are using the default parameters and want to plot the results for all stations.

## ----ts_multagg, fig.width=3.2, fig.height=2.5,fig.align='center'--------
data(object_st)
index_args = list(list(aggt = "seasonal",selagg="DJF"),
                  list(aggt="annual"))
autoplot_ts_stations(dat_p= object_st,index = "dd",index_args = index_args,ts_type = "multi_agg",trendplots=TRUE,
  selpoints = 1,text_cex = 0.6 #this line can be omitted if you are using the default parameters and want to plot the results for all stations.
  )


## ----ts_multpoint, fig.width=3.3, fig.height=2.5,fig.show='hold'---------
data(object_st)
index_args=list(aggt="monthly",selagg=c(1))
autoplot_ts_stations(dat_p = object_st,index="tx10p",index_args=index_args,ts_type="multi_point", pcols=c("purple","green"),selpoints=c(2:3),
 text_cex=0.6) #this line can be omitted if you are using the default parameters and want to plot the results for all stations.
autoplot_ts_stations(dat_p = object_st,index="tn10p",index_args=index_args,ts_type="multi_point",
  text_cex=0.6) #this line can be omitted if you are using the default parameters and want to plot the results for all stations.


## ----ts_dat, fig.width=3.3, fig.height=2.5,fig.align="center"------------
data(object_st, object_hc_st)
index_args=list(aggt="seasonal")
autoplot_ts_stations(dat_p=list(object_st,object_hc_st) ,index="prcptot",index_args=index_args,ts_type="multi_dat",
  selpoints=1,text_cex=0.6) #this line can be omitted if you are using the default parameters and want to plot the results for all stations.



## ----ts_spi, fig.width=3.3, fig.height=2.5,fig.align='center'------------
data(object_st)
index_args=list(aggt="monthly")
autoplot_ts_stations(dat_p = object_st,index="spi",index_args=index_args,ts_type="spi_barplot",
  selpoints=1,text_cex=0.6) #this line can be omitted if you are using the default parameters and want to plot the results for all stations.


## ----clim_map_code,eval=FALSE,echo=TRUE----------------------------------
#  #left figure
#  autoplot_climatology_map(dat_p = object_st,
#    index = "sdii", index_args = list(aggt="other", aggmons=c(11:12,1:4)),
#    plot_value="mean",
#    output="png",plotdir=plotdir,plotname="climatology_map1-1")
#  
#  #right figure
#  ylims=c(-17,-12) #change if using example objects
#  xlims=c(287,292) #change if using example objects
#  plot_args=list(p_cex=2,topo=list(alt=g_dat,lat=g_lat,lon=g_lon),ylims=ylims, xlims=xlims,mask_col="lightblue",NA_col="grey")
#  autoplot_climatology_map(dat_grid=object_grid, dat_p = object_st,
#    index = "sdii", index_args = list(aggt="other", aggmons=c(11:12,1:4)),NAmaxClim = 20,
#    plot_value="mean",plot_args=plot_args,
#    output="png",plotdir=plotdir)
#  

## ----clim_map, fig.width=3.3, fig.height=2.5,fig.show='hold',echo=FALSE----
knitr::include_graphics("Figs/climatology_map1-1_SDII_mean_NDJFMA_1981-2009.png",dpi=400)
knitr::include_graphics("Figs/climatology_map1-2_SDII_mean_NDJFMA_1981-2009.png",dpi=400)

## ----trend_map_code,eval=FALSE,echo=TRUE---------------------------------
#  plot_args=list(p_cex=2,topo=list(alt=g_dat,lat=g_lat,lon=g_lon),ylims=c(y2,y1), xlims=c(x1,x2),mask_col="lightblue",NA_col="grey")
#  autoplot_trend_map(dat_grid=object_grid,
#    index="mean",index_args = list(aggt="seasonal", selagg=c("DJF","JJA"),var="tmin"),plot_args=plot_args,
#      output="png",plotdir=plotdir)

## ----trend_map_plots, fig.width=3.3, fig.height=2.5,fig.show='hold',echo=FALSE----

knitr::include_graphics("Figs/trend_map1-1_mean_tmin_DJF_abs-trend_1981-2010.png",dpi=400)
knitr::include_graphics("Figs/trend_map1-1_mean_tmin_JJA_abs-trend_1981-2010.png",dpi=400)

## ----overview_stations, fig.width=5, fig.height=7,fig.align='center'-----
data("object_st")
indices=c("dd","fd","th","tx90p","cdd","sdii")
common_args=list(aggt="dates",start_days="0000-12-15", end_days="0000-03-15")
indices_args=list(dd=common_args,
  fd=common_args,
  th=c(common_args,list(th=20,thvar="prec",op=">")), 
  tx90p=common_args,
  cdd=common_args,
  sdii=common_args
)

autoplot_overview_stations(dat_p=object_st,indices=indices,indices_args=indices_args,
  addyears=c(1983,1998),yearcols=c("orange","red"), selpoints = 2)


## ----forecast_spi_plots,fig.width=6.3, fig.height=4.5, fig.align="center"----
data(object_st, object_fc_st)
autoplot_forecast_spi(obs_p=object_st, fc_p=object_fc_st,index_args = list(aggt = "monthly", timescale = 3),selpoints=1, plotstart = 2008,text_cex = 0.8)

