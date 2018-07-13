#'Wrapper function to calculate and plot timerseries of SPI from station data with ensemble seasonal forecast
#'
#'The function returns a plot with the historical SPI values as a barplot and the ensemble forecasts as boxplots.
#'The function works with station data only and needs a observational time series and an ensemble forecast as input.
#'To see some example outputs of this function, look at the vignette "autoplot-functions" accessible through the package help main page or by typing \cr
#'\emph{vignette("autoplot-functions",package="ClimIndVis")} into the console.
#'@inheritParams plot_doc
#'@inheritParams spi_doc
#'@param index Character string. This is set to index="spi_forecast" and does not have to be provided.
#'@param index_args List of arguments for index. For arguments see \code{\link{index_arguments.spi_forecast}}
#'@param plotstart Numeric. Chosen start year of historic time series plot. For a reference period of e.g. 1081-2010 and a forecast for 2011, plotstart can be set to 2009 and only 2009-2010 and the forecast 2011 is displayed.
#'@section Details:
#' Note, that for the SPI calculation, aggt needs to be set to "monthly". For further arguments see \code{\link{index_arguments.spi_forecast}}
#' The forecast dates need to continue the time series of the historical dataset, i.e. if the observational data ends on 2010-04-30, the forecast data needs to start 2010-05-01. Then the SPI is calculated for the overlapping period depending on the chosen timescale of the SPI calculation. If there is a time gap between forecast and historical data, no overlapping period is calculated.
#' The function can only be applied for station data and not for gridded data. Note, that you have to provide the same amount of station data in the two climdindvis-objects forecast and observation (obs_p, fc_p). No checks are executed, whether the coordinates of the observational data corresponds to the coordinates of the forecast data. This will be added in a upcoming version.
#'
#' The SPI calculation is based on the SCI package from Lukas Gudmundsson & James Stagge. For the argument distribution, provide a distribution for which the corresponding density function (dname), the corresponding distribution function (pname) and the quantile function (qname) must be defined (see for example GammaDist). Distributions are listed in the package stats.\cr
#'
#'@references
#'Stagee, J.H. ; Tallaksen, L.M.; Gudmundsson, L.; van Loon, A.; Stahl, K.: Candidate Distributions for Climatological Drought Indices (SPI and SPEI), 2015, International Journal of Climatology, 35, 4027-4040, doi:10.1002/joc.4267.
#' @examples
#'
#' #' ## Load example climindvis objects:
#' data(object_st, object_fc_st)
#'
#'autoplot_forecast_spi(
#'      obs_p = object_st,fc_p = object_fc_st, index = "spi_forecast",
#'      index_args = list(aggt = "monthly", timescale= 3),plotstart=2008)
#' @family autoplot_functions
#' @export
autoplot_forecast_spi<-function(obs_p, fc_p,
                               index = "spi_forecast",index_args,trendplots=FALSE,selpoints=NULL,
                               output = NULL,plotdir,plotname ="",plotstart, plot_title=TRUE, title = "",text_cex=1,...){
  opargs <- list(...)
  # 2. check point or grid
  points <- ifelse (missing(obs_p)  & missing (fc_p),0,1)
  grid <- 0
  if(points==0) stop("Point data has to be provided.")

  # 2. check input
  check_dir(plotdir,output)
  dat=get_data_list(c("obs","fc"))
  check_class(dat[!sapply(dat,is.null)],"climindvis")
  if(any(is.element(sapply(dat,function(x) x$data_info$type),c("grid","grid_hc")))) stop ("Function only works for point data.")
  if(any(is.element(sapply(dat,function(x) x$data_info$type),"p_hc"))) stop ("Function does not work for hindcast data.")

  # 3. calculate spi hist and spi forecast
  index_args$forecast <- dat[["fc_p"]]
  ind_dat <-    list(do.call("calc_index", c(list(dat[["obs_p"]],index),index_args)))

  pdims <- match_pdims_index(ind_dat,selpoints=selpoints,...)
  if (is.null(selpoints)) selpoints=1:pdims$pnumber

  # 2.2 calculate missing values
  full_dat_NA = apply(dat$obs_p$data$prec, 1,function(x) sum(is.na(x))*100/length(x))

  plot_spi_forecast(ind_dat,full_dat_NA,trendplots,selpoints=selpoints, pdims=pdims, plotdir=plotdir,output=output,plotname=plotname,plotstart = plotstart,text_cex=text_cex,plot_title=plot_title,title=title,...)

}