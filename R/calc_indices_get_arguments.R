#' get arguments index
#' Get arguments for index to pass to function and check if variable from which index is calculated exists
#' Generic function for getting climindvis index arguments
#' @param NAmaxAgg Maximum number of missing values in daily data of each aggregation period (e.g. in all January values for the whole time series). Value between 0 (=0\%; no missing values allowed) and 100 (=100\%; all values can be missing values). If the number of missing values is higher than NAmaxAgg the index value for the respective grid point/station is set to NA. Default = 20 (20\%).
#' @keywords internal
index_arguments<-function(climindvis,...){
  UseMethod("index_arguments")
}

get_arguments.default<-function(climindvis){
  stop("index is not defined")
}

# documentation of parameters which are the same for all functions  ---------------------------------------------------------------

#'varall
#' @param climindvis Object of type climindvis as returned by \code{\link{make_object}} which must contain the variable "var".
#' @name varall
#' @keywords  internal
NULL

#'varallth
#' @param climindvis Object of type climindvis as returned by \code{\link{make_object}} which must contain the variable "thvar".
#' @name varallth
#' @keywords  internal
NULL

#'varprec
#' @param climindvis Object of type climindvis as returned by \code{\link{make_object}} which must contain the variable precipitation (prec)
#' @name varprec
#' @keywords  internal
NULL

#'vartmin
#' @param climindvis Object of type climindvis as returned by \code{\link{make_object}} which must contain the variable minimum temperature (tmin)
#' @name vartmin
#' @keywords  internal
NULL

#'vartmax
#' @param climindvis Object of type climindvis as returned by \code{\link{make_object}} which must contain the variable maximum temperature (tmax)
#' @name vartmax
#' @keywords  internal
NULL

#'vartavg
#' @param climindvis Object of type climindvis as returned by \code{\link{make_object}} which must contain the variable average temperature (tavg)
#' @name vartavg
#' @keywords  internal
NULL

#'iformat
#'@param iformat Format for indicator "perc"(default)/"days"
#'@name iformat_doc
#'@keywords  internal
NULL

#'trend
#'@param trend Logial (or character). Calculates trends with a predefined method for the chosen index. For information on default method, see  \code{\link{trend_methods}}. You can chose to calculate a non-parameteric trend, by setting trend to a character string "MannKendall". This calculates a TheilSen slope and MannKendall test. Default = FALSE.
#'@param NAmaxTrend Maximum number of years in time series of index which are missing values. Value between 0 (=0\%); no years with missing values allowed) and 100 (=100\%); all years can be missing values). If exceeded, the trend is set to NA for the respective grid point/station. Note that the value is relative to the length of the time series. Default = 20 (20\%).
#'@name trend_doc
#'@keywords internal
NULL

#'quantile
#'@param baseperiod Vector of start and end year for the calculation of quantiles. If not provided, whole range of years in dataset will be used for the calculation of quantiles.
#'@param q Vector of quantiles, default to seq(0,1,by=0.01)
#'@param th_object Climindvis_index object, default=NULL. if calculating a quantile index for forecast data, a climindvis_index object of hindcasts data for the same spatial dimensions and forecast months has to be provided. Threshold values will then be taken from the hindcast object (object$index_info$quantiles). In this case all other arguments (q, baseperiod, threhold,..) are ignored and taken from the climindvis_index object. In case of autoplot functions, these values are handed over automatically and th_object should be left at default value of NULL..
#'@param q_threshold Percentile threshold (numerical). Value between 0 and 100.
#'@param percentile Percentile threshold (numerical). Value between 0 and 100.
#'@param percentile1 Lower percentile threshold (numerical). Value between 0 and 100.
#'@param percentile2 Upper percentile threshold (numerical). Value between 0 and 100.
#'@param n Window size (in days) for running window in temperature quantile calculation. Default =5. See details for further information.
#'@param inbase For temperature quantiles, calculate quantiles inside baseperiod and outside baseperiod follwoing the boostrapping method of Zhang 2005. Only applicable, when baseperiod is selected. Default = TRUE. For hindcast data, inbase is set to FALSE because boostrapping of ensemble and years is computational intensive.
#'@param NAmaxQ Minimim number of days needed for calculation of quantiles. Default = 365 [days]. For monthly aggregation of precipitation, consider lowering the threshold or expanding the baseperiod.
#'@param min_base_fraction Minimum fraction of base data that must be present for quantile to be calculated for a particular day (only applies to temperature quantiles). Default = 0.1.
#'@param qens_all=TRUE logical. Should quantiles of ensembles be calculated over all ensemble members (TRUE) or for each member individually (FALSE)? Default = TRUE
#'@section Details:
#'Quantiles are calculated identically to the calculations performed in the R-package climdex.pcic. For precipitation, quantiles are calculated with the function quantile(..., type = 8) from R stats using all values inside the selceted aggregation.\cr
#'For temperature, quantiles are calculated for every day of the year independently based on a moving window with default 5 days. Following the suggestiosn of Zhang (2005) it is possible to calculate a bootstrapping for quantile thresholds within the baseperiod by setting inbase=TRUE. If inbase is set to FALSE, the same thresholds are used inside and outside the reference period.\cr
#'For ensemble data (hindcasts), it is not possible to calculate the boostrapping inside the reference period. Consider therefore using the full period as reference period.
#'Please note, that the quantile calculation of ensembles stil may be subject to errors. In case you note inconsistencies, please report these to the package authors.
#'For a detailed description of this method, see:
#'Zhang, X, Hegerl, G, Zwiers, F.W and Kenyon, J. (2005). Avoiding inhomogeneity in percentile-base indices of temperature extremes. Journal of Climate, 18.
#'@name quantile_doc
#'@keywords internal
NULL

#'threshold
#'@param threshold Threshold (numerical). Thresholds need to be provided in mm for precipitation and in degrees C for temperature.
#'@param operator One of ">",">=","<","<="
#'@param threshold2 Threshold (numerical)
#'@param operator2 One of ">",">=","<","<="
#'@param thvar Variable for which days in range should be calculated (tmin/tmax/tavg/prec) by threshold operator thvar operator2 threshold2
#'@param dd_threshold Precipitation threshold for dry days in mm. Default = 1
#'@name th_doc
#'@keywords internal
NULL

#'SPI
#'@param timescale Integer. Timescale for monthly SPI calculation. The timescale is the window length (in month) of a backward looking running mean. Default = 6.
#'@param ref Vector with start and end year for reference period. If no reference period is specified, the full period is used as reference period.
#'@param distribution A character string naming a distribution of package stats (see for example GammaDist). For more information on the choice of the distribution, see section Details. Default = "gamma".
#'@param limit Truncate SPI values that are larger than a given threshold, as values larger than 4 are not reasonable. Default = 4. The truncation can be disabled by setting limit = Inf.
#'@section Details:
#' The SPI calculation is based on the SCI package from Lukas Gudmundsson & James Stagge. For the SPI calculation, aggt needs to be set to "monthly". For the argument distribution, provide a distribution for which the corresponding density function (dname), the corresponding distribution function (pname) and the quantile function (qname) must be defined (see for example GammaDist). Distributions are listed in the package stats.\cr
#' Note, that it is important, what distribution you choose for the transformation of your rainfall data. If you are not sure, consider the below references.
#'@references
#'Stagee, J.H. ; Tallaksen, L.M.; Gudmundsson, L.; van Loon, A.; Stahl, K.: Candidate Distributions for Climatological Drought Indices (SPI and SPEI), 2015, International Journal of Climatology, 35, 4027-4040, doi:10.1002/joc.4267.
#'@name spi_doc
#'@keywords internal
NULL


# mean/sum ---------------------------------------------------------------
#'arguments for calculating mean
#'arguments for calculating sum
#'
#' @inheritParams varall
#' @param var Variable for which mean should be calculated (tmin/tmax/tavg/prec)
#' @inheritParams index_arguments
#' @inheritParams trend_doc
#' @examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="mean",aggt="seasonal",var="tmax")
#' @keywords internal
index_arguments.mean<-function(climindvis,var,NAmaxAgg=20,trend=FALSE,NAmaxTrend=20, ...){
  if(missing(climindvis) ||missing(var)) stop("not all mandatory arguments provided for calculation of index")
  check_var(climindvis,var)
  return(list(ifun="aggregate_var",var=var,ifunargs=list(aggfun="mean",NAmaxAgg=NAmaxAgg),

    trend=trend,trendargs=list(method="lin_reg", count=FALSE, log_trans=ifelse(var=="prec",TRUE,FALSE),NAmaxTrend=NAmaxTrend, rel = check_trend(var)),
    plotargs=list(iname=paste0("mean",ifelse(var=="prec"," daily "," "),var),iformat=ifelse(var=="prec","mm","degreeC"))))
}

#'arguments for calculating sum
#' @inheritParams varall
#' @param var Variable for which sum should be calculated (tmin/tmax/tavg/prec)
#' @inheritParams index_arguments
#' @inheritParams trend_doc
#' @examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="sum",aggt="seasonal",selagg=c("JJA","DJF"),var="tmin")
#' @keywords internal
index_arguments.sum<-function(climindvis,var,NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,...){
  if(missing(climindvis) ||missing(var)) stop("not all mandatory arguments provided for calculation of index")
  check_var(climindvis,var)
  return(list(ifun="aggregate_var",var=var,ifunargs=list(aggfun="sum",NAmaxAgg=NAmaxAgg),
    trend=trend,trendargs=list(method="lin_reg", count=FALSE, log_trans=ifelse(var=="prec",TRUE,FALSE),NAmaxTrend=NAmaxTrend, rel = check_trend(var)),
   plotargs=list(iname=paste0("sum ",var),iformat=ifelse(var=="prec","mm","degreeC"))))
}


# minmax ------------------------------------------------------------------

#'arguments for index tnn
#'arguments for calculating sum
#'@inheritParams vartmin
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#' @examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="tnn",aggt="monthly")
#'@keywords internal
index_arguments.tnn<-function(climindvis,NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,...){
  check_var(climindvis,"tmin")
  return(list(ifun="minmax_value",var="tmin",ifunargs=list(func=min, NAmaxAgg=NAmaxAgg),
    trend=trend,trendargs=list(method="lin_reg",count=FALSE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel=FALSE),
    plotargs=list(iname="TNN",iformat="degreeC")))
}

#'arguments for index tnx
#'@inheritParams vartmin
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="tnx",aggt="annual")
#'@keywords internal
index_arguments.tnx<-function(climindvis,NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,...){
  check_var(climindvis,"tmin")
  return(list(ifun="minmax_value",var="tmin",ifunargs=list(func=max, NAmaxAgg=NAmaxAgg),
    trend=trend,trendargs=list(method="lin_reg",count=FALSE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel=FALSE),
    plotargs=list(iname="TNX",iformat="degreeC")))
}

#'arguments for index txn
#'@inheritParams vartmax
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="txn",aggt="other",aggmons=c(4:6))
#'@keywords internal
index_arguments.txn<-function(climindvis,NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,...){
  check_var(climindvis,"tmax")
  return(list(ifun="minmax_value",var="tmax",ifunargs=list(func=min, NAmaxAgg=NAmaxAgg),
    trend=trend,trendargs=list(method="lin_reg",count=FALSE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel=FALSE),
    plotargs=list(iname="TXN",iformat="degreeC")))
}



#'arguments for index txx
#'@inheritParams vartmax
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#' @examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="txx",aggt="monthly", selagg=c(2:4))
#'@keywords internal
index_arguments.txx<-function(climindvis,NAmaxAgg=20,trend=FALSE,NAmaxTrend=20, ...){
  check_var(climindvis,"tmax")
  return(list(ifun="minmax_value",var="tmax",ifunargs=list(func=max, NAmaxAgg=NAmaxAgg),
    trend=trend,trendargs=list(method="lin_reg",count=FALSE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel=FALSE),
    plotargs=list(iname="TXX",iformat="degreeC")))
}

#'arguments for index varmin
#'@inheritParams varall
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#' @examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="varmin",aggt="monthly",var="prec",trend=TRUE)
#'@keywords internal
index_arguments.varmin<-function(climindvis,var,NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,...){
  check_var(climindvis,var)
  var_units<-ifelse (var=="prec","mm","degreeC")
  return(list(ifun="minmax_value",var=var,ifunargs=list(func=min, NAmaxAgg=NAmaxAgg),
    trend=trend,trendargs=list(method="lin_reg",count=FALSE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel = check_trend(var)),
    plotargs=list(iname=paste0("min ",var),iformat=var_units)))
}

#'arguments for index varmax
#'@inheritParams varall
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#' @examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="varmax",aggt="annual",var="tmin",trend=TRUE,NAmaxAgg=10)
#'@keywords internal
index_arguments.varmax<-function(climindvis,var,NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,...){
  check_var(climindvis,var)
  var_units<-ifelse (var=="prec","mm","degreeC")
  return(list(ifun="minmax_value",var=var,ifunargs=list(func=max, NAmaxAgg=NAmaxAgg),
    trend=trend,trendargs=list(method="lin_reg",count=FALSE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel = check_trend(var)),
    plotargs=list(iname=paste0("max ",var),iformat=var_units)))
}

#'arguments for index rx days
#'@inheritParams varprec
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@param rx Length of days to be considered. i.e. rx1day, rx5day, etc. Default set to 3.
#' @examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="rx",aggt="seasonal",rx=7)
#'@keywords internal
index_arguments.rx<-function(climindvis,NAmaxAgg=20,trend=FALSE,rx = 3, NAmaxTrend=20,...){
  check_var(climindvis,"prec")
  return(list(ifun="minmax_value",var="prec",ifunargs=list(func=max, NAmaxAgg=NAmaxAgg, rx=rx),
    trend=trend,trendargs=list(method="lin_reg",count=FALSE,log_trans=TRUE,NAmaxTrend=NAmaxTrend, rel= TRUE),
    plotargs=list(iname=paste0("RX ",rx),iformat="mm")))
}


#'arguments for min/max of x days
#'@inheritParams varall
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@param rx Length of days to be considered. i.e. rx1day, rx5day, etc. Default set to 3.
#'@param fun Calculate min or max of rx days? Default = max.
#'@section Details:
#'For \emph{var}="prec" and \emph{fun}=max, this is equal to the function rx \code{\link{index_arguments.rx}}.
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="minmax_xdays",aggt="seasonal",rx=5,var="tmin",fun="min")
#' calc_index(object_st,index="minmax_xdays",aggt="seasonal",rx=5,var="tmax",fun="max")
#'@keywords internal
index_arguments.minmax_xdays<-function(climindvis,var,NAmaxAgg=20,trend=FALSE,rx =3 ,fun=max,NAmaxTrend=20,...){
  check_var(climindvis,var)
  var_units<-ifelse (var=="prec","mm","degreeC")
  if(!is.function(get(fun))) fun=match(fun)
  return(list(ifun="minmax_value",var=var,ifunargs=list(func=get(fun), NAmaxAgg=NAmaxAgg, rx=rx),
    trend=trend,trendargs=list(method="lin_reg",count=FALSE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel = check_trend(var)),
    plotargs=list(iname=paste0(fun," ",var," ",rx,"d"),iformat=var_units)))
}


# threshold ---------------------------------------------------------------
#'arguments for index th
#'@inheritParams varallth
#'@inheritParams index_arguments
#'@inheritParams iformat_doc
#'@inheritParams th_doc
#'@inheritParams trend_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="th",aggt="dates", start_days="0000-01-15",end_days="0000-04-15",threshold=-5,operator="<=",thvar="tmin",trend=TRUE)
#' @keywords internal
index_arguments.th<-function(climindvis,threshold,operator,thvar,iformat="perc",NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,fyear=FALSE,...){
  if(missing(climindvis) || missing(threshold) || missing(operator) ||missing(thvar)) stop("Not all mandatory arguments provided for calculation of index.")
  check_var(climindvis,thvar)
  return(list(ifun="ndays_op_threshold",var=thvar,ifunargs=list(threshold=threshold,op=operator,iformat=iformat,NAmaxAgg=NAmaxAgg),
              trend=trend,trendargs=list(method="logit_reg", count=ifelse(iformat=="perc",FALSE,TRUE), log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname=paste0(thvar,operator,threshold))))
}

#'arguments for index th_range
#'@inheritParams varallth
#'@inheritParams index_arguments
#'@inheritParams iformat_doc
#'@inheritParams th_doc
#'@inheritParams trend_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="th_range",aggt="seasonal",trend=TRUE,
#' threshold=-5,operator=">=",threshold2 = 5, operator2 = "<=", thvar="tmin")
#' @keywords internal
index_arguments.th_range<-function(climindvis,threshold,operator,threshold2,operator2,thvar,iformat="perc",NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,fyear=FALSE,...){
  if(missing(climindvis) || missing(threshold) || missing(operator) || missing(threshold2) || missing(operator2)||missing(thvar)) stop("not all mandatory arguments provided for calculation of index")
  check_var(climindvis,thvar)
  return(list(ifun="ndays_in_range",var=thvar,ifunargs=list(threshold=threshold,op=operator,threshold2=threshold2,op2=operator2,iformat=iformat,NAmaxAgg=NAmaxAgg),
              trend=trend,trendargs=list(method="logit_reg", count=ifelse(iformat=="perc",FALSE,TRUE), log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname=paste0(threshold,operator,thvar,operator2,threshold2))))
}

#'arguments for index fd
#'@inheritParams vartmin
#'@inheritParams index_arguments
#'@inheritParams iformat_doc
#'@inheritParams trend_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="fd",aggt="annual",iformat="days")
#'@keywords internal
index_arguments.fd<-function(climindvis,iformat="perc",NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,fyear=FALSE,...){
  check_var(climindvis,"tmin")
  return(list(ifun="ndays_op_threshold",var="tmin",ifunargs=list(threshold=0,op="<",iformat=iformat,NAmaxAgg=NAmaxAgg),
              trend=trend,trendargs=list(method="logit_reg", count=ifelse(iformat=="perc",FALSE,TRUE), log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname="frost days")))
}

#'arguments for index dd
#'@inheritParams varprec
#'@inheritParams index_arguments
#'@inheritParams iformat_doc
#'@inheritParams trend_doc
#'@inheritParams th_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="dd",aggt="monthly")
#'@keywords internal
index_arguments.dd<-function(climindvis,iformat="perc",NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,dd_threshold=1,fyear=FALSE,...){
  check_var(climindvis,"prec")
  return(list(ifun="ndays_op_threshold",var="prec",ifunargs=list(threshold=dd_threshold,op="<",iformat=iformat,NAmaxAgg=NAmaxAgg),
              trend=trend,trendargs=list(method="logit_reg", count=ifelse(iformat=="perc",FALSE,TRUE), log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname="dry days")))
}

#'arguments for index sud
#'@inheritParams vartmax
#'@inheritParams index_arguments
#'@inheritParams iformat_doc
#'@inheritParams trend_doc
#'@inheritParams th_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="dd",aggt="monthly")
#'@keywords internal
index_arguments.sud<-function(climindvis,iformat="perc",NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,dd_threshold=1,fyear=FALSE...){
  check_var(climindvis,"tmax")
  return(list(ifun="ndays_op_threshold",var="tmax",ifunargs=list(threshold=20,op=">",iformat=iformat,NAmaxAgg=NAmaxAgg),
    trend=trend,trendargs=list(method="logit_reg", count=ifelse(iformat=="perc",FALSE,TRUE), log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname="summer days")))
}


#'arguments for index sdii
#'@inheritParams varprec
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@inheritParams th_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="sdii",aggt="seasonal",selagg=c("SON"),dd_threshold=0.1)
#'@keywords internal
index_arguments.sdii<-function(climindvis,NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,dd_threshold=1,...){
  check_var(climindvis,"prec")
  return(list(ifun="simple_daily_intensity",var="prec",ifunargs=list(threshold=dd_threshold,op=">=", NAmaxAgg=NAmaxAgg),
    trend=trend,trendargs=list(method="lin_reg", count=FALSE, log_trans=TRUE,NAmaxTrend=NAmaxTrend, rel= TRUE),
    plotargs=list(iname="SDII",iformat="mm")))
}

#'arguments for index prcptot
#'@inheritParams varprec
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@inheritParams th_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="prcptot",aggt="other",aggmons=c(11:12,1:4))
#'@keywords internal
index_arguments.prcptot<-function(climindvis,NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,dd_threshold=1,...){
  check_var(climindvis,"prec")
  return(list(ifun="total_precip_op_threshold",var="prec",ifunargs=list(threshold=dd_threshold,op=">=", NAmaxAgg=NAmaxAgg),
              trend=trend,trendargs=list(method="lin_reg", count=FALSE, log_trans=TRUE,NAmaxTrend=NAmaxTrend, rel= TRUE),
              plotargs=list(iname="PRCPTOT",iformat="mm")))
}

#'arguments for index cdd
#'@inheritParams varprec
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@inheritParams th_doc
#'@param spells_span_agg Logical.Should spells starting before the chosen aggregation period be considered? Default=FALSE.
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="cdd",aggt="annual")
#'@section NOTE: if spells_span_agg=TRUE spells starting before the chosen aggregation period are considered. Spells starting in the chosen period and lasting until the next period however are not accounted for. Therefore it is not recommended to use this function for very short time periods as the returned results may not be very plausible. For years where all values within the chosen aggregation period are dry days, the function returns NA.
#'@keywords internal
index_arguments.cdd<-function(climindvis,NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,dd_threshold=1,spells_span_agg=FALSE,fyear=FALSE,...){
  check_var(climindvis,"prec")
  return(list(ifun="spell_length_max",var="prec",ifunargs=list(threshold=dd_threshold,op="<",NAmaxAgg=NAmaxAgg, spells_span_agg=spells_span_agg),
    trend=trend,trendargs=list(method="logit_reg",count=TRUE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname="CDD",iformat= "dayscount")))
}

#'arguments for index cwd
#'@inheritParams varprec
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@inheritParams th_doc
#'@param spells_span_agg Logical.Should spells starting before the chosen aggregation period be considered? Default=FALSE.
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="cwd",aggt="annual")
#'@section NOTE: if spells_span_agg=TRUE spells starting before the chosen aggregation period are considered. Spells starting in the chosen period and lasting until the next period however are not accounted for. Therefore it is not recommended to use this function for very short time periods as the returned results may not be very plausible. For years where all values within the chosen aggregation period are dry days, the function returns NA.
#'@keywords internal
index_arguments.cwd<-function(climindvis,NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,dd_threshold=1,spells_span_agg=FALSE,fyear=FALSE,...){
  check_var(climindvis,"prec")
  return(list(ifun="spell_length_max",var="prec",ifunargs=list(threshold=dd_threshold,op=">=",NAmaxAgg=NAmaxAgg, spells_span_agg=spells_span_agg),
    trend=trend,trendargs=list(method="logit_reg",count=TRUE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname="CWD",iformat= "dayscount")))
}

#'arguments for index cxd
#'@inheritParams varallth
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@inheritParams th_doc
#'@param spells_span_agg Logical.Should spells starting before the chosen aggregation period be considered? Default=FALSE.
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="cxd",aggt="annual",thvar="tmin",threshold=0,operator="<=")
#'@keywords internal
index_arguments.cxd<-function(climindvis,NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,thvar,threshold,operator,spells_span_agg=FALSE,fyear=FALSE,...){
  check_var(climindvis,thvar)
  return(list(ifun="spell_length_max",var=thvar,ifunargs=list(threshold=threshold,op=operator,NAmaxAgg=NAmaxAgg, spells_span_agg=spells_span_agg),
    trend=trend,trendargs=list(method="logit_reg",count=TRUE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname=paste0("continuous days (",thvar,operator,threshold,")"),iformat= "dayscount")))
}
# temperature-thresholds -------------------------------------------------------------------

#'arguments for index th_tmin
#'@inheritParams vartmin
#'@param threshold Threshold value in degrees C for  tmin < threshold
#'@inheritParams index_arguments
#'@inheritParams iformat_doc
#'@inheritParams trend_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="th_tmin",aggt="seasonal",threshold=-2)
#'@keywords internal
index_arguments.th_tmin<-function(climindvis,threshold,iformat="perc",NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,fyear=FALSE,...){
  if( missing(threshold) ) stop("<<threshold>> not provided>>")
  check_var(climindvis,"tmin")
  return(list(ifun="ndays_op_threshold",var="tmin",ifunargs=list(threshold=threshold,op="<",iformat=iformat,NAmaxAgg=NAmaxAgg),
              trend=trend,trendargs=list(method="logit_reg", count=ifelse(iformat=="perc",FALSE,TRUE), log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname=paste0("minimum temperature below " ,(threshold)))))
}

#'arguments for index th_tmax
#'@inheritParams vartmax
#'@param threshold Threshold value in degrees C for  tmax > threshold
#'@inheritParams index_arguments
#'@inheritParams iformat_doc
#'@inheritParams trend_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="th_tmax",aggt="monthly",threshold=20)
#'@keywords internal
index_arguments.th_tmax<-function(climindvis,threshold,iformat="perc",NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,fyear=FALSE,...){
  if( missing(threshold) ) stop("<<threshold>>not provided")
  check_var(climindvis,"tmax")
  return(list(ifun="ndays_op_threshold",var="tmax",ifunargs=list(threshold=threshold,op=">",iformat=iformat,NAmaxAgg=NAmaxAgg),
              trend=trend,trendargs=list(method="logit_reg", count=ifelse(iformat=="perc",FALSE,TRUE), log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname=paste0("maximum temperature above " ,(threshold)))))
}

#'arguments for index th_topt
#'@inheritParams vartavg
#'@param threshold Threshold value in degrees C for  threshold <= tavg <= threshold
#'@param threshold2 Threshold value in degrees C for  threshold <= tavg <= threshold2
#'@inheritParams index_arguments
#'@inheritParams iformat_doc
#'@inheritParams trend_doc
#' @keywords internal
index_arguments.th_topt<-function(climindvis,threshold,threshold2,iformat="perc",NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,fyear=FALSE,...){
  if( missing(threshold) ) stop("at least one threshold is missing, please provide <<threshold>> and <<threshold2>>")
  check_var(climindvis,"tavg")
  return(list(ifun="ndays_in_range",var="tavg",ifunargs=list(threshold=threshold,op=">=",threshold2=threshold2,op2="<=",iformat=iformat,NAmaxAgg=NAmaxAgg),
              trend=trend,trendargs=list(method="logit_reg", count=ifelse(iformat=="perc",FALSE,TRUE), log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname=paste0("optimum temperature range (", threshold,"-",threshold2,")"))))
}

#----quant values-----

check_qth_args<-function(indexname,th=FALSE){
  env=parent.frame()
  check_class(env$object,"climindvis_index")
  check_class(env$th_object,indexname)
  check_spatial_dims(list(env$climindvis,env$th_object))
  check_fcmon(list(env$climindvis,env$th_object))
  if(th){
    if (env$q_threshold !=env$th_object$index_info$qth*100) stop("q_th in th_object does not match argument <<q_threshold>>")
    env$q_threshold=env$th_object$index_info$qth*100
  }
  #env$q=env$th_object$index_info$q
  env$baseperiod=env$th_object$index_info$baseperiod
}

#'arguments for index qth
#'@inheritParams varallth
#'@inheritParams index_arguments
#'@inheritParams iformat_doc
#'@inheritParams trend_doc
#'@param operator One of ">",">=","<","<="
#'@param thvar Variable for which q_threshold should be calculated (tmin/tmax/tavg/prec)
#'@inheritParams quantile_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="qth",aggt="annual",thvar="tmin",q_threshold=5,operator=">=")
#'
#' ## for forecasts
#' data(object_hc_st, object_fc_st) # load example objects
#' ind_hc<-calc_index(object_hc_st,index="qth",aggt="monthly",thvar="tmin",q_threshold=5,operator=">=")
#' ind_fc<-calc_index(object_fc_st,index="qth",aggt="monthly",th_object=ind_hc,thvar="tmin",q_threshold=5,operator=">=")
#'@keywords internal
index_arguments.qth<-function(climindvis,thvar, NAmaxAgg=20,trend=FALSE ,NAmaxTrend=20,iformat="perc", baseperiod, q_threshold,n=5,inbase = TRUE,
                              min_base_fraction=0.1,operator,th_object=NULL,NAmaxQ=365,qens_all=TRUE,dd_threshold=1,fyear=FALSE,...){
  if(missing(climindvis) || missing(thvar) ||missing(q_threshold)|| missing(operator)) stop("Not all mandatory arguments provided for calculation of index.")
  if(!is.numeric(q_threshold)){q_threshold <- as.numeric(q_threshold)}

    check_var(climindvis,thvar)
  if (!is.null(th_object)){
    check_qth_args(indexname="qth",th=TRUE)
  } else {
    if(grepl("fc",climindvis$data_info$type)) stop ("For objects of type <<p_fc/grid_fc>> quantile thresholds have to be provided in th_object.")
    if (missing(baseperiod)) {
      baseperiod = c(as.numeric(substr(range(climindvis$time),1,4)))
      message("Baseperiod is missing. Full period used as baseperiod") }
    else {
      check_bp(climindvis,baseperiod)
      }
  }
  if(q_threshold<0 || q_threshold >100) stop("q_threshold value has to be between 0 and 100")
  if(thvar=="prec") {inbase <- FALSE}

  return(list(ifun="ndays_op_threshold",var=thvar,
    ifunargs=list(op=operator,iformat=iformat,NAmaxAgg=NAmaxAgg),
    qargs=list(qth=q_threshold/100,n=n,inbase=inbase,min_base_fraction=min_base_fraction,baseperiod = baseperiod,
               th_quantiles=th_object$index_info$th_quantiles,NAmaxQ=NAmaxQ,qens_all=qens_all,q_var=thvar, dd_threshold=dd_threshold),
    trend=trend,trendargs=list(method="logit_reg",count=TRUE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname=paste0(thvar,operator,q_threshold,"th percentile"))))
}

#'arguments for index tn10p
#'@inheritParams vartmin
#'@inheritParams index_arguments
#'@inheritParams iformat_doc
#'@inheritParams trend_doc
#'@inheritParams quantile_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="tn10p",aggt="other",aggmons=c(5:10),baseperiod=c(1985,1995))
#'
#' ## for forecasts
#' data(object_hc_st, object_fc_st) # load example objects
#' ind_hc<-calc_index(object_hc_st,index="tn10p",aggt="monthly")
#' ind_fc<-calc_index(object_fc_st,index="tn10p",aggt="monthly",th_object=ind_hc)
#'@keywords internal
index_arguments.tn10p<-function(climindvis,NAmaxAgg=20,trend=FALSE ,NAmaxTrend=20, iformat="perc",n=5 ,baseperiod,inbase = TRUE,
                                min_base_fraction=0.1,th_object=NULL,NAmaxQ=365,qens_all=TRUE,fyear=FALSE,...){
  if(missing(climindvis)) stop("climindvis object has to be provided for calculation of index")
  check_var(climindvis,"tmin")
  if (!is.null(th_object)){
  check_qth_args(indexname="tn10p",th=FALSE)
  } else {
    if(grepl("fc",climindvis$data_info$type)) stop ("for objects of type <<fc_p/fc_grid>> quantile thresholds have to be provided in th_object.")
    if (missing(baseperiod)) {
      baseperiod = c(as.numeric(substr(range(climindvis$time),1,4)))
      message("Baseperiod is missing. Full period used as baseperiod") }
    else {check_bp(climindvis,baseperiod)}
  }
    return(list(ifun="ndays_op_threshold",var="tmin",
    ifunargs=list(op="<",iformat=iformat,NAmaxAgg=NAmaxAgg),
    qargs=list(qth=(10/100),n=n,baseperiod = baseperiod, inbase=inbase, min_base_fraction = min_base_fraction,
               th_quantiles=th_object$index_info$th_quantiles,NAmaxQ=NAmaxQ,qens_all=qens_all, q_var="tmin"),
    trend=trend,trendargs=list(method="logit_reg",count=FALSE,  log_trans=TRUE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname="TN10p")))
}

#'arguments for index tn90p
#'@inheritParams vartmin
#'@inheritParams index_arguments
#'@inheritParams iformat_doc
#'@inheritParams trend_doc
#'@inheritParams quantile_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="tn90p",aggt="monthly",trend=TRUE,baseperiod=c(1981,2000))
#'
#' ## for forecasts
#' data(object_hc_st, object_fc_st) # load example objects
#' ind_hc<-calc_index(object_hc_st,index="tn90p",aggt="monthly")
#' ind_fc<-calc_index(object_fc_st,index="tn90p",aggt="monthly",th_object=ind_hc)
#'@keywords internal
index_arguments.tn90p<-function(climindvis,NAmaxAgg=20,trend=FALSE ,NAmaxTrend=20,iformat="perc",n=5, baseperiod,inbase = TRUE,
                                min_base_fraction=0.1, th_object=NULL,NAmaxQ=365,qens_all=TRUE,fyear=FALSE,...){
  if(missing(climindvis)) stop("climindvis object has to be provided for calculation of index")
  check_var(climindvis,"tmin")
  if (!is.null(th_object)){
    check_qth_args(indexname="tn90p",th=FALSE)
  } else {
    if(grepl("fc",climindvis$data_info$type)) stop ("For objects of type <<fc_p/fc_grid>> quantile thresholds have to be provided in th_object.")
    if (missing(baseperiod)) {
      baseperiod = c(as.numeric(substr(range(climindvis$time),1,4)))
      message("Baseperiod is missing. Full period used as baseperiod") }
    else {check_bp(climindvis,baseperiod)}
  }

  return(list(ifun="ndays_op_threshold",var="tmin",
    ifunargs=list(op=">",iformat=iformat,NAmaxAgg=NAmaxAgg),
    qargs=list(qth=(90/100),n=n,baseperiod = baseperiod, inbase=inbase, min_base_fraction = min_base_fraction,
               th_quantiles=th_object$index_info$th_quantiles,NAmaxQ=NAmaxQ,qens_all=qens_all, q_var="tmin"),
    trend=trend,trendargs=list(method="logit_reg",count=FALSE,  log_trans=TRUE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname="TN90p")))
}

#'arguments for index tx10p
#'@inheritParams vartmax
#'@inheritParams index_arguments
#'@inheritParams iformat_doc
#'@inheritParams trend_doc
#'@inheritParams quantile_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="tx10p",aggt="annual",baseperiod=c(1981,2000))
#'
#' ## for forecasts
#' data(object_hc_st, object_fc_st) # load example objects
#' ind_hc<-calc_index(object_hc_st,index="tx10p",aggt="monthly")
#' ind_fc<-calc_index(object_fc_st,index="tx10p",aggt="monthly",th_object=ind_hc)
#'@keywords internal
index_arguments.tx10p<-function(climindvis,NAmaxAgg=20,trend=FALSE ,NAmaxTrend=20,iformat="perc",n=5, baseperiod,inbase = TRUE,
                                min_base_fraction=0.1, th_object=NULL,NAmaxQ=365,qens_all=TRUE,fyear=FALSE,...){
  check_var(climindvis,"tmax")
  if (!is.null(th_object)){
    check_qth_args(indexname="tx10p",th=FALSE)
  } else {
    if(grepl("fc",climindvis$data_info$type)) stop ("for objects of type <<fc_p/fc_grid>> quantile thresholds have to be provided in th_object.")
    if (missing(baseperiod)) {
      baseperiod = c(as.numeric(substr(range(climindvis$time),1,4)))
      message("Baseperiod is missing. Full period used as baseperiod") }
    else {check_bp(climindvis,baseperiod)}
  }

  return(list(ifun="ndays_op_threshold",var="tmax",
    ifunargs=list(op="<",iformat=iformat,NAmaxAgg=NAmaxAgg),
    qargs=list(qth=(10/100),n=n,baseperiod = baseperiod, inbase=inbase, min_base_fraction = min_base_fraction,
               th_quantiles=th_object$index_info$th_quantiles,NAmaxQ=NAmaxQ,qens_all=qens_all, q_var="tmax"),
    trend=trend,trendargs=list(method="logit_reg",count=FALSE,  log_trans=TRUE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname="TX10p")))
}

#'arguments for index tx90p
#'@inheritParams vartmax
#'@inheritParams index_arguments
#'@inheritParams iformat_doc
#'@inheritParams trend_doc
#'@inheritParams quantile_doc
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="tx90p",aggt="annual",baseperiod=c(1981,2000))
#'
#' ## for forecasts
#' data(object_hc_st, object_fc_st) # load example objects
#' ind_hc<-calc_index(object_hc_st,index="tx90p",aggt="monthly")
#' ind_fc<-calc_index(object_fc_st,index="tx90p",aggt="monthly",th_object=ind_hc)
#'@keywords internal
index_arguments.tx90p<-function(climindvis,NAmaxAgg=20,trend=FALSE ,NAmaxTrend=20,iformat="perc",n=5, baseperiod,inbase = TRUE,
                                min_base_fraction=0.1, th_object=NULL,NAmaxQ=365,qens_all=TRUE,fyear=FALSE,...){
  check_var(climindvis,"tmax")
  if (!is.null(th_object)){
    check_qth_args(indexname="tx90p",th=FALSE)
  } else {
    if(grepl("fc",climindvis$data_info$type)) stop ("for objects of type <<fc_p/fc_grid>> quantile thresholds have to be provided in th_object.")
    if (missing(baseperiod)) {
      baseperiod = c(as.numeric(substr(range(climindvis$time),1,4)))
      message("Baseperiod is missing. Full period used as baseperiod") }
    else {check_bp(climindvis,baseperiod)}
  }

  return(list(ifun="ndays_op_threshold",var="tmax",
    ifunargs=list(op=">",iformat=iformat,NAmaxAgg=NAmaxAgg),
    qargs=list(qth=(90/100),n=n,baseperiod = baseperiod,inbase=inbase, min_base_fraction= min_base_fraction,
               th_quantiles=th_object$index_info$th_quantiles,NAmaxQ=NAmaxQ,qens_all=qens_all, q_var="tmax"),
    trend=trend,trendargs=list(method="logit_reg",count=FALSE,  log_trans=TRUE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname="TX90p")))
}

#'arguments for index rXptot
#'@inheritParams varprec
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@inheritParams quantile_doc
#'@param q_threshold Threshold of quantile, for example 95th, 98th, 99th quantile. Default=95.
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="rXptot",aggt="annual",q_threshold=99)
#'
#' ## for forecasts
#' data(object_hc_st, object_fc_st) # load example objects
#' ind_hc<-calc_index(object_hc_st,index="rXptot",aggt="monthly")
#' ind_fc<-calc_index(object_fc_st,index="rXptot",aggt="monthly",th_object=ind_hc)
#'@keywords internal
index_arguments.rXptot<-function(climindvis,NAmaxAgg=20,trend=FALSE , NAmaxTrend=20, q_threshold=95, operator=">", dd_threshold=1,baseperiod, th_object=NULL,NAmaxQ=365,qens_all=TRUE,...){

  check_var(climindvis,"prec")
  if (!is.null(th_object)){
    check_qth_args(indexname="rXptot",th=TRUE)
  } else {
    if(grepl("fc",climindvis$data_info$type)) stop ("for objects of type <<fc_p/fc_grid>> quantile thresholds have to be provided in th_object.")
    if (missing(baseperiod)) {
      baseperiod = c(as.numeric(substr(range(climindvis$time),1,4)))
      message("Baseperiod is missing. Full period used as baseperiod") }
    else {check_bp(climindvis,baseperiod)}
  }
  if(!is.numeric(q_threshold)){ q_threshold <- as.numeric(q_threshold)}
  if(q_threshold<0 || q_threshold >100) stop("q_threshold value has to be between 0 and 100")
  return(list(ifun="total_precip_op_threshold",var="prec",ifunargs=list(op=operator,NAmaxAgg=NAmaxAgg),
    qargs=list(qth=(q_threshold/100),dd_threshold = dd_threshold, baseperiod = baseperiod,inbase =FALSE,
               th_quantiles=th_object$index_info$th_quantiles,NAmaxQ=NAmaxQ,qens_all=qens_all, q_var="prec"),
    trend=trend,trendargs=list(method="lin_reg",count=FALSE,  log_trans=TRUE,NAmaxTrend=NAmaxTrend, rel= TRUE),
    plotargs=list(iname=paste0("r",q_threshold,"ptot"),iformat="mm")))

}

#'arguments for index cold spell duration index
#'@inheritParams vartmin
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@inheritParams quantile_doc
#'@param q_threshold Threshold of cold spell quantile. Default to 10th quantile.
#'@param min_length Mimimum length of cold spell. Default to 6 days
#'@param spells_span_agg Logical.Should spells starting before the chosen aggregation period be considered? Default=FALSE.
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="csdi",aggt="annual")
#'
#' ## for forecasts
#' data(object_hc_st, object_fc_st) # load example objects
#' ind_hc<-calc_index(object_hc_st,index="csdi",aggt="monthly")
#' ind_fc<-calc_index(object_fc_st,index="csdi",aggt="monthly",th_object=ind_hc)
#'@keywords internal
index_arguments.csdi<-function(climindvis,NAmaxAgg=20,trend=FALSE ,NAmaxTrend=20, q_threshold=10,n=5,min_length=6,spells_span_agg=FALSE,
                               baseperiod,inbase=FALSE,min_base_fraction=0.1, th_object=NULL,NAmaxQ=365,qens_all=TRUE,fyear=FALSE,...){
  check_var(climindvis,"tmin")
  if (!is.null(th_object)){
    check_qth_args(indexname="csdi",th=TRUE)
  } else {
    if(grepl("fc",climindvis$data_info$type)) stop ("for objects of type <<fc_p/fc_grid>> quantile thresholds have to be provided in th_object.")
    if (missing(baseperiod)) {
      baseperiod = c(as.numeric(substr(range(climindvis$time),1,4)))
      message("Baseperiod is missing. Full period used as baseperiod") }
    else {check_bp(climindvis,baseperiod)}
  }
  if(q_threshold<0 || q_threshold >100) stop("q_threshold value has to be between 0 and 100")
  return(list(ifun="spell_duration",var="tmin",
    ifunargs=list(func="min", op="<",min_length=6,spells_span_agg=spells_span_agg, NAmaxAgg=NAmaxAgg),
    qargs=list(qth=(q_threshold/100),n=n, baseperiod = baseperiod,inbase=inbase, min_base_fraction=min_base_fraction,
               th_quantiles=th_object$index_info$th_quantiles,NAmaxQ=NAmaxQ,qens_all=qens_all,q_var="min"),
    trend=trend,trendargs=list(method="logit_reg",count=FALSE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname="CSDI",iformat="dayscount")))
}

#'arguments for index warm spell duration index
#'@inheritParams vartmax
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@inheritParams quantile_doc
#'@param q_threshold of warm spell quantile. Default to 90th quantile.
#'@param min_length of warm spell definition. Default to 6 days
#'@param spells_span_agg Logical.Should spells starting before the chosen aggregation period be considered? Default=FALSE.
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="wsdi",aggt="annual",baseperiod=c(1981,2000))
#'
#' ## for forecasts
#' data(object_hc_st, object_fc_st) # load example objects
#' ind_hc<-calc_index(object_hc_st,index="wsdi",aggt="monthly")
#' ind_fc<-calc_index(object_fc_st,index="wsdi",aggt="monthly",th_object=ind_hc)
#'@keywords internal
index_arguments.wsdi<-function(climindvis,NAmaxAgg=20,trend=FALSE ,NAmaxTrend=20, q_threshold=90,n=5,min_length=6,spells_span_agg=FALSE,
                               baseperiod,inbase=FALSE,min_base_fraction=0.1, th_object=NULL,NAmaxQ=365,qens_all=TRUE,fyear=FALSE,...){
  check_var(climindvis,"tmax")
  if (!is.null(th_object)){
    check_qth_args(indexname="wsdi",th=TRUE)
  } else {
    if(grepl("fc",climindvis$data_info$type)) stop ("for objects of type <<fc_p/fc_grid>> quantile thresholds have to be provided in th_object.")
    if (missing(baseperiod)) {
      baseperiod = c(as.numeric(substr(range(climindvis$time),1,4)))
      message("Baseperiod is missing. Full period used as baseperiod") }
    else {check_bp(climindvis,baseperiod)}
  }
  if(q_threshold<0 || q_threshold >100) stop("threshold value has to be between 0 and 100")
  return(list(ifun="spell_duration",var="tmax",
    ifunargs=list( func="max", op=">",min_length=6,spells_span_agg=spells_span_agg,NAmaxAgg=NAmaxAgg),
    qargs=list(qth=(q_threshold/100),n=n, baseperiod = baseperiod,inbase=inbase, min_base_fraction=min_base_fraction,
               th_quantiles=th_object$index_info$th_quantiles,NAmaxQ=NAmaxQ,qens_all=qens_all, q_var="tmax"),
    trend=trend,trendargs=list(method="logit_reg",count=FALSE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
    plotargs=list(iname="WSDI",iformat="dayscount")))
}


#'arguments for index dry/wet spell duration
#'@inheritParams varprec
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@param spells_span_agg Logical.Should spells starting before the chosen aggregation period be considered? Default=FALSE.
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="csdi",aggt="annual")
#'
#' ## for forecasts
#' data(object_hc_st, object_fc_st) # load example objects
#' ind_hc<-calc_index(object_hc_st,index="csdi",aggt="monthly")
#' ind_fc<-calc_index(object_fc_st,index="csdi",aggt="monthly",th_object=ind_hc)
#'@keywords internal
index_arguments.spells<-function(climindvis,NAmaxAgg=20,dd_threshold=1,trend=FALSE,fun="mean",op=">" ,NAmaxTrend=20, n=5,spells_span_agg=FALSE, fyear=FALSE,...){
   check_var(climindvis,"prec")
  return(list(ifun="spell_length",var="prec",ifunargs=list(fun=fun,threshold=dd_threshold,op=op,NAmaxAgg=NAmaxAgg, spells_span_agg=spells_span_agg),
              trend=trend,trendargs=list(method="logit_reg",count=TRUE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE,fyear=fyear),
              plotargs=list(iname="spelllength",iformat= "dayscount")))}



# rainy season start------------------------------------------------------------

#'arguments for index rainy_season_start
#'@inheritParams varprec
#'@param rs_method: method for calculation of index (further indicator arguments for each rs_method are given in brackets and defined below, arguments without default that have to be provided are in bold font ):
#'\itemize{
#'    \item climandes (nval): wet day and precipitation sum(5days) > 8mm and maximum number of consecutive dry days within the next 30 days <=7 (definition from Sedlmeier et al.,submitted )
#'    \item jd (nval): 3 of 5 days > 0.1mm & precipitation sum(5days) > 25mm and maximum number of consecutive dry days  within the next 30 days >=7 (definition from Jolliffe and Dodd, 1994 )
#'
#'   \item garcia (nval): precipitation sum(3days) > 20mm and maximum number of consecutive dry days within the next 30 days <=10 (definition from Garcia et al.,2007 )
#'
#'   \item gurgiser (nval): wet day and precipitation sum(7days) > 10mm and number of consecutive wet days   within the next 30 days > 10 (definition from Gurgiser et al.,2017)
#'
#'   \item consec_th (\strong{days,th},nval): wet day and sum of user defined number of consecutive days (<<days>>) above user defined threshold (<<th>>) (e.g. sum of 4 days > 10mm)
#'
#'   \item consec_th_maxcdd (\strong{days,th,mdays,mcdd},nval): wet day and sum of user defined number of consecutive days (<<days>>) above user defined threshold (<<th>>)  and maximum number of consecutive dry days (<<mdays>>)  within a number of user defined days (<<mcdd>>)  (e.g. sum of 4 days > 10mm and maximum 6 consecutive dry days within the next 30 days)
#'
#'   \item sos (th1,th2,nval,NAdays): start of season defined by a threshold amount and distribution of rainfall received in three consecutive dacades (at least th1mm rainfall in first decade and at least th2mm rainfall in following 2 decades. Default values are th1=25mm,th2=20mm). See DETAILS section.
#'   }
#'@param nval Value which is returned if criteria for start of rainy season are not met, default=NA. Other values can be set to distinguish between years where data is NA and years where criteria of index are not met
#'@param days Number of consecutive days of which precipitation sum is higher than threshold <<th>>
#'@param th Threshold for precipitation sum of <<days>> consecutive days in mm
#'@param dd_th Dry day threshold
#'@param mcdd Maximum consecutive dry days in the next <<mdays>> days
#'@param mdays Number of days to check for <<mcdd>>
#'@param NAdays Number of NA days allowed per decade for index <<sos>>. Default=0.
#'@inheritParams trend_doc
#'@section DETAILS:
#'In this case of choosing <<sos>> decades are calculatet starting from start_date defined in the aggregtion.
#'The returned day is the last day of the decade, e.g. if the criterion is met in the second decade, the return value would be 20. The precipitation per decade is calculated by the mean(prec)*10 in order to avoid the influence of missing values. By using the parameter NAdays, the number of allowed missing values per decade can be defined. If the onset criteria are not met the value for the given year is set to NA.
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="rainy_season_start",aggt="dates",start_days="0000-08-01",end_days="0000-03-01",rs_method="gurgiser")
#'
#'@keywords internal
index_arguments.rainy_season_start<-function(climindvis,rs_method,days,th,th1=25,th2=20,dd_th=1,nval=NA,mdays,mcdd,trend=FALSE,NAmaxTrend=20,NAdays=0,...){
  check_var(climindvis,"prec")
  if (rs_method=="sos"){
    method_args=list(rs_method=rs_method,dd_th=dd_th,nval=nval,th1=th1,th2=th2,NAdays=NAdays)
    methodname=rs_method
  } else if (rs_method=="consec_th") {
    if(missing(days) || missing(th)) stop("at least one of the mandatory argumens( <<days>>,<<th>>) is missing")
    method_args=list(rs_method=rs_method,days=days,th=th,dd_th=dd_th,nval=nval)
    methodname=paste0(days,"daysum_gt",th)
  } else if (rs_method=="consec_th_maxcdd" ) {
    if(missing(days) || missing(th) ||missing(mdays) || missing(mcdd)) stop("at least one of the mandatory argumens( <<days>>,<<th>>,<<mdays>>,<<mcdd>>) is missing")
    method_args=list(rs_method=rs_method,days=days,th=th,dd_th=dd_th,mdays=mdays,mcdd=mcdd,nval=nval)
    methodname=paste0(days,"daysum_gt",th,"_",mcdd,"maxcdd_in",mdays,"days")
  } else {
    method_args=list(rs_method=rs_method,dd_th=dd_th,nval=nval)
    methodname=rs_method
  }

  return(list(ifun=paste0("rainy_season_start"),var="prec", ifunargs=method_args,trend=trend, trendargs=list(method="lin_reg", count=TRUE, log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE),
              plotargs=list(name="rainy_season_start",iname=paste0("rsstart-" ,methodname),iformat="days_since")))
}



#'arguments for index rainy_season_end
#'@inheritParams varprec
#'@param rs_method: method for calculation of index (further indicator arguments for each rs_method are given in brackets and defined below, arguments without default that have to be provided are in bold font ):
#'\itemize{
#'   \item climandes (nval): precipitation sum (30days) < 16mm (definition from Sedlmeier et al., submitted)
#'   \item garcia (nval): precipitation sum (20days) = 0 (definition from Garcia et al.,2007 )
#'   \item gurgiser (nval):  dry day and precipitation sum (46d) < 10 mm (definition from Gurgiser et al.,2017)
#'   \item th (\strong{days,th}, nval): dry day and precipitation sum (<<days>>) > <<th>> using user defined thresholds.
#'   }
#'@param th Threshold for precipitation sum of <<days>> consecutive days in mm
#'@param dd_th Dry day threshold
#'@param days Number of days to calculate precipitation sum
#'@param NAdays Number of NA days allowed per decade for index <<sos>>. Default=0.
#'@inheritParams trend_doc
#'@section DETAILS:
#'In case the criteria are not met for a given year, the value for that year is set to NA.
#'
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="rainy_season_end",aggt="dates",start_days="0000-02-01",end_days="0000-08-01",rs_method="gurgiser")
#'
#'@keywords internal

index_arguments.rainy_season_end<-function(climindvis,rs_method,days=NULL,dd_th=1,th=NULL,nval=NA,trend=FALSE,NAmaxTrend=20,NAdays=0,...){
  check_var(climindvis,"prec")
  method_args=list(rs_method=rs_method,dd_th=dd_th,nval=nval,th=th,days=days)
  methodname=rs_method

  return(list(ifun=paste0("rainy_season_end"),var="prec", ifunargs=method_args,trend=trend, trendargs=list(method="lin_reg", count=TRUE, log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE),
              plotargs=list(name="rainy_season_end",iname=paste0("rsend-" ,methodname),iformat="days_since",th=th,days=days)))
}



# rainy season ------------------------------------------------------------

#'arguments for index rainy_season_dur
#'@inheritParams varprec
#'@param rs_method: method for calculation of index (further indicator arguments for each rs_method are given in brackets and defined below, arguments without default that have to be provided are in bold font ):
#'\itemize{
#'   \item garcia (nval): sum(3days) > 20mm and maximum number of consecutive dry days within the next 30 days <=10 (definition from Garcia et al.,2007 )
#'
#'   \item gurgiser (nval): sum(7days) > 10mm and number of consecutive wet days   within the next 30 days > 10 (definition from Gurgiser et al.,2017)
#'   }
#'@param th Threshold for precipitation sum of <<days>> consecutive days
#'@param dd_th Dry day threshold
#'@param NAdays Number of NA days allowed per decade for index <<sos>>. Default=0.
#'@param end rs_method for end date if different than for start. For Default=NULL same method is used for end and for start.
#'@inheritParams trend_doc
#'@section DETAILS:
#'The duration of the rainy season is calculated using the same functions for the start and end of the rainy season. The arguments for the aggregations have to be provided differently. <<day start>> indicates the first day of the calculation for the start of the rainy season and <<day end>> the first day of the time series for calculating the end of the rainy season. nstart/nend are the number of days after day_start/day_end to consider for the calculation (defaults to 300days). If the criterion for start or end are not met the value of the respective year is set to NA.
#'
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="rainy_season_start",aggt="dates",start_days="0000-08-01",end_days="0000-03-01",rs_method="gurgiser")
#'
#'@keywords internal
index_arguments.rainy_season_dur<-function(climindvis,rs_method,days,dd_th=1,nval=NA,trend=FALSE,NAmaxTrend=20,NAdays=0,...){
  check_var(climindvis,"prec")
  method_args=list(rs_method=rs_method,dd_th=dd_th,nval=nval)
  methodname=rs_method

  return(list(ifun=paste0("rainy_season_dur"),var="prec", ifunargs=method_args,trend=trend, trendargs=list(method="lin_reg", count=TRUE, log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE),
              plotargs=list(name="rainy_season_dur",iname=paste0("rsdur-" ,methodname),iformat="days")))
}


#----spi-----
#'arguments for index spi
#'@inheritParams varprec
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@inheritParams spi_doc
#'@section Details:
#' The SPI calculation is based on the SCI package from Lukas Gudmundsson & James Stagge. For the SPI calculation, aggt needs to be set to "monthly". For the argument distribution, provide a distribution for which the corresponding density function (dname), the corresponding distribution function (pname) and the quantile function (qname) are defined (see for example GammaDist). Distributions are listed in the package stats.\cr
#' Note, that it is important, what distribution you choose for the transformation of your rainfall data. If you are not sure, consider the references below.
#'@references
#'Stagee, J.H. ; Tallaksen, L.M.; Gudmundsson, L.; van Loon, A.; Stahl, K.: Candidate Distributions for Climatological Drought Indices (SPI and SPEI), 2015, International Journal of Climatology, 35, 4027-4040, doi:10.1002/joc.4267.
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="spi",aggt="monthly")

#'@keywords internal
index_arguments.spi<-function(climindvis,timescale=6,ref=NULL,distribution="gamma",limit=Inf, trend=FALSE,NAmaxAgg=20,forecast=FALSE,NAmaxTrend=20,...){
  if(missing(climindvis)) stop("Not all mandatory arguments provided")
  check_var(climindvis,"prec")
  return(list(ifun="calc_spi",var="prec",ifunargs=list(distribution=distribution,param=FALSE,timescale=timescale,ref= ref,NAmaxAgg=NAmaxAgg,limit=limit, iformat="SPI"),
              trendargs=list(method="lin_reg", count=FALSE,log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel= TRUE),trend=trend,
              plotargs=list(iname=paste0("SPI",timescale))))
}

#'arguments for index spi_forecast
#'@inheritParams varprec
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@inheritParams spi_doc
#'@param fc_p Climindvis object with forecast data.

#'@section Details:
#'The SPI calculation is based on the SCI package from Lukas Gudmundsson & James Stagge. For integrating forecasts into the SPI plot, the timeseries of forecast and historical data are checked for the overlapping period. For this period a mean
#'@references
#'Stagee, J.H. ; Tallaksen, L.M.; Gudmundsson, L.; van Loon, A.; Stahl, K.: Candidate Distributions for Climatological Drought Indices (SPI and SPEI), 2015, International Journal of Climatology, 35, 4027-4040, doi:10.1002/joc.4267.
#'@keywords internal
index_arguments.spi_forecast<-function(climindvis,fc_p, timescale=6,ref=NULL,distribution="gamma", trend=FALSE,trendargs, NAmaxAgg=20,...){
  if(missing(climindvis)) stop("Not all mandatory arguments provided")
  check_var(climindvis,"prec")
  check_var(fc_p,"prec")

  return(list(ifun="calc_spi",var="prec",ifunargs=list(distribution=distribution,param=TRUE,timescale=timescale, ref,NAmaxAgg=NAmaxAgg,iformat="SPI"),
              trendargs=list(method="lin_reg", count=FALSE,log_trans=FALSE,rel= TRUE),trend=trend,
              plotargs=list(iname=paste0("SPI",timescale, ref))))
}






#'arguments for index qval
#'@inheritParams qth
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@section DETAILS:
#'This index calculated the  percentile of each aggregation time period and year seperately.
#'@examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="qval",aggt="seasonal",percentile=5, var="tmin")
#'@keywords internal
index_arguments.qval<-function(climindvis,NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,percentile,var,...){
  check_var(climindvis,var)
  return(list(ifun="q_agg",var=var,ifunargs=list(q1=percentile/100,q2=NULL, NAmaxAgg=NAmaxAgg),
    trend=trend,trendargs=list(method="lin_reg",count=FALSE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel=FALSE),
    plotargs=list(iname=paste0(var,"_q",percentile),iformat=ifelse(var=="prec","mm","degreeC"))))
}

#'arguments for index qrange
#'@inheritParams qth
#'@inheritParams index_arguments
#'@inheritParams trend_doc
#'@section DETAILS:
#'This index calculated the difference between the two percentile values for each aggregation and year seperately, e.g. in the case of the 25th and the 75th percentile the interquantile range is calculated.
#' @examples
#' data(object_st) # load example objects
#' calc_index(object_st,index="qrange",aggt="seasonal",precentile1=25,percentile2=75,var="tmax")
#'@keywords internal
index_arguments.qrange<-function(climindvis,NAmaxAgg=20,trend=FALSE,NAmaxTrend=20,percentile1,percentile2,var,...){
  check_var(climindvis,var)
  return(list(ifun="q_agg",var=var,ifunargs=list(q1=percentile1/100,q2=percentile2/100, NAmaxAgg=NAmaxAgg),
    trend=trend,trendargs=list(method="lin_reg",count=FALSE,  log_trans=FALSE,NAmaxTrend=NAmaxTrend, rel=FALSE),
    plotargs=list(iname=paste0(var,"_q",percentile1,"_q",percentile2),iformat=ifelse(var=="prec","mm","degreeC"))))
}
