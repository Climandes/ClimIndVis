% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_indices_get_arguments.R
\name{index_arguments.rXptot}
\alias{index_arguments.rXptot}
\title{arguments for index rXptot}
\usage{
\method{index_arguments}{rXptot}(
  climindvis,
  NAmaxAgg = 20,
  trend = FALSE,
  NAmaxTrend = 20,
  q_threshold = 95,
  operator = ">",
  dd_threshold = 1,
  baseperiod,
  th_object = NULL,
  NAmaxQ = 365,
  qens_all = TRUE,
  ...
)
}
\arguments{
\item{climindvis}{Object of type climindvis as returned by \code{\link{make_object}} which must contain the variable precipitation (prec)}

\item{NAmaxAgg}{Maximum number of missing values in daily data of each aggregation period (e.g. in all January values for the whole time series). Value between 0 (=0\%; no missing values allowed) and 100 (=100\%; all values can be missing values). If the number of missing values is higher than NAmaxAgg the index value for the respective grid point/station is set to NA. Default = 20 (20\%).}

\item{trend}{Logial (or character). Calculates trends with a predefined method for the chosen index. For information on default method, see  \code{\link{trend_methods}}. You can chose to calculate a non-parameteric trend, by setting trend to a character string "MannKendall". This calculates a TheilSen slope and MannKendall test. Default = FALSE.}

\item{NAmaxTrend}{Maximum number of years in time series of index which are missing values. Value between 0 (=0\%); no years with missing values allowed) and 100 (=100\%); all years can be missing values). If exceeded, the trend is set to NA for the respective grid point/station. Note that the value is relative to the length of the time series. Default = 20 (20\%).}

\item{q_threshold}{Threshold of quantile, for example 95th, 98th, 99th quantile. Default=95.}

\item{baseperiod}{Vector of start and end year for the calculation of quantiles. If not provided, whole range of years in dataset will be used for the calculation of quantiles.}

\item{th_object}{Climindvis_index object, default=NULL. if calculating a quantile index for forecast data, a climindvis_index object of hindcasts data for the same spatial dimensions and forecast months has to be provided. Threshold values will then be taken from the hindcast object (object$index_info$quantiles). In this case all other arguments (q, baseperiod, threhold,..) are ignored and taken from the climindvis_index object. In case of autoplot functions, these values are handed over automatically and th_object should be left at default value of NULL..}

\item{NAmaxQ}{Minimim number of days needed for calculation of quantiles. Default = 365 [days]. For monthly aggregation of precipitation, consider lowering the threshold or expanding the baseperiod.}
}
\description{
arguments for index rXptot
}
\examples{
data(object_st) # load example objects
calc_index(object_st,index="rXptot",aggt="annual",q_threshold=99)

## for forecasts
data(object_hc_st, object_fc_st) # load example objects
ind_hc<-calc_index(object_hc_st,index="rXptot",aggt="monthly")
ind_fc<-calc_index(object_fc_st,index="rXptot",aggt="monthly",th_object=ind_hc)
}
\keyword{internal}
