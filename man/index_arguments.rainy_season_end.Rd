% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_indices_get_arguments.R
\name{index_arguments.rainy_season_end}
\alias{index_arguments.rainy_season_end}
\title{arguments for index rainy_season_end}
\usage{
\method{index_arguments}{rainy_season_end}(
  climindvis,
  rs_method,
  days = NULL,
  dd_th = 1,
  th = NULL,
  nval = NA,
  trend = FALSE,
  NAmaxTrend = 20,
  NAdays = 0,
  ...
)
}
\arguments{
\item{climindvis}{Object of type climindvis as returned by \code{\link{make_object}} which must contain the variable precipitation (prec)}

\item{days}{Number of days to calculate precipitation sum}

\item{dd_th}{Dry day threshold}

\item{th}{Threshold for precipitation sum of <<days>> consecutive days in mm}

\item{trend}{Logial (or character). Calculates trends with a predefined method for the chosen index. For information on default method, see  \code{\link{trend_methods}}. You can chose to calculate a non-parameteric trend, by setting trend to a character string "MannKendall". This calculates a TheilSen slope and MannKendall test. Default = FALSE.}

\item{NAmaxTrend}{Maximum number of years in time series of index which are missing values. Value between 0 (=0\%); no years with missing values allowed) and 100 (=100\%); all years can be missing values). If exceeded, the trend is set to NA for the respective grid point/station. Note that the value is relative to the length of the time series. Default = 20 (20\%).}

\item{NAdays}{Number of NA days allowed per decade for index <<sos>>. Default=0.}

\item{rs_method:}{method for calculation of index (further indicator arguments for each rs_method are given in brackets and defined below, arguments without default that have to be provided are in bold font ):
\itemize{
  \item climandes (nval): precipitation sum (30days) < 16mm (definition from Sedlmeier et al., submitted)
  \item garcia (nval): precipitation sum (20days) = 0 (definition from Garcia et al.,2007 )
  \item gurgiser (nval):  dry day and precipitation sum (46d) < 10 mm (definition from Gurgiser et al.,2017)
  \item th (\strong{days,th}, nval): dry day and precipitation sum (<<days>>) > <<th>> using user defined thresholds.
  }}
}
\description{
arguments for index rainy_season_end
}
\section{DETAILS}{

In case the criteria are not met for a given year, the value for that year is set to NA.
}

\examples{
data(object_st) # load example objects
calc_index(object_st,index="rainy_season_end",aggt="dates",start_days="0000-02-01",end_days="0000-08-01",rs_method="gurgiser")

}
\keyword{internal}
