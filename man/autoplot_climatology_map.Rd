% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot_climatology_map.R
\name{autoplot_climatology_map}
\alias{autoplot_climatology_map}
\title{Wrapper function to calculate and visualize climatology of indices}
\usage{
autoplot_climatology_map(
  dat_grid,
  dat_p,
  index,
  index_args = list(),
  plot_value = "mean",
  perc = NULL,
  selyears = NULL,
  NAmaxClim = 20,
  col = "default",
  title = "",
  plot_title = TRUE,
  plot_args = list(),
  output = NULL,
  plotdir,
  plotname = ""
)
}
\arguments{
\item{dat_grid}{Climindvis-object with type = "grid" generated by \code{\link{make_object}}.}

\item{dat_p}{Climindvis-object with type = "p" generated by \code{\link{make_object}}.}

\item{index}{Name of index to be calculated., e.g. index = "dd" or index = "tnn". For list of indices see \code{\link{indices_list}}}

\item{index_args}{List of arguments for index. See \code{\link{indices_list}}.}

\item{plot_value}{Climatological value to plot. One of the following character strings: "mean", "median", "min", "max", "sd" (=standard deviation), or "relsd" (sd/abs(mean)) and "percentile". For percentile, the parameter perc has to be defined, also see parameter perc. Default = "mean".}

\item{perc}{Integer value between 0 and 100 for percentile to be calculated. Only if plot_value="percentile".}

\item{selyears}{Integer array of years to be selected from data. For indices with quantiles, years as defined in index_args are used for quantile calculation, but only selected years are plotted. For functions using seasonal forecast data, selyears is only applied to hindcasts.}

\item{NAmaxClim}{Integer [0,100]. Maximum percentage of years with missing values for calculation of climatology. If exceeded value for respective grid point/station is set to NA. Default = 20 ( = 20"\%").}

\item{col}{Array of colors for plotting of length 3 or longer (must be a valid arguments to \code{\link[grDevices]{col2rgb}}). Length of array is adjusted to number of levels. If not provided, package default colors for index are used. Default="default".}

\item{title}{Optional. Character string. If provided added above the plot in addition to default information from the respective function (e.g. index name/aggregation/time period/...). Default = "", no additional title is written.}

\item{plot_title}{Logical. Add plot title to graphic. Default = TRUE.}

\item{plot_args}{Optional. List of arguments passed to plotting function, see \code{\link{plot_args_map}}.}

\item{output}{Format of image to be plotted ("png", "jpeg", "tiff", "pdf","dev.new").  Default = NULL, output to null device (getOption("device") will show you the null device. If you are not using RStudio, NULL and dev.new will give the same output).}

\item{plotdir}{Character string of path of directory for saving plots. Ignored if  \emph{output} is NULL or "dev.new".}

\item{plotname}{Optional. Character string. First part of plot name followed by character string containing index name/aggregation/time period/... depending on the autoplot function. Default = "".}
}
\description{
This function calls the function  \code{\link{calc_index}} and plots the mean, median, min, max or standard deviation of the index values over the whole time series in a map. It can be used for either gridded or station data or a combination of both. \cr
To see some example outputs of this function, look at the vignette "autoplot-functions" accesible through the package help main page or by typing \cr
 \emph{vignette("autoplot-functions",package="ClimIndVis")} into the console.
}
\section{Output}{

This function returns one plot per temporal aggregation whch is covered by the data( e.g. if aggt="seasonal" it would return 4 graphics, one for each season).

If output is not NULL or "dev.new" the graphics are directly saved in the user specified directory (\emph{plotdir}) with the following filename structure: \cr

    \emph{plotname} // index name // plot_value // aggregation // years // \emph{output} \cr e.g.: \cr
    \emph{plotname}"_dry_days_mean_MAM_1981-2010.png \cr
}

\section{Details}{


Please note that calculating the mean does not make sense for indices based on the number of days above/below a quantile threshold (e.g. tn10p,tn90p)

When using ensemble data, the plot value is calculated over the whole ensemble (i.e. ensemble mean/min/max/... over all ensemble members and the whole time period).
}

\examples{

data(object_st, object_grid)
autoplot_climatology_map(
      dat_grid = object_grid, dat_p = object_st,
      index = "tnn", index_args = list(aggt = "seasonal", selagg = "JJA"),
      plot_value = "min" ,  selyears = c(2000:2010))

data(object_hc_grid)
index_args=list(aggt = "dates", start_days = "0000-01-15", end_days = "0000-04-15")
autoplot_climatology_map(
      dat_grid = object_hc_grid,
      index = "dd", index_args = index_args,
      plot_value = "min" , selyears = c(2000:2010))

}
\seealso{
Other autoplot_functions: 
\code{\link{autoplot_anomaly_map}()},
\code{\link{autoplot_anomaly_ts}()},
\code{\link{autoplot_forecast_map}()},
\code{\link{autoplot_forecast_spi}()},
\code{\link{autoplot_forecast_stations}()},
\code{\link{autoplot_overview_stations}()},
\code{\link{autoplot_trend_map}()},
\code{\link{autoplot_ts_stations}()},
\code{\link{autoplot_verification_map}()}
}
\concept{autoplot_functions}
