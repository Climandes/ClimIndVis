% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoplot_forecast_point.R
\name{autoplot_forecast_stations}
\alias{autoplot_forecast_stations}
\title{Wrapper function for visualizing forecast for each station (optionally including skill information and climatology)}
\usage{
autoplot_forecast_stations(
  fc_p,
  hc_p,
  hc_ref_p = NULL,
  obs_p = NULL,
  index,
  index_args = list(),
  selyears = NULL,
  verify = TRUE,
  veri_metric,
  veri_args = list(),
  skillmin = NULL,
  text_noskill = "no skill",
  text_novar = "no or little variation in climatology",
  text_nocat = "no dominant category",
  col = "default",
  prob,
  ncat = 3,
  cat_names = paste0("cat", 1:ncat),
  plot_climatology = FALSE,
  plot_title = TRUE,
  pwidth = NULL,
  pheight = NULL,
  output = NULL,
  plotdir,
  plotname = "",
  title = ""
)
}
\arguments{
\item{fc_p}{Forecasts at stations. Climindvis-object with type = "fc_p" generated by \code{\link{make_object}}.}

\item{hc_p}{Hindcasts at stations. Climindvis-object with type = "hc_p" generated by \code{\link{make_object}}.}

\item{hc_ref_p}{Optional. Reference hindcasts at stations for calculation of skill scores. If not provided (NULL), climatology is taken as reference. Default = NULL.}

\item{obs_p}{Station observations. Climindvis-object with type = "p" generated by \code{\link{make_object}}. For autoplot_forecast_map and autoplot_forecast_point only needed if skill criterion is applied.}

\item{index}{Name of index to be calculated., e.g. index = "dd" or index = "tnn". For list of indices see \code{\link{indices_list}}}

\item{index_args}{List of arguments for index. See \code{\link{indices_list}}.}

\item{selyears}{Integer array of years to be selected from data. For indices with quantiles, years as defined in index_args are used for quantile calculation, but only selected years are plotted. For functions using seasonal forecast data, selyears is only applied to hindcasts.}

\item{verify}{Logical. Should skill criterion be applied?}

\item{veri_metric}{Skill metric to calculate, see \code{\link{verify_index}}.}

\item{veri_args}{Optional list of additional arguments for verification, see \code{\link{veri_args}}}

\item{skillmin}{Minimum skill limit for which forecasts are shown. Default=NULL (no skill criterion applied).}

\item{text_noskill}{Character string. Text to show if forecast skill is lower than noskill. Default = "no skill".}

\item{text_novar}{Character string. Text if climatology shows no or little variation and plotting of forecast does not make sense. Default = "no or little variation in climatology".}

\item{text_nocat}{Character string. Text if forecast shows no dominant forecast category (=probabilities of two or more cateogries are equal). Default = "no dominant category".}

\item{col}{Plot colors, array of same length as ncat. If not provided, package default colors for index are used.}

\item{prob}{Probability thresholds for converting continuous forecasts to category forecasts. If not provided then ncat equidistant categories are calulated: (1:(ncat-1))/ncat).}

\item{ncat}{Number of forecast categories. Default = 3.}

\item{cat_names}{Names of forecast categorys to be used for plot. Default = paste0("cat",1:ncat)}

\item{plot_climatology}{Logical. Should climatology of index be plottet as time series? If TRUE, time series with shading indicating corresponding climatological values for forecast categories are plottet. Default = FALSE.}

\item{plot_title}{Logical. Add plot title to graphic. Default = TRUE.}

\item{pwidth}{Optional, only used if output is not NULL. Width of plot, default=5. Only considered if output!=NULL}

\item{pheight}{Optional, only used if output is not NULL. Height of plot, default=3.}

\item{output}{Format of image to be plotted ("png", "jpeg", "tiff", "pdf","dev.new").  Default = NULL, output to null device (getOption("device") will show you the null device. If you are not using RStudio, NULL and dev.new will give the same output).}

\item{plotdir}{Character string of path of directory for saving plots. Ignored if  \emph{output} is NULL or "dev.new".}

\item{plotname}{Optional. Character string. First part of plot name followed by character string containing index name/aggregation/time period/... depending on the autoplot function. Default = "".}

\item{title}{Optional. Character string. If provided added above the plot in addition to default information from the respective function (e.g. index name/aggregation/time period/...). Default = "", no additional title is written.}
}
\description{
This function generates plots showing the forecast probabilities of category forecasts of indices at station level as a tacho plot. The dominant forecast category is marked by an arrow.\cr
 The forecast categories (e.g. terciles) are determined based on the climatology of the hindcasts. Additionally a skill criterion can be applied and forecasts with skill lower than a minimum skill value are overlayed by a message which can be edited by the user.\cr
To see some example outputs of this function, look at the vignette "autoplot-functions" accesible through the package help main page or by typing \cr
\emph{vignette("autoplot-functions",package="ClimIndVis")} into the console.
}
\section{Output}{

This function returns one plot per temporal aggregation which is covered by the hindcasta and forecasts (e.g. if aggt="seasonal" and the forecast is issued in Jan for 7 months it would return one graphic for "MAM").

If output is not NULL or "dev.new" the graphics are directly saved in the user specified directory (\emph{plotdir}) with the following filename structure: \cr

    \emph{plotname} // "forecast_" // indexname // (skill_criterion) // aggregation //   forecast_year // forecast_month // hindcast_years // station name //\emph{output}\cr  e.g. \cr
    \emph{plotname}"_forecast_dry_days_MAM_2018_fcmon01_hc1981-2010_station2.png \cr
     \emph{plotname}"_forecast_dry_days_EnsRocss>=0.3_MAM_2018_fcmon01_hc1981-2010_station2.png
}

\section{Details}{


The shaded area of the tacho plot is proportional to the probability of the respective categeory, e.g. an empty tacho slice means 0\% probability, a full slice denotes 100\%. The dominant forecast category is marked by an arrow. \cr

If the forecast does not show a dominant forecast category (e.g. if the probabilites for a tercile forecast are cat1:35\%, cat2:35\%, cat3:30\%) no arrow will be drawn and the plot will be overlayed by a message (default \emph{text_nocat}="no dominant category").\cr

If the forecast for the chosen index shows no or little variation in climatology issuing a forecast does not make much sense. Therefore the function checks whether the category boundaries derived from the hindcast are identical and if so the plot is also overlayed by a message (default \emph{text_novar}="no or little variation in climatology). \cr

Optionally a skill criterion can be applied where the plot is overlayed by a message if the skill of the forecast is lower than the user defined minimum skill (default \emph{text_noskill} = "no skill").\cr

 The labels of the categorie can be changed using the parameter \emph{cat_names}\cr.
If \emph{plot_climatology}=TRUE, the function calls the function autoplot_ts_stations (see \code{\link{autoplot_ts_stations}}) and a seperate plot will be created with the cliamtology and tercile boundaries for each station. In the future, the possibility of plotting both climatology and forecast into one plot will be implemented in the package.
}

\examples{

## load example data:
data(object_st, object_hc_st, object_fc_st)

autoplot_forecast_stations(
      fc_p = object_fc_st, hc_p = object_hc_st, obs_p = object_st,
      index = "fd",  index_args = list(aggt = "other", aggmons = c(2:5)),
      verify = FALSE)

## if you want to additionally plot the climatology,  you have to set plot_climatology = TRUE:
autoplot_forecast_stations(
      fc_p = object_fc_st, hc_p = object_hc_st, obs_p = object_st,
      index = "fd",  index_args = list(aggt = "other", aggmons = c(2:5)),
      verify = FALSE, plot_climatology = TRUE)

## for output to file add e.g. output = "png", plotdir = "...directory for plotting..."

}
\seealso{
Other autoplot_functions: 
\code{\link{autoplot_anomaly_map}()},
\code{\link{autoplot_anomaly_ts}()},
\code{\link{autoplot_climatology_map}()},
\code{\link{autoplot_forecast_map}()},
\code{\link{autoplot_forecast_spi}()},
\code{\link{autoplot_overview_stations}()},
\code{\link{autoplot_trend_map}()},
\code{\link{autoplot_ts_stations}()},
\code{\link{autoplot_verification_map}()}
}
\concept{autoplot_functions}
