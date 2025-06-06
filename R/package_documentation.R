#' ClimIndVis
#'
#' Functions for calculation of climate indices and their trends as well as  verification (for seasonal forecasts) and different wrapper functions for visualization.

#'@section Introduction:
#'
#'The ClimIndVis package provides a set of easy-to-use functions for the generation of climate indices products for observational, reanalysis and seasonal forecast data. \cr
#' The \strong{main features} of the package are:
#' \itemize{
#' \item Calculation of indices (most ETCCDI indices (partly modified), threshold indices, consecutive threshold indices, SPI, rainy season indices)
#' \item Calculation of trends
#' \item Verification of seasonal forecasts
#' \item Visualizations of results (as time series, maps,  or seasonal forecast graphics)
#' }
#'
#'The \strong{special feature} of the package are a set of wrapper functions (called \emph{autoplot_...}) which combine all of the above functionalities in one function. These are designed so that users with little R knowledge can easily produce a set of figures.
#'
#' The package works with daily data, \strong{gridded data} as well as \strong{station data}. This could be station observations or gridded observational datasets, reanalysis datasets and seasonal forecast ensembles (gridded or interpolated to stations).\cr
#'  For large datasets the computational time can be quite high, an issue for which improvement is planned.
#'
#' @section Getting started:
#'
#'\enumerate{
#'  \item \strong{generate a climindvis object} from your data - this is needed for all functions of the climindvis package.
#'  For this you need to:
#'    \itemize{
#'      \item read your data into R
#'      \item bring your data into the input format needed for the function \code{\link{make_object}}
#'      \item use the function \code{\link{make_object}}  to generate a climindvis object
#'    }
#'   The package contains some example data (\code{\link{example_data}}) with more information about the needed data format and some example climindvis-objects of the same data for the use the package functions (\code{\link{example_climindvis_objects}}). \cr
#' \item \strong{calculate indices or use one of the autoplot functions:}
#'   \itemize{
#'     \item calculate indices by using the function \code{\link{calc_index}} and then process the indices with your own function or in the case of seasonal forecasts use the \code{\link{verify_index}}, \code{\link{calc_fc_cats}} functions to verifiy your seasonal forecasts of indices or calculate forecast category probabilities. \cr
#'     \item use one of the autoplot functions listed below which directly generate figures. For example graphics check out the vignette  "autoplot-functions" accesible through the package help main page or by the following command: \cr

#'     \emph{vignette("autoplot-functions",package="ClimIndVis")}
#'       \itemize{
#'         \item \code{\link{autoplot_ts_stations}}
#'        \item \code{\link{autoplot_overview_stations}}
#'        \item \code{\link{autoplot_climatology_map}}
#'        \item \code{\link{autoplot_trend_map}}
#'        \item \code{\link{autoplot_verification_map}}
#'        \item \code{\link{autoplot_forecast_map}}
#'        \item \code{\link{autoplot_forecast_stations}}
#'        }
#'   }
#'}
#'@section Outlook next version:
#'
#'In an updated version (planned for late 2018) several additional features are planned:
#'
#'\itemize{
#'\item additional plotting functions/options
#'\item speed up of calculation time
#'\item option to write output to file
#'}
#'If you notice any bugs or have general comments or suggestions concerning the package, please send an email to the authors.
#'
#'@section Acknowledgements:
#'The package was developed within the CLIMANDES project, a project funded by the Swiss Agency for Developement and Cooperation (SDC) and coordinated by the World Meteorological Organization (WMO).
#
#' @docType package
#' @name ClimIndVis
NULL

#' @useDynLib ClimIndVis
#' @importFrom Rcpp sourceCpp
NULL


