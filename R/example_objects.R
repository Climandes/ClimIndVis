#' Objects for using functions of the Climindvis package
#'
#' Climindvis objects as returned by the function \code{\link{make_object}} with the data of \code{\link{example_data}} for using the functions of the package. All objects contain daily values of tmin,tmax and prec data for the years 1981-2010.
#' @docType data
#'
#' @usage data("object name")
#'
#' @name example_climindvis_objects
#' @section Available objects:
#' \itemize{
#'   \item object_st Object of type "p" with station data for 4 stations.
#'   \item object_grid Object of type "grid" with gridded data for a 3x4 grid
#'   \item object_hc_st Object of type "p_hc" with hindcast data for 4 stations and 15 ensemble members. The hindcasts are initialized 1.January for the years 1981-2010 and are issued for the following 214 days.
#'   \item object_hc_grid Object of type "grid_hc"  with hindcast data for a 3x4 grid and 15 ensemble members. The hindcasts are initialized 1.January for the years 1981-2010 and are issued for the following 214 days.
#'   \item object_fc_st Object of type "p_fc" with forecast data for 4 stations and 15 ensemble members. The forecasts are initialized 1.January 2010 and are issued for the following 214 days.
#'   \item object_fc_grid Object of type "grid_fc" with forecast data for a 3x4 grid and 15 ensemble members. The forecasts are initialized 1.January 2010 and are issued for the following 214 days.
#' }
#' @family data_examples
#' @examples
#' ## To use the example objects you first have to load it using data():
#'
#' data(object_st)
#'
#' data(object_hc_grid,object_fc_grid)
#'
#' @source artificial datasets based on SENAMHI station data \url{www.senamhi.gob.pe} and ECMWF seasonal forecast data  \url{www.ecmwf.int}
NULL

