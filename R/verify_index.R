#'Function for verification of hindcasts of indices
#'
#'Verifies daily seasonal hindcasts (re-forecasts) of indices for gridded or station data. It is based on the packages \code{\link{easyVerification}} and \code{\link{SpecsVerification}}.
#'
#'
#'@inheritParams plot_doc
#'@param veri_metrics Character array of verification metrics as defined in the \code{\link{easyVerification}} package, e.g. c("EnsCorr","EnsRpss"). Also see "Verification metrics" below.
#'@param hc_ref Reference forecast for calculation of skill scores. "climindvis"  object of type hc_p/hc_grid as output from \code{\link{make_object}} sing an ensemble of  daily seasonal hindcasts .  If hc_ref=NULL climatology will be taken as reference, Default = NULL.
#' @return Object of class \emph{index} and "climindvis_verification" with following entries:
#' \itemize{
#' \item verification: Named list with verification outputs for each metric in \emph{veri_metric}.
#' \item veri_info: Information needed for plotting of verification results, such as limits and dimensions of metric
#' \item years: years used for verification
#' \item lon: Array of longitudes
#' \item lat: Array of latitudes
#' \item {data_info$obs and $hc ($hc_ref):}{Data_info taken from obs and hc input objects, e.g. name of dataset}
#' }
#'
#' @section Cateogry forecasts:
#'
#' For category skill scores (e.g. ROC area and Rps(s)) forecasts have to be provided in categories. The easyVerification package automatically converts continuous forecast to category forecasts. The optional parameter \emph{veri_args$ncat}  determines the number of equidistant forecast categories (e.g. 3:terciles, 4:quartiles, 5:quintiles). This corresponds to setting the variable \emph{prob} of \code{\link{easyVerification}{veriApply}} to prob=c(1:((ncat-1)/ncat)). However, if you want to use other probability thresholds you can use the argument \emph{veri_args$prob}. The use of absolute thresholds (as possible in \code{\link{easyVerification}{veriApply}}) is currently not implemented.
#'
#' @section Verification metrics:
#'
#' The verify_index function can be used of calculating all of the skill metrics defined in the packages \code{\link{easyVerification}}
#' and \code{\link{SpecsVerification}}. Skill scores ending in "ss" denote the corresponding skill scores (see below).
#'Currently the following verification metrics from the \code{\link{easyVerification}} package can be calculated (the ending "ss" denotes the corresponding skill scores):
#' \describe{
#'  \item{Ens2AFC}{Generalized Discrimination Score}
#'  \item{EnsCorr}{Correlation with Ensemble Mean}
#'  \item{EnsMae/EnsMaess}{Ensemble mean absolute error}
#'  \item{EnsMse/EnsMsess}{Ensemble mean squared error}
#'  \item{EnsRmse/EnsRmsess}{Square root of ensemble mean squared error}
#'  \item{EnsRoca/EnsRocss}{ Area Under the ROC Curve}
#'  \item{EnsSprErr}{Spread to Error Ratio}
#'  \item{FairSprErr}{ Fair Spread to Error Ratio}
#'  \item{EnsRps/EnsRpss}{ Ranked probability skill score}
#'  \item{FairRps/FairRpss}{Fair Ranked probability skill score}
#'  \item{EnsCrps/EnsCrpss}{Continuous Ranked probability skill score}
#'  \item{FairCrps/FairCrpss}{Fair Continuous Ranked probability skill score}
#' }
#'More information about the skill scores can be found in the respective package.
#'
#'Skill scores are defined as (mean score - mean score for reference) / (perfect score - mean score for reference).  The skill score is zero if the mean score of the forecast equals the mean score of the reference forecast, and equals one if the mean score of the forecast equals the best possible score. If hc_ref is not defined, the climatology is taken as reference.
#'
#' For more information about Fair scores, see e.g. Ferro, C. A. T. (2014). Fair scores for ensemble forecasts. Quarterly Journal of the Royal Meteorological Society, 140(683), 1917-1923.
#'
#' @seealso \code{\link{easyVerification}},  \code{\link{calc_index}}
#' @examples
#'
#' ## load example hindcast and observational data for stations
#' ## (see \code{\link{example_climindvis_objects}}):
#'
#' data("object_hc_st", "object_st")
#'
#' ## verify hindcasts of indices using the skill metrics
#' ## "EnsRmsess", "EnsRpss", "EnsCorr":
#'
#' veri<-verify_index(obs = object_st, hc = object_hc_st, index="txx", index_args=list(aggt="seasonal") , veri_metrics = c("EnsRmsess", "EnsRpss", "EnsCorr"))
#'
#'

#' @export
#'
#'

verify_index <-function(
  obs, hc, hc_ref = NULL,
  index, index_args = list(), selyears = NULL,
  veri_metrics, veri_args = list()) {


  #check_input
  tryCatch({
    if (missing(veri_metrics)) stop("veri_metrics need to be passed to function")
    if(missing(obs) | missing (hc)) stop("obs and hc need to be passed to function")
    if (missing(index)) stop("index needs to be defined in function")
  })


  #3. get and check data--------------------------------------------------
  dat<-list(obs=obs,hc=hc)
  if(!is.null(hc_ref)) dat[["hc_ref"]]=hc_ref
  check_same_type(dat)
  switch(is_index_special(index)+1,selyears_data<-selyears,selyears_data<-NULL)
  dat<-cut_to_same_dates(dat,selyears_data)
  #checks
  #check class
  check_class(dat[!sapply(dat,is.null)],"climindvis")
  #check spatial dims
  check_spatial_dims(dat[!sapply(dat,is.null)])
  #check if forecast month is equal for all hc
  check_fcmon(dat[!sapply(dat,is.null)])
  #if !is.null hc_ref compute only skill scores
  veri_metrics<-skill_scores_only(dat,veri_metrics)


  #4. calculate index --------------------------------------------------
  index_args$trend = FALSE

  ind_dat<-lapply(dat, function(dd) do.call("calc_index",c(list(dd,index=index),index_args)))

  if(is_index_special(index) & !is.null(selyears)){
    ind_dat<-cut_to_same_dates_index(ind_dat,selyears)
  }

  #5. . Verifikation --------------------------------------------------
  # verification function only uses matching years and aggregations

  veri<-do.call("verify_climindvis_index",c(list(obs=ind_dat$obs, hc=ind_dat$hc, hc_ref=ind_dat$hc_ref, veri_metrics), veri_args))

  veri$same = NULL
  return(veri)

}




