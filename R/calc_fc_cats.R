#' Get probabilities of forecast categories
#'
#' Calculates index for hindcasts and forecasts and then derives category thresholds from hindcasts and calculates forecast probabilities for each category.
#'The function works with gridded or station data (all input data needs to be of same type) and first calculates the chosen index (using \code{\link{calc_index}}) for hindcasts and forecasts and then divides the forecast into categories (e.g. terciles for ncat=3) based on the climatology of the hindcasts. It outputs the forecast probabilities for each category as well as the category thresholds.
#'
#' @inheritParams plot_doc
#' @inheritParams veri_doc
#' @param counts Logical. Should number of ensemble members per category be returned instead of probabilities. Default=FALSE
#'
#' @return Object of class \emph{index} and "climindvis_fc" with following entries:
#' \itemize{
#'   \item cprob/ccount: Array of forecast probabilities/counts for each category with named dimensions
#'   \item {cpinfo:} {Named list with additional information. Absolute category threshold values from hindcasts (\emph{prob_th}) and the corresponding probability tresholds (\emph{prob}), number of cateogories (\emph{ncat}) and dimensions and dimension names of cprob (\emph{cpdims},\emph{cpdimnames}).}
#'   \item lon: Array of longitudes
#'   \item lat: Array of latitudes
#'   \item index_info: Named list with information on index such as index name and aggregation type
#'   \item {data_info$fc and $hc :}{Data_info taken from fc and hc input objects, e.g. name of dataset}
#' }
#'@section Note:
#'This function either returns the forecast probabilities (\emph{cprob}) or, if \emph{counts} is set to TRUE, the number of ensemble members per category (\emph{ccount}) . Forecast probabilities are calculated as follows:  \emph{ccounts}/number of ensemble members *100.
#'These values are rounded to the nearest integer, therefore adding the forecast probabilities of all categories may not always add up to 100.
#'Note that for some forecast the year dimension might be greater than one, e.g. for a November forecast and monthly temporal aggregation, the first two months (Nov. + Dez) are from another year than the following months.
#' @examples
#'
#' ## Load example climindvis objects:
#'
#' data("object_hc_st", "object_fc_st")
#'
#' calc_fc_cats(fc = object_fc_st, hc = object_hc_st, index = "tnn", index_args = list(aggt="seasonal"))
#'
#'
#' data("object_hc_grid", "object_fc_grid")
#'
#' calc_fc_cats(fc = object_fc_grid, hc = object_hc_grid, index = "tnn", index_args = list(aggt="seasonal"))
#'
#'
#' @export

calc_fc_cats<-function(
  fc, hc,
  index, index_args = list(), selyears = NULL,prob,ncat=3,counts=FALSE) {



  #1. check input--------------------------------------------------
  tryCatch({
      if(missing(fc) | missing (hc)) stop("fc and hc need to be passed to function")
      if(missing(index)) stop("index needs to be specified")
  }, error=function(cond){
    message("error in input data:")
    message(cond)
    stop_quietly()
  })

  #2. get and check data--------------------------------------------------
  dat<-list(fc,hc)
  check_same_type(dat)
  points=ifelse(grepl("p",dat[[1]]$data_info$type),1,0)
  grid=ifelse(grepl("grid",dat[[1]]$data_info$type),1,0)
  suffix=ifelse(points==1,"p","grid")
  names(dat) = list(paste0("fc_",suffix),paste0("hc_",suffix))

  check_class(dat[!sapply(dat,is.null)],"climindvis")
  check_spatial_dims(dat[!sapply(dat,is.null)])
  check_fcmon(dat[!sapply(dat,is.null)])
  switch(is_index_special(index)+1,selyears_data<-selyears,selyears_data<-NULL)
  dat<-cut_to_same_dates(dat,selyears_data)
  #4. calculate index --------------------------------------------------
  if(missing(index_args)) index_args=list()
  index_args$trend = FALSE
  if(!is_index_special(index)){
    ind_dat<-lapply(dat, function(dd) do.call("calc_index",c(list(dd,index=index),index_args)))
  } else {
    ind_dat<-calc_index_special_autoplot(dat,index,index_args, selyears)
  }


  # 3. calc category values -------------------------------------------------
  if(missing(prob)) {
    prob= 1:(ncat-1)/ncat
  } else ncat=length(prob)+1

  chelp=get_cats_fc(ind_dat,grid,points,probs=prob,prob=ifelse(counts,FALSE,TRUE),ret_th=TRUE)[[suffix]]

  iinfo=ind_dat[[1]]$index_info
  cdims=c(iinfo$idims[-which(iinfo$idims=="ens")],"cat")
  dimnames(chelp$val) = c(dimnames(ind_dat[[1]]$index)[-which(iinfo$idims=="ens")],list(paste0("cat",1:ncat)))

  iinfo$idims <- NULL
  if(counts){ cats=list(ccounts=chelp$val,cpinfo=list(cpdims=cdims,cpdimnames=dimnames(chelp),prob=prob,ncat=ncat,prob_th=chelp$q_hc),index_info=iinfo)
    } else {cats=list(cprob=chelp$val,cpinfo=list(cpdims=cdims,cpdimnames=dimnames(chelp),prob=prob,ncat=ncat,prob_th=chelp$q_hc),index_info=iinfo)
    }
  cats[c("lon","lat")]<-fc[c("lon","lat")]
  cats[["data_info"]][["fc"]]<-fc$data_info
  cats[["data_info"]][["hc"]]<-hc$data_info
  class(cats) = c(index,"climindvis_fc")
 return(cats)

}



