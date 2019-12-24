# documentation --------------------------------------
#'Function to calculate climate indices
#'
#'The function calculated climate indices for different temporal aggregations for gridded and station data. It also works for seasonal forecast ensembles (grid and station). As input a climindvis object (output of \code{\link{make_object}} is needed.
#'
#'@param climindvis Object of type climindvis as returned by \code{\link{make_object}}
#'@param index Name of index to be calculated, e.g. "dd","tnn","spi". (for a complete list of indices see see section "indices" below).
#'@param aggt Aggregation type for temporal aggregation:
#' \itemize{
#' \item "annual": annual aggregation
#' \item "seasonal": seasonal aggregation ("MAM","JJA","SON","DJF"), define subset as character array using parameter \strong{selagg}, e.g. selagg=("DJF","MAM") for selecting seasons DJF and MAM only.
#' \item "monthly": Monthly aggregation. For subset of months, define subset as integer array using parameter \strong{selagg}, e.g. c(1:3) for monthly aggregation of Jan,Feb and March
#' \item "other": aggregation over all months defined as integer array in \strong{aggmons}, e.g. c(1:4) aggregates over Jan-Apri.
#' \item "dates": Aggregation of user defined dates defined in \strong{start_days} and \strong{end_days}. start_days and end days are arrays of format date or character(yyyy-mm-dd). If start and end dates are the same for all years they can also be specified as "0000-mm-dd", e.g. "0000-01-01".
#'\item "xdays": Aggregations over time slices of \strong{xday} days, e.g. for xday=7, weekly aggregation. If the parameters  \strong{start}  and/or  \strong{end} are provided, time slices are only calculated between the two dates. If you want weekly aggregations you thus have to set the starting date to a Monday. This aggregation is not implemented for seasonal forecast data and if small xdays are chosen (e.g. 7 or 10) we recommend to use it only for short time periods. Please note that at the moment the autoplot functions do not work with this aggregation type.
#' }
#'@param ... for  index specific arguments see links in section "indices" below.
#'
#'@return The function returns an S3 object of class climindvis_index with the following entries:
#'\itemize{
#'\item index Array of index values of dimension: spatial dimensions x (ensemble members) x aggregations x years
#'\item index_info List with information about the indices (name (@@iname), @@aggt, names of aggregations (@@aggnames), @@years, index format (@@iformat), index dimensions (@@idims)) used for plotting functions.
#'\item lon Array of longitudes
#'\item lat Array of latitudes
#'\item index_trend Array of dimension spatial dimension(s) x (ensemble) x aggregation x 4. The dimensions of the last dimension are 1: fit of trend estimate,  2: lower confidence interval, 3: upper confidence interval and 4:fit of loess-curve.
#'\item trend_info Array of dimension spatial dimension(s) x (ensemble) x aggregation x 6. The dimensions of the last dimension are the 1:start year, 2: end year, 3. p-value, 4. relative trend, 5. absolute trend and 6. the method used to calculate the trend. Methods are numeric and correspond to (1) Least Squares Fit, (2) Logistic Regression and (3) MannKendall.
#'}
#'@section Details:
#'
#'Note that indices are only calculated if all days of an aggregation period are available in the time series . If e.g. the dataset only contains data for Jan-May and aggt="seasonal" is chosen, the index will only be calculated for the season MAM. If the aggregation period spans years, the index will not be calculated for the last year.
#'@section Indices:
#'
#'The following indexs can be calculated with the ClimIndVis package. For index specific arguments and examples, see links.
#'
#'Threshold Indices:
#' \itemize{
#'   \item \strong{dd} Dry days (Prec < 1mm). For arguments and default values see \code{\link{index_arguments.dd}}
#'   \item \strong{fd} Frost days (Tmin<0C). For arguments and default values see \code{\link{index_arguments.fd}}
#'   \item \strong{th_tmin} Days with minimum temperature below a user defined threshold (Tmin < threshold). For arguments and default values see  \code{\link{index_arguments.th_tmin}}
#'   \item \strong{th_tmax} Days with maximum temperature above a user defined threshold (Tmax > threshold). For arguments and default values see  \code{\link{index_arguments.th_tmax}}
#'   \item \strong{th_topt} Days with average temperature within a user defined temperature range (threshold <= Tavg <= threshold2). For arguments and default values see  \code{\link{index_arguments.th_topt}}
#'   \item \strong{th} Threshold function for days below/above a user defined threshold (Variable >/>=/</<= threshold). For arguments and default values see \code{\link{index_arguments.th}}
#'   \item \strong{th_range} Threshold function for days within a user defined range  (threshold >/>=/</<= variable >/>=/</<= threshold2). For arguments and default values see \code{\link{index_arguments.th_range}}

#'   }
#'Minimum-Maximum Indices:
#'\itemize{
#'   \item \strong{tnn} Minimum value of daily minimum temperature. For arguments and default values see  \code{\link{index_arguments.tnn}}
#'   \item \strong{tnx} Maximum value of daily minimum temperature. For arguments and default values see  \code{\link{index_arguments.tnx}}
#'   \item \strong{txn} Minimum value of daily maximum temperature. For arguments and default values see  \code{\link{index_arguments.txn}}
#'   \item \strong{txx} Maximum value of daily maximum temperature. For arguments and default values see  \code{\link{index_arguments.txx}}
#'   \item \strong{varmin} Minimum value of user defined variable. For arguments and default values see  \code{\link{index_arguments.varmin}}
#'   \item \strong{varmax} Maximum value of user defined variable. For arguments and default values see  \code{\link{index_arguments.varmin}}
#'   \item \strong{rx} Maximum rx-day precipitation. \code{\link{index_arguments.rx}}
#'   \item \strong{minmax_xdays} Maximum/minimum rx-day value of variable. For arguments and default values see  \code{\link{index_arguments.minmax_xdays}}
#'   }
#'Spell-Duration Indices:
#'\itemize{
#'   \item \strong{cwd} Consecutive wet days. For arguments and default values see  \code{\link{index_arguments.cwd}}
#'   \item \strong{cdd} Consecutive dry days. For arguments and default values see  \code{\link{index_arguments.cdd}}
#'   \item \strong{cxd} Consecutive days of variable above/below a threshold. For arguments and default values see  \code{\link{index_arguments.cxd}}
#'   \item \strong{csdi} Cold spell duration index. Count of days with at least 6 consecutive days when TN < 10th percentile. For arguments and default values see  \code{\link{index_arguments.csdi}}
#'   \item \strong{wsdi} Warm spell duration index. Count of days with at least 6 consecutive days when TX > 90th percentile. For arguments and default values see  \code{\link{index_arguments.wsdi}}
#'   }
#'Quantile Indices:
#'\itemize{
#'   \item \strong{tn10p} Percentage of days when TN < 10th percentile. For arguments and default values see  \code{\link{index_arguments.tn10p}}
#'   \item \strong{tn90p} Percentage of days when TN > 90th percentile.For arguments and default values see  \code{\link{index_arguments.tn90p}}
#'   \item \strong{tx10p} Percentage of days when TX < 10th percentile.For arguments and default values see  \code{\link{index_arguments.tx10p}}
#'   \item \strong{tx90p} Percentage of days when TX > 90th percentile.For arguments and default values see  \code{\link{index_arguments.tx90p}}
#'   \item \strong{qth} Number of days above/below thershold. (works for precip and temperature data).  For arguments and default values see  \code{\link{index_arguments.qth}}
#'   \item \strong{rXptot} Total precipitation when RR > Xp, e.g. (r95ptot). For arguments and default values see  \code{\link{index_arguments.rXptot}}
#'    \item \strong{qval}  Percentile value for each aggregation period and year. For arguments and default values see  \code{\link{index_arguments.qval}}
#'     \item \strong{qrange} Percentile range for each aggregation period and year. For arguments and default values see  \code{\link{index_arguments.qrange}}
#'   }
#'further Indices:
#'\itemize{
#'  \item \strong{mean} For arguments and default values see  \code{\link{index_arguments.mean}}
#'   \item \strong{sum} For arguments and default values see  \code{\link{index_arguments.sum}}
#'   \item \strong{spi} Standard precipitation index. Argument \emph{aggt} needs to be set to \emph{monthly} for this index.  For more arguments and default values see  \code{\link{index_arguments.spi}}
#'   \item \strong{spi_forecast}  Argument \emph{aggt} needs to be set to \emph{monthly} for this index.  For more arguments and default values see  \code{\link{index_arguments.spi_forecast}}
#'   \item \strong{sdii} Simple precipitation intensity index. Ratio of total rainfall to the number of days when precipitation is higher than a dry day threshold. For arguments and default values see  \code{\link{index_arguments.sdii}}
#'   \item \strong{prcptot} Total precipitation in wet days. For arguments and default values see  \code{\link{index_arguments.prcptot}}
#'   \item \strong{rainy_season_start} Start of the rainy season. Argument \emph{aggt} needs to be set to \emph{dates} and \emph{start_days} and \emph{end_days} additionally provided for this index.  For more arguments and default values see  \code{\link{index_arguments.rainy_season_start}}
#'   #'   \item \strong{rainy_season_end} End of the rainy season. Argument \emph{aggt} needs to be set to \emph{dates} and \emph{start_days} and \emph{end_days} additionally provided for this index.  For more arguments and default values see  \code{\link{index_arguments.rainy_season_end}}
#'   #'   \item \strong{rainy_season_dur} Duration of the rainy season. Argument \emph{aggt} needs to be set to \emph{dates} and \emph{day_start} and \emph{day_end} additionally provided for this index.  For more arguments and default values see  \code{\link{index_arguments.rainy_season_dur}}

#'     }
#'@export
#'@seealso \code{\link{calculate_index.climindvis}}
#'
# functions ----------------------------------------------
calc_index <- function(climindvis, index, aggt, ...) {

    if (is(climindvis,"climindvis")==FALSE) stop ("Function needs climindvis object.")



    if(missing(aggt)) stop("aggt needs to be provided")
    aggt=check_aggt(index,aggt,climindvis$data_info$type)

    class(climindvis) <- append(index,"climindvis")

    if (!exists(paste0("index_arguments.",index),mode="function")){
      stop("Error: chosen index does not exist. Please check index list.")
      stop_quietly()
    }

    climindvis_index<- do.call("calculate_index", list(climindvis,aggt,...))

    climindvis_index[c("lon","lat","data_info")]<-climindvis[c("lon","lat","data_info")]
    #if(!is.null(write)) write_data(climindvis_index, write)

    class(climindvis_index) <- c(index,"climindvis_index")

    return(climindvis_index)

}
# calculate_index generic functions ---------------------------------------
calculate_index <- function (climindvis,...){
  UseMethod("calculate_index")
}

calculate_index.default<- function (climindvis,...){
  warning ("no function for class or index is not class of climindvis object")
}

#' default index function for climindvis objects
#' @param climindvis climindvis object generated using the function make_object
#' @seealso \link{make_object}
#' @param args List of arguments for calculation of index.  Arguments depend on type of index, for more information see documentation of calculate_index
#' @family calculate_index
#' @seealso \link{calc_index}
#' @export
#' @keywords internal

calculate_index.climindvis <- function(climindvis,aggt,...){

  args=list(aggt=aggt,...)

  arguments=c(list(climindvis=climindvis),args)

  iargs<-do.call(index_arguments,arguments)

  #
  sel_time<-get_date_factors(climindvis,args$aggt,args$aggmons,args$selagg,args$start_days,args$end_days,args$start,args$end,args$xdays)

  #iargs$trendargs$numdays <- sel_time$numdays # for logit calculation 
  d=dim(climindvis$data[[iargs$var]])
  ld=length(d)

  dat_help <- array(climindvis$data[[iargs$var]],dim=c(prod(d[1:(ld-1)]),d[ld]))


  if(is_index_special(class(climindvis)[1])){
    if (!is.null(args$th_object)){
      if (any(args$th_object$index_info$aggnames != sel_time$aggnames)) stop("aggregations of th_object and selected aggregation type do not match or data covers different time steps.")

    }
    quants<-get_quants_new(climindvis,var=iargs$var,qargs=iargs$qargs,sel_time)
    if (grepl("hc|fc",climindvis$data_info$type)) iargs$qargs$inbase=FALSE
    if(iargs$qargs$inbase){
      iout<-sapply(1:dim(dat_help)[1],
                 function(x) do.call(iargs$ifun,c(list(temp=dat_help[x,],jdays=climindvis$time_factors$jdays, q_temp=quants$base[,x],q_temp_inbase=quants$inbase[,,x],
                                                   date_factor=sel_time$tfactor),iargs$ifunargs, iargs$qargs)))
    } else {
      iout<-sapply(1:dim(dat_help)[1],
                   function(x) do.call(iargs$ifun,c(list(temp=dat_help[x,],jdays=climindvis$time_factors$jdays, q_temp=quants$base[,x],
                                                         date_factor=sel_time$tfactor),iargs$ifunargs, iargs$qargs)))
    }

    iargs$qargs$th_quantiles=quants$base
  } else {
    iout <- apply(dat_help,1,
                  function(x) do.call(iargs$ifun,c(list(temp=x,date_factor=sel_time$tfactor),iargs$ifunargs)))
  }

  ireturn<-list()
  ireturn$index_info <- c(iargs$ifunargs,iargs$plotargs,iargs$qargs,list(aggt=ifelse(args$aggt=="xdays",paste0(args$xdays,"days"),args$aggt),aggnames=sel_time$aggnames,years=unique(substr(levels(sel_time$tfactor),1,4))))

  if (args$aggt=="xdays"){
    ihelp=rearange_time(iout,levels(sel_time$tfactor))
  } else  ihelp <-rearange_by_year(iout,levels(sel_time$tfactor))
  ireturn$index<-array(ihelp,c(d[1:(ld-1)],dim(ihelp)[-1]),dimnames=c(rep(list(NULL),ld-1),dimnames(ihelp) [-1]))
  ireturn$index_info$idims<-get_idims(climindvis,args)


  if(grepl("fc",climindvis$data_info$type)) iargs$trend=FALSE

  if (iargs$trend != FALSE){
     itrend <- calc_index_trend(index=ireturn$index,iargs$trend, targs=iargs$trendargs)
    ireturn$index_trend <-  itrend$data
    ireturn$trend_info <- itrend$summary
  }

  if (args$aggt=="dates") ireturn$index_info<-c(ireturn$index_info,list(start_days=args$start_days,end_days=args$end_days))
  return(ireturn)

}


calculate_index.rainy_season_dur <- function(climindvis,aggt,...){

  args=list(aggt=aggt,...)
 if(is.null(args$nstart)) args$nstart=300
 if(is.null(args$nend)) args$nend=300
  arguments=c(list(climindvis=climindvis),args)

  iargs<-do.call(index_arguments,arguments)

  #"
  args$start_days=args$day_start
  args$end_days=paste0("0000",substring(as.Date(paste0("1981",substring(args$day_start,5,10)))+args$nstart,5,10))
  sel_time_start<-get_date_factors(climindvis,args$aggt,args$aggmons,args$selagg,args$start_days,args$end_days,args$start,args$end,args$xdays)
  args$start_days=args$day_end
  args$end_days=paste0("0000",substring(as.Date(paste0("1981",substring(args$day_end,5,10)))+args$nend,5,10))
  sel_time_end<-get_date_factors(climindvis,args$aggt,args$aggmons,args$selagg,args$start_days,args$end_days,args$start,args$end,args$xdays)

  d=dim(climindvis$data[[iargs$var]])
  ld=length(d)

  dat_help <- array(climindvis$data[[iargs$var]],dim=c(prod(d[1:(ld-1)]),d[ld]))

  smon=substring(args$day_start,6,7)
  emon=substring(args$day_end,6,7)
  if (emon>=smon){
    diff= as.integer(as.Date(paste0("1981",substring(args$day_end,5,10)))-as.Date(paste0("1981",substring(args$day_start,5,10))))
  } else {
    diff= as.integer(as.Date(paste0("1982",substring(args$day_end,5,10)))-as.Date(paste0("1981",substring(args$day_start,5,10))))
    test=sel_time_end
    levels(test$tfactor) = as.character(as.numeric(levels(test$tfactor))-1)
  }

    istart <- apply(dat_help,1,
      function(x) do.call("rainy_season_start",c(list(temp=x,date_factor=sel_time_start$tfactor),iargs$ifunargs)))
    iend <- apply(dat_help,1,
      function(x) do.call("rainy_season_end",c(list(temp=x,date_factor=sel_time_end$tfactor),iargs$ifunargs)))
if ((emon<smon)) {
     dimnames(iend)[[1]]= as.character(as.numeric(dimnames(iend)[[1]])-1)
     sel=match(dimnames(iend)[[1]],dimnames(istart)[[1]])

}
  
 iout=iend[!is.na(sel),]+diff-istart[sel[!is.na(sel)],]

 if(!is.null(args$nval)){
   iend_sel = iend[!is.na(sel),]
   istart_sel =istart[sel[!is.na(sel)],]
   ind = (iend_sel==args$nval | istart_sel==args$nval)
   iout[ind] = args$nval
 }
 
 
  ireturn<-list()
  ireturn$index_info <- c(iargs$ifunargs,iargs$plotargs,iargs$qargs,list(aggt=ifelse(args$aggt=="xdays",paste0(args$xdays,"days"),args$aggt),aggnames=paste0("start_",smon,substring(args$day_start,9,10),"_end_",emon,substring(args$day_end,9,10)),years=dimnames(iout)[[1]]))

  ihelp <-rearange_by_year(iout,dimnames(iout)[[1]])
  ireturn$index<-array(ihelp,c(d[1:(ld-1)],dim(ihelp)[-1]),dimnames=c(rep(list(NULL),ld-1),dimnames(ihelp) [-1]))
  ireturn$index_info$idims<-get_idims(climindvis,args)


  if(grepl("fc",climindvis$data_info$type)) iargs$trend=FALSE

  if (iargs$trend != FALSE){
    itrend <- calc_index_trend(index=ireturn$index,iargs$trend, targs=iargs$trendargs)
    ireturn$index_trend <-  itrend$data
    ireturn$trend_info <- itrend$summary
  }

  if (args$aggt=="dates") ireturn$index_info<-c(ireturn$index_info,list(start_days=args$start_days,end_days=args$end_days))
  return(ireturn)

}




