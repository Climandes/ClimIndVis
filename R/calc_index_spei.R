## calc_indices_spi

#' spi calculations
#'@param temp vector of daily precipitation data to calculate SPI
#'@param date_factor factors of dates
#'@param forecast logical; Returns SPI parameters of historical timeseries to transform forecast data. Default = FALSE
#'@param NAmaxAgg number of days to check for <<mcdd>>
#'@keywords internal
#'@inheritParams spi_doc
calc_spi <- function (temp = NULL,date_factor, param=FALSE,timescale , ref, distribution, NAmaxAgg = 20,iformat, limit=4)
{
  stopifnot(is.numeric(temp) && is.numeric(timescale) && is.character(distribution))
  if (is.null(ref)){
    refdat <- temp
    reffac <- date_factor
  } else {
    years  <- as.numeric(names(date_factor))
    refdat <- temp[which(years == ref[1])[1]:tail(which(years == ref[2]),n=1)]
    reffac <- date_factor[which(years == ref[1])[1]:tail(which(years == ref[2]),n=1)]
  }

  na <- climdex.pcic:::tapply.fast(is.na(refdat), reffac, sum)
  dlength <- climdex.pcic:::tapply.fast(refdat, reffac,function(x) length(x))
  percna <- na/dlength > (NAmaxAgg/100)
  agg_val <- climdex.pcic:::tapply.fast(refdat, reffac, sum, na.rm=TRUE)
  agg_val[percna] <- NA

  if (is.null(ref)){
    agg_pred <- agg_val
  } else {
    na_pred <- climdex.pcic:::tapply.fast(is.na(temp), date_factor, sum)
    dlength_pred <- climdex.pcic:::tapply.fast(temp, date_factor,function(x) length(x))
    percna_pred <- na_pred/dlength_pred > (NAmaxAgg/100)
    agg_pred <- climdex.pcic:::tapply.fast(temp, date_factor, sum) # add na stuff here
    agg_pred[percna_pred] <- NA
  }

  frstmon <- as.numeric(gsub("....-","", date_factor[1]))# get first months here
  spi_par <- SCI::fitSCI(agg_val, first.mon= frstmon, time.scale= timescale, distr= distribution, p0=TRUE)
  spi_val <- as.vector(SCI::transformSCI(agg_pred, first.mon= frstmon, obj= spi_par, sci.limit= limit))
  names(spi_val) <- levels(date_factor)

  if(param==TRUE){
    return(list(spi_param=spi_par,spi_val=spi_val))
  } else return(spi_val)
}



predict_spi_fcst <- function (temp,spi_par, date_factor, limit=4)
{
  agg_val <- climdex.pcic:::tapply.fast(temp, date_factor, sum, na.rm=TRUE)
  frstmon <- as.numeric(gsub("....-","", date_factor[1]))
  spi_val <- as.vector(SCI::transformSCI(agg_val, first.mon= frstmon, obj= spi_par, sci.limit= limit))
  names(spi_val) <- levels(date_factor)
  return(spi_val)
}




#' default index function for climindvis objects
#' @param climindvis Climindvis object generated using the function make_object.
#' @param forecast Climindvis object with forecast data generated using the function make_object
#' @seealso \link{make_object}
#' @param args List of arguments for calculation of index.  Arguments depend on type of index, for more information see documentation of calculate_index
#' @family calculate_index
#' @seealso \code{\link{calc_index}}
#' @return The function returns a climindvis_index object with an additonal entry <index_fcst>.
#'@section Details:
#'The SPI calculation is based on the SCI package from Lukas Gudmundsson & James Stagge. For integrating forecasts into the SPI plot, the timeseries of forecast and historical data are checked for the overlapping period. For this period a mean
#'@references
#'Stagee, J.H. ; Tallaksen, L.M.; Gudmundsson, L.; van Loon, A.; Stahl, K.: Candidate Distributions for Climatological Drought Indices (SPI and SPEI), 2015, International Journal of Climatology, 35, 4027-4040, doi:10.1002/joc.4267.
#' @export
#' @keywords internal
calculate_index.spi_forecast <- function(climindvis,aggt,...){

  args=list(aggt=aggt,...)
  arguments=c(list(climindvis=climindvis),args)

  iargs<-do.call(index_arguments,arguments)
  sel_time<-get_date_factors(climindvis,args$aggt)

  d=dim(climindvis$data[[iargs$var]])
  ld=length(d)

  sel_timefcst<-get_date_factors(arguments$fc_p,args$aggt)$tfactor
  fcst <- arguments$fc_p$data[[iargs$var]]
  d_fcst=dim(fcst)
  ld_fcst=length(d_fcst)

  if ((climindvis$time[length(climindvis$time)]+1) == arguments$fc_p$time[1]){
    #if (is.element(tail(climindvis$time,n=args$timescale),head(arguments$forecast$time,n=args$timescale))){
      if(!iargs$ifunargs$timescale==1){
      levs <-  levels(climindvis$time_factors$yearmons)
      ym <- levs[(length(levs)-(iargs$ifunargs$timescale-(iargs$ifunargs$timescale-1))):length(levs)]
      hist_add <- index_array(climindvis$data[[iargs$var]],dim=ld, climindvis$time_factors$yearmons%in%ym, drop=FALSE)
      expand <- array(rep(hist_add,d_fcst[ld_fcst-1]), dim=c(d_fcst[1],dim(hist_add)[length(dim(hist_add))],d_fcst[ld_fcst-1]))
      dex <- dim(expand)
      fcst <- abind::abind(aperm(expand,perm=c(1:(length(dex)-2),length(dex),length(dex)-1)),fcst)

    } else {fcst <- fcst}
    d_fcst <- dim(fcst)
    sel_timefcst <-factor(unlist(list(sel_time$tfactor[climindvis$time_factors$yearmons%in%ym], sel_timefcst)))
    } else {message("Historic data does not match forecast data. No overlap of SPI is calculated.")}

  dat_help_fcst <- array(fcst,dim=c(prod(d_fcst[1:(ld_fcst-1)]),d_fcst[ld_fcst]))
  dat_help_hist <- array(climindvis$data[[iargs$var]],dim=c(prod(d[1:(ld-1)]),d[ld]))
  ihelp_hist <-apply(dat_help_hist,1, function(x) do.call(iargs$ifun,c(list(x,sel_time$tfactor),iargs$ifunargs)))
  ihelp <- rearange_by_year(sapply(ihelp_hist, function(x) x[["spi_val"]]),levels(sel_time$tfactor))
  params <- rep(lapply(ihelp_hist, function(x) x[["spi_param"]]),dim(dat_help_fcst)[1]/length(ihelp_hist))

  dat <- lapply(apply(dat_help_fcst, 1, list),function(x) x[[1]])
  out <- mapply(predict_spi_fcst,temp=dat,spi_par= params, MoreArgs = list(date_factor=sel_timefcst), USE.NAMES = TRUE)
  ihelp_fcst <- rearange_by_year(out,levels(sel_timefcst))

  ireturn<-list()
  ireturn$index<-array(ihelp,c(d[1:(ld-1)],dim(ihelp)[-1]),dimnames=c(rep(list(NULL),ld-1),dimnames(ihelp) [-1]))
  ireturn$index_fcst <- array(ihelp_fcst,c(d_fcst[1:(ld_fcst-1)],dim(ihelp_fcst)[-1]),dimnames=c(rep(list(NULL),ld_fcst-1),dimnames(ihelp_fcst) [-1]))
  ireturn$index_info <- c(iargs$ifunargs,iargs$plotargs,list(aggt=args$aggt),hist_time = list(levels(sel_time$tfactor)), fcst_time=list(levels(sel_timefcst)))
  ireturn$index_info$idims<-get_idims(climindvis,args)

  if (iargs$trend == TRUE){

    itrend <- calc_index_trend(index=ireturn$index, targs=iargs$trendargs, iargs=ireturn$index_info)
    ireturn$index_trend <-  itrend$data
    ireturn$trend_info <- itrend$statistics

  }
  if (args$aggt=="dates") ireturn$index_info<-c(ireturn$index_info,list(start_days=args$start_days,end_days=args$end_days))
  return(ireturn)
}

