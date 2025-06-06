#' Function to generate a climindvis object
#'
#'This function creates a climindvis object needed as input for all other functions of the package. The function performs several checks on the data and formats the data as it is needed by the other functions. For the different data types, see the parameter description below.
#'
#'@encoding UTF-8
#'@param tmin Daily minimum temperature data in degrees C (see NOTES)
#'@param tmax Daily maximum temperature data in  degrees C (see NOTES)
#'@param prec Daily total precipitation data in mm (see NOTES)
#'@param tavg Daily average temperature data in degrees C (see NOTES)
#'@param dates_tmin Dates for the daily minimum temperature data (see NOTES)
#'@param dates_tmax Dates for the daily maximum temperature data (see NOTES)
#'@param dates_prec Dates for the daily precipitation data (see NOTES)
#'@param dates_tavg Dates for the daily average temperature data (see NOTES)
#'
#'@param lon Longitudes of input data (1 or 2dim array)
#'@param lat Latitudes of input data (1 or 2dim array)
#'@param data_range Vector with start and end year of calculation period (optional). If supplied, all data is cut to this period. If missing, a period from the earliest until the last date value is defined.
#'
#'@param data_info List of data-specific information with the following elements:
#'\itemize{
#'  \item type [character]: data type (see NOTES)
#'     \itemize{
#'       \item p,p_hc,p_fc: point data for observations/reanalyses, hindcasts,forecasts. For a single station, see NOTES.
#'       \item grid,grid_hc,grid_fc: gridded data for observations/reanalysis, hindcasts,forecasts
#'     }
#'  \item date_format  [character] (see NOTES)
#'   \itemize{
#'       \item t1d: time is a vector (1d), applies to data provided as single time series.
#'       \item t2d: time is a named list, applies to data provided in form of time slices (list elements being the individual time slices). The names of the list elements correspond to the  years of the first entry of each time slice.
#'     }
#'  \item data_name: Name of dataset (e.g. "Station data" or "ECMWF S4 hindcasts"). Nessesary for plot functions
#'
#'  \item pnames: Character array of station names (optional). If provided they can be used for plotting. For point data (type="p*") only.
#'  \item fmon: Only for forecast and hindcast data. Character or integer of month, the forecast is initialzied, eg.  fmon = "12".
#'}
#' @return The function returns a climindvis object with the following entries:
#' \itemize{
#' \item data: Named list containing an array of data (tmin/tmax/prec/tavg) of dimension: spatial dimensions x (ensemble dimensions) x no of timesteps
#' \item time: Array of timesteps which are common for all input data.
#' \item lon: Array of longitudes common for all input data. For gridded data, longitudes need to be in ascending order.
#' \item lat: Array of latitudes common for all input data. For gridded data, latitudes need to be in ascending order.
#' \item data_info: data_info as assed to function.
#' \item time_factors: time factors used for calculation of temporal aggregation of indices.
#' \item mask: (only for gridded data) list containing a mask for each variable with mask=1 for gridpoints where all time steps are NA
#' }
#' @section NOTES:
#'

#' The data input is an array where the dimensions depend on the type of data. Generally there are two possible ways to provide the data depending on the date_format. Currently, for hindcasts and forecasts, date_format has to be "t2d":
#' \itemize{
#' \item time series: In this case \emph{date_format}="t1d" and the dimensions of the data are: \cr
#'  spatial dimensions x ensemble dimension (optional, applies to hindcasts or forecasts) x time \cr
#'  where time is a 1d array (but the time series does not have to be continous and gaps are allowed). \cr
#'   If providing data for a single station, it is a matrix with two dimensions; one for the station (the ensemble dimension in case of hindcasts/forecasts) and one for the time.
#' \item time slices: In this case \emph{date_format}="t2d" and the dimensions of the data are: \cr
#' #'  spatial dimensions x ensemble dimension (optional, applies to hindcasts or forecasts) x forecast days x forecast years \cr
#' and time is a named list of length years with each list element containing the dates for one time slice, the names of the list elements correspond to the year of the first entry of the time slice. \cr
#' In case of forecast data, the array should have the last dimension as year dimension of length one (if the year dimension is missing it is set to one).
#' For date_format="t2d", all variables need to have the same time steps, i.e. dates_xxx need to be equal for all variables.
#' }
#' At least one of the 4 variables (and the respective dates) must be provided, otherwise the function gives an error.
#'
#' For example data and example objects see \code{\link{example_data}} \code{\link{example_climindvis_objects}}.

#'@examples
#' # make climindvis object with station data
#'
#' data(data_st)
#' data_info = list(type="p",date_format="t1d", data_name="test data station",
#'   pnames=paste0("station",1:4))
#' climindvis_st <- make_object(
#'     tmin = data_st$tmin, tmax = data_st$tmax, tavg = data_st$tvag, prec = data_st$prec,
#'     dates_tmin = data_st$time, dates_tmax = data_st$time, dates_tavg = data_st$time, dates_prec = data_st$time,
#'     lon = data_st$lon, lat = data_st$lat,  data_info = data_info, data_range=c(1950,2000))
#' class(climindvis_st)
#'
#' # make climindvis object with gridded data
#'
#' data(data_grid)
#' data_info=list(type="grid",date_format="t1d", data_name="test data grid")
#' climindvis_grid <- make_object(
#'     tmin = data_grid$tmin,tmax = data_grid$tmax, prec = data_grid$prec,
#'     dates_tmin = data_grid$time, dates_tmax = data_grid$time, dates_prec = data_grid$time,
#'     lon = data_grid$lon, lat = data_grid$lat,  data_info = data_info)
#' class(climindvis_grid)
#'
#' # make climindvis object with gridded hindcast data
#'
#' data(data_hc_grid)
#' data_info=list(type="grid_hc",date_format="t2d", data_name="test data hc grid",fmon = "01")
#' climindvis_grid_hc <- make_object(
#'     tmin = data_hc_grid$tmin, tmax = data_hc_grid$tmax, prec = data_hc_grid$prec,
#'     dates_tmin = data_hc_grid$time, dates_tmax = data_hc_grid$time, dates_prec = data_hc_grid$time,
#'     lon = data_hc_grid$lon, lat = data_hc_grid$lat,  data_info = data_info)
#' class(climindvis_grid_hc)

#'@export
make_object<- function(tmin = NULL, tmax = NULL, prec = NULL, tavg = NULL, dates_tmin = NULL, dates_tmax = NULL, dates_prec = NULL, dates_tavg = NULL, lon, lat, data_range = NULL, data_info){

  if(missing(lon)) {stop("no longitudes specified.")}
  if(missing(lat)) {stop("no latitudes specified.")}
  if(is.null(data_info$type)) stop ("<<type>> is missing in data_info")
  if(is.null(data_info$data_name)) stop ("<<data_name>> is missing in data_info")
  if(is.null(data_info$date_format)) stop ("<<date_format>> is missing in data_info")
  if(all(is.null(tmin),is.null(tmax), is.null(tavg),is.null(prec))) stop ("data (tmin,tmax,prec,tavg) is missing")
  if(all(is.null(dates_tmin),is.null(dates_tmax), is.null(dates_tavg),is.null(dates_prec))) stop ("dates are missing")
  if (grepl(paste("hc","fc",sep="|"),data_info$type) & data_info$date_format!= "t2d") stop("For types *hc and *fc only date_format=t2d is currently implemented.")

  for (var in c("tavg","prec","tmin","tmax")){
    if (!is.null(get(var))){

      check_dims(get(var), get(paste0("dates_",var)),var,lon,lat,data_info)
      check_dates(get(paste0("dates_",var)),var)
      check_temp(get(var), var)
    }
    }
  data<-list()
  dates_list <- list(dates_tmin,dates_tmax,dates_tavg,dates_prec)

  if(data_info$date_format=="t2d"){



    if (length(unique(dates_list[!sapply(dates_list,is.null)])) != 1) stop("for date_format=t2d input dates of all variables need to be equal." )
    if (grepl("hc",data_info$type) & length(dates_list[[which(!is.null(dates_list))[1]]])==1) stop("number of hindcast years should be greater than 1.")

    for (var in c("tavg","prec","tmin","tmax")){
      if (!is.null(get(var))){

        if(!is.null(data_info$fmon)){
          data_info$fmon <- check_fmon_object(get(paste0("dates_",var)),data_info$fmon)
        }

        dims <- dim(get(var))
        dl <- length(dims)

        if(!prod(dims[(dl-1):dl]) == length(unlist(get(paste0("dates_",var))))) {
          assign(var, array(data=get(var), dim=c(dim(get(var)),1)))
        }
        dims <- dim(get(var))
        dl <- length(dims)
        if(!prod(dims[(dl-1):dl]) == length(unlist(get(paste0("dates_",var)))))
          stop(paste0("time input does not match time dimensions of data for var = ",var))

        if(!is.list(get(paste0("dates_",var)))) assign(paste0("dates_",var), list(get(paste0("dates_",var))))
        hh=get(paste0("dates_",var))
        if (is.null(names(hh)))  names(hh) =sapply(hh,function(hh) return(substring(hh[1],1,4)),simplify = TRUE)
      data[[var]] <- convert_data_to_1d(get(var))
      time <- convert_time_to_1d(hh)
      }
    }

  } else {

    new_time <- equal_time(dates_list,data_range,data_info)

    for (var in c("tavg","prec","tmin","tmax")){
      if (is.null(get(var))==FALSE){
        dates <- get(paste0("dates_",var))

        if (isTRUE(all.equal(format(dates, '%Y-%m-%d'), format(new_time$time_format,'%Y-%m-%d')))) {data[[var]] <- get(var)}
        else {
          d<-dim(get(var))
          dl<-length(d)
          new_var <- array(NA,dim=c(d[1:(dl-1)],new_time$time_dim))
          date_help <- format(dates, '%Y-%m-%d')
          time_help <- format(new_time$time_format,'%Y-%m-%d')
          if(data_info$type=="p"){
            new_var[,time_help%in%date_help] <- get(var)[,date_help%in%time_help]
          } else {new_var[,,time_help%in%date_help] <- get(var)[,,date_help%in%time_help]}
          data[[var]] <- new_var

        }
      }}
    time <- new_time$time_format
  }

  time_factors <- switch(((tail(time,1)-time[1]+1)!=length(time) | length(time)<365)+1,get_date_factors_obj(time),get_date_factors_obj_timeslice(time))
  if (grepl("grid",data_info$type)){
    climindvis<- list("data"=data,"time"=time,"lon"=lon,"lat"=lat,"data_info"=data_info,"time_factors"=time_factors,"mask"=get_mask_data(data))
  } else climindvis<- list("data"=data,"time"=time,"lon"=lon,"lat"=lat,"data_info"=data_info,"time_factors"=time_factors)
  class(climindvis) <- "climindvis"
  return(climindvis)

}


######################### help functions ###################################################
#function to cut out the first "fmon" months
equal_time<-function(dates_list,data_range,data_info){
  if (length(grep("c",data_info$type))==0 & data_info$date_format=="t1d"){
    if (is.null(data_range)){
      minmax <- get_vals(dates_list[which(sapply(dates_list, is.null)==FALSE)])
      time_format <- as.Date(seq(minmax[1], minmax[2], by="1 day"))
      time_dim <- length(time_format)

    } else {
      time_format <- as.Date(format(seq(as.Date(paste0(data_range[1],"-01-01")), as.Date(paste0(data_range[2],"-12-31")),by="1 day")))
      time_dim <- length(time_format)
    }
  } else {
   time_format = unique(as.Date(unlist(as.Date(dates_list))))

 }
  return(list(time_format=time_format, time_dim=time_dim))
}


get_vals <- function(dates_list){
  all_min <- which.min(sapply(dates_list, min, na.rm=TRUE))
  all_max <- which.max(sapply(dates_list, max, na.rm=TRUE))
  min_val <- dates_list[[all_min]][1]
  max_val <- dates_list[[all_max]][length(dates_list[[all_max]])]
  return(c(min_val, max_val))
}


# converts arrays with two time dimension into one, data=list or array
convert_data_to_1d<-function(data){
  d<-dim(data)
  dl<-length(d)
  data1d <- array(data,dim=c(d[1:(dl-2)],prod(d[(dl-1):dl])))
}

#converts list of timesteps to 1d array of timestepts
convert_time_to_1d<-function(time)  {
  time1d<-as.Date(unlist(time),origin=time[[1]][1]-as.integer(time[[1]][1]))
}


# get date factor from time
get_date_factors_obj<-function(t1d){
  factors<-list()
  factors$years<-factor(format(t1d,"%Y"))
  factors$yearmons<-factor(format(t1d,"%Y-%m"))
  jdays<-format(t1d,"%j")
  jdays<-replace_jday_29feb(jdays)
  factors$jdays=factor(jdays)

  return(factors)

}


get_date_factors_obj_timeslice<-function(t1d){
  factors<-list()
  factors$years<-factor(format(t1d,"%Y"))
  factors$yearmons<-factor(format(t1d,"%Y-%m"))
  jdays<-format(t1d,"%j")
  sel29=which(substring(t1d,6,10)=="02-29")
  end29=sapply(sel29, function(ll) tail(which(substring(names(t1d),1,4)==substring(names(t1d[ll]),1,4)),1))
  sel366=which(jdays == 366)
  start366=sapply(sel366, function(ll) head(which(substring(names(t1d),1,4)==substring(names(t1d[ll]),1,4)),1))
  if (length(sel29) >0 ) for (i in 1:length(sel29)) jdays[sel29[i]:end29[i]] = pad2(59:(59+end29[i]-sel29[i]), width=3)
  if (length(sel366) >0 )for (i in 1:length(sel366)) jdays[start366[i]:sel366[i]] =  pad2((360-(sel366[i]-start366[i])):360,width=3)
  factors$jdays=factor(jdays)

  return(factors)

}


replace_jday_29feb <- function(jdays){
    indices <- which(jdays == 366)
    if (length(indices) > 0)
      jdays[rep(indices, each = 366) + -365:0] <- pad2(c(1:59,59,60:365), width=3)
    return(jdays)
  }



get_mask_data<-function(data){
  mask=lapply(data, function(dd){
    apply(dd,c(1,2), function(x){
      ifelse(all(is.na(x)),1,NA)})
  })
  return(mask)
}
