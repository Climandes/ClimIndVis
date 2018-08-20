check_input<- function(grid,points){
  tryCatch({if(grid == 0 & points == 0) stop("Either grid or point data have to be provided as input")
  }, error=function(cond){
    message("Error in input data:")
    message(cond)
    stop_quietly()
  })
}


#checks for calculations
check_var<-function(dat,varnames){
  tryCatch({
    for (vv in seq_along(varnames)){
      if (is.null(dat$data[[varnames[vv]]]) ){
        stop(paste0("Error in input data:variable ",varnames[vv]," needed for calculation of index is missing"))}
    }}, error=function(cond){
      message("Error in input data:")
      message(cond)
      stop_quietly()
    })
}

#gives error for functions where SPI cannot be calculated
check_SPI<-function(index){
  tryCatch({
    if(grepl("SPI",index)) stop("For plotting SPI please use <<autoplot_ts_stations>> or <<plot_spi_forecast>>")
  }, error=function(cond){
    message("SPI not available for this function:")
    message(cond)
    stop_quietly()
  })
}

check_aggt<- function(index,aggt){
  tryCatch({
    if(!is.character(aggt)) stop("aggt needs to be a character string")
    if(!is.element(aggt,c("annual","seasonal","monthly","other","dates"))) stop("aggt does not exist (maybe check spelling).")
    if (grepl("spi",index) & aggt!="monthly") {
      aggt="monthly"
      message("Warning:Wrong aggt for SPI calculation, aggt is set to <<monthly>>.")
    } else if (grepl("rainy_season",index) & aggt!="dates"){
      stop ("For index rainy season, aggt needs to be <<dates>> and <<start_days>> and <<end_days>> additionally have to be provided.")
    } else aggt=aggt
    return(aggt)
  }, error=function(cond){
    message("error in input function:")
    message(cond)
    stop_quietly()
  })
}

check_dims<- function(data,dates,var,lon,lat,data_info){
  tryCatch({
    if (is.null(dates)) {
      stop(paste0("Time dimension for ",var," is missing") ) }

    dd <- dim(data)
    dl <- length(dd)
    dt <- data_info$type

    if (is.element(dt, c("p","p_hc","p_fc"))){
      #if (length(dd)>2 & dt=="p") stop("<type> is not set correctly")
      if (!dd[1] == length(lon)) { stop("Data dimension does not fit lon dimensions") }
      if (!dd[1] == length(lat)){ stop("Data dimension does not fit lat dimensions") }

      if(is.element(data_info$date_format, "t1d")){
          if(!tail(dd,1) == length(dates)) {stop("Data dimension does not fit time dimension")}
      }  else {
        dates <- unlist(dates)
        #if (data_info$type == "p") {stop("<type> does not fit date_format.")}
        #if (dl==4  & dt == "p_fc") {stop("<type> is not set correctly")}
        #if (dl==3  & dt == "p_hc") {stop("<type> is not set correctly")}
        if (!dd[dl]*dd[dl-1] == length(dates)) stop("Data dimension does not fit time dimensions")

      }
      }

    else if (is.element(data_info$type, c("grid","grid_hc","grid_fc"))){
      if (length(dd)<=2) stop("<type> is not set correctly")
      if (!dd[1] == length(lon)){stop("Data dimension does not fit lon dimensions")}
      if (!dd[2] == length(lat)){stop("Data dimension does not fit lat dimensions")}

      if(is.element(data_info$date_format, "t1d")){
        if (is.list(dates)) stop("if date_format=t1d, time must be an array and not a list.")
        #if (!length(dd)==3) {stop("<type> or <date_format> is not set correctly")}
        if (!tail(dd,1) == length(dates)) {stop("Data dimension does not fit time dimension")}
             } else {
          dates <- unlist(dates)
          #if (length(dd)==5  & data_info$type == "p_fc") {stop("<type> is not set correctly")}
          #if (length(dd)==4  & data_info$type == "p_hc") {stop("<type> is not set correctly")}
          if (!dd[dl]*dd[dl-1] == length(dates)) stop("Data dimension does not fit time dimensions")

      } }
    else stop("<<type>> is not set correctly")
  }, error=function(cond){
    message("Error in input data:")
    message(cond)
    stop_quietly()
  })
}

check_entries_of_list<-function(entry_names,list){
  tryCatch({
    for (a in entry_names) if(is.null(list[[a]])){
      stop ("Entry for argument <<", a, ">> is needed for chosen index")}
  }, error=function(cond){
    message("Error in input data:")
    message(cond)
    stop_quietly()
  })
}

check_arguments_function<-function(argument_list){
  tryCatch({
    for (a in 1:length(argument_list)) if(is.null(argument_list[[a]])){
      stop (paste0("Mandatory argument <<",names(argument_list)[a],">> is missing"))}
  }, error=function(cond){
    message("Error in input data:")
    message(cond)
    stop_quietly()
  })
}

check_class<-function(objects,class){
tryCatch({
    if(!is.null(objects$lon)) objects<-list(objects)
    lapply(objects, function(o) if(!is.element(class,class(o))){
      stop (paste0("At least one input object is not of type ",class))})
    }, error=function(cond){
      message("Error in input data:")
      message(cond)
      stop_quietly()
    })
}

compare_class<-function(obj1,obj2){
  tryCatch({
    if (!all(class(obj1)==class(obj2))) stop("Inputs not of same class")
  }, error=function(cond){
    message("Error in input data:")
    message(cond)
    stop_quietly()
  })
}

compare_value<-function(a,b,valname="values"){
  tryCatch({
    if (!all(a==b)) stop(paste0(valname," are not the same"))
  }, error=function(cond) {
    message("Error in input data:")
    message(cond)
   stop_quietly()
  })
}

#checks if lons/lats are the same for all point/grid objects,respectively
check_spatial_dims<-function(object_list){
  tryCatch({
    grid=which(sapply(object_list, function(x) grepl("grid",x$data_info$type)))
    p=which(sapply(object_list, function(x) grepl("p",x$data_info$type)))

    if (length(grid)>1){
      mlon_grid<-  Reduce(intersect,lapply(object_list[grid], function(x) round(array(x$lon),3)))
      mlat_grid<-  Reduce(intersect,lapply(object_list[grid], function(x) round(array(x$lat),3)))
      invisible(lapply(object_list[grid], function(o){
        if (length(o$lon) != length(mlon_grid))  stop("Longitudes do not match for all grid objects")
        if (length(o$lat) != length(mlat_grid)) stop("latitudes do not match for all grid objects")
      }))
    }
    if(length(p)>1){
      mlon_p<-  Reduce(intersect,lapply(object_list[p], function(x) round(array(x$lon),3)))
      mlat_p<-  Reduce(intersect,lapply(object_list[p], function(x) round(array(x$lat),3)))
      invisible(lapply(object_list[p], function(o){
        if (length(o$lon) != length(mlon_p))  stop("Longitudes do not match for all point objects")
        if (length(o$lat) != length(mlat_p)) stop("Latitudes do not match for all point objects")
      }))
    }
  }, error=function(cond){
    message("Error in dimensions of input data:")
    message(cond)
    stop_quietly()
  })
}

#checks if initial month of forecasts is the same for all hc/fc objects in list
check_fcmon<-function(data){
  tryCatch({
    sfc<-which(sapply(data, function(d) grepl("fc|hc",d$data_info$type)))
    if(length(sfc)>1){
      fmons<-unique(lapply(data[sfc], function(d) d$data_info$fmon))
      if (length(fmons)>1) stop("Initial month of forecast (<<fmon>>) is not the same for all hindcasts/forecasts objects")
    }
  }, error=function(cond){
    message("Error in input data:")
    message(cond)
    stop_quietly()
  })
}

#checks in make object funcion if fcmon is integer/chracter and corresponds to first month of data
check_fmon_object <- function(dates, fmon){
  tryCatch({
    if(is.numeric(fmon)) {fmon <- stringr::str_pad(as.character(fmon),2,pad="0")}
    if(is.character(fmon)) {if(suppressWarnings(is.na(as.numeric(fmon)))) {stop("<fmon> is not set correctly.")}}

    if(!is.list(dates)){dates <- list(dates)}
    firstmon <- unique(sapply(dates, function(x) substr(x[1],6,7)))
    if(length(firstmon)>1) stop("Initial month of fc/hc is not the same for all time slices.")

    if(!firstmon == fmon){
      stop("Initial months of fc/hc data is not the same as <fmon>.")
      }
    }, error=function(cond){
    message("Error in input data:")
    message(cond)
    stop_quietly()
  })
  return(fmon)

}

# checks if input in data_list is of same type
check_same_type<-function(data_list){
  points=length(which(sapply(data_list, function(x) grepl("p",x$data_info$type))))
  grid = length(which(sapply(data_list, function(x) grepl("grid",x$data_info$type))))

  if(!((points==0 & grid==length(data_list) ) | (grid==0 & points==length(data_list)))){
    message("error in input data:all input data needs to be of same type (grid or p")
    stop_quietly()
  }
}


check_type_error<-function(data,error_types){
  tryCatch({
    sapply(data, function(d) {
      if(grepl(paste(error_types,collapse="|"),d$data_info$type)){
        stop(paste0("Function does not support data of the followng type(s)", error_types))
      }})
  }, error=function(cond){
    message("Error in input data:")
    message(cond)
    stop_quietly()
  })
}

check_dir <- function(dir, output) {
  if (!is.null(output)) {
    if (output!="dev.new"){
      tryCatch({
        if (!dir.exists(dir)) {
          dir.create(dir)
        }
      }, error = function(cond) {
        message("Error with directory:")
        message(cond)
        stop_quietly()
      })
    }
  }
}

check_dat<-function(dat){
    tryCatch({
          types <- sapply(dat, function(x) x$data_info$type)
          if(any(is.element(types,"grid"))){
            stop ("Function only works for point data")}
    }, error=function(cond){
      message("Error in data input:")
      message(cond)
      stop_quietly()
    })
}



# for autoplot_ts_stations
check_type<- function(dat, index, index_args, type){
  tryCatch({
    if (any(is.element(lapply(dat, function(x){x$data_info$type}),"grid"))){stop("Data can only be of type point.")}
    if (length(index)!=length(index_args)){
      if(length(index)> 1) stop("<<index_args>> needs to be a list of same length as array <<index>> containing a list of elements for each entry of index")
      if (!is.list(index_args[[1]]) && type=="multi_agg") stop("<<index_args>> needs to be a list of same length as array <<index>> containing a list of elements for each entry of index")
    }
     if (length(index)> 1 & !is.list(index_args[[1]]))
      stop("<<index_args>> needs to be a list of same length as array <<index>> containing a list of elements for each entry of index")
    if (length(dat)> 1 && length(index)>1 | length(dat)> 1 && (length(index_args)>1 && is.list(index_args[[1]])))
      stop(" <type>=",type,"only works for one index and one aggregation. ")
    if (is.element(type,c("single_ts","multi_agg","multi_points"))){
      if (length(index)>1 | length(dat)>1) stop(paste0(" <type>=",type,"only works for one index and one dataset. "))
    }
    if(type=="spi_barplot"){
      if(length(index)>1 & length(dat)>1) stop(paste0(" <type>=",type,"only works for one index and one dataset. "))
    }
  }   , error=function(cond){
    message("Error in input arguments:")
    message(cond)
    stop_quietly()

  })
}



check_trend <- function(var){return(ifelse(is.element(var, c("tmin","tmax","tavg")), FALSE, TRUE))}

# check format of dates
check_dates <- function(dates,var){
  tryCatch({
    if(is.list(dates)) {
      if(any(!sapply(dates, function(x) is_date(x)))){
        stop(paste0("<dates_",var ,"> format not set correctly. It needs to be of class <<Date>>."))
        }
    } else {
        if(!is_date(dates)){
          stop(paste0("<dates_",var ,"> format not set correctly. It needs to of class <<Date>>."))
        }
    }
  }   , error=function(cond){
    message("Error in input data:")
    message(cond)
    stop_quietly()

  })
}
