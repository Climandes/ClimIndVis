#' Wrapper function to calculate and visualize anomaly of indices for selected years
#'
#' This function calls the function  \code{\link{calc_index}} and plots the anomaly of the mean of the index values of defined years against the mean of a set of reference years in a map. It can be used for either gridded or station data or a combination of both. \cr

#'
#' @inheritParams plot_doc
#' @param col Array of colors for plotting of length 3 or longer (must be a valid arguments to \code{\link[grDevices]{col2rgb}}). Length of array is adjusted to number of levels. If not provided, package default colors for index anomaly are used. Default="default".
#' @param refyears Optional. Array of reference years for calculating anomaly. If not defined, all years in dataset are used (default).
#' @param anomyears Year or array of years for which anomaly is calculated with respect to refyears. If more than one anomyear is given, the mean of these years is calculated and subtracted from the mean of refyears.
#' @section Output:
#' This function returns one plot per temporal aggregation whch is covered by the data( e.g. if aggt="seasonal" it would return 4 graphics, one for each season).
#'
#' If output is not NULL or "dev.new" the graphics are directly saved in the user specified directory (\emph{plotdir}) with the following filename structure: \cr
#'
#'     \emph{plotname} //"Anomaly_"// index name // plot_value // aggregation // anomyears // "_refyears-"//refyears// \emph{output} \cr e.g.: \cr
#'     \emph{plotname}"Anomaly_dry_days_mean_MAM_1990_refyears1981-2010.png \cr
#'
#' @section Details:
#'
#' When using ensemble data, the plot value is calculated over the whole ensemble (i.e. ensemble mean over the corresponding time period).
#' @examples
#'
#' data(object_st, object_grid)
#' autoplot_anomaly_map(
#'       dat_grid = object_grid, dat_p = object_st,
#'       anomyears=1990,
#'       index = "tnn", index_args = list(aggt = "seasonal", selagg = "JJA"))
#'
#' data(object_hc_grid)
#' index_args=list(aggt = "dates", start_days = "0000-01-15", end_days = "0000-04-15")
#' autoplot_anomaly_map(
#'       dat_grid = object_hc_grid,
#'       anomyears=1981,refyears=1991:2010,
#'       index = "dd", index_args = index_args)
#'
#' @family autoplot_functions
#' @export
#'
# col-> je nach variable???
autoplot_anomaly_map <-function(
  dat_grid, dat_p,
  index, index_args = list(),refyears = NULL, anomyears, NAmaxClim = 20,
  col = "default",
  title = "", plot_title = TRUE, plot_args = list(),
  output = NULL, plotdir, plotname="") {

  # 1. checks: --------------------------------------------------

  grid <- ifelse(missing(dat_grid), 0, 1)
  points <- ifelse (missing(dat_p), 0, 1)
  check_input(grid,points)
  if (missing(anomyears)) stop("year(s) for calculation of anomaly have to be provided")
  if (index_args$aggt=="xdays") stop("aggt=xdays is not implemented yet for autoplot functions")
  ## check input--------------------------------------------------
  check_dir(plotdir,output)
  if(!is.null(plot_args$topo)) check_topo(plot_args$topo)
  check_SPI(index)
  dat=get_data_list(c("dat"))
  check_class(dat[!sapply(dat,is.null)],"climindvis")
  check_type_error(dat,"fc")
  check_fcmon(dat[!sapply(dat,is.null)])
  dat<-cut_to_same_dates(dat)
  datyears=as.integer(levels(dat[[1]]$time_factors$years))
  if (!is.element(anomyears,datyears)) stop("at least one year in anomyears is missing in input data")
  if (is.null(refyears)) {
    refyears=datyears
  } else if( !all(is.element(refyears,datyears))) stop("at least one year in refyears is missing in input data")

  if(grid==1){
    indexvar=get_indexvar(index,index_args)
    plot_args$mask <-dat_grid$mask[[indexvar]]
  }

  # 2. calculate index --------------------------------------------------
  index_args$trend = FALSE
  ind_dat = lapply(dat, function(dd) do.call("calc_index", c(list(dd, index = index), index_args)))

  ## 2.2 calculate plot value and  check NAs---------------------------------------------------

  ppval = lapply(ind_dat, function (ii){
    iref=index_array(ii$index,dim=which(ii$index_info$idims=="year"),which(is.element(as.integer(ii$index_info$years),refyears)),drop=FALSE)
    irefm=apply(iref,c(1:length(ii$index_info$idims))[-which(is.element(ii$index_info$idims, c("year", "ens")))],
    mean, na.rm = TRUE)
    ian=index_array(ii$index,dim=which(ii$index_info$idims=="year"),which(is.element(as.integer(ii$index_info$years),anomyears)),drop=FALSE)
    ianm=apply(ian,c(1:length(ii$index_info$idims))[-which(is.element(ii$index_info$idims, c("year", "ens")))],
      mean, na.rm = TRUE)
    anom=ianm-irefm
    return(anom)})


  pNA = lapply(ind_dat, function (ii) apply(ii$index,
    c(1:length(ii$index_info$idims))[-which(is.element(ii$index_info$idims, c("year", "ens")))],
    function(x) sum(is.na(x))*100/length(x)) )
  # set values with more than NAmaxClim missing values to NA
  phelp = mapply(function (ii, yy) {ii[which(yy>NAmaxClim)] = NA
  return(ii)}, ppval, pNA, SIMPLIFY = FALSE)

  # 3. Plot --------------------------------------------------
  get_plot_args(autoplot="anomaly_map")

  idims=ind_dat[!sapply(ind_dat,is.null)][[1]]$index_info$idims
  aggl<-ifelse(is.element("agg",idims),TRUE,FALSE)
  for (aa in 1:ifelse(aggl,length(ind_dat[[1]]$index_info$aggnames),1)){


    pdat=lapply((names(phelp)), function(ii){
      if(!is.null(phelp[[ii]])){
        if(aggl){
          cd=ifelse(grepl("p",ind_dat[[ii]]$data_info$type),1,2)
          ret=index_array(phelp[[ii]],cd+1,list(aa),drop=TRUE)
        } else ret=phelp[[ii]]
      } else ret=NULL
    })
    names(pdat)=names(phelp)


    if(grid==1){
      plot_args$mask_NA <- array(NA,dim=dim(pdat$dat_grid))
      plot_args$mask_NA[is.na(pdat$dat_grid)] <- 1
    }      # define mask

    ## call plot function

    pnames=get_plot_title(titlestring=title,show_title=plot_title,autoplot="anomaly_map",aa)
    plot_args[c("output","outfile","plot_title")]<-list(output,pnames$f,pnames$t)
    if(grid == 1 & points == 1){
      #plot grid and points
      do.call("plot_map_grid_points", c(list(g_dat = pdat$dat_grid, g_lon = ind_dat$dat_grid$lon, g_lat = ind_dat$dat_grid$lat,
        p_dat = pdat$dat_p, p_lon = ind_dat$dat_p$lon, p_lat = ind_dat$dat_p$lat, p_col_info = "same"), plot_args))
    } else if (grid == 1 & points == 0){
      plot_args[c("p_col", "p_col_center")] = NULL
      do.call("plot_map_grid_points", c(list(g_dat = pdat$dat_grid, g_lon = ind_dat$dat_grid$lon, g_lat = ind_dat$dat_grid$lat), plot_args))
    } else if (grid == 0 & points == 1){
      do.call("plot_map_grid_points", c(list(p_dat = pdat$dat_p, p_lon = ind_dat$dat_p$lon, p_lat = ind_dat$dat_p$lat,
        p_col_info = "cbar", p_legend = "cbar"), plot_args))
    }
  }# end aa

} # end of function







