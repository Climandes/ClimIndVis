#' Wrapper function to calculate and visualize climatology of indices
#'
#' This function calls the function  \code{\link{calc_index}} and plots the mean, median, min, max or standard deviation of the index values over the whole time series in a map. It can be used for either gridded or station data or a combination of both. \cr
#' To see some example outputs of this function, look at the vignette "autoplot-functions" accesible through the package help main page or by typing \cr
#'  \emph{vignette("autoplot-functions",package="ClimIndVis")} into the console.
#'
#' @param plot_value Climatological value to plot. One of the following character strings: "mean", "median", "min", "max", "sd" (=standard deviation), or "relsd" (sd/abs(mean)), also see NOTES. Default = "mean".
#' @inheritParams plot_doc
#' @param col Array of colors for plotting of length 3 or longer (must be a valid arguments to \code{\link[grDevices]{col2rgb}}). Length of array is adjusted to number of levels. If not provided, package default colors for index are used. Default="default".
#' @section Output:
#' This function returns one plot per temporal aggregation whch is covered by the data( e.g. if aggt="seasonal" it would return 4 graphics, one for each season).
#'
#' If output is not NULL or "dev.new" the graphics are directly saved in the user specified directory (\emph{plotdir}) with the following filename structure: \cr
#'
#'     \emph{plotname} // index name // plot_value // aggregation // years // \emph{output} \cr e.g.: \cr
#'     \emph{plotname}"_dry_days_mean_MAM_1981-2010.png \cr
#'
#' @section Details:
#'
#' Please note that calculating the mean does not make sense for indices based on the number of days above/below a quantile threshold (e.g. tn10p,tn90p)
#'
#' When using ensemble data, the plot value is calculated over the whole ensemble (e.g. ensemble mean over the whole time period).
#' @examples
#'
#' data(object_st, object_grid)
#' autoplot_climatology_map(
#'       dat_grid = object_grid, dat_p = object_st,
#'       index = "tnn", index_args = list(aggt = "seasonal", selagg = "JJA"),
#'       plot_value = "min" ,  selyears = c(2000:2010))
#'
#' data(object_hc_grid)
#' index_args=list(aggt = "dates", start_days = "0000-01-15", end_days = "0000-04-15")
#' autoplot_climatology_map(
#'       dat_grid = object_hc_grid,
#'       index = "dd", index_args = index_args,
#'       plot_value = "min" , selyears = c(2000:2010))
#'
#' @family autoplot_functions
#' @export
#'

autoplot_climatology_map <-function(
  dat_grid, dat_p,
  index, index_args = list(),  plot_value = "mean", selyears = NULL, NAmaxClim = 20,
  col = "default",
  title = "", plot_title = TRUE, plot_args = list(),
  output = NULL, plotdir, plotname="") {

  # 1. checks: --------------------------------------------------

  grid <- ifelse(missing(dat_grid), 0, 1)
  points <- ifelse (missing(dat_p), 0, 1)
  check_input(grid,points)

  ## check input--------------------------------------------------
  check_dir(plotdir,output)
  if(!is.null(plot_args$topo)) check_topo(plot_args$topo)
  check_SPI(index)
  dat=get_data_list(c("dat"))
  check_class(dat[!sapply(dat,is.null)],"climindvis")
  check_type_error(dat,"fc")
  check_fcmon(dat[!sapply(dat,is.null)])
  switch(is_index_special(index)+1,selyears_data<-selyears,selyears_data<-NULL)
  dat<-cut_to_same_dates(dat,selyears_data)
  if(grid==1){
    indexvar=get_indexvar(index,index_args)
    plot_args$mask <-dat_grid$mask[[indexvar]]
  }

  # 2. calculate index --------------------------------------------------
  index_args$trend = FALSE
  ind_dat = lapply(dat, function(dd) do.call("calc_index", c(list(dd, index = index), index_args)))
  if(is_index_special(index) & !is.null(selyears)){
    ind_dat<-cut_to_same_dates_index(ind_dat,selyears)
  }
  ## 2.2 calculate plot value and  check NAs---------------------------------------------------
  if(plot_value=="relsd"){ plot_fun<-function(x,na.rm=FALSE,...) stats::sd(x,na.rm=na.rm)/abs(mean(x,na.rm=na.rm))*100
  } else plot_fun=plot_value

  phelp = lapply(ind_dat, function (ii) apply(ii$index,
    c(1:length(ii$index_info$idims))[-which(is.element(ii$index_info$idims, c("year", "ens")))],
    plot_fun, na.rm = TRUE) )
  pNA = lapply(ind_dat, function (ii) apply(ii$index,
    c(1:length(ii$index_info$idims))[-which(is.element(ii$index_info$idims, c("year", "ens")))],
    function(x) sum(is.na(x))*100/length(x)) )
  # set values with more than NAmaxClim missing values to NA
  phelp = mapply(function (ii, yy) {ii[which(yy>NAmaxClim)] = NA
  return(ii)}, phelp, pNA, SIMPLIFY = FALSE)

  # 3. Plot --------------------------------------------------

  get_plot_args(autoplot="climatology_map")

  idims=ind_dat[!sapply(ind_dat,is.null)][[1]]$index_info$idims
  aggl<-ifelse(is.element("agg",idims),TRUE,FALSE)
  for (aa in 1:ifelse(aggl,length(ind_dat[[1]]$index_info$aggnames),1)){


    pdat=lapply((names(phelp)), function(ii){
      if(!is.null(phelp[[ii]])){
        if(aggl){
          cd=ifelse(ind_dat[[ii]]$data_info$type=="p",1,2)
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

    pnames=get_plot_title(titlestring=title,show_title=plot_title,autoplot="climatology_map",aa)
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







