#' Wrapper function to calculate and plot trend of indices.
#'
#'This function calls the function \code{\link{calc_index}} and plots the significant trend values in a map. It can be used for either gridded or station data or a combination of both.\cr
#'To see some example outputs of this function, look at the vignette "autoplot-functions" accessible through the package help main page or by typing  \cr
#'\emph{vignette("autoplot-functions",package="ClimIndVis")} into the console.
#'
#'@inheritParams plot_doc
#'@param sig_lev Level of significance. Only significant trend estimates are plotted. Default = 0.95.
#'@param abs Logical. Plot absolute trend (unit/decade) instead of relative trend (percentage change relative to middle time). Default = TRUE. For indices with continuous temperature values (e.g. TNN, TNX, etc.), no relative trend is calculated.
#' @section Details:
#' For every index a default method is used for the trend calculation. The default trend estimation and test depends on the nature of the calculated index: \cr
#'
#'For continuous data (e.g. sum, mean, prcptot, spi) by default an ordinary linear regression is calculated with time as predictor for the trend estimate and applying a students-t test for significance testing. If the data is not normaly distributed, a logarithmic transformation is applied before the trend calculation, e.g. for precipitation sums. \cr
#'
#'For count data (e.g dd, cdd, cwd) by default a logistic regression is calculated with time as predictor for the trend estimation and applying a students-t test for significance testing. In the calculation, a correction for overdispersion is applied. \cr
#'
#' If you set trend = "MannKendall" inside the list of index arguments (index_args), the non-parametric Mann-Kendall test based on the relative ranking in the series is applied and a Theil-Sen slope is estimated. See e.g. Yue et al. 2002 or Mann (1945), Kendall (1975) for further references.
#' @section Output:
#' This function returns one plot per temporal aggregation which is covered by the data( e.g. aggt="seasonal" returns 4 graphics, one for each season).
#'
#' If output is not NULL or "X11" the graphics are saved in the user specified directory (\emph{plotdir}) with the following filename structure: \cr
#'
#'     \emph{plotname} // index name // aggregation // "abs-/rel_trend" // years // \emph{output} \cr e.g.: \cr
#'     \emph{plotname}"_dry_days_MAM_rel-trend_1981-2010.png \cr
#'

#'@examples
#' data(object_st,object_grid)
#' autoplot_trend_map(dat_grid=object_grid,
#'                          index="mean",index_args=list(var="tmax",aggt="seasonal",selagg="SON"))
#'
#' @family autoplot_functions
#'@export
autoplot_trend_map <-function(
  dat_grid, dat_p,
  index, index_args = list(),abs = TRUE,sig_lev = 0.95, selyears=NULL,
  title = "", plot_title = TRUE, plot_args = list(),
  output=NULL, plotdir, plotname = "") {

  # 1. checks: --------------------------------------------------

  grid <- ifelse(missing(dat_grid), 0, 1)
  points <- ifelse (missing(dat_p), 0, 1)
  check_input(grid,points)

  #2. check input--------------------------------------------------
  check_dir(plotdir,output)
  check_SPI(index)
  if(!is.null(plot_args$topo)) check_topo(plot_args$topo)
  dat=get_data_list(c("dat"))
  check_class(dat[!sapply(dat,is.null)],"climindvis")
  check_type_error(dat,"fc")
  check_fcmon(dat[!sapply(dat,is.null)])
  #if (!is.null(selyears) & !all(diff(selyears)) == 1) stop("if trends are calculated, selagg has to consist of consecutive years")
  switch(is_index_special(index)+1,selyears_data<-selyears,selyears_data<-NULL)
  dat<-cut_to_same_dates(dat,selyears_data)
  if(grid==1){
    indexvar=get_indexvar(index,index_args)
    plot_args$mask <-dat_grid$mask[[indexvar]]
  }

  # 2. calculate index --------------------------------------------------

  if (is.null(index_args$trend)){index_args$trend = TRUE}

  ind_dat = lapply(dat, function(dd) do.call("calc_index", c(list(dd, index = index), index_args)))


  # 3. Plotten --------------------------------------------------

  ## 3.1. get aggregations---------------------------------------------------
  sig_val <- 1-sig_lev
  if(points==1){
    plot_args$p_pch <- c(24,25)
    plot_args$trend <- TRUE}
  if (is.null(plot_args$g_breaks[1]) & is.null(plot_args$g_nlev)) {
    plot_args$g_nlev=10
  }
  if (grid==0 & is.null(plot_args$p_nlev)){
    plot_args$p_nlev =10}

  # get trend data and check for significance
  iname <- unique(lapply(ind_dat, function(x) x$index_info$iname) )
  if(abs==FALSE){
    if(is.element(iname, c("TNN","TNX","TXN","TXX","sum tmax","sum tmin", "sum tavg","mean tmax","mean tmin", "mean tavg"))){
      abs <- TRUE
      message("For this index the relative trend is not calculated. Absolute trend is plotted.")
    }
  }
  tdat<-get_trend_data(ind_dat,sig_val,abs = abs)
  tmet<- get_t_met(ind_dat)

  aggl=is.element("agg",ind_dat[!sapply(ind_dat,is.null)][[1]]$index_info$idims)
  aggn=ind_dat[!sapply(ind_dat,is.null)][[1]]$index_info$aggnames
  units = get_leg_units(ind_dat,trend=TRUE) #ind_dat[!sapply(ind_dat,is.null)][[1]]$index_info$iformat

  #for (aa in 1:ifelse(aggl,1:length(aggn),1)){
    for (aa in 1:length(aggn)){

    #cut out plot data pdat for aggregation
    pdat=lapply((names(tdat)), function(ii){
      if(!is.null(tdat[[ii]])){
        if(aggl & length(aggn)>1){
          cd=ifelse(ind_dat[[ii]]$data_info$type=="p",1,2)
          ret=index_array(tdat[[ii]],cd+1,list(aa),drop=TRUE)
        } else ret=tdat[[ii]]
      } else ret=NULL
    })
    names(pdat)=names(tdat)

    if (grid==1){
      plot_args$mask_NA <- array(NA,dim=dim(pdat$dat_grid))
      plot_args$mask_NA[is.na(pdat$dat_grid)] <- 1
      if(!all(unlist(lapply(pdat, is.na)))) {
        # define limit as q80 of trend distribution
        new_breaks<-get_breaks(pdat, center=TRUE) #Farbe fuer Stationen und Grid immer dieselbe
        plot_args$g_breaks <- new_breaks$breaks
        plot_args$outliers <- new_breaks$outliers
        plot_args$g_col <- colorRampPalette(RColorBrewer::brewer.pal(8,"PRGn"))(length(plot_args$g_breaks)-1)

        lb<- length(new_breaks$breaks)
        pdat$dat_grid[pdat$dat_grid>new_breaks$breaks[lb] ]<- new_breaks$breaks[lb]
        pdat$dat_grid[pdat$dat_grid<new_breaks$breaks[1] ]<- new_breaks$breaks[1]
      } else {
        plot_args$g_breaks <- 0
        message("No significant trends for grid at this significance level.")

      }



    }
    if(points==1){
      if (grid==1){
        plot_args$p_breaks <- plot_args$g_breaks
        plot_args$p_col <- plot_args$g_col
        plot_args$p_pch <- ifelse(is.na(pdat$dat_p),p_ch <-23,ifelse(pdat$dat_p>=0, p_ch <-24, p_ch <- 25))
        plot_args[c("p_col_info","p_legend")] <- list("same")

      } else {
        if (all(is.na(pdat$dat_p))) {
          message("No significant trends for stations this significance level")
          upper_lim<-0
          # sonst gibt es Error spaeter im script
          plot_args$p_breaks <- 0
          plot_args$outliers <- FALSE
          plot_args[c("p_pch_s","p_pch")] <-list( ifelse(is.na(pdat$dat_p),p_ch <- 23,ifelse(pdat$dat_p>=0, p_ch <-24, p_ch <- 25)))
          plot_args[c("p_col_info","p_legend")] <- "cbar"

        } else {
          new_breaks<-get_breaks(pdat$dat_p,center = TRUE)
          plot_args$p_breaks <- new_breaks$breaks
          plot_args$outliers <- new_breaks$outliers
          plot_args$p_col <- colorRampPalette(RColorBrewer::brewer.pal(8,"PRGn"))(length(plot_args$p_breaks)-1)
          plot_args[c("p_pch_s","p_pch")] <-list( ifelse(is.na(pdat$dat_p),p_ch <- 23,ifelse(pdat$dat_p>=0, p_ch <-24, p_ch <- 25)))
          plot_args[c("p_col_info","p_legend")] <- list("cbar")
        }
      }
      lb<- length(new_breaks$breaks)
      pdat$dat_p[pdat$dat_p>new_breaks$breaks[lb] ]<- new_breaks$breaks[lb]
      pdat$dat_p[pdat$dat_p<new_breaks$breaks[1] ]<- new_breaks$breaks[1]
      pdat_help <- abs(pdat$dat_p)
      p_dat_size <- (pdat_help-min(pdat_help, na.rm=TRUE))/(max(pdat_help, na.rm=TRUE)-min(pdat_help, na.rm=TRUE))
      plot_args$p_cex <- p_dat_size+1
      plot_args$p_cex[is.na(plot_args$p_cex)] <- 1
    }

    pnames=get_plot_title(titlestring=title,show_title=plot_title,autoplot="trend_map",aa,abs=abs,sig_lev=sig_lev, tmet=tmet)
     # plot arguments
    plot_args[c("output","outfile","plot_title","units")]<-list(output,pnames$f,pnames$t,ifelse(abs==TRUE, paste0("[",units, "/decade]"), "[%]"))

    if(grid==1 & points==1){
      #plot grid and points
      do.call("plot_map_grid_points",c(list(g_dat=pdat$dat_grid,g_lon=ind_dat$dat_grid$lon,g_lat=ind_dat$dat_grid$lat,p_dat=pdat$dat_p,p_lon=ind_dat$dat_p$lon,p_lat=ind_dat$dat_p$lat),plot_args))
    } else if (grid==1 & points==0){
      #plot only grid
      do.call("plot_map_grid_points",c(list(g_dat=pdat$dat_grid,g_lon=ind_dat$dat_grid$lon,g_lat=ind_dat$dat_grid$lat),plot_args))
    } else if (grid==0 & points==1){
      do.call("plot_map_grid_points",c(list(p_dat=pdat$dat_p,p_lon=ind_dat$dat_p$lon,p_lat=ind_dat$dat_p$lat),plot_args))
    }
  } # end aa

} # end of function




get_trend_data<-function(ind_dat,sig_val,abs){
  tdata=lapply(ind_dat, function (ii){
    if (!is.null(ii)){
      dims_t<-dim(ii$trend_info)
      tdat<- index_array(ii$trend_info, dim=length(dims_t),value=ifelse(abs==TRUE,5,4), drop=TRUE)
      tsig <- index_array(ii$trend_info, dim=length(dims_t),value=3, drop=TRUE)
      tsig[tsig>sig_val] <- NA
      tsig[tsig<=sig_val] <- 1
      tval=tdat*tsig
      tval[tval==-99.9] <- NA
      return(tval=tval)
    } else return(NULL)
  })

  return(tdata)
}

get_t_met<-function(ind_dat){
  tdata=lapply(ind_dat, function (ii){
    if (!is.null(ii)){
      dims_t<-dim(ii$trend_info)
      tmet<- unique(index_array(ii$trend_info, dim=length(dims_t),value=6, drop=TRUE))
       return(tmet)
    } else return(NULL)
  })
  return(tdata)
}