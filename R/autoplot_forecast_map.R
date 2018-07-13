#' Wrapper function for plotting maps of seasonal forecasts
#'
#'Generates maps showing the forecast probability of the dominant forecast category for a  seasonal forecasts of indices. Additionally a skill criterion can be applied which omits showing the forecasts with a skill lower than a user defined minimum skill. \cr
#'The function works with gridded and/or station data and first calculates the chosen index (using \code{\link{calc_index}}) for hindcasts and forecasts and then divides the forecast into categories (e.g. terciles) based on the climatology of the hindcasts. If the skill criterion is applied, indices are additionally calculated for observational data which is then used for verification (using \code{\link{verify_index}}). The output of the function is one plot per temporal aggregation which can directly be saved to a user specified directory.  \cr
#'To see some example outputs of this function, look at the vignette "autoplot-functions" accessible through the package help main page or by typing \cr
#'\emph{vignette("autoplot-functions",package="ClimIndVis")} into the console.
#'
#' @inheritParams plot_doc
#' @param col Color scale for plotting. Array of same length as ncat/(prob-1) with entries being valid arguments to \code{\link[grDevices]{col2rgb}}. If not provided, package default colors for index are used.
#' @param nocat_col Color for marking grid point/stations with no dominant category . Default="grey"
#' @param noskill_col Color for marking grid point/stations without skill (only if skillmin!=NULL). Default="black"
#' @inheritParams veri_doc
#' @section Output:
#' This function returns one plot per temporal aggregation for which data is available, e.g. if index_args$aggt="monthly", for all months which are covered by the forecast.
#'
#' If output is not NULL or "dev.new" the graphics are directly saved in the user specified directory (\emph{plotdir}) with the following filename structure: \cr
#'
#'     \emph{plotname} // "forecast_" // indexname // (skill_criterion) // aggregation // forecast_year // forecast_month // hindcast_years // \emph{output}\cr e.g. \cr
#'     \emph{plotname}_forecast_dry_days_MAM_2018_fcmon01_hc1981-2010.png \cr
#'      \emph{plotname}_forecast_dry_days_EnsRocss>=0.3_MAM_2018_fcmon01_hc1981-2010.png
#'
#'
#' @section Details:
#'
#' This function visualizes the probability of the dominant forecast category where the color shows the category (e.g. for a tercile forecast of dry days; a:below-normal, b:normal, c:above-normal one color for each category) and the inverse transparency the probability value of the forecast for this category (e.g. for a forecast a:0\%,b:0\%,c:100\% ).transparency=0\%).
#'
#' Gridpoints/stations colored in the color  \emph{nocat_col} either don't show a dominant forecast category or the index value is NA. The latter can occur e.g. for the start of the rainy season if the index criteria are not met within the specified time period or if the skill criterion is applied and the verifying observations are NA. If the probabilities of two or more categories are equal (e.g. a:35,b:35,c:30) no dominant category can be determined. This also applies when calculating a category forecast does not make sense , e.g. in areas where the number of frost days is always zero or maybe only in one or two years slightly above zero and the category boundaries derived from the hindcasts are all equal.
#'
#' If the skill criterion is applied, grid points/stations with a lower skill than defined in \emph{skillmin} are colored in \emph{noskill_col}. If skill is calculated, values where the verifying observations are NA will also be shown as NA. Note that when using the category functions, only probability thresholds can be used to calculate forecast categories. If \emph{verify_metric} is EnsRoca or EnsRocss, the skill value of the dominant forecast category is taken.
#'
#'

#' @examples
#'
#' ## Load example climindvis objects:
#'
#' data("object_hc_grid", "object_fc_grid", "object_hc_st", "object_fc_st", "object_st", "object_grid")
#'
#'
#' ## If you use the default values, the only inputs needed are the climindvis objects,
#' ## the index and the index_args, e.g. for a tercile dry day forecast
#' ## for MAM without skill information:
#'
#' autoplot_forecast_map(
#'      fc_grid = object_fc_grid, hc_grid = object_hc_grid,
#'      fc_p = object_fc_st, hc_p = object_hc_st,
#'      index = "dd", index_args = list(aggt = "seasonal", selagg = "MAM"))
#'
#'
#' ## If you additionally want to cut out all stations with a EnsRocss < 0.3 you have to add:
#' ## veri_metric = "EnsRocss" , skill_min = 0.2.
#'
#' autoplot_forecast_map(
#'       fc_grid = object_fc_grid, hc_grid = object_hc_grid, obs_grid = object_grid,
#'       fc_p = object_fc_st, hc_p = object_hc_st, obs_p = object_st,
#'       veri_metric = "EnsRocss", skillmin = 0.3,
#'       index = "dd", index_args = list(aggt = "seasonal", selagg = "MAM"))
#'
#' @family autoplot_functions
#' @export

autoplot_forecast_map<-function(
  fc_grid, hc_grid, hc_ref_grid = NULL, obs_grid = NULL,
  fc_p, hc_p, hc_ref_p = NULL, obs_p = NULL,
  index, index_args = list(), selyears = NULL,
  veri_metric, veri_args = list(), skillmin = NULL,
  col = "default", noskill_col = "black", nocat_col = "grey", prob, ncat = 3, cat_names = paste0("cat", 1:ncat),
  plot_title = TRUE, plot_args = list(),
  output = NULL, plotdir, plotname = "", title = "") {

  # 1. check if point or grid --------------------------------------------------

  grid <- ifelse(missing(fc_grid) | missing (hc_grid),0,1)
  points <- ifelse (missing(fc_p) | missing (hc_p),0,1)
  check_input(grid,points)

  #2. check input--------------------------------------------------
  check_dir(plotdir,output)
  check_SPI(index)
  if(!is.null(plot_args$topo)) check_topo(plot_args$topo)
  tryCatch({
    if (!missing(veri_metric) & is.null(skillmin)) stop("both <<veri_metric>> and <<skillmin>> need to be passed to function.")
    if (!is.null(skillmin)){
      if(missing(veri_metric)) stop("<<veri_metric>> needs to be specified if <<skillmin>>!=NULL")
      if(grid==1) if(is.null(obs_grid)) stop("gridded observations <<obs_grid>> need to be provided for verification of gridded data if <<skillmin>>!=NULL ")
      if(points==1) if(is.null(obs_p)) stop("point observations <<obs_p>> need to be provided for verification of point data <<skillmin>>!=NULL")
    }
  }, error=function(cond){
    message("error in input data:")
    message(cond)
    stop_quietly()
  })
  if(grid==1){
    indexvar=get_indexvar(index,index_args)
    plot_args$mask <-fc_grid$mask[[indexvar]]
  }


  #3. get and check data--------------------------------------------------
  dat=get_data_list(c("fc","hc","hc_ref","obs"))
  # skip_index==FALSE
  # tryCatch({
  check_class(dat[!sapply(dat,is.null)],"climindvis")
  # }, error=function(cond){
  #   tryCatch({
  #   check_class(dat[!sapply(dat,is.null)],"climindvis_index")
  #   }, error=function(cond2){
  #     message("problem with input data:")
  #     message("input data is not of type <<climindvis>> nor of type <<climindvis_index>>")
  #     stop_quietly()
  #   }, finally = {skip_index=TRUE})})
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

  #5. verify indices ----------------------------------------------------------
  if (!is.null(skillmin)){
    veri_dat<-call_verify_helper_autoplot(ind_dat,veri_metric,veri_args,grid,points)
    if (veri_dat$error!=0){
      message("Skill won't be used in graphic")
      skillmin=NULL
    }
    veri_dat[["error"]]=NULL
  }
  # 4. calc category values -------------------------------------------------
  if(missing(prob)) prob= 1:(ncat-1)/ncat
  cats=get_cats_fc(ind_dat,grid,points,probs=prob,prob=TRUE)

  maxcat= lapply(cats, function(x) {
    if(!is.null(x)){
      apply(x$val,1:(ndims(x$val)-1),sel_maxcat)
    } else NULL})

  maxcat_value= lapply(cats, function(x) {
    if(!is.null(x)){
      apply(x$val,1:(ndims(x$val)-1),sel_maxcat_value)
    } else NULL})

  #5. calc plot_values-------------------------------------------------

  pval=mapply(function(mc,mcv) {
    if(!is.null(mc) & !is.null(mcv)){
      pv<-mcv+mc*100
      pv[is.na(mc) | mc==0 | mc==-999]=50
      return(pv)
    } else return(NULL)
  },maxcat,maxcat_value)

  if(!is.null(skillmin)){

    if((veri_dat[[ifelse(points==1,1,2)]]$veri_info[veri_metric,"nout"]!=1)){ # erweitern auf andere Skillmetriken mit Kategorien
      skill = get_skill_maxcat(veri_dat,maxcat,veri_metric)
    } else  skill = lapply(veri_dat, function(v) v$verification[[veri_metric]])

    pval=mapply(function(pv,sk){
      if(!is.null(pv) & !is.null(sk)){
        pv[sk<skillmin]=(ncat+1.55)*100
        pv[is.na(sk)]=50
        return(pv)
      } else return(NULL)
    },pval,skill)

  }

  #6. plot_arguments-------------------------------------------------
  if(col == "default"){
    ch = grDevices::colorRampPalette(get_default_color_fc(index,  ind_dat$fc_p$index_info$iname)$col)(ncat)
  } else {
    ch=grDevices::colorRampPalette(col)(ncat)
  }

  get_plot_args(autoplot="forecast_map")

  # 5. plot forecasts ---------------------------------------------------------

  yl<-ifelse(is.null(cats[[1]]$dims$yd),FALSE,TRUE)
  aggl<-ifelse(is.null(cats[[1]]$dims$aggd),FALSE,TRUE)

  for (yy in 1:ifelse(yl,dim(pval[[1]])[cats[[1]]$dims$yd],1)){
    for (aa in 1: ifelse(aggl,dim(pval[[1]])[cats[[1]]$dims$aggd],1)){

      pdat<-mapply(function(pv,i){
        if (!is.null(pv) & !is.null(i)){
          index_array(pv,dim=c(i$dims$aggd,i$dims$yd),
            value=c(switch(is.null(i$dims$aggd)+1,list(aa),NULL),switch(is.null(i$dims$aggd)+1,list(yy),NULL)),drop=TRUE)
        } else NULL
      },pval,cats)

      if (grid==1){
        plot_args$mask_NA <- array(NA,dim=dim(pdat$grid))
        plot_args$mask_NA[is.na(pdat$grid)] <- 1
      }

      error=sum(sapply(pdat, function(pp){
        if (!is.null(pp)){
          ifelse(all(is.na(pp)),1,0)
        } else 0}))

      if (error==0){

        pnames=get_plot_title(titlestring=title,show_title=plot_title,autoplot="forecast_map",aa,yy)
        plot_args[c("fc_plot","output","outfile","plot_title")]<-list(TRUE,output,pnames$f,pnames$t)

        if(grid == 1 & points == 1){
          do.call("plot_map_grid_points", c(list(g_dat = pdat$grid, g_lon = ind_dat$fc_grid$lon, g_lat = ind_dat$fc_grid$lat,
            p_dat = pdat$p, p_lon = ind_dat$fc_p$lon, p_lat = ind_dat$fc_p$lat, p_col_info = "same"), plot_args))
        } else if (grid == 1 & points == 0){
          plot_args[c("p_col", "p_col_center")] = NULL
          do.call("plot_map_grid_points", c(list(g_dat = pdat$grid, g_lon = ind_dat$fc_grid$lon, g_lat = ind_dat$fc_grid$lat), plot_args))
        } else if (grid == 0 & points == 1){
          do.call("plot_map_grid_points", c(list(p_dat = pdat$p, p_lon = ind_dat$fc_p$lon, p_lat = ind_dat$fc_p$lat,
            p_col_info = "cbar", p_legend = "cbar"), plot_args))
        }

      } #error
    } #aa
  } #yy
}



