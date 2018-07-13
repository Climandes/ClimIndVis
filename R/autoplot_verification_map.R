#' Wrapper function to calculate and visualize verification of indices
#'
#' This function combines the functions  \code{\link{calc_index}},\code{\link{verify_index}} to generate skill maps for hindcast (re-forecasts). It can be used for either gridded or station data or a combination of both.\cr
#' To see some example outputs of this function, look at the vignette "autoplot-functions" accesible through the package help main page or by typing \cr
#'  \emph{vignette("autoplot-functions",package="ClimIndVis")} into the console.
#'
#' @inheritParams plot_doc
#' @param col Plot colors for colorscale of index. If not provided, package default colors for skill metric are used.
#' @section Output:
#' This function returns one plot per verification metric (and category in the case of the ROCSS and year in the case of e.g. the RPS) and all temporal aggregations which are covered by the hindcasts  e.g. if aggt="seasonal" and the forecast is issued in Jan for 7 months it would return one graphic for "MAM"). The function creates folders for each verification metric in the output directory.
#'
#' If output is not NULL or "dev.new" the graphics are directly saved in the user specified directory (\emph{plotdir}) with the following filename structure: \cr
#'
#'     \emph{plotname} // verification metric // (category name for category fc) // (year for RPS/CRPS) // index name // aggregation //  forecast_month // hindcast_years // \emph{output} \cr e.g.: \cr
#'     \emph{plotname}"_EnsCorr_dry_days_MAM_fcmon01_hc1981-2010.png \cr
#'      \emph{plotname}"_EnsRocss_cat2_dry_days_MAM_fcmon01_hc1981-2010.png

#' @section Details:
#' The verification functions are all based on the package \code{\link{easyVerification}}.
#' @examples
#'
#' ## load data (see \code{\link{example_climindvis_objects}}:
#' data("object_hc_grid", "object_hc_st", "object_st", "object_grid")
#'
#' ## Verification of index = "dd"  and season MAM. Output to console.
#'
#' autoplot_verification_map(
#'       obs_grid = object_grid, hc_grid = object_hc_grid,
#'       obs_p = object_st, hc_p = object_hc_st,
#'       index = "dd", index_args = list(aggt = "seasonal", selagg = "MAM"),
#'       veri_metrics = c("EnsCorr", "EnsRocss"))
#'
#'@family autoplot_functions
#'
#' @export
#'

autoplot_verification_map <-function(
  obs_grid, hc_grid, hc_ref_grid = NULL,
  obs_p, hc_p, hc_ref_p = NULL,
  index, index_args = list(), selyears = NULL,
  veri_metrics, veri_args = list(),
  col = "default", ncat=3,
  output = NULL, plotdir, plot_title = TRUE, plotname = "", title = "", plot_args = list()) {


  # 1. check if point or grid --------------------------------------------------
  grid <- ifelse(missing(obs_grid) | missing (hc_grid),0,1)
  points <- ifelse (missing(obs_p) | missing (hc_p),0,1)
  if(grid==0 & points==0) stop("either grid or point data have to be provided as input")


  #2. check input--------------------------------------------------
  check_dir(plotdir,output)

  #3. get and check data--------------------------------------------------
  dat=get_data_list(c("obs","hc","hc_ref"))
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
  veri_metrics<-skill_scores_only(dat,veri_metrics) #wird spaeter in verfy_index nochmal gecheckt aber wenn es hier einen Fehler gibt dann muss man Indikatoren erst gar nicht berechnen.


  #4. calculate index --------------------------------------------------
  index_args$trend = FALSE

  ind_dat<-lapply(dat, function(dd) do.call("calc_index",c(list(dd,index=index),index_args)))

  if(is_index_special(index) & !is.null(selyears)){
    ind_dat<-cut_to_same_dates_index(ind_dat,selyears)
  }

  #5. . Verifikation --------------------------------------------------
  # verification function only uses matching years and aggregations

  veri_dat<-call_verify_helper_autoplot(ind_dat,veri_metrics,veri_args,grid,points)
  if (veri_dat$error!=0){
    stop("skill cannot be calculated due to error in verify_index function")
  }
  veri_dat$error=NULL


  #   4. Plotten --------------------------------------------------

  #1 Plot pro Metrik + Aggregation -> dimnames evtl in Verifikationfunktion dazufuegen

  zlims_in=plot_args$zlims
  pzlims_in=plot_args$p_zlims

  for (vv in veri_metrics){

    tdims=Reduce(union,lapply(veri_dat, function(dd) if (!is.null(dd)) dd$veri_info[vv,"tdim"]))
    tdims=ifelse(tdims=="ny",length(ind_dat[[1]]$index_info$years),as.integer(tdims))
    nout=Reduce(union,lapply(veri_dat, function(dd) if (!is.null(dd)) dd$veri_info[vv,"nout"]))
    if (nout=="ncat") nout=ncat

    plot_args[c("zlims","p_zlims")]=NULL
    zlims=unlist(unique(lapply(veri_dat, function(dd) dd$veri_info[vv,c("lim1","lim2")])))

    if(grid==1 & all(is.null(zlims_in))& !all(is.na(zlims))) plot_args$zlims=zlims
    if(grid==0 & all(is.null(pzlims_in))& !all(is.na(zlims))) plot_args$p_zlims=zlims

    if(col=="default"){
      vcol=switch(any(is.na(zlims))+1,gplots::bluered(10),gplots::greenred(10))
    } else vcol=col
    vcol[which(vcol=="#FFFFFF")]="#BFBFBF"


    aggl<-ifelse(is.element("agg",ind_dat[[1]]$index_info$idims),TRUE,FALSE)
    for (aa in 1:ifelse(aggl,length(ind_dat[[1]]$index_info$aggnames),1)){
      for (tt in 1:tdims){
        for (cc in 1:nout){
          tname=switch((tdims>1)+1,NULL,ind_dat[[1]]$index_info$years[tt])
          cname=switch((nout>1)+1,NULL,paste0("cat",cc))
          pskill=get_skill_data(veri_dat,vv,aa,tt,cc,aggl,nout,tdims)

          pnames=get_plot_title(titlestring=title,show_title=plot_title,autoplot="verification_map",aa=aa,vv=vv,cname=cname,tname=tname)
          plot_args[c("output","outfile","plot_title")]<-list(output,pnames$f,pnames$t)
          # verallgemeinern!!!
          if (grid==1 & points==1 ){
            do.call("plot_map_grid_points",c(list(g_dat=pskill$grid,g_lon=veri_dat$grid$lon,g_lat=veri_dat$grid$lat,
              g_col=vcol,p_dat=pskill$p,p_lon=veri_dat$p$lon,p_lat=veri_dat$p$lat,p_col_info="same"),plot_args))

          } else if (grid==1 & points==0) {
            do.call("plot_map_grid_points",c(list(g_dat=pskill$grid,g_lon=veri_dat$grid$lon,g_lat=veri_dat$grid$lat,
              g_col=vcol),plot_args))

          } else if (grid==0 & points==1){
            do.call("plot_map_grid_points",c(list(p_dat=pskill$p,p_lon=veri_dat$p$lon,p_lat=veri_dat$p$lat,
              p_col=vcol,p_col_info="cbar",p_legend="cbar"),plot_args))
          }
        }
      }
    }
  }




}

