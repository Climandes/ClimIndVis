#' Wrapper function for visualizing forecast for each station (optionally including skill information and climatology)
#'
#' This function generates plots showing the forecast probabilities of category forecasts of indices at station level as a tacho plot. The dominant forecast category is marked by an arrow.\cr
#'  The forecast categories (e.g. terciles) are determined based on the climatology of the hindcasts. Additionally a skill criterion can be applied and forecasts with skill lower than a minimum skill value are overlayed by a message which can be edited by the user.\cr
#'To see some example outputs of this function, look at the vignette "autoplot-functions" accesible through the package help main page or by typing \cr
#'\emph{vignette("autoplot-functions",package="ClimIndVis")} into the console.
#'
#' @inheritParams plot_doc
#' @inheritParams veri_doc
#' @param verify Logical. Should skill criterion be applied?
#' @param col Plot colors, array of same length as ncat. If not provided, package default colors for index are used.
#'
#'@section Output:
#' This function returns one plot per temporal aggregation which is covered by the hindcasta and forecasts (e.g. if aggt="seasonal" and the forecast is issued in Jan for 7 months it would return one graphic for "MAM").
#'
#' If output is not NULL or "dev.new" the graphics are directly saved in the user specified directory (\emph{plotdir}) with the following filename structure: \cr
#'
#'     \emph{plotname} // "forecast_" // indexname // (skill_criterion) // aggregation //   forecast_year // forecast_month // hindcast_years // station name //\emph{output}\cr  e.g. \cr
#'     \emph{plotname}"_forecast_dry_days_MAM_2018_fcmon01_hc1981-2010_station2.png \cr
#'      \emph{plotname}"_forecast_dry_days_EnsRocss>=0.3_MAM_2018_fcmon01_hc1981-2010_station2.png
#'

#' @section Details:
#'
#' The shaded area of the tacho plot is proportional to the probability of the respective categeory, e.g. an empty tacho slice means 0\% probability, a full slice denotes 100\%. The dominant forecast category is marked by an arrow. \cr
#'
#' If the forecast does not show a dominant forecast category (e.g. if the probabilites for a tercile forecast are cat1:35\%, cat2:35\%, cat3:30\%) no arrow will be drawn and the plot will be overlayed by a message (default \emph{text_nocat}="no dominant category").\cr
#'
#' If the forecast for the chosen index shows no or little variation in climatology issuing a forecast does not make much sense. Therefore the function checks whether the category boundaries derived from the hindcast are identical and if so the plot is also overlayed by a message (default \emph{text_novar}="no or little variation in climatology). \cr
#'
#' Optionally a skill criterion can be applied where the plot is overlayed by a message if the skill of the forecast is lower than the user defined minimum skill (default \emph{text_noskill} = "no skill").\cr
#'
#'  The labels of the categorie can be changed using the parameter \emph{cat_names}\cr.
#' If \emph{plot_climatology}=TRUE, the function calls the function autoplot_ts_stations (see \code{\link{autoplot_ts_stations}}) and a seperate plot will be created with the cliamtology and tercile boundaries for each station. In the future, the possibility of plotting both climatology and forecast into one plot will be implemented in the package.
#'
#' @examples
#'
#' ## load example data:
#' data(object_st, object_hc_st, object_fc_st)
#'
#' autoplot_forecast_stations(
#'       fc_p = object_fc_st, hc_p = object_hc_st, obs_p = object_st,
#'       index = "fd",  index_args = list(aggt = "other", aggmons = c(2:5)),
#'       verify = FALSE)
#'
#' ## if you want to additionally plot the climatology,  you have to set plot_climatology = TRUE:
#' autoplot_forecast_stations(
#'       fc_p = object_fc_st, hc_p = object_hc_st, obs_p = object_st,
#'       index = "fd",  index_args = list(aggt = "other", aggmons = c(2:5)),
#'       verify = FALSE, plot_climatology = TRUE)
#'
#' ## for output to file add e.g. output = "png", plotdir = "...directory for plotting..."
#'
#' @family autoplot_functions
#' @export

autoplot_forecast_stations<-function(
  fc_p, hc_p, hc_ref_p = NULL, obs_p = NULL,
  index, index_args = list(), selyears = NULL,
  verify = TRUE, veri_metric, veri_args = list(),  skillmin = NULL,
  text_noskill = "no skill", text_novar = "no or little variation in climatology", text_nocat = "no dominant category",
  col = "default", prob, ncat = 3, cat_names = paste0("cat", 1:ncat),
  plot_climatology = FALSE, plot_title = TRUE,
  output = NULL, plotdir, plotname = "", title = "") {

  #1. checks ------------------------------------------------------------------

  tryCatch({
    check_dir(plotdir,output)
    if(grepl("SPI",index)) stop("For plotting SPI please use <<autoplot_ts_stations>> or <<plot_spi_forecast>>")
    if (missing(fc_p) | missing(hc_p)) stop("forecast and hindcast data (<<fc_p>> and <<hc_p>>) have to be provided")
    if (verify & missing(veri_metric)) stop("<<veri_metric>> needs to be specified if <<verify>>=TRUE")
    if (verify & missing(obs_p)) stop("if <<verify>>=TRUE obs_p is needed for verification")
    if (!is.null(skillmin) & !verify) warning("if <<verify>> =FALSE, skillmin is ignored and skill won't be plottet")
    if (plot_climatology & missing(obs_p)) stop("observations <<obs_p>> need to be provided for climatology")
    if (index_args$aggt=="xdays") stop("aggt=xdays is not implemented for forecasts")
  }, error=function(cond){
    message("error(s) in input data:")
    message(cond)
    stop_quietly()
  }, warning=function(cond){
    message("warning about input data:")
    message(cond)
  })


  #2. get and check data--------------------------------------------------
  points=1
  grid=0
  dat=get_data_list(c("fc","hc","hc_ref","obs"))
  switch(is_index_special(index)+1,selyears_data<-selyears,selyears_data<-NULL)
  dat<-cut_to_same_dates(dat,selyears_data)
  #checks
  #check class
  check_class(dat[!sapply(dat,is.null)],"climindvis")
  #check spatial dims
  check_spatial_dims(dat[!sapply(dat,is.null)])
  #check if forecast month is equal for all hc
  check_fcmon(dat[!sapply(dat,is.null)])


  #3. calculate indices -------------------------------------------------------
  if(missing(index_args)) index_args=list()
  index_args$trend = FALSE
  if(!is_index_special(index)){
    ind_dat<-lapply(dat, function(dd) do.call("calc_index",c(list(dd,index=index),index_args)))
  } else {
    ind_dat<-calc_index_special_autoplot(dat,index,index_args, selyears)
  }


  # 4. verify indices ----------------------------------------------------------
  if (verify){
      veri_dat<-call_verify_helper_autoplot(ind_dat,veri_metric,veri_args,grid=0,points=1)
      if (veri_dat$error!=0){
        skillmin=NULL
        verify=FALSE
        warning("warning in verify_index:skill won't be used in graphic")
      }
    veri_dat$error<-NULL
  }

  #4. calculate terciles
  if(missing(prob)) prob= 1:(ncat-1)/ncat
  cats=get_cats_fc(ind_dat,grid,points,probs=prob,prob=TRUE)

  maxcat= lapply(cats, function(x) {
    if(!is.null(x)){
      apply(x$val,1:(ndims(x$val)-1),sel_maxcat)
    } else NULL})


  if(verify){
    if(veri_dat[[1]]$veri_info[veri_metric,"nout"]!=1){ # erweitern auf andere Skillmetriken mit Kategorien
      skill = get_skill_maxcat(veri_dat,maxcat,veri_metric)
    } else  skill = lapply(veri_dat, function(v) v$verification[[veri_metric]])

    noskill=veri_dat[[1]]$veri_info[veri_metric,"noskill"]
    maxskill=veri_dat[[1]]$veri_info[veri_metric,"lim2"]
    skill=mapply(function(sk,mc){
      if(!is.null(sk) & !is.null(mc)){
        if(any(ndims(mc) != ndims(sk))) mc=index_array(mc,ndims(mc),list(1),drop=TRUE)
        if(any(is.na(mc))) sk[is.na(mc)] = NA
        sk[mc==0] = NA
      } else sk=NULL
      return(sk)
    },skill,maxcat)

  } else{
    noskill=0
    maxskill=1
  }

  # plot data
  if (col == "default"){
    ch = grDevices::colorRampPalette(get_default_color(index, ind_dat$fc_p$index_info$iname,fc=TRUE)$col)(ncat)
  } else {
    ch=grDevices::colorRampPalette(col)(ncat)
  }

  breaks=seq(0,100,100/ncat)
  breaksm=seq(0,100,100/(ncat*2))
  r1=0
  r3=1.1

  # ier ist ein Problem mit der Auswahl!!!
  pos<-lapply(maxcat, function(mc) {
    if(!is.null(mc)){
      pos=apply(mc,1:ndims(mc), function(m){
        if (is.na(m)){ NA
        } else if (m==0){ 0
        } else if (m==-999){ -999
        } else breaksm[m*2]})
      return(pos)
    }else return(NULL)
  })


  # 5. plot forecasts ---------------------------------------------------------
  nst=length(ind_dat$fc_p$lon)
  yl<-ifelse(is.null(cats[[1]]$dims$yd),FALSE,TRUE)
  aggl<-ifelse(is.null(cats[[1]]$dims$aggd),FALSE,TRUE)

  for (pp in 1:nst){
    for (yy in 1:ifelse(yl,dim(cats[[1]]$val)[cats[[1]]$dims$yd],1)){
      for (aa in 1: ifelse(aggl,dim(cats[[1]]$val)[cats[[1]]$dims$aggd],1)){

        ppos<-mapply(function(po,i){
          index_array(po,dim=c(1,i$dims$aggd,i$dims$yd),
            value=c(list(pp),switch(is.null(i$dims$aggd)+1,list(aa),NULL),switch(yl+1,NULL,list(yy))),drop=TRUE)
        },pos,cats)

        cprobs<-lapply(cats,function(cc){
          index_array(cc$val,dim=c(1,cc$dims$aggd,cc$dims$yd),
            value=c(list(pp),switch(is.null(cc$dims$aggd)+1,list(aa),NULL),switch(yl+1,NULL,list(yy))),drop=TRUE)
        })

        if(verify){
          pskill<-mapply(function(sk,i){

            index_array(sk,dim=c(1,i$dims$aggd),
              value=c(list(pp),switch(is.null(i$dims$aggd)+1,list(aa),NULL)),drop=TRUE)
          },skill,cats)
        } else {
          pskill=list()
          pskill[c("p","grid")]=maxskill
        }


        if (!is.na(ppos$p)){
          if (is.null(skillmin)) skillmin=noskill
          if(ppos$p==0) { message_text=text_novar
          } else if (verify & !is.na(pskill$p)){
            if (pskill$p<skillmin) { message_text=text_noskill
            } else message_text=NULL
          } else if (ppos$p==-999) { message_text=text_nocat
          } else message_text=NULL

          pnames=get_plot_title(titlestring=title,show_title=plot_title,autoplot="forecast_point",aa,yy,pp)

          #if (!plot_climatology){
            plot_tacho(filename=pnames$f,pos=ppos$p,perc_cat=switch(is.na(pskill$p)+1,cprobs$p,rep(0,ncat)),colors=ch,pskill$p,skillmin,noskill,maxskill,breaks,r1=0,r3=1.1,tacholabs=cat_names,message_text=message_text,title=pnames$t)

        } #no values
      } #agg
    } #yy
  } #points


   if(plot_climatology){

     if (!is.null(output)) {
       if (output!="dev.new") {
         if(!dir.exists(paste0(plotdir,"climatology/")))
         dir.create(paste0(plotdir,"climatology/"))
         filenamestring=paste0(replace_operator_name(ind_dat$fc_p$index_info$iname),"_climatology")
         plotdir_clim=paste0(plotdir,"climatology/")
         out=1
       } else out=NULL
       } else out=NULL
       if (is.null(out)){
         plotdir_clim=NULL
         filenamestring=""
       }
       if (is.element(index_args$aggt,c("seasonal","monthly"))) index_args$selagg=ind_dat$fc_p$index_info$aggnames
     autoplot_ts_stations(obs_p, index, index_args,trendplots = FALSE,shading=TRUE,nshading=ncat,col_shading=paste0(ch,80),pcols="black",
       output = output,plotdir=plotdir_clim,plotname=filenamestring,title = "",plot_title=TRUE,
       type = "single_ts",selyears=selyears)



   }




}


