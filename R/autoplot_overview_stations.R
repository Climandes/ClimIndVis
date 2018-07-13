#'wrapper function to show boxplots and index values of selected years for different indices for each station.
#'
#'This function calculates different indices at station level and then combines their climatology as boxplots in one graphic. Optionally selected years can be added as points (e.g. the current year or some years of special interest.) \cr
#'To see some example outputs of this function, look at the vignette "autoplot-functions" accesible through the package help main page or by typing \cr
#'\emph{vignette("autoplot-functions",package="ClimIndVis")} into the console.
#'
#' @inheritParams plot_doc
#' @param addyears Array of years (integer or character) to add as points to the plot.
#' @param yearcols Array of same length as addyears with colors for marking selected years.
#' @param yearpchs Array of pch for each year.
#' @param selpoints Integer array of stations to select for plotting. Default= NULL (all stations are plottet).
#' @section Output:
#' This function returns one plot per station and temporal aggregation whch is covered by the data( e.g. if aggt="seasonal" it would return 4 graphics, one for each season).
#'
#' If output is not NULL or "dev.new" the graphics are directly saved in the user specified directory (\emph{plotdir}) with the following filename structure: \cr
#'
#'     \emph{plotname} // "overview_indices" // aggregation // years // addyears //\emph{output} \cr e.g.: \cr
#'     \emph{plotname}"_overview_indices_MAM_1981-2010_add2018.png \cr
#'
#'
#'@section Details:
#' The selected indices are grouped by their units (e.g. mm, \%, no of days,...) before plotting.
#'
#' @examples
#' indices = c("dd", "fd", "th", "tx90p", "cdd", "sdii")
#'
#' aggt = "seasonal"
#' indices_args = list(dd = list(aggt = aggt),
#'                    fd = list(aggt = aggt),
#'                    th = list(aggt = aggt, th = 5, thvar = "prec", op = ">"),
#'                    tx90p = list(aggt = aggt),
#'                    cdd = list(aggt = aggt),
#'                    sdii = list(aggt = aggt)
#'                    )
#'
#'
#' autoplot_overview_stations(dat_p = object_st, indices = indices, indices_args = indices_args,
#'                           addyears = 1981, selpoints = 1)
#'
#' @family autoplot_functions
#' @export
#'
#'
#'

autoplot_overview_stations<-function(dat_p,
                                    indices, indices_args, selpoints = NULL,
                                    addyears = NULL, yearcols = 1:length(addyears), yearpchs = 18, NAmaxClim = 20,
                                    output = NULL, plotdir, plotname = "", plot_title = TRUE, title = "", graphic_device) {

  check_class(dat_p,"climindvis")
  if(length(indices)!=length(indices_args) || length(indices)> 1 & !is.list(indices_args[[1]]) ) stop("<<indices_args>> needs to be a list of same length as array <<indices>> containing a list of elements for each entry of indices")


  if (is.null(dat_p$data_info$pnames)){
    message("No station names provided in object <<dat_p>> for plot title, numbers will be used")
    pnames=NULL
  } else pnames=dat_p$data_info$pnames
  # check if aggregation is the same if index is not rainy season start, wenn Index keine Aggregation hat dann extra behandeln, selagg-> alles ausserdiese Indikatoren
  years=as.character(addyears)
  dyears=range(levels(dat_p$time_factors$years))
    ilist<- lapply(1:length(indices), function(ii) do.call("calc_index", c(list(dat_p,indices[ii]),indices_args[[ii]])))
    names(ilist)=indices
    iformat=sapply(ilist, function(x) x$index_info$iformat)
    ilongnames=sapply(ilist, function(x) x$index_info$iname)
    ut=title != ""
    selagg=which(iformat!="days_since")
    noagg=which(iformat=="days_since")
    aggt=unique(sapply(ilist[selagg], function (ia) ia$index_info$aggt))
    if (length(aggt)>1) stop("indices (except rainy_season indices) must have same aggregation")
    aggnames=unique(unlist(lapply(ilist[selagg], function (ia) ia$index_info$aggnames)))

    agg=ifelse(is.element("agg",ilist[selagg][[1]]$index_info$idims),TRUE,FALSE)


    if(length(noagg)>0){
      for (nn in noagg){
        nc=ilist[[nn]]$index_info$start_days
        iformat[nn]= paste0("days since ",substring(ilist[[nn]]$index_info$start_days,nc-4,nc))
      }
    }
    sel<-lapply(unique(iformat), function(x) which(iformat==x))
    names(sel)=c(unique(iformat))



    sels=which(sapply(sel,function(x) length(x)!=0))
    lsels=sapply(sel[sels],length)

   if(is.null(selpoints)) selpoints=1:length(dat_p$lon)
    for (pp in selpoints){
      for (aa in 1:length(aggnames)){



        if (!is.null(output)){
          if (output=="dev.new"){
            dev.new()
          } else {
            filename=paste0(plotdir,plotname,ifelse(plotname=="","","_"),"overview_indices_",aggnames[aa],"_",paste(dyears,collapse="-"),"_",ifelse(length(years)>0,paste0("add",paste(years,collapse="+"),"_")),ifelse(is.null(pnames),paste0("station",pp),pnames[pp]),".",output)
          if (missing(plotdir)) stop("no outfile specified")
          if(!dir.exists(plotdir))   dir.create(plotdir)
            if(!dir.exists(plotdir)) stop("directory does not exist")
          if(missing(graphic_device)) graphic_device=list()
          if(is.null(graphic_device$height)) graphic_device$height=(sum(lsels)+(length(sels)-1)*0.25+0.5)
          if(is.null(graphic_device$width)) graphic_device$width=7
          if (is.element(output,c("png","jpeg","bmp","tiff"))){
            if(is.null(graphic_device$res)) graphic_device$res=200 # res=pres,units="in"
            if(is.null(graphic_device$units)) graphic_device$units="in"
            do.call(output,c(list(filename=filename),graphic_device))
          } else if (output == "pdf"){
            do.call(output,c(list(file=filename),graphic_device))
          }
          par(mai=c(graphic_device$width/20,graphic_device$width/3,rep(graphic_device$width/20,2)))
          }
        }

        xlab=NULL
        xat=NULL

        ifelse(plot_title,par(oma=c(1,1,4+ut,1)),par(oma=c(1,1,0.5,1)))
        graphics::plot(0,type="n",xlim=c(0,100),ylim=c(0.5,sum(lsels)+(length(sels)-1)*0.75+ifelse(is.null(years),0,1.2)),xlab="",ylab="",axes=FALSE,xaxs="i",yaxs="i") #

        if (agg){
          pdat=list()
          pdat[selagg]=lapply(ilist[selagg], function (ii) ii$index[pp,aa,])
          pdat[noagg]=lapply(ilist[noagg], function (ii) ii$index[pp,])
        } else pdat=lapply(ilist, function (ii) ii$index[pp,])

        for (ff in seq_along(sels)){
          format=names(sels)[ff]
          #xxifelse(format=="dayscount",)
          if(format!="perc"){
            vals=pretty(range(pdat[sel[[format]]],na.rm=TRUE))
            pdath=lapply(pdat[sel[[format]]], function(pp) {
                                                if (sum(!is.na(pp)) > length(pp)*NAmaxClim/100){
                                                  (pp-vals[1])*100/diff(range(vals))
                                                } else NULL})
            vals[1]=""
            vals[length(vals)] <-""
          } else {
            vals=seq(0,100,20)
            pdath=lapply(pdat[sel[[format]]], function(pp) {
              if (sum(!is.na(pp)) > length(pp)*NAmaxClim/100){
                pp
              } else NULL})
          }
          nn<-!sapply(pdath,is.null)
          if(any(nn)){
            boxplot(pdath[nn],at=((sum(lsels[0:(ff-1)])+1):sum(lsels[1:ff]) +(ff-1)*0.75+0.25)[nn],horizontal=TRUE,add = TRUE,axes=FALSE)
            if (!is.null(years)){
              for (x in 1:lsels[ff]) {
                if (nn[x]){
                  points(pdath[[x]][years],rep(((sum(lsels[0:(ff-1)])+1):sum(lsels[1:ff]) +(ff-1)*0.75+0.25)[x],length(years)),pch=yearpchs,col=yearcols,cex=3)
                  NAyears=is.na(pdath[[x]][years])
                  if(any(NAyears)){
                    text((1:length(years)*3-1)[is.na(pdath[[x]][years])],((sum(lsels[0:(ff-1)])+1):sum(lsels[1:ff]) +(ff-1)*0.75)[x],
                         rep("NA",length(years))[(is.na(pdath[[x]][years]))],col=yearcols[is.na(pdath[[x]][years])],cex=0.7,lwd=1)
                  }
                }
              }

            }
          }
          text (50,((sum(lsels[0:(ff-1)])+1):sum(lsels[1:ff]) +(ff-1)*0.75+0.25)[!nn],paste0("more than ",NAmaxClim,"% of years = NA"),cex=0.7)
          if(length(vals)>1){
            axis(side=1,pos=sum(lsels[0:(ff-1)])+0.5+(ff-1)*0.75,labels=vals,at=seq(0,100,100/(length(vals)-1)),tck=0.02,mgp=c(2,0.1,0))
          } else axis(side=1,pos=sum(lsels[0:(ff-1)])+0.5+(ff-1)*0.75,labels=c("",""),at=c(0,100),tck=0.02,mgp=c(2,0.1,0))
          text(x=50,y=sum(lsels[0:(ff-1)])+0.55+(ff-1)*0.75+0.1,convert_deg(format))

          xat=c(xat,(sum(lsels[0:(ff-1)])+1):sum(lsels[1:ff]) +(ff-1)*0.75+0.25)
          xlab=c(xlab,ilongnames[sel[[format]]])

        }
        axis(side=2,at=xat,labels=xlab,las=1)
        cx=1-(round(length(years)/5)*0.2)
        legend("topright",legend=years,col=yearcols,pch=yearpchs,pt.cex=1.5*cx,ncol=length(years),cex=cx)
        box()
        if(plot_title){

            if(ut) mtext(side=3,line=3.5+ut, title, adj=NA, cex=1)
            mtext(side=3,line=2.5, paste0("Overview ",ifelse(is.null(pnames),paste0("station",pp),pnames[pp]),"(lon:",round(dat_p$lon[pp],digits=1),"/ lat:",round(dat_p$lat[pp],digits=1),") ", aggnames[aa]), adj=0, cex=ifelse(ut,0.8,1))
            mtext(side=3,line=1.5, paste0("Period: ",paste0(dyears,collapse="-")), adj=0, cex=0.8)
            mtext(side=3,line=0.5,paste0("Data: ",dat_p$data_info$data_name), adj=0, cex=0.8)
        }

   if (!is.null(output)){
     if (output!="dev.new") dev.off()
   }

      } #aa
    }#pp

}
