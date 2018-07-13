get_fdims_index <- function(ind_dat,...){
  fdims = list()
  years <- sapply(ind_dat, function(x){
    dl <- length(dim(x$index_fcst))
    years <- dimnames(x$index_fcst)[dl]
    return(years)},simplify=TRUE )
  ipoint <- lapply(ind_dat,function(x){1:dim(x$index)[1]})
  time <- sapply(ind_dat, function(x) {x$index_info$fcst_time})
  return(list(ipoint=ipoint,years=years, time=time))
}


get_plot_data_spi<-function(ind_dat,years,ip,pdiminfo){
  pdata<-mapply(function(x,y) {
   data<-index_array(x$index, c(1,which(x$index_info$idims=="agg"),which(x$index_info$idims=="year")), list(y$ipoint[ip],seq(1,12,1),years), drop=TRUE)}
   ,ind_dat,pdiminfo,SIMPLIFY = FALSE)
  return(pdata)
}


get_plot_data_fc<-function(ind_dat,years,ip,fdiminfo){
  fdata<-mapply(function(x,y) {
    dims <- dim(x$index_fcst)
    data<-index_array(x$index_fc, c(1:length(dims)),
                      list(y[ip],1:dims[2],1:dims[3],years[[1]]))
    mean_spi<-apply(data,c(length(dims)-1,length(dims)),mean,na.rm=TRUE)
    sd_spi <- apply(data,c(length(dims)-1,length(dims)),sd,na.rm=TRUE)
    return(list(data=data,mean_spi=mean_spi, sd_spi=sd_spi))},
    ind_dat,fdiminfo,SIMPLIFY = FALSE)
  return(fdata)
}



#' function for plotting spi for station data
#'@param ind_dat 1 List of climindvis_index objects (as returned by \code{\link{calc_index}}).
#' You can either plot
#'  \itemize{
#'   \item different indices calculated from the same dataset (...$data_info$data_name has to be the same for all element of ind_dat) or
#'   \item one indice for different datasets (class of list elements and ...$index_info$iname have to be the same for all element of ind_dat)
#'   }
#'@param trendplots logical. Array of same length as ind_dat indicating for each index whether or not the trend should be plotted. If not specified all values are set to false.
#'@param output: type of output xxx auch write moeglich
#'@param plotdir: directory where files/plots are saved to.
#'@param plotname: string for outputnames, the final name will be outfilestring_month/season/etc..
#'@param selpoints Array to select subset of points to plot,there are two options:
#'    \itemize{
#'   \item character array of station names (works only if names are defined in climindvis_index objects ...$data_info$pnames and if they are the same for all objects in ind_dat)
#'   \item integer array of station indices (works only if  coordinates of all objects in ind_dat are the same)
#'   }
#'@param plotstart Start year of plot, e.g plotstart = 1990 ends at end of forecast time.
#'@param ylims Limits of xaxis for plot of format ylims=c(ymin,ymax). If not provided limits are taken from the data
#'@param pwidth,pheight,pres  Width,Height and Resolution of plot (see width in  \code{\link[grDevices]{png}}).If output=="pdf" the heights and widths are adjusted to pwidth/res and pheight/res. Default values are pwidth=2000,pheight=1300,pres=250.
#'@param cex cex of plots (see par()). Default=1
#'
plot_spi_forecast <- function(ind_dat,full_dat_NA, trendplots,selpoints,pdims, plot_title=TRUE,title="",output=NULL,plotdir=NULL,plotname=NULL,ylims=NULL,plotstart =NULL,pwidth=2000,pheight=1300,pres=250,cex=1,text_cex=1,show.grid.h=TRUE, ...){
  opargs<-list(...)

  if(any(is.element(class(ind_dat), "climindvis_index"))) ind_dat<-list(ind_dat)
   if (missing(trendplots)) trendplots=rep(FALSE,length(ind_dat))
  if(length(trendplots) != length(ind_dat)) {
    trendplots = c(trendplots,rep(FALSE,length(ind_dat)-length(trendplots)))
    message("Length of <<trendplots>> differs from length of <<ind_dat>> , array filled up with <<FALSE>>")
  }
  if(all(trendplots)==TRUE){
    if(sum(sapply(ind_dat,function(x) is.na(x["index_trend"]))==TRUE)>1)
      message(" Trend calculation in climindvis Index List is missing ")}

  lty <- rep(1, length(ind_dat))
  datname<-unique(sapply(ind_dat,function(x) x$data_info$data_name))

  tdims<-get_matching_tdims_index(ind_dat,...)
  tdims$hist_time <- sapply(ind_dat, function(x) {
    time <- x$index_info$hist_time
    return(time)
  })
  pdims <-match_pdims_index(ind_dat,selpoints=selpoints,...)
  fdims <- get_fdims_index(ind_dat,...)

  pinfo<-list()
  pinfo$ylab=paste0("SPI ", ind_dat[[1]]$index_info$timescale)
  if(!is.null(ylims)) {pinfo$ylims <- ylims} else { pinfo$ylims=c(-3,3)}
  pinfo$yaxis=FALSE

  pinfo$titlestring<-ind_dat[[1]]$index_info$iname
  # pinfo$legend<-sapply(ind_dat,function(x) x$data_info$data_name)

  breaks <- c(0,0.675,0.92, 1.2,1.6,2)
  colors.spi <- get_default_color("spi", ind_dat$index_info$iname)$col
  colors.spi <- colors.spi[2:(length(colors.spi)-1)]
  colors.pos <- colors.spi[((length(colors.spi)/2)+1):length(colors.spi)]
  colors.neg <- rev(colors.spi[1:(length(colors.spi)/2)])

  if (trendplots==TRUE){
    pinfo$legend[[length(pinfo$legend)+1]] <- "Fit"
    pinfo$legend[[length(pinfo$legend)+1]] <- "Lw"

  }

  for ( pp in 1:pdims$pnumber){

    pdata<-get_plot_data_spi(ind_dat,years=tdims$year,ip=pp,pdiminfo=pdims$dims)
    fdata<-get_plot_data_fc(ind_dat,years=fdims$years,ip=pp,fdiminfo=fdims$ipoint)
    pname<-ifelse(is.null(pdims$pnames[pp]),as.character(pp),pdims$pnames[pp])
    agg <- NA

    if(!is.null(output)) {
      plotfile<-paste0(plotdir,plotname,ifelse(plotname == "", "", "_"),pinfo$titlestring,"fcst","_",tdims$year[1],tdims$year[length(tdims$year)],"_",pname,".",output, collapse = "")
      if(is.element(output, c("png","jpeg","tiff","bmp"))){
        do.call(output,c(list(filename=plotfile,height=pheight,width=pwidth,res=pres)))
      } else if (output == "pdf"){
        do.call(output,c(list(file=plotfile,height=pheight/pres,width=pwidth/pres)))
      } else if (output=="dev.new") dev.new()
    }

    tottime <- c(tdims$hist_time[!is.element(tdims$hist_time, fdims$time)], fdims$time)
    if (is.null(plotstart)) {prange <- c(1,length(tottime))}
    else {
      start <- as.numeric(gsub("-.*","",as.character(tottime)))
      prange <- c(which(start==plotstart,arr.ind = TRUE)[1] , length(tottime))
    }

    # get pos and neg colors
    pdata.pos = unlist(pdata)
    pdata.neg = unlist(pdata)

    pdata.pos[!pdata.pos>=0] = NA
    pdata.neg[pdata.neg>=0] = NA

    pdata.pos = pdata.pos[prange[1]:prange[2]]
    pdata.neg = pdata.neg[prange[1]:prange[2]]

    offset_fcst <- abs(prange[1]-length(unlist(pdata)))-1
    # barplot
    t.m.neg = -expand_bar(-pdata.neg, breaks)
    t.m.pos = expand_bar(pdata.pos, breaks)
    t.n = length(prange[1]:prange[2])

    #
    ut=ifelse(title=="",FALSE,TRUE)
    ifelse(plot_title,par(mar=c(2,4*text_cex,(5+ut)*text_cex,2)+0.1),par(oma=c(2,4*text_cex,1,2)+0.1))

    barplot(rep(NA, t.n),xaxs = "i", ylim = pinfo$ylims, border = NA,
            space = 0, yaxt = "n")
    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col = "white", border = "black")

    abline(h = 0, col = "black", lwd = 0.5)
    barplot(t.m.pos, col=colors.pos, offset = 0, border = NA,
            add = TRUE, space = 0, yaxt = "n", xpd = FALSE)
    barplot(t.m.neg, col=colors.neg,  offset = 0, border = NA,
            add = TRUE, space = 0, yaxt = "n", xpd = FALSE)

    axis(side=1,labels=tottime[prange[1]:prange[2]],at=1:length(tottime[prange[1]:prange[2]]),cex.axis=text_cex)

     if(pinfo$yaxis==TRUE){
      axis(side=2,labels=pinfo$ylabels,at=c(pinfo$yat),cex.axis=text_cex)
    } else axis(side=2,cex.axis=text_cex)

    abline(h = 0, col = "gray56")
    if (show.grid.h) {
      abline(h = breaks, lwd = 0.5, col = "gray56")
      abline(h = -breaks, lwd = 0.5, col = "gray56")
    }


    for (i in 1:length(ind_dat)){
        # plot forecast
        fdat.mem <- fdata[[i]]$data
        fdat.ts.mat <- rearrange_fcst(fdat.mem)
        fdl <- length(fdat.ts.mat[1,])
        breaks.all <- c(rev(-breaks),breaks[2:6])
        boxcol <- colors.spi[cut(apply(fdat.ts.mat,2,median, na.rm=TRUE), breaks=breaks.all)]
        boxplot(fdat.ts.mat,at=seq(offset_fcst+1.5,(offset_fcst+fdl+0.5),by=1), add=TRUE, names= rep("",fdl), col=boxcol,outline = FALSE, pars=list(xaxt="n"), border = "gray40", yaxt="n")
      }

      if (trendplots==TRUE){
        tdata<-get_plot_data_points_trend(ind_dat,iagg=agg,years=tdims$year,ip=pp,pdiminfo=pdims$dims)

        if (TRUE %in% is.na(tdata[[i]]$data[length(tdata[[i]]$data[,1]),])){
          tdata[[i]]$data<- tdata[[i]]$data[-length(tdata[[i]]$data[,1]),]
        }

        #if (!is.na(nagg)){
        if (!is.na(agg)){
          if (TRUE %in% is.na(tdata[[i]]$data[length(tdata[[i]]$data[,1]),])){
            tdata[[i]]$data<- tdata[[i]]$data[-length(tdata[[i]]$data[,1]),]
          }

          # if (!is.na(nagg)){
          if (!is.na(agg)){
            polygon(c(1:length(tdata[[i]]$data[,1]),length(tdata[[i]]$data[,1]):1),c(tdata[[i]]$data[,2],rev(tdata[[i]]$data[,3])),border=NA,col=rgb(colv[1],colv[2],colv[3], alpha=70, maxColorValue = 255),cex=cex)
            lines(tdata[[i]]$data[,1],type="l",col=rgb(colv[1],colv[2],colv[3],maxColorValue = 255),cex=cex)
            lines(tdata[[i]]$data[,4],type="l",col=rgb(colv[1],colv[2],colv[3],maxColorValue = 255),lty=2,cex=cex)

          } else {
            polygon(c(1:length(tdata[[i]]$data[,1]),length(tdata[[i]]$data[,1]):1),c(tdata[[i]]$data[,2],rev(tdata[[i]]$data[,3])),border=NA,col=rgb(colv[1],colv[2],colv[3], alpha=70, maxColorValue = 255),cex=cex)
            lines(tdata[[i]]$data[,1],type="l",col=rgb(colv[1],colv[2],colv[3],maxColorValue = 255),cex=cex)
            lines(tdata[[i]]$data[,4],type="l",col=rgb(colv[1],colv[2],colv[3],maxColorValue = 255),lty=2,cex=cex)

          }
          pcols <- c("gray33", rep("black",2))
        }
      }
    if (plot_title){
      if(title!="") mtext(title,side=3,line=5*text_cex,adj=0.5,cex=1.5*text_cex)
    mtext(paste0(pinfo$titlestring," ",pname," (lon: ", pdims$lon[pp],"/lat: ",pdims$lat[pp],")"),side=3,line=4*text_cex,cex=1*text_cex, adj=0)
    years <- range(tdims$year)
    mtext(paste0("Reference Period: ", years[1],"-",years[2]), side=3, line = 3*text_cex, cex=0.8*text_cex, adj=0)
    mtext(paste0("Missing Values: ",round(full_dat_NA[pp]) ," [%]"), side=3, line = 2 *text_cex, cex=0.8*text_cex, adj=0)
    mtext (paste0("Data: ",paste0(unique(sapply(ind_dat, function(ii) ii$data_info$data_name)),collapse=",")), side=3, line = text_cex, cex=0.8*text_cex, adj=0)

     if (trendplots==TRUE){
      lty <- c(lty, rep(c(2,3), length(ind_dat)))

      if(!is.na(agg) & length(nagg)> 1 ){
        trinfo <- lapply(ind_dat, function(ll) index_array(ll$trend_info,c(1),pdims$dims[[1]]$ipoint[pp],drop=TRUE))
        mtext(paste0("Period: ",paste0(trinfo[[1]][which(nagg==agg),1],"-",trinfo[[1]][which(nagg==agg),2]),"  p-value: ",paste0(1:length(ind_dat),":",round( as.numeric(sapply(trinfo,function(x) x[which(nagg==agg),3])),3)," ",collapse="") ,"  relative trend: ",paste0(1:length(ind_dat),":",round(as.numeric(sapply(trinfo,function(x) x[which(nagg==agg),4])),3)," ",collapse="")),side=3, line = 0.1,cex=0.7)
      } else{
        trinfo <- sapply(ind_dat, function(ll) index_array(ll$trend_info,c(1),pdims$dims[[1]]$ipoint[pp]))
        mtext(paste0("Period: ",trinfo[1,1],"-",trinfo[2,1],"  p-value: ",paste0(1:length(ind_dat),":",round( as.numeric(trinfo[3,]),3)," ",collapse="") ,"  relative trend: ",paste0(1:length(ind_dat),":",round(as.numeric(trinfo[4,]),3)," ",collapse="")),side=3, line = 0.1,cex=0.7)

      }}
    }

    if(!is.null(output)) dev.off()
    }
}


expand_bar <- function (values, breaks) {
  l.breaks = length(breaks)
  s = sapply(values, function(value) {
    r = rep(NA, l.breaks)
    r = value - breaks
    r[r < 0] = NA
    t.i = sum(!is.na(r))
    if (t.i > 1)
      r[1:(t.i - 1)] = diff(breaks)[1:(t.i - 1)]
    r
  })
  s
}

rearrange_fcst <- function(fdat.mem){

  dims <- dim(fdat.mem)
  fdat.ts <- data.frame(matrix(NA, ncol=dims[3]*dims[4],nrow=dims[2]))
  fdat.ts[,1:dims[3]] <- fdat.mem[1,,,1]
  fdat.ts[,(dims[3]+1):length(fdat.ts[1,])] <- fdat.mem[1,,,2]
  start <- sum(is.na(fdat.ts[1,1:dims[3]]))+1
  fdat.ts <- fdat.ts[,start:length(fdat.ts[1,])]

  return(fdat.ts)
}
