
# Function to get default colors for plotting climatology
# # muessen noch zugeordnet werden
# "th_range","pheno_tmin","pheno_tmax","pheno_topt"
# "qtot"
# "sdi"
get_indexvar<-function(index,index_args){

  preci<-c("dd" ,"cdd","prcptot","rx","cwd","sdii","rXptot","spi","spi_forecast","rainy_season_start","rainy_season_end","rainy_season_dur")
  tmini<-c("fd","tn10p","tnn","tn90p","tnx","th_tmin","csdi")
  tmaxi<-c("tx10p","txn","txx","tx90p","th_tmax","wsdi")
  tavgi<-c("th_topt")

 tryCatch({
  if (is.element(index,preci)){
    var="prec"
  } else if (is.element(index,tmini)){
    var="tmin"
  } else if (is.element(index,tmaxi)){
    var="tmax"
  } else if (is.element(index,tavgi)){
    var="tavg"
  } else {
    if (!any(grepl("var",names(index_args)))) stop(paste0("for index ",index, " var/thvar needs to be defined in index_args"))
    var=unlist(index_args[grepl("var",names(index_args))])
  }
   return(var=var)

 }, error=function(cond){
   message("error in <<ind_args>>:")
   message(cond)
   stop_quietly()
 })


}

get_default_color<-function(index,iname,fc=FALSE){

  if(!fc){
    cold<-c("#0E6134","#2BAC66","#ABCF63","#E8F69E","#D7E3EE","#B5CAFF","#8FB3FF","#7F97FF","#0570B0","#023858")
    hot<-c("#FFFF8F","#FED679","#FBBA5B","#F99C4F","#E76636","#D62F27","#B50626","#6B0026","#2B0016")
    sun=c("#FFF5CC","#FFE670","#FFCC33","#FFAF33","#FF9933","#FF6F00","#FF5500","#E6281E","#C81E14")
    prec<-c("#EDFAC2","#CDFFCD","#99F0B2","#53BD9F","#32A696","#3296B4","#0570B0","#05508C","#0A1F96","#2C0246","#6A2C5A")
    dry= c("#9ACD32","#C9E959","#EBF3B1","#E1D075","#B48222","#6F5741")
    vegetation<-c("#6F5741","#B48222","#E1D075","#EBF3B1","#CCFF9C","#C9E959","#9ACD32","#4FBD33","#619E40","#326329")
    pheno<-c("#8B3A3A","#F4BA00","#009999")
    #--- 0 zentriert----------------------
    prec_anom<-c("#B66A28","#CD853F","#E1A564","#F5E09E","#FFF5BA","#FFFADC","#E6FFE6","#99F4B2","#53BD9F","#6EAAC8","#0570B0","#023858")
    temp_abs<-c("#071E46","#08529C","#4292C7","#78BFD6","#DBDBDB","#FC9272","#F03C2B","#A60F14","#5F0000")


  } else {
    cold<-c("#0E6134","#ABCF63","#023858")
    hot<-c("#E76636","#6B0026","#2B0016")
    sun=c("#FFF5CC","#FFE670","#FFCC33","#FFAF33","#FF9933","#FF6F00","#FF5500","#E6281E","#C81E14")
    prec<-c("#53BD9F","#0A1F96","#2C0246")
    dry= c("#9ACD32","#EBF3B1","#B48222")
    vegetation<-c("#6F5741","#B48222","#4FBD33")
    pheno<-c("#8B3A3A","#F4BA00","#009999")
  }


  center=FALSE # color scale centered around 0?
  if (is.element(index,c("dd" ,"cdd")) | (index=="cxd" & grepl("prec<",iname))){
    col=dry
  } else if (is.element(index,c("fd","tn10p","th_tmin","csdi")) | (index=="cxd" & grepl("tmin<",iname))) {
    col = cold
  } else if ((is.element(index,c("tnn"))& fc)  |(index=="cxd" & grepl("tmin>",iname))) {
    col = rev(cold)
    center=TRUE
  } else if (is.element(index, c("prcptot","rx","cwd","sdii","rXptot")) | (index=="cxd" & grepl("prec>",iname))){
    col=prec
  } else if (is.element(index, c("spi","spi_forecast"))){
    col=prec_anom
    center=TRUE
  } else if (is.element(index,c("tx90p","th_tmax","wsdi")) ){
    col=hot
  } else if (is.element(index,c("txx"))& fc){
    col=hot
    center=TRUE
  } else if (is.element(index,c("tn90p","tx10p"))){
    col=sun[1:5]
  } else if (is.element(index,c("txn","tnx","txx","tnn")) & !fc){
    col=temp_abs
    center=TRUE
  }  else if (is.element(index,c("qval","qrange")) & grepl("t",iname) & !fc){
      col=temp_abs
      center=TRUE
  }else if (is.element(index,c("qval","qrange")) & grepl("prec",iname) & !fc){
    col=prec
  }else if (is.element(index,c("txn","tnx")) & fc){
    col=sun[1:5]
    center=TRUE
  } else if (index=="cxd" & grepl("tmax",iname)){
    col=switch(grepl(">",iname)+1,cold,sun)
  } else if (index == "rainy_season_start"){
    col=rev(vegetation)
  } else if (index == "rainy_season_end") {
    col=vegetation
  } else if (index == "rainy_season_dur") {
    col=vegetation
  }else if (is.element(index,c("minmax_xdays","varmin","varmax"))){
    if (grepl("min",iname)){
      col(switch(grepl("prec",iname)+1),cold,prec)
    } else if (grepl("max",iname)){
      col=switch(grepl("prec",iname)+1,sun,prec)
    }
  } else if (is.element(index,c("th","qth"))){
    if (grepl("prec>",iname) ){
      col=prec
    } else if (grepl("prec<",iname)){
      col=dry
    } else if (grepl(paste(c("tmin<","tavg<","tmax<"),collapse="|"),iname) ){
      col=cold
    } else if (grepl(paste(c("tmin>","tavg>","tmax>"),collapse="|"),iname) ){
      col=sun[1:5]
    } else grDevices::colorRampPalette(c("lightgrey","darkgrey"))(10)
  } else if (is.element(index,c("mean","sum"))){
      if (grepl("prec",iname)){
        col=prec
      } else if  (length(grep(paste(c("tmin","tavg","tmax"),collapse="|"),iname))>0 ){
        col=switch(fc+1,temp_abs,sun)
        center=TRUE
      } else grDevices::colorRampPalette(c("lightgrey","darkgrey"))(10)
  } else if (grepl(paste(c("th_topt","th_range"),collapse="|"),index)){
    col=pheno
  } else if (index=="cxd"){
      if(grepl("tmin",iname) & grepl("<", iname)) {
        col=cold
      } else col=grDevices::colorRampPalette(c("lightgrey","darkgrey"))(10)
  } else {
    message(paste0("no default color defined for index",index))
    col=grDevices::colorRampPalette(c("lightgrey","darkgrey"))(10)
  }
  return(list(col=col,center=center))
}

get_default_color_anom<-function(index,iname){

    prec_anom<-c("#B66A28","#CD853F","#E1A564","#F5E09E","#FFF5BA","#FFFADC","#E6FFE6","#99F4B2","#53BD9F","#6EAAC8","#0570B0","#023858")
    temp_abs<-c("#071E46","#08529C","#4292C7","#78BFD6","#DBDBDB","#FC9272","#F03C2B","#A60F14","#5F0000")

  center=TRUE # color scale centered around 0?
  if (is.element(index,c("dd" ,"cdd")) | (index=="cxd" & grepl("prec<",iname))){
    col=rev(prec_anom)
  } else if (is.element(index,c("fd","tn10p","th_tmin","csdi")) | (index=="cxd" & grepl("tmin<",iname))) {
    col = rev(temp_abs)
  } else if (is.element(index,c("tnn"))  |(index=="cxd" & grepl("tmin>",iname))) {
    col = temp_abs

  } else if (is.element(index, c("prcptot","rx","cwd","sdii","rXptot")) | (index=="cxd" & grepl("prec>",iname))){
    col=prec_anom
  } else if (is.element(index, c("spi","spi_forecast"))){
    col=prec_anom

  } else if (is.element(index,c("tx90p","th_tmax","wsdi")) ){
    col=temp_abs
  } else if (is.element(index,c("txx"))){
    col=temp_abs

  } else if (is.element(index,c("tn90p","tx10p"))){
    col=temp_abs
  } else if (is.element(index,c("txn","tnx"))){
    col=temp_abs

  } else if (index=="cxd" & grepl("tmax",iname)){
    col=switch(grepl(">",iname)+1,rev(temp_abs),temp_abs)
  } else if (index == "rainy_season_start"){
    col=rev(prec_anom)
  } else if (index == "rainy_season_end") {
    col=prec_anom
  } else if (is.element(index,c("minmax_xdays","varmin","varmax"))){
    if (grepl("min",iname)){
      col(switch(grepl("prec",iname)+1),temp_abs,prec_anom)
    } else if (grepl("max",iname)){
      col=switch(grepl("prec",iname)+1,temp_abs,prec_anom)
    }
  } else if (is.element(index,c("th","qth"))){
    if (grepl("prec>",iname) ){
      col=prec_anom
    } else if (grepl("prec<",iname)){
      col=rev(prec_anom)
    } else if (grepl(paste(c("tmin<","tavg<","tmax<"),collapse="|"),iname) ){
      col=rev(temp_abs)
    } else if (grepl(paste(c("tmin>","tavg>","tmax>"),collapse="|"),iname) ){
      col=temp_abs
    } else grDevices::colorRampPalette(c("lightgrey","darkgrey"))(10)
  } else if (is.element(index,c("mean","sum"))){
    if (grepl("prec",iname)){
      col=prec_anom
    } else if  (length(grep(paste(c("tmin","tavg","tmax"),collapse="|"),iname))>0 ){
      col=temp_abs
    } else col=gplots::bluered(10)
  } else if (grepl(paste(c("th_topt","th_range"),collapse="|"),index)){
    col=temp_abs
  } else if (index=="cxd"){
    if(grepl("tmin",iname) & grepl("<", iname)) {
      col=rev(temp_abs)
    } else col=gplots::bluered(10)
  } else {
    message(paste0("no default color defined for index",index))
    col=gplots::bluered(10)
  }
  return(list(col=col,center=center))
}



# get name of aggregation type
get_aggtname<-function(index_args,aggnames) {
   if(index_args$aggt=="monthly" & !is.na(aggnames[1])) {aggtname="mon"
   } else if (index_args$aggt=="seasonal"& !is.na(aggnames[1])) { aggtname="seas"
  } else aggtname=""
   return(aggtname)
}
#get aggregation names
get_aggnames<-function(climindvis_index) {
  if(is.element("agg",climindvis_index$index_info$idims)){
    aggnames=dimnames(climindvis_index$index)[[which(climindvis_index$index_info$idims=="agg")]]
  } else aggnames=NA
  return(aggnames)
}

check_topo <- function(topo){
  if(dim(topo$alt)[1]!=length(topo$lon)){warning("Dimesions of Topography do not fit provided longitudes")}
  if(dim(topo$alt)[2]!=length(topo$lat)){warning("Dimesions of Topography do not fit provided latitudes")}
# add comparison with g_lon,g_lat

}

get_trinfo <- function(climindvis_index, aggreg, ip){

  if(is.element(climindvis_index$index_info$aggt,"annual")){
    return(climindvis_index$trend_info[ip, ])}
  else{
      return(climindvis_index$trend_info[ip,which(dimnames(climindvis_index$trend_info)[[2]]==aggreg),] )
    }
}

image_scale <- function(breaks,col, axis.pos=1, add.axis=TRUE, xlab,ylab,scale_lab, key.extend=c(FALSE,FALSE),fc_axis=NULL,useraxis=NULL, equidist=FALSE,mar=c(5,1,5,3),las=1,...){

   # if(!missing(breaks)){
  #   if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  # }

  poly <- vector(mode="list", length(col))
  for(i in seq(poly)[1:length(poly)]){
    if(equidist==TRUE){
      poly[[i]] <- c(i+key.extend[1],i+key.extend[1]+1, i+key.extend[1]+1, i+key.extend[1])
    } else  poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }

  ifelse(equidist,height<-c((key.extend[1]+1):(length(breaks)-key.extend[2])),height<-breaks)
  #ifelse(equidist,height<-c((key.extend[1]+1):(length(breaks)-key.extend[2])),height<-breaks[(key.extend[1]+1):(length(breaks)-key.extend[2])])
  if(axis.pos %in% c(1,3)){ylim<-c(0,1);xlim=range(height) }
  if(axis.pos %in% c(2,4)){ylim<-range(height); xlim<-c(0,1)}
  if(missing(ylab)){ylab=""}
  if(missing(xlab)){xlab=""}
  par(mar=mar)
  if (!is.null(fc_axis)){
    if(axis.pos %in% c(1,3)) par(mar=c(2,1,1,1))
    if(axis.pos %in% c(2,4)) par(mar=c(1,1,1,2))
  }
  plot(1,1,t="n",ylim=ylim, xlim=xlim, axes=FALSE, xlab=xlab, ylab=ylab, xaxs="i", yaxs="i") #, ...)
  for(i in seq(poly)){
    if(axis.pos %in% c(1,3)){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(axis.pos %in% c(2,4)){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }

  if(axis.pos %in% c(2,4) & is.null(fc_axis)){
    mtext(side=4,at=breaks[round(length(breaks)/2)],scale_lab,line=1.5,cex=1,las=las, padj = 1)
  }
  if (any(key.extend)) {
    dl <- diff(breaks[2:3])
    last <- length(breaks)
    xi <- 0
    xa <- 1
    apex <- 1
    clipmax <- apex + (0.05*apex)
    if(axis.pos %in% c(1,3)){
      # rect(breaks[(1+key.extend[1]):(last-1-key.extend[2])],xi,
      #   breaks[(2+key.extend[1]):(last-key.extend[2])],xa,
      #   col = col[(1+key.extend[1]):(length(col)-key.extend[2])])
      clip(breaks[1]-(dl*clipmax), breaks[last]+(dl*clipmax),xi,xa )
    } else {
      # rect(xi, breaks[(1+key.extend[1]):(last-1-key.extend[2])],
      # xa, breaks[(2+key.extend[1]):(last-key.extend[2])],
      # col = col[(1+key.extend[1]):(length(col)-key.extend[2])])
      clip(xi,xa, breaks[1]-(dl*clipmax), breaks[last]+(dl*clipmax))
      }

    if (key.extend[1]) {
      lower_break <- breaks[1]-dl
      if(axis.pos %in% c(1,3)){
        polygon( c(breaks[1], breaks[1],
          lower_break-(dl*apex)),c(xi,xa,xa/2),
          col = col[1], border="black")
      } else {polygon(c(xi,xa,xa/2),
        c(breaks[1], breaks[1], lower_break),
        col = col[1], border=NA)}
    }
    if (key.extend[2]) {
      upper_break <- breaks[last]+dl
      if(axis.pos %in% c(1,3)){
        polygon( c(breaks[last]+(dl), breaks[last], breaks[last],
          breaks[last]+(dl), upper_break+(dl*apex)),
          c(xi,xi,xa,xa,xa/2),
          col = col[length(col)], border=NA)

      } else {polygon(c(xi,xa/2,xa),
                      c(breaks[last], upper_break, breaks[last]),
                      col = col[last-1], border=NA)}
    }
    new_low<- ifelse(key.extend[1], lower_break, breaks[1])
    new_up<- ifelse(key.extend[2], upper_break, breaks[last])

    if(axis.pos %in% c(1,3)){
     polygon(c(breaks[last],new_up, breaks[last],breaks[1], new_low, breaks[1]),c(xi,xa/2,xa,xa,xa/2,xi),
            col = NA, border="black" )
    } else{
      polygon(c(xi,xa/2,xa,xa,xa/2,xi),c(breaks[last],new_up, breaks[last],breaks[1], new_low, breaks[1]),
              col = NA, border="black" )


    }

  } else
    if(!is.null(fc_axis)){
    wc=c(0,which(col[2:(length(col)-1)]=="white")+1,max(height))
    pbox <- vector(mode="list", length(wc)+1)
    for(i in 1:(length(wc)-1)){
      if(axis.pos %in% c(1,3)){
        polygon(height[c(wc[i]+1,wc[i+1], wc[i+1], wc[i]+1)],c(0,0,1,1))
        abline(v=height[-c(wc,wc+1)],col="black",lty=2,lwd=0.7)
      } else {
        polygon(c(0,0,1,1),height[c(wc[i]+1,wc[i+1], wc[i+1], wc[i]+1)])
        abline(h=height[-c(wc,wc+1)],col="black",lty=2,lwd=0.7)
      }
    }

    ats=c(height[-wc][!is.even(height[-wc])])+0.5
    mtext(side=4,at=ats,xlab,line=0.1,cex=0.7,las=1)
    mtext(side=2,at=seq(6,length(fc_axis)*8-2,8),fc_axis,line=0.2,cex=1)

  } else {box()}
  if(add.axis) axis(axis.pos)
  if(!is.null(useraxis)){
    
    axis(side=4,at=useraxis$ats, labels=useraxis$labs, line=0, cex.axis=useraxis$cex,las=1)
    
  }

}

get_lims <- function(dat){
  if(is.null(dat)){
    message("Limits are set to 0, as input data is NULL")
    lim=c(0,0)
    diffs=NULL
  }
if(!all(unlist(lapply(dat,is.na)))) {
    lim <- c()
    lims=range(dat,na.rm=TRUE)
    if(length(unique(lims))==1) {
      lim=lims
      diffs=NULL
    } else {
      if (!is.list(dat)) dat<-list(dat)
      q <- quantile(unlist(dat),probs=seq(0,1,0.2), na.rm=TRUE)
      diffq <- diff(q[1:length(q)])
      set <- diffq[c(1,length(diffq))]/median(diffq)>4
      if (median(diffq)==0) set <- diffq[c(1,length(diffq))]/median(diffq[abs(diffq)>0])>4
      ifelse(set[1]==TRUE, lim[1] <- q[2],lim[1] <- lims[1])
      ifelse(set[2]==TRUE, lim[2] <- q[length(diffq)], lim[2] <- lims[2])
      if(any(set)) {
        diffs <-ifelse(set, lims[set],NA)
      } else {diffs <- NULL}
    }
} else {
  message("Limits are set to 0, as input data is NA")
  lim=c(0,0)
  diffs=NULL

  }
  return(list(lim=lim, diffs=diffs))
}

set_upper_limit<-function(data,upper_lim=NULL,probs=0.98){
  if (is.null(upper_lim)) upper_lim <- ceiling(quantile(abs(data),probs=probs, na.rm=TRUE))
  ifelse(upper_lim>150, upper_lim<- 150, upper_lim <- upper_lim)
  data[which(data >= upper_lim)] <- upper_lim
  data[which(data < -upper_lim)] <- -upper_lim
  return(list(data=data,upper_lim=upper_lim))
}

get_breaks <- function(dat=NULL,zlims=NULL,nlev=8,center=FALSE){
  if(center & nlev %% 2 != 0) { nlev <- nlev+1}
  if(is.null(zlims)) {
    zlims <- get_lims(dat)
    }
  else {
    rdat <- range(dat, na.rm=TRUE)
    if (!is.list(zlims)){
      f <- ifelse(rdat[2]>zlims[2],rdat[2],NA)
      d <- ifelse(rdat[1]<zlims[1],rdat[1],NA)
      zlims <- list(lim= zlims, diffs =c(d,f))
    } else {
      f <- ifelse(rdat[2]>zlims$lim[2],rdat[2],NA)
      d <- ifelse(rdat[1]<zlims$lim[1],rdat[1],NA)
      zlims$diffs=c(d,f)
    }}
  if(any(is.null(zlims$lim))){
    zlims$lim <- c(0,0)}

  if(center){
      mlim <- max(abs(zlims$lim),na.rm=TRUE)
      ifelse(mlim<5,rval <-0.5,ifelse(mlim<20,rval<-2,ifelse(mlim<50, rval <-5, ifelse(mlim< 100,rval <-10,rval <- 20))))
      mlim2 <- round(mlim/rval)*rval # rounds to 0.5,1,2, 5
      if(mlim2<mlim) { mlim2 <- ceiling(mlim)    }

      breaks <- pretty(c(-mlim2,mlim2), nlev)
      #breaks <- get_perfect_breaks(mlim2,rval,nlev)
  } else  {
    breaks <-pretty(zlims$lim,nlev)
  }
  if(!is.null(zlims$diffs)){
      if (center){
        zdiff <- ifelse(abs(zlims$diffs)< mlim, NA, zlims$diffs)
      } else {
        zlim <- range(dat, na.rm=TRUE)
        zdiff <- ifelse(abs(zlims$diffs)< zlim, NA, zlims$diffs)
      }
  #   breaks_n <- c(zdiff[1],breaks,zdiff[2])
  #   breaks <- breaks_n[!is.na(breaks_n)]
    outliers <- ifelse(is.na(zdiff), FALSE, TRUE)
   } else outliers <- c(FALSE,FALSE)

  return(list(breaks=breaks, outliers=outliers))
}

get_perfect_breaks <- function(mlim,rval,nlev){
  if(rval==0.5) {values <- c(0.1,0.2,0.25,0.4,0.5)}
  if(rval==2) {values <- c(1,1.5,2,2.5,3,3.5)}
  if(rval==5) {values <- c(2,2.5,5,3,3.5,4,4.5,5,6,7.5,8,10,12.5,15)}
  if(rval==10) {values <- c(10,12.5,15,20,25)}
  if(rval==20) {values <- c(20,25,seq(30,1000,by=5))}

  breaks <- NULL

  for(step in values){
    nnlev <- (2*mlim)/step
    if(nnlev%%1 !=0)  {next} else {
      if(mlim%%step != 0){
        if((2*mlim)%%step ==0){
          if(!is.even(nnlev)&& nnlev >2){
            breaks <-seq(-mlim,mlim,by=step)
            break
          } else {next}
        } else {
          next
        }
      } else {
        next
      }
    }
  }
  if(is.null(breaks)) {
    for(v in 1:3){
    mlim2 <- mlim
    mlim2 <- mlim2 + rval
    for(step in values){
      nnlev <- (2*mlim2)/step
      if(nnlev%%1 !=0)  {next} else {
        if(mlim2%%step != 0){
          if((2*mlim2)%%step ==0){
            if(!is.even(nnlev)&nnlev >2 ){
              breaks <-seq(-mlim2,mlim2,by=step)
              break
            } else {next}
          } else {
            next
          }
        } else {
          next
        }
      }
    }
    }

  }
  if(is.null(breaks)){breaks <- pretty(c(-mlim,mlim),nlev)}
  return(breaks)
}



get_fdims_index <- function(climindvis_index_list,...){
  fdims = list()
  years <- sapply(climindvis_index_list, function(x){
    dl <- length(dim(x$index_fcst))
    years <- dimnames(x$index_fcst)[dl]
    return(years)},simplify=TRUE )
  ipoint <- lapply(climindvis_index_list,function(x){1:dim(x$index)[1]})
  time <- sapply(climindvis_index_list, function(x) {x$index_info$fcst_time})
  return(list(ipoint=ipoint,years=years, time=time))
}


get_plot_data_spi<-function(climindvis_index_list,years,ip,pdiminfo){
  pdata<-mapply(function(x,y) {
    data<-index_array(x$index, c(1,which(x$index_info$idims=="agg"),which(x$index_info$idims=="year")), list(y$ipoint[ip],seq(1,12,1),years), drop=TRUE)}
    ,climindvis_index_list,pdiminfo,SIMPLIFY = FALSE)
  return(pdata)
}


get_plot_data_fc<-function(climindvis_index_list,years,ip,fdiminfo){
  fdata<-mapply(function(x,y) {
    dims <- dim(x$index_fcst)
    data<-index_array(x$index_fc, c(1:length(dims)),
      list(y[ip],1:dims[2],1:dims[3],years[[1]]))
    mean_spi<-apply(data,c(length(dims)-1,length(dims)),mean,na.rm=TRUE)
    sd_spi <- apply(data,c(length(dims)-1,length(dims)),sd,na.rm=TRUE)
    return(list(data=data,mean_spi=mean_spi, sd_spi=sd_spi))},
    climindvis_index_list,fdiminfo,SIMPLIFY = FALSE)
  return(fdata)
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


get_p_cex <- function(pdata){
  pdat_help <- abs(pdata)
  if(length(pdat_help[!is.na(pdat_help)])<=1){
    p_dat_size <- pdat_help
    pdat_help[!is.na(pdat_help)] <- 0

  } else {
    p_dat_size <- (pdat_help-min(pdat_help, na.rm=TRUE))/(max(pdat_help, na.rm=TRUE)-min(pdat_help, na.rm=TRUE))
  }
  p_cex <- p_dat_size+1
  p_cex[is.na(p_cex)] <- 1
  return(p_cex)
}