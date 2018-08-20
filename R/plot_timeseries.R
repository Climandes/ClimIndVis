
single_ts <- function(ind_dat, day_dat_na,tdims, pdims, pinfo, trendplots, output,plotdir,plotname,pwidth,pheight,pres,selpoints,cex,text_cex,plot_title,title,opargs){

  if(is.null(opargs$shading) & is.null(opargs$yval_shading)){
    if (length(ind_dat)> 1) message(" Shading will be calculated from first data set in <<ind_dat>>")
  }

  if (is.null(opargs$pcols)) {
    pcols=RColorBrewer::brewer.pal(3, "Dark2")
  } else pcols=opargs$pcols

  aggn <- lapply(ind_dat, function(x){ x$index_info$aggnames})[[1]]
  # start plot loop
  for (pp in 1:pdims$pnumber){
    if (!is.null(tdims$agg)){nagg=tdims$agg
    } else {nagg=NA}

    for (agg in nagg){
      pdata<-get_plot_data_points(ind_dat,iagg=agg,years=tdims$year,ip=pp,pdiminfo=pdims$dims)
      pname<-ifelse(is.null(pdims$pnames[pp]),as.character(pp),pdims$pnames[pp])

      if(!is.null(output)) {
        plotfile<-paste0(plotdir,ifelse(is.null(plotname),"",paste0(plotname,"_")),"single_ts","_",pinfo$index_name,"_",
                         ifelse(is.na(agg),paste(aggn[1],"_"),paste0(aggn[which(nagg==agg)],"_")),
                         tdims$year[1],"-",tdims$year[length(tdims$year)],"_",pname,".",output)
        if(is.element(output, c("png","jpeg","tiff","bmp"))){
          do.call(output,c(list(filename=plotfile,height=pheight,width=pwidth,res=pres)))
        } else if (output == "pdf"){
          do.call(output,c(list(file=plotfile,height=pheight/pres,width=pwidth/pres)))
        } else if (output=="dev.new") dev.new()
      }

      for (i in 1:length(ind_dat)){
        if(pinfo$ylims[1]=="ind"){
          ylims <- round(range(pdata[[i]]$data, na.rm=TRUE), digits=2)
        } else {ylims <- pinfo$ylims}

        ut<-ifelse(title=="",FALSE,TRUE)
        ifelse(plot_title,par(mar=c(2,4*text_cex,(5+ut)*text_cex,1)+0.1,tcl=ifelse(text_cex<1,-0.5+(0.5*text_cex),-0.5)),par(mar=c(2,4*text_cex,1,1)+0.1,tcl=ifelse(text_cex<1,-0.5+(0.5*text_cex),-0.5)))
        plot(0,type="n",xlab="",ylab="",axes=FALSE,xlim=c(1,length(tdims$year)+1),ylim=ylims)

        if(!is.null(opargs$shading)){
          if(is.null(opargs$nshading)) {
            stop("number of polygons for shading <<nshadin>> needs to be provided")
          } else nshd=opargs$nshading
          if (is.null(opargs$yval_shading)) {
            datsh=quantile(pdata[[1]]$data,1:(nshd-1)/nshd,na.rm = TRUE , type=8)
          } else {
            datsh=opargs$yval_shading
            if (length(datsh)> opargs$nshading+1) stop("<<yval_shading>> should have length <<nshading>>")
          }
          if (length(opargs$col_shading) != opargs$nshading) stop("<<opargs$col_shading>> should have length <<nshading>>")
          datsh=c(pinfo$ylims[1]-10,datsh,pinfo$ylims[2]+10)
          for (nsh in 1:opargs$nshading){
            polygon(c(-10,length(tdims$year)+10,length(tdims$year)+10,0),c(rep(datsh[nsh],2),rep(datsh[nsh+1],2)),col=opargs$col_shading[nsh],border=NA)
          }
        }
        na_vals <- ifelse(is.na(agg),day_dat_na[[i]][selpoints[pp]],day_dat_na[[i]][selpoints[pp],agg==nagg])
        axis(side=1,labels=tdims$year,at=c(1:length(tdims$year)),cex.axis=text_cex)
        year_seq <- c(1:length(tdims$year))
        if(is.null(opargs$shading)){
          abline(v=year_seq[which((year_seq%%5)==0)],col="gray66" ,lty=2)
        }

        if(!is.null(pinfo$yaxis)){
          axis(side=2,labels=pinfo$ylabels,at=c(pinfo$yat),line=0,cex.axis=text_cex)
        } else axis(side=2,cex.axis=text_cex)
        mtext(side=2,pinfo$ylab,line=3*text_cex,cex=text_cex)

        colv <- col2rgb(pcols[i])
        if (length(pdata[[i]]$sd)>1 ){
          polygon(c(1:length(pdata[[i]]$data),length(pdata[[i]]$data):1),c(pdata[[i]]$data-pdata[[i]]$sd,rev(pdata[[i]]$data+pdata[[i]]$sd)),
                  border=NA,col=rgb(colv[1],colv[2],colv[3],alpha=70,maxColorValue = 255),cex=cex)
        }
        lines(as.vector(pdata[[i]]$data),col=pcols[i],cex=1)
        points(as.vector(pdata[[i]]$data),pch=16,col=pcols[i],cex=1)

        # plot subtitles
        if(plot_title){
          if(ut)           mtext(title, side=3,line=4*text_cex,cex=1.5*text_cex,adj=0.5)
          mtext(paste0(ifelse(!is.null(pinfo$titlestring),paste0(pinfo$titlestring," "),""),pinfo$index_name," ",
                       ifelse(is.na(agg),paste(aggn[1]," "),paste0(aggn[which(nagg==agg)]," ")),"",pname," (", pdims$lon[pp],"/ ",pdims$lat[pp],")"),
                side=3,line=3*text_cex,cex=text_cex,adj=0)
          years <- ind_dat[[1]]$index_info$years
          mtext(paste0("Period: ", years[1],"-",years[length(years)]), side=3, line = 2*text_cex, cex=0.8*text_cex, adj=0)
          mtext(paste0("Missing Values: ",round(na_vals) ," [%]"), side=3, line = text_cex, cex=0.8*text_cex, adj=0)
        }

        if (trendplots){
          tdata<-get_plot_data_points_trend(ind_dat,iagg=agg,years=tdims$year,ip=pp,pdiminfo=pdims$dims)
          plot_polyt(tdata=tdata,colv=colv,cex=cex,i=i)

          if(plot_title){
            trinfo <- t(round(sapply(ind_dat, function(ll)
              index_array(ll$trend_info, switch(is.na(agg)+1,c(1,2),1),
                          list(pdims$dims[[1]]$ipoint[pp],switch(is.na(agg)+1,which(agg==nagg),NULL)))),digits=2))
            write_trend(trinfo,text_cex, dif=FALSE)

          }
          #legend("topright", legend=c("index","trend","loess"))

          }
      }
      box()
      if(!is.null(output)){
        if (output!="dev.new") dev.off()
      }
    }
  }
}


multi_ind <- function(ind_dat, day_dat_na,tdims, pdims, pinfo, trendplots, output,plotdir,plotname,pwidth,pheight,pres,selpoints,cex,text_cex,plot_title,title,opargs){

  if(length(ind_dat)==1) message("Only one index provided. Please provide several climindvis objects for a multi-index plot.")

  if (is.null(opargs$pcols)) {
    pcols=RColorBrewer::brewer.pal(ifelse(length(ind_dat)<3,3,length(ind_dat)), "Dark2")
  } else pcols=opargs$pcols

  aggn <- lapply(ind_dat, function(x){ x$index_info$aggnames})
  aggn <- unlist(aggn)

  pinfo$legend <- sapply(ind_dat, function(x) x$index_info$iname)
  paxis <- sapply(ind_dat, function(x) {x$index_info$iformat})
  ifelse(length(unique(paxis))>1,axt <- TRUE, axt <- FALSE)
  leftaxis <- sapply(ind_dat, function(x) {x$index_info$iformat == paxis[1]})

  for ( pp in 1:pdims$pnumber){
    if (!is.null(tdims$agg)){
      nagg=tdims$agg
    } else {
      nagg=NA
    }

    for (agg in nagg){
      pdata<-get_plot_data_points(ind_dat,iagg=agg,years=tdims$year,ip=pp,pdiminfo=pdims$dims)
      pname<-ifelse(is.null(pdims$pnames[pp]),as.character(pp),pdims$pnames[pp])

      if(!is.null(output)) {
          plotfile<-paste0(plotdir,ifelse(is.null(plotname),"",paste0(plotname,"_")),"multind_ts","_",paste(pinfo$index_name,collapse = ""),
                           "_",ifelse(is.na(agg),paste0(aggn,collapse = "","_"),paste0(aggn[which(nagg==agg)],"_")),tdims$year[1],"-",tdims$year[length(tdims$year)],"_",pname,".",output)
          if(is.element(output, c("png","jpeg","tiff","bmp"))){
            do.call(output,c(list(filename=plotfile,height=pheight,width=pwidth,res=pres)))
          } else if (output == "pdf"){
            do.call(output,c(list(file=plotfile,height=pheight/pres,width=pwidth/pres)))
          } else if (output=="dev.new") dev.new()
      }

      if(pinfo$ylims[1]=="ind"){
        dat4lim<-mapply(get_plot_data_points,lapply(ind_dat[leftaxis],list),rep(agg,length(ind_dat[leftaxis])),MoreArgs = list(years=unlist(tdims$year),ip=pp,pdiminfo=pdims$dims[1]))
        ylims <- round(range(dat4lim, na.rm=TRUE), digits=2)
      } else {ylims <- pinfo$ylims}

      #par(mar=c(3,4,4,4))
      ut<-ifelse(title=="",FALSE,TRUE)
      ifelse(plot_title,par(mar=c(2,4*text_cex,(5+ut)*text_cex,ifelse(axt,4*text_cex,2))+0.1,tcl=ifelse(text_cex<1,-0.5+(0.5*text_cex),-0.5)),
             par(mar=c(2,4*text_cex,1,ifelse(axt,4*text_cex,2))+0.1,tcl=ifelse(text_cex<1,-0.5+(0.5*text_cex),-0.5)))
      plot(0,type="n",xlab="",ylab="",axes=FALSE,xlim=c(1,length(tdims$year)+1),ylim=ylims)

      axis(side=1,labels=tdims$year,at=c(1:length(tdims$year)),cex.axis=text_cex,tick=TRUE)
      year_seq <- c(1:length(tdims$year))
      abline(v=year_seq[which((year_seq%%5)==0)],col="gray66" ,lty=2)

      if(pinfo$yaxis==TRUE){
        axis(side=2,labels=pinfo$ylabels,at=c(pinfo$yat),cex.axis=text_cex,tick=TRUE)
      } else axis(side=2,cex.axis=text_cex)
      mtext(side=2,pinfo$ylab,line=3*text_cex,cex=text_cex)
      na_vals <- list()

      for (i in which(leftaxis==TRUE)){

        na_vals[[i]] <- ifelse(is.na(agg),day_dat_na[[i]][pp],day_dat_na[[i]][pp,agg])
        colv <- col2rgb(pcols[i])
        lines(as.vector(pdata[[i]]$data),col=pcols[i],cex=cex)
        points(as.vector(pdata[[i]]$data),pch=16,col=pcols[i],cex=cex)

        if (trendplots==TRUE){
          tdata<-get_plot_data_points_trend(ind_dat[i],iagg=agg,years=tdims$year,ip=pp,pdiminfo=pdims$dims)
          plot_polyt(tdata=tdata,colv=colv,cex=cex,i=i)}
      }

      if(axt){
        par(new=TRUE)
        dat4lim<-mapply(get_plot_data_points,lapply(ind_dat[!leftaxis],list),rep(agg,length(ind_dat[!leftaxis])),MoreArgs = list(years=unlist(tdims$year),ip=pp,pdiminfo=pdims$dims[1]))
        ylims2 <- round(range(dat4lim, na.rm=TRUE), digits=2)
        plot(0,type="n",xlab="",ylab="",axes=FALSE,xlim=c(1,length(tdims$year)+1),ylim=ylims2)
        axis(side=4,cex.axis=text_cex)
        mtext(side = 4, line = 3, paxis[!leftaxis],cex=text_cex)

        for(j in which(!leftaxis==TRUE)){

          na_vals[[j]] <- ifelse(is.na(agg),day_dat_na[[j]][pp],day_dat_na[[j]][pp,agg])

          colv <- col2rgb(pcols[j])
          lines(as.vector(pdata[[j]]$data),col=pcols[j],cex=cex)
          points(as.vector(pdata[[j]]$data),pch=16,col=pcols[j],cex=cex)

          if (trendplots==TRUE){
            tdata<-get_plot_data_points_trend(ind_dat[j],iagg=agg,years=tdims$year,ip=pp,pdiminfo=pdims$dims)
            plot_polyt(tdata=tdata,colv=colv,cex=cex,i=j)
          }
        }
      }
      # plot subtitles
      if(plot_title){
        mtext(paste0(ifelse(!is.null(pinfo$titlestring),paste0(pinfo$titlestring," "),""),
                     ifelse(is.na(agg),paste0(unique(aggn),collapse = ""," "),paste0(aggn[which(nagg==agg)]," ")),"",pname," (", pdims$lon[pp],"/ ",pdims$lat[pp],")"),side=3,line=3*text_cex,cex=text_cex,adj=0)
        years <- ind_dat[[1]]$index_info$years
        mtext(paste0("Period: ", years[1],"-",years[length(years)]), side=3, line = 2*text_cex, cex=0.8*text_cex, adj=0)
        miss_val <- paste0(round(unlist(na_vals)),collapse = "/")
        mtext(paste0("Missing Values: ", miss_val ," [%]"), side=3, line = text_cex, cex=0.8*text_cex, adj=0)

        if (trendplots==TRUE){
          trinfo <- t(round(sapply(ind_dat, function(ll)
            index_array(ll$trend_info, switch(is.na(agg)+1,c(1,2),1),
                        list(pdims$dims[[1]]$ipoint[pp],switch(is.na(agg)+1,which(agg%in%nagg),NULL)))),digits=2))
          write_trend(trinfo,text_cex,dif=TRUE)
        }
      }

      legend("topright",legend=pinfo$legend,col=pcols,lwd=2,bty="n",cex=0.8*text_cex)
      box()

      if(!is.null(output)){
        if (output!="dev.new") dev.off()
        }
    }
  }
}

multi_agg <- function(ind_dat, day_dat_na,tdims, pdims, pinfo,trendplots, output,plotdir,plotname,pwidth,pheight,pres,selpoints,cex,text_cex,plot_title,title,opargs){
  #if(is.null(tdims$agg)) stop("no valid aggregation for this index, change type argument")

  aggType <- Reduce(union,lapply(ind_dat, function(x) x$index_info$aggt))
  nagg <-  lapply(ind_dat, function(x) x$index_info$aggnames)
  if(any(is.element(aggType,c("annual","other","dates")))) {
    # necessary for extracting data
    nagg[[which(is.element(aggType,c("annual","other","dates")))]] <- NA
  }

  if(any(is.element(aggType,c("monthly")))){
    nagg[[which(is.element(aggType,c("monthly")))]] <- return_m(nagg[[which(is.element(aggType,c("monthly")))]])
  }
  aggn <- lapply(ind_dat, function(x) x$index_info$aggnames)
  years <- lapply(ind_dat, function(x) x$index_info$years)[[1]]
  pinfo$legend <- unlist(aggn)

  if (is.null(opargs$pcols)) {
    # this is to avoid error message in brewer.pal
   pcols= suppressWarnings(RColorBrewer::brewer.pal(ifelse(length(unlist(aggn))<3,3,length(unlist(aggn))), "Set3"))
    if(length(unlist(aggn)) > 12) message("More than 12 aggregations selected. Only 12 colors available at the moment.")
  } else pcols=opargs$pcols

  lty <- rep(1, length(nagg))
  for (pp in 1:pdims$pnumber){
    pname<-ifelse(is.null(pdims$pnames[pp]),as.character(pp),pdims$pnames[pp])

    if(!is.null(output)){
      plotfile<-paste0(plotdir,ifelse(is.null(plotname),"",paste0(plotname,"_")),"multiagg_ts","_",unique(pinfo$index_name),"_",
                       paste0(pinfo$legend,collapse = ""),"_",
                       years[1],"-",years[length(years)],"_",pname,".",output)
      if(is.element(output, c("png","jpeg","tiff","bmp"))){
        do.call(output,c(list(filename=plotfile,height=pheight,width=pwidth,res=pres)))
      } else if (output == "pdf"){
        do.call(output,c(list(file=plotfile,height=pheight/pres,width=pwidth/pres)))
      } else if (output=="dev.new") dev.new()
    }


    if(pinfo$ylims[1]=="ind"){
      pdata<-get_plot_data_points(ind_dat,iagg=nagg,years=years,ip=pp,pdiminfo=pdims$dims[1])
      ylims <- round(range(pdata, na.rm=TRUE), digits=2)
    } else {ylims <- pinfo$ylims}

    ut<-ifelse(title=="",FALSE,TRUE)
    ifelse(plot_title,par(mar=c(2,4*text_cex,(5+1)*text_cex,1)+0.1,tcl=ifelse(text_cex<1,-0.5+(0.5*text_cex),-0.5)),
           par(mar=c(2,4*text_cex,1,1)+0.1,tcl=ifelse(text_cex<1,-0.5+(0.5*text_cex),-0.5)))
    plot(0,type="n",xlab="",ylab="",axes=FALSE,xlim=c(1,length(years)+1),ylim=ylims)
    axis(side=1,labels=years,at=c(1:length(years)),cex.axis=text_cex)
    year_seq <- c(1:length(years))
    abline(v=year_seq[which((year_seq%%5)==0)],col="gray66" ,lty=2)

    if(pinfo$yaxis==TRUE){
      axis(side=2,labels=pinfo$ylabels,at=c(pinfo$yat),cex.axis=text_cex)
    } else axis(side=2,cex.axis=text_cex)
    mtext(side=2,pinfo$ylab,line=3*text_cex,cex=text_cex)

    trinfo <- matrix(NA,ncol=6, nrow=length(unlist(aggn)), dimnames = list(unlist(aggn), NULL))
    na_vals <- list()

    for (al in 1:length(aggType)){
      na_vals[[al]] <- list()
      for (i in 1:length(nagg[[al]])){
        pdata<-get_plot_data_points(ind_dat[al],iagg=nagg[[al]][i],years=years,ip=pp,pdiminfo=pdims$dims[1]) #ifelse(length(ind_dat)==1,tdims$year,unlist(tdims$year[1]))
        na_vals[[al]][[i]]  <- round(day_dat_na[[al]][i],digits=2)
        chelp <- which(unlist(aggn)==aggn[[al]][i])
        lines(pdata[[1]]$data[1,],col=pcols[chelp],cex=cex)
        points(pdata[[1]]$data[1,],pch=16,col=pcols[chelp],cex=cex)

        if (trendplots){
          tdata<-get_plot_data_points_trend(ind_dat[al],iagg=nagg[[al]][i],years=years,ip=pp,pdiminfo=pdims$dims[al])
          plot_polyt(tdata=tdata,colv=col2rgb(pcols[chelp]),cex=cex,i=1)
          trinfo[aggn[[al]][i],] <- round(index_array(ind_dat[[al]]$trend_info,
                                               dim=switch(is.na(nagg[[al]][i])+1,c(which(is.element(ind_dat[[al]]$index_info$idims,c("p"))),which(ind_dat[[al]]$index_info$idims=="agg")),
                                                          which(is.element(ind_dat[[al]]$index_info$idims,c("p")))),
                                               value=switch(is.na(nagg[[al]][i])+1, list(pdims$dims[[al]]$ipoint[pp],nagg[[al]][i]),list(pdims$dims[[al]]$ipoint[pp])), drop=TRUE),digits=2)

     }
    }
    }

    if(plot_title){
      mtext(paste0(pinfo$titlestring,"",unique(pinfo$index_name)," ", pname," (", pdims$lon[pp],"/",pdims$lat[pp],")"),side=3,line=3*text_cex,cex=text_cex, adj=0)
      mtext(paste0("Period: ", years[1],"-",years[length(years)]), side=3, line = 2*text_cex, cex=0.8*text_cex, adj=0)
      miss_val <- paste0(round(unlist(na_vals)),collapse = "/")
      mtext(paste0("Missing Values: ", miss_val ," [%]"), side=3, line = text_cex, cex=0.8*text_cex, adj=0)
      if(trendplots){write_trend(trinfo,text_cex, dif=FALSE)}
    }

    legend("topright",legend=pinfo$legend,col=pcols,lty=lty,lwd=2,bty="n",cex=0.8*text_cex)
    box()

    if(!is.null(output)){
      if (output!="dev.new") dev.off()
      }

}
}


multi_dat <- function(ind_dat, day_dat_na,tdims, pdims, pinfo,trendplots, output,plotdir,plotname,pwidth,pheight,pres,selpoints,cex,text_cex,plot_title,title,opargs){

  if (is.null(opargs$pcols)) {
    pcols=RColorBrewer::brewer.pal(ifelse(length(ind_dat)<3,3,length(ind_dat)), "Dark2")
  } else pcols=opargs$pcols

  if (!is.null(tdims$agg)){
    nagg=tdims$agg
  } else {
    nagg=NA
  }
  #pinfo$legend <- lapply(ind_dat, function(x) x$data_info$long_data_name)
  if(all(is.null(pinfo$legend))) pinfo$legend <- lapply(ind_dat, function(x) x$data_info$data_name)
  aggn <- unique(lapply(ind_dat, function(x) x$index_info$aggnames))
  years <- lapply(ind_dat, function(x) x$index_info$aggnames)


  for (pp in 1:pdims$pnumber){
    for (agg in nagg){
      pdata<-get_plot_data_points(ind_dat,iagg=agg,years=tdims$year,ip=pp,pdiminfo=pdims$dims)
      na_vals <- round(switch(is.na(agg)+1,
                              mapply(function(x,y){x[y$ipoint[[pp]],agg==nagg]}, x=day_dat_na, y=pdims$dims),
                              mapply(function(x,y){x[y$ipoint[[pp]]]}, x=day_dat_na, y=pdims$dims)),digits = 0)
      if(trendplots) {tdata<-get_plot_data_points_trend(ind_dat,iagg=agg,years=tdims$year,ip=pp,pdiminfo=pdims$dims)}
      ifelse(is.null(pdims$pnames[pp]),pname <- as.character(pp),pname <- pdims$pnames[pp])

      if(pinfo$ylims[1]=="ind"){
           ylims <- round(range(lapply(pdata, function(x) x$data), na.rm=TRUE), digits=2)
        } else {ylims <- pinfo$ylims}

      if(!is.null(output)){
        plotfile<-paste0(plotdir,ifelse(is.null(plotname),"",paste0(plotname,"_")),"multidat_ts","_",unique(pinfo$index_name),"_",
                         ifelse(is.na(agg),paste0(aggn,collapse = "","_"),paste0(nagg[which(nagg==agg)],"_")),
                         tdims$year[1],"-",tdims$year[length(tdims$year)],"_",pname,".",output)
        if(is.element(output, c("png","jpeg","tiff","bmp"))){
          do.call(output,c(list(filename=plotfile,height=pheight,width=pwidth,res=pres)))
        } else if (output == "pdf"){
          do.call(output,c(list(file=plotfile,height=pheight/pres,width=pwidth/pres)))
        } else if (output=="dev.new") dev.new()
      }
      ut<-ifelse(title=="",FALSE,TRUE)
    ifelse(plot_title,par(mar=c(2,4*text_cex,(5+ut)*text_cex,1)+0.1,tcl=ifelse(text_cex<1,-0.5+(0.5*text_cex),-0.5)),par(mar=c(2,4*text_cex,1,1)+0.1,tcl=ifelse(text_cex<1,-0.5+(0.5*text_cex),-0.5)))
    plot(0,type="n",xlab="",ylab="",axes=FALSE,xlim=c(1,length(tdims$year)+1),ylim=ylims)

    axis(side=1,labels=tdims$year,at=c(1:length(tdims$year)),cex.axis=text_cex)
    year_seq <- c(1:length(tdims$year))
    abline(v=year_seq[which((year_seq%%5)==0)],col="gray66" ,lty=2)

    if(pinfo$yaxis==TRUE){
      axis(side=2,labels=pinfo$ylabels,at=c(pinfo$yat),cex.axis=text_cex)
    } else axis(side=2,cex.axis=text_cex)
    mtext(side=2,pinfo$ylab,line=3*text_cex,cex=text_cex)

    for (i in 1:length(ind_dat)){
      colv <- col2rgb(pcols[i])

      if(ind_dat[[i]]$data_info$type=="p"){
        dat <- pdata[[i]]$data[1,]
        lines(dat,col=pcols[i],cex=cex)
        points(dat,pch=16,col=pcols[i],cex=cex*0.5)
      } else {
        dat <- pdata[[i]]$data[1,]
        sd_hc <- pdata[[i]]$sd
        lyear<- length(tdims$year)
        if(is.na(sd_hc[length(sd_hc)])){
          lyear <- lyear-1
          dat <- dat[!is.na(sd_hc)]
          sd_hc <- sd_hc[!is.na(sd_hc)]
          }
        polygon(c(1:lyear,lyear:1),c(dat-sd_hc,rev(dat+sd_hc)),border=NA, col=rgb(colv[1],colv[2],colv[3],alpha=70,maxColorValue = 255))
        lines(dat,col=pcols[i],cex=cex)
        points(dat,pch=16,col=pcols[i],cex=cex*0.5)
      }

      if (trendplots){
        polygon(c(1:length(tdata[[i]]$data[,1]),length(tdata[[i]]$data[,1]):1),c(tdata[[i]]$data[,2],rev(tdata[[i]]$data[,3])),border=NA,col=rgb(colv[1],colv[2],colv[3], alpha=70, maxColorValue = 255),cex=cex)
        lines(tdata[[i]]$data[,1],type="l",col=rgb(colv[1],colv[2],colv[3],maxColorValue = 255),cex=cex)
        lines(tdata[[i]]$data[,4],type="l",col=rgb(colv[1],colv[2],colv[3],maxColorValue = 255),lty=2,cex=cex)
      }
    }

    if(plot_title){
      mtext(paste0(pinfo$titlestring,"",pinfo$index_name," ", ifelse(is.na(agg),paste0(aggn,collapse = ""," "),paste0(nagg[which(nagg==agg)]," ")),pname," (", pdims$lon[pp],"/",pdims$lat[pp],")"),side=3,line=3*text_cex,cex=text_cex, adj=0)
      mtext(paste0("Period: ", tdims$year[1],"-",tdims$year[length(tdims$year)]), side=3, line = 2*text_cex, cex=0.8*text_cex, adj=0)
      miss_val <- paste0(round(unlist(na_vals)),collapse = "/")
      mtext(paste0("Missing Values: ", miss_val ," [%]"), side=3, line = text_cex, cex=0.8*text_cex, adj=0)
      if (trendplots){
        trinfo <- get_trend_info(ind_dat,agg,pdims$dims,nagg,pp)
        write_trend(trinfo,text_cex, dif=FALSE)
       }
    }

    box()
    legend("topright",legend=pinfo$legend,col=pcols,lty=1,lwd=2,bty="n",cex=0.8*text_cex)

    if(!is.null(output)){
      if (output!="dev.new") dev.off()
      }
  }}
}


multi_point <- function(ind_dat,day_dat_na, tdims, pdims, pinfo,trendplots, output,plotdir,plotname,pwidth,pheight,
                        pres,selpoints,cex,text_cex,plot_title,title,opargs){

  if (is.null(opargs$pcols)) {
    pcols=RColorBrewer::brewer.pal(max(length(selpoints),3), "Dark2")[1:length(selpoints)]
  } else pcols=opargs$pcols

  if (!is.null(tdims$agg)){
    nagg=tdims$agg
  } else {
    nagg=NA
  }
  aggn <- unique(unlist(lapply(ind_dat, function(x){x$index_info$aggnames})))
  pp <- 1:length(selpoints)
  ifelse(class(pdims$pnames[pp])=="character",pinfo$legend<-pdims$pnames[pp],pinfo$legend<-paste0(pdims$pnames[pp]," (", pdims$lon[pp],"/",pdims$lat[pp],")"))

  for (agg in nagg){
    pdata<-get_plot_data_points(ind_dat,iagg=agg,years=tdims$year,ip=pp,pdiminfo=pdims$dims)
    na_vals <- switch(is.na(agg)+1,round(day_dat_na[[1]][pdims$dims$dat_p$ipoint,agg],digits=1),round(day_dat_na[[1]][pdims$dims[[1]]$ipoint],digits=1))
    if(trendplots) {tdata<-get_plot_data_points_trend(ind_dat,iagg=agg,years=tdims$year,ip=pp,pdiminfo=pdims$dims)}

    ifelse(is.null(pdims$pnames[pp]),pname <- as.character(pp),pname <- pdims$pnames[pp])

    if(pinfo$ylims[1]=="ind"){
      ylims <- round(range(pdata, na.rm=TRUE), digits=2)
    } else {ylims <- pinfo$ylims}



    if(!is.null(output)) {
      plotfile<-    paste0(plotdir,ifelse(is.null(plotname),"",paste0(plotname,"_")),"multipoint_ts","_",pinfo$index_name,"_",
                           ifelse(is.na(agg),paste0(aggn,collapse = "","_"),paste0(aggn[which(nagg==agg)],"_")),
                           tdims$year[1],"-",tdims$year[length(tdims$year)],".",output)
      if(is.element(output, c("png","jpeg","tiff","bmp"))){
        do.call(output,c(list(filename=plotfile,height=pheight,width=pwidth,res=pres)))
      } else if (output == "pdf"){
        do.call(output,c(list(file=plotfile,height=pheight/pres,width=pwidth/pres)))
      } else if (output=="dev.new") dev.new()
      }
    ut<-ifelse(title=="",FALSE,TRUE)
    ifelse(plot_title,par(mar=c(2,4*text_cex,(5+ut)*text_cex,1)+0.1,tcl=ifelse(text_cex<1,-0.5+(0.5*text_cex),-0.5)),
           par(mar=c(2,4*text_cex,1,1)+0.1,tcl=ifelse(text_cex<1,-0.5+(0.5*text_cex),-0.5)))
    plot(0,type="n",xlab="",ylab="",axes=FALSE,xlim=c(1,length(tdims$year)+2),ylim=ylims,yaxs="i")

    axis(side=1,labels=tdims$year,at=c(1:length(tdims$year)),cex.axis=text_cex)
    year_seq <- c(1:length(tdims$year))
    abline(v=year_seq[which((year_seq%%5)==0)],col="gray66" ,lty=2)

    if(pinfo$yaxis==TRUE){
      axis(side=2,labels=pinfo$ylabels,at=c(pinfo$yat),cex.axis=text_cex)
    } else axis(side=2,cex.axis=text_cex)
    mtext(side=2,pinfo$ylab,line=3*text_cex,cex=text_cex)

    for (i in 1:length(pp)){
      colv <- col2rgb(pcols[i])

      #pdata hat nur ein Datensatz..
      ifelse(is.na(nagg)[1],dat <- pdata[[1]]$data[i,],dat <- pdata[[1]]$data[i,,])
      lines(dat,col=pcols[i],cex=cex)
      points(dat,pch=16,col=pcols[i],cex=cex*0.5)

      if (trendplots){
        polygon(c(1:length(tdata[[1]]$data[i,,1]),length(tdata[[1]]$data[i,,1]):1),c(tdata[[1]]$data[i,,2],rev(tdata[[1]]$data[i,,3])),border=NA,col=rgb(colv[1],colv[2],colv[3], alpha=70, maxColorValue = 255),cex=cex)
        lines(tdata[[1]]$data[i,,1],type="l",col=rgb(colv[1],colv[2],colv[3],maxColorValue = 255),cex=cex)
        lines(tdata[[1]]$data[i,,4],type="l",col=rgb(colv[1],colv[2],colv[3],maxColorValue = 255),lty=2,cex=cex)
      }
    }

    if(plot_title){
      mtext(paste0(pinfo$titlestring,"",pinfo$index_name," ",ifelse(is.na(agg),paste0(aggn,collapse = ""," "),paste0(aggn[which(nagg==agg)]," "))),side=3,line=3*text_cex,cex=text_cex, adj=0)
      years <- ind_dat[[1]]$index_info$years
      mtext(paste0("Period: ", years[1],"-",years[length(years)]), side=3, line = 2*text_cex, cex=0.8*text_cex, adj=0)
      miss_val <- paste0(round(unlist(na_vals)),collapse = "/")
      mtext(paste0("Missing Values: ", miss_val ," [%]"), side=3, line = 1*text_cex, cex=0.8*text_cex, adj=0)

      if(trendplots==TRUE){
        trinfo <- round(lapply(ind_dat, function(ll)
          index_array(ll$trend_info, switch(is.na(agg)+1,c(1,2),1),
                      list(pdims$dims[[1]]$ipoint[pp],switch(is.na(agg)+1,which(agg%in%nagg),NULL)),drop=TRUE))[[1]],digits=2)
        write_trend(trinfo,text_cex, dif=FALSE)
      }
    }


    box()
    legend("topright",legend=pinfo$legend,col=pcols,lty=1,lwd=2,bty="n",cex=0.8*text_cex)
    if(!is.null(output)){
      if (output!="dev.new") dev.off()
      }
  }
}

spi_barplot <- function(ind_dat,day_dat_na,tdims, pdims, pinfo, trendplots, output,plotdir,plotname,pwidth,pheight,pres,selpoints,cex,text_cex,plot_title,title,opargs,show.grid.h=TRUE){

  breaks <- c(0,0.675,0.92, 1.2,1.6,2)

  colors.spi <- get_default_color("spi", ind_dat[[1]]$index_info$iname)$col
  colors.pos <- colors.spi[((length(colors.spi)/2)+1):length(colors.spi)]
  colors.neg <- rev(colors.spi[1:(length(colors.spi)/2)])

  years <- sapply(ind_dat, function(x) x$index_info$years)
  aggs <- sapply(ind_dat, function(x) x$index_info$aggnames)
  tottime =  paste0(rep(years, each=length(aggs)), "-",rep(aggs, length(years)))
  spiscale <- Reduce(intersect,lapply(ind_dat, function(x){x$index_info$timescale}))

  for (pp in 1:pdims$pnumber){

    pdata<-get_plot_data_spi(ind_dat,years=tdims$year,ip=pp,pdiminfo=pdims$dims)
    pname<-ifelse(is.null(pdims$pnames[pp]),as.character(pp),pdims$pnames[pp])
    na_vals <- round(mean(day_dat_na[[1]][pdims$dims$dat_p$ipoint[pp],]),digits=1)

    pdata.pos = unlist(pdata)
    pdata.neg = unlist(pdata)
    pdata.pos[!pdata.pos>=0] = NA
    pdata.neg[pdata.neg>0] = NA
    # barplot
    t.m.neg = -expand_bar(-pdata.neg, breaks)
    t.m.pos = expand_bar(pdata.pos, breaks)
    t.n = length(tottime)


    if(!is.null(output)) {
      plotfile<-paste0(plotdir,ifelse(is.null(plotname),"",paste0(plotname,"_")),"spi_",spiscale,"_",tdims$year[1],"-",tdims$year[length(tdims$year)],"_",pname,".",output)
      if(is.element(output, c("png","jpeg","tiff","bmp"))){
        do.call(output,c(list(filename=plotfile,height=pheight,width=pwidth,res=pres)))
      } else if (output == "pdf"){
        do.call(output,c(list(file=plotfile,height=pheight/pres,width=pwidth/pres)))
      } else if (output=="dev.new") dev.new()
    }
    ut<-ifelse(title=="",FALSE,TRUE)
    ifelse(plot_title,par(mar=c(2,4*text_cex,(5+ut)*text_cex,1)+0.1,tcl=ifelse(text_cex<1,-0.5+(0.5*text_cex),-0.5)),
           par(mar=c(2,4*text_cex,1,1)+0.1,tcl=ifelse(text_cex<1,-0.5+(0.5*text_cex),-0.5)))
    barplot(rep(NA, t.n),xaxs = "i", ylim = pinfo$ylims, border = NA,
            space = 0, yaxt = "n")
    abline(h = 0, col = "gray", lwd = 0.5)
    barplot(t.m.pos, col=colors.pos, offset = 0, border = NA,
            add = TRUE, space = 0, yaxt = "n", xaxt="n", xpd = FALSE)
    barplot(t.m.neg, col=colors.neg,  offset = 0, border = NA,
            add = TRUE, space = 0, yaxt = "n", xaxt="n", xpd = FALSE)
    axis(side=1,labels=tottime[seq(1,length(tottime),by=12*5)],at=seq(1,length(tottime),by=12*5),cex.axis=text_cex)
    abline(v=seq(1,length(tottime),by=12*5),col="gray66" ,lty=2)

    if(pinfo$yaxis==TRUE){
      axis(side=2,labels=pinfo$ylabels,at=c(pinfo$yat),cex.axis=text_cex)
    } else axis(side=2,cex.axis=text_cex)
    mtext(side=2,pinfo$ylab,line=3*text_cex,cex=text_cex)

    if (show.grid.h) {
      abline(h = breaks, lwd = 0.5, col = "gray66")
      abline(h = -breaks, lwd = 0.5, col = "gray66")
    }

    trendplots <- FALSE
    if(plot_title){
      mtext(paste0(pinfo$titlestring,"",pname,"( ", pdims$lon[pp],"/ ",pdims$lat[pp],")"),side=3,line=3*text_cex,cex=text_cex, adj=0)
      mtext(paste0("Period: ", years[1],"-",years[length(years)]), side=3, line = 2*text_cex, cex=0.8*text_cex, adj=0)
      miss_val <- paste0(round(unlist(na_vals)),collapse = "/")
      mtext(paste0("Missing Values: ", miss_val ," [%]"), side=3, line = text_cex, cex=0.8*text_cex, adj=0)

    if (trendplots){
      tdata<-get_plot_data_points_trend_spi(ind_dat,iagg=NA,years=tdims$year,ip=pp,pdiminfo=pdims$dims)
      lines(as.vector(tdata[[1]]$data[,,1]),type="l",col="black",cex=cex, lwd=2)*text_cex
      trinfo <- round(lapply(ind_dat, function(ll) index_array(ll$trend_info,c(1),pdims$dims[[1]]$ipoint[pp],drop=TRUE))[[1]],digits=2)
      mtext( paste0("p-value"," / ", "rel.trend"," / ","abs.trend "), side=3,adj=1,line = 3, cex=0.6)
      mtext(paste0(trinfo[,3],"  /  ",trinfo[,4], "  /  ",trinfo[,5]), side=3,adj=1, line = 2, cex=0.6)
    }



    }

    box()

    if(!is.null(output)){
      if (output!="dev.new") dev.off()
    }
  }
}


get_matching_tdims_index<-function(ind_dat,...){
  inarg<-list(...)
  cdim=Reduce(intersect,lapply(ind_dat, function(x) x$index_info$idims))# intersect to union, wo hat das Konsequenzen?
  if( is.element("ens",cdim)) cdim=cdim[-(which(cdim=="ens"))]

    test<-lapply(ind_dat, function(x) if (!all.equal(x$index_info$idims[!is.element(x$index_info$idims,"ens")],cdim )) stop("at least one climindvis_index object is missing a dimension"))


  seldims=list()
  griddims<-which(is.element(cdim,c("p","lon","lat")))
  for (cd in cdim[-griddims]){
    help<-Reduce(intersect,lapply(ind_dat, function(x) dimnames(x$index)[[which(x$index_info$idims==cd)]]))
    if (length(help)==0 | is.null(help)) {stop(paste0("<<",cd,">> dimension does not have common entries for all list elements"))
    } else seldims[[cd]]<-help
  }
  if(!is.null(inarg$selagg)){
    if (is.element("agg",cdim) & inarg$selagg[1]!="all"){
      if(!is.character(inarg$selagg)) inarg$selagg=as.character(inarg$selagg)
      seldims$agg=Reduce(intersect,list(seldims$agg,inarg$selagg))
      if(length(seldims$agg)==0) stop("one or more of the climindvis objects does not contain the aggregations you have chosen in <<selagg>>")
    }
  }
  if(!is.null(inarg$selyears)) {
    if(inarg$selyears[1]!="all"){
      if(!is.character(inarg$selyears)) inarg$selyears=as.character(inarg$selyears)
      help<-Reduce(intersect,list(seldims$year,inarg$selyears))
      if (length(help) == 0) stop("none of the years you have selected in <<selyears>> are  available in the climindvis object")
      if(length(help)<length(inarg$selyears)) warning("not all years selected in <<selyears>> are available in climindvis objects, only available years are used")
      seldims$year=help
    }
  }
  return(seldims)
}

get_matching_tdims_index_multi_agg<-function(ind_dat,...){
  inarg<-list(...)
  cdim=Reduce(union,lapply(ind_dat, function(x) x$index_info$idims))# intersect to union, wo hat das Konsequenzen?
  seldims=list()
  griddims<-which(is.element(cdim,c("p","lon","lat")))
  test <- unlist(lapply(ind_dat, function(x) length(dim(x$index))== length(cdim)))
  climallagg <- ind_dat[test]

  for (cd in cdim[-griddims]){
    seldims[[cd]]<-lapply(climallagg, function(x) dimnames(x$index)[[which(x$index_info$idims==cd)]])
  }
  if(!is.null(inarg$selagg)){
    if (is.element("agg",cdim) & inarg$selagg[1]!="all"){
      if(!is.character(inarg$selagg)) inarg$selagg=as.character(inarg$selagg)
      if(any(nchar(inarg$selagg)<2)){
        inarg$selagg[nchar(inarg$selagg)<2] <- paste0(0,inarg$selagg[nchar(inarg$selagg)<2])
      }
      seldims$agg= lapply(seldims$agg, function(x) {intersect(x, inarg$selagg)})
      if(length(seldims$agg)==0) stop("one or more of the climindvis objects does not contain the aggregations you have chosen in <<selagg>>")
    }
  }

  return(seldims)
}

match_pdims_index<- function(ind_dat,...){
  inargs<-list(...)

  mlon<-  Reduce(intersect,lapply(ind_dat, function(x) round(x$lon,3)))
  mlat<-  Reduce(intersect,lapply(ind_dat, function(x) round(x$lat,3)))
  mname<- Reduce(intersect,lapply(ind_dat, function(x) x$data_info$pnames))
  if (length(mname) < length(mlon)) {names=NULL
  message("Point names are not provided or do not match, please check pnames in data_info of climindvis objects. Names won't be used for plot.")
  }
  if(!is.null(inargs$selpoints)){
    if(is.character(inargs$selpoints)){
      if (is.null(names)) stop(" point names (<<data_info$pnames>>) are not specified in climindvis_index object. points cannot be selected by names, please use index in <<selpoints>>")
      sel<-which(is.element(mname,inargs$selpoints))
    } else {
      len<-unique(sapply(ind_dat, function(x) length(x$lat)))
      if(length(len)>1 || len!=length(mlon) ) stop("climindvis objects need to have same dimensions in order to select points with index in <<selpoints>>")
      if (length(ind_dat) > 1) {for (i in 1:(length(ind_dat)-1)) if(sum(round(ind_dat[[i]]$lon,2)-round(ind_dat[[i+1]]$lon,2)!=0)) stop("Coordinates of climindvis objects are not the same or not in the same order, in order to select points with <<selpoints>> coordiantes need to be the same for all points")}
      sel=inargs$selpoints
    }
    mlon<-mlon[sel]
    mlat=mlat[sel]
    mname=mname[sel]
  }
  ipoint<-array(NA)
  pname<-array(NA)

  help<-lapply(ind_dat, function(x) {
    for (i in 1:length(mlon)) {
      ipoint[i]=which(round(x$lon,3)==mlon[i] & round(x$lat,3) == mlat[i])
      if (!is.null(names) ) {
        if(x$data_info$pnames[ipoint[i]] == mname[i]) { pname[i]= x$data_info$pnames[ipoint[i]]
        } else { pname[i]=NA
        warning(paste0("no name for point at ",mlon[i],",",mlat[i]))
        }
      }}
    return(list(ipoint=ipoint,pname=pname) )})
  pdims=list()
  pdims$pnumber<-length(mlon)
  pdims$dims=help
  pdims$lon=mlon
  pdims$lat=mlat
  pdims$pnames=mname
  return(pdims)
}

get_plot_data_points<-function(ind_dat,iagg,years,ip,pdiminfo){

  pdata<-mapply(function(x,y,z) {
    if(!is.na(z)){
      help<-index_array(x$index, c(1,which(x$index_info$idims=="agg"),which(x$index_info$idims=="year")), list(y$ipoint[ip],z,years))
    } else
      help<-index_array(x$index, c(1,which(x$index_info$idims=="year")), list(y$ipoint[ip],years))

    if(length(z)==1 & length(ip)==1) {
      data<-apply(help,c(1,which((x$index_info$idims=="year"))),mean,na.rm=TRUE)
    } else {
      data<-apply(help,c(1,which(x$index_info$idims=="agg"),which((x$index_info$idims=="year"))),mean,na.rm=TRUE)
    }

    if (is.element("ens",x$index_info$idims)){
      sd<-apply(help,which((x$index_info$idims=="year")),sd,na.rm=TRUE)
      return(list(data=data,sd=sd))
    } else  return(list(data=data, sd=NA))
  },ind_dat,pdiminfo,iagg,SIMPLIFY = FALSE)
  return(pdata)
}

get_plot_data_points_trend<-function(ind_dat,iagg,years,ip,pdiminfo){
  tdata<-mapply(function(x,y) {
    if(!is.na(iagg)){
      help<-index_array(x$index_trend, c(1,which(x$index_info$idims=="agg"),which(x$index_info$idims=="year")), list(y$ipoint[ip],iagg,years),drop=TRUE)
    } else {help<-index_array(x$index_trend, c(1,which(x$index_info$idims=="year")), list(y$ipoint[ip],years),drop=TRUE)}
    data<-help
    if (is.element("ens",x$index_info$idims)){
      # calculate median and sd for flotting ensemble
      data<-apply(help,c(which(dim(help)==length(years)),which(dim(help)==4)),median,na.rm=TRUE)
      sd<-apply(help,c(which(dim(help)==length(years)),which(dim(help)==4)),sd,na.rm=TRUE)
      return(list(data=data,sd=sd))
    } else  return(list(data=data, sd=NA))
  },ind_dat,pdiminfo,SIMPLIFY = FALSE)
  return(tdata)
}

get_trend_info <- function(ind_dat,agg,pdims,nagg,pp) {
  infos <- mapply(function(x,y){index_array(x$trend_info, switch(is.na(agg)+1,c(which(is.element(x$index_info$idims,c("p")))),
                                                                 c(which(is.element(x$index_info$idims,c("p"))),which(x$index_info$idims=="agg"))),
                                            switch(is.na(agg)+1,list(y$ipoint[pp]), list(y$ipoint[pp],which(agg%in%nagg))),drop=TRUE)}, x=ind_dat,y=pdims)
  dattype <- sapply(ind_dat, function(x) x$data_info$type)
  if(is.element("p_hc", dattype)){
    infos <- infos[[-which(dattype=="p_hc")]]
  }
  infos <- round(t(as.matrix(infos)),digits=2)
  return(infos)
}

get_pinfo <- function(ind_dat,type,ylims,opargs){
  pinfo<- list()
  pinfo$index_name <- sapply(ind_dat, function(x) x$index_info$iname)
  if(is.null(ylims)){
    pinfo$ylims=c(range(sapply(ind_dat, function(x) x$index),na.rm=TRUE))
  } else pinfo$ylims <- ylims
  if (is.null(ind_dat[[1]]$index_info$iformat)){
    pinfo$ylab=""
    pinfo$yaxis=FALSE
  } else if(ind_dat[[1]]$index_info$iformat=="perc"){
    pinfo$ylab<- "% days"
    pinfo$yaxis=TRUE
  } else if(ind_dat[[1]]$index_info$iformat=="days"){
    pinfo$ylab="days"
    pinfo$yaxis=FALSE
  } else if(ind_dat[[1]]$index_info$iformat=="dayscount"){
    pinfo$ylab<- paste0("days since ",substring(ind_dat[[1]]$index_info$start_days,6,10))
    pinfo$yaxis=FALSE
  } else if(ind_dat[[1]]$index_info$iformat=="SPI"){
    pinfo$ylab=paste0("SPI ", ind_dat[[1]]$index_info$timescale)
    pinfo$yaxis=FALSE
  } else {
    pinfo$ylab=paste0(ind_dat[[1]]$index_info$iformat)
    pinfo$yaxis=FALSE
  }
  if (!is.null(opargs$title)){
    pinfo$titlestring<-opargs$title}
  return(pinfo)
}

plot_polyt <- function(tdata,colv,cex,i){
  polygon(c(1:length(tdata[[i]]$data[,1]),length(tdata[[i]]$data[,1]):1),c(tdata[[i]]$data[,2],rev(tdata[[i]]$data[,3])),border=NA,col=rgb(colv[1],colv[2],colv[3], alpha=70, maxColorValue = 255),cex=cex)
  lines(tdata[[i]]$data[,1],type="l",col=rgb(colv[1],colv[2],colv[3],maxColorValue = 255),cex=cex)
  lines(tdata[[i]]$data[,4],type="l",col=rgb(colv[1],colv[2],colv[3],maxColorValue = 255),lty=2,cex=cex)
}


write_trend<- function(trinfo,text_cex, dif = FALSE){

  dl <- length(dim(trinfo))
  trend_type <- c("rel.trend", "abs.trend")
  #text <- switch(all(trinfo[,5]==-99.9)+1,paste0("p-value"," / ", "rel.trend "," / ","abs.trend "),paste0("p-value"," / ", "rel.trend "))
  ifelse(all(trinfo[,4:5]== -99.9),types <- c(FALSE,FALSE),
         ifelse(all(trinfo[,4]== -99.9),types <- c(FALSE, TRUE), ifelse(all(trinfo[,5]==-99.9),types <- c(TRUE,FALSE),types <- c(TRUE,TRUE))))
  #types <- ifelse(all(trinfo[,4:5]== -99.9),FALSE, TRUE)
  text <- paste("p-value", paste0(trend_type[types], collapse=" / "), sep = "  / ")
  trinfo <- as.data.frame(trinfo)
  for (i in 4:5) {trinfo[,i] <- ifelse(trinfo[,i]>=0,paste0(" ",trinfo[,i]),trinfo[,i])}
  trinfo[trinfo==-99.9] <- NA
  trinfo[is.na(trinfo)] <- " ----  "

  if(all(types)){
    trds <- paste0(trinfo[,4], " [%] /  ",trinfo[,5], " [u/decade]")
  } else if (all.equal(types,c(TRUE,FALSE))==TRUE) {
    trds <- paste0("  / ",trinfo[,4], " [%] ")
    }
  else if (all.equal(types,c(FALSE,TRUE))==TRUE){
    trds <- paste0(" ",trinfo[,5], " [u/decade] ")
  } else if (all(!types)){
    trds <- paste0(" no trends calculated. ")
  }

  vals <- paste0(trinfo[,3], " / ",trds)

  lines <- rev(seq(switch((length(trinfo[,1])>2)+1,1,0),3,length.out=length(trinfo[,1])+1))*text_cex
  ifelse(lines>=4, tcex <- 0.6*text_cex, tcex<- 0.8*text_cex)

  mtext(text, side=3, line = max(lines), cex=tcex,adj=1)
  mtext(vals, side=3, adj=1,line = lines[-which.max(lines)], cex=tcex)
  mtext(paste0("Trend: ",paste0(get_method(trinfo[,6], dif), collapse = " | ")), side=3, line = 0, cex=tcex, adj=0)
}
