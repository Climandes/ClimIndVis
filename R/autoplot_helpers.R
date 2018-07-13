
#gets data for point and grid data
get_data_list<-function(data_names){
  pf=parent.frame()
  tryCatch({
  if (pf$grid==0 & pf$points==1){
    datn=as.list(paste0(data_names,"_p"))
  } else if (pf$grid==1 & pf$points==0) {
    datn=as.list(paste0(data_names,"_grid"))
  } else if (pf$grid==1 & pf$points ==1){
    datn=as.list(c(paste0(data_names,"_p"),paste0(data_names,"_grid")))
  } else stop("either grid or point data have to be provided as input")

  dat=lapply(datn[!sapply(datn,function(x) is.null(pf[[x]]))], function(dd) pf[[dd]])
  names(dat)=lapply(datn[!sapply(datn,function(x) is.null(pf[[x]]))], function(dd) dd)
  return(dat)
  }, error=function(cond){
    message("error in input data:")
    message(cond)
    stop_quietly()
  })

}

# function to seect only matching time slices of different climindvis objects and/or select selyears
#data = named list of climindvis objects
cut_to_same_dates<-function(data,selyears=NULL){
  tryCatch({
    if(any(is.element(class(data), "climindvis"))) data<-list(data)
    sapply(data,function(x) check_class(x,"climindvis"))
    if(length(data) ==1 & is.null(selyears)){
      return(data)
    } else {

      tfactors=lapply(data, function(dd) factor(dd$time))
      tintersect=factor(Reduce(intersect,tfactors))
      fc=which(sapply(data, function(x) grepl("fc",x$data_info$type )))
      if(length(fc)>0){
        dfactors=lapply(data, function(dd) factor(dd$time_factors$jdays))
        dintersect=factor(Reduce(intersect,dfactors))
        tfactors[fc]=NA
        tintersect=factor(Reduce(intersect,tfactors[-fc]))
        if(length(dintersect)==0) stop("no matching time steps in all climindvis objects")
      }
      if(!is.null(selyears)){
        tintersect=tintersect[which(is.element(substring(tintersect,1,4),as.character(selyears)))]
      }
      if(length(tintersect) == 0) stop("no matching time steps in climindvis objects")

      if (length(fc)>0){
        selt=mapply(function(tt,jj) {
          if (!is.na(tt[1])) {
            return(which(is.element(tt,tintersect) & is.element(jj,dintersect)))
          } else return(which(is.element(jj,dintersect)))}
          ,tfactors, dfactors)
      } else {
        selt=lapply(tfactors, function(tt) which(is.element(tt,tintersect)))
      }

      data_out=mapply(function(dd,yy) {
        dd$time=dd$time[yy]
        for(x in 1:length(dd$time_factors)){
          dd$time_factors[[x]]=factor(dd$time_factors[[x]][yy])
        }
        for (x in 1:length(dd$data)){
          #Daten auch zuschneiden
          # alle in array umformen mit nur letzter Spalte Zeit alle anderen zusammen. sel replizieren
          d=dim(dd$data[[x]])
          ld=length(d)
          dhelp <- array(dd$data[[x]],dim=c(prod(d[1:(ld-1)]),d[ld]))[,yy]
          dd$data[[x]]=array(dhelp,c(d[1:(ld-1)],dim(dhelp)[-1]),dimnames=c(dimnames(dd$data[[x]])))
        }
        return(dd)
      },data,selt,SIMPLIFY = FALSE)

      return(data_out)
  }

  }, error=function(cond) {
    message("error when cutting out same time steps for climindvis_index objects:")
    message(cond)
    stop_quietly()
  })
}

get_index_array_input<-function(index,dimnames,dimsel){
  dims=which(is.element(index$index_info$idims,dimnames))
  sel=lapply(seq_along(dimnames), function(dn){
    r<-which(is.element(dimnames(index$index)[[dims[dn]]],dimsel[[dn]]))
    if(length(r)==0) r<-NULL
    return(r)
  } )
  return(list(dims=dims,sel=sel))
}

cut_to_same_dates_index<-function(index_data,selyears=NULL){
  tryCatch({
  if(any(is.element(class(index_data), "climindvis_index"))) data<-list(data)
  sapply(index_data,function(x) check_class(x,"climindvis_index"))
  if(length(index_data) ==1 & is.null(selyears)){
    return(index_data)
  } else {

    aggt=unique(sapply(index_data, function(dd) factor(dd$index_info$aggt)))
    if(length(aggt)>1) stop("aggregation needs to be the same for all input objects")

    agg=Reduce(intersect,lapply(index_data, function(dd) dd$index_info$aggnames))
    if(length(agg)==0) stop("no matching aggregations in climindvis_index objects")
    if(any(is.element(month.abb,agg))) agg=pad2(which(is.element(month.abb,agg)))
    fc=which(sapply(index_data, function(x) grepl("fc",x$data_info$type )))


    index_out=list()
    if(length(fc)>0){
      years_fc=Reduce(intersect,lapply(index_data[fc], function(dd) dd$index_info$years))
      if (length(years_fc)==0) stop("no matching years in climindvis_index forecast objects")
      years=Reduce(intersect,c(lapply(index_data[-fc], function(dd) dd$index_info$years),list(selyears)))
      if (length(years)==0) stop("no matching years/no years selected in <<selyears>> in climindvis_index objects" )
      index_out[fc]<-lapply(index_data[fc], function(dd){
        sel=get_index_array_input(dd,c("agg","year"),list(agg,years_fc))
        dd$index=index_array(dd$index,sel$dims,sel$sel)
        return(dd)
      })

      index_out[-fc]<-lapply(index_data[-fc], function(dd){
        sel=get_index_array_input(dd,c("agg","year"),list(agg,years))
        dd$index=index_array(dd$index,sel$dims,sel$sel)
        return(dd)
      })
    } else {
      years=Reduce(intersect,c(lapply(index_data, function(dd) dd$index_info$years),list(selyears)))
      if (length(years)==0) stop("no matching years/no years selected in <<selyears>> in climindvis_index objects" )
      index_out<-lapply(index_data, function(dd){
        sel=get_index_array_input(dd,c("agg","year"),list(agg,years))
        dd$index=index_array(dd$index,sel$dims,sel$sel)
        return(dd)
      })
    }
    return(index_out)
  }
  }, error=function(cond) {
    message("error when cutting out same time steps for climindvis_index objects:")
    message(cond)
    stop_quietly()
  })
}

#testet ob Index Quantile benutzt
is_index_special<-function(index){
  r<-ifelse (is.element(index,c("qth","qtot","rXptot","tn10p","tx10p","tn90p","tx90p","csdi","wsdi")),TRUE,FALSE )
  return(r)
}
get_leg_units<-function(ind_dat,trend=FALSE){

  units=Reduce(union,lapply(ind_dat[!sapply(ind_dat,is.null)], function(id) id$index_info$iformat))
  if (length(units)>1) stop("the units of the different indices should be the same")
  if(units=="dayscount") units="days"
  if(units=="perc") units = "%"
  if(units=="degreeC") units="C"
  if(units=="days_since") {
    nc=nchar(ind_dat[[1]]$index_info$start_days)
    units = ifelse(trend,"days",paste0("days since ",substring(ind_dat[[1]]$index_info$start_days,nc-4,nc)))
  }
  ret=ifelse(trend,units,paste0("[",units,"]"))
  return(ret)
}


get_plot_args<-function(autoplot){
  env=parent.frame()
  if(autoplot=="forecast_map"){
    ifelse(env$col == "default",ch <- colorRampPalette(get_default_color_fc(env$index, env$iname)$col)(env$ncat),ch <- colorRampPalette(col)(env$ncat))
    breaks=c(0,100,110,sapply(1:env$ncat*100 , function(n) n+ seq(40 ,110,10)),(env$ncat+1.55)*100)
    if(is.null(env$skillmin)) breaks=breaks[1:(length(breaks)-2)]
    env$plot_args[c("p_breaks","g_breaks")] <-list(breaks)
    cols=c(env$nocat_col,"white",sapply(ch , function(n) c(colorRampPalette(c("white",n))(9)[c(2,4:9)],"white")),env$noskill_col)
    if(is.null(env$skillmin)) cols=cols[1:(length(cols)-2)]
    env$plot_args[c("g_col","p_col")]=list(cols)

    env$plot_args$leg_labs=gsub("40","<40",c("no cat/NA",paste0(rep(seq(40 ,100,20),env$ncat),"%"),switch(is.null(env$skillmin)+1,"no skill",NULL)))
    env$plot_args$fc_catnames=env$cat_names
    env$plot_args$leg_title=ifelse(is.null(env$leg_title),"",env$leg_title)
    env$plot_args[c("p_zlims","zlims")]=list(range(env$plot_args[c("p_breaks","g_breaks")]))

  } else if (autoplot=="climatology_map"){
    iname=Reduce(union,lapply(env$ind_dat, function(id) id$index_info$iname))
    env$plot_args["units"] = get_leg_units(env$ind_dat) #paste0("[",Reduce(union,lapply(env$ind_dat, function(id) id$index_info$iformat)),"]")

    if (env$col == "default"){
      if (env$plot_value == "sd" | env$plot_value == "relsd"){
        env$plot_args[c("p_col", "g_col")] = list(colorRampPalette(c("lightgrey", "palevioletred4", "slateblue4"))(10))
      } else {
        def_col = get_default_color(env$index, iname)
        env$plot_args[c("p_col", "g_col")] <- list(def_col$col)
        env$plot_args[c("p_col_center", "g_col_center")] = list(def_col$center)
      }
    } else {
      env$plot_args[c("p_col", "g_col")] = list(env$col)
      env$plot_args[c("p_col_center", "g_col_center")] = FALSE
    }

  } else if (autoplot=="trend_map"){

    }
}


get_plot_title<-function(titlestring,show_title=TRUE,autoplot,aa=NULL,yy=NULL,pp=NULL,vv=NULL,cname=NULL,tname=NULL,abs=FALSE,sig_lev,tmet){
  env=parent.frame()
  iinfo=lapply(env$ind_dat, function(i) i$index_info)
  dnames=lapply(env$ind_dat, function(i) i$data_info$data_name )
  fc<-which(grepl("fc",names(iinfo)))
  hc<-which(grepl("hc",names(iinfo)))
  if(length(c(fc,hc))>0){
    fcmon=env$ind_dat[[c(fc,hc)[1]]]$data_info$fmon
  }
  st=which(grepl("p",names(iinfo)))
  stn<-iinfo[[st[1]]]$pname
  if(length(fc)==0){
    years=as.integer(iinfo[[1]]$years)
  } else if(length (hc)>0){
    years=as.integer(iinfo[[hc[1]]]$years)
  }

  if (all(diff(years)) == 1) {
    yearnames = paste(range(years), collapse = "-")
  } else yearnames = paste(years, collapse = "+")

  output=FALSE
  if(!is.null(env$output)){
    if (env$output!="dev.new") output=TRUE
  }
  if(autoplot=="forecast_map"){
    if(show_title){
      t <- list(utitle=switch((titlestring != "")+1, NULL,titlestring),
                title=paste0(iinfo[[1]]$iname," forecast ",iinfo[[1]]$aggnames[aa]," ", iinfo[[fc[1]]]$years[yy]),
                period = paste0("Initialized: 01-", fcmon,"-",iinfo[[fc[1]]]$years[1]," (Model climatology:", yearnames, ") "),
                skill = switch(is.null(env$skillmin)+1,paste0(env$veri_metric," >= ",env$skillmin),NULL),
                datap = switch(env$points==1,paste0("Stations: ",paste0(unique(dnames[grepl("p",names(env$ind_dat))]),collapse=",")),NULL),
                datag = switch(env$grid==1,paste0("Grid: ",paste0(unique(dnames[grepl("grid",names(env$ind_dat))]),collapse=",")),NULL))

    } else {t <- NA}

     f=ifelse(!is.null(env$output),paste0(env$plotdir, env$plotname, ifelse(env$plotname == "", "", "_"), "forecast_", replace_operator_name(iinfo[[1]]$iname), ifelse(!is.null(env$skillmin),paste0("_",env$veri_metric,"_gt_",env$skillmin),""), "_", iinfo[[1]]$aggnames[aa],"_",iinfo[[fc[1]]]$years[yy],"_fcmon",fcmon,"_hc",yearnames),"")

  } else if (autoplot=="forecast_point"){
    if(show_title){
      t <- list(utitle=switch((titlestring != "")+1, NULL,titlestring),
                title=paste0(iinfo[[1]]$iname," forecast ",iinfo[[1]]$aggnames[aa]," ", iinfo[[fc[1]]]$years[yy]),
                station = ifelse(!is.null(stn[pp]),stn[pp],paste0("station",pp)),
                period = paste0("Initialized: 01-", fcmon,"-",iinfo[[fc[1]]]$years[1]," (Model climatology:", yearnames, ") "),
                skill = switch(env$verify+1,NULL,paste0(env$veri_metric," >= ",env$skillmin)),
                data = paste0(unique(dnames[grepl(paste(c("hc","fc"),collapse="|"),names(env$ind_dat))]),collapse=","))
    } else {t <- NA}
    f=ifelse(!is.null(env$output),paste0(env$plotdir, env$plotname, ifelse(env$plotname == "", "", "_"), "forecast_", replace_operator_name(iinfo[[1]]$iname), ifelse(!is.null(env$skillmin),paste0("_",env$veri_metric,"_gt_",env$skillmin),""), "_", iinfo[[1]]$aggnames[aa],"_",iinfo[[fc[1]]]$years[yy],"_fcmon",fcmon,"_hc",yearnames,"_",ifelse(!is.null(stn[pp]),stn[pp],paste0("st",pp)),".",env$output),"")

  } else if (autoplot=="verification_map"){
    if (!is.null(env$output)) {
      if(!file.exists(paste0(env$plotdir,vv,"/"))){
      dir.create(paste0(env$plotdir,vv,"/"))
      }
    }
    if(show_title){
      t <- list(utitle=switch((titlestring != "")+1, NULL,titlestring),
                title=paste0(iinfo[[1]]$iname, " ", iinfo[[1]]$aggnames[aa],"; ",vv, ifelse(is.null(cname),"",paste0("(",cname,")"))," ", ifelse(is.null(tname)," ",paste0(" ",tname))),
                period =paste0("Initialization: 01-", fcmon, " (Model climatology:", yearnames, ") "),
                datap = switch(env$points==1,paste0("Stations: ",paste0(unique(dnames[grepl("p",names(env$ind_dat))]),collapse=",")),NULL),
                datag = switch(env$grid==1,paste0("Grid: ",paste0(unique(dnames[grepl("grid",names(env$ind_dat))]),collapse=",")),NULL))
    } else {t <- NA}

    f=ifelse(!is.null(env$output),paste0(env$plotdir,vv,"/", env$plotname,ifelse(env$plotname == "", "", "_"), vv,ifelse(is.null(cname),"",paste0("_",cname)),ifelse(is.null(tname),"_",paste0("_",tname,"_")), replace_operator_name(iinfo[[1]]$iname), "_", iinfo[[1]]$aggnames[aa],"_fcmon",fcmon,"_hc",yearnames),"")

  } else if (autoplot=="climatology_map"){
    if(show_title){
      t <- list(utitle=switch((titlestring != "")+1, NULL,titlestring),
                title=paste0(iinfo[[1]]$iname," ",iinfo[[1]]$aggnames[aa]),
                period = paste0(env$plot_value," ",yearnames),
                data = paste0(ifelse(env$points==1,paste0("Stations: ",paste0(unique(dnames[grepl("p",names(env$ind_dat))]),collapse=","),ifelse(env$grid==1 & env$points==1," | ","")),"") ,
                              ifelse(env$grid==1,paste0("Grid: ",paste0(unique(dnames[grepl("grid",names(env$ind_dat))]),collapse=",")),"")))
    } else {t <- NA}

    f=ifelse(!is.null(env$output),paste0(env$plotdir, env$plotname, ifelse(env$plotname == "", "", "_"), replace_operator_name(iinfo[[1]]$iname),"_",env$plot_value, "_", iinfo[[1]]$aggnames[aa],"_",yearnames),"")

  } else if (autoplot=="trend_map"){
    if(show_title){
      t <- list(utitle=switch((titlestring != "")+1, NULL,titlestring),
                title=paste0(iinfo[[1]]$iname," ",iinfo[[1]]$aggnames[aa]),
                period = paste0("Period: ",yearnames),
                trend= paste0("Trend:  ",lapply(tmet, function(t) get_method(unique(t)))),
                sig = paste0("Sig.lev: ", sig_lev),
                data = paste0(ifelse(env$points==1,paste0("Stations: ",paste0(unique(dnames[grepl("p",names(env$ind_dat))]),collapse=","),ifelse(env$grid==1 & env$points==1," | ","")),"") ,ifelse(env$grid==1,paste0("Grid: ",paste0(unique(dnames[grepl("grid",names(env$ind_dat))]),collapse=",")),"")))
    } else {t <- NA}

    f=ifelse(!is.null(env$output),paste0(env$plotdir, env$plotname, ifelse(env$plotname == "", "", "_"), replace_operator_name(iinfo[[1]]$iname), "_",
                                         iinfo[[1]]$aggnames[aa],"_",ifelse(abs==TRUE,"abs-", "rel-"),"trend_",yearnames),"")


    }
  return(list(t=t,f=f))
}



calc_index_special_autoplot<- function(data,index,index_args, selyears){
  fc=which(sapply(data, function(dd) grepl("fc",dd$data_info$type )))
  if(length(fc)==0){
    ind_dat<-lapply(data, function(dd) do.call("calc_index",c(list(dd,index=index),index_args)))
  } else {
    ind_dat=list()
    ind_dat[(1:length(data))[-fc]]<-lapply(data[-fc], function(dd) do.call("calc_index",c(list(dd,index=index),index_args)))
    ind_dat[fc]<-lapply(fc, function (ff){
      index_args$th_object=ind_dat[[which(names(data)==gsub("fc","hc",names(dat)[ff]))]]
      do.call("calc_index",c(list(data[[ff]],index=index),index_args))})
  }
  names(ind_dat)=names(data)
  if (!is.null(selyears)){
    ind_dat<-cut_to_same_dates_index(ind_dat,selyears)
  }
  return(ind_dat)
}

get_skill_data<-function(veri_dat,vv,aa,tt,cc,aggl,nout,tdims){
  #same einbauen -> dann NA
  skill=lapply(veri_dat, function(vd){
    if(!is.null(vd)){
      if(!aggl & tdims==1){
        same=vd$same
        if(nout>1) {
          ret=vd$verification[[vv]][[cc]]
        } else ret=vd$verification[[vv]]
      } else {
        cd=ifelse(vd$data_info[[1]]$type=="p",1,2)
        dims=c(switch(aggl+1,NULL,cd+1), switch((tdims>1)+1,NULL,cd+aggl+1))
        sel=list(switch(aggl+1,NULL,aa),switch((tdims>1)+1,NULL,tt))
        if(!aggl){
          same=vd$same
        } else same=index_array(vd$same,cd+1,sel[1],drop=TRUE)
        if (nout>1){
          ret=index_array(vd$verification[[vv]][[cc]],dims,sel,drop=TRUE)
        } else ret=index_array(vd$verification[[vv]], dims,sel,drop=TRUE)
      }
      ret[same]=NA
    } else ret=NULL
    return(ret)
  })
}

is.even <- function(x) x %% 2 == 0


