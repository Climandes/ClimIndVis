### get_quants new


get_quants_new <- function(climindvis,var,qargs,sel_time){

  ifelse(grepl("grid",climindvis$data_info$type),gd<-c(1,2),gd<-1)
  switch(grepl("fc|hc",climindvis$data_info$type)+1,ed<-NULL,ed<-tail(gd,1)+1) # in welcher dimension sind die members

  agg_factor <- as.factor(gsub(".*-","",sel_time$tfactor))
  aggd=length(sel_time$aggnames)>1

  if (is.null(qargs$th_quantiles)){
    #

    # 3) Calculate quantiles
    # for temperature
    if (var %in% c("tmin","tavg","tmax")) {

     if(!is.null(ed)) {
       edim=dim(climindvis$data[[var]])[ed]
       if(qargs$inbase) {
         qargs$inbase <- FALSE
         message("<<inbase>> set to FALSE. Bootstrapping for ensemble is computational intensive.")
       }
       # if(qargs$qens_all){ ## brauchen wir das noch?
       #   agg_factor_h=rep(agg_factor,each=edim)
       # } else gd=c(gd,ed)
     }

      quants <- get_temp_quantiles(climindvis,var,qargs, ed, sel_time)
      # here quants is already inbase and outbas


    }
    # for precipitation
    if (var == "prec") {

      cdim <- dim(climindvis$data[[var]])
      ind <- as.numeric(gsub("-.*","",sel_time$tfactor))
      agg_factor[ind<qargs$baseperiod[1] | ind > qargs$baseperiod[2]]=NA
      agg_factor_h<-agg_factor

      dat_help_q<-climindvis$data[[var]]

      if(!is.null(ed)) {
        edim=cdim[ed]
        if(qargs$qens_all){
          agg_factor_h=rep(agg_factor_h,each=edim)
        } else gd=c(gd,ed)
      }



    if(qargs$qens_all & !is.null(ed)){
      dim(dat_help_q) <- c(prod(cdim[1:(length(cdim)-2)]), edim*cdim[length(cdim)])
    } else {dim(dat_help_q) <- c(prod(cdim[1:(length(cdim)-1)]), cdim[length(cdim)])}

    quants <- apply(dat_help_q,1, FUN=get_prec_quantiles, sel_time =sel_time, qargs=qargs, ed=ed,gd=gd, agg_factor_h=agg_factor_h)
    aggnames=dimnames(quants)[[1]]

    if(!is.null(ed) & qargs$qens_all)  {
      quants=replicate(edim,quants)
      dq<- dim(quants)
      switch(aggd+1,dim(quants) <- c(prod(dq)), dim(quants) <- c(dq[1],prod(dq[2:length(dq)])))
     }
    ifelse(is.null(dim(quants)),dq<-length(quants),dq<-dim(quants))
    if(!aggd) { quants<-array(quants,dim=c(1,dq))}
    dimnames(quants) <- list(aggnames, rep(NULL,dq))
    quants <- list(base=quants)

    }

  } else {

    cdim <- dim(climindvis$data[[var]])
    if(!is.null(ed)) edim=cdim[ed]
    quants=list(base=qargs$th_quantiles)
    if (dim(quants$base)[length(dim(quants$base))]!=prod(cdim[1:ed])) {
      qhelp=array(quants$base,dim=c(dim(quants$base)[1],cdim[1:ed]))
      if(qargs$qens_all){
        if (length(apply(qhelp, gd+1,function(qq) unique(array(qq))))!= prod(dim(qhelp)[gd+1])){
          stop("if <<qens_all>>==TRUE thresholds provided in th_object need to be the same for all ensemble members for one grid point/station.")
        }
        quants$base = array(qhelp,dim=c(dim(qhelp)[1],prod(cdim[1:ed])),dimnames=list(dimnames(quants$base)[[1]]))
        message("Warning when calculating quantiles: ")
        message("number of ensemble members in th_object and climindvis object are not the same. Same thresholds used for all members.")
      } else {
        stop(" number of ensemble members in th_object and climindvis object are not the same, if <<qens_all>>==FALSE, thresholds are calculated individually for each member and thresholds in th_object need to have the same ensemble dimension as the climindvis object.")
      }

    }

  }

  switch((qargs$inbase==TRUE)+1,return(list(base=quants$base)),
    return(list(inbase=quants$inbase, base=quants$base)))

}



get_temp_quantiles <- function (climindvis,var,qargs,ed, sel_time)
  {

  bootstrap_range <- as.Date(c(paste0(qargs$baseperiod,c("-01-01","-12-31"))))
  dates_base <-  seq(bootstrap_range[1], bootstrap_range[2], by = "day")

  base_val_nl <- which(climindvis$time%in%dates_base & substr(climindvis$time,6,10)!="02-29")

  if(!is.null(ed)){
    temp_base <- get_full_year(climindvis,var, bootstrap_range, base_val_nl,ed)
    tbdim <- dim(temp_base)
    tbl <- length(tbdim)
    if(qargs$qens_all){
      temp_base <- aperm(temp_base,perm=c(1:(tbl-2),tbl, tbl-1))
      dim(temp_base) <- c(prod(tbdim[1:(tbl-2)]), prod(tbdim[(tbl-1):tbl]))
      bootstrap_range = FALSE
    } else {
      dim(temp_base) <- c(prod(tbdim[1:(length(tbdim)-1)]), tbdim[length(tbdim)])
    }
  } else {
    temp <- climindvis$data[[var]]
    tdim <- dim(temp)
    dim(temp) <- c(prod(tdim[1:(length(tdim)-1)]), tdim[length(tdim)])
    temp_base <- temp[,base_val_nl]
  }


  outbase <- apply(temp_base,1, FUN=zhang_running_qtile,dates_base,qargs$qth, bootstrap_range = bootstrap_range,
                   n = qargs$n, min_fraction = qargs$min_base_fraction, NAmaxQ = qargs$NAmaxQ)

  if(!is.null(ed) & qargs$qens_all){
     ## increase qdat to ensemble again
    outbase <- array(rep(outbase,tbdim[tbl-1]),dim=c(365,prod(tbdim[1:(tbl-1)])))
  }

  if(qargs$inbase){
    inbase_list <- sapply(apply(temp_base,1, FUN=zhang_running_qtile, dates_base,  qargs$qth, bootstrap_range= bootstrap_range,
      n = qargs$n, get_bootstrap_data = TRUE, min_fraction=qargs$min_base_fraction, NAmaxQ = qargs$NAmaxQ), as.matrix)

    inbase <- do.call(get("abind",asNamespace("abind")),c(inbase_list, along=4))
    dib<- dim(inbase)
    dim(inbase) <- c(dib[1]*dib[2],dib[3:length(dib)])
    return(list(base=outbase, inbase=inbase))

  }
  return(list(base=outbase))

}


get_full_year <- function(climindvis,var, bootstrap_range, base_val_nl,ed){
  dat <- climindvis$data[[var]]
  dd <- dim(dat)
  tdat <- climindvis$time
  new_time <- seq(tdat[1],bootstrap_range[2], by ="day")
  new_time_noleap <- new_time[-which(substr(new_time,6,10)=="02-29")]
  datn <- array(NA, dim=c(dd[1:(length(dd)-1)], length(new_time_noleap)))
  switch((!ed==2)+1,datn[,,which(new_time_noleap%in%tdat)] <- dat[,,base_val_nl],datn[,,,which(new_time_noleap%in%tdat)] <- dat[,,,base_val_nl])
  return(datn)

}


### here, we have to include the choice of aggregation via sel_time
get_prec_quantiles <- function (prec, sel_time, qargs, ed, gd, agg_factor_h){

  # only select wet days above threshold
  wet_days <- !(is.na(prec) | prec < qargs$dd_threshold)
  prec <- prec[wet_days]
  agg_factor_h <- agg_factor_h[wet_days]

  if(!is.null(ed) & qargs$qens_all) prec<-array(prec)
  if (sel_time$aggnames[1]!="annual"){
    if(!all(grepl("-", sel_time$tfactor)) & length(sel_time$aggnames)==1) {
      levels(agg_factor_h)<- rep(sel_time$aggnames,length(levels(agg_factor_h)))
    }
    q_thresh<- climdex.pcic:::tapply.fast(prec, agg_factor_h,  function(x){
      #xl=length(x)
      if (any(is.na(x))) {
        x=x[-(which(is.na(x)))]
      }
      # instead of relative NA threshold, use an absolute NA threshold for Quantile calculation, i.e. at least 300 values for quantiles...
      if (length(x) < qargs$NAmaxQ){
        thresh=NA
        message("There is less than NAmaxQ values within your base period. Consider changing your baseperiod/check your input data.")
      } else thresh <- quantile(x,probs = qargs$qth, na.rm = TRUE, type=8)

      return(thresh)})
    #names(q_thresh) <- unique(gsub(".*-","",levels(sel_time$tfactor)))
    #if (length(q_thresh)==1) q_thresh=c(remove=NA,assign(names(q_thresh),q_thresh))
    if (length(q_thresh)==1) q_thresh=matrix(q_thresh,nrow=1,ncol=1,dimnames=list(names(q_thresh),names(q_thresh)))
  } else{
    dl=length(prec)
    if (any(is.na(prec))) {
      prec=prec[-(which(is.na(prec)))]
    }
    if (length(prec) < qargs$NAmaxQ){
      q_thresh=NA
    } else q_thresh=quantile(prec,probs = qargs$qth, type=8)

  }

  return(q_thresh)
}


zhang_running_qtile <- function (temp, dates_base, qtiles, bootstrap_range,
          n = 5, get_bootstrap_data = FALSE, min_fraction = 0.2, NAmaxQ=365)
{
 ## for selection of running window
  switch(is.logical(bootstrap_range)+1,inset <- climdex.pcic:::get.bootstrap.set(dates_base, bootstrap_range),inset <- rep(TRUE, length(temp)))
  dpy <- 365

  if (length(temp) < NAmaxQ){
    qdat=NA
    message("There is less than NAmaxQ values within your base period. Consider changing your baseperiod/check your input data.")
    return(qdat)
  } else {

    nyears <- floor(sum(inset)/dpy)
    bs_data <- temp[inset]
    qdat <- NULL
    if (get_bootstrap_data) {
      d <- .Call(get("running_quantile_windowed_bootstrap",asNamespace("climdex.pcic")), bs_data,
      # d <- .Call("running_quantile_windowed_bootstrap", bs_data,
        n, qtiles, dpy, min_fraction, PACKAGE = "climdex.pcic")
      dim(d) <- c(dpy, nyears, nyears - 1, length(qtiles))
      qdat <- lapply(1:length(qtiles), function(x) {
        r <- d[, , , x, drop = FALSE]
        dim(r) <- dim(r)[1:3]
        r
      })
    }
    else {
      qdat <- running_quantile(bs_data, n, qtiles, dpy, min_fraction)
    }
    return(qdat)
  }

}





running_quantile <- function (data, n, q, dpy, min.fraction){
  ret <- .Call(get("running_quantile_windowed",asNamespace("climdex.pcic")), data, n, q, dpy,
    min.fraction, PACKAGE = "climdex.pcic")
  dim(ret) <- c(length(q), dpy)
  return(t(ret))
}

