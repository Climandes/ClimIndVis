

rainy_season_start<-function (temp, date_factor, rs_method,nval,dd_th=1,days=NA,th=NA,mdays=NA,mcdd=NA,th1=NA,th2=NA,NAdays=0){
  tryCatch({
  stopifnot(is.numeric(temp) && is.character(rs_method) && is.factor(date_factor))
  stopifnot(exists(paste0("rainy_season_start_",rs_method)))
  stopifnot(is.function(get(paste0("rainy_season_start_",rs_method))))
  }, error = function(cond){
  message("error in calculating start of rainy season: ")
  message(cond)
  stop_quietly()
  })
  start<-climdex.pcic:::tapply.fast(temp,date_factor,paste0("rainy_season_start_",rs_method),na_handling="strict",nval=nval,days=days,th=th,dd_th=dd_th,mdays=mdays,mcdd=mcdd,th1=th1,th2=th2,NAdays=NAdays)
  return(start)
}


rainy_season_start_sos<-function(x,na_handling="strict",nval,th1=25,th2=20,NAdays=0,...){

    fac=rep(seq(1,floor(length(x)/10)),each=10)[1:length(x)]
    if (length(fac)<length(x)) fac[(length(fac)+1):length(x)]=NA
    dec=tapply(x,fac,mean,na.rm=TRUE)*10
    decna=tapply(x,fac,function(x) return(length(which(is.na(x)))))
    dec[which(decna>NAdays)]=NA

    lt <- length(dec)
    test1 <- ifelse(dec>=th1,1,0)
    test2 <- ifelse(dec>=th2,1,0)

    val <- which(test1[1:(lt-2)]==1 & test2[2:(lt-1)]==1 & test2[3:lt]==1)[1]

    return_day=ifelse(length(val)==1,(val-1)*10+10,NA)
    if (!is.na(return_day) & any(is.na(dec))) if (na_handling=="strict" & val > which(is.na(dec))[1]) return_day=nval

  return(tdiff=return_day)
}

rainy_season_start_jd<-function(x,na_handling="strict",nval,dd_th=0.1,...){
  tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-30,length(x)-30)
  if(tend<1){
    return_day=nval
  } else {
    temp= x[1:tend]>=dd_th & consecsum(x[1:(tend+4)]>=dd_th,4,2)>3 & consecsum(x[1:(tend+4)],5,1)>25
    ht<-which(temp)
    if (length(ht)>0){
      temp2=sapply(ht,function(i) max_consec(x[i:(i+30)]<dd_th) <=7)
      val=which(temp2)
    } else val=NULL
    return_day=ifelse(length(val)!=0 & !is.null(val),ht[val[1]],ifelse(any(is.na(x)),nval,NA))
  }
  return(tdiff=return_day)
}

rainy_season_start_climandes<-function(x,na_handling="strict",nval,dd_th=0.1,...){
  tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-30,length(x)-30)
  if(tend<1){
    return_day=nval
  } else {
    temp= x[1:tend]>=dd_th & consecsum(x[1:(tend+2)],5,1)>8
    ht<-which(temp)
    if (length(ht)>0){
      temp2=sapply(ht,function(i) max_consec(x[i:(i+30)]<dd_th) <=7)
      val=which(temp2)
    } else val=NULL
    return_day=ifelse(length(val)!=0 & !is.null(val),ht[val[1]],ifelse(any(is.na(x)),nval,NA))
  }
  return(tdiff=return_day)
}

rainy_season_start_garcia<-function(x,na_handling="strict",nval,dd_th=0.1,...){
  tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-30,length(x)-30)
  if(tend<1){
    return_day=nval
  } else {
    temp= x[1:tend]>=dd_th & consecsum(x[1:(tend+2)],3,1)>20
    ht<-which(temp)
    if (length(ht)>0){
      temp2=sapply(ht,function(i) max_consec(x[i:(i+30)]<dd_th) <=10)
      val=which(temp2)
    } else val=NULL
    return_day=ifelse(length(val)!=0 & !is.null(val),ht[val[1]],ifelse(any(is.na(x)),nval,NA))
  }
  return(tdiff=return_day)
}

rainy_season_start_gurgiser <- function(x,na_handling="strict",nval,dd_th=0,...){
  tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-30,length(x)-30)
  if(tend<1){
    return_day=nval
  } else {
    temp= x[1:tend]>=dd_th & consecsum(x[1:(tend+6)],7,1)>10 & consecsum(x[1:(tend+30)]>=dd_th,31,1)>10
    val=which(temp)
    return_day=ifelse(length(val)!=0,val[1],ifelse(any(is.na(x)),nval,NA))
  }

  return(tdiff=return_day)
}

rainy_season_start_consec_th <- function(x,na_handling="strict",nval=NA,days,th,dd_th=1,...){
  tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-days+1,length(x)-days+1)
  if(tend<1){
    return_day=nval
  } else {
    temp= x[1:tend]>=dd_th & consecsum(x[1:(tend+(days-1))],days,1)>=th
    val=which(temp)
    return_day=ifelse(length(val)!=0,val[1],ifelse(any(is.na(x)),nval,NA))
  }
  return(tdiff=return_day)
}

rainy_season_start_consec_th_maxcdd <- function(x,na_handling="strict",nval,dd_th=1,days,th,mdays,mcdd,...){
  tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-max(days,mdays)+1,length(x)-max(days,mdays)+1)
  if(tend<1){
    return_day=nval
  } else {
    temp= x[1:tend]>=dd_th & consecsum(x[1:(tend+(days-1))],days,1)>=th
    ht<-which(temp)
    if (length(ht)>0){
      temp2=sapply(ht,function(i) max_consec(x[i:(i+mdays-1)]<dd_th) <=mcdd)
      val=which(temp2)
    } else val=NULL
    return_day=ifelse(length(val)!=0 & !is.null(val),ht[val[1]],ifelse(any(is.na(x)),nval,NA))
  }
  return(tdiff=return_day)
}



rainy_season_end<-function (temp, date_factor, rs_method, nval, dd_th = 1, th = 20,days=NULL,...)
{
  tryCatch({
    stopifnot(is.numeric(temp) && is.character(rs_method) && is.factor(date_factor))
    stopifnot(exists(paste0("rainy_season_end_", rs_method)))
    stopifnot(is.function(get(paste0("rainy_season_end_",
                                     rs_method))))
  }, error = function(cond) {
    message("error in calculating end of rainy season: ")
    message(cond)
    stop_quietly()
  })
  end <- climdex.pcic:::tapply.fast(temp, date_factor, paste0("rainy_season_end_",rs_method), na_handling = "strict", nval = nval, dd_th = dd_th, th = th,days=days)
  return(end)
}


rainy_season_end_gurgiser<-function(x,na_handling="strict",nval,dd_th=1,...){

  tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-45,length(x)-45)
  if(tend<1){
    return_day=nval
  } else {

    temp_e= x[1:tend]<dd_th & consecsum(x[1:(tend+45)],46,1)<10
    val_e=which(temp_e)

    return_day=ifelse(length(val_e)!=0,val_e[1],ifelse(any(is.na(x)),nval,NA))
  }
  return(tend=return_day)
}



rainy_season_end_garcia<-function(x,na_handling="strict",nval,dd_th=0,th=20,...){

  tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-19,length(x)-19)
  if(tend<1){
    return_day=nval
  } else {
    dd=ifelse(dd_th==0,x==0,x<dd_th)
    dd_sum=sapply(1:tend,function(t) sum(dd[t:(t+19)]))
    val_e=which(dd_sum==th)[1]
    return_day=ifelse(length(val_e)!=0,val_e[1],ifelse(any(is.na(x)),nval,NA))
  }
  return(tlength=return_day)
}

rainy_season_end_th<-function(x,na_handling="strict",nval,dd_th=1,days=30,th=16,...){

  tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-days+1,length(x)-days+1)
  if(tend<1){
    return_day=nval
  } else {

    temp_e= x[1:tend]<dd_th & consecsum(x[1:(tend+days-1)],days,1)<th
    val_e=which(temp_e)

    return_day=ifelse(length(val_e)!=0,val_e[1],ifelse(any(is.na(x)),nval,NA))
  }
  return(tend=return_day)
}

rainy_season_end_climandes<-function(x,na_handling="strict",nval,dd_th=1,...){

  tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-29,length(x)-29)
  if(tend<1){
    return_day=nval
  } else {

    temp_e= x[1:tend]<dd_th & consecsum(x[1:(tend+29)],30,1)<16
    val_e=which(temp_e)

    return_day=ifelse(length(val_e)!=0,val_e[1],ifelse(any(is.na(x)),nval,NA))
  }
  return(tend=return_day)
}



max_consec<-function(d,NA_val=0) {
  if(class(d) != "logical") stop("input for max_consec needs to be of class logical")
  d[which(d==FALSE)]=NA
  if(all(is.na(d))){
    r<-NA_val
  } else r<-max((d) * unlist(lapply(rle(d)$lengths, seq_len)),na.rm=TRUE)
  return(r)
}

consecsum<-function(data,n,i=1){
  cx <- c(0,cumsum(data))
  rsum <- cx[(n+i):length(cx)] - cx[i:(length(cx) - n)]
  return(rsum)
}


