

rainy_season_start<-function (temp, date_factor, rs_method,nval,dd_th=1,days=NA,th=NA,mdays=NA,mcdd=NA){
  tryCatch({
  stopifnot(is.numeric(temp) && is.character(rs_method) && is.factor(date_factor))
  stopifnot(exists(paste0("rainy_season_start_",rs_method)))
  stopifnot(is.function(get(paste0("rainy_season_start_",rs_method))))
  }, error = function(cond){
  message("error in calculating start of rainy season: ")
  message(cond)
  stop_quietly()
  })
  start<-climdex.pcic:::tapply.fast(temp,date_factor,paste0("rainy_season_start_",rs_method),na_handling="strict",nval=nval,days=days,th=th,dd_th=dd_th,mdays=mdays,mcdd=mcdd)
  return(start)
}

rainy_season_start_stern<-function(x,na_handling="strict",nval,dd_th=0.1,...){
      tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-30,length(x)-30)
      if(tend<1){
        return_day=nval
      } else {
        temp= x[1:tend]>dd_th & consecsum(x[1:(tend+4)]>dd_th,4,2)>1 & consecsum(x[1:(tend+4)],5,1)>25
        ht<-which(temp)
        if (length(ht>1)){
        temp2=sapply(ht,function(i) max_consec(x[i:(i+30)]<=dd_th) <=7)
        val=which(temp2)[1]
        } else val=NULL
        return_day=ifelse(length(val)==1,ht[val],NA)
      }
  return(tdiff=return_day)
}

rainy_season_start_garcia<-function(x,na_handling="strict",nval,dd_th=0.1,...){
  tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-30,length(x)-30)
  if(tend<1){
    return_day=nval
  } else {
    temp= x[1:tend]>dd_th & consecsum(x[1:(tend+2)],3,1)>20
    ht<-which(temp)
    if (length(ht>1)){
      temp2=sapply(ht,function(i) max_consec(x[i:(i+30)]<=dd_th) <=10)
      val=which(temp2)[1]
    } else val=NULL
    return_day=ifelse(length(val)==1,ht[val],NA)
  }
  return(tdiff=return_day)
}

rainy_season_start_gurgiser <- function(x,na_handling="strict",nval,dd_th=1,...){
  tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-30,length(x)-30)
  if(tend<1){
    return_day=nval
  } else {
    temp= x[1:tend]>dd_th & consecsum(x[1:(tend+6)],7,1)>10 & consecsum(x[1:(tend+30)]>dd_th,31,1)>10
    val=which(temp)[1]
    return_day=ifelse(length(val)==1,val,NA)
  }

  return(tdiff=return_day)
}

rainy_season_start_consec_th <- function(x,na_handling="strict",nval=NA,days,th,dd_th=1,...){
  tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-days+1,length(x)-days+1)
  if(tend<1){
    return_day=nval
  } else {
    temp= x[1:tend]>dd_th & consecsum(x[1:(tend+(days-1))],days,1)>=th
    val=which(temp)[1]
    return_day=ifelse(length(val)==1,val,NA)
  }
  return(tdiff=return_day)
}

rainy_season_start_consec_th_maxcdd <- function(x,na_handling="strict",nval,dd_th=1,days,th,mdays,mcdd,...){
  tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-max(days,mdays)+1,length(x)-max(days,mdays)+1)
  if(tend<1){
    return_day=nval
  } else {
    temp= x[1:tend]>dd_th & consecsum(x[1:(tend+(days-1))],days,1)>=th
    ht<-which(temp)
    if (length(ht>1)){
      temp2=sapply(ht,function(i) max_consec(x[i:(i+mdays-1)]<=dd_th) <=mcdd)
      val=which(temp2)[1]
    } else val=NULL
    return_day=ifelse(length(val)==1,ht[val],NA)
  }
  return(tdiff=return_day)
}



# rainy_season_end<-function (temp, date_factor, rs_method,nval,dd_th=1,days=NA,th=NA,mdays=NA,mcdd=NA){
#   tryCatch({
#   stopifnot(is.numeric(temp) && is.character(rs_method) && is.factor(date_factor))
#   stopifnot(exists(paste0("rainy_season_end_",rs_method)))
#   stopifnot(is.function(get(paste0("rainy_season_end_",rs_method))))
#   }, error = function(cond){
#     message("error in calculating end of rainy season: ")
#     message(cond)
#     stop_quietly()
#   })
#   end<-climdex.pcic:::tapply.fast(temp,date_factor,paste0("rainy_season_end_",rs_method),na_handling="strict",nval=nval,dd_th=dd_th,days=days,th=th,mdays=mdays,mcdd=mcdd)
#   return(end)
# }
#
# rainy_season_end_gurgiser<-function(x,na_handling="strict",nval,dd_th=1,...){
#
#   tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-45,length(x)-45)
#   if(tend<1){
#     return_day=nval
#   } else {
#     temp= x[1:tend]<=dd_th & consecsum(x[1:(tend+45)],46,1)<10
#     val=which(temp)[1]
#     return_day=ifelse(length(val)==1,val,NA)
#   }
#   return(tdiff=return_day)
# }
#
# rainy_season_length<-function (temp, date_factor, rs_method,nval,dd_th=1,days=NA,th=NA,mdays=NA,mcdd=NA){
#   tryCatch({
#     stopifnot(is.numeric(temp) && is.character(rs_method) && is.factor(date_factor))
#     stopifnot(exists(paste0("rainy_season_length_",rs_method)))
#     stopifnot(is.function(get(paste0("rainy_season_length_",rs_method))))
#   }, error = function(cond){
#     message("error in calculating end of rainy season: ")
#     message(cond)
#     stop_quietly()
#   })
#   length<-climdex.pcic:::tapply.fast(temp,date_factor,paste0("rainy_season_length_",rs_method),na_handling="strict",nval=nval,dd_th=dd_th,days=days,th=th,mdays=mdays,mcdd=mcdd)
#   return(length)
# }
#
#
# rainy_season_length_gurgiser<-function(x,na_handling="strict",nval,dd_th=1,...){
#
#   tend=ifelse(any(is.na(x)) & na_handling=="strict",which(is.na(x))[1]-45,length(x)-45)
#   if(tend<1){
#     return_day=nval
#   } else {
#     temp_s= x[1:tend]>dd_th & consecsum(x[1:(tend+6)],7,1)>10 & consecsum(x[1:(tend+30)]>dd_th,31,1)>10
#     val_s=which(temp_s)[1]
#
#     temp_e= x[1:tend]<=dd_th & consecsum(x[1:(tend+45)],46,1)<10
#     val_e=which(temp_e)[1]
#
#     return_day=ifelse(length(val_s)==1 & length(val_e)==1,val_e-val_s,NA)
#   }
#   return(tlength=return_day)
# }


#for logical vec
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


