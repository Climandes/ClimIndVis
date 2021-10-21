

#puts default arguments("argdefaults") for args in "argnames" in "arglist" if arguments are not provided
put_default_args<-function(argnames,argdefaults,arglist){
  for (a in argnames) if(is.null(arglist[[a]])) arglist[[a]]<-argdefaults[[which(argnames==a)]]
  return(arglist)
}



##################helpers for calculate_index.climindvis##################################
####time factors####
# gets factors to cut out wanted time slices. "annual","monthly","seasonal" or array with consecutive months
get_date_factors<-function(climindvis,agg,aggmons=NULL,selagg=NULL,start_days=NA,end_days=NA,start=NULL,end=NULL,xdays=NULL){
  if(agg=="other") if(is.null(aggmons)) stop ("if <<aggt>> is other, <<aggmons>> need to be specified")
  if(agg=="dates") if(is.null(start_days) | is.null(end_days)) stop ("if <<aggt>> is dates, <<start_days>> and <<end_days>> need to be specified")
  if (agg=="xdays") if (is.null(xdays)) stop("if <<aggt>> is xdays,  <<xdays>> needs to be specified")
  years<-levels(climindvis$time_factors$years)
  if (agg == "annual") {
    r<-factor(climindvis$time_factor$years)
    r<-check_agg_complete(r,agg)
    aggnames="annual"
    jdays <- climindvis$time_factors$jdays
  } else if (agg=="monthly" & is.null(selagg)){
    r<-factor(climindvis$time_factor$yearmons)
    r<-check_agg_complete(r,agg)
    aggnames=month.abb[as.integer(unique(substr(levels(r),6,7)))]
    jdays <- climindvis$time_factors$jdays
  } else if (agg=="monthly" & !is.null(selagg)){
    r<-factor(select_months_factors(climindvis$time_factors$yearmons,levels(climindvis$time_factors$years),selagg))
    r<-check_agg_complete(r,agg)
    aggnames=month.abb[as.integer(unique(substr(levels(r),6,7)))]
    jdays <- select_jdays(climindvis, r)
  } else if (agg == "seasonal") {
    r<-factor(select_seas_factors(climindvis$time_factors$yearmons,levels(climindvis$time_factors$years),selagg))
    r<-check_agg_complete(r,agg)
    aggnames=unique(substr(levels(r),6,8))
    jdays <- select_jdays(climindvis, r)
  } else if (agg == "other") {
    r<-factor(select_date_factors_monthly(climindvis$time_factors$yearmons,levels(climindvis$time_factors$years),aggmons))
    if(all(is.element(diff(aggmons),c(1,-11))) ) {
      aggnames=month_abb(aggmons)
    } else aggnames=paste0("mons",paste(aggmons,collapse="-"))
    jdays <- select_jdays(climindvis, r)
  } else if (agg == "dates"){
    r<-factor(select_date_factors(climindvis$time,start_days,end_days))
    if (as.integer(format(as.Date(start_days),"%Y"))[1] !=0){
      aggnames="user_dates"
    } else  aggnames=paste0(gsub("-","",substring(start_days,6,10)),"-",gsub("-","",substring(end_days,6,10)))
    jdays <- select_jdays(climindvis, r)
  } else if (agg == "xdays"){
    r<-factor(select_xday_factors(climindvis$time,start,end,xdays))
    jdays <- select_jdays(climindvis, r)
    aggnames=paste0(gsub("-","",levels(r)),"+",xdays)

  }
  names(r) = climindvis$time_factors$years
  if (all(is.na(r))) stop("selected time period is not available in data")
   switch(is_index_special(class(climindvis)[1])+1 ,return(list(tfactor=r,aggnames=aggnames)),
    return(list(tfactor=r,aggnames=aggnames, jdays=jdays)))
}


select_xday_factors<- function(time,start,end,xdays){
  if (!is.null(start)) if (class(start)!= "Date") start=as.Date(start)
  if (!is.null(end)) if (class(end)!= "Date") start=as.Date(end)
  if (is.element(start,time)) {
    sel=ifelse(is.null(start),1,which(time==start))
    if (!is.null(end)) sel2=which(time==end)
  } else stop("start is not an element of time. Please check time steps or change start")
  nt=length(time)
  helpd=array(NA,nt)
  count=switch(is.null(end)+1,seq(sel,nt,xdays),seq(sel,sel2,xdays))
  for (cc in 1:(length(count)-1)){
    helpd[count[cc]:(count[cc+1]-1)]=paste0(time[count[cc]])
  }
  return(helpd)
}




# function to select jdays based on selected period
select_jdays <- function(climindvis, r){

  jd <- climindvis$time_factors$jdays
  jd[is.na(r)] <- NA
  return(jd)
}

#help functions for get_date_factors
select_date_factors_monthly<-function(yearmons,years,aggmons){
  mons<-as.integer(unique(sapply(strsplit(levels(yearmons),split="-"),"[",2)))
  #if(length(mons<12) & tail(mons,1)< head(mons,1)) years = years[1:(length(years)-1)]
  if(all(is.element(aggmons,mons))){
    addy<-ifelse(tail(aggmons,1)>=aggmons[1],0,1)
    beg_dates<-sapply(years, function(x) {
      if (is.element(paste0(x,"-",pad2(aggmons[1])),yearmons)){
        which(as.character(yearmons)==paste0(x,"-",pad2(aggmons[1])))[1]
      } else NA})
    end_dates<-sapply(years, function(x) {
      if (is.element(paste0(as.integer(x)+addy,"-",pad2(tail(aggmons,1))),yearmons)) {
        tail(which(as.character(yearmons)==paste0(as.integer(x)+addy,"-",pad2(tail(aggmons,1)))),1)
      } else NA})
    if(addy==1){
      end_dates[is.na(beg_dates)]=NA
      beg_dates[is.na(end_dates)]=NA
    }
    help<-array(NA,length(yearmons))
    for(i in 1:length(beg_dates)) if(!is.na(beg_dates[i])) help[beg_dates[i]:end_dates[i]]=substring(yearmons[beg_dates[i]],1,4)
  } else stop("data does not contain (all) months to be selected, please choose different time slice")
  return(help)
}

#selects time factors for dates, either same day and month for every year, where date needs to be of format
# "0000-mm-dd, e.g.
#start_days=as.Date("0000-06-01")
#end_days=as.Date("0000-06-05")
# or array of dates of same length as years, e.g. for 3 years:
# start_days=as.Date(c("1981-01-06","1982-02-07","1983-05-02"))
# end_days=as.Date(c("1981-06-06","1982-03-07","1983-04-02"))
select_date_factors<-function(times,start_days,end_days){
  if (is.na(start_days) || is.na(end_days)) stop("<<start_days>> and <<end_days>> need to be defined.")
  years=as.integer(unique(format(times,"%Y")))
  if (class(start_days)!= "Date") start_days=as.Date(start_days)
  if (class(end_days)!= "Date") end_days=as.Date(end_days)
  fs=rawToChar(unique(charToRaw(format(start_days,"%Y")[1])))
  fe=rawToChar(unique(charToRaw(format(end_days,"%Y")[1])))
  if(length(start_days)==1 & length (end_days)==1 & (nchar(fs) !=4 & !is.na(as.numeric(fs)) & nchar(fe) !=4 & !is.na(as.numeric(fe)))){

    if ((fs !="0" | fe !="0" ) & length(years!=1)) stop("start and end string have to be of type 0000-mm-dd, eg 0000-08-01")
    addy<-ifelse(end_days-start_days <0,1,0)
    beg_dates<-sapply(years, function(x) {
      bd=paste0(x,"-",format(start_days,"%m-%d"))
      if (is.element(as.Date(bd),times) & as.Date(bd)-head(times,1)>=0 & as.Date(bd)-tail(times,1)<=0  ) {
        return(which(times == bd))
        } else return(NA)})
    end_dates<-sapply(years, function(x) {
      ed=paste0(x+addy,"-",format(end_days,"%m-%d"))
      if (is.element(as.Date(ed),times) & as.Date(ed)-head(times,1)>=0 & as.Date(ed)-tail(times,1)<=0 ) {
        return(which(times == ed))
    }else return(NA)})


      end_dates[is.na(beg_dates)]=NA
      beg_dates[is.na(end_dates)]=NA
    #}
      if(sum(is.na(end_dates) > 2)) stop("dates are not available for all years, please check input data.")
    vals=years

  } else {
    if (length(start_days)!=length(end_days)) stop ("length of start_days and end_days differs")
    years_match<-is.element(as.integer(unique(format(start_days,"%Y"))),years) & is.element(as.integer(unique(format(end_days,"%Y"))),years)
    if (any(years_match)==FALSE) stop("climindvis object does not contain any of the years to be selected")
    if (all(years_match)==FALSE) message("Warning: Dates to be selected and years in climindvis object do not match. Only matching years are used ")

    beg_dates=which(is.element(times,start_days[years_match]))
    end_dates=which(is.element(times,end_days[years_match]))
    vals=format(start_days,"%Y")
  }
  helpd<-array(NA,length(times))
  for(i in 1:length(beg_dates)) {
    if(!is.na(beg_dates[i]) & !is.na(end_dates[i])){
      # helpd[beg_dates[i]:end_dates[i]]=vals[i]
      helpd[beg_dates[i]:end_dates[i]]=vals[i]
    } }

return(helpd)
}

select_seas_factors<-function(yearmons,years,selagg=NULL){
  beg=c(3,6,9,12)
  end=c(5,8,11,2)
  seasnames=c("MAM","JJA","SON","DJF")
  help<-array(NA,length(yearmons))
  mons<-as.integer(unique(sapply(strsplit(levels(yearmons),split="-"),"[",2)))
  c=0
  if(!is.null(selagg)){
    loop= which(is.element(seasnames,selagg))
    } else loop=1:4
  for (i in loop){
    if(all(is.element(monlist(beg[i],end[i]),mons))){
      c=c+1
      addy<-ifelse(beg[i]<end[i],0,1)
      beg_dates<-sapply(years, function(x) which(as.character(yearmons)==paste0(x,"-",pad2(beg[i])))[1])
      end_dates<-sapply(years, function(x) { h<-which(as.character(yearmons)==paste0(as.integer(x)+addy,"-",pad2(end[i])))
                                             ifelse (length(h)>0,tail(h,1),NA)})
      if(addy==1){
        end_dates<-c(unlist(end_dates),NA)
        beg_dates[is.na(end_dates)]=NA
      }
      for(k in 1:length(beg_dates)) if(!is.na(beg_dates[k]) & !is.na(end_dates[k])) help[beg_dates[k]:end_dates[k]]=paste0(years[k],"-",seasnames[i])

    }
  }
  if (c==0) stop("data does not include any selected season")
  return(factor(help))
}

select_months_factors<-function(yearmons,years,aggmons){
    help<-yearmons
    help[!is.element(sapply(strsplit(as.character(yearmons),split="-"),"[",2),pad2(aggmons))]=NA
    return(help)
}


#### quantile functions ####
get_quants <- function(climindvis,var,qargs,sel_time){

  ifelse(grepl("grid",climindvis$data_info$type),gd<-c(1,2),gd<-1)
  switch(grepl("fc|hc",climindvis$data_info$type)+1,ed<-NULL,ed<-tail(gd,1)+1)


  agg_factor <- as.factor(gsub(".*-","",sel_time$tfactor))
  aggd=length(sel_time$aggnames)>1

  if (is.null(qargs$th_quantiles)){
    ind <- as.numeric(gsub("-.*","",sel_time$tfactor))
    agg_factor[ind<qargs$baseperiod[1] | ind > qargs$baseperiod[2]]=NA
    agg_factor_h<-agg_factor

    if(!is.null(ed)) {
      edim=dim(climindvis$data[[var]])[ed]
      if(qargs$qens_all){
        agg_factor_h=rep(agg_factor,each=edim)
      } else gd=c(gd,ed)
    }

    quants=apply(climindvis$data[[var]],gd,function(d){
      if(!is.null(ed) & qargs$qens_all) d<-array(d)
      if (sel_time$aggnames[1]!="annual"){
        if(!all(grepl("-", sel_time$tfactor)) & length(sel_time$aggnames)==1) {
          levels(agg_factor_h)<- rep(sel_time$aggnames,length(levels(agg_factor_h)))
        }
        q_thresh<- climdex.pcic:::tapply.fast(d, agg_factor_h,  function(x){
          xl=length(x)
          if (any(is.na(x))) {
            x=x[-(which(is.na(x)))]
          }
          if (length(x)/xl*100<(100-qargs$NAmaxbasep)){
            thresh=NA
          } else thresh <- climdex.pcic::climdex.quantile(x,q = qargs$qth)
          return(thresh)})

      } else{
        dl=length(d)
        if (any(is.na(d))) {
          d=d[-(which(is.na(d)))]
        }
        if (length(d)/dl*100<(100-qargs$NAmaxbasep)){
          q_thresh=NA
        } else q_thresh=climdex.pcic::climdex.quantile(d,q = qargs$qth)

      }
      return(q_thresh)
    })

    if(!is.null(ed) & qargs$qens_all)  quants=replicate(edim,quants)
    ifelse(is.null(dim(quants)),dq<-length(quants),dq<-dim(quants))
    if(!aggd) quants<-array(quants,dim=c(1,dq))
    dimnames(quants)=c(list(sel_time$aggnames),rep(list(NULL),ndims(quants)-1))

  } else {
    if(!is.null(ed)) edim=dim(climindvis$data[[var]])[ed]
    quants=qargs$th_quantiles
    if (dim(quants)[ed+1]!=edim) {
      if(qargs$qens_all){
        if (length(apply(quants, gd+1,unique))!= prod(dim(quants)[gd+1])){
          stop("if <<qens_all>>==TRUE thresholds provided in th_object need to be the same for all ensemble members for one grid point/station")
        }
        rep(index_array(quants,1,gd+1),edim)
        message("Warning when calculating quantiles: ")
        message("number of ensemble members in th_object and climindvis object are not the same. Same thresholds used for all members")
      } else {
        stop(" number of ensemble members in th_object and climindvis object are not the same, if <<qens_all>>==FALSE, thresholds are calculated individually for each member and thresholds in th_object need to have the same ensemble dimension as the climindvis object.")
      }

    }
  }


  # if(any(is.na(quants))) warning(paste0("Quantiles for ",paste0(names(aggThresh[is.na(aggThresh)]), collapse = " "),
  #   " can not be calculated. Check number of  missing values or increase baseperiod. "))

  d=dim(climindvis$data[[var]])
  ld=length(d)

  qh<-array(quants,dim=c(dim(quants)[1],prod(d[1:(ld-1)])))
  quants_help<-array(NA,dim=c(prod(d[1:(ld-1)]),d[ld]))
  if (aggd){
    for (i in 1:dim(quants)[1]){
      facti<-which(agg_factor==levels(agg_factor)[i] & !is.na(agg_factor))
      quants_help[,facti] = replicate(length(facti),qh[i,])
    }
  } else {
    facti=which(!is.na(agg_factor))
    quants_help[,facti] = replicate(length(facti),qh[1,])
  }

  return(list(quants=quants,quants_help=quants_help))

}

rearange_time<-function(data,data_levels){
  dd=dim(data)
  data_out=aperm(data,c(2:length(dd),1))
  dimnames(data_out) <-list(NULL,data_levels)
  return(data_out)
}

####other helpers for calculate_index.climindvis####
rearange_by_year<-function(data,data_levels){
  rn<-data_levels
  yy<-as.factor(sapply(strsplit(rn,split="-"),"[",1))
  yyn<-levels(yy)
  tt<-as.factor(sapply(strsplit(rn,split="-"),"[",2))
  ttn<-levels(tt)
  if (length(ttn)==0){
    if(length(rn)>1){
      data_out<-array(NA,dim=c(dim(data)[-1],length(yyn)))
      for (y in yyn) if (is.element(y,rn)) data_out[,y==yyn]<-data[y,]
    } else {
      data_out<-array(data,dim=c(length(data),length(yyn)))
    }
    dimnames(data_out) <-list(NULL,yyn)
  } else if (length(ttn)>0) {
    if(length(rn)>1){
      data_out<-array(NA,dim=c(dim(data)[-1],length(ttn),length(yyn)))
      for (y in yyn) for (t in ttn) if (is.element(paste0(y,"-",t),rn)) data_out[,t==ttn,y==yyn]<-data[paste0(y,"-",t),]
    } else {
      data_out<-array(data,dim=c(length(data),length(ttn),length(yyn)))
    }
    dimnames(data_out) <-list(NULL,ttn,yyn)
  }
  return(data_out)
}


get_idims<-function(climindvis,args){
  if(length(grep("p",climindvis$data_info$type))>0){
    idims="p"
  } else if (length(grep("grid",climindvis$data_info$type))>0){
    idims=c("lon","lat")
  }
  if (length(grep("hc|fc",climindvis$data_info$type))>0){
    idims=c(idims,"ens")
  }
  if (is.element(args$aggt,c("seasonal","monthly"))){
    idims=c(idims,"agg")
  }
  if (args$aggt=="xdays"){
    idims=c(idims,paste0("dates"))
  } else idims=c(idims,"year")
  return(idims)
}


check_agg_complete<-function(r,agg){
  aa=levels(r)

  cut=sapply(aa, function(a){
    if (agg=="annual" ){
      la = ifelse(is.leapyear(as.integer(a)),366,365)
    } else if (agg =="monthly") {
      ay = as.integer(substring(a,1,4))
      am = as.integer(substring(a,6,7))
      la = mon_length(am,ay)
    } else if (agg == "seasonal") {
      ay=as.integer(substring(a,1,4))
      as = substring(a,6,8)
      la = seas_length(as,ay+1)
    } else if (agg == "other") {
      ay=as.integer(substring(a,1,4))
      la = other_length(aggmons,ay)
    }
    if(!length(which(r == a))==la) return(a)
  } )

  cut=unlist(cut[!sapply(cut,is.null)])

  if(length(cut)!=0){
    if (agg == "annual" & agg == "other"){
      if (!(is.element(as.integer(cut),range(as.integer(aa))))& length(aa)>2){
        message("input data does not cover whole time span of aggt, please change aggt or check for missing dates in input data.")
        stop_quietly()
      }
    } else if (agg == "monthly"){
      years=unique(as.integer(substring(cut,1,4)))
      if(length(years)>2){
      chelp=cut
      cut=NULL
      cm=unique(substring(chelp,6,7))
      for (i in cm){
        if (!all(is.element(years[substring(chelp,6,7)==i],range(years)))){
          cut=c(cut,paste0(years,"-",i))
          message(paste0("warning: data for month", i, "is not complete. Index is not calculated for this month."))
        }
      }
      }
    } else if (agg == "seasonal"){
      years=unique(as.integer(substring(aa,1,4)))
      if(length(years)>2){
      chelp=cut
      cut=NULL
      cs=unique(substring(chelp,6,8))

      for (i in cs){
        if (!(all(is.element(years[substring(chelp,6,8)==i],range(years))) & length(years)>2)){
          cut=c(cut,paste0(years,"-",i))
          message(paste0("warning: data for season", i, "is not complete. Index is not calculated for this season."))
        }
      }
      }
  }
  r<-plyr::mapvalues(r,from=cut, to=rep(NA,length(cut)))

  if (all(is.na(r))) {
    message("input data does not cover time span of aggt, please change aggt")
    stop_quietly()
  }
  }
  return(r)
}

#testet ob Index Quantile benutzt
is_index_special<-function(index){
  r<-ifelse (is.element(index,c("qth","qtot","rXptot","tn10p","tx10p","tn90p","tx90p","csdi","wsdi")),TRUE,FALSE )
  return(r)
}


# get_numdays <- function(sel_time, climindvis,iargs){
#   len_factors <- tapply(sel_time$tfactor,sel_time$tfactor, length)
#   ## acount for na values
#
#   nna <- climdex.pcic:::tapply.fast(sel_time$tfactor)
#
#
#   return(numdays)
# }


