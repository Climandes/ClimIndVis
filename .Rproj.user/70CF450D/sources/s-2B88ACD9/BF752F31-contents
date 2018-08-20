#shortcut function for str_pad(x,2,pad="0")
pad2<- function(x){
  stringr::str_pad(x,2,pad="0")
}

#make array of months from start month to end month, e.g. 11:2 -> c(11,12,1,2)
monlist <- function(start_mon,end_mon){
  help=c(1:12,1:12)
  a<-which(help==start_mon)[1]
  b<-which(help == end_mon)
  x<-help[a:b[which(b>a)[1]]]
  return(x)
}

#make array of months from start month to start month+ nmon, e.g. 11,3 -> c(11,12,1,2)
monlist2 <- function(start_mon,nmon){
  help=c(1:12,1:12)
  a<-which(help==start_mon)[1]
  b<-a+nmon
  x<-help[a:b]
  return(x)
}

#function to get first day of months from start month and number of months (for plotting ticks)
monlength2<- function(smon,nmon,msum=FALSE){
  help<-c(1:12,1:12)
  x<-help[smon:(smon+nmon-1)]
  ml<-c(31,28,31,30,31,30,31,31,30,31,30,31)
  if (msum==TRUE){
    help2<-c(1,ml[x])
    names(help2)<-month.abb[help[smon:(smon+nmon)]]
    return (cumsum(help2))
  } else return(ml[x])

}

other_length<-function(mons,year){

  lp=is.leapyear(year)
  if(is.element(2,mons)){
    if(!all(diff(mons)==1) & which(diff(mons)!=1) < which(mons==2)){
      lp=is.leapyear(year+1)
    }
  }
  ml<-c(31,ifelse(lp,29,28),31,30,31,30,31,31,30,31,30,31)
  return(sum(ml[mons]))
}



mon_length<-function(mon,year){
  lp=is.leapyear(year)
  ml<-c(31,ifelse(lp,29,28),31,30,31,30,31,31,30,31,30,31)
  return(ml[mon])
}


seas_length<-function(seas,year){
  lp=is.leapyear(year)
  sl<-c(MAM=92,JJA=92,SON=91,DJF=ifelse(lp,91,90))
  return(sl[seas])
}

is.leapyear=function(year){
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}


month_abb<-function(months){
mn=c("J","F","M","A","M","J","J","A","S","O","N","D")
mi=c(1:12)
return(paste0(mn[months],collapse=""))

}


#function to get lenght of months in array x
monlength<- function(x,msum=FALSE){
  ml<-c(31,28,31,30,31,30,31,31,30,31,30,31)
  names(ml)=month.abb[1:12]
  if (msum==TRUE){
    return (cumsum(ml[x]))
  } else return(ml[x])

}


#from https://stackoverflow.com/questions/14500707/select-along-one-of-n-dimensions-in-array
index_array <- function(x, dim, value=NULL, drop = FALSE) {
 indices <- rep(list(bquote()), length(dim(x)))
 if(is.null(value)){
   value=lapply(dim, function(dd) 1:dim(x)[dd])
 }
  if (length(dim)>1){
    if(!is.list(value)) stop("for multiple dimensions, value has to be of type list")
    value=value[!sapply(value,is.null)]
    #value[sapply(value,is.null)] = lapply(dim[sapply(value,is.null)],function(dd) 1:dim(x)[dd])
    if(length(dim)!=length(value)) stop("length of <<dim>> and <<value>> do not match")
    for (dd in 1:length(dim)){
      indices[[dim[dd]]]<-value[[dd]]
    }
  } else indices[[dim]] <- unlist(value)

   call <- as.call(c(
    list(as.name("["), quote(x)),
    indices,
    list(drop = drop)))
  eval(call)
}

## adaption von tapply.fast fuer aggregierete quantil berechnung
mapply_fast <- function (X,Y, INDEX, FUN = NULL,z ,  simplify = TRUE)
{
  FUN <- if (!is.null(FUN))
    match.fun(FUN)
  if (!is.factor(INDEX))
    stop("INDEX must be a factor.")
  if (length(INDEX) != length(X))
    stop("arguments must have same length")
  if (is.null(FUN))
    return(INDEX)
  namelist <- levels(INDEX)
  ans <- mapply(FUN,split(X, INDEX),split(Y, INDEX), z)
  ans <- unlist(ans, recursive = FALSE)
  names(ans) <- levels(INDEX)
  return(ans)
}


replace_operator_name<-function(string){
 op1<-c(">",">=","<","<="," ")
 op2<-c("gt","ge","lt","le","_")

 for (i in seq_along(op1)){
   string=gsub(op1[i],op2[i],string)
 }
 return(string)
 }


mround <- function(x,base){base*round(x/base)}

ndims<-function(x){
  out<-length(dim(x))
  if (out==0) out=1
  return(out)
}


convert_deg<-function(string){

  if (string=="degreeC"){
    r=expression(paste(degree,"C"))
  } else r=string
   return(r)
}

stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

is_date <- function(x) inherits(x, 'Date')


