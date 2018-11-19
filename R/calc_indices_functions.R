
####################### index functions ##################################################################
aggregate_var<-function (temp, date_factor,NAmaxAgg=20,aggfun)
{
  stopifnot(is.numeric(temp) && is.factor(date_factor))
  agg<-suppressWarnings(climdex.pcic:::tapply.fast(temp, date_factor,
    aggfun, na.rm = TRUE))
  agg[is.infinite(agg)]<-NA
  nna<-climdex.pcic:::tapply.fast(is.na(temp)==FALSE, date_factor,
    sum, na.rm = TRUE)
  length<-climdex.pcic:::tapply.fast(temp, date_factor,
    function(x) length(x))
  agg[nna/length*100<(100-NAmaxAgg)]=NA
  return(agg)
}


#calculate number of days op(">","<","<=",">=") a threshold using functions from climdex.pcic
ndays_op_threshold<-function (temp,jdays, q_temp=NULL,q_temp_inbase=NULL,date_factor, threshold=NULL, op = "<",iformat="perc",NAmaxAgg=20,...)
{
  opargs <- list(...)
  stopifnot(is.numeric(temp) && is.factor(date_factor) && ( ifelse(!is.null(q_temp),is.numeric(q_temp) | all(is.na(q_temp)),TRUE)| is.numeric(threshold)))


  if(!is.null(q_temp)){
    if (missing(jdays)) stop("error in index function: argument jdays is missing")
    jdays <- as.numeric(jdays)
    f <- match.fun(op)
    dat <- f(temp,q_temp[jdays])
    if(!is.null(q_temp_inbase)){

      df_year <- as.numeric(substr(date_factor,1,4))
      df_year2 <- as.numeric(substr(names(date_factor),1,4))
      inset <- df_year >= opargs$baseperiod[1] & df_year <= opargs$baseperiod[2]
      inset[is.na(inset)] <- FALSE
      jdays_base <- jdays[inset]
      temp_base <- temp[inset]
      years_br <- range(df_year2[inset], na.rm=TRUE)
      years_base <- df_year2[inset]
      byrs <- (years_br[2] - years_br[1] + 1)

      ## acount for missing values in baseperiod??

      bdim <- dim(q_temp_inbase)
      yday_byr <- jdays_base + (years_base - years_br[1]) * 365
      fresult <- match.fun(op)(rep(temp_base, byrs - 1), q_temp_inbase[yday_byr,  ])
      dat[inset] <- rowSums(fresult, na.rm = TRUE)/(byrs - 1)
      days<- climdex.pcic:::tapply.fast(dat, date_factor,sum, na.rm=TRUE)

    } else {
      days<- climdex.pcic:::tapply.fast(dat, date_factor,sum, na.rm=TRUE)
      }

  } else {
    days<-climdex.pcic:::tapply.fast(match.fun(op)(temp, threshold), date_factor,
      sum, na.rm = TRUE)
  }
  nna<-climdex.pcic:::tapply.fast(is.na(temp)==FALSE, date_factor,
    sum, na.rm = TRUE)
  length<-climdex.pcic:::tapply.fast(temp, date_factor,
    function(x) length(x))
  days[nna/length*100<(100-NAmaxAgg)]=NA
  if(iformat=="perc"){
    nna<-climdex.pcic:::tapply.fast(is.na(temp)==FALSE, date_factor,
      sum, na.rm = TRUE)
    out<-round(days/nna*100,digits=1)
    out[which(nna==0)]=NA
    return(out)
  } else
    return(days)
}

#calculate sum of of days op a threshold
total_precip_op_threshold <- function(temp, q_temp=NULL,date_factor, threshold=NULL, op= ">", NAmaxAgg=20,...){
  stopifnot(is.numeric(temp) && is.factor(date_factor) && (ifelse(!is.null(q_temp),is.numeric(q_temp) | all(is.na(q_temp)),TRUE) | is.numeric(threshold)))

  if(!is.null(q_temp)){
    ind <- get_ind(date_factor, q_temp)
    mm <- mapply_fast(temp,q_temp[ind], date_factor, function(x,y,z){
      if(all(is.na(y))) {NA}
      else {
        sum(x[match.fun(op)(x,y)], na.rm=TRUE)
        }
    }, z=op)

  } else {
    mm <- climdex.pcic:::tapply.fast(temp[match.fun(op)(temp, threshold)],date_factor[match.fun(op)(temp, threshold)],
      sum,na.rm=TRUE)}

  nna<-climdex.pcic:::tapply.fast(is.na(temp)==FALSE, date_factor,
    sum, na.rm = TRUE)
  length<-climdex.pcic:::tapply.fast(temp, date_factor,
    function(x) length(x))
  mm[nna/length*100<(100-NAmaxAgg)]=NA

  return(mm)

}


get_ind <- function(date_factor, q_temp){

  ind <- gsub(".*-","",date_factor)
  nind <- nchar(ind[!is.na(ind)])
  if(unique(nind)==4) {
    ind <- rep(1, length(date_factor))
  }
  return(ind)
}

#calculate daily mean precipitation op a threshold
dailymean_precip_op_threshold <- function(temp, date_factor, threshold=NULL, op= ">", NAmaxAgg=20) {

  stopifnot(is.numeric(temp) && is.numeric(threshold) && is.factor(date_factor))

  mm_int <- climdex.pcic:::tapply.fast(temp[match.fun(op)(temp, threshold)],
                                       date_factor[match.fun(op)(temp, threshold)],
    function(x) {return(sum(x)/sum(!is.na(x)))})
  nna<-climdex.pcic:::tapply.fast(is.na(temp)==FALSE, date_factor,
    sum, na.rm = TRUE)
  length<-climdex.pcic:::tapply.fast(temp, date_factor,
    function(x) length(x))
  mm_int[nna/length*100<(100-NAmaxAgg)]=NA
  return(mm_int)
}

#simple daily intensity
simple_daily_intensity <- function(temp, date_factor, threshold=NULL, op =">", NAmaxAgg=20){

  stopifnot(is.numeric(temp) && is.numeric(threshold) && is.factor(date_factor))

  mm <- climdex.pcic:::tapply.fast(temp, date_factor, function(x) {
    idx <- x >= 1 & !is.na(x)
    if (sum(idx) == 0) {
      return(0)
    } else {
      return(sum(x[idx], na.rm = TRUE)/sum(idx))
    }
  })
  nna<-climdex.pcic:::tapply.fast(is.na(temp)==FALSE, date_factor,
                                  sum, na.rm = TRUE)
  length<-climdex.pcic:::tapply.fast(temp, date_factor,
                                     function(x) length(x))
  mm[nna/length*100<(100-NAmaxAgg)]=NA
  return(mm)
}


#calculate number of days between two thresholds using functions from climdex.pcic
ndays_in_range<-function (temp, date_factor, threshold, op = ">",threshold2,op2="<",iformat="perc",NAmaxAgg=20)
{stopifnot(is.numeric(temp) && is.numeric(threshold) && is.numeric(threshold2) && is.factor(date_factor))
  days<-climdex.pcic:::tapply.fast(match.fun(op)(temp, threshold) & match.fun(op2)(temp, threshold2), date_factor,
    sum, na.rm = TRUE,drop=FALSE)
  nna<-climdex.pcic:::tapply.fast(is.na(temp)==FALSE, date_factor,
    sum)
  length<-climdex.pcic:::tapply.fast(temp, date_factor,
    function(x) length(x))
  days[nna/length*100<(100-NAmaxAgg)]=NA
  if(iformat=="perc"){
    out<-round(days/nna*100,digits=1)
    out[which(nna==0)]=NA
    return(out)
  } else  return(days)
}


spell_length_max <- function(temp, date_factor, threshold, op, spells_span_agg=TRUE, NAmaxAgg =20){
  stopifnot(is.numeric(temp) && is.numeric(threshold) &&  is.factor(date_factor))
  days_op<-match.fun(op)(temp, threshold)


  if (spells_span_agg) {
    all.true <- climdex.pcic:::tapply.fast(days_op, date_factor, all)
    max.spell <- climdex.pcic:::tapply.fast(get_series_lengths_at_ends(days_op),
      date_factor, function(x) {
        namask=switch((length(which(is.na(x)))/length(x)*100 >NAmaxAgg)+1,1,NA)
        val=max(x)*namask
        return(val)})
    na.mask <- c(1, NA)[as.integer((max.spell == 0) & all.true) +
        1]
    max.spell <- max.spell * na.mask

    return(max.spell)
  }else {
    return(climdex.pcic:::tapply.fast(days_op, date_factor, function(x) {
      namask=switch((length(which(is.na(x)))/length(x)*100 >NAmaxAgg)+1,1,NA)
      val=max(get_series_lengths_at_ends(x))*namask
      return(val)}))
  }}

minmax_value <- function(temp, date_factor, func, NAmaxAgg=20,rx=1) {
  stopifnot(is.numeric(temp) && is.function(func) && is.factor(date_factor))

  if ( rx > 1){
    #temp[is.na(temp)]<-0
    temp.sum <- caTools::runmean(temp, k= rx, endrule="NA")
    temp.sum[is.na(temp.sum)]<- NA
    vals <- suppressWarnings(climdex.pcic:::tapply.fast(temp.sum, date_factor, func, na.rm=TRUE) * rx)
  } else vals <- suppressWarnings(climdex.pcic:::tapply.fast(temp,date_factor,func, na.rm=TRUE))
  vals[is.infinite(vals)] <- NA
  nna<-climdex.pcic:::tapply.fast(is.na(temp)==FALSE, date_factor,sum, na.rm = TRUE)
  length<-climdex.pcic:::tapply.fast(temp, date_factor, function(x) length(x))
  vals[nna/length*100<(100-NAmaxAgg)]=NA
  return(vals)
}

spell_duration <- function (temp, jdays, q_temp=NULL,q_temp_inbase=NULL,date_factor, func, op = ">",
  min_length = 6, spells_span_agg = TRUE, NAmaxAgg,...)
{
  stopifnot(is.numeric(c(temp, min_length)) , (is.numeric(q_temp)) ,
    is.factor(date_factor) , is.function(match.fun(op))& min_length >
      0)

  f <- match.fun(op)

  ## this function only considers outbase quantiles
  if (spells_span_agg) {
    periods <- select_block_gt_length(f(temp, q_temp[jdays]),
      min_length - 1)
    vals <- climdex.pcic:::tapply.fast(periods, date_factor, sum)
  }
  else {
    vals <- climdex.pcic:::tapply.fast(1:length(temp), date_factor,
      function(idx) {
        sum(select_block_gt_length(f(temp[idx],q_temp[jdays[idx]]), min_length - 1))
        })
  }

  nna<-climdex.pcic:::tapply.fast(is.na(temp)==FALSE, date_factor,sum, na.rm = TRUE)
  length<-climdex.pcic:::tapply.fast(temp, date_factor, function(x) length(x))
  vals[nna/length*100<(100-NAmaxAgg)]=NA
  return(vals)
}






#################################help functions#####################################

select_block_gt_length <- function (d, n, na_value = FALSE)
{ stopifnot(is.logical(d), is.numeric(n))
  if (n < 1)
    return(d)
  if (n >= length(d))
    return(rep(FALSE, length(d)))
  d[is.na(d)] <- na_value
  d2 <- Reduce(function(x, y) {
    return(c(rep(FALSE, y), d[1:(length(d) - y)]) & x)
  }, 1:n, d)
  return(Reduce(function(x, y) {
    return(c(d2[(y + 1):length(d2)], rep(FALSE, y)) | x)
  }, 1:n, d2))
}

get_series_lengths_at_ends <- function (x, na.value = FALSE) {
  stopifnot(is.logical(x) && is.logical(na.value))
  n <- length(x)
  if (n == 1)
    return(as.numeric(x))
  res <- rep(0, n)
  x[is.na(x)] <- na.value
  start <- which(x & !(c(FALSE, x[1:(n - 1)])))
  end <- which(x & !(c(x[2:n], FALSE)))
  res[end] <- end - start + 1
  return(res)
}
