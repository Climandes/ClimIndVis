
####################### index functions ##################################################################
aggregate_var<-function (temp, date_factor,NAmaxAgg=20,aggfun)
{
  stopifnot(is.numeric(temp) && is.factor(date_factor))
  agg<-climdex.pcic:::tapply.fast(temp, date_factor,
    aggfun, na.rm = TRUE)
  nna<-climdex.pcic:::tapply.fast(is.na(temp)==FALSE, date_factor,
    sum, na.rm = TRUE)
  length<-climdex.pcic:::tapply.fast(temp, date_factor,
    function(x) length(x))
  agg[nna/length*100<(100-NAmaxAgg)]=NA
  return(agg)
}


#calculate number of days op(">","<","<=",">=") a threshold using functions from climdex.pcic
ndays_op_threshold<-function (temp,q_temp=NULL,date_factor, threshold=NULL, op = "<",iformat="perc",NAmaxAgg=20,...)
{
  stopifnot(is.numeric(temp) && is.factor(date_factor) && (is.numeric(q_temp) | is.numeric(threshold)))


  if(!is.null(q_temp)){
    days <- mapply_fast(temp,q_temp, date_factor, function(x,y,z){
      sum(match.fun(op)(x,y), na.rm=TRUE)}, z=op)

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
    # ifelse(is.null(q),outlist <- out,outlist <- list(data=out,quant=quants$tresh))
    return(out)
  } else
    #ifelse(is.null(q),outlist <- days,outlist <- list(data=days,quant=quants$tresh))
    return(days)
}

#calculate sum of of days op a threshold
total_precip_op_threshold <- function(temp, q_temp=NULL,date_factor, threshold=NULL, op= ">", NAmaxAgg=20){
  stopifnot(is.numeric(temp) && is.factor(date_factor) && (is.numeric(q_temp) | is.numeric(threshold)))

  if(!is.null(q_temp)){

    mm <- mapply_fast(temp,q_temp, date_factor, function(x,y,z){
      sum(x[match.fun(op)(x,y)], na.rm=TRUE)
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
      date_factor, max)
    na.mask <- c(1, NA)[as.integer((max.spell == 0) & all.true) +
        1]
    return(max.spell * na.mask)
  }
  else {
    return(climdex.pcic:::tapply.fast(days_op, date_factor, function(x) {
      max(get_series_lengths_at_ends(x))
    }))
  }}

minmax_value <- function(temp, date_factor, func, NAmaxAgg=20,rx=1) {
  stopifnot(is.numeric(temp) && is.function(func) && is.factor(date_factor))

  if ( rx > 1){
    #temp[is.na(temp)]<-0
    temp.sum <- caTools::runmean(temp, k= rx, endrule="NA")
    temp.sum[is.na(temp.sum)]<- NA
    vals <- climdex.pcic:::tapply.fast(temp.sum, date_factor, func, na.rm=TRUE) * rx
  } else vals <- climdex.pcic:::tapply.fast(temp,date_factor,func, na.rm=TRUE)

  nna<-climdex.pcic:::tapply.fast(is.na(temp)==FALSE, date_factor,sum, na.rm = TRUE)
  length<-climdex.pcic:::tapply.fast(temp, date_factor, function(x) length(x))
  vals[nna/length*100<(100-NAmaxAgg)]=NA
  return(vals)
}

spell_duration <- function (temp, q_temp=NULL,date_factor, func, op = ">",
  min_length = 6, spells_span_agg = TRUE, NAmaxAgg)
{
  stopifnot(is.numeric(c(temp, min_length)) , (is.numeric(q_temp)) ,
    is.factor(date_factor) , is.function(match.fun(op))& min_length >
      0)

  f <- match.fun(op)

  if (spells_span_agg) {
    periods <- select_block_gt_length(f(temp, q_temp),
      min_length - 1)
    vals <- climdex.pcic:::tapply.fast(periods, date_factor, sum)
  }
  else {
    vals <- climdex.pcic:::tapply.fast(1:length(temp), date_factor,
      function(idx) {sum(select_block_gt_length(f(temp[idx],
        q_temp[idx]), min_length - 1))})
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
