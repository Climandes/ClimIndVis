#'A function to calculate a logistic, linear or non-parametric trend of an index.
#'@param index Array (dim>1) of index or variable for which trend is calculated.
#'@param trend Logical or character. If trend is a character string with "MannKendall" a non-parametric Mann-Kendall test is applied and a TeilSen slope estimated.
#'@param time Vector of years to base trend calculation on. If not provided and data is not a named vector or array, calculation is aborted.
#'@param targs List of arguments for calculation of trends. Depending on index or variable different methods for the trend calculations have to be chosen.
#' \itemize{
#'   \item \strong{method} Three methods are available: (1) logit ("log_reg") or (2) linear regression ("lin_reg") or (3) "MannKendall" calculates a TheilSen slope and a MannKendall test.
#'   \item \strong{count} set to TRUE, if the data is count data.
#'   \item \strong{log_trans} if the data is not normaly distributed, set log_trans to TRUE and a logarithmic transformation is applied.
#'   \item \strong{NAmaxTrend} Maximum value of NAs for calculation of trend. If value is exceeded, trend is set to NA.
#'
#'       }
#'@return The function returns a list with two elements "statistics" and "data". Data is an array with the variable dimension and four additional dimensions for fitted trend line, upper and lower confidence interval and a local polynomial regression fitting. "statistics" is an array with the variable dimensions and additional a dimension for start year, end year, p-value, relative trend, absolute trend and trend method.
#'@section Details: The default trend estimation and test depends on the nature of the calculated index: \cr
#'
#'For continuous data (e.g. sum, mean, prcptot, spi) ny default an ordinary linear regression is calculated with time as predictor for the trend estimate and a students-t test. If the data is not normaly distributed, a logarithmic transformation is applied before the trend calculation, e.g. for . \cr
#'
#'For count data (e.g dd, cdd, cwd) by default a logistic regression is calculated with time as predictor for the trend estimation and a students-t test. In the calculation a correction for overdispersion is applied. \cr
#'
#'If you choose trend = "MannKendall" instead of trend = TRUE the non-parametric Mann-Kendall test based on the relative ranking in the series is applied and a Theil-Sen slope is estimated. See e.g. Yue et al. 2002 or Mann (1945), Kendall (1975).
#'@keywords internal
#'

calc_index_trend <- function(index,trend,targs,time=NULL ){

  if(trend!=TRUE){targs$method <- "MannKendall"}
  sel_dim <- 1:(length(dim(index))-1)
  ifelse(length(sel_dim) >1,mydim <- sel_dim[-length(sel_dim)],mydim<- 1)
  if (is.null(time)){
    time <- as.numeric(dimnames(index)[[length(dim(index))]])
  }

   itrend <- as.array(apply(index,c(sel_dim), function(x)
    # tryCatch um fehlermeldungen abzufangen...
    tryCatch(calc_trend(x, time,targs),error=function(e) {
      dat.final <- matrix(-99.9,nrow = 4,ncol=length(x))
      stat <- data.frame(matrix(-99.9, nrow=1,ncol=6))
      return(list(data = dat.final, stat=stat))})
   ))


  tout <- apply(itrend,mydim,function(x) lapply(x, function(y) y[["data"]]))
  statistics <- apply(itrend,mydim,function(x) lapply(x, function(y) y[["stat"]]))
  data <-  array(unlist(tout), dim=c(4,rev(dim(index)[-mydim]),dim(index)[mydim]),
                 dimnames=c(list(c("fit","lwr","upr","loess")),
                            rev(dimnames(index)[-mydim]), dimnames(index)[mydim]))
  dl <- length(dim(data))
  data <- aperm(data, perm=c(mydim+dl-length(mydim), rev((1:dl)[-(mydim+dl-length(mydim))])))
  summary <- array(unlist(statistics), dim=c(6,rev(dim(index)[sel_dim[-mydim]]),dim(index)[mydim]),
                   dimnames= c(list(c("start","end","pval","rel.trend","abs.trend","method")),
                               rev(dimnames(index)[sel_dim[-mydim]]), dimnames(index)[mydim]))

  dl_s <- length(dim(summary))
  summary <- aperm(summary, perm=c((mydim+dl_s-length(mydim)),rev((1:dl_s)[-(mydim+dl_s-length(mydim))])))

  return(list(data=data,summary=summary))
}



calc_trend <- function(data,time ,targs){
  DAT <- data.frame(years=time, ind=data)
  leapyear_shift =TRUE
  dat.final <- matrix(NA,nrow = 4,ncol=length(DAT[,1]))

  if(all(is.na(data))) {return(list(data = dat.final, stat=stat))}
  else if(sum(is.na(data))>length(data)*(targs$NAmaxTrend)/100) # mehr als 80pro fehlen, trend nicht berechnet
    {return(list(data = dat.final, stat=stat))}
  else if(targs$method == "logit_reg"  & targs$count == T){
    s <- 0
    if(leapyear_shift == T){s <- 1}#if year starts in July (southern hemisphere)
    a_bisiesto <- data.frame(DAT[,1], numdays=ifelse(((DAT[,1] +s)%%4==0 & (DAT[,1] +s)%%100!=0) | (DAT[,1] +s)%%400==0,366,365) )
    DAT <- data.frame(years=DAT[,1],ind=DAT[,2],V3=a_bisiesto[,2] - DAT[,2])
    trd1  <- trend.logit(DAT[,1:3], type = "counts")
    pred1 <- predict(trd1$mod, data.frame(year = DAT[,1]), interval = "confidence", se.fit = T)
    fit1   <- pred1$fit
    se.fit1<- pred1$se.fit
    lw1 <- predict(loess(ind ~ years, data = DAT), data.frame(years = DAT[,1]))
    dat.final[1,]  = exp(fit1)/(1+exp(fit1))*365
    dat.final[2,] = exp(fit1-1.96*se.fit1)/(1+exp(fit1-1.96*se.fit1))*365
    dat.final[3,]  = exp(fit1+1.96*se.fit1)/(1+exp(fit1+1.96*se.fit1))*365
    dat.final[4,]   = lw1

    stat <- data.frame(
      inicio = DAT[1,1],
      fin = DAT[dim(DAT)[1],1],
      p.value = trd1$pval,
      trend.rel = (trd1$trend.rel*100),
      trend.abs = trd1$trend.rel*mean(dat.final[1,], na.rm=TRUE)/dim(DAT)[1]*10,
      method = 2)
  }

  #########################################################################################
  # Indicators such as tx90p that provide a percentage as value
  #########################################################################################
  else if(targs$method == "logit_reg"  & targs$count == F){
    if(max(DAT[,2], na.rm =T) > 1){
      DAT[,2] <- DAT[,2]/100 # change percentage to probabilities
    }
    trd1  <- trend.logit(DAT[,1:2], type = "probs")
    pred1 <- predict(trd1$mod, data.frame(year = DAT[,1]),
                     interval = "confidence", se.fit = T)
    fit1   <- pred1$fit
    se.fit1<- pred1$se.fit
    lw1 <- predict(loess(ind~years, data = DAT), data.frame(years = DAT[,1]))
       dat.final[1,]  = exp(fit1)/(1+exp(fit1)) * 100
       dat.final[2,]  = exp(fit1-1.96*se.fit1)/(1+exp(fit1-1.96*se.fit1)) * 100
       dat.final[3,]  = exp(fit1+1.96*se.fit1)/(1+exp(fit1+1.96*se.fit1)) * 100
       dat.final[4,]    = lw1 * 100

    stat <- data.frame(
      inicio = DAT[1,1],
      fin = DAT[dim(DAT)[1],1],
      p.value = trd1$pval,
      trend.rel = (trd1$trend.rel*100),
      trend.abs = trd1$trend.rel*mean(dat.final[1,], na.rm=TRUE)/dim(DAT)[1]*10,
      method = 2)
  }

  ######Calculo de Tendencia lineal############################
  else if(targs$method == "lin_reg"){
    if(targs$log_trans == T & length(which(DAT[,2]== 0)) == 0){
      DAT[,3] <- log(DAT[,2])
      trd1<- trend.linreg(DAT[,c(1,3)])
      pred1 <- predict(trd1$mod, data.frame(year = DAT[,1]),
                       interval = "confidence", se.fit = T, level = 0.95)
      fit <- pred1$fit
      method <- 1}
    if(targs$log_trans == F){
      trd1<- trend.linreg(DAT[,c(1,2)])
      pred1 <- predict(trd1$mod, data.frame(year = DAT[,1]),
                       interval = "confidence", se.fit = T, level = 0.95)
      fit <- pred1$fit
      method <-1
    }
    if(targs$log_trans == T & length(which(DAT[,2]== 0)) != 0){
      DAT[which(DAT[,2]==0),2] <- 0.0000001
      DAT[,3] <- log(DAT[,2])
      trd1<- trend.linreg(DAT[,c(1,3)])
      pred1 <- predict(trd1$mod, data.frame(year = DAT[,1]),
                       interval = "confidence", se.fit = T, level = 0.95)
      fit <- pred1$fit
      method <- 1
    }
    lw1 <- predict(loess(ind~years, data = DAT), data.frame(years = DAT[,1]))
    if(targs$log_trans == T){
         dat.final[1,]  = exp(fit[,1])
        dat.final[2,]  = exp(fit[,2])
        dat.final[3,]   = exp(fit[,3])
        dat.final[4,]   = lw1

      trend.abs <- (dat.final[1,dim(DAT)[1]] - dat.final[1,1])/(dim(DAT)[1])*10
      trend.rel <- ((dat.final[1,dim(DAT)[1]] - dat.final[1,1])/(dat.final[1,round(dim(DAT)[1]/2)])*100)
      stat <- data.frame(
        inicio = DAT[1,1],
         fin = DAT[dim(DAT)[1],1],
        p.value = trd1$pval,
        trend.rel = switch(targs$rel +1, -99.9 ,trend.rel),
        trend.abs = trend.abs,
        method = method
      )
    }else{
      dat.final[1,]  = pred1$fit[,1]
      dat.final[2,]  = pred1$fit[,2]
      dat.final[3,]  = pred1$fit[,3]
      dat.final[4,]  = lw1

      stat <- data.frame(
        inicio = DAT[1,1],
        fin = DAT[dim(DAT)[1],1],
        p.value = trd1$pval,
        trend.rel = switch(targs$rel +1, -99.9 ,trd1$trend.rel),
        trend.abs = trd1$trend.abs/dim(DAT)[1]*10,
        method= method)
    }
  }
  else if(targs$method == "MannKendall"){
    trd1<- trend.MannKendall(DAT[,c(1,2)])
    #pred1 <- predict(trd1$mod, data.frame(year = DAT[,1]),
    #                 interval = "confidence", se.fit = T, level = 0.95)
    #fit <- pred1$fit
    lw1 <- predict(loess(ind~years, data = DAT), data.frame(years = DAT[,1]))

    dat.final[1,]  = trd1$fitted$value
    dat.final[2,]  = rep(NA,length(lw1))
    dat.final[3,]  = rep(NA,length(lw1))
    dat.final[4,]  = lw1

    stat <- data.frame(
      inicio = DAT[1,1],
      fin = DAT[dim(DAT)[1],1],
      p.value = trd1$pval,
      trend.rel = trd1$trend.rel*100,
      trend.abs = trd1$trend.abs/dim(DAT)[1]*10,
      method= 3)

  }
 return(list(data = dat.final, stat= stat))

}

#################################################################
######### trend lin reg from Chr.Frei
#################################################################

trend.linreg <-
  function (dat.df,conf.probs=c(0.025,0.975)) {
    # predictor
    year <- as.array(dat.df[,1])
    # predictand (responses)
    resp <- as.array(dat.df[,2])
    # calculate linear regression
    trd.lm <- stats::lm(resp ~ year,na.action=na.omit)
    # prepare elements of trend object:
    # intermediate calculations
    coef <- stats::coefficients(trd.lm)   # model coefficients
    # standard error of slope
    stderr <- summary(trd.lm)$coefficients[2,2]
    t2 <- year[length(year)]      # last year
    t1 <- year[1]                 # first year
    t0 <- (t2+t1)/2               # middle year
    v0 <- coef[1]+t0*coef[2]      # value at middle year
    # fitted values:
    indx <- !is.na(resp)       # fitted omits NAs in predictor !
    fits <- matrix(c(year[indx],fitted(trd.lm)),ncol=2)

    # trend magnitude as absolute change over the full period
    trd.abs <- ((coef[1]+t2*coef[2])-(coef[1]+t1*coef[2]))
    # trend magnitude as fractional change of average
    if (v0 == 0) {
      trd.rel <- NA
    } else {
      trd.rel <- ((coef[1]+t2*coef[2])-(coef[1]+t1*coef[2]))/v0
    }
    # trend magnitude as ratio between end/beginning
    if ((coef[1]+t1*coef[2]) == 0) {
      trd.rat <- NA
    } else {
      trd.rat <- ((coef[1]+t2*coef[2])/(coef[1]+t1*coef[2]))
    }

    # confidence interval of trend magnitude
    df <- summary(trd.lm)$df[2]
    tcrit <- stats::qt(p=conf.probs,df=df)
    trd.conf.abs <- ((t2-t1)*(coef[2]+stderr*tcrit))
    names(trd.conf.abs) <- as.character(conf.probs)
    #trd.conf.rel <- ((v0+(t2-t0)*(coef[2]+stderr*tcrit))-
    #                 (v0+(t1-t0)*(coef[2]+stderr*tcrit)))/v0
    #names(trd.conf.rel) <- as.character(conf.probs)

    # p-value of trend coefficient using standard T-Test
    pval <- summary(trd.lm)$coefficients[2,4]

    # return trend-object
    res <- list(data=dat.df,
                mod=trd.lm,
                trend.abs=trd.abs,
                trend.rel=trd.rel,
                trend.rat=trd.rat,
                trend.conf.abs=trd.conf.abs,
                #trend.conf.rel=trd.conf.rel,
                coef=coef,
                pval=pval,
                fitted=fits,
                method="linreg")
    return(res)
  }

# trend logit

trend.logit <-  function (dat.df,type="counts",cor.overdisp=TRUE,
            conf.probs=c(0.025,0.975)) {

    # functions (logit and inverse logit)
    logit <- function(pi) log(pi/(1-pi))
    alogit <- function(x) exp(x)/(1+exp(x))

    # predictor
    year <- dat.df[,1]

    # predictand (responses)
    if ((type == "counts") && (dim(dat.df)[2] <= 2)) {
      stop(paste("** ERROR ** logit.trend, negative responses required ",
                 "in third column if type=counts",sep="")) }
    resp <- as.matrix(switch(type,
                             "counts" = dat.df[,c(2,3)], # c(successes, failures)
                             "probs" = dat.df[,2]) )      # probabilities [0,1]

    # calculate logistic regression with/without
    # correction for overdispersion
    if(cor.overdisp) {
      trd.glm <- stats::glm(resp ~ year,
                     family=quasibinomial(link=logit),
                     na.action=na.omit)
    } else {
      trd.glm <- stats::glm(resp ~ year,
                     family=binomial(link=logit),
                     na.action=na.omit)
    }

    # prepare elements of trend object:

    # intermediate calculations
    coef <- coefficients(trd.glm)   # model coefficients
    stderr <- summary(trd.glm)$coefficients[2,2]   # standard error of slope
    t2 <- year[length(year)]      # last year
    t1 <- year[1]                 # first year
    t0 <- (t2+t1)/2               # middle year
    p0 <- alogit(coef[1]+t0*coef[2])   # probability at middle year

    # fitted values:
    # note that fitted returns probs and not counts
    # here probs are augmented to counts if type = "counts"
    indx <- !is.na(resp[,1])       # fitted omits NAs in predictor !
    fits <- matrix(
      c(year[indx],
        switch(type,
               "counts" = (resp[indx,1]+resp[indx,2])*fitted(trd.glm),
               "probs"  = fitted(trd.glm))),
      ncol=2)

    # trend magnitude as fractional change of average

    ###### modified imn #### no relativ trend for low values middle values
    test <- p0 < 0.0001 & p0 > -0.0001
    switch(test+1,trd <- (alogit(coef[1]+t2*coef[2])-alogit(coef[1]+t1*coef[2]))/p0, trd <- NA)
    ######

    # trend magnitude and confidence interval as odds-ratio
    trd.or <- exp(coef[2]*(t2-t1))
    df <- summary(trd.glm)$df[2]
    tcrit <- qt(p=conf.probs,df=df)
    trd.or.conf <- exp((coef[2]+stderr*tcrit)*(t2-t1))

    # confidence interval of trend magnitude
    df <- summary(trd.glm)$df[2]
    tcrit <- qt(p=conf.probs,df=df)
    trd.conf <- (alogit((coef[1]+coef[2]*t0)+(t2-t0)*(coef[2]+stderr*tcrit))-
                   alogit((coef[1]+coef[2]*t0)+(t1-t0)*(coef[2]+stderr*tcrit)))/p0

    # p-value of trend coefficient
    pval <- summary(trd.glm)$coefficients[2,4]

    # dispersion factor
    dsper <- summary(trd.glm)$dispersion

    # return trend-object
    res <- list(mod=trd.glm,
                trend.rel=trd,
                odds.ratio=trd.or,
                odds.ratio.conf=trd.or.conf,
                trend.conf=trd.conf,
                pval=pval,
                fitted=fits,
                dispersion=dsper,
                method=paste("logit.",type,sep=""))
    return(res)
  }


trend.MannKendall<- function (dat.df) {
  ii <- !is.na(dat.df[, 2])
  tt <- as.array(dat.df[ii, 1])
  aa <- as.array(dat.df[ii, 2])
  ttt <- as.array(dat.df[, 1])
  num.na <- length(ii) - sum(ii)
  if (any(sign(diff(ttt)) < 0)) {
    stop("Input data must be sequential.")
  }
  if (length(aa) < 8) {
    stop(paste("Too small sample. Only asymptotic version",
               "of test implemented so far."))
  }
  TSS <- TheilSen(dat.df)
  TS <- TSS$TS
  t2 <- ttt[length(ttt)]
  t1 <- ttt[1]
  t0 <- (t2 + t1)/2
  v0 <- TS[1] + t0 * TS[2]
  fits <- TS[1] + ttt * TS[2]
  fits <- data.frame(time = ttt, value = fits)
  trd.abs <- ((TS[1] + t2 * TS[2]) - (TS[1] + t1 * TS[2]))
  if (v0 == 0) {
    trd.rel <- NA
  }
  else {
    trd.rel <- ((TS[1] + t2 * TS[2]) - (TS[1] + t1 * TS[2]))/v0
  }
  if ((TS[1] + t1 * TS[2]) == 0) {
    trd.rat <- NA
  }
  else {
    trd.rat <- ((TS[1] + t2 * TS[2])/(TS[1] + t1 * TS[2]))
  }
  S <- TSS$S
  ll <- length(aa)
  sigma <- ll * (ll - 1) * (2 * ll + 5)/18
  Z <- (S - sign(S))/sqrt(sigma)
  pval <- 2 * pnorm(q = Z, lower.tail = (Z < 0))
  res <- list(data = dat.df, TheilSen = TS, coef = TS, fitted = fits,
              trend.abs = trd.abs, trend.rel = trd.rel, trend.rat = trd.rat,
              S = S, Z = Z, pval = pval, num.na = num.na, method = "Mann-Kendall")
  return(res)
}

TheilSen <- function (dat.df) {
  ii <- !is.na(dat.df[, 2])
  tt <- as.array(dat.df[ii, 1])
  aa <- as.array(dat.df[ii, 2])
  ll <- length(aa)
  ind.p <- expand.grid(1:ll, 1:ll)
  ind.p <- ind.p[(ind.p[, 1] < ind.p[, 2]), ]
  slopes <- (aa[ind.p[, 2]] - aa[ind.p[, 1]])/(tt[ind.p[, 2]] -
                                                 tt[ind.p[, 1]])
  slope <- stats::median(slopes)
  intercpt <- stats::median(aa) - slope * median(tt)
  S <- sum(sign(aa[ind.p[, 2]] - aa[ind.p[, 1]]))
  TS <- c(intercpt, slope)
  list(TS = TS, S = S)
}

