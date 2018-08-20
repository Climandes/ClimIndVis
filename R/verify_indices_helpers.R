

verify_climindvis_index<-function(obs, hc, veri_metrics, hc_ref = NULL, na_rm = TRUE, nmin = 20, ncat = 3, prob = NULL){

  library(easyVerification)
  if (missing(prob)) prob = NULL
  check_class(list(obs,hc),"climindvis_index")
  compare_class(obs,hc)
  check_spatial_dims(list(obs,hc))
  if(!is.null(hc_ref)){
    check_class(list(hc,hc_ref),"climindvis_index")
    compare_class(hc,hc_ref)
    check_spatial_dims(list(hc,hc_ref))
  }


  tryCatch({
    if(!is.null(hc_ref)){
      sel<-sapply(veri_metrics, function(x) str_sub(x,-2) == "ss")
      if(is.element("EnsRocss",veri_metrics)) sel[which(veri_metrics == "EnsRocss")]=FALSE
      veri_metrics<-veri_metrics[sel]
      if(length(veri_metrics) == 0){
        stop("if verified agains reference forecast (hc_ref) only skill scores are computed. <<veri_metrics>> does not contain any skill scores")}
      if (!all(sel)) message("If verified agains hc_ref, only skill scores are computed.")
    }
    if(is.null(hc_ref)) {
      tdims<-get_matching_tdims_index(list(obs,hc))
    } else tdims<-get_matching_tdims_index(list(obs,hc,hc_ref))
    if (length(tdims$year)<nmin) stop("number of forecasts that are not-missing are smaller than value of nmin (=",nmin,"). Check input data or change <<nmin>> in arguments passed to verify_index function.")
    gdim<-which(is.element(hc$index_info$idims,c("p","lon","lat")))
    lg=length(gdim)
    ensdim=1+lg
    nens=dim(hc$index)[ensdim]
  }, error=function(cond){
    message("error in input data:")
    message(cond)
    stop_quietly()
  })

  veri_info<-get_veri_info(veri_metrics)
  tdim<-ifelse(is.element("agg",hc$index_info$idims) ,3+lg,2+lg)


  if (!is.null(tdims$agg)){
    help_obs<-index_array(obs$index, c(which(obs$index_info$idims == "agg"),which(obs$index_info$idims == "year")), list(tdims$agg,tdims$year))
    help_hc <-index_array(hc$index, c(which(hc$index_info$idims == "agg"),which(hc$index_info$idims == "year")), list(tdims$agg,tdims$year))
    if(!is.null(hc_ref))  help_hc_ref <-index_array(hc_ref$index, c(which(hc_ref$index_info$idims == "agg"),which(hc_ref$index_info$idims == "year")), list(tdims$agg,tdims$year))
  } else {
    help_obs<-index_array(obs$index, c(which(obs$index_info$idims == "year")), list(tdims$year))
    help_hc <-index_array(hc$index, c(which(hc$index_info$idims == "year")), list(tdims$year))
    if(!is.null(hc_ref)) help_hc_ref <-index_array(hc_ref$index, c(which(hc_ref$index_info$idims == "year")), list(tdims$year))
  }
  if(is.null(hc_ref)) help_hc_ref=NULL

  veri<-list()
  for (vv in veri_metrics){
    if(veri_info[vv,"probs"] == TRUE) {
      if(is.null(prob))prob= 1:(ncat-1)/ncat
    } else prob=NULL

    veri[[vv]]<-easyVerification::veriApply(vv, help_hc, help_obs, fcst.ref = help_hc_ref,
      tdim = tdim, ensdim = ensdim, prob = prob, na.rm = na_rm,
      nmin = nmin)
    if (is.list(veri[[vv]]) ){
      if (veri_info[vv,"nout"]!=1){#checken ob das fr alle gilt
        veri[[vv]]<-veri[[vv]][1:ncat]
      } else veri[[vv]]=veri[[vv]][[1]]
    }
    if (!is.null(tdims$agg)) {
      if (veri_info[vv,"tdim"] == "ny") {
        if (veri_info[vv,"nout"]!=1){
          for (i in 1:ncat) dimnames(veri[[vv]][[i]])=c(rep(list(NULL),lg),list(tdims$agg,tdims$year))
        } else dimnames(veri[[vv]])=c(rep(list(NULL),lg),list(tdims$agg,tdims$year))
      } else {
        if (veri_info[vv,"nout"]!=1) {
          for (i in 1:ncat) dimnames(veri[[vv]][[i]])=c(rep(list(NULL),lg),list(tdims$agg))
        }  else dimnames(veri[[vv]])=c(rep(list(NULL),lg),list(tdims$agg))
      }
    }
  }

  same_obs=apply(help_obs,c(1:ndims(help_obs))[-(tdim-1)], function(x){
    if (length(unique(quantile(x,probs=1:(ncat-1)/ncat,na.rm=TRUE)))<(ncat-1)){
      return(TRUE)
    } else return(FALSE)
  })
  same_hc=apply(help_hc,c(1:ndims(help_hc_ref))[-c(ensdim,tdim)], function(x){
    if (length(unique(quantile(x,probs=1:(ncat-1)/ncat,na.rm=TRUE)))<(ncat-1)){
      return(TRUE)
    } else return(FALSE)
  })
  if(!is.null(help_hc_ref)){
    same_hc_ref=apply(help_hc_ref,c(1:ndims(help_hc_ref))[-c(ensdim,tdim)], function(x){
      if (length(unique(quantile(x,probs=1:(ncat-1)/ncat,na.rm=TRUE)))<(ncat-1)){
        return(TRUE)
      } else return(FALSE)
    })
    same=same_obs+same_hc+same_hc_ref>0
  } else same=same_obs+same_hc>0



  climindvis_veri<-list()
  climindvis_veri$metrics<-veri_metrics
  climindvis_veri<- list(verification=veri,veri_info=veri_info,years=paste0(min(tdims$year),"-",max(tdims$year)),same=same)
  climindvis_veri[c("lon","lat")]<-obs[c("lon","lat")]
  climindvis_veri[["data_info"]][["obs"]]<-obs$data_info
  climindvis_veri[["data_info"]][["hc"]]<-hc$data_info
  class(climindvis_veri) <- c(class(obs)[1],"climindvis_verification")
  return(climindvis_veri)
}


#get information on verification functions
get_veri_info<-function(verilist_in){
  #vinfo_all<-read.csv(system.file("data","verification_metrics_info.csv",package="ClimIndVis"),header=TRUE,sep=";",row.names=1)
  x<-sapply(verilist_in,function(v) {
    r<-which(rownames(vinfo_all) == v)
    if (length(r)  ==  0 ) {stop (paste0("metric <<",v,">> is not implemented (yet). Please add information on metric to file. See also ?verification_metric_info"))
      return(FALSE)
    } else return(r)})
  return(vinfo_all[x,])
}
