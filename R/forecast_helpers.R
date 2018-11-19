
#for single stations input data spatial dimension still needs to be of length 1
get_cats<-function(dat,cd,thresholds,ydim=0,prob=FALSE){

  dd=dim(dat)
  dt=dim(thresholds)

  same=array(apply(thresholds,2:length(dt), function(x) {
    ifelse(length(unique(x))==1,TRUE,FALSE)
  }))
  if(is.null(dt)) dt=length(thresholds)
  if(ydim!=0){
      cd=c(cd,ydim)
      thresholds=replicate(dd[ydim],thresholds)
      dt=c(dt,dd[ydim])
    }
  nens=dd[-cd][1]
  if(!all(dd[cd]==dt[2:length(dt)])) stop("dimensions of dat and thresholds do not match")
  hdat<-array(aperm(dat,c(cd,(1:ndims(dat))[-cd])),dim=c(prod(dd[cd]),dd[-cd]))
  hth=array(thresholds,dim=c(dt[1],prod(dt [2:ndims(thresholds)])))

  chelp<- sapply(1:prod(dd[cd]),function(x) {
    if(sum(is.na(hdat[x,]))< length(hdat[x,])*0.2){
      r<-easyVerification::convert2prob(t(as.numeric(hdat[x,][!is.na(hdat[x,])])),threshold=hth[,x])
    } else r<-array(NA,dim=c(1,3))
    return(r)})
  #chelp[,which(same)]=NA
  chelp[,which(same)]=0
  if(prob==TRUE) chelp=round(chelp/nens*100)

  cats<-aperm(array(chelp,dim=c(dt[1]+1,dd[cd])),c(2:(length(cd)+1),1))
  return(cats)
}

get_cats_fc<-function(ind_dat,grid,points,probs,prob=TRUE,ret_th=FALSE){

  cat=lapply(list("points","grid"), function(x) {
    if(get(x)==1){
      suffix=ifelse(x=="points","p",x)
      ifelse(suffix=="p",cd<-1,cd<-c(1,2))
      ad=which(ind_dat[[paste0("hc_",suffix)]]$index_info$idims=="agg")
      q_hc<- apply(ind_dat[[paste0("hc_",suffix)]]$index,c(cd,ad),function(x) quantile(x,probs,na.rm = TRUE , type=8))
      ly=length(ind_dat[[paste0("hc_",suffix)]]$index_info$years)
      yd=which(ind_dat[[paste0("fc_",suffix)]]$index_info$idims=="year")
      cat_fc<-get_cats(ind_dat[[paste0("fc_",suffix)]]$index,c(cd,ad),q_hc,ydim=ifelse(length(yd)==0,0,yd),prob=prob)
      cat_dims=list(cd=cd,aggd=ad-1,yd=yd-1)
      if(length(ad)==0) cat_dims$aggd=NULL
      if(ret_th){
        return(list(val=cat_fc,dims=cat_dims,q_hc=q_hc))
      } else return(list(val=cat_fc,dims=cat_dims))
    } else return(NULL)
  })
  names(cat)=list("p","grid")
  return(cat)
}

get_cth_obs<-function(ind_dat,grid,points,probs){

  cth= lapply(list("points","grid"), function(x) {
    if(get(x)==1){
      suffix=ifelse(x=="points","p",x)
      ifelse(suffix=="p",cd<-1,cd<-c(1,2))
      ad=which(ind_dat[[paste0("obs_",suffix)]]$index_info$idims=="agg")
      q_obs<- apply(ind_dat[[paste0("obs_",suffix)]]$index,c(cd,ad),function(x) quantile(x,probs,na.rm = TRUE , type=8))
      return(q_obs)
    } else return(NULL)
  })
  names(cth)=c("points","grid")
  return(cth)
}



sel_maxcat<- function(dat){
  if(all(is.na(dat))){
    mc<-NA
  } else if (all(dat==0)){
    mc<-0
  } else {
    help = which(dat == max(dat))
    if (length(help)>1){
      mc=-999
    } else mc=help
  }
  return(mc)
}

sel_maxcat_value<- function(dat){
  if(all(is.na(dat))){
    mc<-NA
  } else if (all(dat==0)){
    mc=0
  } else {
    help = which(dat == max(dat))
    if (length(help)>1){
      mc=-999
    } else mc=dat[help]
  }
  return(mc)
}

get_skill_maxcat<-function(veri_dat,maxcat,veri_metric){
  skill=mapply(function(vd,mc){

    if(!is.null(vd) & !is.null(mc)){
      hvd=vd$verification[[veri_metric]]
      same=vd$same
      res=array(NA,dim=dim(mc))
      for (i in 1:length(hvd)) {
        res[mc==i & !is.na(mc)]=hvd[[i]][mc==i & !is.na(mc)]
      }
      res[mc==0 & !is.na(mc)]=0
      res[same]=NA
      return(res)
    } else return(NULL)
  },veri_dat,maxcat,SIMPLIFY = FALSE)
  return(skill)
}

call_verify_helper_autoplot<-function(ind_data,veri_metrics,veri_args,grid,points){
  error=0
  if (points==1 ) {
    veri_ind_p<-tryCatch({
      veri_ind_p<-do.call("verify_climindvis_index",c(list(obs=ind_data$obs_p,hc=ind_data$hc_p,hc_ref=ind_data$hc_ref_p,veri_metrics), veri_args))
    }, error=function(cond){
      message(cond)
    })
    if(is.null(veri_ind_p)) error=1
  } else veri_ind_p<-NULL

  if(grid==1 & error!=1){
    veri_ind_grid<-tryCatch({
      do.call("verify_climindvis_index",c(list(obs=ind_data$obs_grid,hc=ind_data$hc_grid,hc_ref=ind_data$hc_ref_grid,veri_metrics), veri_args))}, error=function(cond){
        message(cond)
      })
    if(is.null(veri_ind_grid)) error=1
  } else veri_ind_grid<-NULL

  return(list(p=veri_ind_p,grid=veri_ind_grid,error=error))
}


#if any hc_ref !=NULL only skill scores are computed
skill_scores_only<-function(data,veri_metrics){
  tryCatch({
    if(any(grepl("hc_ref",names(data)))){
      if(any(sapply(which(grepl("hc_ref",names(data))),function(d) !is.null(data[[d]])))){
        sel<-sapply(veri_metrics, function(x) stringr::str_sub(x,-2)=="ss")
        if(is.element("EnsRocss",veri_metrics)) sel[which(veri_metrics=="EnsRocss")]=FALSE
        veri_metrics<-veri_metrics[sel]
        if(length(veri_metrics)==0) {
          veri_metrics=NULL
          stop("if verified against reference forecast (hc_ref) only skill scores are computed. <<veri_metrics>> does not contain any skill scores")
        }
        if (!all(sel)) message("If verified against hc_ref, only skill scores from <<veri_metrics>> are computed.")

      }}
  }, error=function(cond){
    message("error in verification")
    message(cond)
  }, warning=function(cond){
    message("warning related to verification:")
    message(cond)
  }, finnaly={})
  return(veri_metrics)
}



