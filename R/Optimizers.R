
optE_int = function(par,dists,pows,opt=T){
  alp = exp(par)/(1+exp(par))
  thresh<-quantile(dists[!pows],1-alp)
  beta<-mean(dists[pows]<thresh)
  if(opt)return((alp-beta)^2)
  if(!opt)return(c(alp,beta))
}


optimE<-function(ECP_obj,OMind = 1:48, Iind=NULL,yind=1:6,powind=1,byyr=F){

  if(is.null(Iind))Iind=ECP_obj$Defaults$Data
  if(!byyr) out=get_pred_sim(ECP_obj,OMind,Iind,powind=powind)
  if(byyr) out=get_pred_sim_yr(ECP_obj,OMind,Iind,yind, powind=powind)

  preds=out$preds
  obs=out$obs
  pows=out$pows
  yrs=out$yrs

  nullcov<-cov(preds)
  nullm<-apply(preds,2,mean,na.rm=T)
  dists<-mahalanobis_robust(x=preds, center=nullm, cov=nullcov)

  opt=optimize(optE_int,interval=c(-4,4),dists=dists, pows=pows)
  optE_int(opt$minimum,dists=dists, pows=pows, opt=F)

}


Brute_find = function(ECP_obj, OMind, Iind, yind, powind, tail, targ_cml_alp = 0.2, yr=3){

  ni = length(Iind)
  perms = expand.grid(rep(list(c(T,F)),ni))
  np = nrow(perms)

  findpow = function(x,perms,ECP_obj, OMind, Iind, yind, powind, tail, targ_cml_alp = 0.1,yr){
    incvec = unlist(perms[x,])
    errmat=EPTO(ECP_obj, OMind, Iind[incvec], yind, powind, tail[incvec], alp_rng = c(0.001,0.05),ys=c(yr,6),plot=F)
    t1s =  errmat[1,,1]
    pows = errmat[1,,2]
    approx(t1s,pows,targ_cml_alp)$y
  }

  pows = sapply(1:10,findpow,perms=perms,ECP_obj=ECP_obj, OMind=OMind, Iind=Iind, yind=yind, powind=powind, tail=tail, targ_cml_alp = 0.1)

  sfInit(parallel=T,cpus=7)
  sfExport(list=list('perms',"ECP_obj",'OMind','Iind','yind','powind','tail','targ_cml_alp','EPTO','Seq_Pow_Calc_Marg','yr','pcalc2'))
  pows = sfSapply(1:np,findpow,perms=perms,ECP_obj=ECP_obj, OMind=OMind, Iind=Iind, yind=yind, powind=powind, tail=tail, targ_cml_alp = 0.1)


}
