
is_Blow = function(MSEobj,MPind,PPDy,stockind=2){

  yind = MSEobj@nyears+11:30 # projection years 1:30 for Blim
  Blow = function(x)any(x<0.2) # half of Blim
  temp = MSEobj@B_BMSY[MPind,,stockind,yind] #  stocks
  apply(temp,1,Blow)

}

Index_compiler<-function(dir=NULL,name=NULL, MPind=NULL, dummy_obs=T, First_Yr = 2020,dummycut=5){

  if(is.null(Indices))stop("You need to run loadABT()")

  files<-list.files(dir)
  isMSE<-grepl("MSE_",files)
  MSEref<-c(paste0("MSE_",1:48),paste0("MSE_R_",1:44))
  nOMs<-length(MSEref)
  fileref<-paste0(dir,MSEref,".rda")


  MSEobj= readRDS(fileref[1])
  if(length(MSEobj@PPD)==0) stop(paste0("There are no posterior predicted data in: ",fileref[1],". You need to run the MSEs with the argument returnMSE = T, see CMP dev guide"))
  if(is.null(MPind))MPind = length(MSEobj@PPD)

  Eastind = 1
  PPDi = MSEobj@PPD[[MPind]][[Eastind]]$Iobs # second tier is stock and that is identical in terms of index reporting - same indices are available to both stocks
  idims = dim(PPDi)

  lastYr = 2019
  nsim = idims[1]
  nind = idims[2]
  PPDy = idims[3]
  yind = (MSEobj@nyears+1):PPDy
  npy=length(yind)
  Yrs = lastYr + 1: npy

  PPD = array(NA,c(nsim,nOMs,nind,npy))
  npow=3
  Pow = array(F,c(nsim,nOMs,npow))

  # fileref = rep(fileref[1:48],20)[1:nOMs]

  for(OM in 1:nOMs){
    MSEobj<-readRDS(fileref[OM])
    PPDi = MSEobj@PPD[[MPind]][[Eastind]]$Iobs # second tier is stock and that is identical in terms of index reporting - same indices are available to both stocks
    PPD[,OM,,] = PPDi[,,yind]
    Pow[,OM,1]= is_Blow(MSEobj,MPind,PPDy,stockind=1)
    Pow[,OM,2]= is_Blow(MSEobj,MPind,PPDy,stockind=2)
    Pow[,OM,3]= is_Blow(MSEobj,MPind,PPDy,stockind=1:2)
  }

  DesignRef = ABTMSE::Design$Design_Ref
  # ref OMs default: 1L, 2L, 1L, 2L.

  ROMind = c(rep(c(1,2,4,5),8),c(3,6,3,6),rep(c(1,2,4,5),2))
  Design = rbind(DesignRef, DesignRef[ROMind,])
  Design = cbind(Design,c(rep("Ref",48),rep("Rob",44)))
  names(Design) = c("Recruitment","Mat_M","Scale","CompWt","Ref_Rob")
  row.names(Design) = 1:nrow(Design)

  dimnames(PPD)[[1]] = dimnames(Pow)[[1]] = paste0("Sim_",1:48)
  dimnames(PPD)[[2]] = dimnames(Pow)[[2]] = c(paste0("Ref_",1:48),paste0("Rob_",1:44))
  dimnames(PPD)[[3]] = Indices$Name
  dimnames(PPD)[[4]] = Yrs
  dimnames(Pow)[[3]] = c("E 1/2 Blim","W 1/2 Blim","EorW 1/2 Blim")

  Data_Design = rep("Index",nind)
  Defaults = list(Data = c(1,2,3,4,5,6,10,12,13,14), Yrs = 1:9)
  Obs = PPD[1,1,,]
  if(dummy_obs)  Obs[,dummycut:npy] = NA
  if(!dummy_obs) Obs[] = NA

  return(list(PPD=PPD, OM_Design=Design, Defaults = Defaults, Obs = Obs, Pow=Pow,
              First_Yr = First_Yr, Version = packageVersion('ABTMSE'),Sys.time=Sys.time()))

  # ECP_obj = list(PPD=PPD, OM_Design=Design, Defaults = Defaults, Obs = Obs, Pow=Pow, First_Yr = First_Yr, Version = packageVersion('ABTMSE'),Sys.time=Sys.time())


}

slp4<-function(y){
  x1<-1:length(y)
  x1 <- x1[!is.na(y)]
  y<-y[!is.na(y)]
  y<-log(y)
  mux<-mean(x1)
  muy<-mean(y,na.rm=T)
  SS<-sum((x1-mux)^2,na.rm=T)
  (1/SS)*sum((x1-mux)*(y-muy),na.rm=T)

}

Append_Comp_Ind = function(ECP_obj,
                           Igroups = list(ECI = c(1,2,4,5,6),
                                          WCI = c(3,13,14,10,12)),
                           Iwts = list(ECI = c(1.33,1.66,1.06,1.43,1.33),
                                       WCI = c(1.33,2.55,1.39,3.96,2.88))){

  ECP_nu = ECP_obj
  PPD = ECP_obj$PPD

  #rownames(ECP_obj$Obs)
  dims = dim(PPD)
  nsim = dims[1]
  nOM = dims[2]
  nI = dims[3]
  ny = dims[4]
  ng = length(Igroups)

  nu_obs = array(NA,c(ng,ny))
  nu_PPD = array(NA,c(nsim, nOM, ng, ny))
  dimnames(nu_PPD)[[3]] = rownames(nu_obs)= names(Igroups)
  for(i in c(1,2,4))dimnames(nu_PPD)[[i]]=dimnames(PPD)[[i]]
  colnames(nu_obs)=colnames(ECP_obj$Obs)

  for(gg in 1:ng){
    for(yy in 1:ny){
      nu_obs[gg,yy] = weighted.mean(ECP_obj$Obs[Igroups[[gg]],yy],Iwts[[gg]])
      for(i in 1:nsim){
        for(OM in 1:nOM){
          nu_PPD[i,OM,gg,yy] =  weighted.mean(PPD[i,OM,Igroups[[gg]],yy],Iwts[[gg]])
        }
      }
      cat(yy);cat(" - ")
    }
  }
  cat('\n')

  ECP_nu$PPD = abind(ECP_obj$PPD, nu_PPD, along=3)
  ECP_nu$Obs = rbind(ECP_nu$Obs, nu_obs )
  #ECP_nu$Defaults$Data = c(ECP_obj$Defaults$Data, dim(ECP_obj$Obs)[1] + ECP_obj$Defaults$Data)
  ECP_nu

}

Append_Index_Slopes = function(ECP_obj){

  ECP_nu = ECP_obj
  obslp = apply(ECP_obj$Obs[,1:4],1,slp4)

  obsmat = array(NA,dim(ECP_obj$Obs))
  obsmat[,4] = obslp
  dimnames(obsmat)[[2]] = dimnames(ECP_obj$Obs)[[2]]
  dimnames(obsmat)[[1]] = paste0("Slp_",dimnames(ECP_obj$Obs)[[1]])

  ECP_nu$Obs = rbind(ECP_obj$Obs,obsmat)

  predmat = array(NA, dim(ECP_obj$PPD))
  ny = dim(ECP_obj$PPD)[4]

  for(y in 3:ny){
    predmat[,,,y] = apply(ECP_obj$PPD[,,,(y-2):y],1:3,slp4)
    cat(paste(y,"- "))
  }
  cat("\n")
  dimnames(predmat) = dimnames(ECP_obj$PPD)
  dimnames(predmat)[[3]] = paste0("Slp_",dimnames(ECP_obj$PPD)[[3]])

  ECP_nu$PPD = abind(ECP_obj$PPD,predmat,along=3)
  ECP_nu$Defaults$Data = c(ECP_obj$Defaults$Data, dim(ECP_obj$Obs)[1] + ECP_obj$Defaults$Data)
  ECP_nu

}

Append_SOO = function(ECP_obj, dir = "C:/temp2/ECP2/",
                      OMdirs =paste0("C:/Users/tcar_/Dropbox/abft-mse/objects/OMs/",c(1:48,c(rep(c(1,2,4,5),8),c(3,6,3,6),rep(c(1,2,4,5),2)))),
                      OMIs = paste0("OMI_",c(1:48,c(rep(c(1,2,4,5),8),c(3,6,3,6),rep(c(1,2,4,5),2)))),
                      dummy_obs=T,MPind=NULL){

  files<-list.files(dir)
  isMSE<-grepl("MSE_",files)
  MSEref<-c(paste0("MSE_",1:48),paste0("MSE_R_",1:44))
  nOMs<-length(MSEref)
  fileref<-paste0(dir,MSEref,".rda")

  MSEobj= readRDS(fileref[1])
  if(length(MSEobj@PPD)==0) stop(paste0("There are no posterior predicted data in: ",fileref[1],". You need to run the MSEs with the argument returnMSE = T, see CMP dev guide"))
  if(is.null(MPind))MPind = length(MSEobj@PPD)

  Eastind = 1
  PPDi = MSEobj@PPD[[MPind]][[Eastind]]$Iobs # second tier is stock and that is identical in terms of index reporting - same indices are available to both stocks
  idims = dim(PPDi)

  lastYr = 2019
  nsim = idims[1]
  nind = 8 # WATL ac23 season#, GSL 123, NATL 123 EATL SOO by age class
  PPDy = idims[3]
  yind = (MSEobj@nyears+1):PPDy
  npy=length(yind)
  Yrs = lastYr + 1: npy
  PPD = array(NA,c(nsim,nOMs,nind,npy))
  #as = c(2,3,5,6) # WATL, GSL, NATL, EATL

  set.seed(1)
  sfInit(parallel=T,cpus=8)
  sfLibrary(ABTMSE)
  OMIex<-paste0("OMI_",1:48)
  sfExport(list=as.list(OMIex)) # OMs are the actual objects

  getSOOobs = function(OM,fileref,OMdirs,OMIs,MPind,nsim,npy,yind){
    #for(OM in 1:nOMs){
    ilogit = function(x)exp(x)/(1+exp(x))
    logit = function(x)log(x/(1-x))

    MSEobj<-readRDS(fileref[OM])
    out =  M3read(OMDir=OMdirs[OM])
    OMI = get(OMIs[OM])
    SOOobs = OMI@SOOobs
    names(SOOobs) =  c("a",  "y", "s", "r",   "N", "probE",  "SE", "Type", "wt")
    SOOpred = out$SOOpred
    CTA = MSEobj@CTA[MPind,,,,,] #  48   3 109   4   7

    err = array(NA,c(nsim,8,npy))
    j = 0
    for(ac in 2:3){
      for(ss in 1:4){
        SOOind = SOOobs[,1] == ac & SOOobs[,3] == ss & SOOobs[,4]==2
        SOOo = SOOobs[SOOind,6]
        SOOp = SOOpred[SOOind]
        SD = sd(SOOo-SOOp) # log sd
        #SD=1
        j=j+1
        err[,j,] = logit(CTA[,ac,yind,ss,2])*array(rnorm(nsim*npy,0,SD),c(nsim,npy))
        #print(paste(ac,"-",ss,"-",length(SOOp),"-",SD))


      }
    }
    #cat(OM);cat("-")
    err
  }

  errs = sfLapply(1:nOMs,getSOOobs,fileref=fileref,OMdirs=OMdirs,OMIs=OMIs,MPind=MPind,nsim=nsim,npy=npy,yind=yind)
  for(OM in 1:nOMs)  PPD[,OM,,] = errs[[OM]]#logit(CTA[,ac,yind,ss,2])*err # area 2 is WATL

  dimnames(PPD)[[1]] = paste0("Sim_",1:48)
  dimnames(PPD)[[2]] = c(paste0("Ref_",1:48),paste0("Rob_",1:44))
  dimnames(PPD)[[3]] = paste0("WATL_SOO_a",rep(2:3,each=4),"_q",rep(1:4,2))
  dimnames(PPD)[[4]] = Yrs

  ECP_nu = ECP_obj

  obsmat=PPD[1,1,,]
  if(dummy_obs)obsmat[,5:npy] = NA
  if(!dummy_obs)obsmat[] = NA

  ECP_nu$Obs = rbind(ECP_obj$Obs,obsmat)

  ECP_nu$PPD = abind(ECP_obj$PPD,PPD,along=3)
  ECP_nu$Defaults$Data = c(ECP_obj$Defaults$Data, dim(ECP_obj$Obs)[1] + 1:nind)
  ECP_nu

}
