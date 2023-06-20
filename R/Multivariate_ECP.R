# Multivariate ECP

mahalanobis_robust<-function (x, center, cov, inverted = FALSE) {

  x <- if (is.vector(x)){
    matrix(x, ncol = length(x))
  } else {
    as.matrix(x)
  }
  if (!identical(center, FALSE)) x <- sweep(x, 2L, center)

  invcov <- corpcor::pseudoinverse(cov)
  setNames(rowSums(x %*% invcov * x), rownames(x))

}

getsegment<-function(densobj,thresh,lower=T){
  if(lower){
    cond<-densobj$x<thresh
  }else{
    cond<-densobj$x>thresh
  }

  xs<-c(0,densobj$y[cond],0)
  ys<-densobj$x[cond]
  ys<-c(ys[1],ys,ys[length(ys)])

  list(x=xs,y=ys)
}


get_pred_sim=function(ECP_obj,OMind,Iind,powind=1){

  Obs = ECP_obj$Obs[Iind,,drop=F]
  PPD = ECP_obj$PPD[,OMind,Iind,]
  nsim = dim(PPD)[1]
  nd = dim(PPD)[3]
  ny = dim(PPD)[4]
  nOM = length(OMind)
  varfunc = function(x)sd(x)>0.00001 & !is.na(sd(x))
  add2calc = apply(PPD[,1,,],2:3,varfunc)&!is.na(Obs)
  ys = array(rep(1:ny,each=nd),c(nd,ny))[add2calc]
  ds = array(rep(1:nd,ny),c(nd,ny))[add2calc]
  ndat=sum(add2calc)
  preds = array(NA,c(nsim*nOM,ndat))
  obs = rep(NA,ndat)
  for(i in 1:ndat){
    normswitch = any(PPD[,,ds[i],ys[i]]<0)
    if(normswitch){
      preds[,i] = as.vector(PPD[,,ds[i],ys[i]])
      obs[i] =Obs[ds[i],ys[i]]
    }else{
      preds[,i] = log(as.vector(PPD[,,ds[i],ys[i]]))
      obs[i] =log(Obs[ds[i],ys[i]])

    }
  }

  pows=as.vector(ECP_obj$Pow[,OMind,powind])

  Inams = 1:length(dimnames(PPD)[[3]])
  nams = paste0(Inams[ds],"_",ys+ECP_obj$First_Yr-1)

  yrs= min(ys+ECP_obj$First_Yr-1):max(ys+ECP_obj$First_Yr-1)
  list(preds=preds,obs=obs,nams=nams,pows=pows,yrs=yrs)
}

get_pred_sim_yr=function(ECP_obj,OMind,Iind,yind=1:6,powind=1){
  PPD = ECP_obj$PPD[,OMind,Iind,yind,drop=F]
  Obs = ECP_obj$Obs[,yind]
  nsim = dim(PPD)[1]
  nd = dim(PPD)[3]
  ny = dim(PPD)[4]
  nOM = length(OMind)
  varfunc = function(x)sd(x)>0.00001 & !is.na(sd(x))
  add2calc = apply(PPD[,1,,,drop=F],3:4,varfunc)
  ys = array(rep(1:ny,each=nd),c(nd,ny))[add2calc]
  ds = array(rep(1:nd,ny),c(nd,ny))[add2calc]
  ndat=sum(add2calc)
  preds = array(NA,c(nsim*nOM,ndat))
  obs = rep(NA,ndat)
  for(i in 1:ndat){
    normswitch = any(PPD[,,ds[i],ys[i]]<0)
    if(normswitch){
      preds[,i] = as.vector(PPD[,,ds[i],ys[i]])
      obs[i] =Obs[ds[i],ys[i]]
    }else{
      preds[,i] = log(as.vector(PPD[,,ds[i],ys[i]]))
      obs[i] =log(Obs[ds[i],ys[i]])

    }
  }
  pows=as.vector(ECP_obj$Pow[,OMind,powind])
  Inams = 1:length(dimnames(PPD)[[3]])
  nams = paste0(Inams[ds],"_",ys+ECP_obj$First_Yr-1)
  yrs=ECP_obj$First_Yr+yind-1
  list(preds=preds,obs=obs,nams=nams,pows=pows,yrs=yrs)
}

# alpha=0.05; OMind = 1:48
plot_mdist<-function(ECP_obj,alp=0.05, OMind = 1:48, Iind=NULL, yind=1:6){

  if(is.null(Iind))Iind=ECP_obj$Defaults$Data

  out=get_pred_sim(ECP_obj,OMind,Iind)

  preds=out$preds
  obs=out$obs

  nullcov<-cov(preds)
  nullm<-apply(preds,2,mean,na.rm=T)

  dist<-mahalanobis_robust(x=obs, center=nullm, cov=nullcov)
  dists<-mahalanobis_robust(x=preds, center=nullm, cov=nullcov)

  xlim=c(min(dist*0.95,quantile(dists,0.0005)),max(dist*1.05,quantile(dists,0.9995)))
  par(mfrow=c(1,1),mai=c(0.7,0.7,0.5,0.05))
  dens<-density(dists,from=0,to=xlim[2])

  plot(dens,xlab="",main="",col='white',ylab="",xlim=xlim); grid()
  lines(dens,xlab="",main="",col='blue')
  thresh<-quantile(dists,1-alp)
  abline(v=thresh,lty=2,lwd=2,col='orange')

  cex = 0.85
  text(thresh+0.5*(xlim[2]-thresh),max(dens$y)*0.9,paste0("V"),cex=cex,col="orange")
  text(dist+0.4*(xlim[2]-dist),max(dens$y)*0.97,"D (observed data)",font=2,cex=cex,col="black")

  mtext("Multivariate distance",1,line=2)
  mtext("Density",2,line=2)
  subdens<-getsegment(dens,thresh,lower=F)
  polygon(y=subdens$x,x=subdens$y,col="#0000ff95",border=NA)

  leg<-"Outlier detected (D > V)"
  lcol<-"Red"
  if(dist<thresh){
    leg<-"Outlier not detected (D < V)"
    lcol="darkgreen"
  }

  abline(v=dist,lwd=2,col="black")

  legend('topleft',legend=c(paste0("Critical value = ",round(thresh,2)),
                            paste0("Test statistic = ",round(dist,2))),
         text.col=c('orange','black'),cex=0.8,bty='n')


  cex = 0.85
  legend('right',legend=paste("Type I err = ",round(alp*100,1),"%"),
         ,
         fill="#0000ff95", border="#0000ff95",bty='n',cex=cex)

  mtext(leg,col=lcol,3,line=0.2,font=2)

}

encircle2<-function (x, y, col = "red", perc = 0.05, xrange = NA,
                     yrange = NA, log = F, lty = 1, lwd = 1, labels = NA, drawlabels = F)
{
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package \"MASS\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  nsim <- length(x)
  if (log) {
    x <- log(x)
    y <- log(y)
  }
  if (is.na(xrange[1]))
    xrange <- range(x)
  if (is.na(yrange[1]))
    yrange <- range(y)
  if (log) {
    xrange[xrange == 0] <- 0.01
    yrange[yrange == 0] <- 0.01
    xrange <- log(xrange)
    yrange <- log(yrange)
  }
  kerneld <- MASS::kde2d(x, y, n = 100, lims = c(xrange, yrange))
  pp <- array()
  for (i in 1:nsim) {
    z.x <- max(which(kerneld$x < x[i]))
    z.y <- max(which(kerneld$y < y[i]))
    pp[i] <- kerneld$z[z.x, z.y]
  }
  confidencebound <- quantile(pp, perc, na.rm = TRUE)
  if (log) {
    kerneld$x <- exp(kerneld$x)
    kerneld$y <- exp(kerneld$y)
  }
  if (is.na(labels)) {
    contour(kerneld, levels = confidencebound, col = col,
            add = TRUE, drawlabels = drawlabels, lty = lty, lwd = lwd)
  }
  else {
    contour(kerneld, levels = confidencebound, col = col,
            add = TRUE, drawlabels = T, lty = lty, lwd = lwd,
            labels = labels)
  }
}



plot_mdist_pow<-function(ECP_obj,alp=0.05, OMind = 1:48,
                         Iind=NULL,yind=1:6,powind=1,
                         byyr=F,add=F,labs=T,res=T, ploty=T){

  if(is.null(Iind))Iind=ECP_obj$Defaults$Data

  if(!byyr) out=get_pred_sim(ECP_obj,OMind,Iind,powind=powind)
  if(byyr) out=get_pred_sim_yr(ECP_obj,OMind,Iind,yind,powind=powind)

  preds=out$preds
  obs=out$obs
  pows=out$pows
  yrs=out$yrs

  nullcov<-cov(preds)
  nullm<-apply(preds,2,mean,na.rm=T)

  if(!byyr) dist<-mahalanobis_robust(x=obs, center=nullm, cov=nullcov)

  dists<-mahalanobis_robust(x=preds, center=nullm, cov=nullcov)

  xlim=c(min(quantile(dists,0.0005)),max(quantile(dists,0.9995)))
  if(!add)par(mfrow=c(1,1),mai=c(0.65,0.65,0.5,0.05))
  dens_null<-density(dists[!pows],from=0,to=xlim[2])
  dens_alt<-density(dists[pows],from=0,to=xlim[2])

  thresh<-quantile(dists[!pows],1-alp)
  beta<-mean(dists[pows]<thresh)*100

  if(ploty){
    ylim=c(0,max(dens_null$y,dens_alt$y))
    plot(dens_null,xlab="",main="",col='white',ylab="",xlim=xlim,ylim=ylim); grid()
    lines(dens_null,xlab="",main="",col='blue',lwd=2)
    lines(dens_alt,xlab="",main="",col='red',lwd=2)

    abline(v=thresh,lty=2,lwd=2,col="orange")
    cols = c("#0000ff95","#ff000095","white","white")

    cex = 0.85
    legend('right',legend=c(paste("Type I err = ",round(alp*100,1),"%"),
                            paste("Type II = ",round(beta,1),"%"),
                            paste("Tot Err. = ",round(beta+alp*100,1),"%"),
                            paste("Power = ",round(100-beta,1),"%")),
           fill=cols, border=cols,bty='n',cex=cex)
    legend('left',legend=c("Null","Alternate"),lwd=2,col=c("blue","red"),bty='n',cex=0.85)

    if(labs)mtext("Multivariate distance",1,line=2)
    if(labs)mtext("Density",2,line=2)


    subdens_null<-getsegment(dens_null,thresh,lower=F)
    polygon(y=subdens_null$x,x=subdens_null$y,col=cols[1],border=NA)

    subdens_alt<-getsegment(dens_alt,thresh,lower=T)
    polygon(y=subdens_alt$x,x=subdens_alt$y,col=cols[2],border=NA)

    if(!byyr){
      leg<-"Outlier detected (D > V)"
      lcol<-"purple"
        if(dist<thresh){
          leg<-"Outlier not detected (D < V)"
          lcol="darkgreen"
        }
    }

    adj = 0.07*(xlim[2]-thresh)
    text(thresh+adj,ylim[2]*0.9,"V",font=2,cex=cex,col="orange")
    if(!byyr) text(dist+adj, ylim[2]*0.97,"D",font=2,cex=cex,col=lcol)
    if(!byyr) abline(v=dist,lwd=2,col=lcol,lty=2)

    if(!byyr){
      legend('topleft',legend=c(paste0("Crit. value = ",round(thresh,2)),
                                paste0("Test stat. = ",round(dist,2))),
             text.col=c('orange',lcol),cex=cex,bty='n')
    }else{
      legend('topleft',legend=paste0("Crit. value = ",round(thresh,2)),
             text.col=c('orange'),cex=cex,bty='n')
    }

    legend('topright',legend=c(paste("n data =",ncol(preds)),paste("Yrs:",min(yrs),"-",max(yrs))),
           bty='n',cex=cex)
    if(!byyr&res)mtext(leg,col=lcol,3,line=0.2,font=2)
  } # end of if ploty

  c(alp = alp*100, beta = beta, pow = 100-beta, toterr = beta+alp*100)
}
