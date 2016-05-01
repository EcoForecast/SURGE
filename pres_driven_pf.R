install.packages(smfsb)
library(smfsb)

ne = 200
## number of ensembles

ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

col.alpha <- function(col,alpha=1){
  rgb = col2rgb(col)
  rgb(rgb[1],rgb[2],rgb[3],alpha*255,maxColorValue=255)
}

wtd.quantile <- function(x,wt,q){ 
  ord <- order(x)
  wstar <- cumsum(wt[ord])/sum(wt)
  qi <- findInterval(q,wstar); qi[qi<1]=1;qi[qi>length(x)]=length(x)
  return(x[ord[qi]])
}


model = out.pres.driven
model.ci = apply(model,2,quantile,c(0.025,0.5,0.975))
data= data$surge_data
data.sd = rep(1.767434, length(data)) ## sd of tide_height
Msel = 1:ncol(model.ci)

## calculate the cumulative likelihoods
## to be used as PF weights

Surgelike = array(NA,dim(model))

sel=1:ncol(model.ci)
for(i in 1:nrow(model)){
  Surgelike[i,] = dnorm(model[i,],data[sel],data.sd[sel],log=TRUE)  ## calculate log likelihoods
  Surgelike[i,is.na(Surgelike[i,])] = 0       ## missing data as weight 1; log(1)=0
  Surgelike[i,] = exp(cumsum(Surgelike[i,]))  ## convert to cumulative
}

## Non-resampling Particle Filter
## calculation of CI

nobs = ncol(Surgelike)  ## number of observations
Surgepf = matrix(NA,3,nobs)
wbar = apply(Surgelike,2,mean) ## full of NAs--why?
for(i in 1:nobs){
  Surgepf[,i] = wtd.quantile(model[,i],Surgelike[,i]/wbar[i],c(0.025,0.5,0.975))
}

## plot original ensemble and PF with data
plot(time[Msel],model.ci[2,],ylim=range(c(range(model.ci),range(data,na.rm=TRUE))),
     type='n',ylab="Surge",xlab="Time")
ciEnvelope(time[Msel],model.ci[1,],model.ci[3,],col=col.alpha("lightGrey",0.5))
ciEnvelope(time[Msel],Surgepf[1,],Surgepf[3,],col=col.alpha("lightBlue",0.5))
points(time,data)    
##for(i in 1:length(data)){
##  if(!is.na(QC[i])){
##    lines(rep(time[i],2),LAIr[i]+c(-1,1)*data.sd[i])
##  }
##}
