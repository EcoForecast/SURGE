tau_add = 1/sqrt(out.pres.driven[,"tau_add"])
tau_obs = mean(out.pres.driven[,"tau_obs"])
beta1 = out.pres.driven[,"beta1"]
beta2 = out.pres.driven[,"beta2"]
beta3 = out.pres.driven[,"beta3"]
surge.IC = out.pres.driven[,"surge_final[600]"] ## last time step

##nstart = 600
Nmcmc = length(tau_add)
Nmc = 1000  #number of MC iterations
nt = 96 ## two day forecast
##MC = matrix(NA,Nmc,nt) #row=iteration, col=location

Xf = MC[,1]
Y = surge[nstart+1]

EnKF <- function(Xf,Y,t){
  
  ##Analysis
  mu.f = mean(Xf)
  P.f = 1/var(Xf)
  P.a = P.f + tau_obs
  mu.a = (P.f*mu.f + tau_obs*Y)/P.a
  
  ## Forecast
  X.a = rnorm(Nmc,mu.a,1/sqrt(P.a))
  mu = tide[nstart+t] + beta1[rand]*(X.a-tide[nstart+t-1]) + beta2[rand]*pressure[nstart+t] + beta3[rand]*wind[nstart+t]
  X.f = rnorm(Nmc,mu,tau_add[rand])
  ##return(list(X.f=X.f,mu.a=mu.a,P.a=P.a))
  
  output=list(X.f=X.f,mu.a=mu.a,P.a=P.a)
  return(output)
}
EnKF_output=matrix(data=NA,nrow=nt,ncol=(Nmc+2))
#colnames(EnKF_output) <- c("X.f[1]","X.f[2]","X.f[3]","X.f[4]","X.f[5]","X.f[6]","X.f[7]","X.f[8]","X.f[9]","X.f[10]","mu.a","P.a")
for (t in 1:nt){
  fx = EnKF(Xf,surge[nstart+t],t)
  ##EnKF_output[[t+1]] = output
  EnKF_output[t,]=t(as.matrix(unlist(fx)))
  ##first Nmc are the X.f,next two are mu.a and P.a  
  print(t)
  save(t,EnKF_output,file="EnKF_Output.Rdata")
  
}

time=c((nstart+1):(nstart+nt))
ci.a = matrix(NA,3,nt) 
ci.a[2,] = EnKF_output[,11]
ci.a[1,] = EnKF_output[,11]-1.96*EnKF_output[,12]
ci.a[3,] = EnKF_output[,11]+1.96*EnKF_output[,12]

ci.f <- apply(EnKF_output[,1:Nmc],1,quantile,c(0.025,0.5,0.975))

jpeg(file="~/SURGE/web/EnKF_Forecast.jpg")
plot(time,EnKF_output[,11],ylim=range(ci,na.rm=TRUE),xlim=c(600,700),type='n',main="24-hour Forecast")
ciEnvelope(time,ci.a[1,],ci.a[2,],col="lightBlue")
ciEnvelope(time,ci.f[1,],ci.f[2,],col="lightGrey")
lines(time,EnKF_output[,11],col="blue")
legend(600,7,legend=c("Analysis CI","Forecast CI", "Forecast"),lty=c(1,1),lwd=c(2.5,2.5),col=c("lightBlue","lightGrey","blue"))


jpeg(file="~/SURGE/web/EnKF_Forecast_Process_Error.jpg")
plot(time,tau_add[(nstart+1):(nstart+nt)],ylim=range(tau_add[(nstart+1):(nstart+nt)],na.rm=TRUE),ylab="Process Error in m",xlim=range(time,na.rm=TRUE),xlab="Time",type='l',main="Forecast Process Error")
dev.off()

obs = out.pres.driven[,"tau_obs"]
jpeg(file="~/SURGE/web/EnKF_Forecast_Obs_Error.jpg")
plot(time,obs[(nstart+1):(nstart+nt)],ylim=range(obs[(nstart+1):(nstart+nt)],na.rm=TRUE),ylab="Observation Error",xlim=range(time,na.rm=TRUE),xlab="Time",type='l',main="Forecast Observation Error")
dev.off()

total_error = rep(NA, nt)
for (i in 1:nt){
  total_error[i] = tau_add[nstart+i] + obs[nstart+i]
}

jpeg(file="~/SURGE/web/EnKF_Forecast_Error_Partition.jpg")
plot(time, (tau_add[(nstart+1):(nstart+nt)]/total_error),ylab="Error % of Total",xlab="Time",ylim = c(0,1),type='l', col="blue",main="Forecast Error Partition")
lines(time, (obs[(nstart+1):(nstart+nt)]/total_error),type='l',col="red")
legend(600,0.6,legend=c("Process Error","Observation Error"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red"))
dev.off()

##move counter for next forecast
nstart=nstart+nt