## monte carlo forecast
```{r}

##load("")  ## load your fit RData
## rerun, save beta this time
## out.pres.driven

tau_add = 1/sqrt(out.pres.driven[,"tau_add"])
beta1 = out.pres.driven[,"beta1"]
beta2 = out.pres.driven[,"beta2"]
beta3 = out.pres.driven[,"beta3"]
surge.IC = out.pres.driven[,"surge_final[600]"] ## last time step

nstart = 600
Nmcmc = length(tau_add)
Nmc = 1000  #number of MC iterations
nt = 96 ## one day forecast
MC = matrix(NA,Nmc,nt) #row=iteration, col=location

time=c((nstart+1):(nstart+nt))


##Wind = Tide_height = Pressure = Beta = rep(NA,Nmc) ## what is this for??
## in the step above all the variables have to be included from the final equation calculating surge.
rand = sample.int(Nmcmc,Nmc,replace = TRUE)
for(i in 1:Nmc){
  
  print(i)
  m = rand[i]
  MC[i,1] = surge.IC[m]
  
  for(t in 2:nt){
    #mu = (wind[t-1]/beta[m]) * (9.8/(MC[i,t-1]))*(pressure[t-1]/beta[m])
    mu = tide[nstart+t] + beta1[m]*(MC[i,t-1]-tide[nstart+t-1]) + beta2[m]*pressure[nstart+t] + beta3[m]*wind[nstart+t]
    MC[i,t] = rnorm(1,mu,tau_add[m])
  }
}

## MC mean

#meanMC = rep(NA,nt-1) ## first column is beta
#for (j in 2:nt) {  
#  meanMC[j-1] = mean(MC[,j])
#}

ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}
ci <- apply(MC,2,quantile,c(0.025,0.5,0.975))
#ci<-ci[,1:length(time)]

jpeg(file="~/SURGE/web/Present_MC_Output.jpg")
plot(time,ci[2,], xlab="Time", ylab="Surge Height in m",type='l',ylim=range(ci),main="Monte Carlo Simulation")
ciEnvelope((nstart+1):(nstart+nt),ci[1,],ci[3,],col="lightblue")
lines(time,ci[2,])
points(time,surge[(nstart+1):(nstart+nt)],pch="+",cex=0.5)
legend(600,7,legend=c("CI","Forecast", "Observed"),lty=c(1,1,NA),lwd=c(2.5,2.5),col=c(1,"lightblue", 1),pch=c("","","+"))
dev.off()


```