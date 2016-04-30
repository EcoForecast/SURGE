
## monte carlo forecast
```{r}

##load("")  ## load your fit RData
## rerun, save beta this time
## out.pres.driven

tau_add = out.pres.driven[,"tau_add"]
beta = out.pres.driven[,"beta"]
surge.IC = out.pres.driven[,"surge_final[5760]"] ## last time step

Nmcmc = length(tau_add)
Nmc = 10  #number of MC iterations
MC = matrix(0,Nmc,length(time)) #row=iteration, col=location


nt = 192 ## two day forecast

Wind = Tide_height = Pressure = Beta = rep(NA,Nmc) ## what is this for??
## in the step above all the variables have to be included from the final equation calculating surge.
rand = sample.int(Nmcmc,Nmc,replace = TRUE)
for(i in 1:Nmc){
  
  print(i)
  m = rand[i]
  MC[i,1] = surge.IC[m]
  
  for(t in 2:nt){
    mu = (wind[t-1]/beta[m]) * (9.8/(MC[t-1]))*(pressure[t-1]/beta[m])
    MC[i,t] = dnorm(1,mu,tau_add[m])
  }
}

## MC mean

meanMC = rep(NA,nt)
for (j in 1:nt) {  
  meanMC[j] = mean(MC[,j])
}

ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}
ci <- apply(meanMC[,3:ncol(meanMC)],2,quantile,c(0.025,0.5,0.975))
ci<-ci[,1:length(time)]

plot(meanMC, xlab="Time", type='l')
ciEnvelope(time,ci[1,],ci[3,],col="lightblue")
points(time,surge,pch="+",cex=0.5)
```
