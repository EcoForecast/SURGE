
## monte carlo forecast
```{r}

##load("")  ## load your fit RData
## rerun, save beta this time
## out.pres.driven

tau_add = out.pres.driven[,"tau_add"]
beta = out.pres.driven[,"beta"]
surge.IC = out.pres.driven[,"surge[2000]"]

Nmcmc = length(tau_add)
Nmc = 10  #number of MC iterations
MC = matrix(0,Nmc,length(time)) #row=iteration, col=location



## windspeed = tideHeight = Pressure = beta = rep(NA,Nmc)
## in the step above all the variables have to be included from the final equation calculating surge.
rand = sample.int(Nmcmc,Nmc,replace = TRUE)
for(i in 1:Nmc){
  
  m = rand[i]
  MC[i,1] = surge.IC[m]
  
  for(t in 2:nt){
    mu = ( wind[t-1]/beta[m] ) * (9.8/(MC[t-1]))*(pressure[t-1]/beta[m])
    MC[i,t] = dnorm(1,mu,tau_add[m])
  }
}
```
