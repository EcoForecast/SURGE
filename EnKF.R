## Kalman Filter iterative forecast--one time step

tau_add = 1/sqrt(out.pres.driven[,"tau_add"])
tau_obs = mean(out.pres.driven[,"tau_obs"])
beta1 = out.pres.driven[,"beta1"]
beta2 = out.pres.driven[,"beta2"]
beta3 = out.pres.driven[,"beta3"]
surge.IC = out.pres.driven[,"surge_final[600]"] ## last time step

nstart = 600
Nmcmc = length(tau_add)
Nmc = 10  #number of MC iterations
nt = 192 ## two day forecast
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
  return(list(X.f=X.f,mu.a=mu.a,P.a=P.a))
}




