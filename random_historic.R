library(rjags)
beta=22535 ##check number, seems rediculus
y=y ##pull from tide data
wind.effect=wind_##something ##pull from wind data 

RandomWalk = "
model{

#### Data Model
for(i in 1:n){
y[i] ~ dnorm(x[i],tau_obs)
}

for(i in 1:n){
wind.effect[i] ~ dnorm(0,1)
}

#### Process Model
for(i in 2:n){
##final total will be???= total <- x[i] + beta*wind.effect[i]*((9.8/y[i])*(25+pressure))
total <- x[i] + wind.effect[i]
x[i]~dnorm(x[i-1],tau_add)
}

#### Priors
x[1] ~ dunif(-1,10)
tau_obs ~ dgamma(a_obs,r_obs)
tau_add ~ dgamma(a_add,r_add)
}
"
data <- list(y=y,n=length(y),a_obs=1,r_obs=1,a_add=1,r_add=1)
nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(y,length(y),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(y.samp)),tau_obs=5/var(y.samp))
}
j.model   <- jags.model (file = textConnection(RandomWalk),
                         data = data,
                         inits = init,
                         n.chains = 3)
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("tau_add","tau_obs"),
                            n.iter = 100)
plot(jags.out)