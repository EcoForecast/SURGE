library(rjags)
beta=22535 ##check number, seems ridiculous
y=y ##pull from tide data
wind.effect=wind_##something ##pull from wind data 

SurgeHeight = "
model{

#### Data Model
for(i in 1:n){
y[i] ~ dnorm(x[i],tau_obs)
}

for(i in 1:n){
wind.effect[i] ~ dnorm(wind[i],tau_wind)
}

for(i in 1:n){
pressure_effect[i] ~dnorm(pressure[i], tau_presssure)
}

#### Process Model
for(i in 2:n){
total <- x[i] + (beta * wind.effect[i] ) * ((9.8/y[i])*(25 + pressure_effect[i])) + tau_add
x[i]~dnorm(x[i-1],tau_add)##change to how surge changes
}

#### Priors
x[1] ~ dunif(-1,10)
tau_obs ~ dgamma(a_obs,r_obs) ##observation error for tide height
tau_add ~ dgamma(a_add,r_add) ##process error
tau_wind ~ dgamma(1,1) ##obervation error for wind measurements
wind[1] ~ dnorm(0,3) ##uninformative prior for wind ##3 makes sense w/ units?
pressure[1] ~ dunif(0,5) ##uninformative prior for pressure
##what are pressure units, does prior make sense
tau_pressure ~dgamma(1,1) ##obs error for pressure measurement
##all taus unknown, so went with same as given taus
}
"
data <- list(y=y,n=length(y),a_obs=1,r_obs=1,a_add=1,r_add=1)
nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(y,length(y),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(y.samp)),tau_obs=5/var(y.samp))
}
j.model   <- jags.model (file = textConnection(SurgeHeight),
                         data = data,
                         inits = init,
                         n.chains = 3)
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("tau_add","tau_obs"),
                            n.iter = 100)
plot(jags.out)