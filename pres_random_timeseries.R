library(rjags)


RandomWalk = "
model{

#### Data Model
for(i in 1:n){
tide[i] ~ dnorm(surge[i],tau_obs)
}

#### Process Model
for(i in 2:n){
surge[i]~dnorm(surge[i-1],tau_add)
}

#### Priors
surge[1] ~ dunif(-1,10)
tau_obs ~ dgamma(a_obs,r_obs)
tau_add ~ dgamma(a_add,r_add)
}
"

## MCMC
data <- list(tide=tide,n=length(tide),a_obs=1,r_obs=1,a_add=1,r_add=1)
nchain = 3
init <- list()
for(i in 1:nchain){
  tide.samp = sample(tide,length(tide),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(tide.samp)),tau_obs=5/var(tide.samp))
}
j.model   <- jags.model (file = textConnection(RandomWalk),
                         data = data,
                         inits = init,
                         n.chains = 3)

jags.out   <- coda.samples (model = j.model,
                            variable.names = c("surge","tau_add","tau_obs"),
                            n.iter = 10000,
                            )
summary(jags.out)

out.pres.random <- as.matrix(jags.out)
save(out.pres.random, file="out.pres.random.ofJAGS")


time = 1:length(tide)
time.rng = c(1,length(time)) ## adjust to zoom in and out
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}
out <- as.matrix(jags.out)
ci <- apply(out[,3:ncol(out)],2,quantile,c(0.025,0.5,0.975))

jpeg(file="~/SURGE/web/Present_RandomWalk_Output.jpg")
plot(time,ci[2,],type='l',ylim=range(tide,na.rm=TRUE),ylab="Surge Height",xlim=time[time.rng])
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng) < 100){ 
  axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
}
ciEnvelope(time,ci[1,],ci[3,],col="red")
points(time,tide,pch="+",cex=0.5)
dev.off()


