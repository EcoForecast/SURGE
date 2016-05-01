tide_height = read.csv("~/SURGE/tide height data/2015NHA.txt", sep="", skip="10")
sorted_height<-tide_height[order(tide_height$yyyy.mm.dd),]
dates <- as.Date(sorted_height$yyyy.mm.dd)
date1 <- as.Date("2015-03-01")
date2 <- Sys.Date()-365


desired_rows <- which(dates >= date1 & dates <= date2)
height<-sorted_height[desired_rows, ]

library(chron)
tod<-chron(times=height$hh.mi.ssf)
dtod<-paste(dates[desired_rows] , tod)
x<- strptime(dtod, format="%Y-%m-%d %H:%M:%S")


surgeAndTide <-as.numeric(levels(height$f)[height$f])



library(rjags)

RandomWalk = "
model{

#### Data Model
for(i in 1:n){
surge[i] ~ dnorm(surgeAndTide[i],tau_obs)
}

#### Process Model
for(i in 2:n){
surge_final[i]~dnorm((surge_final[i-1]+surge[i])/2,tau_add)
}

#### Priors
surge_final[1] ~ dunif(-1,10)
tau_obs ~ dgamma(a_obs,r_obs)
tau_add ~ dgamma(a_add,r_add)
}
"

## MCMC
data <- list(surgeAndTide=surgeAndTide,n=length(surgeAndTide),a_obs=1,r_obs=1,a_add=1,r_add=1)
nchain = 3
init <- list()
for(i in 1:nchain){
  surge.samp = sample(surgeAndTide,length(surgeAndTide),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(surge.samp)),tau_obs=5/var(surge.samp))
}
j.model   <- jags.model (file = textConnection(RandomWalk),
                         data = data,
                         inits = init,
                         n.chains = 3)

jags.out   <- coda.samples (model = j.model,
                            variable.names = c("surge","tau_add","tau_obs"),
                            n.iter = 100)
summary(jags.out)

out.pres.random <- as.matrix(jags.out)
save(out.pres.random, file="out.pres.random.ofJAGS")


time = 1:length(surgeAndTide)
time.rng = c(1,length(time)) ## adjust to zoom in and out
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

ci <- apply(out.pres.random[,3:ncol(out.pres.random)],2,quantile,c(0.025,0.5,0.975))

jpeg(file="~/SURGE/web/Present_RandomWalk_Output.jpg")
plot(time,ci[2,],type='l',ylim=range(-3:11,na.rm=TRUE),ylab="Surge Height",xlim=time[time.rng])
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng) < 100){ 
  axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
}
ciEnvelope(time,ci[1,],ci[3,],col="red")
points(time,surgeAndTide,pch="+",cex=0.5)
dev.off()
#
