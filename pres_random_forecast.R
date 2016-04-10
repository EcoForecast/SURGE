##messy but pulls code, need to streamline
tide_height = read.csv("~/SURGE/tide height data/2015NHA.txt", sep="", skip="10")
tideFormat<-as.numeric(levels(tide_height$f)[tide_height$f])
surgeFormat<-as.numeric(levels(tide_height$f2)[tide_height$f2])
less.surge<-(tideFormat+surgeFormat)
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
tide <-less.surge[desired_rows]

##get ready to model!
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
##creates vectors of proper length
time <- 1:length(tide)
forecast.time <- 1:(length(tide)+672)
none <- rep(NA, 672)
tide.fore<- append(tide, none, after = length(tide))##creates vector with data, then NAs filled in after

## MCMC
data.fore <- list(tide=tide.fore,n=length(forecast.time),a_obs=1,r_obs=1,a_add=1,r_add=1)
nchain = 3
init.fore <- list()
for(i in 1:nchain){
  tide.samp.fore = sample(tide,forecast.time,replace=TRUE)
  init.fore[[i]] <- list(tau_add=1/var(diff(tide.samp.fore)),tau_obs=5/var(tide.samp.fore))
}
j.model.fore   <- jags.model (file = textConnection(RandomWalk),
                         data = data.fore,
                         inits = init.fore,
                         n.chains = 3)

jags.out.fore   <- coda.samples (model = j.model.fore,
                            variable.names = c("surge","tau_add","tau_obs"),
                            n.iter = 100
                            )

##summary(jags.out.fore, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))
##do we need this, better way?
out.pres.random.fore <- as.matrix(jags.out.fore)
##save(out.pres.random.fore, file="out.pres.random.ofJAGS")


time.rng.fore = c(1,length(time.fore)) ## adjust to zoom in and out

ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

ci <- apply(out.pres.random.fore[,3:ncol(out.pres.random.fore)],2,quantile,c(0.025,0.5,0.975))

##final plat, it works!
jpeg(file="~/SURGE/web/Present_RandomWalk_Output_Forecast.jpg")
plot(time.fore,ci[2,],type='l',ylim=range(tide,na.rm=TRUE),ylab="Surge Height",xlim=time.fore[time.rng.fore])
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng.fore) < 100){ 
  axis.Date(1, at=seq(time.fore[time.rng.fore[1]],time.fore[time.rng.fore[2]],by='month'), format = "%Y-%m")
}
ciEnvelope(time.fore,ci[1,],ci[3,],col="red")
points(time,tide,pch="+",cex=0.5)
dev.off()
