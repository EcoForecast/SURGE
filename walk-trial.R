tide_height = read.csv("~/SURGE/tide height data/2015NHA.txt", sep="", skip="10")
tide<-as.numeric(levels(height$f)[height$f])
surge<-as.numeric(levels(height$f2)[height$f2])
less.surge<-(tide+surge)
sorted_height<-tide_height[order(tide_height$yyyy.mm.dd),]
dates <- as.Date(sorted_height$yyyy.mm.dd)
date1 <- as.Date("2015-01-01")
date2 <- Sys.Date()-365
desired_rows <- which(dates >= date1 & dates <= date2)
height<-sorted_height[desired_rows, ]
library(chron)
tod<-chron(times=height$hh.mi.ssf)
dtod<-paste(dates[desired_rows] , tod)
x<- strptime(dtod, format="%Y-%m-%d %H:%M:%S")
y <-less.surge[desired_rows]

library(rjags)
nchain = 3

y.1=as.numeric(y)
data=list(n=length(desired_rows), z=y.1)
Surge.Height="
model{
  
  #### Process Model
  for(i in 2:n){
    height.new[i] <-total.height[i] + tau_obs
    total.height[i] ~ dnorm(z,1)
  }
 
  ####Prior
   tau_obs ~ dnorm(1,2)
}"

init <- list()
for(i in 1:nchain){
  surge.samp = sample(y,length(y),replace=TRUE)
  init[[i]] <- list(tau_obs=5)
}
j.model   <- jags.model (file = textConnection(Surge.Height),
                         data = data,
                         inits = init,
                         n.chains = 3)
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("tau_obs"),
                            n.iter = 1000)
plot(jags.out)