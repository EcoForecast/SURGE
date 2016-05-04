library(RCurl)
##Pull weather data
todaydate = (Sys.Date() - 365)
date = gsub("-","/",todaydate)
startdate = as.Date("2015/03/01")

firstpart = "https://www.wunderground.com/history/airport/EGFF/"
lastpart = "/DailyHistory.html?req_city=Cardiff&req_state=&req_statename=United+Kingdom&reqdb.zip=00000&reqdb.magic=1&reqdb.wmo=03717&format=1"


#weather_data = read.csv(paste(firstpart,date,lastpart)) 
weather_data = list()

load("weather_data.Rdata")
start = i
for (i in start:(todaydate-startdate)) {  
  
  print(i)
  date2 = gsub("-","/", startdate+i)
  met.url = paste(firstpart,date2,lastpart,sep="")
  # system(paste0("cd met; wget ",met.url))
  x = getURL(met.url)
  weather_data[[i+1]] = read.csv(text=x) 
  save(i,weather_data,file="weather_data.Rdata")
}

wind = unlist(sapply(weather_data,function(x){x$Wind.SpeedMPH},simplify = TRUE))
pres = unlist(sapply(weather_data,function(x){x$Sea.Level.PressureIn},simplify = TRUE))
pres[pres < 5] = NA
dateUTC = unlist(sapply(weather_data,function(x){x$DateUTC},simplify = TRUE))
day = strptime(sub(pattern = "<br />","",as.character(dateUTC)),format="%Y-%m-%d %T",tz="GMT")

##Pull tide data
tide_height = read.csv("~/SURGE/tide height data/2015NHA.txt", sep="", skip="10")
tideFormat<-as.numeric(levels(tide_height$f)[tide_height$f])
surgeFormat<-as.numeric(levels(tide_height$f2)[tide_height$f2])

surge<-(tideFormat+surgeFormat)
sorted_height<-tide_height[order(tide_height$yyyy.mm.dd),]
dates <- as.Date(sorted_height$yyyy.mm.dd)

desired_rows <- which(dates >= startdate & dates <= todaydate)
height<-sorted_height[desired_rows, ]


library(chron)
tod<-chron(times=height$hh.mi.ssf)
dtod<-paste(dates[desired_rows] , tod)
time<- strptime(dtod, format="%Y-%m-%d %H:%M:%S",tz="GMT")

surge <-surge[desired_rows]
tide<-as.numeric(levels(height$f)[height$f])

library(xts)
tide.ts = xts(tide,order.by = time)
surge.ts = xts(surge, order.by = time)
wind.ts = xts(wind,order.by = day)
pres.ts = xts(pres,order.by = day)
met = merge(tide.ts,surge.ts,wind.ts,pres.ts)
met$wind.ts <- na.approx(met$wind.ts,rule=2)
met$pres.ts <- na.approx(met$pres.ts,rule=2)
data_times <- met[index(tide.ts),]

pressure<-as.numeric(data_times$pres)
wind<-as.numeric(data_times$wind)


library(rjags)


SurgeModel = "
model{

#### Data Model
for(i in 1:n){
surge_data[i] ~ dnorm(surge_final[i],tau_obs)
}
#for(i in 1:n){
#pressure.effect[i] ~ dnorm(pressure_data[i],tau_pres)
#}
#for(i in 1:n){
#wind.effect[i] ~ dnorm(wind_data[i],tau_wind)
#}

#### Process Model
for(i in 2:n){
  surge[i]<- tide_data[i] + beta1*(surge_final[i-1]-tide_data[i-1]) + beta2*pressure_data[i] + beta3*wind_data[i]
                                                 ## ^ modified from tide[i-1]
  surge_final[i]~dnorm(surge[i],tau_add)
}

#### Priors
surge_final[1] ~ dunif(-1,10)
tau_obs ~ dgamma(a_obs,r_obs)
tau_add ~ dgamma(a_add,r_add)
#tau_pres ~ dgamma(1,.1)
#tau_wind ~ dgamma(1,.1)
#beta ~ dnorm(25,0.16)
beta1 ~ dnorm(0,0.001)
beta2 ~ dnorm(0,0.001)
beta3 ~ dnorm(0,0.001)
}
"

## MCMC
nd = 1:600
data <- list(surge_data =surge[nd],tide_data=tide[nd],pressure_data=pressure[nd],wind_data=wind[nd], n=length(nd),a_obs=1,r_obs=1,a_add=1,r_add=1)
nchain = 3
init <- list()
for(i in 1:nchain){
  surge.samp = sample(surge,length(surge),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(surge.samp)),tau_obs=5/var(surge.samp),tau_pres=1/var(surge.samp),tau_wind=1/var(surge.samp))
}
j.model   <- jags.model (file = textConnection(SurgeModel),
                         data = data,
                         inits = init,
                         n.chains = 3)

jags.out   <- coda.samples (model = j.model,
#                            variable.names = c("surge_final","tau_add","tau_obs","tau_pres","tau_wind","beta"),
                            variable.names = c("surge_final","tau_add","tau_obs","beta1","beta2","beta3"),
                            n.iter = 1000)
##summary(jags.out)

out.pres.driven <- as.matrix(jags.out)
save(out.pres.driven, file="out.pres.driven.ofJAGS")


time = nd
time.rng = c(1,length(time)) ## adjust to zoom in and out
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

ci <- apply(out.pres.driven[,3:ncol(out.pres.driven)],2,quantile,c(0.025,0.5,0.975))
ci<-ci[,1:length(time)]

jpeg(file="~/SURGE/web/Present_DrivenWalk_Output.jpg")
plot(time,ci[2,],type='l',ylim=range(-1:10,na.rm=TRUE),ylab="Surge Height",xlim=time[time.rng],main="Calibration Timeseries")
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng) < 100){ 
  axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
}
ciEnvelope(time,ci[1,],ci[3,],col="red")
points(time,surge[nd],pch="+",cex=0.5)
dev.off()
