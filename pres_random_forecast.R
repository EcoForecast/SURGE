library(RCurl)
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

day = strptime(sub(pattern = "<br />","",as.character(dateUTC)),format="%Y-%m-%d %T")

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
time<- strptime(dtod, format="%Y-%m-%d %H:%M:%S",tz="GMT")


tide <-less.surge[desired_rows]


library(xts)
tide.ts = xts(tide,order.by = time)
wind.ts = xts(wind,order.by = day)
pres.ts = xts(pres,order.by = day)
met = merge(tide.ts,wind.ts,pres.ts)
met$wind.ts <- na.approx(met$wind.ts,rule=2)
met$pres.ts <- na.approx(met$pres.ts,rule=2)

data_times <- met[index(tide.ts),]

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
tau_obs ~ dgamma(a_obs,r_obs)  ## observation error
tau_add ~ dgamma(a_add,r_add)  ## process error
}
"
##creates vectors of proper length
time <- 1:length(tide)
time.fore <- 1:(length(tide)+672)
none <- rep(NA, 672)
tide.fore<- append(tide, none, after = length(tide))##creates vector with data, then NAs filled in after

## MCMC
data.fore <- list(tide=tide.fore,n=length(time.fore),a_obs=1,r_obs=1,a_add=1,r_add=1)
nchain = 3
init.fore <- list()
for(i in 1:nchain){
  tide.samp.fore = sample(tide,time.fore,replace=TRUE)
  init.fore[[i]] <- list(tau_add=1/var(diff(tide.samp.fore)),tau_obs=5/var(tide.samp.fore))
}
j.model.fore   <- jags.model (file = textConnection(RandomWalk),
                         data = data.fore,
                         inits = init.fore,
                         n.chains = 3)

jags.out.fore   <- coda.samples (model = j.model.fore,
                            variable.names = c("surge","tau_add","tau_obs"),
                            n.iter = 1000
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

##final part, it works!
jpeg(file="~/SURGE/web/Present_RandomWalk_Output_Forecast.jpg")
plot(time.fore,ci[2,],type='l',ylim=range(tide,na.rm=TRUE),ylab="Surge Height",xlim=time.fore[time.rng.fore])
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng.fore) < 100){ 
  axis.Date(1, at=seq(time.fore[time.rng.fore[1]],time.fore[time.rng.fore[2]],by='month'), format = "%Y-%m")
}
ciEnvelope(time.fore,ci[1,],ci[3,],col="red")
points(time,tide,pch="+",cex=0.5,col="blue")
dev.off()

## obs vs process error partitioning

## following adapted from lab 7
prec = out.pres.random.fore[,grep("tau",colnames(out.pres.random.fore))]
for(i in 1:ncol(prec)){
  hist(1/sqrt(prec[,i]),main=colnames(prec)[i])
}
cor(prec)
pairs(prec)

total_error = rep(NA, nrow(prec))
for (i in 1:nrow(prec)){
  total_error[i] = prec[i,1] + prec[i,2]
}


## error plots and partition
jpeg(file="~/SURGE/web/Present_RandomWalk_Forecast_Process_Error.jpg")
plot(time.fore[1:nrow(prec)],prec[,1],ylim=range(prec[,1],na.rm=TRUE),ylab="Process Error",xlim=range(time.fore[1:3000],na.rm=TRUE),xlab="Time",type='l')
dev.off()

jpeg(file="~/SURGE/web/Present_RandomWalk_Forecast_Obs_Error.jpg")
plot(time.fore[1:nrow(prec)],prec[,2],ylim=range(prec[,1],na.rm=TRUE),ylab="Observation Error",xlim=range(time.fore[1:3000],ylim=200:300,na.rm=TRUE),xlab="Time",type='l')
dev.off()

jpeg(file="~/SURGE/web/Present_RandomWalk_Forecast_Error_Partition.jpg")
plot(time.fore[1:nrow(prec)], (prec[,1]/total_error),ylab="Error % of Total",xlab="Time",ylim = c(0,1),type='l', col="blue")
lines(time.fore[1:nrow(prec)], (prec[,2]/total_error),type='l',col="red")
legend(0.6,legend=c("Process Error","Observation Error"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red"))
dev.off()
