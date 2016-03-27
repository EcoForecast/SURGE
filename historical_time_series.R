###Historical Time Series###

```{r}
##pull tide data
tide_height = read.csv("~/SURGE/tide height data/2015NHA.txt", sep="", skip="10")
tide<-as.numeric(tide_height$f)
surge<-as.numeric(tide_height$f2)

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
######

## pull weather data
startdate = as.Date("2014/01/01")
enddate = as.Date("2014/06/30")

firstpart = "http://www.wunderground.com/history/airport/EGFF/"
lastpart = "/DailyHistory.html?req_city=Cardiff&req_state=&req_statename=United+Kingdom&reqdb.zip=00000&reqdb.magic=1&reqdb.wmo=03717&format=1"

##weather_data = read.csv(paste(firstpart,date,lastpart)) 
hist_weather_data = list()
for (i in 0:(enddate-startdate)) {  
  
  dateToo = gsub("-","/", startdate+i)
  hist_weather_data[[i+1]] = read.csv(paste(firstpart,dateToo,lastpart,sep="")) 
  
}

hist_wind = unlist(sapply(hist_weather_data,function(x){x$Wind.SpeedMPH},simplify = TRUE))
hist_pres = unlist(sapply(hist_weather_data,function(x){x$Sea.Level.PressureIn},simplify = TRUE))
hist_pres[hist_pres < 5] = NA
dateUTC = unlist(sapply(hist_weather_data,function(x){x$DateUTC},simplify = TRUE))

day = strptime(sub(pattern = "<br />","",as.character(dateUTC)),format="%Y-%m-%d %T")
```

```{r}

SLOSHSurge = "

model{

  ##Data Model
  
  for (i in 1:n) {
    

  }

  
  ##Process Model

  ## surge (+ tide) = (water depth*Coriolus*wind speed) * ((gravity/tide height) * (depth + pressure))
  ## S (+ tide) = (25*901.4*wind speed) * ((9.8/tide height) * (25 + pressure))


  ##Priors
  
  ## credible range: -1 to 10m
  ## prior ~ dunif(-1, 10)

}
"
```
```{r}

##MCMC

  ##(example from Lab 6)
  
  ##define data and priors as list
  ##data <- list(y=log(y),n=length(y),x_ic=log(1000),tau_ic=100,a_obs=1,r_obs=1,a_add=1,r_add=1)
  
  ##define initial model state
  ##nchain = 3
  ##init <- list()
  ##for(i in 1:nchain){
  ##  y.samp = sample(y,length(y),replace=TRUE)
  ##  init[[i]] <- list(tau_add=1/var(diff(log(y.samp))),tau_obs=5/var(log(y.samp)))
  ##}
  
  ##j.model   <- jags.model (file = textConnection(RandomWalk),
  ##                         data = data,
  ##                         inits = init,
  ##                         n.chains = 3)
  
  ## burn-in
  ##jags.out   <- coda.samples (model = j.model,
  ##                            variable.names = c("tau_add","tau_obs"),
  ##                            n.iter = 1000)
  ##plot(jags.out)

  ## larger sample after convergence
  ##jags.out   <- coda.samples (model = j.model,
  ##                           variable.names = c("x","tau_add","tau_obs"),
  ##                           n.iter = 10000)

  ##CI plot
  ##time.rng = c(1,length(time)) ## adjust to zoom in and out
  ##ciEnvelope <- function(x,ylo,yhi,...){
  ##  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
  ##                                      ylo[1])), border = NA,...) 
  ##}
  ##out <- as.matrix(jags.out)
  ##ci <- apply(exp(out[,3:ncol(out)]),2,quantile,c(0.025,0.5,0.975))
  
  ##plot(time,ci[2,],type='n',ylim=range(y,na.rm=TRUE),ylab="Flu Index",log='y',xlim=time[time.rng])
  ## adjust x-axis label to be monthly if zoomed
  ##if(diff(time.rng) < 100){ 
  ##  axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
  ##}
  ##ciEnvelope(time,ci[1,],ci[3,],col="lightBlue")
  ##points(time,y,pch="+",cex=0.5)

  ##posterior distributions
  ##layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
  ##hist(1/sqrt(out[,1]),main=colnames(out)[1])
  ##hist(1/sqrt(out[,2]),main=colnames(out)[2])
  ##plot(out[,1],out[,2],pch=".",xlab=colnames(out)[1],ylab=colnames(out)[2])
  ##cor(out[,1:2])


```
```{r}

##Upload to Server



```



