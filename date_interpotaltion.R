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
