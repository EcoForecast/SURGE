library(RCurl)
todaydate = (Sys.Date() - 365)
date = gsub("-","/",todaydate)
startdate = as.Date("2015/03/01")

firstpart = "https://www.wunderground.com/history/airport/EGFF/"
lastpart = "/DailyHistory.html?req_city=Cardiff&req_state=&req_statename=United+Kingdom&reqdb.zip=00000&reqdb.magic=1&reqdb.wmo=03717&format=1"

##weather_data = read.csv(paste(firstpart,date,lastpart)) 
weather_data = list()
for (i in 0:(todaydate-startdate)) {  
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

#met = Reduce(function(...) merge(...,all=TRUE),weather_data)

jpeg(file="~/SURGE/web/WindSpeed.jpg")
     plot(day,wind, ylab="Wind Speed (mph)", xlab="Date", main="Wind Speed in 30 min Intervals in Cardiff,UK",type='l')
     dev.off()
     
jpeg(file="~/SURGE/web/SeaLevelPressure.jpg")
          plot(day,pres, ylab="Sea Level Pressure (in)", xlab="Date", main="Sea Level Pressure in 30 min Intervals in Cardiff,UK",type='l')
dev.off()

          