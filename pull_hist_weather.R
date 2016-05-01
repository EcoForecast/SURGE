library(RCurl)
startdate = as.Date("2014/03/01")
enddate = as.Date("2014/04/30")

firstpart = "https://www.wunderground.com/history/airport/EGFF/"
lastpart = "/DailyHistory.html?req_city=Cardiff&req_state=&req_statename=United+Kingdom&reqdb.zip=00000&reqdb.magic=1&reqdb.wmo=03717&format=1"

##weather_data = read.csv(paste(firstpart,date,lastpart)) 
hist_weather_data2 = list()
load("hist_weather_data2.Rdata")
start = 1
for (i in start:(enddate-startdate)) {  
  
  print(i)
  date2 = gsub("-","/", startdate+i)
  met.url = paste(firstpart,date2,lastpart,sep="")
  # system(paste0("cd met; wget ",met.url))
  x = getURL(met.url)
  hist_weather_data2[[i+1]] = read.csv(text=x) 
  save(i,hist_weather_data,file="hist_weather_data2.Rdata") 
  
}

hist_wind = unlist(sapply(hist_weather_data2,function(x){x$Wind.SpeedMPH},simplify = TRUE))
hist_pres = unlist(sapply(hist_weather_data2,function(x){x$Sea.Level.PressureIn},simplify = TRUE))
hist_pres[hist_pres < 5] = NA
dateUTC = unlist(sapply(hist_weather_data2,function(x){x$DateUTC},simplify = TRUE))

day = strptime(sub(pattern = "<br />","",as.character(dateUTC)),format="%Y-%m-%d %T")

#met = Reduce(function(...) merge(...,all=TRUE),weather_data)

jpeg(file="~/SURGE/web/HistoricalWindSpeed.jpg")
plot(day,hist_wind, ylab="Wind Speed (mph)", xlab="Date", main="Historical Wind Speed in Cardiff, UK",type='l')
dev.off()

jpeg(file="~/SURGE/web/HistoricalSeaLevelPressure.jpg")
plot(day,hist_pres, ylab="Sea Level Pressure (in)", xlab="Date", main="Historical Sea Level Pressure in Cardiff, UK",type='l')
dev.off()