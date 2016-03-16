todaydate = (Sys.Date() - 365)
date = gsub("-","/",todaydate)
startdate = as.Date("2015/01/01")

firstpart = "http://www.wunderground.com/history/airport/EGFF/"
lastpart = "/DailyHistory.html?req_city=Cardiff&req_state=&req_statename=
            United+Kingdom&reqdb.zip=00000&reqdb.magic=1&reqdb.wmo=03717&format=1"
##weather_data = read.csv(paste(firstpart,date,lastpart))

for (i in startdate:todaydate) {
  
  wind_data = read.csv(paste(firstpart,startdate,lastpart))
  

  x <- wind_data$DateUTC.br...
  
  
  y <-wind_data$Wind.SpeedMPH
  
  jpeg(file="~/SURGE/web/Wind Speed ")
  plot(x,y, ylab="Wind Speed (mph)", xlab="Date", main="Wind Speed in 30 min Intervals in Cardiff,UK")
  dev.off()
  startdate = startdate + 1
  
}

