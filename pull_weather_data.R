todaydate = (Sys.Date() - 365)
date = gsub("-","/",todaydate)
startdate = as.Date("2015/01/01")

firstpart = "http://www.wunderground.com/history/airport/EGFF/"
lastpart = "/DailyHistory.html?req_city=Cardiff&req_state=&req_statename=United+Kingdom&reqdb.zip=00000&reqdb.magic=1&reqdb.wmo=03717&format=1"

##weather_data = read.csv(paste(firstpart,date,lastpart)) ##works

for (i in 0:(todaydate-startdate)) {  
  
  weather_data = read.csv(paste(firstpart,(startdate+i),lastpart,sep="")) ##runs but doesn't work--weird format
  

  x <- weather_data$DateUTC
  
  y <-weather_data$Wind.SpeedMPH
  
  jpeg(file="~/SURGE/web/Wind Speed ")
  plot(x,y, ylab="Wind Speed (mph)", xlab="Date", main="Wind Speed in 30 min Intervals in Cardiff,UK")
  dev.off()
  
  ## "need finite 'xlim' values" ?
}


