hist_tide_height = read.csv("~/SURGE/2014 tide height data/2014NHA.txt", sep="", skip="10")
hist_tideFormat<-as.numeric(levels(tide_height$f)[tide_height$f])
hist_surgeFormat<-as.numeric(levels(tide_height$f2)[tide_height$f2])

less.surge<-(hist_tideFormat+hist_surgeFormat)
hist_sorted_height<-hist_tide_height[order(hist_tide_height$yyyy.mm.dd),]
dates <- as.Date(hist_sorted_height$yyyy.mm.dd)
date1 <- as.Date("2014-03-01")
date2 <- as.Date("2014-04-30")


desired_rows <- which(dates >= date1 & dates <= date2)
height<-hist_sorted_height[desired_rows, ]

library(chron)
tod<-chron(times=height$hh.mi.ssf)
dtod<-paste(dates[desired_rows] , tod)
x<- strptime(dtod, format="%Y-%m-%d %H:%M:%S")


hist_tide <-less.surge[desired_rows]

jpeg(file="~/SURGE/Historical Tide Height")
plot(x,hist_tide, ylab="Historical Tide Height (m)", xlab="Date", main="Historical Tide Height in Newport,UK")
dev.off()
