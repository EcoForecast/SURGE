tide_height = read.csv("~/SURGE/tide height data/2015NHA.txt", sep="", skip="10")
sorted_height<-tide_height[order(tide_height$yyyy.mm.dd),]
dates <- as.Date(sorted_height$yyyy.mm.dd)
date1 <- as.Date("2015-03-01")
date2 <- Sys.Date()-365


desired_rows <- which(dates >= date1 & dates <= date2)
height<-sorted_height[desired_rows, ]

library(chron)
tod<-chron(times=height$hh.mi.ssf)
dtod<-paste(dates[desired_rows] , tod)
x<- strptime(dtod, format="%Y-%m-%d %H:%M:%S")


surgeAndTide <-as.numeric(levels(height$f)[height$f])

jpeg(file="~/SURGE/web/Tide+Surge_Height_until_Now.jpg")
plot(x,surgeAndTide, ylab="Tide Height (m)", xlab="Date", main="Tide+Surge height at 15 minute intervals in Newport,UK")
dev.off()
