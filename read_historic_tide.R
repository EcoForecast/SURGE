tide_height.h = read.csv("~/SURGE/2014 tide height data/2014NHA.txt", sep="", skip="10")
tide.h<-as.numeric(levels(tide_height.h$f)[tide_height.h$f])
surge.h<-as.numeric(levels(tide_height.h$f2)[tide_height.h$f2])

less.surge.h<-(tide.h+surge.h)
sorted_height.h<-tide_height.h[order(tide_height.h$yyyy.mm.dd),]
dates.h <- as.Date(sorted_height.h$yyyy.mm.dd)



library(chron)
tod.h<-chron(times=tide_height.h$hh.mi.ssf)
dtod.h<-paste(dates.h , tod.h)
x.h<- strptime(dtod.h, format="%Y-%m-%d %H:%M:%S")
x.h<- as.list(as.character(x.h))
y.h <-less.surge.h
total.data<- cbind(x.h,y.h)

write.csv(total.data,file="historic.tide.data")
