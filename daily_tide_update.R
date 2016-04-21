library(RCurl)
todaydate = (Sys.Date() - 365)
startdate = as.Date("2015/03/01")

tide_data=list()
for (i in 0:(todaydate-startdate)) {  
  
  load("tide_data.Rdata")
  start = i
  for (i in start:(todaydate-startdate)) {  
    
    print(i)
    tide_data[[i+1]] = read.csv("~/SURGE/tide height data/2015NHA.txt") 
    save(i,tide_data,file="tide_data.Rdata")
    }
}

    
 