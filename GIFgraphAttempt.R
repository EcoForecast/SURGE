library(animation)


saveHTML({
  for(i in 1:6){

  t=(i+192)
    plot(time[1:i],ci[2,][1:i],type='l',ylim=range(-10:10,na.rm=TRUE),ylab="Surge Height",xlim=range(1:1272))
    ciEnvelope(time[1:t],ci[1,][1:t],ci[3,][1:t],col="red")
    points(time[1:i],surge[1:i],pch="+",cex=0.3,col="blue")
    lines(time[1:i],ci[2,][1:i])
    
  }
}, interval = 0.5, ani.width = 550, ani.height = 350, movie.name = "timestepAnimation")