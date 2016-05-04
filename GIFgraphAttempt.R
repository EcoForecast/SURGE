library(animation)

saveHTML({
  for(i in 1:96){
    t=i
    fx = EnKF(Xf,surge[600+t],t)
    EnKF_output[t,]=t(as.matrix(unlist(fx)))
    ##first 10 are the X.f,next two are mu.a and P.a  
    save(t,EnKF_output,file="EnKF_Output.Rdata")
    
    ci.f <- apply(EnKF_output[,1:10],1,quantile,c(0.025,0.5,0.975))
    plot(time[1:96],EnKF_output[1:96,11],ylim=range(ci,na.rm=TRUE),xlim=c(600,700),type='n',ylab="Surge Height in m",xlab="Time",main="24-hour Forecast")
    ciEnvelope(time[1:96],ci.a[1,],ci.a[2,],col="lightBlue")
    ciEnvelope(time[1:96],ci.f[1,],ci.f[2,],col="lightGrey")
    lines(time[1:96],EnKF_output[,11],col=4)
    points(time[1:i],surge[601:(600+i)])
  }
}, interval = 0.3, ani.width = 800, ani.height = 500, movie.name = "Forecast-Animation")



