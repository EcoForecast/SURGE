## Diagnostic Plots

library("plotrix")

jpeg(file="~/SURGE/web/Taylor_Diagram.jpg")
taylor.diagram(ref= surge[(nstart+1):(nstart+nt)], model=EnKF_output[,11],normalize=TRUE,ref.sd=TRUE)
dev.off()

jpeg(file="~/SURGE/web/Predicted_Observed_Curves.jpg")
plot(EnKF_output[,11], ylim=c(0, 7), xlab="Time", ylab="Surge Height", main="Predicted vs Observed",col="RED", type='l')
lines(surge[(nstart+1):(nstart+nt)], col="Blue")
legend(0,7, legend=c("Predicted", "Observed"), cex=0.6,lty=c(1,1), lwd=c(2.5,2.5), col=c("Red", "Blue"))
dev.off()

## messed up e vs o
jpeg(file="~/SURGE/web/PredictedvsObserved.jpg")
plot(EnKF_output[,11], surge[(nstart+1):(nstart+nt)], xlab="Predicted", ylab="Observed", col="RED", type='l')
abline(0,1,col="Green",lwd=2)
dev.off()

## Summary 
##summary(EnKF_output)

## RMSE Calculation
RMSE = sqrt(mean((EnKF_output[,11]-surge[(nstart+1):(nstart+nt)])^2))
RMSE
