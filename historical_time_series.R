###Historical Time Series###

```{r}

SurgeHeight = "

model{

  ##Data Model




  ##Process Model




  ##Priors


}
"
```
```{r}

##MCMC

  ##(example from Lab 6)
  
  ##define data and priors as list
  ##data <- list(y=log(y),n=length(y),x_ic=log(1000),tau_ic=100,a_obs=1,r_obs=1,a_add=1,r_add=1)
  
  ##define initial model state
  ##nchain = 3
  ##init <- list()
  ##for(i in 1:nchain){
  ##  y.samp = sample(y,length(y),replace=TRUE)
  ##  init[[i]] <- list(tau_add=1/var(diff(log(y.samp))),tau_obs=5/var(log(y.samp)))
  ##}
  
  ##j.model   <- jags.model (file = textConnection(RandomWalk),
  ##                         data = data,
  ##                         inits = init,
  ##                         n.chains = 3)
  
  ## burn-in
  ##jags.out   <- coda.samples (model = j.model,
  ##                            variable.names = c("tau_add","tau_obs"),
  ##                            n.iter = 1000)
  ##plot(jags.out)

  ## larger sample after convergence
  ##jags.out   <- coda.samples (model = j.model,
  ##                           variable.names = c("x","tau_add","tau_obs"),
  ##                           n.iter = 10000)

  ##CI plot
  ##time.rng = c(1,length(time)) ## adjust to zoom in and out
  ##ciEnvelope <- function(x,ylo,yhi,...){
  ##  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
  ##                                      ylo[1])), border = NA,...) 
  ##}
  ##out <- as.matrix(jags.out)
  ##ci <- apply(exp(out[,3:ncol(out)]),2,quantile,c(0.025,0.5,0.975))
  
  ##plot(time,ci[2,],type='n',ylim=range(y,na.rm=TRUE),ylab="Flu Index",log='y',xlim=time[time.rng])
  ## adjust x-axis label to be monthly if zoomed
  ##if(diff(time.rng) < 100){ 
  ##  axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
  ##}
  ##ciEnvelope(time,ci[1,],ci[3,],col="lightBlue")
  ##points(time,y,pch="+",cex=0.5)

  ##posterior distributions
  ##layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
  ##hist(1/sqrt(out[,1]),main=colnames(out)[1])
  ##hist(1/sqrt(out[,2]),main=colnames(out)[2])
  ##plot(out[,1],out[,2],pch=".",xlab=colnames(out)[1],ylab=colnames(out)[2])
  ##cor(out[,1:2])


```
```{r}

##Upload to Server



```



