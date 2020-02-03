#PH MODELS
logL <- function(x){
  18*x-4*log(30*exp(x)+17)-4*log(27*exp(x)+14)-log(23*exp(x)+14)-2*log(22*exp(x)+12)-2*log(20*exp(x)+10)-5*log(18*exp(x)+8)-2*log(12*exp(x)+8)-log(10*exp(x)+8)-2*log(7*exp(x)+4)
}  
  
plot(logL)

plot(logL,xlim=c(-1,3))
plot(logL,from=-1 , to=3)
plot(logL,-1,3,xlab="Beta",ylab="log-likelihood",main="Log-LIkelihood of Cox PH model")


plot(logL,0.66,0.671,xlab="Beta",ylab="log-likelihood",main="Log-LIkelihood of Cox PH model")
#approx 0.68

#non-linear minimisation
?nlm
nlm(-logL,0.7)


neglogL <- function(x){
  -logL(x)
}

MLE <- nlm(neglogL,0.7)
MLE$estimate
exp(MLE$estimate)
#95.3% higher than the other patients

#h(t,zi)=h0(t)*exp(b*z)
#for compromised patient
#h(t,zi)=l*exp(-lx)*exp(b)

#for non compromised patient
#h(t,zi)=l*exp(-lx)

lambda=0.2439
comp <- function(x){
  lambda*exp(-lambda*x)*exp(MLE$estimate)
}

notcomp <- function(x){
  lambda*exp(-lambda*x)
}

x <- seq(0,12,0.001)

plot(x,comp(x),type="l",xlab="durtion",ylab="hazard rate",main="Comparison of hazard rate for cox model")
lines(x,notcomp(x),col="red")
