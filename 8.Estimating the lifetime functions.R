#Estimating the lifetime distributions
#(i)b
tj <- c(4,5,8,10,11,15,20,22,24)
dj <- c(2,2,1,3,1,0,5,2,1)
nj <- c(100,98,96,90,87,86,76,71,67)


#lambda j = dj/nj
#S kaplan meir = pi(1-lambda j)
1-dj/nj

#cumulative product

SKM = cumprod(1-dj/nj)
SKM

#plot by default we get a  scatter plot
plot(tj,SKM)

#for a step function
plot(tj,SKM,type="s",xlim=c(0,25),ylim=c(0,1),xlab="Time t" , ylab ="Survival prob",main="Kaplan Meir")


#integrated hazard
#lambda=summation(dj/nj)
lambda=cumsum(dj/nj)
lambda

#var(lambda)=summation(dj*(nj-dj)/nj^3)
var=cumsum(dj*(nj-dj)/nj^3)
var

plot(tj,lambda,type="s",xlim=c(0,25),ylim=c(0,0.3),xlab="Time t" , ylab ="Integrated Hazard" , main="Nalson AALen Estimator of Integrated HAzard")


lines(tj,lambda-1.96*sqrt(var),type="s",col="red") #CONFIENCE INTERVAL
lines(tj,lambda+1.96*sqrt(var),type="s",col="blue")


Sna=exp(lambda)
Sna

data.frame(tj,SKM,Sna,SKM<=Sna)

SKM<=Sna
