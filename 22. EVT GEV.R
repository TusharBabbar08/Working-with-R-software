#Extreme Value Theory
#GEV Distribution

#block Maxima

#fitting a GEV Dist

#Goodness of Fit




data <- read.table("EVTdata.txt",header=TRUE)
head(data)


#Block Maxima
#grouping by year and month

?aggregate

maxima <- aggregate(Claim~Year:Month,data,max)
head(maxima)

min(data[,2])


yearindex <- (data[,2]-1990)%/%5 + 1 

92/5
92%/%4   # integer part only


yearindex


data <- cbind(data,yearindex)
head(data)


#grouping by year
maxima <- aggregate(Claim~Year,data,max)
head(maxima)

max(data[,2])


#Fitting a GEV Dist
alpha <- 10
beta <- 10
gamma <- 10

p <- c(alpha,beta,gamma)

fMLE <- function(params){
  f <- 1/params[2]*(1+params[3]*(maxima[,2]-params[1])/params[2])^(-1-1/params[3])*
    exp(-(1+params[3]*(maxima[,2]-params[1])/params[2])^(-1/params[3]))
  lnf <- log(f)
  sum(-lnf)
}

fMLE(p)


MLE <- nlm(fMLE,p)
MLE$estimate

#alpha=12636
#beta=8148
#gamma=-0.04

#USing the Fitted Distribution
#estiamte the prob t\hat the  max claim in any year is > 30000

alpha <- MLE$estimate[1]
beta <- MLE$estimate[2]
gamma <- MLE$estimate[3]

m <- 30000
prob <- 1-exp(-(1+gamma*(m-alpha)/beta)^(-1/gamma))
prob


#Goodness of Fit
#Histogram original dtaa and fitted data
#Empirical density plot
#QQ plot


hist(maxima[,2],freq=FALSE,main="HISTOGRAM",xlab="MAXIMA")

x <- seq(0,50000)

GEV <- 1/beta*(1+gamma*(x-alpha)/beta)^(-1-1/gamma)*
  exp(-(1+gamma*(x-alpha)/beta)^(-1/gamma))

lines(x,GEV,col="red")


#empirical density function
plot(density(maxima[,2]),xlab="MAXIMA",main="Empiricla density vs fitted distb",ylim=c(0,0.00005))
x <- seq(0,50000)

GEV <- 1/beta*(1+gamma*(x-alpha)/beta)^(-1-1/gamma)*
  exp(-(1+gamma*(x-alpha)/beta)^(-1/gamma))

lines(x,GEV,col="red")  #not a good fit due to less sample size


#QQ plot

rGEV <- function(n,a,b,g){
  f <- a + b/g*((-log(runif(n)))^(-g)-1)
  return(f)
}

set.seed(29)
x <- rGEV(1000,alpha,beta,gamma)
x

qqplot(x,maxima[,2],xlab="Quantiles from fitted distb",ylab="sample Quantiles",main="QQ plot")
abline(0,1,col="red")
# fit doesnt work well on extremes
