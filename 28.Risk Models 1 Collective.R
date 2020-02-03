#Risk Models
#Collective Risk Models

#Compound Poisson

#S=X1 + X2 + ..........+ Xn
#N~P(1000)  No. of claims (Dis)
#X~Gamma(750,0.25) Shape parameter= 750 rate=0.25 amount of claim
#Simulate 10000 observations from S

set.seed(123)
n <- rpois(1,1000)
n

x <- rgamma(n,shape=750,rate=0.25)
x
sum(x)


set.seed(123)
n <- rpois(10000,1000)
n

s <- rep(0,10000)
for(i in 1:10000){
  x <- rgamma(n[i],shape=750,rate=0.25)
  s[i]=sum(x)
}

s


#____________________________________________________________________________________________________
#Example
#Agg claims S~ a comp distb , where the no. of claims N~ Comp Poisson distb with para 100
#Individual claim Sizes X follow an exponential distb with parameters 0.002.
#use seed value 8143 to simulate 1000 RV from the comp distb . Hence State the value of the 
#98th simulated value of S.
#Estimate the medain of the insurers aggregate claim payments.
#Estimate the coeff of skewness of the insurers aggregate claim payments S.


set.seed(8143)
n <- rpois(1000,100)
n

s <- rep(0,1000)
for(i in 1:1000){
  x <- rexp(n[i],0.002)
  s[i]=sum(x)
}

s

s[98]



#MEDIAN

quantile(s,0.5)
median(s)


#mean
mean(s)

#var
var(s)

#sd 
sd(s)



#COeff of skewness
skew <- sum((s-mean(s))^3)/length(s)
skew            


cs <- skew/var(s)^3/2
cs



#empirical prob
length(s[s>3000000])/length(s)
quantile(s,0.9)
 



#Fitting a dist to a compound poisson distb
#normal Dist

f <- function(par){
  lnl <- dnorm(s,mean=par[1],sd=par[2],log=TRUE)
  sum(-lnl)
}

p <- c(mean(s),sd(s))


l <- nlm(f,p)
l

l$estimate


mu <- l$estimate[1]
sigma <- l$estimate[2]



#analyse the fit
#histogram
#QQ plot

plot(density(s),main="Simulated vs fitted distb",xlab="agg claim",col="red")

c <- seq(min(s),max(s))


min(s)
max(s)


y <- dnorm(c,mu,sigma)

lines(c,y,col="blue")





#QQ plot
agg <- rnorm(1000,mu,sigma)

 qqplot(agg,s,xlab="quantiles of fitted distb",ylab="quantiles of simulated data",main="QQ plot") 
abline(0,1,col="red") 



#---------------------------------------------------------------------------------------

#Compound Poisson

#S=X1 + X2 + ..........+ Xn
#N~P(1000)  No. of claims (Dis)
#X~Gamma(750,0.25) Shape parameter= 750 rate=0.25 amount of claim
#Simulate 10000 observations from S

#S=SI + SR
#SI= Y1 + Y2 + ............ + YN



set.seed(123)
n <- rpois(10000,1000)
n

SI <- rep(0,10000)
SR <- rep(0,10000)
M <- 2500

for(i in 1:10000){
  x <- rgamma(n[i],shape=750,rate=0.25)
  y <- pmin(x,M)
  z <- pmax(0,x-M)
  SI[i]=sum(y)
  SR[i] <- sum(z)
}

s

mean(SI)
mean(SR)
mean(s)


var(SR)
var(SI)




#_______________________________
#Proportional Reinsurance

set.seed(123)
n <- rpois(10000,1000)
n
?rep
SI <- rep(0,10000)
SR <- rep(0,10000)
alpha <- 0.7

for(i in 1:10000){
  x <- rgamma(n[i],shape=750,rate=0.25)
  y <- 0.7*x
  z <- 0.3*x
  SI[i]=sum(y)
  SR[i] <- sum(z)
}

s

mean(SI)
mean(SR)
mean(s)


var(SR)
var(SI)