#CS1 Distribution
#dbinom   #dpois    #dexp
#pbinom   #ppois    #pexp
#qbinom   #qpois    #qexp
#rbinom   #rpois    #rexp
#X~Bin(15,0.4)
#dbinom(x,n,p) this will give P(X=x) for bin(n,p)

dbinom(10,20,0.80) #this will give P(X=10) for Bin(20,0.8)dist
n <- 15
p <- 0.4

#P(X=5)
dbinom(5,n,p)


#P(X<=4)
dbinom(0,n,p)+dbinom(1,n,p)+dbinom(2,n,p)+dbinom(3,n,p)+dbinom(4,n,p)
sum(dbinom(c(0:4),n,p))

#pbinom(x,n,p) this will give P(X<=X) for B(n,p)
pbinom(4,n,p)

#P(X<9) = P(X<=8)
pbinom(8,n,p)

#P(X>3) = 1 - P(X<=3)
1 - pbinom(3,n,p)
pbinom(3,n,p,lower=FALSE) #P(X>x)


#P(X>=4) = P(X>3)

#P(3<=X<10) = P(X<10) - P(X<3) = P(X<=9) - P(X<=2)
pbinom(9,n,p) - pbinom(2,n,p)


#qbinom(percentile,n,p) this will give the minimum value of x for which P(X<=x)>=percentile
qbinom(0.5,n,p)  #median    #q ka output will be p ka input

pbinom(5,n,p)
pbinom(6,n,p)


qbinom(0.25,n,p) #first quartile
pbinom(4,n,p)

qbinom(0.75,n,p) #third quartile 
qbinom(0.75,n,p,lower=FALSE)  #1st Quartile


qbinom(0.99,n,p)  #  99th percentile
qbinom(0.1,n,p)   #10th percentile


_________________________________________________________________________________________
####### GRAPH OF PDF  

uv <- 0:n                   #dbinom = at a point probability
plot(uv,dbinom(uv,n,p))



plot(uv,dbinom(uv,n,p),type="h",xlab="No. of success",ylab="P(X=x)" , main = "Graph of Pdf of B(15,0.4) dist")


barplot(dbinom(uv,n,p), names = uv,xlab="No. of success",ylab="P(X=x)" , main = "Graph of Pdf of B(15,0.4) dist")

___________________________________________________________________________________________
############## GRAPH OF CDF
plot(uv,pbinom(uv,n,p),type="s",xlab="No. of success",ylab="P(X=x)" , main = "Graph of CDf of B(15,0.4) dist")



_________________________________________________________________________
#rbinom(no. of values , n,p)  this will generate random values
set.seed(50)
rbinom(100,12,0.2)



#generate 200 values of B(15,0.4) dist using seed 20 and save them in an object S
set.seed(20)
S <- rbinom(200,n,p)

S
table(S) # it will show us the frequency distribution of the values
mean(S)
var(S)
sd(S)


skew <- sum((S-mean(S))^3)/length(S)
skew


coefficientskewness <- (skew/sd(S)^3)
coefficientskewness

hist(S,breaks = (-0.5:13.5)) #converted  convertd data to dicrete by applying breaks

___________________________________________________________________________
#Poisson
#dpois(x,lambda)
#ppois(x,lambda)
#qpois(percentile,lambda)
#rpois(no. of values,lambda)

#dgeom this works for type 2 dist by default
#dnbinom  same as above
#dhyper


#Type 2 Geom: NO. of failures before 1st success   x=0,1,2..........
#Type 1 GEom: No. of trials before 1st success    x=1,2,............


dgeom(5,0.1) #Prob of 5 failures before first success or prob of 1st Success in 6th trial




#X~Exp(5)
#dexp(x,l) #f(x) for Exp(l)
#pexp(x,l) #P(X<=x)=P(X<x) for Exp(l)
#qexp(percentile,l) this will calculate the value of x for which P(X<=x)=percentile
#rexp(no. of values , l) this will generate the random numbers from Exp(l) distribution

l <- 5
#PDF
#l*exp(-l*x)
#f(2) = 5*exp(-5*2)
5*exp(-5*2)

dexp(2,l)


#CDF
#P(X<=x)=1-exp(-l*x)
#P(X<=3)=1-exp(-l*3)
1-exp(-l*3)

pexp(3,l)
#P(0.1<=X<0.5)=P(X<0.5)-P(X<=0.1)
pexp(0.5,l)-pexp(0.1,l)
#P(X>0.4)=1-P(X<=0.4)
1-pexp(0.4,l)
pexp(0.4,l,lower=FALSE)


#GRAPH OF PDF
#METHOD 1 
x <- seq(0,4,0.001)
plot(x,dexp(x,l))   #SCATTER PLOT


plot(x,dexp(x,l),type="l",xlab="x",ylab="f(x)",main="Graph of PDF of exp")   #JOINING THROUGH LINES

#METHOD 2
curve(dexp(x,l)) # curve command takes the value of x from 0 to 1 by default
curve(dexp(x,l),0,4,xlab="x",ylab="f(x)",main="Graph of PDF of exp")



#GRAPH OF CDF
#METHOD 1

plot(x,pexp(x,l),type="l",xlab="x",ylab="f(x)",main="Graph of CDF of exp")


#METHOD 2

curve(pexp(x,l),0,4,xlab="x",ylab="f(x)",main="Graph of CDF of exp")


#qexp(percentile,l)
qexp(0.5,l)     #median
qexp(0.25,l)    #first quartile
qexp(0.75,l)
qexp(0.95,l)


#rexp(no. of values , l )
rexp(1000,l) #this will generate 1000 values of exp(5) distribution
set.seed(100)
S <- rexp(1000,l)
S
sum(S)
mean(S)
var(S)
sd(S)

runif(100) # by default 0 and 1
runif(200,2,8)

rnorm(500) #by deafault this works for N~(0,1)
rnorm(1000,5,10) #N(5,100)
?rnorm


rlnorm(300,5,10)
?rlnorm
