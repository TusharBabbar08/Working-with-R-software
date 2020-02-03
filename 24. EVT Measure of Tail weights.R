#Measures of Tail Weight

#limiting Density Ration

#Lets say we have two RV X1 and X2 , where X1~Beta(2,0.5) and #X2~Beta(3,0.8) .
#We want to establish which of the two distributions has a heavier tale.

x <- seq(0.001,0.999,0.001)

f1 <- dbeta(x,2,0.5)
f2 <- dbeta(x,3,0.8)

r <- f1/f2
r
#X1 has a heavier tale

plot(x,r,type="l",xlim=c(0.95,1),ylim=c(0.5,3),main="Density Ratio")


#_______________________________________________________________________________________________________



#Hazard Rates

#pdf = HR * Survival function
#HR=PDF/SF
#HR=PDF/1-F(X)


#Example 1 
#(i) Calc the Hazard rate for the RV X, where X~logN(mum=10,sigma=2).
#(ii) Plot the hazard rate , and interpret the result.

x <- seq(1,1000,0.01)
p <- dlnorm(x,10,2)
F <- plnorm(x,10,2)


H <- p/(1-F)
H

#(ii)
plot(x,H,type="l",main="Hazard rate for logN(10,4)")




#Example 2
#The RV X~Burr(alpha=0.5,lambda=10,gamma=2). Calc the survival rate for x=3


dburr <- function(x,a,l,g){
  ((a*g*l^a)*x^(g-1))/((l+x^g)^(a+1))
}


pburr <- function(x,a,l,g){
  1-(l/(l+x^g))^a
}

x <- 3
p <- dburr(x,0.5,10,2)
F <- pburr(x,0.5,10,2)


H <- p/(1-F)
H


#______________________________________________________________________________________________




#MEan Residual Lifetime

#ex= integration of SF from x to infinity/SF

#Example 1
#Calc the mean residual life of the RV X~GA(50,1.5), for x = 40.

?pgamma
#shape=alpha
#scale=lambda
alpha <- 50
lambda <- 1.5
x <- 40
S <- pgamma(x,shape=alpha,scale=lambda,lower=FALSE)  #P(X>40)
S


Sy <- function(y){
  pgamma(y,shape=alpha,scale=lambda,lower=FALSE)
}

I <- integrate(Sy,x,Inf)
I$value

num <- I$value


ex <- num/S
ex




#Example 2
#Calc the mean residual life at x = 10 for X~W(0.001,2)


?pweibull

#shape=gamma
#scale=1/c^(1/gamma)

g <- 2
c <- 0.001
b <- c^(-1/g)


Sw <- function(y){
  pweibull(y,shape=g,scale=b,lower=FALSE)
}
x <- 10
I2 <- integrate(Sw,x,Inf)
I$value

num2 <- I2$value

den2 <- pweibull(x,shape=g,scale=b,lower=FALSE)

ex2 <- num2/den2
ex2
