#Stationarity of Multivariate TS

#Example 1
#Xt = 0.2Xt-1 + 0.4Yt-1 + etx
#Yt = 0.1Xt-1 +0.3Yt-1 + ety


A <- matrix(c(0.2,0.1,0.4,0.3),2,2)
A

eigen(A)$values







#Example 2
#Xt = -0.6Xt-1 + 0.5Yt-1 +etx
#Yt = -0.1Xt-1 + 0.8Yt-1 + ety
#Wt = Xt-1 + 0.20Yt-1 + 0.2Wt-1 + etw

A1 <- matrix(c(-0.6,-0.1,0.5,0.8),2,2)
A1


eigen(A1)$values




#Example 3
#Xt = 3Xt-1 + 0.2Yt-1 + 0.7Wt-1 + etx
#Yt = 0.7Xt-1 + 0.9Yt-1 + 0.4Wt-1 + ety

A3 <- matrix(c(3,0.7,1,0.2,0.9,0.2,0.7,0.4,0.2),3,3)
A3
eigen(A3)$values    #not Stationary





#___________________________________________________________

#Cointegrated TS
#X and Y are I(1) process
#theree exist a vector (a,b),ax+by


xy <- read.table("cointegration.txt",header=TRUE)
xy

x <- xy$x
x


y <- xy$y
y



PP.test(x)  #49%  H0 not rejected therefore diff req
dx <- diff(x)
PP.test(dx)  #stationary after 1 differencing


PP.test(y)   #69%
dy <- diff(y)
PP.test(dy)   #stat



find.coint <- function(coint){
  comb <- coint[1]*x + coint[2]*y
  test <- PP.test(comb)$p.value
  return(test)
}

v <- c(1,1)
X <- nlm(find.coint,v)
X$estimate


a <- X$estimate[1]
b <- X$estimate[2]


Y <- a*x + b*y

PP.test(Y)   #there could be multiple answers
