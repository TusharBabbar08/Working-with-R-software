#Simulating a stationary TIME SERIES

#Simulate 100 obs from the following ARMA(2,1) time series using seed 1000
#Yt=0.5Yt-1 - 0.1Yt-2 - et + 0.2et-1
#et~N(0,16) dist
set.seed(1000)
Yt <- arima.sim(n=100,list(ar=c(0.5,-0.1),ma=0.2),sd=4)
Yt


#Wt=10+0.8Wt-1 + et + 0.4 et-1 + 0.1et-2  sigma=5 seed 100 n=1000
set.seed(100)
Wt <- 10 + arima.sim(n=1000,list(ar=0.8,ma=c(0.4,0.1)),sd=5)
Wt


#n: no. of obs required
#ar: coeff of ar part
#ma; coeff of ma part


#simulating a non statioanary time series


#DEeterministic Trend
#Xt=3+8t+Yt , Yt is a stationary time series

t <- 100
set.seed(50)
Xt <- 3+8*(1:t) + Yt
Xt





#STochastic Trend

#Yt is a stationary time series

#Yt=Zt - Zt-1
#Yt=(1-B)Zt

#(1-B)Zt=0.5Yt-1 + 0.2Yt-2 + et + 0.2et-1
set.seed(1000)
Yt <- arima.sim(n=100,list(ar=c(0.5,-0.1),ma=0.2),sd=4)
Yt


#Z2=Y1+Y2
#Z2 - Z1 = Y3
#Z3 = Y3 + Z2 = Y3 + Y2
#Z4 - Z3 = Y4
#Z4 = Y4 + Z3 = Y4+y3+Y2

Zt <- cumsum(Yt)
Zt

zt <- ts(Zt)
zt


#Time Seriess plot

plot(Yt,main="Differenced time series")
plot(Zt,type="l")
plot(zt,main="ARIMA(2,1,1)",col="blue")

par(mfrow=c(2,1))
plot(Yt,main="Differenced time series",col="red")
plot(zt,main="ARIMA(2,1,1)",col="blue")


par(mfrow=c(1,1))
