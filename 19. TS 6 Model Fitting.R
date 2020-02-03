#Having obtained stationary data we can identify what order
#model to fit by examining the sample ACF and the sample PACF:
#if the sample ACF cuts off for lags k>q , then we would fit a MA(q) to the stationary data
#if the sample PACF cuts off for lag K>p , then we would fit an AR(p) to the  stationary data
#if both the ACF and PACF decay,then we would fit an ARMA(p,q) model,
#using trial and error to find the optimal values of p and q.



#example 1
data <- ts(read.table("fittingmodelEg1.csv"))
head(data)


par(mfrow=c(3,1))

plot(data,main="WHAT model to fit?",ylab="Data of Eg 1")
acf(data,main="Sample ACF")
pacf(data,main="Sample PACF")


#Eg1 ARIMA(0,0,3)=MA(3)

m <- matrix(c(1,2,1,3),2,2)
m
layout(m)

plot(data,main="WHAT model to fit?",ylab="Data of Eg 1")
acf(data,main="Sample ACF")
pacf(data,main="Sample PACF")



data2 <- ts(read.table("fittingmodelEg2.csv"))
plot(data2,main="WHAT model to fit?",ylab="Data of Eg 1")
acf(data2,main="Sample ACF")
pacf(data2,main="Sample PACF") #lag 3 onwards , data is cut off

#ARIMA(2,0,0)=AR(2)



#EX 3
data3 <- ts(read.table("fittingmodelEg3.csv"))

plot(data3,main="WHAT model to fit?",ylab="Data of Eg 1")
acf(data3,main="Sample ACF")
pacf(data3,main="Sample PACF")
#since we could see a trend so we try differencing

#Differencing data 3
d <- diff(data3,lag=1,differences = 1)



m <- matrix(c(1,2,1,3,4,5,4,6),2,4)
m
layout(m)

plot(data3,main="WHAT model to fit?",ylab="Data of Eg 1")
acf(data3,main="Sample ACF")
pacf(data3,main="Sample PACF")
plot(d,main="Differnced Time Series")
acf(d,main="DIff ACF")
pacf(d,main="DIff PACF")

#ARIMA (p,1,q)  using hit and trial for p and q 

#Fitiing a time series model

#arima(x,order=c(1,2,1))
#EG 1

data

arima(data,order=c(0,0,3))

#Xt=mu + b1et-1 + b2et-2 + b3et-3 +et 
#Akaike INF Criteria


arima(data,order=c(1,0,3))  #aic minimuum better fit

#EX 2
ar2 <- arima(data2,order=c(2,0,0))  #log likelihood


#Xt= mu +  a1(Xt-1 - mu) + a2(Xt-2 - mu) +et

ar(data2)   #Yule walker 
ar(data2)$x.mean


#EX 3

arima(data3,order=c(1,1,1))
arima(d,order=c(1,0,1))





#TEsting the fit


#calculating residuals
#eg1
data


ma3 <- arima(data,order=c(0,0,3))
e <- ma3$residuals

par(mfrow=c(2,1))
plot(e,main="Errors of MA(3) process",ylab="Residuals")
acf(e,ylab="ACF of Residualss")

#errors have no dependency on lag theirfore MA(3) is a reasonable fit

#tsdiag does the same we need not calc the errors too
tsdiag(ma3)
tsdiag(ar2)


#Ljung And Box test
#CHI df m-(p+q)
#eg ma(3)
Box.test(e,lag=5,fitdf=3,type="Ljung")
#insufficient evidence to reject h0
#H0: Residuals are indep

#since p-value>5% theirfore errors are indep


#EG 2
e2 <- ar2$residuals
Box.test(e2,lag=10,fitdf=2,type="Ljung")




#Akaikes Information criteria

#jitna chota utna model is a good fit
d
#ARMA(2,1)
arima(d,order=c(2,0,1))

#ARMA(1,2)
arima(d,order=c(1,0,2))  #better fit
