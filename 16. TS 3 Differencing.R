#ANALYSING STATIONARITY

#Differencing

#Least square Trend Removal
getwd()

test.stationary <- read.table("testing.stationarity.txt")
head(test.stationary)
str(test.stationary)

test.st <- ts(test.stationary)


#Question 1
#Plot the sample ACF for the time series data test.stationarity,
#and hence conclude , whether this data should be differenced or not.



par(mfrow=c(1,1))
acf(test.st,main="Sample ACF of Data",ylab="Sample ACF") #slow decay , therefore need to be differenced




#Question 2 
#For the Pp(philips-perron)test , we have:
#H0: The time Series has a unit root (and hemce can be differenced).
#H1: The time series does not need to be differenced.


#PP Test(Philips-Perron)

#P-value<5% sufficient evidence to reject H0

PP.test(test.st) #therefore diferencing requiired




#Question 3
#Run the following code:
set.seed(1901)
n=365
data <- arima.sim(list(ma=c(-1.4,0.8)),n)
#plot the sample ACF for the time series object data , and 
#carry out the PP tesst , state your conclusion.

acf(data,main="Sample ACF of Time Series data",ylab="Sample ACF")  #rapid decay , therefore diff not required
PP.test(data)  # p-value = 1%  therefore no diff


#Differencing
?diff


Xt <- diff(test.st,lag=1,differences = 1)
Xt

test.st

#Xt=(1-B)test.st = nabla(test.st)

par(mfrow=c(2,2))


plot(test.st,main="Original Time Series")
plot(Xt,main="Differenced Time Series",ylab="diff values")
acf(test.st,main="Sample ACF of Original Series",ylab="Sample ACF")
acf(Xt,main="Sample ACF of Diff Series",ylab="Diff ACF")


#therefore here differencing a good approach

par(mfrow=c(1,1))


PP.test(Xt)


#choosing d using variance
var(test.st)
var(Xt)  #var will decrease until stationarity after that again increasses


d2t <- diff(test.st,lag=1,differences = 2)
#d2t <- (1-B)^2 Test.st

d2t <- diff(Xt,lag=1,differences = 1)
d2t
var(d2t)  # var increases therefore no more diff rreq


#Least square trend removal

#Question4
set.seed(123)
n=1000
sim <- arima.sim(list(ar=0.9),n)
xt <- 2000+cumsum(sim)

ts.plot(xt)


time <- seq(1,1000)

fit <- lm(xt~time)   #Y=a+bX  # here the input is time
names(fit)

fit$fitted.values
fit$coefficients


#residuals
fit$residuals #actual and differnced or fitted 
xt


par(mfrow=c(2,1))
ts.plot(xt,main="Time Series with linear trend")

lines(time,fit$fitted.values,type="l",col="blue")

plot(fit$residuals,ylab="REsiduals",main="PLOT of Residuals",type="l")
