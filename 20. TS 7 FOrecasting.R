#Time Series Forecasting

#The Box-Jenkins method
#Exponential Smoothing

#Example 1- stationary data
#First lets simulate some time series data. The following code generates 1000
#observations from a stationary AR(p) process with unknown order p.
#(Dont worry about what does this code does , it has been purposely made difficult 
#so that you cant easily see what model has been fitted)

set.seed(476)
abc <- sort(rbinom(3,1,0.6),decreasing=TRUE)*c(0.86,-0.4,0.15)
data <- 50+arima.sim(list(ar=abc),n=1000)


#(i)Fit an Autoregressive mode to this data.
#arima(p,0,0)
#ar  here ar is better since we do not know p 


fit <- ar(data)
fit
#Xt=mu + a1(Xt-1 - mu) +a2(Xt-2 - mu) + et
fit$x.mean



#(ii)Forecast the next 20 values
#BOX JENKINS

p <- predict(fit,n.ahead=20)
p

#(iii) Extract the estimated value of the time series at t=1010 and 1015

p$pred[10]
p$pred[15]
p$se[19]


par(mfrow=c(1,1))
#plolt of 1000 original values and 20 predicted values
plot(data,xlim=c(0,1020),main="Forecasting example",ylab="Data values")
lines(p$pred,col="red")




#__________________________________________________________________________________________________


#Example 2 - non- Stationary Data
#Recall that if the time series is not stationary ,
#we must first obtain a stationary database by removing the trends from the data.
#Then we can fit a time seris model to result.

#The code below stimulates on ARIMA(0,1,2) process , recorded daily
#fromm 1st Jauary 2015 for 4 years.

set.seed(476)
a <- runif(1,0.6,0.8)
b <- runif(1,-0.3,1)
data <- 50+cumsum(arima.sim(list(ma=c(a,b)),n=1460))
data <- ts(data,start=c(2015,1),frequency=365)


#(i)Difference the ARIMA(0,1,2) series and fit a MA(2) to the result.

d <- diff(data)

fit2 <- arima(d,order=c(0,0,2))
fit2



#(ii)Write the code required to forecast the vallue of the time series for each of the 180 days

p2 <- predict(fit2,n.ahead=180)
p2$pred


data
data[1460]
tail(data,1)



pwithtrend <- cumsum(p2$pred) + tail(data,1)
plot(data)
ts.plot(pwithtrend)


end(data)
frequency(data)


pwithtrend <- ts(pwithtrend,start=c(2019,1),frequency = 365)
plot(pwithtrend)


plot(data,xlim=c(2015,2019+0.75))
lines(pwithtrend,col="red")





#_________________________________________________________________________________________
#EXponential Smoothing

xt <- read.table("forecasting.csv")
head(xt)
tail(xt)

Xt <- ts(xt,start=c(2017,20),frequency = 365)
Xt


end(Xt)

#154th day of 2018 = 3rd June 2018
#Forecast using exponential smoothing

HW <- HoltWinters(Xt,alpha=0.7,beta=FALSE,gamma=FALSE)
?HoltWinters
HW



predict(HW,n.ahead=1)  #Forecasted for 4th June

predict(HW,level=0.95,prediction.interval=TRUE)





#Estimate the value on 8th June 2018
predict(HW,n.ahead=5)


#Calculate 95% CI for the value of the time series on 4th June 2018 , using optimaml smoothing Parameter



HW2 <- HoltWinters(Xt,beta=FALSE,gamma=FALSE)
HW2
predict(HW2,prediction.interval = TRUE)
