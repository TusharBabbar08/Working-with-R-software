#Identifying Seasonality

#Removing Seasonality

# we will use a inbuilt death data named ldeaths

ldeaths
#Method 1

plot(ldeaths)
par(mfrow=c(1,1))

#seasonality trend may be there occuring after 12 months

#Method 2 acf plot

acf(ldeaths)


#Spectrum

spectrum(ldeaths,main="Periodogram",xlab="Corresponding deaths frequesncy")


#Combining plots

m <- matrix(c(1,2,1,3),2,2)
m
x=layout(m);x
plot(ldeaths)
acf(ldeaths)
spectrum(ldeaths,main="Periodogram",xlab="Corresponding deaths frequesncy")



par(mfrow=c(1,1))




#Removing Seasonality Trend

#Differencing with lag 12

sdiff <- diff(ldeaths,lag=12,differences = 1)
sdiff


par(mfrow=c(2,2))

plot(ldeaths,main="Original Time Series")
acf(ldeaths,main="Sample ACF OF originL ts")

plot(sdiff,main="Differenced Time Series")
acf(sdiff,main="Sample ACF OF Differenced ts")
