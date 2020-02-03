#Importing a Time Series Data


getwd()
Xt <- read.table("Xt.csv")
head(Xt)


#seperator
Xt <- read.table("Xt.csv",sep=",")
head(Xt)

str(Xt)

#JAN 2000
xt <- ts(Xt,start=c(2000,1),frequency = 12)

plot(xt)

# for weekly data = 52
# for quarterly data = 4
#for annual data =1

#March 1950
xtquart <- ts(Xt,start=c(1950,3),frequency = 4)
plot(xtquart)


#Graphs of Time Series
plot(xt,main="Time series Xt",ylab="sale",col="red")

ts.plot(Xt,main="Time series Xt",ylab="sale",col="red")
ts.plot(xt,main="Time series Xt",ylab="sale",col="red")

points(xt,col="black",cex=1)  #cex is for size



#PLOTTING SAMPLE ACF AND SAMPLE PACF
xt <- ts(Xt,start=c(2000,1))
acf(xt)
acf(xt,lag.max=18,main="this is the graph of Samplle ACF for Xt ",ylab="Sample ACF")

pacf(xt,lag.max=18,main="this is the graph of Samplle PACF for Xt ",ylab="Sample PACF")


#The blue dotted lines indicate 95%confidence interval for ACF and PACF 
#The value falling inside the CI are Statistically insignificant(ACF=PACF=0 for those lags)


#plotting theoritical ACF and PACF
#Yt=0.5Yt-1 - 0.1Yt-2 +et + 0.2et-1

modelacf <- ARMAacf(ar=c(0.5,-0.1),ma=0.2,lag.max=15)
modelacf


modelpacf <-  ARMAacf(ar=c(0.5,-0.1),ma=0.2,lag.max=15,pacf=TRUE)
modelpacf


#BARPLOT
barplot(modelacf[-1],main="ACF of arma(2,1",col="red",xlab="lag",ylab="ACF")
barplot(modelpacf,main="PACF of arma(2,1",col="blue",xlab="lag",ylab="PACF",names=1:15)


par(mfrow=c(2,1))

#extracting key numbers
frequency(xt)
start(xt)
end(xt)


#Calculations of Sample ACF and PACF
a <- acf(xt,lag.max=18,main="this is the graph of Samplle ACF for Xt ",ylab="Sample ACF",plot=FALSE)
a
a[5]
a$acf[5]
p <- pacf(xt,lag.max=18,main="this is the graph of Samplle PACF for Xt ",ylab="Sample PACF",plot=FALSE)
p
p[5]
p$acf[5]
