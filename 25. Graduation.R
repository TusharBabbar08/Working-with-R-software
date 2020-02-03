#Graduation
data <- read.table("Graduation2.csv",header=TRUE,sep=",")
data
head(data)

data$CRUDE=data$DEATHS/data$ETR
head(data)

#ln(mux) = log(B) + x*log(C)
#y= a + b*x


#lm(Y~X)
gompertz <- lm(log(data$CRUDE)~data$AGE)
gompertz


names(gompertz)
a <- as.numeric(gompertz$coefficients[1])
b <- as.numeric(gompertz$coefficients[2])
B <- exp(a)
c <- exp(b)
c


#ii c
data$GRADUATED <- round(B*c^data$AGE,6)
head(data)


#ii d

plot(data$AGE,data$CRUDE,xlab="Age",ylab="Mortality Rates",main="Crude v s Graduated")
lines(data$AGE,data$GRADUATED)


#iii a

x <- 1:10
y <- x[-1]-x[-length(x)]

diff <- function(v)v[-1]-v[-length(v)]
#first order diference


diffcrud <- round(diff(diff(diff(data$CRUDE))),6)
diffcrud

diffgrad <-  round(diff(diff(diff(data$GRADUATED))),6)
diffgrad



cbind(data$AGE,diffcrud,diffgrad)


data$EXPECTED <- round(data$ETR*data$GRADUATED,2)
head(data)

data$ZX <- round((data$DEATHS-data$EXPECTED)/sqrt(data$EXPECTED),3)
head(data)



plot(data$AGE,data$ZX,xlab="age",ylab="ZX",main="ISD" , type="b")
length(data$ZX[data$ZX>-2&data$ZX<2])

7/51

length(data$ZX[data$ZX>0])
33/51
