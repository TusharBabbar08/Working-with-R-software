#USE OF PACKAGES
#1. MARKOVCHAIN
install.packages("markovchain")
library("markovchain")
TPM <- new("markovchain",states=c("H","S","D"),transitionMatrix=matrix(c(0.2,0.3,0.4,0.5,0.5,0.5,0.3,0.2,0.1),nrow=3),name="P1")
TPM
TPM^2
TPM^100


Initialstate=c(1,0,0)
After3months=Initialstate*TPM^3
After3months


#2. SURVIVAL 
#survfit(formula, conf.int=0.95, conf.type="log")
install.packages("survival")
library(survival)
time <- c(3,4,6,11,11,17,21,24,25,26,rep(30,5))
delta <- c(1,1,0,1,1,0,1,0,0,1,rep(0,5))

time
delta

a <- Surv(time,delta)# creates a survival type object
a
summary(a)
?Surv

fit <- survfit(a~1, conf.int=0.95, conf.type="log")
fit
summary(fit)

plot(fit)

fit <- survfit(Surv(time,delta)~1)
summary(fit)
plot(fit,xlab="time",ylab="survival function",main="KM estimate of survival function")

fit$n.risk       #nj
fit$n.event      #dj

#Nelson AAlen

L <- cumsum(fit$n.event/fit$n.risk)   #Capital lambda
L  

Sna <- exp(-L)
Sna


data <- read.table("CS2B_survival.csv", sep="," , header=TRUE)
head(data)


time <- data$ï..DUR
head(time)


delta <- data$EVENT

a <- Surv(time,delta)
a


fit <- survfit(Surv(time,delta)~1)
summary(fit)


plot(fit,xlab="time",ylab="survival function",main="KM estimate of survival function")


#(ii)
coxfit <- coxph(Surv(time,delta)~AGE + URBAN + POOREST + POOR + MIDDLE + RICH + YEAR + LOWER,data=data)
coxfit


#H0: b1 = b2 = b3 = ...=b8 = 0

#p-value < 5%  , that is a significant covariate


#(iii)
# AGE is the only input variable
coxfit1 <- coxph(Surv(time,delta)~AGE ,data=data)
coxfit1


#Adding LOWER to the model
coxfit2 <- coxph(Surv(time,delta)~AGE + LOWER,data=data)
coxfit2
coxfit2$loglik

2*(-27871.87 + 27882.09)

?qchisq
qchisq(0.95,1)
?coxph

31.66-20.45
qchisq(0.95,6)
# first is better since 


