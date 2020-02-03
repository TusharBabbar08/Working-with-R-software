#Mortality Projection
data <- read.table("ELT.csv",header=TRUE,sep=",")
head(data)
#aplhax = ultimae reduction facton
#fn,x=function of n and x

alpha <- function(x){
  min(max(0.13+0.87*(x-60)/50,0.13),1)
}

fnx <- function(x){
  min(max(0.55-0.26*(x-60)/50,0.29),0.55)
}


Rxt20 <- function(x){
  alpha(x)+(1-alpha(x))*(1-fnx(x))
}

?sapply


age <- seq(0,100,10)
qx2011 <- data$ELT15*sapply(age,Rxt20)
qx2011


Rxt30 <- function(x){
  alpha(x) + (1-alpha(x))*(1-fnx(x))^(30/20)
}

qx2021 <- data$ELT15*sapply(age,Rxt30)
qx2021



cbind(data,qx2011,qx2021)


#iv a
plot(age,data$ELT17[data$SEX=="M"],log="y",type="l",xlab="ages",ylab="Mortality",main="MOrtality rates for males acc to ELT17 table")
lines(age,qx2011[1:11],col="red",lty="dotted")

legend(10,0.1,c("ELT17","Projected"),pch=c("-","-"),col=c("black","red"))

