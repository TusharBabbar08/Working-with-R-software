#EVT
#GPD Distribution

#Threshhold Excedence

data <- read.table("EVTdata.txt",header=TRUE)
head(data)

u <- 10000
x <- data[,3]
x

x <- x[x>u]
x

te <- x-u
te

#Alternative method

x <- data[,3]
te <- x[x-u]-u
te







#Fitting a GPD distribution
# we are asked to fit a pareto distb to the threshhold exceedances of the claim 
#data above , for a thres u>10000.
#Find the MLE of the parameters of this distb
#using starting values of beta=gamma=1




beta <- 1
gamma <- 1
p <- c(beta,gamma)

fMLE <- function(params) {
  f <- 1/params[1]*((1+te/(params[2]*params[1]))^-(params[2]+1))
  lnf <- log(f)
  sum(-lnf)
}

MLE <- nlm(fMLE,p)
MLE

beta <- MLE$estimate[1]
gamma <- MLE$estimate[2]


#HISTOGRAM
hist(te,freq = FALSE,xlab="THRESHHOLD EXCEEDANCE",main="HISTOGRAM vs fitted GPD distb")

x <- seq(0,35000)
GPD <- 1/beta*((1+x/(gamma*beta))^-(gamma+1))


lines(x,GPD)


#Empirical Density Function

plot(density(te,from=min(te),to=max(te)),xlab="THreshhold Exceedances",main="Empirical Density Function vs Fitted GPD ")
lines(x,GPD,col="red")


#QQplot

rGPD <- function(n,g,b) {
  rp <- g*b*((1-runif(n))^(-1/g)-1)
  rp
}


set.seed(31)
x <- rGPD(1000,gamma,beta)
x


qqplot(x,te,xlab="Quantiles from fitted distb",ylab="quantiles from sample data",main="QQ plot")
abline(0,1,col="red")
#