#MAXIMUM LIKELIHOOD ESTIMATE
#Run the following code
set.seed(10249)
yclaims <- pmin(rexp(10000,rgamma(1,3,runif(1,580,620))),1000)

#The vector  yclaims denotes the amounts paid by an insurer for a book of general
#insurance business , net of reinsurance.
#The reinsurance is an excess of loss arrangement with retension limit M=1000
#so that the insurer pays a maximum of 1000 for any individual claim.
#Hence , many claims in yclaims are exactly equal to 1000.
#You believe that the underlying claim size random variable X(before reinsurance)
#is exponentially distrib with unknown parameter l.
#(i)Calc the MLE of l
#(ii)Plot a graph of your fitted distb against the empirical density plot of the data yclaims.
#your axis should extend from 100 to 1000 and label your graph appropriately
#(iii)Comment on the key features of the graph.

yclaims

m <- length(yclaims[yclaims==1000])
m


n <- 10000-m
n


#solving method

logL <- function(l){
  fx <- -n*log(l) + l*(sum(yclaims))
  return(fx)
}
logL(1)

nlm(logL,5)
nlm(logL,50)



#directly coding the log likelihood


y <- yclaims[yclaims<1000]

lnl <- function(l){
  f <- -sum(dexp(y,l,log=TRUE))-m*pexp(1000,l,lower=FALSE,log.p=TRUE)
  return(f)
}

lnl(1)

a <- nlm(lnl,5)
l <- a$estimate




#(ii)
#plot

plot(density(yclaims,from=100,to=1000),xlab="Insurers claim payment",main="Empirical density function")



x <- seq(100,1000,0.001)
pd <- dexp(x,l)

lines(x,pd,col="green")
#__________________________________________________________________________________________________-

#Designing a reinsurance arrangement
#X~Exp(0.005)

set.seed(100)
x <- rexp(10000,0.005)
x
max(x)
min(x)

M <- 800

z <- pmax(0,x-M)
sum(z)

zprofit <- function(premium){
  total <- sum(z)
  profit <- premium-sum(z)
  return(abs(profit))
}

zprofit(40000)


nlm(zprofit,1000)
