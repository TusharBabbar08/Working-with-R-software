#Copulas

#(ii)b
set.seed(17)
n <- 1000
u <- runif(n)
v <- runif(n)


density <- function(u,v){
  3*u^(-3)*v^(-3)*(u^-2 + v^-2 - 1)^(-1/2-2)
}

d <- density(u,v)
d
head(d)


#(ii)c
maxval <- max(d)
maxval


prob <- d/max(d)
prob


#(ii)d
random <- runif(n)


fulldata <- cbind(u,v,prob,random)
fulldata
head(fulldata)


subset <- fulldata[random<=prob,]
head(subset)
dim(subset)
dim(fulldata)



#(iii)
plot(subset[,1],subset[,2],xlab="u",ylab="v",main="Simulation of Clayton Copula with alpha =2")


#(iv)
cor(subset[,1],subset[,2])   #by defualt is pearson
round(cor(subset[,1],subset[,2],method="pearson"),2)
round(cor(subset[,1],subset[,2],method="spearman"),2)
round(cor(subset[,1],subset[,2],method="kendall"),2)


#(v)
#F(x)=u

qnorm(0.975)  #this will give the value of x for which P(N(0,1)<=x)


x <- sapply(subset[,1],qnorm)
head(x)
qnorm(0.1188054)


y <- sapply(subset[,2],qnorm)
head(y)


plot(x,y,main="Scatterplpot of random variables X and Y")
