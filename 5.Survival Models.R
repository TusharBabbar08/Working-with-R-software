#Survival Models
a0 <- 0.00005887
a1 <- -0.0004988
b0 <- -4.363378
b1 <- 5.544956
b2 <- -0.620345
mu <- function(x){
  t=(x-70)/50
  M=a0+a1*t + exp(b0+b1*t+b2*(2*t^2-1))
  return(M)
}

mu(80)

#i(d)
plot(mu) #by default it takes the value of x from 0 to 1

plot(mu , from = 17 , to= 119)
plot(mu,17,119,xlab="Age(x)",ylab="mu(x)", main="Graph of mu(x) for AM92 table")

plot(mu,17,119,log="y" , xlab="Age(x)",ylab="mu(x)", main="Graph of mu(x) for AM92 table")







x <- seq(20,110,10)
x

fexp <- function(x){
  1-exp(-mu(x+0.5))
}
fexp(20)
fexp(30)
#sapply this will apply the function to a vector

qx <- round(sapply(x,fexp),6)
qx
QX <- data.frame(x,qx)
QX

#curtate expectation
#ex=summation(tpx) from t=1 to t=infinity


#e25
119-25
#1p25+2p25+3p25+..........94p25
#1p25+1p25*1p26+1p25*2p26..


age=25
s=0
npx=1
for(i in 1:(119-age)){
  px <- 1-fexp(age+i-1)
  npx <- npx*px
  s <- s+npx
}
s

#1st time
#px=1p25
#npx=1*1p25 = 1p25
#s=1p25

#2nd time
#px=1p26
#npx=1p25*1p26
#s=1p25+1p25*1p26


#3rd time
#px=1p27
#npx=1p25*1p26*1p27
#s=