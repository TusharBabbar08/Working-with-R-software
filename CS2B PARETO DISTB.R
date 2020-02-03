#PARETO DISTRIBUTION
#X~pareto(alpha,lambda)
#PDF     f(x)=alpha*lambda^alpha/(lambda+x)^(alpha+1)
#CDF     F(x)=P(X<=x)=1-(lambda/(lambda+x))^alpha

dpareto <- function(x,alpha,lambda){
  fx <- (alpha*lambda^alpha/(lambda+x)^(alpha+1))
  return(fx)
}
dpareto(5,10,100) #f(5) for Pareto(10,1000)

ppareto <- function(x,alpha,lambda){
  Fx <- 1-(lambda/(lambda+x))^alpha
  return(Fx)
}
ppareto(5,8,200)  #P(x<=5) fro Pareto(8,200) dst

runif(100)  #U(0,1)
 
set.seed(80)
rpareto <- function(n,alpha,lambda){
  u <- runif(n)
  x <- lambda*((1-u)^(-1/alpha)-1)
return(x)
} 

rpareto(200,5,200) #this will generate 200 random numbers from Pareto(5,200)dist

 