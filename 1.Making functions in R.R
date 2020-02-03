#Making Functions in R
mean(25,57,35)



circarea <- function(r){
  area=pi*r^2
  return(area)
}
circarea(5)
circarea(10)

x <- c(1,2,3,4,5)
x <- 1:5
circarea(x)

circlestat <- function(r){
  area <- pi*r^2
  cf <- 2*pi*r
  return(c(Area=area,CF=cf))
}
circlestat(5)
circlestat(x)


circlestat <- function(r){
  area <- pi*r^2
  cf <- 2*pi*r
  return(list(Area=area,CF=cf))
}
circlestat(x)


circlestat <- function(r){
  area <- pi*r^2
  cf <- 2*pi*r
  return(data.frame(Radius=r,Area=area,CF=cf))
}
circlestat(x)


#PDF of poisson
#p(X=x)=exp(-mu)*mu^x/x!
poispdf <- function(x,mu){
  prob <- exp(-mu)*mu^x/factorial(x)
  return(data.frame(X=x,MU=mu,PROB=prob))
}
poispdf(3,6)
poispdf(c(1,2,4,8,10),c(1,3,5,2,9))