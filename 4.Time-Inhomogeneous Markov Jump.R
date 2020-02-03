#TIME IN-HOMOGENEOUS MARKOV JUMP(function of time)
mu11 <- function(t) {
        -t
}
mu11(1)
mu11(0.5)
mu12 <- function(t) {
     0.4*t
}
mu12(1)
mu32 <- function(t) {
      1.2*t
}
mu32(1)
mu13 <- function(t) {
     0.6*t
}
mu13(1)
mu21 <- function(t) {
  0.5*t
}
mu22 <- function(t) {
  -1.5*t
}
mu23 <- function(t) {
  t
}
mu31 <- function(t) {
  0.8*t
}
mu33 <- function(t) {
  -2*t
}
At <- function(t) {
  N=matrix(c(mu11(t),mu21(t),mu31(t),mu12(t),mu22(t),mu32(t),mu13(t),mu32(t),mu33(t)),3,3)
  return(N)
}
At(1)
#METHOD 2
A2t <- function(t) {
  A=matrix(c(-t,0.5*t,0.8*t,0.4*t,-1.5*t,1.2*t,0.6*t,t,-2*t),3,3)
  return(A)
}

A2t(2)

#-------------------------------------------------------------------
# short time transition probabilities for inhomogeneous markov jump
#Pij(s,s+h) = ! + h*muii(s) + o(h) when i=j
#Pij(s,s+h) = h*muij(s) + o(h) when i not equal to j

tpms <- function(s,h) {
  ph=diag(3)+A2t(s)*h
  return(ph)
}
tpms(5,1/1000)
#)____________________________
#for long duration or General Probabilities pij(s,t)
#from time 3 to 4 with h=0.1
#s=3,t=4,h=0.1

tpml <- function(s,t,h) {
r=diag(3)
for (i in 1:((t-s)/h)-1) {#to make the loop run for 500 times only we do a minus 1 else we can start the loop from 2
  r=r%*%tpms(s,h)
  s=s+h
}
return (r)
}
tpml(5,10,0.01)#going from 5 to 10 with the steps of 0.01
#________________________________________________________________________
tpml=function(s,t,h){
  ph=diag(3)+A2t(s)*h
  m=ph
  for(j in 1:((t-s)/h)-1) {
    ph=diag(3)+A2t(s+h*j)*h
    m=m%*%ph
  }
  return(m)
}
tpml(3,5,0.01)