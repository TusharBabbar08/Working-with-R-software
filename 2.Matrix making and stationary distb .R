#Creatting matrices in R
P <- matrix(0,nrow=2,ncol=2)
P <- matrix(0,2,2)
P

P <- matrix(c(1,2,3,4),2,2)
P <- matrix(c(1,2,3,4),3,3)
P
P <- matrix(0,5,5)
P
P[1,1]=1
P

#GAmbler Question
X0 <- c(0,0,1,0,0)
X5 <- X0%*%P%*%P%*%P%*%P%*%P
X5
X5[1]=0.375
X5[2]=0.125
X5[4]=0.125
X5[5]=0.375
X5


R=matrix(c(0.8,0.5,0.2,0.5),2,2)
R
rowSums(R)

M <- R%*%R%*%R
M
M%*%M #This will give R^6
d <- diag(2)
d
Rn <- function(R,n) {
  d=diag(2)
  for (i in 1:n) {
    d=d%*%R
  }
  return(d)
}
Rn(R,3)


#Stationary Distribution -----Pi*P=Pi

Rn(R,20) #since value becomes constant
Rn(R,10)

Q <- Rn(R,20)
Q
S <- c(Q[1,1],Q[1,2])
S

#recommended method for stationary distribution
initial <- c(1,0)
for(i in 1:30){
  initial=initial%*%R
}
initial

