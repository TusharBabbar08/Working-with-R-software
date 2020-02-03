#Markov Jump4
A=matrix(0,3,3)
A
A[1,2]=0.12
A[1,3]=0.2
A[1,1]=-0.12-0.2
A[2,1]=0.18
A[2,3]=0.05
A[2,2]=-0.18-0.05
A[3,1]=0.013
A[3,2]=0.05
A[3,3]=-0.013-0.05
A


#pij(h) = I + H*A
h <- 1/12
ph=diag(3) + h*A
ph



m=diag(3)
for(i in 1:120){
  m=m%*%ph
}
m



_____________________________________________
#markov Jump HSTD diagram 
A <- matrix(c(-0.07,0.02,0,0.05,1,-1.2,0.15,0.05,0,0,-0.4,0.4,0,0,0,0),4,4,byrow=TRUE)
A
rowSums(A)

#p(1/365)
#P1 ; transition probibilities for 1 day
p1 <- diag(4)+1/365*A
p1
rowSums(p1)

#P1 ; transition probibilities for 1 month
M=diag(4)
Pn <- function(p1,n){
  for(i in 1:n){
    M=M%*%p1
  }
  M
}
P30 <- Pn(p1,30)
P30

#prob that he will be sick after 1 moonth provided he is hhealthy today
P30[1,2]
