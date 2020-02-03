#Machine Learning

plot(NULL,NULL,xlim=c(0,100),ylim=c(0,100),xlab="Factor 1",ylab="Factor 2")

polygon(c(40,40,100,100),c(40,100,100,40),border="red",lwd=2)
polygon(c(10,10,70,70),c(90,30,30,90),border="blue",lwd=2)
polygon(c(0,0,60,60),c(80,0,0,80),border="green",lwd=2)




#(ii)
sample(40:100,10,replace=TRUE)
set.seed(19)
type1 = matrix(c(1:10,sample(40:100,10,replace=TRUE),sample(40:100,10,replace=TRUE),rep(1,10)),10,4)
type1



type2 = matrix(c(11:20,sample(10:70,10,replace=TRUE),sample(30:90,10,replace=TRUE),rep(2,10)),10,4)
type2



type3 = matrix(c(21:30,sample(0:80,10,replace=TRUE),sample(0:60,10,replace=TRUE),rep(3,10)),10,4)
type3


?rbind

simData <- rbind(type1,type2,type3)
simData

colnames(simData)=c("S.No.","Factor1","Factor2","Type")
simData


#(iii)
?kmeans


model <- kmeans(simData[,2:3],3)
model


#Cluster 1 corresponds to Type 3
#Cluster 3 corresponds to Type 1

4-model$cluster  #Type 1
model$cluster

21/30


correct <- length(which(4 - model$cluster == simData[,4]))
c(correct,correct/length(simData[,4]))


model$cluster
model
model$centers


#checking distance of 50,50 from each coordinate
x1 <- model$centers[,1]
y1 <- model$centers[,2]

x2 <- 50
y2 <- 50

sqrt((x2-x1)^2 + (y2-y1)^2)


#cluster 2 has the nearest centeroid




#___________________________________________________

#(iv)
f <- function(x){
  ifelse(x[2]<10 | x[3]< 30,3,ifelse(x[2]>70 | x[3] > 90 , 1 ,2) )
}

f(c(NA,50,50,NA))


model1 <- apply(simData,1,f)
model1

simData[,4]







#______________________________________
f1 <- function(x){
  ifelse(x[2]<40,0,1/(100-40+1))*ifelse(x[3]<40,0,1/(100-40+1))
}


f2 <- function(x){
  ifelse(x[2]<10 | x[2]> 70 ,0,1/(70-40+1))*ifelse(x[3]<30 | x[3]> 90,0,1/(90-40+1))
}

f3 <- function(x){
  ifelse(x[2]>80,0,1/(80+1))*ifelse(x[3]>60,0,1/(60+1))
}


#P(a/Bi) - likelihood
#P(Bi)-prior
#P(Bi/A) - posterior


post <- function(x){
 postB1 <- 0.5*f1(x)
  postB2 <- 0.3*f2(x)
  postB3 <- 0.2*f3(x)
  return(c(postB1,postB2,postB3))
} 

post(c(NA,50,50,NA))


fmax <- function(x){
  postB1 <- 0.5*f1(x)
  postB2 <- 0.3*f2(x)
  postB3 <- 0.2*f3(x)
  m=max(c(postB1,postB2,postB3))
  ifelse(postB1==m,1,ifelse(postB2==m,2,3))
}


model12 <- apply(simData,1,fmax)
model12
simData[,4]



correct2 <- length(which(4 - model12 == simData[,4]))
c(correct2,correct2/length(simData[,4]))

