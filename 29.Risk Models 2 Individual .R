#Individual Risk Model
#the number of risks n is fixed
#risks are independent
#the number of claims on each risk is either 0 or 1.
#Also recall that claim amounts need not be identically distributed for the
#individual risk model to apply.

#S = Y1 + Y2 + ...+Yn
#Y1=0 or Y1= X11


#Company A has sold 1,000 independent life insurance policies.
#250 of these policies were sold to people who smoke, and the remaining 750 policies
#were sold to non-smokers.

#IF a claim is paid on a policy, the size of that claim follows a gamma distribution
#such that X~Ga(200, 0.8) for policyholders who smoke, and
#Y~Ga(300, 0.4) , for policyholders who don't smoke.
#The probability of death occurring during the term of the policy is qs=0.002 for
#smokers and qn =0.001 for non-smokers.
#We're going to generate a sample of 500 aggregate claim amounts from this
#portfolio of policies.

a.s <- 200
a.ns <- 300

l.s <- 0.8
l.ns <- 0.4


qs <- 0.002
qn <- 0.001

x <- rep(0,1000)
death <- rep(0,1000)

death

for(i in 1:250){
  x[i] <- rgamma(1,shape=a.s,rate=l.s)
}

for (i in 251:1000){
  x[i] <- rgamma(1,shape=a.ns,rate=l.ns)
}


for (i in 1:250){
  death[i] <- rbinom(1,1,prob = qs)
}


for (i in 251:1000){
  death[i] <- rbinom(1,1,prob = qn)
}


death

x[99]
death[99]


y <- x*death
y


y[99]


s <- sum(y)
s


#For 500 values
#Exam
set.seed(1000)
S <- rep(0,500)
for(j in 1:500){
  
for(i in 1:250){
  x[i] <- rgamma(1,shape=a.s,rate=l.s)
}

for (i in 251:1000){
  x[i] <- rgamma(1,shape=a.ns,rate=l.ns)
}


for (i in 1:250){
  death[i] <- rbinom(1,1,prob = qs)
}


for (i in 251:1000){
  death[i] <- rbinom(1,1,prob = qn)
}
y <- x*death
S[j] <- sum(y)
}

S

mean(S)
var(S)
sd(S)


#P(S>500)
length(S[S>500])/length(S)

median(S)
quantile(S,0.95)



#Excess of Loss Reinsurance

#M=500

i <- pmin(x,500)
i

x[x<=200]
Si <- sum(i)
Si

r <- pmax(0,x-500)
r
SR <- sum(r)
SR
