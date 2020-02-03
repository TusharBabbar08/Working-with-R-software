#EXCESS OF LOSS REINSURANCE
#Claims from a particular portfolio of insurance policies are expected to follow
#a Pareto distribution with parameters a ??? 2 and I ??? 800 . An individual excess of loss
#^arrangement is in force where the insurer pays a maximum of 1,500 on any claim.
#(i) Simulate 10,000 claims from this portfolio using seed value 5
#(ii) Estimate the mean and variance of the amount 
#paid by the insurer and the reinsurer
#in respect of a single claim. *
#(iii) Estimate the probability that a claim involves the reinsurer.
#(iv) Estimate the reinsurer's mean claim amount, for claims on which it is involved.
#Estimate the insurer's median claim amount, net of reinsurance.



#(i)
set.seed(5)
x <- rpareto(10000,2,800)
x

min(x)
M <- 1500
y <- pmin(x,M)
y

x[20]
y[20]


z <- pmax(0,x-M)
z
x[3]
y[3]
z[3]


#(ii)
mean(y)
var(z)

mean(z)
var(z)


#(iii)
zinv <- z[z>0]
zinv
sum(zinv)


z1 <- x[x>M]
z1


length(zinv) # wherever reinsurer is involved
length(zinv)/length(x)  #prob = 12.31%


#(iv)
mean(zinv)
mean(z)


#(v)
y

quantile(y,0.25)
quantile(y,0.75)
quantile(y,0.5)



median(y)



#assess the effect of reinsurance

(mean(y)-mean(x))/mean(x)

(var(y)-var(x))/var(x)






#INFLATION
#Y=min(KX<M)   here impact is less
#Z=max(KX-m,0)   here impact is greater than the inflation rate


#Inflation rate is 10% pa

y1 <- pmin(1.1*x,M)
y1

y[1]
y1[1]


z1 <- pmax(1.1*x-M,0)
z1


x[3]
z[3]


mean(y1)
mean(y)


#%change

((mean(y1)-mean(y)))/mean(y)

((mean(z1)-mean(z)))/mean(z)


#suppose retenson limit is also increasing by 10% pa
M1 <- M*1.1
M1


y2 <- pmin(1.1*x,M1)
y2

((mean(y2)-mean(y)))/mean(y)


z2 <- pmax(1.1*x-M1,0)
z2

((mean(z2)-mean(z)))/mean(z)
