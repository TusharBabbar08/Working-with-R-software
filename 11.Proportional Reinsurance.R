#Proportional Reinsurance

#Simulate 1000 values from a logN(7,1) using seed 50 and store them in an object x

set.seed(50)
x <- rlnorm(1000,7,1)
x


#Insurance claims from a particualr portfolio follow a lognormal dsit with 
#paraeters mu=7 and sigma=1 .
#A Propofrtional Reinsurance model is in force with a retained proportion of 80%
#Estimate the mean and variance of the amount paid by the insurer and the reinsurer for a single claim.


#Y=aX    Z=(1-a)X

y <- 0.8*x
y

z <- 0.2*x
z
?rlnorm

#mean
mean(y)
mean(z)

mean(x)
mean(y)+mean(z)


#var
var(y)
var(z)

sd(y)
sd(z)



#assess the effect of reinsurance

#% decrease in mean amount

((mean(y)-mean(x))/mean(x))*100


#% decrease in sd

((sd(x)-sd(y))/sd(x))*100


#varY = a^x varX
#sd(y)=asd(x)