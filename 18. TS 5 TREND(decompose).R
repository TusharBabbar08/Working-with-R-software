#Using inbuilt functions to seperate out trend
#seasonality and white noise

#core reading has only decompose
#alternative is stl

ldeaths


plot(decompose(ldeaths,type="additive"))
  
d <- decompose(ldeaths,type="additive")

d$x          
s <- d$seasonal;s
trend <- d$trend


par(mfrow=c(1,1))

#First principle approach
plot(ldeaths)
points(ldeaths,cex=0.7,col="red")

lines(trend,col="blue")
lines(s+trend,col="green")





#stl Function

plot(stl(ldeaths,s.window="periodic"))
stl(ldeaths,s.window="periodic")
