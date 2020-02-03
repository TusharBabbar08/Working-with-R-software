par(mfrow=c(1,1))
plot(sin,-pi,pi)
x=seq(-pi,pi,0.001)
plot(x,cos(x),type="s")


plot(1:26,rep(1,26),pch=0:25)
text(1:26,0.95,as.character(0:25))

plot(x,sin(x),main="Graph of sinx")
lines(x,cos(x))

ts.plot(diff(ldeaths))
