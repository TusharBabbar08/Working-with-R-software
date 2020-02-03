#Exposed to Risk

data <- read.table("FuneralData.csv",header=TRUE,sep=",")
head(data)

max(data$LIFE)
summary(data)
tail(data)


#(ii)c
date70 <- data$BIRTH+70
head(date70)


date0 <- 2013.000
data$ENTRY

startdate <- pmax(date70,data$ENTRY,date0)
head(startdate)
data[1,]
data[2,]


date71 <- date70+1
date99 <- 2017.999  #31st dec 2017
date4 <- data$DEATH
date4[is.na(date4)]=date99
head(date4)


enddate <- pmin(date71,date4,date99)
head(enddate)
head(date71)


max(data$ENTRY)  #people who enters after entry date
enddate2 <- pmax(startdate,enddate)
head(enddate2)
temp <- enddate2-startdate
head(temp)
data[1,]


temp
ecx <- (sum(temp))
ecx




#(ii)d
deaths <- data$DEATH[!is.na(data$DEATH)]
deaths
data$DEATH

deaths <- data$DEATH[!is.na(data$DEATH)&data$DEATH>2013.000&data$DEATH<2018.000]

births <- data$BIRTH[!is.na(data$DEATH)&data$DEATH>2013.000&data$DEATH<2018.000]


age <- deaths-births
age


d <- age[age>=70&age<=71]
d



#alternative
agelast <- sapply(age,floor)
agelast


d1 <- agelast[agelast==70]
d1

lives <- data$LIFE[!is.na(data$DEATH)&data$DEATH>2013.000&data$DEATH<2018.000]
lives

lives[agelast==70]
data[lives[agelast==70],]


#(ii)e
mu70 <- length(d)/ecx
mu70


#(iii)a
ageat2013 <- ifelse(data$ENTRY<=2013.000 & (data$DEATH>2013.000 | is.na(data$DEATH)),floor(2013.000-data$BIRTH),0)
ageat2013   #Age last Birthday


p2013 <- length(ageat2013[ageat2013==70])
p2013


#2014
ageat2014 <- ifelse(data$ENTRY<=2014.000 & (data$DEATH>2014.000 | is.na(data$DEATH)),floor(2014.000-data$BIRTH),0)
ageat2014   #Age last Birthday


p2014 <- length(ageat2014[ageat2014==70])
p2014

#2015=18
#2016=14
#2017=14
#2017=19


#iii c
ecx2 <- 6+9+18+14+14+19/2
ecx2
ecx


Mu70 <- length(d)/ecx2
Mu70
