library(plyr)
#Loading and preprocessing the data
datos=read.csv("activity.csv",header=T)
head(datos)
#Mean total number of steps taken per day
#1.
totalsteps=ddply(datos,~date,summarise,steps=sum(steps))
NAtotalsteps=na.omit(totalsteps)
#2.
hist(NAtotalsteps$steps,main="Histogram \n Total of steps per day",xlab="steps",col = "lightgray")
#3.
meansteps=ddply(datos,~date,summarise,mean=mean(steps),median=median(steps))
NAmeansteps=na.omit(meansteps)
NAmeansteps
# average daily activity pattern
#1.
DateF=as.character(NAtotalsteps$date)
DateF=strptime(DateF,"%Y-%m-%d")
plot(DateF,NAtotalsteps$steps,type="l" ,main="Time Series\nMean of steps")
#2.
NAdatos=na.omit(datos)
meaninterval=ddply(NAdatos,~interval,summarise,mean=mean(steps))
meaninterval
maxcol=which.max(meaninterval$mean)
meaninterval[maxcol,]
#Imputing missing values
#1.
NAs=sum(is.na(datos))
NAs
#2. , 3.
#impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
#sinna=ddply(datos, ~ date, transform, steps = impute.mean(steps))
#datos$steps[is.na(datos$steps)] <- ave(datos$steps, datos$date, 
#                               FUN = function(z) 
#                                 mean(z, na.rm = TRUE))[c(which(is.na(datos$steps)))]
sinna=datos
sinna$steps[is.na(sinna$steps)] =0

#4.
sinnatotalsteps=ddply(sinna,~date,summarise,steps=sum(steps))
hist(sinnatotalsteps$steps,main="Histogram \n Total of steps per day",xlab="steps",col = "lightgray")
sinnameansteps=ddply(sinna,~date,summarise,mean=mean(steps))
sinnameansteps


