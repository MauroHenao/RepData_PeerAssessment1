library(plyr)
library(Hmisc)
library(timeDate)
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
summary(NAmeansteps$mean)
# average daily activity pattern
#1.
NAdatos=na.omit(datos)
meaninterval=ddply(NAdatos,~interval,summarise,mean=mean(steps))
plot(meaninterval$interval,meaninterval$mean,type="l" ,main="Time Series\nMean of steps")
#2.
meaninterval
maxcol=which.max(meaninterval$mean)
meaninterval[maxcol,]
#Imputing missing values
#1.
NAs=sum(is.na(datos))
NAs
#2. , 3.

sinna=ddply(datos,"interval",mutate,imputed=impute(steps,mean))


#4.
sinnatotalsteps=ddply(sinna,~date,summarise,steps=sum(steps))
hist(sinnatotalsteps$steps,main="Histogram \n Total of steps per day",xlab="steps",col = "lightgray")
sinnameansteps=ddply(sinna,~date,summarise,mean=mean(steps))
summary(sinnameansteps$mean)
summary(sinna$steps)

par(mfrow=c(1,2))
hist(NAtotalsteps$steps,main="Histogram \n Total of steps per day",xlab="steps",col = "lightgray")
hist(sinnatotalsteps$steps,main="Histogram \n Total of steps per day",xlab="steps",col = "lightgray")
par(mfrow=c(1,1))
#differences in activity patterns between weekdays and weekends
#1.
d=strptime(datos$date,"%Y-%m-%d")
datos <- mutate(datos,habil=factor(isWeekday(d),labels=c("weekend","weekday")))
#2.
week=subset(datos,habil=="weekday")
meanweek=ddply(week,~interval,summarise,mean=mean(na.omit(steps)))
weekend=subset(datos,habil=="weekend")
meanweekend=ddply(weekend,~interval,summarise,mean=mean(na.omit(steps)))
plot(meanweek,type="l" ,col="red",main="Time Series\nMean of steps")
lines(meanweekend,col="blue")
legend("topright",legend=c("week","weekend"),cex=0.7,fill=c("red","blue"))
