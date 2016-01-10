# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(plyr)
library(Hmisc)
```

```
## Warning: package 'Hmisc' was built under R version 3.2.3
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Warning: package 'Formula' was built under R version 3.2.3
```

```
## Loading required package: ggplot2
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:plyr':
## 
##     is.discrete, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
library(timeDate)
```

```
## Warning: package 'timeDate' was built under R version 3.2.3
```

```r
datos=read.csv("activity.csv",header=T)
```
## What is mean total number of steps taken per day?
The NA values are ignored in this section. The next figure is a histogram with the total number of steps taken daily. 

```r
totalsteps=ddply(datos,~date,summarise,steps=sum(steps))
NAtotalsteps=na.omit(totalsteps)
hist(NAtotalsteps$steps,main="Histogram \n Total of steps per day",xlab="steps",col = "lightgray")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-2-1.png" title="" alt="" width="480px" height="480px" />

```r
meansteps=ddply(NAtotalsteps,~date,summarise,mean=mean(steps),median=median(steps))
summary(meansteps$mean)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```


## What is the average daily activity pattern?
The NA values are ignored in this section.Here a graph is produced of the mean of steps taking per interval

```r
NAdatos=na.omit(datos)
meaninterval=ddply(NAdatos,~interval,summarise,mean=mean(steps))
plot(meaninterval$interval,meaninterval$mean,type="l" ,main="Time Series\n Mean of steps",xlab="Interval",ylab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)\

Next, the five minute iterval with the maximum number of steps is shown 


```r
maxcol=which.max(meaninterval$mean)
meaninterval[maxcol,]
```

```
##     interval     mean
## 104      835 206.1698
```

## Imputing missing values
This section shows the number of NAs that the table has. 


```r
NAs=sum(is.na(datos))
NAs
```

```
## [1] 2304
```

the mean of 5minutes intervals is imputed to the NAs.


```r
sinna=ddply(datos,"interval",mutate,imputed=impute(steps,mean))
```

histogram with the total number of steps taken daily including the imputed values


```r
sinnatotalsteps=ddply(sinna,~date,summarise,steps=sum(steps))
hist(sinnatotalsteps$steps,main="Histogram \n Total of steps per day",xlab="steps",col = "lightgray")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)\

the mean of total steps taken per day is shown


```r
sinnameansteps=ddply(sinnatotalsteps,~date,summarise,mean=mean(steps))
summary(sinnameansteps$mean)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

Graphs, comparing the original and the imputed tables


```r
totalsteps=ddply(datos,~date,summarise,steps=sum(steps))
NAtotalsteps=na.omit(totalsteps)
par(mfrow=c(1,2))
hist(NAtotalsteps$steps,main="Histogram \n Total of steps per day",xlab="steps",col = "lightgray")
hist(sinnatotalsteps$steps,main="Histogram \n Total of steps per day",xlab="steps",col = "lightgray")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)\

```r
par(mfrow=c(1,1))
```

## Are there differences in activity patterns between weekdays and weekends?
Graph ilustrating the difference between the number of steps taken in the week and in the weekend

```r
d=strptime(datos$date,"%Y-%m-%d")
datos <- mutate(datos,habil=factor(isWeekday(d),labels=c("weekend","weekday")))
week=subset(datos,habil=="weekday")
meanweek=ddply(week,~interval,summarise,mean=mean(na.omit(steps)))
weekend=subset(datos,habil=="weekend")
meanweekend=ddply(weekend,~interval,summarise,mean=mean(na.omit(steps)))
plot(meanweek,type="l" ,col="red",main="Time Series\nSpeteps week vs Steps weekend")
lines(meanweekend,col="blue")
legend("topright",legend=c("week","weekend"),cex=0.7,fill=c("red","blue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)\

