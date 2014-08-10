Project 1 - Activity Monitoring
========================================================
Raw data downloaded from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip on Saturday, August 9, 2014 at 20:55pm 

```r
setwd("/Users/pesto/Documents/school/coursera/rr/proj1")
adm<-read.csv("activity.csv")
```
**What is mean total number of steps taken per day?**
For this part of the assignment, you can ignore the missing values in the dataset.
Make a histogram of the total number of steps taken each day

```r
admStepsPerDay<-data.frame(aggregate(steps ~ date, data=adm, sum))
hist(admStepsPerDay$steps,xlab="Steps",main="Total Steps per Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 
Calculate and report the mean and median total number of steps taken per day

```r
admMeanStepsPerDay<-data.frame(aggregate(steps ~ date, data=adm, mean))
print(admMeanStepsPerDay)
```

```
##          date   steps
## 1  2012-10-02  0.4375
## 2  2012-10-03 39.4167
## 3  2012-10-04 42.0694
## 4  2012-10-05 46.1597
## 5  2012-10-06 53.5417
## 6  2012-10-07 38.2465
## 7  2012-10-09 44.4826
## 8  2012-10-10 34.3750
## 9  2012-10-11 35.7778
## 10 2012-10-12 60.3542
## 11 2012-10-13 43.1458
## 12 2012-10-14 52.4236
## 13 2012-10-15 35.2049
## 14 2012-10-16 52.3750
## 15 2012-10-17 46.7083
## 16 2012-10-18 34.9167
## 17 2012-10-19 41.0729
## 18 2012-10-20 36.0938
## 19 2012-10-21 30.6285
## 20 2012-10-22 46.7361
## 21 2012-10-23 30.9653
## 22 2012-10-24 29.0104
## 23 2012-10-25  8.6528
## 24 2012-10-26 23.5347
## 25 2012-10-27 35.1354
## 26 2012-10-28 39.7847
## 27 2012-10-29 17.4236
## 28 2012-10-30 34.0938
## 29 2012-10-31 53.5208
## 30 2012-11-02 36.8056
## 31 2012-11-03 36.7049
## 32 2012-11-05 36.2465
## 33 2012-11-06 28.9375
## 34 2012-11-07 44.7326
## 35 2012-11-08 11.1771
## 36 2012-11-11 43.7778
## 37 2012-11-12 37.3785
## 38 2012-11-13 25.4722
## 39 2012-11-15  0.1424
## 40 2012-11-16 18.8924
## 41 2012-11-17 49.7882
## 42 2012-11-18 52.4653
## 43 2012-11-19 30.6979
## 44 2012-11-20 15.5278
## 45 2012-11-21 44.3993
## 46 2012-11-22 70.9271
## 47 2012-11-23 73.5903
## 48 2012-11-24 50.2708
## 49 2012-11-25 41.0903
## 50 2012-11-26 38.7569
## 51 2012-11-27 47.3819
## 52 2012-11-28 35.3576
## 53 2012-11-29 24.4688
```
**What is the average daily activity pattern?**
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
admIS<-data.frame(aggregate(steps~interval, data=adm,mean))
with(admIS,(plot(steps~interval,type="l",pch=22,ylab="Avg. steps taken")))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```
## NULL
```
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
admIS[admIS$interval==max(admIS$interval),]
```

```
##     interval steps
## 288     2355 1.075
```
**Imputing missing values**
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

The following code is correct. NAs only appear in sum() column. See sum(is.na(adm$date)) and 
sum(is.na(adm$interval))

```r
sum(is.na(adm))
```

```
## [1] 2304
```
Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated. For example, 
you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy here is to use the the average across the entire dataset to fill in
the NAs.

Create a new dataset that is equal to the original dataset but
with the missing data filled in.

```r
tadm<-adm
tadm$steps[is.na(tadm$steps)]<-mean(tadm$steps)
```

Make a histogram of the total number of steps taken each day and 
Calculate and report the mean and median total number of steps taken per day. 
Do these values differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total daily 
number of steps?

Steps per day.

```r
tadmStepsPerDay<-data.frame(aggregate(steps ~ date, data=tadm, sum))
hist(tadmStepsPerDay$steps,xlab="Steps",main="Total Steps per Day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 
Mean steps per day

```r
tadmMeanStepsPerDay<-data.frame(aggregate(steps ~ date, data=tadm, mean))
print(tadmMeanStepsPerDay)
```

```
##          date   steps
## 1  2012-10-02  0.4375
## 2  2012-10-03 39.4167
## 3  2012-10-04 42.0694
## 4  2012-10-05 46.1597
## 5  2012-10-06 53.5417
## 6  2012-10-07 38.2465
## 7  2012-10-09 44.4826
## 8  2012-10-10 34.3750
## 9  2012-10-11 35.7778
## 10 2012-10-12 60.3542
## 11 2012-10-13 43.1458
## 12 2012-10-14 52.4236
## 13 2012-10-15 35.2049
## 14 2012-10-16 52.3750
## 15 2012-10-17 46.7083
## 16 2012-10-18 34.9167
## 17 2012-10-19 41.0729
## 18 2012-10-20 36.0938
## 19 2012-10-21 30.6285
## 20 2012-10-22 46.7361
## 21 2012-10-23 30.9653
## 22 2012-10-24 29.0104
## 23 2012-10-25  8.6528
## 24 2012-10-26 23.5347
## 25 2012-10-27 35.1354
## 26 2012-10-28 39.7847
## 27 2012-10-29 17.4236
## 28 2012-10-30 34.0938
## 29 2012-10-31 53.5208
## 30 2012-11-02 36.8056
## 31 2012-11-03 36.7049
## 32 2012-11-05 36.2465
## 33 2012-11-06 28.9375
## 34 2012-11-07 44.7326
## 35 2012-11-08 11.1771
## 36 2012-11-11 43.7778
## 37 2012-11-12 37.3785
## 38 2012-11-13 25.4722
## 39 2012-11-15  0.1424
## 40 2012-11-16 18.8924
## 41 2012-11-17 49.7882
## 42 2012-11-18 52.4653
## 43 2012-11-19 30.6979
## 44 2012-11-20 15.5278
## 45 2012-11-21 44.3993
## 46 2012-11-22 70.9271
## 47 2012-11-23 73.5903
## 48 2012-11-24 50.2708
## 49 2012-11-25 41.0903
## 50 2012-11-26 38.7569
## 51 2012-11-27 47.3819
## 52 2012-11-28 35.3576
## 53 2012-11-29 24.4688
```
Median steps per day

```r
tadmMedStepsPerDay<-data.frame(aggregate(steps ~ date, data=tadm, median))
print(tadmMedStepsPerDay)
```

```
##          date steps
## 1  2012-10-02     0
## 2  2012-10-03     0
## 3  2012-10-04     0
## 4  2012-10-05     0
## 5  2012-10-06     0
## 6  2012-10-07     0
## 7  2012-10-09     0
## 8  2012-10-10     0
## 9  2012-10-11     0
## 10 2012-10-12     0
## 11 2012-10-13     0
## 12 2012-10-14     0
## 13 2012-10-15     0
## 14 2012-10-16     0
## 15 2012-10-17     0
## 16 2012-10-18     0
## 17 2012-10-19     0
## 18 2012-10-20     0
## 19 2012-10-21     0
## 20 2012-10-22     0
## 21 2012-10-23     0
## 22 2012-10-24     0
## 23 2012-10-25     0
## 24 2012-10-26     0
## 25 2012-10-27     0
## 26 2012-10-28     0
## 27 2012-10-29     0
## 28 2012-10-30     0
## 29 2012-10-31     0
## 30 2012-11-02     0
## 31 2012-11-03     0
## 32 2012-11-05     0
## 33 2012-11-06     0
## 34 2012-11-07     0
## 35 2012-11-08     0
## 36 2012-11-11     0
## 37 2012-11-12     0
## 38 2012-11-13     0
## 39 2012-11-15     0
## 40 2012-11-16     0
## 41 2012-11-17     0
## 42 2012-11-18     0
## 43 2012-11-19     0
## 44 2012-11-20     0
## 45 2012-11-21     0
## 46 2012-11-22     0
## 47 2012-11-23     0
## 48 2012-11-24     0
## 49 2012-11-25     0
## 50 2012-11-26     0
## 51 2012-11-27     0
## 52 2012-11-28     0
## 53 2012-11-29     0
```

**Are there differences in activity patterns between weekdays and weekends?**
Create a new factor variable with two levels, weekday and weekend indicating whether 
a given date is on a weekend or weekday

```r
adm$date<-as.Date(adm$date)
adm$day<-weekdays(as.Date(adm$date))
adm$wday[adm$day=="Sunday"]<-"weekend"
adm$wday[adm$day=="Saturday"]<-"weekend"
adm$wday[is.na(adm$wday)]<-"weekday"
adm$wday<-factor(adm$wday,levels=c("weekday","weekend"))
wdaydf<-as.data.frame(adm[adm$wday=="weekday",])
wenddf<-as.data.frame(adm[adm$wday=="weekend",])
wdayagdf<-aggregate(steps ~ interval, data=wdaydf,mean)
wendagdf<-aggregate(steps ~ interval, data=wenddf,mean)
```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
par(mfrow=c(2,1))
with(wendagdf,(plot(steps~interval,type="l",pch=22,ylab="Number of steps", main="weekend")))
```

```
## NULL
```

```r
with(wdayagdf,(plot(steps~interval,type="l",pch=22,ylab="Number of steps", main="weekday")))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

```
## NULL
```
