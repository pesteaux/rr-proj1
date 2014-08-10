#Raw data downloaded from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
#   on Saturday, August 9, 2014 at 20:55pm 

setwd("/Users/pesto/Documents/school/coursera/rr/proj1")
adm<-read.csv("activity.csv")

#Steps per day.
admStepsPerDay<-data.frame(aggregate(steps ~ date, data=adm, sum))
hist(admStepsPerDay$steps,xlab="Steps",main="Total Steps per Day")

#Mean steps per day
admMeanStepsPerDay<-data.frame(aggregate(steps ~ date, data=adm, mean))
print(admMeanStepsPerDay)

#Median steps per day
admMedStepsPerDay<-data.frame(aggregate(steps ~ date, data=adm, median))
print(admMedStepsPerDay)
hist(admdf$steps)

#Make a time series plot (i.e. type = "l") of the 5-minute 
#interval (x-axis) and the average number of steps taken, 
# averaged across all days (y-axis)
admIS<-data.frame(aggregate(steps~interval, data=adm,mean))
with(admIS,(plot(steps~interval,type="l",pch=22,ylab="Avg. steps taken")))

#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
admIS[admIS$interval==max(admIS$interval),]

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
# This is correct, NAs only appear in sum() column. see sum(is.na(adm$date)) and 
# sum(is.na(adm$interval))
sum(is.na(adm))

# Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, 
#you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# The strategy here is to use the the average across the entire dataset to fill in
# the NAs.

# Create a new dataset that is equal to the original dataset but
#  with the missing data filled in.


tadm<-adm
tadm$steps[is.na(tadm$steps)]<-mean(tadm$steps)

# Make a histogram of the total number of steps taken each day and 
# Calculate and report the mean and median total number of steps taken per day. 
# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily 
# number of steps?

#Steps per day.
tadmStepsPerDay<-data.frame(aggregate(steps ~ date, data=tadm, sum))
hist(tadmStepsPerDay$steps,xlab="Steps",main="Total Steps per Day")

#Mean steps per day
tadmMeanStepsPerDay<-data.frame(aggregate(steps ~ date, data=tadm, mean))
print(tadmMeanStepsPerDay)

#Median steps per day
tadmMedStepsPerDay<-data.frame(aggregate(steps ~ date, data=tadm, median))
print(tadmMedStepsPerDay)

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
par(mfrow=c(2,1))
with(wendagdf,(plot(steps~interval,type="l",pch=22,ylab="Number of steps", main="weekend")))
with(wdayagdf,(plot(steps~interval,type="l",pch=22,ylab="Number of steps", main="weekday")))

