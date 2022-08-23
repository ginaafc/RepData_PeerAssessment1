---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data  

1. Load the data (i.e. read.csv())  


```r
unzip("activity.zip")
dataAct<-read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis  
  -- Changing the format of the date variable


```r
library(lubridate)
dataAct$date<-ymd(dataAct$date)
```

## What is mean total number of steps taken per day?  

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
library(dplyr)
stepsPerDay<-dataAct %>% 
             group_by(date) %>% 
             summarize(sumSteps=sum(steps,na.rm = T))
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
library(formattable)
hist(stepsPerDay$sumSteps, xlab="Steps per day", main="Total number of steps taken each day",xaxt="n")
axis(1, at=pretty(stepsPerDay$sumSteps),labels = comma(pretty(stepsPerDay$sumSteps), digits = 0),las = 1)
```

![plot of chunk unnamed-chunk-97](figure/unnamed-chunk-97-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day


```r
meanStepsPerDay<-comma(mean(stepsPerDay$sumSteps),digits = 2)
print (meanStepsPerDay)
```

```
## [1] 9,354.23
```

```r
medianStepsPerDay<-comma(median(stepsPerDay$sumSteps),digits = 2)
print(medianStepsPerDay)
```

```
## [1] 10,395.00
```
The mean of the total number of steps taken per day is 9,354.23 and the median is 10,395.00

## What is the average daily activity pattern?  

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  


```r
stepsPerInterv<-dataAct %>% 
             group_by(interval) %>% 
             summarize(meanSteps=mean(steps,na.rm = T))

plot(stepsPerInterv$interval,stepsPerInterv$meanSteps, type="l",xlab="5-minute interval", ylab="Average number of steps taken", main="Average number of steps taken by 5-minute interval",xaxt="n")
axis(1, at=pretty(stepsPerInterv$interval),labels = comma(pretty(stepsPerInterv$interval), digits = 0),las = 1)
```

![plot of chunk unnamed-chunk-99](figure/unnamed-chunk-99-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxStepsInt<-comma(stepsPerInterv$interval[which.max(stepsPerInterv$meanSteps)],digits=0)
print(maxStepsInt)
```

```
## [1] 835
```

The the maximum number of steps is contained in the 835 5-minute interval.

## Imputing missing values  

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
numberNAs<-comma(sum(is.na(dataAct$steps)),digits = 0)
print(numberNAs)
```

```
## [1] 2,304
```
The total number of missing values in the dataset is 2,304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy for filling all of the missing values will be the mean for that 5-minute interval

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
dataActFill<-dataAct %>%
         group_by(interval) %>%
         mutate(steps=ifelse(is.na(steps),ceiling(mean(steps,na.rm=T)),steps))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
stepsPerDayFill<-dataActFill %>%
                 group_by(date) %>%
                 mutate(steps=sum(steps,na.rm=T))

hist(stepsPerDayFill$steps, xlab="Steps per day", main="Total number of steps taken each day (filled data)",xaxt="n")
axis(1, at=pretty(stepsPerDayFill$steps),labels = comma(pretty(stepsPerDayFill$steps), digits = 0),las = 1)
```

![plot of chunk unnamed-chunk-103](figure/unnamed-chunk-103-1.png)

```r
meanStPerDayfill<-comma(mean(stepsPerDayFill$steps))
print (meanStPerDayfill)
```

```
## [1] 10,784.92
```

```r
medianStPerDayfill<-comma(median(stepsPerDayFill$steps))
print (medianStPerDayfill)
```

```
## [1] 10,909.00
```

The mean steps per day changes from 9,354.23 to 10,784.92, and the median from 10,395.00 to 10,784.92. In this case, the mean and median increase.

## Are there differences in activity patterns between weekdays and weekends? 
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
dataActFill<-dataActFill %>% 
             mutate(typeday=as.factor(if_else(wday(date) %in% c(7,1),"weekend","weekday")))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
stepsPerIntervFill<- dataActFill %>% 
  group_by(ID=paste(typeday,interval)) %>% summarize(meanSteps=mean(steps,na.rm=T),typeday=first(typeday),interval=first(interval)) %>% 
  arrange(interval,typeday)

library(lattice)

xyplot(meanSteps ~ interval | typeday, stepsPerIntervFill, type = "l", layout =c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-105](figure/unnamed-chunk-105-1.png)
