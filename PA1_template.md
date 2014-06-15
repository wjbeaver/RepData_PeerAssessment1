# Reproducible Research: Peer Assessment 1

William J Beaver

## Loading and preprocessing the data


```r
### libraries used
library(lattice)

### get data
data<-read.csv(unzip("activity.zip"))
```
Look at the data to see if it needs transforming.


```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Nothing to clean, just make the intervals a factor.


```r
data$interval<-factor(data$interval)

str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

## What is mean total number of steps taken per day?

First we sum the steps over each day


```r
byday<-aggregate(data$steps, by=list(data$date), sum)

names(byday)<-c("date", "sum")
```

The plot a histogram of the summed steps


```r
histogram(~sum, byday, ylab="Precent of Total by Day", xlab="Sum of Steps", main="Histogram of Daily Steps", col="tan")
```

![plot of chunk histna](figure/histna.png) 


The mean of the summed steps by day is 1.0766 &times; 10<sup>4</sup> and the median is 10765.

## What is the average daily activity pattern?

### intervals averaged over all days


```r
byinterval<-aggregate(data$steps, by=list(data$interval), function(x) mean(x, na.rm=TRUE))

names(byinterval)<-c("interval", "mean_steps")
```

Time series plot of the intervals averaged over all days.


```r
xyplot(mean_steps~as.integer(interval), byinterval, type="l", aspect="fill", xlab="Interval", ylab="Mean Steps", main="Mean Steps 5 Minute Intervals")
```

![plot of chunk xyintervalplot](figure/xyintervalplot.png) 


The most active period is at 8 hours and 40 minutes. 

## Imputing missing values

Where are the NAs?


```r
colSums(is.na(data))
```

```
##    steps     date interval 
##     2304        0        0
```

Since they are in steps, replace NAs with interval averages.


```r
data.noNAs<-data

### function to replace NAs by interval averages
averageNAs <- function (x) {        
        if (is.na(x[1])) {
                average<-subset(byinterval, interval==x[3])[[2]]
                as.numeric(average)
        } else {
                as.numeric(x[1])
        }
}

data.noNAs$steps<-apply(data, 1, averageNAs)
```

Any NAs left?


```r
colSums(is.na(data.noNAs))
```

```
##    steps     date interval 
##        0        0        0
```

Histogram without NAs


```r
byday<-aggregate(data.noNAs$steps, by=list(data.noNAs$date), sum)

names(byday)<-c("date", "sum")

histogram(~sum, byday, ylab="Precent of Total by Day", xlab="Sum of Steps", main="Histogram of Daily Steps, No NAs", col="tan")
```

![plot of chunk histogramnonas](figure/histogramnonas.png) 


The histogram looks different and the mean is 1.0766 &times; 10<sup>4</sup> and the median is 1.0766 &times; 10<sup>4</sup>. The mean has not changed and the median has gotten closer to the mean.

What about the intervals?


```r
byinterval<-aggregate(data.noNAs$steps, by=list(data.noNAs$interval), function(x) mean(x, na.rm=TRUE))

names(byinterval)<-c("interval", "mean_steps")

xyplot(mean_steps~as.integer(interval), byinterval, type="l", aspect="fill", xlab="Interval", ylab="Mean Steps", main="Mean Steps 5 Minute Intervals, No NAs")
```

![plot of chunk intervalsnona](figure/intervalsnona.png) 


The plot looks the same and the most active period is now at 8 hours and 40 minutes. No change!

## Are there differences in activity patterns between weekdays and weekends?


```r
## function to divide the dates into weekdays and weekends
cutWeek<-function(x) {
        if (weekdays(as.Date(x[2]))=="Saturday" || weekdays(as.Date(x[2]))=="Sunday") {
                "weekend"
        } else {
                "weekday"
        }
}

data.week<-data.noNAs

data.week$week<-apply(data.week, 1, cutWeek)

byinterval<-aggregate(data.week$steps, by=list(data.week$interval, data.week$week), function(x) mean(x, na.rm=TRUE))

names(byinterval)<-c("interval", "week", "mean_steps")

byinterval$week<-factor(byinterval$week)

xyplot(mean_steps~as.integer(interval)|week, byinterval, type="l", layout=c(1,2), aspect="fill", xlab="Interval", ylab="Mean Steps")
```

![plot of chunk weekdif](figure/weekdif.png) 

During the weekdays there is a big peak in number of steps in the morning with less activity during the day. On weekends this peak is much less with activity spaced out over the day.
