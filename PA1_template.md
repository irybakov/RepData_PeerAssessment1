---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

```r
activity = read.csv('activity.csv')
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```


```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


```r
tail(activity)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```


## What is mean total number of steps taken per day?


```r
dailySteps <- aggregate(steps ~ date, data = activity, FUN=sum,na.rm=T)
theMean <- mean(dailySteps$steps,na.rm=T)
theMedian <- median(dailySteps$steps,na.rm=T)
hist(dailySteps$steps, xlab="steps",ylab="Frequency",
     main="Steps per day distribution",border='orange',col="gray50")
abline(v = theMean, col = "blue", lwd = 5)
abline(v = theMedian, col = "red", lwd = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



```r
summary(dailySteps)
```

```
##          date        steps      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

So Mean is 1.0766189\times 10^{4} and Median is 10765


## What is the average daily activity pattern?


```r
fiveMinSteps <- aggregate(steps ~ interval, data = activity, FUN=mean, na.rm=T)
plot(steps~interval,data=fiveMinSteps,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxInterval <- fiveMinSteps[which.max(fiveMinSteps$steps),]
maxInterval
```

```
##     interval    steps
## 104      835 206.1698
```

**835's**  5-minute interval, on average across all the days in the dataset, contains the maximum number of steps. And steps values is **206.1698113**

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
