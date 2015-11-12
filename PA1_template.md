# Reproducible Research: Peer Assessment 1

```r
suppressMessages(library(dplyr))
library(lattice)
```

## Loading and preprocessing the data

```r
data <- read.csv("./data/activity.csv", sep = ",")
data$date <- as.Date(data$date)
dataGrouped <- group_by(data, date)
dataSum <- summarise(dataGrouped, stepsSum = sum(steps))
```

## What is mean total number of steps taken per day?

```r
histogram( ~ stepsSum,data=dataSum, type="count", xlab="Sum of Steps",
           ylab="Count", main="Total Number of Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
mean(tapply(data$steps,data$date,sum), na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(tapply(data$steps,data$date,sum), na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
