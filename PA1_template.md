# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
# Load the required packages
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)

# Unzip and load the data
unzip("./activity.zip")
dt <- read.csv("activity.csv")

# Convert the date variable to Date class
dt$date <- as.Date(dt$date)
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

3. Calculate and report the mean and median of the total number of steps taken per day


```r
dtDay <- summarise(group_by(dt, date),
                   totalSteps = sum(steps),
                   meanSteps = mean(steps, na.rm = TRUE),
                   medianSteps = median(steps, na.rm = TRUE))

hist(dtDay$totalSteps, 
     xlab="Steps",
     main="Histogram of the total number of steps taken each day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
par(mfrow=c(1,2))
with(dtDay, { 
    plot(meanSteps ~ date,
        type = "l",
        xlab = "Date",
        ylab = "Mean of Steps")
    plot(medianSteps ~ date,
        type = "l",  
        xlab = "Date",
        ylab = "Median of Steps")
})
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-2.png) 

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
dtInterval <- summarise(group_by(dt, interval),
                        meanSteps = mean(steps, na.rm = TRUE))

with(dtInterval, {
plot(meanSteps ~ interval,
    type = "l",  
    xlab = "Interval",
    ylab = "Mean of Steps")
})
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
filter(dtInterval, meanSteps == max(meanSteps, na.rm = TRUE))[,1]
```

```
## Source: local data frame [1 x 1]
## 
##   interval
## 1      835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
sum(is.na(dt$steps))
```

```
## [1] 2304
```

```r
# Fill NA values of steps with the mean for interval
dt2 <- filter(dt,is.na(steps))
dt2[,1] <- dtInterval$meanSteps

dtImputed <- rbind(filter(dt,!is.na(steps)), dt2)
dtImputed <- arrange(dtImputed, date, interval)

dtImputedDay <- summarise(group_by(dtImputed, date),
                   totalSteps = sum(steps),
                   meanSteps = mean(steps, na.rm = TRUE),
                   medianSteps = median(steps, na.rm = TRUE))
hist(dtImputedDay$totalSteps, 
     xlab="Steps",
     main="Histogram of the total number of steps taken each day after imputing")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
par(mfrow=c(1,2))
with(dtImputedDay, { 
    plot(meanSteps ~ date,
        type = "l",
        xlab = "Date",
        ylab = "Mean of Steps")
    plot(medianSteps ~ date,
        type = "l",  
        xlab = "Date",
        ylab = "Median of Steps")
})
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-2.png) 

### Observation: There is slight variation in the mean and median steps per day after imputing the missing values. Total daily number of steps got increased after imputing the missing values.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
weekdays <- weekdays(dtImputed$date)
dtImputed$day <- factor(ifelse(weekdays == "Saturday" | weekdays == "Sunday", 
                             "weekend", "weekday"))

dtWeekdays <- summarise(group_by(dtImputed, interval, day),
                            meanSteps = mean(steps))
xyplot(meanSteps ~ interval | day,
       dtWeekdays,
       type = "l",
       layout = c(1, 2),
       xlab = "Interval",
       ylab = "Number of steps",
       col = "blue")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
