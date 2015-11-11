Peer Assessment 1 - Report on activity monitoring data
======================================================

This R Markdown document is a report on the Dataset Activity monitoring data and will answer differnt questions regarding this dataset.  
The data for this assignment can be downloaded from the course web site: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

### Load the data

The following R code will load the data from the current working directory:


```r
data <- read.csv("C:/Users/swe/Documents/activity.csv")
```

### What is mean total number of steps taken per day?

The following R Code calculates the total number of steps taken per day and creates a histogram of the result:

```r
library(plyr)
daysteps <- ddply(data, "date", summarise, total = sum(steps))
daysteps
```

```
##          date total
## 1  2012-10-01    NA
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08    NA
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01    NA
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04    NA
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09    NA
## 41 2012-11-10    NA
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14    NA
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30    NA
```

```r
with(daysteps, hist(daysteps$total, main = "total number of steps taken per day", xlab = ""))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

The following calculates the mean of the total number of steps taken per day:

```r
mean(daysteps$total, na.rm = TRUE)
```

```
## [1] 10766.19
```
And the median of the total numbers of steps taken per day:

```r
median(daysteps$total, na.rm = TRUE)
```

```
## [1] 10765
```

### What is the average daily activity pattern?

The following shows the the average number of steps taken, averaged across all days:

```r
interval_steps <- aggregate(steps ~ interval, data, mean)
with(interval_steps, plot(interval_steps$interval, interval_steps$steps, type = "l",
                          main = "average number of steps taken", 
                    xlab = "interval", ylab = "averaged across all days", col="lightcoral"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

The 5-minute interval, on average across all the days, with the maximum number of steps is shown by:

```r
interval_steps$interval[which.max(interval_steps$steps)]
```

```
## [1] 835
```

### Imputing missing values

Report of the total numbers of missing Values:

```r
colSums(is.na(data))
```

```
##    steps     date interval 
##     2304        0        0
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.
Strategy is to use the average number of steps taken.

```r
# Combine average steps take with original dataset, by interval
data_new <- merge(data, interval_steps, by="interval", suffixes = c(".old",".new"))

# Create a logical vector to kno the positions of the NAs in the dataset
NAs <- is.na(data_new$steps.old)

# replace the NAs with the average value
data_new$steps.old[NAs] <- data_new$steps.new[NAs]
data_new <- data_new[ , c(1:3)]

daysteps_new <- ddply(data_new, "date", summarise, total = sum(steps.old))
with(daysteps_new, hist(daysteps_new$total, main = "total number of steps taken per day", xlab = ""))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
mean(daysteps_new$total)
```

```
## [1] 10766.19
```

```r
median(daysteps_new$total)
```

```
## [1] 10766.19
```

As you see the mean is the same and the median is only a little bit changed. Now that are all observations filled with values naturally the Frequency in the histogram is higher.

### Are there differences in activity patterns between weekdays and weekends?


```r
# convert date formate so use of function weekdays() is possible:
data_new$date <- as.Date(data_new$date, "%Y-%m-%d")
# enhance new column with the weekday
data_new$day <- weekdays(data_new$date)
# enhance new column for weekend/weekday
data_new$type <- c("weekday")

# rename type, if day is Saturday (German="Samstag") or Sunday (German="Sonntag")
for (i in 1:nrow(data_new)){
  if (data_new$day[i] == "Samstag" || data_new$day[i] == "Sonntag"){
    data_new$type[i] <- "weekend"
  }
}
# convert type from character to factor
data_new$type <- as.factor(data_new$type)

# get average number of steps taken, averaged across all weekday days or weekend days
interval_type <- aggregate(steps.old ~ interval+type, data_new, mean)

library(ggplot2)
# Make a panel plot
qplot(interval, steps.old, data=interval_type, geom="line", xlab="Interval", 
      ylab="Number of steps", main="") + facet_wrap(~ type, ncol=1)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 











