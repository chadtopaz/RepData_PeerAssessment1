

# Reproducible Research: Peer Assessment 1

## Introduction

This project examines data gathered from monitoring devices such as the
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), and
[Jawbone Up](https://jawbone.com/up). These devices monitor a person's activity and they report measurements in order to help the user improve their health and/or understand their behvario.

## Data

The personal monitoring device used gathers data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables in the raw data set are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken
    
The raw dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this
dataset.

The anaysis that follows assumes the lubridate and dplyr packages are installed.

## Loading and preprocessing the data

### 1. Load the data.

First, we [download the zip file](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) and record the date and time of download.

```r
temp <- tempfile()
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
    temp)
downloadTime <- Sys.time()
print(downloadTime)
```

```
## [1] "2014-06-15 09:48:04 CDT"
```

We then unzip the downloaded zip file and read the resulting .csv file into a usable R data frame.

```r
zipFileInfo <- unzip(temp, list = TRUE)
activity <- read.csv(unz(temp, as.character(zipFileInfo$Name)), header = TRUE)
unlink(temp)
```


It is worthwhile to look at the structure of the data.

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


### 2. Process/transform the data (if necessary) into a format suitable for your analysis.

The data appear tidy and in a usable format. However, it is inconvenient that the intervals are integer-valued, and not time variables. We correct this now by converting the integer valued interval data into a character string, appending leading zeros as necessary to make it four digits long, and converting that into a time variable using the lubridate package.

```r
library("lubridate")
activity$time <- parse_date_time(sprintf("%04s", paste(activity$interval)), 
    "hm")
```


Additionally, it will be helpful to have a day name affixed to each date. We do this with the weekdays command.

```r
activity$dayofweek <- weekdays(as.Date(activity$date))
```



## What is mean total number of steps taken per day?

We calculate the total number of steps per day. For this question, we use only complete records.

```r
completeActivity <- na.omit(activity)
```

We perform the analysis using the dplyr package. We group the activity data by date, and then within each group, we summarize by totaling the number of steps.

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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
grouped <- group_by(completeActivity, date)
totalGrouped <- summarise(grouped, totalSteps = sum(steps, na.rm = TRUE))
```

### 1. Make a histogram of the total number of steps taken each day.
Here is a histogram of the total number of steps per day.

```r
hist(totalGrouped$totalSteps, xlab = "Steps per day", main = "Histogram of steps per day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

### 2. Calculate and report the mean and median total number of steps taken per day.
We calculate the mean and median values.

```r
meansteps <- mean(totalGrouped$totalSteps)
mediansteps <- median(totalGrouped$totalSteps)
```

The mean value is 10766.18868 steps per day and the median value is 10765 steps per day.

## What is the average daily activity pattern?

We calculate the average steps taken in each interval, averaged across all days. For this question, we exclude NA values as before by using the completeActivity variable constructed previously. We perform the analysis using the dplyr package. We group the activity data by interval, and then within each group, we summarize by averaging the number of steps.

```r
library(dplyr)
grouped <- group_by(completeActivity, time)
meanGrouped <- summarise(grouped, meanSteps = sum(steps))
```


### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
Here is a time series of the average steps taken.

```r
plot(meanSteps ~ time, data = meanGrouped, type = "l", xlab = "Interval", ylab = "Mean steps taken across all days", 
    main = "Time series of average activity pattern")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
We calculate the interval containing the maximum (average) number of steps.

```r
maxinterval <- subset(meanGrouped, meanSteps == max(meanGrouped$meanSteps), 
    select = "time")$time
```

The interval of maximum activity is 08:35:00 in the morning.

## Imputing missing values

### 1. There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

We count the number of rows with missing values by counting the total number of rows and subtracting the number of complete cases.

```r
incompleteCases = nrow(activity) - sum(complete.cases(activity))
```

There are `R incompleteCases` rows with missing values.


### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

We will impute by both weekday and interval. In other words, if there is a missing value at 11:15 on a Thursday, we will fill it in with the average steps taken on other Thursdays at 11:15.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

First, we look in the reduced data set completeActivity which has no missing data. We calculate the mean steps within each combination of day of week and interval. This is the data we will use for imputation.


```r
impute <- group_by(completeActivity, dayofweek, interval)
imputeMean <- as.data.frame(summarise(impute, meansteps = mean(steps)))
```


Now, we copy the original data set and loop over it, replacing missing values with the mean calculated above.


```r
imputedActivity <- activity
for (i in 1:nrow(imputedActivity)) {
    if (is.na(imputedActivity$steps[i])) {
        thisdayofweek = imputedActivity$dayofweek[i]
        thisinterval = imputedActivity$interval[i]
        meanval = subset(imputeMean, dayofweek == thisdayofweek & interval == 
            thisinterval, select = "meansteps")
        imputedActivity$steps[i] <- meanval
    }
}
imputedActivity$steps <- as.numeric(imputedActivity$steps)
```


### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

We repeat the process earlier in this doument.


```r
grouped <- group_by(imputedActivity, date)
totalGrouped <- summarise(grouped, totalSteps = sum(steps))
hist(totalGrouped$totalSteps, xlab = "Steps per day", main = "Histogram of steps per day with imputed data")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 

```r
meansteps <- mean(totalGrouped$totalSteps)
mediansteps <- median(totalGrouped$totalSteps)
```

The mean value with imputed data is 10821.2096 steps per day and the median value is 11015 steps per day. These values are both higher that the corresponding values in the non-inputed data set (above).

## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
imputedActivity$daytype <- imputedActivity$dayofweek %in% c("Saturday", "Sunday")
imputedActivity$daytype <- factor(imputedActivity$daytype, levels = c(FALSE, 
    TRUE), labels = c("weekday", "weekend"))
```


### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

First, we group and summarize the data by whether or not it is a weekday

```r
weekdaydata <- subset(imputedActivity, daytype == "weekday")
weekdayInterval <- group_by(weekdaydata, time)
weekdaymean <- summarise(weekdayInterval, meanSteps = mean(steps))
weekenddata <- subset(imputedActivity, daytype == "weekend")
weekendInterval <- group_by(weekenddata, time)
weekendmean <- summarise(weekendInterval, meanSteps = mean(steps))
```


Then, we plot it in two panels using the base plotting system. This appears different from the sample plot given, which used ggplot, but it is still a panel plot.


```r
par(mfrow = c(2, 1))
plot(meanSteps ~ time, data = weekdaymean, type = "l", xlab = "Time", ylab = "Mean steps", 
    main = "Average activity pattern on weekdays")
plot(meanSteps ~ time, data = weekendmean, type = "l", xlab = "Time", ylab = "Mean steps", 
    main = "Average activity pattern on weekends")
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19.png) 


The patterns indeed appear different.
