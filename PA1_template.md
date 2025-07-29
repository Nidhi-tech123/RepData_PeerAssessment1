---
title: "Reproducible Research:Peer Assessment 1"
author: "Nidhi G"
date: "2025-07-29"
output: html_document
---


<h1 style="font-size:30px;">Loading and Preprocessing the Data</h1>


``` r
# Unzip the dataset
unzip("repdata_data_activity.zip")
data <- read.csv("activity.csv")

# Preview the data
head(data)
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

<h1 style="font-size:30px;">Mean Total Number of Steps Taken Per Day</h1>



``` r
# Aggregate total steps by date
z <- aggregate(steps ~ date, data, sum, na.rm = TRUE)

# View total steps
head(z)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

``` r
tail(z)
```

```
##          date steps
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

``` r
# Plot histogram of total daily steps
hist(z$steps, main = "Total Steps Per Day", xlab = "Steps", col = "skyblue")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

``` r
# Calculate mean and median
mean_steps <- mean(z$steps)
median_steps <- median(z$steps)

mean_steps
```

```
## [1] 10766.19
```

``` r
median_steps
```

```
## [1] 10765
```
We aggregated total steps by day, plotted a histogram, and calculated the mean and median ignoring NA values.


<h1 style="font-size:30px;">Average Daily Activity Pattern</h1>

``` r
# average steps for each 5-minute interval
y <- aggregate(steps ~ interval, data, mean, na.rm = TRUE)

# View sample
head(y)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

``` r
# Time series plot
plot(y$interval, y$steps, type = "l", col = "red",
     main = "Average Daily Activity Pattern",
     xlab = "5-Minute Interval", ylab = "Average Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

``` r
# Find interval with maximum average steps
max_index <- which.max(y$steps)
y[max_index, ]
```

```
##     interval    steps
## 104      835 206.1698
```
This shows how activity varies throughout the day. The interval with the maximum number of average steps is calculated as above.



<h1 style="font-size:30px;">Imputing Missing Values</h1>

``` r
# Count missing values
sum(is.na(data$steps))
```

```
## [1] 2304
```

``` r
# Merge with interval means
merged_data <- merge(data, y, by = "interval", suffixes = c("", ".mean"))

# Fill missing step values
merged_data$steps_filled <- ifelse(is.na(merged_data$steps),
                                   merged_data$steps.mean,
                                   merged_data$steps)

# Clean new dataset
data_filled <- merged_data[, c("steps_filled", "date", "interval")]
names(data_filled)[1] <- "steps"

head(data_filled)
```

```
##      steps       date interval
## 1 1.716981 2012-10-01        0
## 2 0.000000 2012-11-23        0
## 3 0.000000 2012-10-28        0
## 4 0.000000 2012-11-06        0
## 5 0.000000 2012-11-24        0
## 6 0.000000 2012-11-15        0
```
replacing each NA with the mean for that 5-minute interval 


<h1 style="font-size:30px;">Histogram & Comparison</h1>

``` r
# Aggregate total steps again
q <- aggregate(steps ~ date, data_filled, sum)

# Plot new histogram
hist(q$steps, main = "Total Steps Per Day (After Imputation)",
     xlab = "Steps", col = "lightgreen")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

``` r
# Recalculate mean and median
mean_steps_new <- mean(q$steps)
median_steps_new <- median(q$steps)

mean_steps_new
```

```
## [1] 10766.19
```

``` r
median_steps_new
```

```
## [1] 10766.19
```
The new mean and median are similar to the original. This confirms that imputing missing values with interval means maintains overall data distribution.



<h1 style="font-size:30px;">Differences in Activity Patterns Between Weekdays and Weekends</h1>

``` r
# Convert date to Date class
data_filled$date <- as.Date(data_filled$date)

# Create day type variable
data_filled$day_type <- ifelse(weekdays(data_filled$date) %in% c("Saturday", "Sunday"),
                               "weekend", "weekday")
data_filled$day_type <- as.factor(data_filled$day_type)

# Count occurrences
table(data_filled$day_type)
```

```
## 
## weekday weekend 
##   12960    4608
```


<h1 style="font-size:30px;">Line Plot Using ggplot2</h1>

``` r
# Average steps by interval and day type
interval_avg_by_daytype <- aggregate(steps ~ interval + day_type,
                                     data = data_filled, mean)

# Load ggplot2
library(ggplot2)

# Panel plot
ggplot(interval_avg_by_daytype, aes(x = interval, y = steps)) +
  geom_line(color = "pink") +
  facet_wrap(~ day_type, ncol = 1) +
  labs(title = "Average Steps per Interval: Weekday vs Weekend",
       x = "5-Minute Interval",
       y = "Average Steps") +
  theme_minimal()
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)
We can see Weekday and Weekend activity per 5 minute interval.
