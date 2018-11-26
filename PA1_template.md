---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
knitr::opts_chunk$set(
  echo = TRUE,
  fig.width = 10, 
  fig.height = 7.5
)

library(tidyverse)
```

```
## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 3.1.0     ✔ purrr   0.2.5
## ✔ tibble  1.4.2     ✔ dplyr   0.7.7
## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
## ✔ readr   1.1.1     ✔ forcats 0.3.0
```

```
## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(lattice)
```

## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. `read.csv()`)

2. Process/transform the data (if necessary) into a format suitable for your analysis

### Import


```r
activity <- read.csv(paste0("~/GitHub/RepData_PeerAssessment1/", "activity.csv"), header = TRUE, as.is = TRUE)
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research the difference between them
    + Make a histogram of the total number of steps taken each day

3. Calculate and report the mean and median of the total number of steps taken per day

### Transform


```r
total <- activity %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(steps = sum(steps, na.rm = TRUE))
```

### Visualize


```r
total %>%
  ggplot(aes(x = steps)) +
    geom_histogram(binwidth = 1000) +
    labs(
      title = "Histogram of the total number of steps taken each day",
      x = "total number of steps taken each day (binwidth = 1000)",
      y = "count"
    )
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### Model


```r
tnstpd_avg <- as.integer(round(mean(total$steps, na.rm = TRUE)))
tnstpd_med <- as.integer(round(median(total$steps, na.rm = TRUE)))
```

Total Number of Steps Taken per Day:

- Mean = 9354

- Median = 10395

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

### Transform


```r
avg_by_interval <- activity %>%
  dplyr::group_by(interval) %>%
  dplyr::summarise(steps = mean(steps, na.rm = TRUE))
```

### Visualize


```r
avg_by_interval %>%
  ggplot(aes(x = interval, y = steps)) +
    geom_line() +
    labs(
      title = "Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days",
      x = "5-minute Interval",
      y = "Average Number of Steps Taken"
    )
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

### Model


```r
max_interval <- avg_by_interval$interval[which.max(avg_by_interval$steps)]
```

5-minute interval containing the maximum number of steps: 835

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

2. Devise a strategy for filling in all of the missing values in the dataset
    + The strategy does not need to be sophisticated
    + For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day
    + Do these values differ from the estimates from the first part of the assignment?
    + What is the impact of imputing missing data on the estimates of the total daily number of steps?

### Transform


```r
# change "steps" to "avg_steps" before merge
names(avg_by_interval) <- c("interval", "avg_steps") 

activity2 <- activity %>%
  left_join(avg_by_interval, by = "interval")

# replace missing values with avg_steps by interval
activity2$steps <- with(activity2, ifelse(is.na(steps), avg_steps, steps))

total2 <- activity2 %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(steps = sum(steps, na.rm = TRUE))
```

### Visualize


```r
total2 %>%
  ggplot(aes(x = steps)) +
    geom_histogram(binwidth = 1000) +
    labs(
      title = "Histogram of the total number of steps taken each day",
      subtitle = "After replacing missing values with average for interval",
      x = "total number of steps taken each day (binwidth = 1000)",
      y = "count"
    )
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### Model


```r
missing_steps <- sum(is.na(activity$steps))

tnstpd_avg2 <- as.integer(round(mean(total2$steps, na.rm = TRUE)))
tnstpd_med2 <- as.integer(round(median(total2$steps, na.rm = TRUE)))
```

Total number of missing values in the dataset: 2304

Total Number of Steps Taken per Day after after replacing missing values with average for interval:

- Mean = 10766

- Median = 10766

## Are there differences in activity patterns between weekdays and weekends?

For this part the `weekdays()` function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

2. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
    + See the README file in the GitHub repository to see an example of what this plot should look like using simulated data

### Transform


```r
activity2 <- activity2 %>%
  dplyr::mutate(
    Date = as.Date(date),
    day_name = weekdays(Date),
    day_type = if_else(day_name %in% c("Saturday", "Sunday"), "weekend", "weekday")
  )

avg_by_interval2 <- activity2 %>%
  dplyr::group_by(interval, day_type) %>%
  dplyr::summarise(steps = mean(steps, na.rm = TRUE))
```

### Visualize


```r
xyplot(
  steps ~ interval | day_type, 
  data = avg_by_interval2, 
  layout = c(1, 2),
  ylab = "Number of steps",
  type = "l"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
