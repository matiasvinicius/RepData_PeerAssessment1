---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

#### Libraries

```r
library(dplyr)
library(lubridate)
library(ggplot2)
```

## Loading and preprocessing the data

```r
unzip('activity.zip')
activity <- read.csv('activity.csv') %>%
  mutate(date = as_date(date))

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

## What is mean total number of steps taken per day?

```r
# Getting the total number of steps of each day
steps_by_date <- activity %>%
  group_by(date) %>%
  summarise(steps = sum(steps, na.rm = TRUE), .groups = "drop")

# Creating the histogram
ggplot(steps_by_date, aes(x = steps)) +
  geom_histogram(bins = 10) +
  geom_vline(aes(xintercept = mean(steps), color = "Mean"),
             linetype = "dashed") +
  geom_vline(aes(xintercept = median(steps), color = "Median"),
             linetype = "dashed")+
  labs(x = "Number of steps in one day",
       y = "Frequency",
       title = "Total number of steps taken per day") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name = "Legend", 
                     values = c(Median = "green", Mean = "red"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Mean and median in console
message("Mean: ", mean(steps_by_date$steps), 
        "\nMedian: ", median(steps_by_date$steps))
```

```
## Mean: 9354.22950819672
## Median: 10395
```

## What is the average daily activity pattern?

```r
#The average number of steps taken, averaged across all days 
mean_steps_interval <- activity %>%
  select(interval, steps) %>%
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm = TRUE), .groups = "drop")

# Time series plot
ggplot(mean_steps_interval, aes(x = interval, y = steps)) +
  geom_line() +
  labs(x = "5-minute interval",
       y = "average number of steps",
       title = "Average daily activity pattern") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_avg_steps <- mean_steps_interval %>%
  filter(steps == max(steps)) %>%
  select(interval) %>%
  pull()
message("5-minute interval with max average of steps: ", max_avg_steps)
```

```
## 5-minute interval with max average of steps: 835
```


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
