---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## 1.0 Loading and preprocessing the data


```r
library(ggplot2)
library(dplyr)

# load data
data <- read.csv(unz("activity.zip", "activity.csv"))

# change date class as Date
data$date = as.Date(as.character(data$date))
```



## What is mean total number of steps taken per day?


```r
data_sum = data %>% group_by(date) %>% summarise(total_number_steps_taken_each_day = sum(steps)) 

data_sum
```

```
## # A tibble: 61 x 2
##    date       total_number_steps_taken_each_day
##    <date>                                 <int>
##  1 2012-10-01                                NA
##  2 2012-10-02                               126
##  3 2012-10-03                             11352
##  4 2012-10-04                             12116
##  5 2012-10-05                             13294
##  6 2012-10-06                             15420
##  7 2012-10-07                             11015
##  8 2012-10-08                                NA
##  9 2012-10-09                             12811
## 10 2012-10-10                              9900
## # ... with 51 more rows
```

```r
data_sum %>% ggplot(aes(x= total_number_steps_taken_each_day)) + geom_histogram(binwidth = 2000)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
summary(data_sum$total_number_steps_taken_each_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```

The mean of the total number of steps taken per day is 10766 while the median is 10765



## What is the average daily activity pattern?


```r
# time series

data %>% mutate(steps = ifelse(is.na(steps),0,steps)) %>% group_by(interval) %>% summarise(average_number_steps_taken_each_day = mean(steps)) %>% ggplot(aes(x=interval,y=average_number_steps_taken_each_day)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

data2 = data %>% mutate(steps = ifelse(is.na(steps),0,steps)) %>% group_by(interval) %>% summarise(mean_number_steps_taken_each_day = mean(steps))

data2 = data2 %>% arrange(desc(mean_number_steps_taken_each_day))

data2$interval[1]
```

```
## [1] 835
```

The 835,  5-minute interval on average across all the data in dataset which contains the max number of steps


## Imputing missing values


```r
# Check missing data
summary(data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
# Impute missing data by exchanging NA into mean steps

data_no_na = data %>% filter(!is.na(steps))

meansteps = mean(data_no_na$steps)

data3 = data %>%  mutate(steps = ifelse(is.na(steps),meansteps,steps))

# no more na in data

summary(data3)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 37.38   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

```r
# plot histogram

data3 %>% group_by(date) %>% summarise(total_number_steps_taken_each_day = sum(steps)) %>% ggplot(aes(x= total_number_steps_taken_each_day)) + geom_histogram(binwidth = 2000)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# median and mean. no change

summary(data_sum$total_number_steps_taken_each_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```

The value does not differ before and after we impute the NA values into the data. However the total number of steps taken for each day increases.


## Are there differences in activity patterns between weekdays and weekends?


```r
data3$days = weekdays(data3$date)

data3 = data3 %>% mutate(days_group = ifelse(days == "Saturday" |days == "Sundays","weekends","weekdays"))

data3$days_group = as.factor(data3$days_group)

data3 %>% group_by(interval,days_group) %>% summarise(average_number_steps_taken_each_day = mean(steps)) %>% ggplot(aes(x=interval,y=average_number_steps_taken_each_day)) + geom_line() + facet_grid(~days_group)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Yes there are differences between weekends which has more average steps compare to weekdays which have slightly lower average steps
