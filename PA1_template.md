---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


```r
#Read the data in
activity_data <- read.csv("./activity.csv")

#Create a variable containing the activity dataset with the NAs removed
activity_data_clean <- activity_data[-which(is.na(activity_data$steps)),]

#Find the total and average number of steps per day
activity_data_grouped <- activity_data_clean %>% 
  group_by(date) %>% 
  summarize(total_steps = sum(steps), average_steps = mean(steps))

activity_data_grouped
```

```
## # A tibble: 53 x 3
##    date       total_steps average_steps
##    <fct>            <int>         <dbl>
##  1 2012-10-02         126         0.438
##  2 2012-10-03       11352        39.4  
##  3 2012-10-04       12116        42.1  
##  4 2012-10-05       13294        46.2  
##  5 2012-10-06       15420        53.5  
##  6 2012-10-07       11015        38.2  
##  7 2012-10-09       12811        44.5  
##  8 2012-10-10        9900        34.4  
##  9 2012-10-11       10304        35.8  
## 10 2012-10-12       17382        60.4  
## # ... with 43 more rows
```

## What is mean total number of steps taken per day?


```r
#mean steps per day
mean(activity_data_grouped$total_steps)
```

```
## [1] 10766.19
```

```r
#median steps per day
median(activity_data_grouped$total_steps)
```

```
## [1] 10765
```

```r
#Histogram of the total steps taken each day
ggplot(activity_data_grouped, aes(total_steps))+
  geom_histogram(binwidth = 2000, fill = "orange")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#Save the plot to comapre when missing values are replaced.
original_plot <- ggplot(activity_data_grouped, aes(total_steps))+
  geom_histogram(binwidth = 2000, fill = "orange")
```

## What is the average daily activity pattern?


```r
# Line plot of steps taken over time
activity_data_grouped$date <- ymd(activity_data_grouped$date)

ggplot(activity_data_grouped, aes(date, average_steps))+
  geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#find which 5 minute interval on average has the most number of steps
activity_data_grouped2 <- activity_data_clean %>% 
  group_by(interval) %>% 
  summarize(total_steps = sum(steps), average_steps = mean(steps))

activity_data_grouped2[which.max(activity_data_grouped2$average_steps), 1]
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```

## Imputing missing values

```r
#Find the total number of NAs in the dataset 
sum(is.na(activity_data))
```

```
## [1] 2304
```

```r
#Save missing values in its own variable
ind <- which(is.na(activity_data$steps))
na_steps <- activity_data[ind,]

#join the missing data with their average steps by index
na_steps_joined <- left_join(x = na_steps, y = activity_data_grouped2, by = "interval" )

activity_data_na_replaced <- activity_data
#Replace the NA indexs with their average value

activity_data_na_replaced[ind,1] <- na_steps_joined$average_steps

activity_data_na_replaced_grouped <- activity_data_na_replaced %>% 
  group_by(date) %>% 
  summarize(total_steps = sum(steps), average_steps = mean(steps))

na_replaced_plot <- ggplot(activity_data_na_replaced_grouped, aes(total_steps))+
  geom_histogram(binwidth = 2000)

original_plot
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
na_replaced_plot
```

![](PA1_template_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
mean(activity_data_na_replaced_grouped$total_steps)
```

```
## [1] 10766.19
```

```r
median(activity_data_na_replaced_grouped$total_steps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
activity_data_na_replaced$wday <- wday(activity_data_na_replaced$date, label = T)

activity_data_na_replaced$on_weekend <- ifelse(activity_data_na_replaced$wday %in% c("Sat", "Sun"), yes = 1, no = 0)

table(activity_data_na_replaced$on_weekend, activity_data_na_replaced$wday )
```

```
##    
##      Sun  Mon  Tue  Wed  Thu  Fri  Sat
##   0    0 2592 2592 2592 2592 2592    0
##   1 2304    0    0    0    0    0 2304
```

```r
activity_data_na_replaced_grouped <- activity_data_na_replaced %>% 
  group_by(interval, on_weekend) %>% 
  summarise(mean_steps = mean(steps))

xyplot(mean_steps~interval|on_weekend, data=activity_data_na_replaced_grouped, type="l",  layout = c(1,2),
       main="mean Steps per Interval Based on weedays vs weekends", 
       ylab="Average Number of Steps", xlab="Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


