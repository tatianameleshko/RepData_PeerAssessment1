# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(ggplot2)
activity_file <- unzip("activity.zip")
activity_df<-read.csv(activity_file)
activity_df <- transform(activity_df,date=as.Date(date))
```

## What is mean total number of steps taken per day?

###Make a histogram of the total number of steps taken each day

```r
ggplot(data = activity_df, aes(date, steps)) + stat_summary(fun.y = sum,geom = "bar")+ggtitle("Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

###Calculate and report the mean and median total number of steps taken per day


```r
result1_df<-aggregate(activity_df[, 1], list(activity_df$date), sum)
mean(result1_df$x,na.rm = T)
```

```
## [1] 10766.19
```

```r
median(result1_df$x,na.rm = T)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

###Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
result2_df<-aggregate(.~interval,activity_df,FUN=mean,na.rm=T,na.action = NULL)
plot(result2_df$steps~result2_df$interval,type="l",main="Average number of steps taken", ylab="Steps", xlab="Intervals ")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
result2_df$interval[which.max(result2_df$steps)]
```

```
## [1] 835
```


## Imputing missing values

###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity_df$steps))
```

```
## [1] 2304
```

```r
sum(is.na(activity_df$date))
```

```
## [1] 0
```

```r
sum(is.na(activity_df$interval))
```

```
## [1] 0
```

###Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. 
####I used mean for that 5-minute interval.

###Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity2_df<-activity_df
na_values<-sapply(which(is.na(activity_df$steps)),function(x) result2_df$steps[result2_df$interval==activity_df$interval[x]])
activity2_df$steps[is.na(activity_df$steps)] <- na_values
```

###Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
ggplot(data = activity2_df, aes(date, steps)) + stat_summary(fun.y = sum,geom = "bar")+ggtitle("Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
result3_df<-aggregate(activity2_df[, 1], list(activity2_df$date), sum)
mean(result3_df$x)
```

```
## [1] 10766.19
```

```r
median(result3_df$x)
```

```
## [1] 10766.19
```
####Mean and median of new dataframe are 1.0766189\times 10^{4} and 1.0766189\times 10^{4}.
####Mean and median of original dataframe are 1.0766189\times 10^{4} and 10765. Mean stays the same, median slightly increased after imputing.

## Are there differences in activity patterns between weekdays and weekends?

###Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
activity2_df$factor_w <- "weekday"
activity2_df$factor_w[weekdays(activity2_df$date) %in% c("Saturday","Sunday")] <-"weekend"
activity2_df <- transform(activity2_df,factor_w = as.factor(factor_w))
```

###Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
result4_df<-aggregate(activity2_df[, 1], list(activity2_df$interval,activity2_df$factor_w),mean)
colnames(result4_df) <- c("interval","factor_w","steps")
qplot(interval,steps,data=result4_df,facets = factor_w~.,geom = "line",main="Average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
