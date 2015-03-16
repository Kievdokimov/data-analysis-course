---
title: "PA1_template.Rmd"
output: html_document
---

Firstly, we load data into R


```r
data<-read.csv("./activity.csv")
```

Here is a histogram of the total number of steps taken each day


```r
step_sum<-tapply(data$steps, data$date, sum)
  hist(step_sum, main="Histogram of total number of steps taken each day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

The mean and median of the total number of steps taken per day are as follows:

```r
mean<-mean(step_sum, na.rm=T)
median<-median(step_sum, na.rm=T)
```
Mean =1.0766189 &times; 10<sup>4</sup>  
Median =10765



```r
daily_activ<-tapply(data$steps, data$interval, mean, na.rm="T")
  x<-levels(factor(data$interval))
  max_5_min<-data[which(max(daily_activ)==daily_activ),3]
```
As can be seen from the plot below 835th 5-minute interval contains the maximum number of steps


```r
plot(x,daily_activ, type="l", main="Average daily activity pattern",xaxt="n")
axis(1, at = seq(0, 3000, by = 100), las=2)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


```r
count_na<-sum(is.na(data$steps))
```

Total number of missing values in the dataset is 2304.

Filling NA-s with the mean for that 5-minute interval.

```r
# x<-levels(factor(data$interval))
data2<-data
  
  for (i in 1:length(data2[,1]))
    {if(is.na(data2[i,1]))
        data2[i,1]<-daily_activ[which(as.numeric(x)==data2[i,3])]
    }
```

New histogram:


```r
step_sum2<-tapply(data2$steps, data2$date, sum)
hist(step_sum2)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
mean2<-mean(step_sum2, na.rm=F)
median2<-median(step_sum2, na.rm=F)
```

Mean2 =1.0766189 &times; 10<sup>4</sup>  
Median2 =1.0766189 &times; 10<sup>4</sup>

As we filled Na-s with the mean for that 5-minute interval the resulted mean remained unchanged and median became equal to mean.


```r
date2 <- data.frame(lapply(data2$date, as.Date), stringsAsFactors=FALSE)
  date2<-sapply(date2,weekdays)
  date_f<-factor(date2)
  levels(date_f)[1:5]<-"Weekday"
  levels(date_f)[2:3]<-"Weekend"
  
  data_weekday<-data2[which(date_f=="Weekday"),]
  data_weekend<-data2[which(date_f=="Weekend"),]
  
  activ_weekdays<-tapply(data_weekday$steps, data_weekday$interval, mean, na.rm="T")
  activ_weekends<-tapply(data_weekend$steps, data_weekend$interval, mean, na.rm="T")
```
Comparison of activities on weekdays and weekends:


```r
par(bg="transparent")
 par(mfrow=c(2,1))
 plot(x,activ_weekdays, type="l", main="Average daily activity on weekdays",xlab="interval",ylab="number of steps")
 plot(x,activ_weekends, type="l", main="Average daily activity on weekends",xlab="interval",ylab="number of steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

