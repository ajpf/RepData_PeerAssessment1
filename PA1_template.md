Reproducible Research Project 1 JHU
===================================

What is mean total number of steps taken per day?
-------------------------------------------------

Loading Data

    activity<-read.csv("activity.csv")

1.  Calculate the total number of steps taken per day

<!-- -->

    library(dplyr)

    stepsDate<-activity%>%
            group_by(date)%>%
            summarise(steps=sum(steps))

1.  Make a histogram of the total number of steps taken each day

<!-- -->

    library(ggplot2)

    ggplot(stepsDate,aes(steps),gridline=FALSE)+
            geom_histogram(binwidth=2000,fill="blue4")+
            labs(title="Total number of steps per day",x="No of Steps",y="Frequency")+
            theme(panel.background = element_blank())

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

1.  Calculate and report the mean and median of the total number of
    steps taken per day

<!-- -->

    smean<-mean(stepsDate$steps, na.rm = TRUE)
    smedian<-median(stepsDate$steps, na.rm = TRUE)

The mean is 1.076618910^{4} and the median is 10765

What is the average daily activity pattern?
-------------------------------------------

1.  Make a time series plot (i.e. type = "l") of the 5-minute
    interval (x-axis) and the average number of steps taken, averaged
    across all days (y-axis)

<!-- -->

    average<-activity%>%
            group_by(interval)%>%
            summarise(averageDay=mean(steps, na.rm=TRUE))

    ggplot(average,aes(interval,averageDay))+
            geom_line(linetype=1,color="brown",size=1)+
            labs(title="Average number of steps per interval",y="No of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    maxInterval<-filter(average,averageDay==max(averageDay))

Interval 835 with 206.1698113 number of steps.

Imputing missing values
-----------------------

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs)

<!-- -->

    summary(activity)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

    naRows<-data.frame(table(!is.na(activity)))

Number of rows with NAs is 2304

1.  Devise a strategy for filling in all of the missing values in
    the dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

The following algorithm replaces the NA´s by steps average by interval:

    average<-activity%>%
            group_by(interval)%>%
            summarise(averageDay=mean(steps, na.rm=TRUE))

    activityNew<-left_join(activity,average,by="interval")

    activityNew<-activityNew%>%
            mutate(steps1=ifelse(is.na(steps),averageDay,steps))%>%
            select(steps1,date,interval)%>%
            rename(steps=steps1)%>%
            mutate(date=as.Date(date))

1.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in

<!-- -->

    average<-activity%>%
            group_by(interval)%>%
            summarise(averageDay=mean(steps, na.rm=TRUE))

    activityNew<-left_join(activity,average,by="interval")

    activityNew<-activityNew%>%
            mutate(steps1=ifelse(is.na(steps),averageDay,steps))%>%
            select(steps1,date,interval)%>%
            rename(steps=steps1)%>%
            mutate(date=as.Date(date))
    head(activityNew)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

1.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

<!-- -->

    stepsDateNew<-activityNew%>%
            group_by(date)%>%
            summarise(steps=sum(steps))

    ggplot(stepsDateNew,aes(steps),gridline=FALSE)+
            geom_histogram(binwidth=2000,fill="darkgreen")+
            labs(title="Total number of steps per day, NA´s replaced",x="No of               
            Steps",y="Frequency")+
            theme(panel.background = element_blank())

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    nmean<-mean(stepsDateNew$steps, na.rm = TRUE)
    nmedian<-median(stepsDateNew$steps, na.rm = TRUE)

The mean is 1.076618910^{4} and the median is 1.076618910^{4}, since
there is plenty of data... replacing NA´s value by the mean for that
5-minute interval does not have much impact on the outcome.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.  Create a new factor variable in the dataset with two levels -
    "weekday" and "weekend" indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    activityNew$day<-weekdays(activityNew$date)
    activityNew$day<-ifelse(activityNew$day=="Saturday" | activityNew$day=="Sunday","weekend","weekday")


    head(activityNew)

    ##       steps       date interval     day
    ## 1 1.7169811 2012-10-01        0 weekday
    ## 2 0.3396226 2012-10-01        5 weekday
    ## 3 0.1320755 2012-10-01       10 weekday
    ## 4 0.1509434 2012-10-01       15 weekday
    ## 5 0.0754717 2012-10-01       20 weekday
    ## 6 2.0943396 2012-10-01       25 weekday

1.  Make a panel plot containing a time series plot (i.e. type = "l") of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

<!-- -->

    weekActivity<-activityNew%>%
            group_by(interval,day)%>%
            summarise(stepsAverage=mean(steps))

    ggplot(weekActivity,aes(interval,stepsAverage))+
            geom_line(color="darkslateblue")+
            facet_grid(day~.)+
            labs(title="Steps by weekday/weekend",y="No of steps")

![Sample panel plot](instructions_fig/sample_panelplot.png)
