Load Data
---------

Load data from the file (assuming it is in your working directory).
Download from
here:<https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>

    activity <- read.csv("repdata_data_activity/activity.csv")

Total Steps Per Day
-------------------

To do analysis we will use dplyr. This code will load dplyr and output
summary information on steps per day

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    Total_Steps <- activity %>%
            group_by(date) %>%
            dplyr::summarize(Total_Steps=sum(steps, na.rm=TRUE))
    Total_Steps

    ## # A tibble: 61 x 2
    ##    date       Total_Steps
    ##    <fct>            <int>
    ##  1 2012-10-01           0
    ##  2 2012-10-02         126
    ##  3 2012-10-03       11352
    ##  4 2012-10-04       12116
    ##  5 2012-10-05       13294
    ##  6 2012-10-06       15420
    ##  7 2012-10-07       11015
    ##  8 2012-10-08           0
    ##  9 2012-10-09       12811
    ## 10 2012-10-10        9900
    ## # … with 51 more rows

    hist(Total_Steps$Total_Steps, breaks=10, main="Daily Step Total Frequency", xlab="Steps in a day")

![](PA1_template_files/figure-markdown_strict/steps%20per%20day-1.png)

    Total_Steps %>%
            dplyr::summarize(Mean_Total_Steps=mean(Total_Steps), Median_Total_Steps=median(Total_Steps))

    ## # A tibble: 1 x 2
    ##   Mean_Total_Steps Median_Total_Steps
    ##              <dbl>              <int>
    ## 1            9354.              10395

Average Daily Pattern
---------------------

To do analysis we will use dplyr. Here we will average the steps in each
interval to see when more steps are taken within the days.

    library(dplyr)
    Average_Interval_Steps <- activity %>%
            group_by(interval) %>%
            dplyr::summarize(Average_Steps=mean(steps, na.rm=TRUE))

    plot(Average_Interval_Steps$interval, Average_Interval_Steps$Average_Steps, type="l", main="Time Series, Steps per Interval", ylab="Steps", xlab="Interval")

![](PA1_template_files/figure-markdown_strict/average%20daily%20pattern-1.png)

    Average_Interval_Steps %>%
            filter(Average_Steps==max(Average_Steps))

    ## # A tibble: 1 x 2
    ##   interval Average_Steps
    ##      <int>         <dbl>
    ## 1      835          206.

NA Analysis
-----------

To do analysis we will use dplyr. Here we analyze and address NA issues.

    library(dplyr)
    activity %>%
            dplyr::summarize(NAs_in_Step_Data=sum(is.na(steps)))

    ##   NAs_in_Step_Data
    ## 1             2304

    ## Using a general purpose for loop 
    ## (it checks all columns for numeric data, even if in this case there is only 1)
    ## This will replace NAs with the mean based on interval
    clean_activity<-activity
    for (i in which(sapply(clean_activity, is.numeric))) {
        for (j in which(is.na(clean_activity[, i]))) {
            clean_activity[j, i] <- 
                    mean(clean_activity[clean_activity[, "interval"] == clean_activity[j, "interval"], i]
                         ,  na.rm = TRUE)
        }
    }

    Total_Clean_Steps <- clean_activity %>%
            group_by(date) %>%
            dplyr::summarize(Total_Steps=sum(steps, na.rm=TRUE))

    hist(Total_Clean_Steps$Total_Steps, breaks=10, main="Daily Step Total Frequency", xlab="Steps in a day")

![](PA1_template_files/figure-markdown_strict/NA%20analysis-1.png)

    Total_Clean_Steps %>%
            dplyr::summarize(Mean_Total_Steps=mean(Total_Steps), Median_Total_Steps=median(Total_Steps))

    ## # A tibble: 1 x 2
    ##   Mean_Total_Steps Median_Total_Steps
    ##              <dbl>              <dbl>
    ## 1           10766.             10766.

Weekend Analysis
----------------

Here we analyze the data to see if there is any difference between
weekdays and weekend.

    library(dplyr)
    library(ggplot2)

    Weekday_Analysis<- clean_activity %>%
            mutate(Day_Type=weekdays(as.Date(date))) %>%
                           mutate(Day_Type=replace(Day_Type, 
                                                   Day_Type=="Saturday"| Day_Type=="Sunday", "Weekend")) %>%
                            mutate(Day_Type=replace(Day_Type, Day_Type!="Weekend", "Weekday" ))
     
    Weekday_Analysis_Interval_Steps <- Weekday_Analysis %>%
            group_by(interval, Day_Type) %>%
            dplyr::summarize(Steps=mean(steps, na.rm=TRUE))

    ggplot(data = Weekday_Analysis_Interval_Steps, aes(x = interval, y = Steps)) + geom_line(color = "#00AFBB", size = 1) + facet_wrap(Day_Type ~ ., ncol=1)

![](PA1_template_files/figure-markdown_strict/Weekend%20Analysis-1.png)
