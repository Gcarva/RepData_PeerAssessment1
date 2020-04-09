---
title: "Course 5 Project 1"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Load Data

Load data from the file (assuming it is in your working directory). Download from here:https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

```{r load, echo=TRUE}
activity <- read.csv("repdata_data_activity/activity.csv")
```

## Total Steps Per Day

To do analysis we will use dplyr. This code will load dplyr and output summary information on steps per day

```{r steps per day, echo=TRUE}
library(dplyr)
Total_Steps <- activity %>%
        group_by(date) %>%
        dplyr::summarize(Total_Steps=sum(steps, na.rm=TRUE))
Total_Steps

hist(Total_Steps$Total_Steps, breaks=10, main="Daily Step Total Frequency", xlab="Steps in a day")

Total_Steps %>%
        dplyr::summarize(Mean_Total_Steps=mean(Total_Steps), Median_Total_Steps=median(Total_Steps))


```
## Average Daily Pattern

To do analysis we will use dplyr. Here we will average the steps in each interval to see when more steps are taken within the days.

```{r average daily pattern, echo=TRUE}
library(dplyr)
Average_Interval_Steps <- activity %>%
        group_by(interval) %>%
        dplyr::summarize(Average_Steps=mean(steps, na.rm=TRUE))

plot(Average_Interval_Steps$interval, Average_Interval_Steps$Average_Steps, type="l", main="Time Series, Steps per Interval", ylab="Steps", xlab="Interval")

Average_Interval_Steps %>%
        filter(Average_Steps==max(Average_Steps))
```

## NA Analysis

To do analysis we will use dplyr. Here we analyze and address NA issues.

```{r NA analysis, echo=TRUE}
library(dplyr)
activity %>%
        dplyr::summarize(NAs_in_Step_Data=sum(is.na(steps)))

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

Total_Clean_Steps %>%
        dplyr::summarize(Mean_Total_Steps=mean(Total_Steps), Median_Total_Steps=median(Total_Steps))
```
## Weekend Analysis

Here we analyze the data to see if there is any difference between weekdays and weekend.

```{r Weekend Analysis, echo=TRUE}
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


       
```