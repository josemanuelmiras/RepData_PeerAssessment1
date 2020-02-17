---
title: "PA1_template.Rmd"
author: "JMMA"
date: "14/2/2020"
output: html_document
---

Reproducible Research: Peer Assingment 1
==========================================
## by José Manuel Mirás Avalos
## February 2020

In this document, I will present my answers to the assigment 1 from the Reproducible Research course.
In this assignment, data from a personal activity monitoring device are used. 
The data consists of two months of data from an anonymous individual collected during the months of October and November in 2012 and include the number of steps taken in 5 minute intervals each day.

The objective of this work is to answer four questions by following a series of steps involving reading and transforming the data, making plots, calculating means and other statistical indicators.

First, let's see the guidelines that must be followed.

### Assignment Instructions
1.Code for reading in the dataset and/or processing the data
2.Histogram of the total number of steps taken each day
3.Mean and median number of steps taken each day
4.Time series plot of the average number of steps taken
5.The 5-minute interval that, on average, contains the maximum number of steps
6.Code to describe and show a strategy for imputing missing data
7.Histogram of the total number of steps taken each day after missing values are imputed
8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report


### Step 1
### Code for reading in the dataset and/or processing the data
First, I will set my working directory, download the data file and unzip it. For doing this, I will use the following code in R.

```{r, echo = TRUE}
###Setting the working directory
getwd()
setwd(getwd())

###This code is to check if the file already exists in the working directory. If not, this will download the dataset from the internet.
filename <- "Rawdata.zip"
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileURL, filename)
}  

###This code is for Checking whether folder exists. If not, it will unzip the file and create the "Dataset" directory.
if (!file.exists("Dataset")) { 
  unzip(filename) 
}
```

Once I got the data in my computer, I will proceed to load the data file and check its appearance by using several functions of R, including *head*, *dim*, and *str*.
Therefore, I will use this chunk of code:

```{r, echo = TRUE}
###Reading the data
activity <- read.csv("activity.csv")
head(activity)
dim(activity)
str(activity)
```

Then, I will transform the data in order to get the number of steps as a numeric vector and adding a new variable indicating the dates as weekdays or weekend. This is the code:

```{r, echo = TRUE, warning = FALSE}
###Transforming the data
activity$steps <- as.numeric(activity$steps)


activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity,weekday)
```

Now, it comes the time to answer the first question of the assignment, which is this one:
## Step 2
## What is mean total number of steps taken per day?
In order to answer this, I will create an histogram of the total number of steps taken each day using the following code chunk:

```{r, echo = TRUE, warning = FALSE}
total_steps <- aggregate(activity$steps, by = list(activity$date), FUN = sum, na.rm = TRUE)
names(total_steps) <- c("Date", "Total.Steps")

hist(total_steps$Total.Steps, main = "Histogram for the total number of steps per day", xlab = "Total steps per day", col = "darkgreen", ylim = c(0,20), breaks = 10)

```

So, let's move forward to the next step in this assignment.

## Step 3
## Mean and median number of steps taken each day
To answer this question, I will use the following R code:

```{r, echo = TRUE, warning = FALSE}
mean(total_steps$Total.Steps)
median(total_steps$Total.Steps)
```

As we can see, the average number of steps that this person makes per day is 9354.23, whereas the median is 10395.

## Step 4
## Time series plot of the average number of steps taken
In the assignment, it is required to design a plot (type = "l") for displaying the average number of steps taken per 5-minute interval. In order to answer this question, I will use the following code:

```{r, echo = TRUE, warning = FALSE}
daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN = mean, na.rm =TRUE)
names(daily_activity) <- c("Interval", "Mean")

plot(daily_activity$Interval, daily_activity$Mean, type = "l", col = "red", lwd =2, xlab = "5-minute Interval", ylab = "Average number of steps", main = "Average number of steps per 5-minute interval")
```

## Step 5
## The 5-minute interval that, on average, contains the maximum number of steps
In order to answer this question, I will use this code:
```{r, echo = TRUE, warning = FALSE}
daily_activity[which.max(daily_activity$Mean), ]$Interval
max(daily_activity$Mean)
```

As we can see, the 5-minute interval that contains the maximum number of steps is, on average, the interval 835.

## Step 6
## Code to describe and show a strategy for imputing missing data
The next step requires to write a code for imputing missing data. This can be done with the following chunk:

```{r, echo = TRUE, warning = FALSE}
sum(is.na(activity$steps))

imputed_steps <- daily_activity$Mean[match(activity$interval, daily_activity$Interval)]

activity_no_missing_data <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed_steps, no = activity$steps))
Total_Steps_Without_Missing_Data <- aggregate(steps ~ date, activity_no_missing_data, sum)
names(Total_Steps_Without_Missing_Data) <- c("Date", "Daily_steps")
```

## Step 7
## Histogram of the total number of steps taken each day after missing values are imputed
## What is mean total number of steps taken per day?
This step requires to create a histogram with the total number of steps taken each day after imputing missing values. Then, we are required to determine the mean total number of steps taken per day.
The histogram can be created using the following code chunk:

```{r, echo=TRUE, warning = FALSE}
hist(Total_Steps_Without_Missing_Data$Daily_steps, col = "darkgreen", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps per day", breaks = 10)
```

For calculating the mean and the median of the total number of steps taken per day, it is sufficient to use this:
```{r, echo = TRUE, warning = FALSE}
mean(Total_Steps_Without_Missing_Data$Daily_steps)
median(Total_Steps_Without_Missing_Data$Daily_steps)
```

So, answering the question posed in the assignment, the mean total number of steps taken per day is 10766.19. In this case, mean and median coincide.

## Step 8
## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
## Are there differences in activity patterns between weekdays and weekends?
In this step, we need to prepare a code for plotting the average number steps taken per 5-minute interval over the weekdays and weekends in order to compare both patterns.
As an answer to this part of the assignment, I prepared the following code chunk using the ggplot2 package:

```{r, echo = TRUE, warning = FALSE}
library(ggplot2)
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
  if (weekdays(x) == "sábado" | weekdays(x) =="domingo") 
  {y <- "Weekend"} else 
  {y <- "Weekday"}
  y
})

activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
  geom_line() +
  labs(title = "Average daily steps in weekdays and weekends", x = "Interval", y = "Average number of steps") +
  facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
```

From this figure, it seems that this individual has a different activity pattern during the weekdays, when compared with weekends. For instance, this individual takes more steps in the mornings during the weekdays but more over the day in the case of the weekends.

## Step 9
## All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
As you could see in my report, all the R code needed to reproduce the results given is included in this R markdown file. Hope you like it!

