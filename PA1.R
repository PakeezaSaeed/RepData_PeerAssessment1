library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(data.table)
library(zoo)

#Read Data into R
data <- read_csv("activity.csv")
#Convert date variable into class date
#data$date <- as.Date(data$date)

#Calculate total number of steps taken per day
data$totsteps <- data %>% group_by(date) %>% summarise(TotalSteps = sum(steps))

#Make a histogram of the total number of steps taken each days
stepsHist <- ggplot(totSteps, aes(date,TotalSteps))
stepsHist + geom_histogram(stat="identity",position="dodge")

#Calculate and report the mean and median of the total number of steps taken per day
gdata <- data %>% group_by(date)
stepsMean <- gdata %>% summarise(mean(steps, na.rm = TRUE))
stepsMedian <- gdata %>% summarise(median(steps, na.rm = TRUE))

#Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") 
#of the 5-minute interval (x-axis) and the average number of steps 
#taken, averaged across all days (y-axis)

idata <- data %>% group_by(interval) %>% summarise(AvgSteps = mean(steps, na.rm = TRUE))
iplot <- ggplot(idata, aes(interval, AvgSteps))
iplot +geom_line(color = "red")

#Which 5-minute interval, on average across all the days in the 
#dataset, contains the maximum number of steps?
maxinterval <- filter(idata, AvgSteps == max(AvgSteps))

#Calculate and report the total number of missing values in the 
#dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

nas <- sum(is.na(data$steps))

#Devise a strategy for filling in all of the missing values in the dataset.

library(zoo)
data2 <- transform(data, newsteps = na.aggregate(steps, by = interval))

#Histogram of total number of steps per day 
totStepsNew <- data2 %>% group_by(date) %>% summarise(TotalSteps2 = sum(newsteps))
stepsHistNew <- ggplot(totStepsNew, aes(date,TotalSteps2))
stepsHistNew + geom_histogram(stat="identity",position="dodge")

#mean and median
newmean <- data2 %>% group_by(date) %>% summarise(newmean = mean(newsteps))
newmedian <- data2 %>% group_by(date) %>% summarise(newmedian = median(newsteps))

#Extracting names of weekdays 
data2$days <- weekdays(data2$date)
#Creating Factors with 2 levels
data2 <- data2 %>% 
   mutate(days = as.factor(case_when(
         days %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekdays",
         days %in% c("Saturday", "Sunday") ~ "Weekend"
       )))
#Make a panel plot containing a time series plot 
#(i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval
#(x-axis) and the average number of steps taken, averaged across all 
#weekday days or weekend days (y-axis). 
data2 <- data2 %>% group_by(interval) %>% summarise(AvgSteps2 = mean(newsteps, na.rm = TRUE))
newplot <- ggplot(data2, aes(interval, AvgSteps2))
newplot +geom_line(color = "blue")









