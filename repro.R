# loading libraries
library(dplyr)
library(lattice)

# load the data
data <- read.csv("./data/activity.csv", sep = ",")

# check the data
str(data)
summary(data)

# format the data
data$date <- as.Date(data$date)

# calculate the mean and median of steps per day
dataGrouped <- group_by(data, date)

summarise(dataGrouped, mean(steps))
summarise(dataGrouped, median(steps))

mean(tapply(data$steps,data$date,sum), na.rm=TRUE)
median(tapply(data$steps,data$date,sum), na.rm=TRUE)

# calculate the sum of steps per day
dataSum <- summarise(dataGrouped, stepsSum = sum(steps))

# identify na's
which(is.na(data$steps))
sum(is.na(data$steps))

histogram( ~ stepsSum,data=dataSum, type="count", xlab="Sum of Steps", ylab="Count", main="Total Number of Steps per Day")
