# loading libraries
library(dplyr)
library(ggplot2)
library(lattice)

# load the data
data <- read.csv("./data/activity.csv", sep = ",")

# check the data
str(data)
summary(data)

# format the data
data$date <- as.Date(data$date)
data$weekday <- as.POSIXlt(data$date)$wday
dataNoNA <- na.omit(data)
dataGrouped <- group_by(data, date)
dataGroupedInt <- group_by(data, interval)

# calculate the sum of steps per day
stepsPerDay <- summarise(dataGrouped, stepsSum = sum(steps, na.rm = TRUE))
stepsPerInterval <- summarise(dataGroupedInt, stepsSum = mean(steps, na.rm = TRUE))

# calculate the mean and median of steps per day -> One value
summarise(stepsPerDay, mean(stepsSum), median(stepsSum))

# identify na's
sum(is.na(data$steps))

# hist(stepsPerDay$stepsSum, xlab="Sum of Steps", ylab="Count", main="Total Number of Steps per Day")
histogram( ~ stepsSum,data=stepsPerDay, type="count", xlab="Sum of Steps", ylab="Count", main="Total Number of Steps per Day")
xyplot(ts(stepsPerInterval$stepsSum), xlab="5-minute intervall", ylab="Steps")


arrange(stepsPerInterval, desc(stepsSum))

data$weekday <- (data$weekday > 0 & data$weekday < 6)
data$weekday = factor(data$weekday, labels = c("Weekend", "Weekday"))
dataGroupedWeekday <- group_by(data, interval, weekday)

stepsPerWeekday <- summarise(dataGroupedWeekday, steps = mean(steps, na.rm = TRUE))

ggplot(stepsPerWeekday, aes(interval, steps)) +
  geom_line() +
  facet_grid(weekday ~ .) +
  theme_bw() +
  labs(x = "Interval",
       y = "Number of Average Steps")