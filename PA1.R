library("dplyr")
library(ggplot2)

## 1 Loading and preprocessing the data
# 1.1 Load the data (i.e. read.csv())
setwd("~/src/jhu-data-science-course/external/repdata-031/RepData_PeerAssessment1")
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}
d <- read.csv("activity.csv")


# 1.2 Process/transform the data (if necessary) into a format suitable for your analysis

## 2 What is mean total number of steps taken per day?
# 2.1 Calculate the total number of steps taken per day
stepsPerDay <- aggregate(x=d[c("steps")], FUN=sum, by = list(day = d$date))
## Alternative
#hist(sapply(split(d$steps, d$date), sum),
#     xlab = "Number of steps in a day", ylab = "Frequency (Number of days)",
#     main = "Total number steps taken per day")


# 2.2 If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
hist(stepsPerDay$steps, freq=T,
     main="Steps Taken Per Day",
     xlab="Steps Taken")

# 2.3 Calculate and report the mean and median of the total number of steps taken per day
mean(stepsPerDay$steps, na.rm=T)
median(stepsPerDay$steps, na.rm=T)


## 3 What is the average daily activity pattern?
# 3.1 Make a time series plot (i.e. type = "l") of the 5-minute interval
# (x-axis) and the average number of steps taken, averaged across all days (y-axis)
avgStepsPerInterval <- aggregate(x=d[c("steps")],
                              FUN=function(e) { mean(e, na.rm=T)},
                              by = list(interval = d$interval))
avgStepsPerInterval  <- rename(avgStepsPerInterval, mean.steps = steps)

plot(avgStepsPerInterval, type="l",
     main="Average Steps Take", xlab="Interval", ylab="Avg. Steps")
str(avgStepsPerInterval)
summarise(avgStepsPerInterval)

# 3.2 Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
avgStepsPerInterval[which.max(avgStepsPerInterval$mean.steps), ]$interval

### 4 Imputing missing values
# 4.1 Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)
d.na  <- filter(d, is.na(steps))
# nas  <- d[is.na(d$steps) == TRUE, ]

# 4.2 Devise a strategy for filling in all of the missing values in the dataset.
# The strategy does not need to be sophisticated. For example, you could use the
# mean/median for that day, or the mean for that 5-minute interval, etc.

# Could us but assignment seems to not want it. library("zoo")


# create test set
t <- head(d, 10)
t <- rbind(t, tail(d[which(d$steps > 0), ]))
# t[is.na(t$steps), ] // Selects only rows that have NA for steps


t[is.na(t$steps)] <- avgStepsPerInterval$interval == t$interval
avgStepsPerInterval$interval == t[t$interval == 0]

ifelse(is.na(t$steps), avgStepsPerInterval[t$interval], t$steps)

d2 <- d
d2$steps <- ifelse(is.na(d$steps),
            avgStepsPerInterval[avgStepsPerInterval$interval == d$interval, ]$mean.steps,
            d$steps)

d2StepsPerDay <- aggregate(x=d2[c("steps")], FUN=sum, by = list(day = d2$date))

hist(d2StepsPerDay$steps, freq=T,
     main="Steps Taken Per Day",
     xlab="Steps Taken")
mean(d2StepsPerDay$steps, na.rm=T)
median(d2StepsPerDay$steps, na.rm=T)

### 5 Are there differences in activity patterns between weekdays and weekends?
### For this part the weekdays() function may be of some help here.
### Use the dataset with the filled-in missing values for this part.

# 5.1 Create a new factor variable in the dataset with two levels –
# “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

# For testing
#t <- head(d2)
#t <- rbind(t, tail(d[which(weekdays.Date(as.Date(d$date)) == "Saturday"), ]))
#t


d2$sow <- ifelse(weekdays.Date(as.Date(t$date)) %in% c("Saturday", "Sunday"),
                 "Weekend", "Weekday")

# 5.2 Make a panel plot containing a time series plot (i.e. type = "l") of the
# 5-minute interval (x-axis) and the average number of steps taken, averaged
# across all weekday days or weekend days (y-axis). See the README file in the
# GitHub repository to see an example of what this plot should look like using
# simulated data.

d2AvgStepsPerInterval <- aggregate(x=d2[c("steps")], mean,
                                 by = list(interval = d2$interval))

d2AvgStepsPerInterval  <- rename(d2AvgStepsPerInterval, mean.steps = steps)

d2AvgStepsPerInterval[which.max(d2AvgStepsPerInterval$steps), ]$interval
