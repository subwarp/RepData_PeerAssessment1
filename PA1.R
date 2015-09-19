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
avgStepsPerInterval[which.max(avgStepsPerInterval$steps), ]$interval

### 4 Imputing missing values
# 4.1 Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
d.na  <- filter(d, is.na(steps))
# nas  <- d[is.na(d$steps) == TRUE, ]

# 4.2 Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use the 
# mean/median for that day, or the mean for that 5-minute interval, etc.
library("zoo")

lapply(head(d, 2), function(irec) {
  print("In Loop")
  print(irec$steps)
})
