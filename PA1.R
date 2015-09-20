library("dplyr")
library("lattice")

library(logging)
basicConfig()


## 1 Loading and preprocessing the data
# 1.1 Load the data (i.e. read.csv())
setwd("~/src/jhu-data-science-course/external/repdata-031/RepData_PeerAssessment1")
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}
d <- read.csv("activity.csv")


# 1.2 Process/transform the data (if necessary) into a format suitable for your analysis
### Total 

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

# 3.2 Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
avgStepsPerInterval[which.max(avgStepsPerInterval$mean.steps), ]$interval

### 4 Imputing missing values
# 4.1 Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)
d.na  <- filter(d, is.na(steps))
# d.na  <- d[is.na(d$steps) == TRUE, ]

# 4.2 Devise a strategy for filling in all of the missing values in the dataset.
# The strategy does not need to be sophisticated. For example, you could use the
# mean/median for that day, or the mean for that 5-minute interval, etc.

# Could us but assignment seems to not want it. library("zoo")
FillNaWithMeanOfInterval  <- function(x) {
  steps  <- x[1]
  interval  <- x[3]
  
  if(is.na(steps)) {
    avg  <- avgStepsPerInterval[as.numeric(avgStepsPerInterval$interval) == as.numeric(interval), ]
    avg$mean.steps
  } else {
    steps
  }  
}


# Create a new dataset that is equal to the original dataset but with the missing data filled in.
d2  <- d
d2$steps  <- apply(d2, 1, FUN=FillNaWithMeanOfInterval)


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

# Add factor to describe which segment of week date falls on. 
d2$sow <- ifelse(weekdays.Date(as.Date(d2$date)) %in% c("Saturday", "Sunday"),
                 "Weekend", "Weekday")

# 5.2 Make a panel plot containing a time series plot (i.e. type = "l") of the
# 5-minute interval (x-axis) and the average number of steps taken, averaged
# across all weekday days or weekend days (y-axis). See the README file in the
# GitHub repository to see an example of what this plot should look like using
# simulated data.


X-Axis - intervals
Y-Axis - mean(steps aggregated by sow)

d3  <- aggregate(steps ~ interval+sow, 
                 data=d2, 
                 FUN=function(e) mean(as.numeric(e)))

xyplot(steps ~ interval | sow, data = d3, type="l", layout=c(1,2))

