
setwd("~/src/jhu-data-science-course/external/repdata-031/RepData_PeerAssessment1")
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}

d <- read.csv("activity.csv")

class(d)



## Alternative
#hist(sapply(split(d$steps, d$date), sum),
#     xlab = "Number of steps in a day", ylab = "Frequency (Number of days)",
#     main = "Total number steps taken per day")

stepsPerDay <- aggregate(x=d[c("steps")], FUN=sum, by = list(day = d$date))
class(stepsPerDay$day)
library(ggplot2)
hist(stepsPerDay$steps, freq=T,
     main="Steps Taken Per Day",
     xlab="Steps Taken")

mean(stepsPerDay$steps, na.rm=T)
median(stepsPerDay$steps, na.rm=T)

plot(d$interval, d$steps, type="l")

stepsPerInterval <- aggregate(x=d[c("steps")],
                              FUN=function(e) { mean(e, na.rm=T)},
                              by = list(interval = d$interval))

