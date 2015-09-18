
setwd("~/src/jhu-data-science-course/external/repdata-031/RepData_PeerAssessment1")
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}

d <- read.csv("activity.csv")

class(d)
stepsPerDay = aggregate(x=d[c("steps")], FUN=sum, by = list(day = d$date))
hist(stepsPerDay$steps, main="Steps Taken Per Day", xlab="Steps Taken", breaks=c(5))

mean(stepsPerDay$steps, na.rm=T)
median(stepsPerDay$steps, na.rm=T)

plot()
