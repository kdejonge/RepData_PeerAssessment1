# Set workspace and load data

setwd("D:/OneDrive - Esri Nederland/Kristin de Jonge/Studie/DataScience/Data Science Specialization/Course5")
data <- read.csv("activity.csv")

# Check data
head(data)
summary(data)

# Complete data
mydata <- as.data.frame(data[complete.cases(data), ])
summary(mydata)

# Total steps a day
TotalStepsaDay <- tapply(mydata$steps, mydata$date, sum)
hist(TotalStepsaDay)

# Mean total steps a day
meanTotStepsDay <- mean(TotalStepsaDay, na.rm = TRUE)
meanTotStepsDay

# Median total steps a day
medianTotStepsDay <- median(TotalStepsaDay, na.rm = TRUE)
medianTotStepsDay

# Average Daily activity pattern
avgDailActPatt <- aggregate(mydata$steps, by = list(mydata$interval), FUN = mean)
summary(avgDailActPatt)
plot(avgDailActPatt[, 1], avgDailActPatt[, 2], type = "l", xlab = "5 min Intervals in a day", ylab = "Average Number of Steps", main = "The Average Daily Activity Pattern")
maxStepsIntervalTime <- avgDailActPatt[which.max(avgDailActPatt[, 2]), 1]
maxStepsIntervalTime

# Missing Values
summary(data)
sum(!complete.cases(data))

# Fill missing values with the mean
mydata2 <- data
data1 = nrow(mydata2)
data2 = nrow(avgDailActPatt)
for (i in 1:data1) {
  if (is.na(mydata2$steps[i])) {
    for (j in 1:data2) {
      if (mydata2$interval[i] == avgDailActPatt[j, 1]) {
        mydata2$steps[i] = avgDailActPatt[j, 2]
      }
    } 
  }    
}
summary(mydata2)

# Recalculate total, mean and median
totalStepsaDay2 <- tapply(mydata2$steps, mydata2$date, sum)
hist(totalStepsaDay2)

meanTotStepsDay <- mean(totalStepsaDay2, na.rm = TRUE)
meanTotStepsDay

medianTotStepsDay <- median(totalStepsaDay2, na.rm = TRUE)
medianTotStepsDay

# Week and weekday

# add new column
mydata$weekday = TRUE

# fill new column
weekday <- weekdays(as.POSIXct(mydata$date, format = "%Y-%m-%d" ))
for (i in 1:length(weekday)) {
  if (weekday[i] == "Saturday" | weekday[i] == "Sunday") {
    mydata$weekday[i] = FALSE
  }
}
Weekday <- mydata[which(mydata$weekday == TRUE), ]
Weekend <-mydata[which(mydata$weekday == FALSE), ]

avgWeekdayPatt <- aggregate(Weekday$steps, by = list(Weekday$interval), FUN = mean)
names(avgWeekdayPatt) <- c("interval", "steps")
avgWeekdayPatt$dayTag <- "weekday"
avgWeekendPatt <- aggregate(Weekend$steps, by = list(Weekend$interval), FUN = mean)
names(avgWeekendPatt) <- c("interval", "steps")
avgWeekendPatt$dayTag <- "weekend"

avgPatt <- rbind(avgWeekdayPatt, avgWeekendPatt)

library(lattice)
xyplot(steps ~ interval | dayTag, data = avgPatt, type = "l", layout = c(1, 2))

