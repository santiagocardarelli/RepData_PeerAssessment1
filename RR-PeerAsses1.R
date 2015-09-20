library(knitr)

opts_chunk$set(echo = TRUE, results = 'hold')

library(data.table)
library(ggplot2) # we shall use ggplot2 for plotting figures

# Load the raw activity data
rdata <- read.csv('data/activity.csv', header = TRUE, sep = ",",colClasses=c("numeric", "character", "numeric"))

#tidy the data or preprocess the data
rdata$date <- as.Date(rdata$date, format = "%Y-%m-%d")
rdata$interval <- as.factor(rdata$interval)

#1-What is mean total number of steps taken per day?
#Calculate the total number of steps taken per day
steps_per_day <- aggregate(steps ~ date, rdata, sum)
colnames(steps_per_day) <- c("date","steps")

ggplot(steps_per_day, aes(x = steps)) + 
  geom_histogram(fill = "green", binwidth = 1000) + 
  labs(title="Histogram of Steps Taken per Day", 
       x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 

#Calculate and report the mean and median of the total number of steps taken per day
steps_mean   <- mean(steps_per_day$steps, na.rm=TRUE)
steps_median <- median(steps_per_day$steps, na.rm=TRUE)


#2-What is the average daily activity pattern?
#Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
steps_per_interval <- aggregate(rdata$steps, 
                                by = list(interval = rdata$interval),
                                FUN=mean, na.rm=TRUE)
#convert to integers
##this helps in plotting
steps_per_interval$interval <- 
  as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])
colnames(steps_per_interval) <- c("interval", "steps")

ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
  geom_line(color="orange", size=1) +  
  labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
  theme_bw()

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_interval <- steps_per_interval[which.max(steps_per_interval$steps),]


#3-Imputing missing values

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missing_vals <- sum(is.na(rdata$steps))

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#Create a new dataset that is equal to the original dataset but with the missing data filled in.
na_fill <- function(data, pervalue) {
  na_index <- which(is.na(data$steps))
  na_replace <- unlist(lapply(na_index, FUN=function(idx){
    interval = data[idx,]$interval
    pervalue[pervalue$interval == interval,]$steps
  }))
  fill_steps <- data$steps
  fill_steps[na_index] <- na_replace
  fill_steps
}

rdata_fill <- data.frame(  
  steps = na_fill(rdata, steps_per_interval),  
  date = rdata$date,  
  interval = rdata$interval)
str(rdata_fill)

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
fill_steps_per_day <- aggregate(steps ~ date, rdata_fill, sum)
colnames(fill_steps_per_day) <- c("date","steps")

##plotting the histogram
ggplot(fill_steps_per_day, aes(x = steps)) + 
  geom_histogram(fill = "blue", binwidth = 1000) + 
  labs(title="Histogram of Steps Taken per Day", 
       x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 



#Do these values differ from the estimates from the first part of the assignment?
#Calculate and report the mean and median total number of steps taken per day.

steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)
steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)



#What is the impact of imputing missing data on the estimates of the total daily number of steps?
#As you can see, comparing with the calculations done in the first section of this document, we observe that while the mean value remains unchanged, the median value has shifted and virtual matches to the mean.
#Since our data has shown a t-student distribution (see both histograms), it seems that the impact of imputing missing values has increase our peak, but it's not affect negatively our predictions.


#4-Are there differences in activity patterns between weekdays and weekends?
#activity <- read.csv("activity.csv", colClasses = c("numeric", "character","numeric"))
#activity <- read.csv("data/activity.csv", colClasses = c("numeric", "character","numeric"))
library(lattice)
activity <- read.csv('data/activity.csv', header = TRUE, sep = ",",colClasses=c("numeric", "character", "numeric"))
activity$date <- as.Date(activity$date, "%Y-%m-%d")
day <- weekdays(activity$date)
daylevel <- vector()
for (i in 1:nrow(activity)) {
  if (day[i] == "sábado") {
    daylevel[i] <- "Weekend"
  } else if (day[i] == "domingo") {
    daylevel[i] <- "Weekend"
  } else {
    daylevel[i] <- "Weekday"
  }
}
activity$daylevel <- daylevel
activity$daylevel <- factor(activity$daylevel)

stepsByDay <- aggregate(steps ~ interval + daylevel, data = activity, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")
#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:
  
  xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
         xlab = "Interval", ylab = "Number of steps")