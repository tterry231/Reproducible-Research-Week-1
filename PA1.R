#
# Load the "activity.csv" dataset to working data frame
#

act <- read.csv("activity.csv")

#
# Exploratory review of the data finds NA values in "steps"
#

summary(act$steps)

#
# Remove NA values from working data frame and create a clean data frame
#

act_clean <- act[complete.cases(act), ]

#
# Calculate the total number of steps take per day
#

steps_day <- aggregate(steps ~ date, act_clean, sum)

#
# Create histogram of total number of steps taken each day
#

hist(steps_day$steps, 
     main = "Histogram - Total Steps Per Day", 
     xlab = "Steps Per Day", 
     col = "steelblue", 
     ylim = c(0, 30)
     )

#
# Calculate the mean and median values for steps taken per day
#

mean(steps_day$steps)

median(steps_day$steps)



#
# Create a data frame with the average number of steps taken for each interval
#

avg_steps_int <- aggregate(steps ~ interval, act_clean, mean)

#
# Generate a time series plot with interval on the x-axis and average number of steps on y-axis
#

plot(avg_steps_int$interval, 
     avg_steps_int$steps,
     type = "l",
     col = "steelblue",
     main = "Average # of Steps (5 minute intervals)",
     xlab = "Time Intervals",
     ylab = "Average # of Steps"
     ) 

#
# Determine which 5 minute interval contains the maximum number of steps
#

avg_steps_int[which.max(avg_steps_int$steps), ]$interval

#
# Determine the total number of missing values in the original data frame
#

sum(is.na(act))


#
# Create a new data frame that will have the imputed values loaded to replace NA values
#

act_imp <- act

#
# Loop through the original data frame and replace NA values with the average values for the respective interval
#

for (i in 1:nrow(act_imp)) {
    if(is.na(act_imp$steps[i])) {
        imp_val <- avg_steps_int$steps[which(avg_steps_int$interval == act_imp$interval[i])]
        act_imp$steps[i] <- imp_val 
    }
}


#
# Aggregate total steps per day using new data frame with imputed values
#

steps_day_imp <- aggregate(steps ~ date, act_imp, sum)

#
# Create histogram of total number of steps taken each day using new data frame with imputed values
#

hist(steps_day_imp$steps, 
     main = "Histogram - Total Steps Per Day (from Imputed Data)", 
     xlab = "Steps Per Day (Imputed)", 
     col = "steelblue", 
     ylim = c(0, 30)
     )

#
# Calculate the mean and median values for total number of steps per day using new data frame with imputed values
#

mean(steps_day_imp$steps)

median(steps_day_imp$steps)


#
# Compare histograms of the clean data against the imputed data
#

par(mfrow = c(1,2))

hist(steps_day$steps, 
     main = "Total Steps Per Day - Clean", 
     xlab = "Steps Per Day", 
     col = "steelblue", 
     ylim = c(0, 30)
     )

hist(steps_day_imp$steps, 
     main = "Total Steps Per Day - Imputed", 
     xlab = "Steps Per Day (Imputed)", 
     col = "steelblue", 
     ylim = c(0, 30)
     )


#
# Compare means and medians between clean data and imputed data
#

mean(steps_day$steps)
mean(steps_day_imp$steps)

median(steps_day$steps)
median(steps_day_imp$steps)



#
# Create a new column for type of day using the date field with two values "Weekday" and "Weekend"
#

act_imp['day_type'] <- weekdays(as.Date(act_imp$date))
act_imp$day_type[act_imp$day_type %in% c('Saturday','Sunday') ] <- "Weekend"
act_imp$day_type[act_imp$day_type != "Weekend"] <- "Weekday"


#
# Convert the new field to factor for plotting purposes
#

act_imp$day_type <- as.factor(act_imp$day_type)

#
# Aggregate the average number of steps by interval for comparison plot
#

steps_day_int_imp <- aggregate(steps ~ interval + day_type, act_imp, mean)

#
# Create a panel plot to compare weekday and weekend average number of steps
#

library(ggplot2)

g <- ggplot(steps_day_int_imp, aes(interval, steps))

g + geom_line(stat = "identity", aes(color = day_type)) +
    facet_grid(day_type ~.) +
    labs(y = expression("Number of Steps")) +
    labs(x = "Interval") +
    labs(title = "Number of Steps - Weekday vs Weekend\n(from Imputed Data)")















































































