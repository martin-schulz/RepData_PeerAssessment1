library(ggplot2)

# Read data
activity <- read.csv("./data/activity.csv")

# Summarise data - 2,304 missing values in steps
summary(activity)
sum(is.na(activity$steps))

# Calculate daily step totals
dailysteps <- aggregate(activity$steps, list(activity$date), FUN=sum)
names(dailysteps) <- c("Date","Daily Steps")

# Histogram of daily step totals
hist(dailysteps$`Daily Steps`, main = "Distribution of Daily Step Totals",
     xlab = "Daily Steps")

# Mean and Median of daily step totals
summary(dailysteps$`Daily Steps`)

# Calculate mean steps for each 5-min. interval across all days, omitting NAs
fiveminsteps <- aggregate(activity$steps, list(activity$interval),
                          FUN=mean, na.action=na.pass, na.rm=TRUE)
names(fiveminsteps) <- c("Interval","Avg. Steps")

# Plot time series of mean steps for each 5-min. interval across all days
plot(fiveminsteps, type="l", main = "Average Steps per 5-minute Interval")
fiveminsteps[which(fiveminsteps$`Avg. Steps`==max(fiveminsteps$`Avg. Steps`)),]

# Creating copy of activity dataset
activity2 <- activity

# Imputing missing values in 'steps' with interval means
activity2$steps[is.na(activity2$steps)] <-
        ave(activity2$steps,activity2$interval,
            FUN=function(x)mean(x,na.rm = T))[is.na(activity2$steps)]

# Summarise new data - no missing values in steps
summary(activity2)

# Calculate new daily step totals
dailysteps2 <- aggregate(activity2$steps, list(activity2$date), FUN=sum)
names(dailysteps2) <- c("Date","Daily Steps (Imputed)")

# Histogram of new daily step totals
hist(dailysteps2$`Daily Steps`, main =
             "Distribution of Daily Step Totals (Imputed)",
     xlab = "Daily Steps (Imputed)")

# Mean and Median of new daily step totals
summary(dailysteps2$`Daily Steps (Imputed)`)

# Determining weekdays and weekend days
for (i in 1:nrow(activity2)) {
        activity2$Day[i] <- ifelse(weekdays(as.Date(activity2$date[i]))
                                   %in% c("Saturday","Sunday"),
                                   "Weekend","Weekday")
}
activity2$Day <- as.factor(activity2$Day)

# Calculate new mean steps for each 5-min. interval by day type for all days
fiveminsteps2 <- aggregate(activity2$steps, list(activity2$interval,
                                                activity2$Day),FUN=mean)
names(fiveminsteps2) <- c("Interval","Day","Avg. Steps (Imputed)")

# Creating multipanel plot of new mean steps for each 5-min. interval
# by day type
g <- ggplot(fiveminsteps2, aes(Interval,`Avg. Steps (Imputed)`))
g + geom_line() + facet_grid(. ~ Day) +
    labs(title = "Average Steps per 5-minute Interval",
         x = "Interval", y = "Avg. Steps (Imputed)")