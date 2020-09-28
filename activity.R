library(dplyr)
library(ggplot2)

activity <- read.csv("activity.csv")

head(activity)

str(activity)

summary(activity)

# I group for day the total steps in a new dataframe and i graphy

activity_day <- activity%>%
  group_by(date)%>%
  summarise(steps = sum(steps, na.rm = T))


with(activity_day, ggplot(mapping = aes(steps))+
       geom_histogram(col='black', fill='green', alpha = 0.4))


# generated calculous of the mean and median to each day
# act_mm_d (activity_mean and median for day)

act_mm_d <- activity %>%
  group_by(date)%>%
  summarise(mean.steps = mean(sum(steps, na.rm = T)))

act_mmed_d <- activity %>%
  group_by(date)%>%
  summarise(median.steps = median(sum(steps, na.rm = T)))


# I rectified the median and mean 

act_mm_d <- mean(tapply(activity$steps, activity$date, sum), na.rm = T)

act_mmed_d <- median(tapply(activity$steps, activity$date, sum), na.rm = T)


# generated graph with average number of steps

plot(act_mm_d, type = "l", main = "time series of the average number of steps to ech day",
    ylab = "The average number steps")

# I obtained the average of intevals to 5 minuts and added  avgstep colum in the dataframe activity.avgstep
# calculated the max number of steps and  saved in the variable maxstep

activity.avgstep <- tapply(activity$steps, activity$interval, mean, na.rm = T)

plot(activity.avgstep, type = "l", main = "The average steps for intervals of 5 minuts",
     xlab = "interval of 5 minutes")

maxstep <- which.max(activity.avgstep)

maxstep


#immputing missing data

summary(activity)

# I do used library Hmisc for switching  the values NA of the colum STEP

library(Hmisc)

#I change the NA values by the average of the total data

activity.copy <- activity

activity.copy$steps <- impute(activity.copy$steps, mean)

summary(activity.copy)

# I do a histogram of the total steps, now with the missing values inputed

activity.copy.total.steps <- activity.copy%>%
  group_by(date)%>%
  summarise(total.steps = sum(steps))

with(activity.copy.total.steps,ggplot(mapping = aes(total.steps))+
  geom_histogram(col='black', fill='green', alpha = 0.4))

# I compared the average number steps taken per 5-minute interval across weekdays and weekends


library(lubridate)

activity.copy$date <- as.POSIXct(activity.copy$date)

activity.copy <- mutate(activity,week = wday(activity$date))

activity.weekdays<-activity.copy%>%
  filter(week == 2 | week == 3 | week == 4 | week == 5 | week == 6)

activity.weekends<-activity.copy%>%
  filter(week == 7 | week == 1)

act.weekday.mean <- tapply(activity.weekdays$steps, activity.weekdays$interval, mean, na.rm = T)

act.weekend.mean <- tapply(activity.weekends$steps, activity.weekends$interval, mean, na.rm = T)

par(mfrow = c(2,1), mar = c(4,4,2,1))

plot(act.weekday.mean, type = "l", xlab = "inteval", ylab = "average steps by inteval each 5-minute",
     main = "activity of each weekday")

plot(act.weekend.mean, type = "l", xlab = "inteval", ylab = "average steps by inteval each 5-minute",
     main = "activity of each weekend")
