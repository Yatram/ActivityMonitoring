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




