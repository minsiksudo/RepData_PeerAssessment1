#Loading the data

data<-read.csv("activity.csv")
head(data)

#Remove NAs
sum(is.na(data$steps))
sum(is.na(data$date))
sum(is.na(data$interval))
length(data$steps)
data<-subset(data, is.na(data$steps) == F)
data

#groupping data
library(dplyr)
?group_by
by_date<- data %>% group_by(date)
#calcuating mean values of steps each day
by_date %>% summarise( steps = mean (steps))

