#Loading the data

data<-read.csv("activity.csv")
head(data)
data

#groupping data
library(dplyr)
?group_by
by_date<- data %>% group_by(date)

#Total number of steps taken per day
total_date<-by_date %>% summarise( steps = sum (steps))
total_date
#barplot
barplot(total_date$steps~total_date$date)
#Histogram of total number of steps taken per day
hist(total_date$steps, breaks = 30)
#Mediam and mean value of total number of steps taken per day
summary(total_date$steps)



#calcuating mean values of steps each day
by_date %>% summarise( steps = mean (steps))
by_interval<-noNA %>% group_by(interval)
#Plotting average value of steps, at specific time interval
dummy<-by_interval %>% summarise (steps = mean (steps))
plot(dummy, type = "l")
subset(dummy, dummy$steps==(max(dummy$steps)))
#Maximum value of steps can be found 


#Total numnber of missing values
sum(is.na(data$steps))
sum(is.na(data$date))
sum(is.na(data$interval))
#avg value for the specific interval will be used to fill NAs.
avg_interval<-by_interval %>% summarise( steps = mean (steps))
avg_interval
#Making a fuction whose NAs were replaced with average values
replace<-function(data){
        for (i in 1:length(rownames(data))){
          if(is.na(data[i,1])){
              data[i,1]<-subset(avg_interval$steps, avg_interval$interval==(data[i,3]))
          }
          
        }
        return(data)
}
new_data<-replace(data)
head(new_data)

#Grouping new data by date
new_group<-group_by(new_data, date)
new_data2<-new_group %>% summarise(steps = sum(steps))
#Making histogram with modified data
hist(new_data2$steps, breaks = 30)
summary(new_data2)



new_data$day<-weekdays(as.Date(new_data$date))
new_data$week<-NA
new_data
for(i in 1:length(row.names(new_data))){
  if (new_data[i,4] == "Sunday"){
    new_data[i,5]<-"weekend"
  }
  else if (new_data[i,4] == "Saturday"){
    new_data[i,5]<-"weekend"
  }
  else {
    new_data[i,5]<-"weekdays"
  }
}
weekday_data<-subset(new_data, new_data$week=="weekdays")
weekend_data<-subset(new_data, new_data$week=="weekend")
#weekday data
weekday_by_interval<- weekday_data %>% group_by(interval)
weekday_profile<-weekday_by_date %>% summarise( steps = mean (steps))
#weekend data
weekend_by_interval<- weekend_data %>% group_by(interval)
weekend_profile<-weekend_by_interval %>% summarise( steps = mean (steps))

par(mfrow = c(1,2))
plot(weekday_profile, type = "l")
title(main= "Pattern during weekdays")
plot(weekend_profile, type = "l")
title(main= "Pattern during weekends")


