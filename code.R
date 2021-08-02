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
hist(total_date$steps)
#Mediam and mean value of total number of steps taken per day
summary(total_date$steps)

#calcuating mean values of steps each day
by_date %>% summarise( steps = mean (steps))



#Total numnber of mission values
sum(is.na(data$steps))
sum(is.na(data$date))
sum(is.na(data$interval))

#Filling NAs with average value of that specific interval
#NA removed dataset
noNA<-subset(data, is.na(data$steps) == F)
by_interval<-noNA %>% group_by(interval)
#avg value for the specific interval
avg_interval<-by_interval %>% summarise( steps = mean (steps))
avg_interval

#replacing with average values
replace<-function(data){
        for (i in 1:length(rownames(data))){
          if(is.na(data[i,1])){
              data[i,1]<-subset(avg_interval$steps, avg_interval$interval==(data[i,3]))
          }
          
        }
        return(data)
}
str(new_data)


