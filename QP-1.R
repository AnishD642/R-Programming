
#Load Required Libraries 
library(dslabs)
library(ggplot2)
library(tidyverse)

################################################################################

#Q1
#Loading website-traffic.csv dataset
web=read.csv('C:/Users/ANISHDESAI/Documents/Lab_DataViz/website-traffic.csv')
#Viewing the dataset
View(web)
#Loading room-temperature.csv dataset
room_temp=read.csv('C:/Users/ANISHDESAI/Documents/Lab_DataViz/room-temperature.csv')
#Viewing the dataset
View(room_temp)

################################################################################

#Q2
#Finding the Avg Visit based on the Day of the week 
DayOfWeekAvgVisit <- web %>% group_by(DayOfWeek) %>% summarise(Avg_Visits=mean(Visits))
#Viewing the Avg Visit Summary
View(DayOfWeekAvgVisit)
#Plot
barplot(DayOfWeekAvgVisit$Avg_Visits~DayOfWeekAvgVisit$DayOfWeek, xlab = 'DayOfWeek', ylab = 'Avg_Visits', main = 'Day Of Week wise Average Visit', col= 'green',horiz = FALSE)

################################################################################

#Q3
#Copy dataset in a variable so as to not lose original data 
web_data <- web 
#Removing Dates in MonthDay column 
web_data$MonthDay <- gsub("[[:digit:]]+","",web_data$MonthDay)
#Plot 
web_data %>% ggplot(aes(x=DayOfWeek, y=Visits)) + 
             geom_point(aes(shape=MonthDay, color=MonthDay), size=2.5) +
             scale_shape_manual(values=c(0,1,2,3,4,5,6)) +
             ggtitle("Day Of Week wise Visit") + 
             xlab("Day Of Week") +
             ylab("Visits")

################################################################################

#Q4
#Extracting four columns whose boxplot is to be plotted 
room_temp_values <- room_temp %>% select(FrontLeft,FrontRight,BackLeft,BackRight)
#View the modified dataset 
view(room_temp_values)
#Box Plots
boxplot(room_temp_values, main="Multiple Boxplots of Room Temperature", xlab="Room Portion", ylab="Temperature (in K)")

################################################################################

#Q5
#gather() : Combining columns FrontLeft, FrontRight, BackLeft, BackRight 
#into one column RoomPosition and their corresponding temperatures 
room_temp_gather <- room_temp %>% gather("RoomPosition","Temperature",2:5)
#View the modified dataset 
view(room_temp_gather)
#Density Plots
ggplot(room_temp_gather) +
  geom_density(aes(x = Temperature,
                   fill = RoomPosition), alpha = 0.4) +
  facet_wrap(~RoomPosition)
