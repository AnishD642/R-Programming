
#Load Libraries
library(nycflights13)
library(ggplot2)
library(tidyverse)
#Loading the dataset
data(flights)
#Viewing the dataset
View(flights)
#Storing the dataset in another variable 
data_flights <- flights 

################################################################################

#Q1

#MVA-1
#histogram
ggplot(data_flights) +
  geom_histogram(aes(x = air_time), fill = 'blue',
                 color = "lightblue", binwidth = 5)+
  ggtitle("Basic Histogram") +
  xlab("Air Time") +
  ylab("Frequency") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#MVA-2
#Stacked Bar Plot
data_flights %>%
  ggplot(aes(x = carrier, fill = origin)) + 
  geom_bar() +
  ggtitle("Stacked Bar Plot") +
  xlab("Carrier") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#MVA-3
#Stacked Bar Plot in same height
ggplot(data_flights) +
  geom_bar(aes(x = carrier, fill = origin), position = 'fill') +
  ggtitle("Stacked Bar Plot in Same Height") +
  xlab("Carrier") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10)) 

#MVA-4
#Tiles plot
ggplot(as.data.frame(table(data_flights$carrier,
                           data_flights$month))) +
  geom_tile(aes(x = Var1, y = Var2, fill = Freq)) +
  geom_text(aes(x = Var1, y = Var2, label = Freq),
            color = "yellow") +
  ggtitle("Tiles Plot") +
  xlab("Carrier") +
  ylab("Month") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#MVA-5
#Violin plot
data_flights <- data_flights %>% 
  mutate(speed = distance / air_time * 60)
ggplot(data_flights) +
  geom_violin(aes(x = origin , y = speed, fill = origin)) +
  ggtitle("Violin Plot") +
  xlab("Origin") +
  ylab("Speed") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#MVA-6
#Ridge Plot
library(ggridges)
ggplot(data_flights) +
  geom_density_ridges(aes(x = speed , y = carrier,
                          fill = origin), alpha = 0.7) +
  ggtitle("Ridge Plot") +
  xlab("Speed") +
  ylab("Carrier") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))


################################################################################


#Q2
#Adding Speed and Time gain feature 
data_flights <- data_flights %>% 
  mutate(time_gain = dep_delay - arr_delay ,
         speed = distance / air_time * 60)
#Extracting certain features of the dataset 
df <- data_flights %>% select(air_time,dep_time,arr_time,
                              sched_dep_time,sched_arr_time,dep_delay,
                              arr_delay,distance,speed,time_gain)
#View the new dataset
View(df)
df <- na.omit(df) # Omit NA
#R does not automatically re-number the rows when we drop those with NA values, 
#we can force re-numbering
rownames(df) <- NULL 
#Obtaining Principal components 
pca.fit <- prcomp(df, scale. = TRUE)

#Calculate total variance explained by each principal component
#sdev : standard deviation 
pca.fit$sdev^2 / sum(pca.fit$sdev^2)
#Storing the variance result 
var_explained = pca.fit$sdev^2 / sum(pca.fit$sdev^2)

#Create scree plot
#Ten columns used
qplot(c(1:10), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

#Plot using PC1 and PC2
biplot(pca.fit,choices=c(1,2))

pca.fit$rotation