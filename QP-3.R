
#Q1
#Linear Discriminant Analysis LDA 
#Load Required Libraries
library(MASS)
library(ggplot2)
library(tidyverse)
#Attach diamonds dataset to make it easy to work with
attach(diamonds)
view (diamonds)
#View structure of dataset
str(diamonds)
#Create a copy of the dataset 
diamonds_data <- diamonds
#Scale the values of numeric columns which are 
#to be used as predictor variables 
diamonds_data[c(5,8,9,10)] <- scale(diamonds_data[c(5,8,9,10)])
#Find mean of each predictor variable
apply(diamonds_data[c(5,8,9,10)], 2, mean)
#Find standard deviation of each predictor variable
apply(diamonds_data[c(5,8,9,10)], 2, sd) 
#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(diamonds_data), replace=TRUE, prob=c(0.7,0.3))
train <- diamonds_data[sample, ]
test <- diamonds_data[!sample, ] 

#Q1.1 
#Training the model 
#Fit LDA model
model <- lda(cut~., data=train)
#View model output
model
predicted <- predict(model, test)
#View predicted class for first six observations in test set
head(predicted$class)
#View posterior probabilities for first six observations in test set
head(predicted$posterior)
#View linear discriminant for first six observations in test set
head(predicted$x)
#predicted$class is factor data type which makes it incompatible 
#Hence convert it to ord. factor 
predicted$class<-as.ordered(predicted$class)
#Find accuracy of model
mean(predicted$class==test$cut)
#Define and Gather data to plot 
lda_plot <- cbind(train, predict(model)$x)
#create plot
ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = cut))

#Q1.2
model <- lda(color~., data=train)
model
predicted <- predict(model, test)
head(predicted$class)
head(predicted$posterior)
head(predicted$x)
predicted$class<-as.ordered(predicted$class)
mean(predicted$class==test$color)
lda_plot <- cbind(train, predict(model)$x)
ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = color))

#Q1.3
model <- lda(clarity~., data=train)
model
predicted <- predict(model, test)
head(predicted$class)
head(predicted$posterior)
head(predicted$x)
predicted$class<-as.ordered(predicted$class)
mean(predicted$class==test$clarity)
lda_plot <- cbind(train, predict(model)$x)
ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = clarity))

#Q1.4
model <- lda(carat~., data=train)
model
predicted <- predict(model, test)
head(predicted$class)
head(predicted$posterior)
head(predicted$x)
predicted$class<-as.ordered(predicted$class)
mean(predicted$class==test$carat)
lda_plot <- cbind(train, predict(model)$x)
ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = carat))

#Q1.5
model <- lda(price~., data=train)
model
predicted <- predict(model, test)
head(predicted$class)
head(predicted$posterior)
head(predicted$x)
predicted$class<-as.ordered(predicted$class)
mean(predicted$class==test$price)
lda_plot <- cbind(train, predict(model)$x)
ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = price))



################################################################################



#Q2
#Correlation Analysis 
#Load Required Libraries 
library(ggplot2)
library(tidyverse)
library("ggpubr")
diamonds_data_2 <- diamonds[c(1,5,6,7,8,9,10)]
View(diamonds_data_2)
#CORRELATION MATRIX
#correlations coefficients between the possible pairs of variables
D<-cor(diamonds_data_2)
round(D,2)

#Correlogram : Visualizing the correlation matrix
library(corrplot)

#Q2.1
corrplot(D, method="circle")

#Q2.2
corrplot(D, method="pie")

#Q2.3
corrplot(D, method="color")

#Q2.4
corrplot(D, method="number")

#Q2.5
#Display a chart of a correlation matrix
library("PerformanceAnalytics")
diamonds_data_3 <- diamonds[, c(1,5,6,7,8,9,10)]
chart.Correlation(diamonds_data_3, histogram=TRUE, pch=19)
#Values in the blocks represent the correlation between variables. 
#Each significance level is associated to a symbol : 
#p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols("***", "**", 
#"*", ".", " ")
