
#Q1

library(tidyverse)
library(caret)
theme_set(theme_bw())
#Loading the dataset
harvest=read.csv("C:/Users/ANISHDESAI/Documents/Lab_DataViz/harvest.csv")
#View the dataset
view(harvest)

#Split the data into training and test set
#To make it reproduce-able - same sample test for every run  
set.seed(123)
#yield is the dependent variable 
training.samples <- harvest$yield %>%
  #80% training sample and 20% testing sample 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- harvest[training.samples, ]
test.data <- harvest[-training.samples, ]

#Build the model
model <- lm(yield ~., data = train.data) 
#Summarize the model
summary(model)

#plot LR
plot(harvest$density,harvest$yield,
     main='Regression for density and yield',
     xlab='density',ylab='yield')
abline(lm(yield ~ density,data=harvest),col='red')

plot(harvest$block,harvest$yield,
     main='Regression for block and yield',
     xlab='block',ylab='yield')
abline(lm(yield ~ block,data=harvest),col='red')

plot(harvest$fertilizer,harvest$yield,
     main='Regression for fertilizer and yield',
     xlab='fertilizer',ylab='yield')
abline(lm(yield ~ fertilizer,data=harvest),col='red')

#Predict value using LR
density = 2
block = 4
fertilizer = 3
data_harvest = data.frame(density,block,fertilizer)
data_harvest
prediction <- predict(model, data_harvest)
prediction

#Load Library
library(hydroGOF)
predictYlinregress <- predict(model,test.data)
RMSE(train.data$yield, predictYlinregress) 


################################################################################


#Q2

#Loading the library
library(rattle.data)
#Dataset 'harvest' already loaded
#Checking the structure of harvest dataset
str(harvest)

#Prep Training and Test data
library(dplyr)
#Using sample_frac to create 70 - 30 slipt into test and train
train <- sample_frac(harvest, 0.7)
sample_id <- as.numeric(rownames(train)) 
test <- harvest[-sample_id,]
require(nnet)
#Training the multinomial model
#'yield' is the dependent variable 
multinom.fit <- multinom(yield ~., data = train)

#Checking the model
summary(multinom.fit)

#Predicting the values for train dataset
train$precticed <- predict(multinom.fit, newdata = train, "class")
#Building classification table
ctable <- table(train$yield, train$precticed)
#Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)

#Predicting the values for test dataset
test$precticed <- predict(multinom.fit, newdata = test, "class")
#Building classification table
ctable <- table(test$yield, test$precticed)
#Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)

#Predict value using LogR
density = 2
block = 4
fertilizer = 3
data_harvest = data.frame(density,block,fertilizer)
data_harvest
prediction <- predict(multinom.fit, data_harvest)
prediction


################################################################################


#Q3

#Load Required Libraries
library(e1071)
library(hydroGOF)

#Plot
makePlot <-function(x,y){
  plot(x,y,col="black",pch=5,lwd=1)
  lines(x,y,lty=2, lwd=2)
  grid()}

#Predict value using SVM
density = 2
block = 4
fertilizer = 3
data_harvest = data.frame(density,block,fertilizer)

#SVM model 1
svm1 <- svm(yield ~ density , harvest)
#predicted values
predictYsvm1 <- predict(svm1, harvest)
#viz comparison
makePlot(harvest$density,harvest$yield)
title("Original data  + SVR Model")
points(harvest$density, predictYsvm1, col = "blue", pch=4)
points(harvest$density, predictYsvm1, col = "blue", type="l")
#Checking the model
summary(svm1)
#Predicting result for some value of x 
pred_svm1 <- predict(svm1,data_harvest$density)
pred_svm1
#comparing the result with LR
lrm1 <- lm(yield ~ density, data=harvest)
predictYlinregress1 <- predict(lrm1,harvest)
RMSE(harvest$yield, predictYlinregress1) 
RMSE(harvest$yield, predictYsvm1)   

#SVM model 2
svm2 <- svm(yield ~ block , harvest)
#predicted values
predictYsvm2 <- predict(svm2, harvest)
#viz comparison
makePlot(harvest$block,harvest$yield)
title("Original data  + SVR Model")
points(harvest$block, predictYsvm2, col = "blue", pch=4)
points(harvest$block, predictYsvm2, col = "blue", type="l")
#Checking the model
summary(svm2)
#Predicting result for some value of x 
pred_svm2 <- predict(svm2,data_harvest$block)
pred_svm2
#comparing the result with LR
lrm2 <- lm(yield ~ block, data=harvest)
predictYlinregress2 <- predict(lrm2,harvest)
RMSE(harvest$yield, predictYlinregress2) 
RMSE(harvest$yield, predictYsvm2)   

#SVM model 3
svm3 <- svm(yield ~ fertilizer , harvest)
#predicted values
predictYsvm3 <- predict(svm3, harvest)
#viz comparison
makePlot(harvest$fertilizer,harvest$yield)
title("Original data  + SVR Model")
points(harvest$fertilizer, predictYsvm3, col = "blue", pch=4)
points(harvest$fertilizer, predictYsvm3, col = "blue", type="l")
#Checking the model
summary(svm3)
#Predicting result for some value of x 
pred_svm3 <- predict(svm3,data_harvest$fertilizer)
pred_svm3
#comparing the result with LR
lrm3 <- lm(yield ~ fertilizer, data=harvest)
predictYlinregress3 <- predict(lrm3,harvest)
RMSE(harvest$yield, predictYlinregress3) 
RMSE(harvest$yield, predictYsvm3)   


################################################################################


#Q4

#Load the package
library(rpart)

#Create decision tree using regression
#For regression, method = 'anova' 
#Predict yield using density, block and fertilizer  
fit <- rpart(harvest$yield ~ harvest$density + 
               harvest$block + harvest$fertilizer, 
             method = "anova", data = harvest)
#Plot
plot(fit, uniform = TRUE,
     main = "Yield Decision 
                 Tree using Regression")
text(fit, use.n = TRUE, cex = .7)

#Print model
print(fit)

#Create test data
df_dtr  <- data.frame (density = 2,
                   block = 4,
                   fertilizer = 3)
#Predicting yield
#using testing data and model
predict(fit, df_dtr, method = "anova")

#Checking Performance 
pred_dtr <- predict(fit,harvest,method="anova")
#Building classification table
ctable <- table(harvest$yield, pred_dtr)
#Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)
