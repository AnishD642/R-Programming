
#Linear Regression

library(tidyverse)
library(caret)
theme_set(theme_bw())
#Load the data
data("marketing", package = "datarium")
#Inspect the data
sample_n(marketing, 3)

#Split the data into training and test set
#To make it reproduce-able - same sample test for every run  
set.seed(123)
#sales is the dependent variable 
training.samples <- marketing$sales %>%
  #80% training sample and 20% testing sample 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- marketing[training.samples, ]
test.data <- marketing[-training.samples, ]

#Build the model
model <- lm(sales ~., data = train.data) #lm() is used to compute linear regression model.
#Summarize the model
summary(model)
#The summary outputs shows 6 components, including:
#Call. Shows the function call used to compute the regression model.

#Coefficients. Shows the regression beta coefficients and their statistical 
#significance. Predictor variables, that are significantly associated to 
#the outcome variable, are marked by stars.
#For a given the predictor, the t-statistic evaluates whether or not 
#there is significant association between the predictor and the outcome 
#variable, that is whether the beta coefficient of the predictor is 
#significantly different from zero.
#It can be seen that, changing in youtube and facebook advertising budget 
#are significantly associated to changes in sales while changes in newspaper 
#budget is not significantly associated with sales.
#For example, for a fixed amount of youtube and newspaper advertising 
#budget, spending an additional 1000 dollars on facebook advertising leads 
#to an increase in sales by approximately 0.1885*1000 = 189 sale units, on 
#average.
#The youtube coefficient suggests that for every 1000 dollars increase 
#in youtube advertising budget, holding all other predictors constant, we 
#can expect an increase of 0.045*1000 = 45 sales units, on average.
#We found that newspaper is not significant in the multiple regression 
#model. This means that, for a fixed amount of youtube and newspaper 
#advertising budget, changes in the newspaper advertising budget will 
#not significantly affect sales units.
#intercept indicates the location where it intersects an axis.

#As the newspaper variable is not significant, it is possible to remove 
#it from the model:
model <- lm(sales ~ youtube + facebook, data = train.data)
summary(model)
#Finally, our model equation can be written as follow: sales = 3.43+ 0.045youtube + 0.187facebook.

#goodness-of-fit
#The overall quality of the linear regression fit can be assessed using 
#the following three quantities, displayed in the model summary:
#Residual Standard Error (RSE): The RSE (or model sigma), corresponding 
#to the prediction error, represents roughly the average difference between 
#the observed outcome values and the predicted values by the model. The lower 
#the RSE the best the model fits to our data.
#R-squared (R2) and adjusted R2: The R2 measures, how well the model fits 
#the data. The higher the R2, the better the model. However, a problem with 
#the R2, is that, it will always increase when more variables are added to 
#the model, even if those variables are only weakly associated with the 
#outcome (James et al. 2014). A solution is to adjust the R2 by taking into 
#account the number of predictor variables.
#F-statistic: A large F-statistic will corresponds to a statistically 
#significant p-value (p < 0.05). In our example, the F-statistic equal 
#644 producing a p-value of 1.46e-42, which is highly significant.

#plot LR
plot(marketing$youtube,marketing$sales,
     main='Regression for youtube and sales',
     xlab='youtube',ylab='sales')
abline(lm(sales ~ youtube,data=marketing),col='red')

plot(marketing$facebook,marketing$sales,
     main='Regression for facebook and sales',
     xlab='facebook',ylab='sales')
abline(lm(sales ~ facebook,data=marketing),col='red')

plot(marketing$newspaper,marketing$sales,
     main='Regression for newspaper and sales',
     xlab='newspaper',ylab='sales')
abline(lm(sales ~ newspaper,data=marketing),col='red')

#Make predictions
predictions <- model %>% predict(test.data)
#Model performance
#(a) Prediction error, RMSE
RMSE(predictions, test.data$sales)
#(b) R-square
R2(predictions, test.data$sales)
#From the output above, the R2 is 0.91, meaning that the observed and 
#the predicted outcome values are highly correlated, which is very good.
#The prediction error RMSE is 1.95, representing an error rate of 
#1.95/mean(test.data$sales) = 1.95/17 = 9.2%, which is good.
mean(test.data$sales)



###########################################################################################


#Logistic regression #Binary 

#The goal here is to model and predict if a given specimen 
#(row in dataset) is benign or malignant, based on 9 other cell features. 
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ]  #Create copy
view(bc)
str(bc)

#Remove id column
bc <- bc[,-1]
#Convert factors to numeric
for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}
#Whenever the Class is malignant, it will be 1 else it will be 0. 
#Then, I am converting it into a factor.
bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))
str(bc)
table(bc$Class)

#Prep Training and Test data.
set.seed(100)
trainDataIndex <- createDataPartition(bc$Class, p=0.7, list = F)  #70% training data
trainData <- bc[trainDataIndex, ]
testData <- bc[-trainDataIndex, ]

#Build Logistic Model
logitmod <- glm(Class~., family = "binomial", data=trainData)
summary(logitmod)
#glm stands for generalised linear models

pred <- predict(logitmod, newdata = testData, type = "response")
head(pred)
#if pred is greater than 0.5, it is malignant else it is benign.
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class
mean(y_pred == y_act) #accuracy


#######################################################################################


#Logistic regression #Multiclass

#Loading the library
library(rattle.data)
#Loading the wine data
data(wine)
#Checking the structure of wine dataset
str(wine)
#Confusion matrix 
with(wine, table(wine$Type, wine$Type))

#Prep Training and Test data.
library(dplyr)
#Using sample_frac to create 70 - 30 slipt into test and train
train <- sample_frac(wine, 0.7)
sample_id <- as.numeric(rownames(train)) # rownames() returns character so as.numeric
test <- wine[-sample_id,]
#Loading the nnet package to use multinom
require(nnet)
#Training the multinomial model
multinom.fit <- multinom(Type ~ Alcohol + Color -1, data = train)

#Checking the model
summary(multinom.fit)
#Predicting the values for train dataset
train$precticed <- predict(multinom.fit, newdata = train, "class")
table(train$precticed)
table(train$Type)

#Building classification table
ctable <- table(train$Type, train$precticed)
table(train$Type, train$precticed)
#Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)

#Predicting the values for test dataset
test$precticed <- predict(multinom.fit, newdata = test, "class")

#Building classification table
ctable <- table(test$Type, test$precticed)

#Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)
table(train$Type, train$precticed)
table(test$precticed)
table(test$Type)
table(test$Type, test$precticed)


####################################################################################


#Predict value using LR
youtube = 100
facebook = 100
newspaper = 10
datawine = data.frame(youtube,facebook,newspaper)
datawine
prediction <- predict(model, datawine)
prediction

