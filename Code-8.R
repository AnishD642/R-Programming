
#Support vector regression

#Generate random data
#x value ranges between 1 and 75 
x = 1:75
#y value based on some formula and input x 
y = cumsum((rnorm(length(x))))
#Plot
makePlot <-function(x,y){
  plot(x,y,col="black",pch=5,lwd=1)
  lines(x,y,lty=2, lwd=2)
  grid()}
makePlot(x,y)
title("original data")
#Make data frame named `Data`
Data<-data.frame(cbind(x,y))

library(e1071)
#SVM model 
svm_model <- svm(y ~ x , Data)
#predicted values for all X
predictYsvm <- predict(svm_model, Data)
#viz comparison
makePlot(x,y)
title("original data  + svr")
points(Data$x, predictYsvm, col = "blue", pch=4)
points(Data$x, predictYsvm, col = "blue", type="l")
#Checking the model
summary(svm_model)

#Load Library
library(hydroGOF)
RMSE(Data$y, predictYsvm)   
#comparing the result with LR
linregress_model <- lm(y ~ x, data=Data)
#make predictions for regression model for each x val
predictYlinregress <- predict(linregress_model,Data)
RMSE(Data$y, predictYlinregress) 


###################################################################################


#Decision tree regression

#Load the package
library(rpart)
attach(iris)
#Create decision tree using regression
#For regression, method = 'anova' 
#Predict sepal width using sepal length, petal length, petal width and species 
fit <- rpart(Sepal.Width ~ Sepal.Length + 
               Petal.Length + Petal.Width + Species, 
             method = "anova", data = iris)
#Plot
plot(fit, uniform = TRUE,
     main = "Sepal Width Decision 
                 Tree using Regression")
text(fit, use.n = TRUE, cex = .7)

#Print model
print(fit)
#Deviance is a measure of goodness of fit of a generalized linear model. 
#Or rather, it's a measure of badness of fit-higher numbers indicate 
#worse fit.
#yval is the predicted response at that node.

# Create test data
df  <- data.frame (Species = 'versicolor', 
                   Sepal.Length = 5.1,
                   Petal.Length = 4.5,
                   Petal.Width = 1.4)
# Predicting sepal width
# using testing data and model
# method anova is used for regression
predict(fit, df, method = "anova")

#AIC (Akaike information criterion) is most often used for model selection. By calculating and 
#comparing the AIC scores of several possible models, you can choose 
#the one that is the best fit for the data. So if two models explain 
#the same amount of variation, the one with fewer parameters will have 
#a lower AIC score and will be the better-fit model.



#####################################################################################



#ANOVA
#Loading Libraries
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(rstatix)

my_data <- PlantGrowth
view(my_data)
#Show the levels
levels(my_data$group)
summary(my_data)
str(my_data)

#Compute the analysis of variance
res.aov <- aov(weight ~ group, data = my_data)
# Summary of the analysis
summary(res.aov)
#As the p-value is less than the significance level 0.05, we can conclude 
#that there are significant differences between the groups highlighted 
#with �*" in the model summary.

# 1. Homogeneity of variances
plot(res.aov, 1)
#Points 17, 15, 4 are detected as outliers, which can severely affect 
#normality and homogeneity of variance. It can be useful to remove outliers 
#to meet the test assumptions.
#The Residuals vs. Fitted plot, shows the ﬁtted values plotted against the 
#model residuals. If the residuals follow any particular pattern, such as 
#a diagonal line, there may be other predictors not yet in the model that 
#could improve it. The ﬂat lowess line looks very good as the single 
#predictor variable or regressor sufficiently explaining the dependent 
#variable.

#It’s also possible to use the Levene’s test to check the homogeneity of variances:
PlantGrowth %>% levene_test(weight ~ group)
#From the output above, we can see that the p-value is > 0.05, which is not 
#significant. This means that, there is not significant difference between 
#variances across groups. Therefore, we can assume the homogeneity of 
#variances in the different treatment groups.

# 2. Normality
plot(res.aov, 2)
#The normal probability plot of residuals is used to check the assumption 
#that the residuals are normally distributed. It should approximately 
#follow a straight line.
#As all the points fall approximately along this reference line, we can 
#assume normality.

# 3. Scale-Location plot
plot(res.aov, 3)
#If The lowess line that ﬁts this is fairly ﬂat, it indicates that the spread 
#in the predictions is almost the same across the prediction line, 
#indicating the very less chances of failure of meeting the assumption

# 4. Residuals vs. Leverage plot 
plot(res.aov, 5)
#Since ctrl has one and trt1 has two outlier points stand, we can assume 
#that there are some outliers having undue influence on the ﬁt of the model.
PlantGrowth %>% 
  group_by(group) %>%
  identify_outliers(weight)
#Note that, in the situation where you have extreme outliers, this can be 
#due to: 1) data entry errors, measurement errors or unusual values.

#Report
PlantGrowth %>%
  group_by(group) %>%
  get_summary_stats(weight, type = "mean_sd")
#A one-way ANOVA was performed to evaluate if the plant growth was 
#different for the 3 different treatment groups: ctr (n = 10), 
#trt1 (n = 10) and trt2 (n = 10).
#Plant growth decreased in trt1 group (4.66 +/- 0.79) compared to ctr 
#group (5.03 +/- 0.58). It increased in trt2 group (5.53 +/- 0.44) compared 
#to trt1 and ctr group.



#####################################################################################



#Two way annova
data("jobsatisfaction", package = "datarium")
view(jobsatisfaction)
jobsatisfaction %>%
  group_by(gender, education_level) %>%
  get_summary_stats(score, type = "mean_sd")

bxp <- ggboxplot(
  jobsatisfaction, x = "gender", y = "score",
  color = "education_level", palette = "jco"
)
bxp

#Identify outliers in each cell design:
jobsatisfaction %>%
  group_by(gender, education_level) %>%
  identify_outliers(score)
#There were no extreme outliers.

#Compute two-way ANOVA test
res.aov2 <- aov(score ~ gender + education_level, data = jobsatisfaction)
summary(res.aov2)
#From the ANOVA results, you can conclude the following, based on the p-values and a significance level of 0.05:
#1. the p-value of gender is 0.234 (not significant: p>0.05).
#2. the p-value of education_level is < 2e-16 (significant), which indicates 
#that the levels of education_level are associated with significant 
#different score.

#Two-way ANOVA with interaction effect
#These two calls are equivalent
res.aov3 <- aov(score ~ gender * education_level, data = jobsatisfaction)
res.aov3 <- aov(score ~ gender + education_level + gender:education_level, data = jobsatisfaction)
summary(res.aov3)
#There was a statistically significant interaction between gender and 
#level of education for job satisfaction score, F = 7.34, p = 0.002.

#1. Homogeneity of variances
plot(res.aov3, 1)
jobsatisfaction %>% levene_test(score ~ gender*education_level)
#The Levene’s test is not significant (p > 0.05). Therefore, we can 
#assume the homogeneity of variances in the different groups.

#2. Normality
plot(res.aov3, 2)

#3. Scale-Location plot
plot(res.aov3, 3)

#Tukey multiple pairwise-comparisons
#As the ANOVA test is significant, we can compute Tukey HSD 
#(Tukey Honest Significant Differences, R function: TukeyHSD()) for 
#performing multiple pairwise-comparison between the means of groups.
TukeyHSD(res.aov3, which = "education_level")
#It can be seen from the output, that all pairwise comparisons are 
#significant with an adjusted p-value < 0.05.
TukeyHSD(res.aov3, which = "gender")



#Find out best fit model
res.aov1 <- aov(score ~ gender, data = jobsatisfaction)
res.aov2 <- aov(score ~ gender+education_level, data = jobsatisfaction)
library(AICcmodavg)
model.set <- list(res.aov1, res.aov2)
model.names <- c('res.aov1', 'res.aov2')
aictab(model.set, modnames = model.names)
#From these results, it appears that the two.way model is the best fit.
#The two-way model has the lowest AIC value



#####################################################################################



#MANOVA
#In the situation where there multiple response variables you can test 
#them simultaneously using a multivariate analysis of variance (MANOVA).
# Store the data in the variable my_data
my_data <- iris
sample_n(my_data, 10)
#We want to know if there is any significant difference, in sepal and 
#petal length, between the different species.
# MANOVA test
res.man <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
summary(res.man)

# Look to see which differ
summary.aov(res.man)
#From the output above, it can be seen that the two variables are highly 
#significantly different among Species.
