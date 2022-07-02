
#Matrices are another type of object that are common in R. 
#Matrices are similar to data frames in that they are two-dimensional: 
#they have rows and columns.
#Can store only one type of data 
#4 rows, 3 columns, store numbers from 1 to 12 
#Entry column-wise
mat <- matrix(1:12, 4, 3)
mat
mat[2, 3]
mat[2, ]
mat[, 3]
mat[, 2:3]
mat[1:2, 2:3]

#We can convert matrices into data frames using the function
as.data.frame(mat)
#In matrices data have to be all the same type. For this reason data 
#frames are much more useful for storing data, since we can have 
#characters, factors, and numbers in them.



#We can create vectors using the function c, which stands for concatenate
codes <- c(380, 124, 818)
codes
library(tidyverse)
#Viewing in tabular format 
view(codes)
#Can use either single quotes or double quotes 
country <- c("italy", "canada", "egypt")
view(country)
country1 <- c('italy', 'canada', 'egypt')
country1
#Can use mixed data 
misc <- c('italy', 'canada', 'egypt', 500)
view(misc)
#Sometimes it is useful to name the entries of a vector. For example, 
#when defining a vector of country codes, we can use the names to 
#connect the two:
codes <- c(italy = 380, canada = 124, egypt = 818)
codes
class(codes)
names(codes)
view(codes)

#Another useful function for creating vectors generates sequences:
seq(1, 10)
seq(1, 10, 2)
1:10
class(1:10)
class(seq(1, 10, 0.5))

#We use square brackets to access specific elements of a vector.
codes[2]
codes[c(1,3)]
codes[1:2]
codes[c("egypt","italy")]

#Coercion is an attempt by R to be flexible with data types
x <- c(1, "canada", 3)
x
class(x)



#Conversions
x <- 1:5
y <- as.character(x)
y
as.numeric(y)

#When a function tries to coerce one type to another and encounters 
#an impossible case, it usually gives us a warning and turns the entry 
#into a special value called an NA for "not available."
x <- c("1", "b", "3")
class(x)
as.numeric(x)
h <- c("1", "3")
class(h)
as.numeric(h)



#Say we want to rank the states from least to most gun murders. 
#The function sort sorts a vector in increasing order. We can therefore 
#see the largest number of gun murders by typing:
library(dslabs)
data(murders)
sort(murders$total) #Ascending 



#Use airquality
data(airquality)
view(airquality)
summary(airquality)
par(mfrow=c(1,1))
plot(airquality)
#Multiple box plots
boxplot(airquality[,0:3], main='Multiple Box plots')
#Upper edge of the boxplot represents 75th percentile 
#Lower edge represents 25th percentile 
#Thick line represents median of the data (50th percentile)
#If the thick line is exactly in between the upper edge and lower edge, 
#then there is symmetric distribution of data 
#If it is towards upper edge, then the data is right-skewed (Mean > Median)
#Most of the data is in upper-range 
#And vice-versa 
#The dots represent outliers 



#Loading the package 'lattice'
library(lattice)  
#Loading the dataset
#The attach function attaches the database to the R search path
#so the objects in the database can be accessed by 
#simply giving their names.
attach(mtcars)
view(mtcars)
head(mtcars)
#since we have attached the dataset mtcars, we do not need to 
#specify mtcars$gear or mtcars$cyl
gear
#Don't use the dataset directly
#Copy it in another variable and then use that variable for operations
mtcars_data <- mtcars
#Converting categorical data into factor 
gear_factor<-factor(mtcars_data$gear,levels=c(3,4,5),
                    labels=c("3gears","4gears","5gears")) 
cyl_factor <-factor(mtcars_data$cyl,levels=c(4,6,8),
                    labels=c("4cyl","6cyl","8cyl"))
#use of lattice plot
#Kernel density plots - Smoother version of histogram 
densityplot(cyl, main="Density Plot",  xlab="Miles per gallon")
densityplot(carb, main="Density Plot",  xlab="Density value")
#Scatterplot
splom(mtcars[c(1,3,4,5,6)], main="MTCARS Data")



#Loading the package 'ggplot2'
library(ggplot2)
#Loading the dataset
attach(mtcars)
mtcars_data <- mtcars
#Create factors with value labels 
mtcars_data$gear <- factor(mtcars_data$gear,levels=c(3,4,5),  
                      labels=c("3gears", "4gears", "5gears"))  
mtcars_data$am <- factor(mtcars_data$am,levels=c(0,1),  
                    labels=c("Automatic","Manual"))  
mtcars_data$cyl <- factor(mtcars_data$cyl,levels=c(4,6,8),  
                     labels=c("4cyl","6cyl","8cyl"))



#Scatterplots: geom_point() is used to create scatterplots.
#aes map variables in the data for visual properties of ggplot 
#geoms (points, bars, box plot, etc). These visual characteristics are 
#known as aesthetics (or aes) and include: color and fill. 
#points shape.
p <- ggplot(data = mtcars, aes(x = wt, mpg))
p + geom_point()
p + geom_point(aes(color = qsec))
p + geom_point(aes(alpha = qsec))
p + geom_point(aes(size = qsec))
p + geom_point(aes(color = cyl))
p + geom_point(aes(color = factor(cyl)))
p + geom_point(aes(shape = factor(cyl)))
p + geom_point(aes(shape = factor(cyl), color = factor(cyl)))
p + geom_point(aes(shape = factor(qsec), color = factor(cyl)))
p + geom_point(aes(shape = factor(carb), color = factor(cyl)))
p + geom_point(aes(shape = factor(carb), color = factor(cyl)), size =2.25)
p + geom_point(aes(shape = factor(carb), color = factor(cyl)), size =2.25) + theme_bw()
p + geom_point(aes(shape = factor(carb), color = factor(cyl)), size =2.25) + theme_bw() + theme(axis.title = element_text(size=rel(1.5)))

