
#Q1
#Time Series Analysis 
library(forecast)
#Using 2016 and 2017 Monthly Rainfall Dataset
rainfall <- c(224.0,234.0,245.0,85.0,152.0,31.0,10.0,8.0,0.0,6.0,11.0,51.0,207.0,301.0,250.0,92.0,152.0,68.0,48.0,2.0,8.0,118.0,73.0,87.0)
rainfall.ts <- ts(rainfall,start = c(2016,1),frequency = 12)
rainfall.ts
autoplot(rainfall.ts,xlab = "Year", ylab = "Rainfall (in cms)")
#Decompose it
rainfall.comp <- decompose(rainfall.ts)
#Access components
rainfall.comp$trend
rainfall.comp$seasonal
rainfall.comp$random
autoplot(rainfall.comp)

#Forecast Method
#a. The Mean Method
#Use meanf() to forecast monthly rainfall in 2018
rainfall.fc <- meanf(rainfall.ts, h=12)
#Plot and summarize the forecasts
autoplot(rainfall.fc,xlab = "Month", ylab = "Rainfall (in cms)")
summary(rainfall.fc)

#b. The Naive Method
#Use naive() to forecast monthly rainfall in 2018
rainfall.fc <- naive(rainfall.ts, h=12)
autoplot(rainfall.fc)
summary(rainfall.fc)

#c. The Simple Moving Average Method
library(smooth)
rainfall.comp <- decompose(rainfall.ts)
autoplot(rainfall.comp)
summary(rainfall.comp)

#Use sma() to forecast monthly rainfall in 2018
rainfall.fc <- sma(rainfall.ts, order=12, h=12,silent=FALSE)
#Print model summary
summary(rainfall.fc)
#Print the forecasts
fc <- forecast(rainfall.fc)
print(fc)

# Examining the Residuals
rainfall.fc <- meanf(rainfall.ts, h=12)
checkresiduals(rainfall.fc)


################################################################################


#Q2
#Stream Data Visualization 
library(tidyverse)
library(stream)
set.seed(1000)
#DSD_Gaussians 
stream <- DSD_Gaussians(k = 3, d = 2)
plot(stream)

#DSC_DStream
dstream <- DSC_DStream(gridsize = .1, Cm = 1.2)
update(dstream, stream, n = 500)
dstream
plot(dstream)

#K-Means Clustering 
km <- DSC_Kmeans(k = 3)
recluster(km, dstream)
plot(km, stream, type = "both")

#DSD_BarsAndGaussians
stream2 <- DSD_BarsAndGaussians(angle=45, noise=0.1)
plot(stream2)

#DSD_mlbenchData
stream3 <- DSD_mlbenchData("Shuttle")
stream3
plot(stream3, n=100)

#DSD_mlbenchGenerator
stream4 <- DSD_mlbenchGenerator(method="cassini")
stream4
plot(stream4, n=500)
library("mlbench")
set.seed(1234)
Cassini <- mlbench.cassini(1000)
view(Cassini)

#DSD_Target
stream5 <- DSD_Target()
plot(stream5)

#DSD_UniformNoise
stream6 <- DSD_UniformNoise(d=2)
plot(stream6, n=100)
stream7 <- DSD_UniformNoise(d=3, range=rbind(c(0,1), c(0,10), c(0,5)))
plot(stream7, n=100)

set.seed(1000)
stream8 <- DSD_Gaussians(k = 3, d = 3, noise = .05, p = c(.5, .3, .1))
stream8
p <- get_points(stream8, n = 5)
p
p <- get_points(stream8, n = 100, class = TRUE)
head(p, n = 10)
plot(stream8, n = 500)
plot(stream8, n = 500, method = "pc")

#DSD_Benchmark
set.seed(1000)
stream <- DSD_Benchmark(1)
stream
plot(stream)


#Animation
library('animation')
reset_stream(stream)
animate_data(stream, n = 10000, horizon = 100, xlim = c(0, 1), ylim = c(0, 1))

animation::ani.options(interval = .1)
ani.replay()

saveHTML(ani.replay())
saveGIF(ani.replay())

#Outlier generating data streams
set.seed(1000)
stream <- DSD_Gaussians(k = 3, d = 2, outliers = 4, outlier_options = list(outlier_horizon = 10000), separation = 0.3, space_limit = c(0,1))
reset_stream(stream)
p <- get_points(stream, n = 10000, outlier = TRUE)
head(p)
out_marks <- attr(p, "outlier")
sum(out_marks)
which(out_marks)

#Advanced statistical data streams
set.seed(1000)
stream1 <- DSD_Gaussians(k = 3, d = 2, variance_limit = 0.2, space_limit = c(0, 5))
plot(stream1)
set.seed(1000)
stream2 <- DSD_Gaussians(k = 3, d = 2, variance_limit = 2, space_limit = c(0, 5))
plot(stream2)


set.seed(1000)
stream1 <- DSD_Gaussians(k = 5, d = 2, variance_limit = 0.2,
                         space_limit = c(0, 7),
                         separation_type = "Mahalanobis",
                         separation = 4)
plot(stream1)


set.seed(1000)
stream2 <- DSD_Gaussians(k = 5, d = 2, variance_limit = 0.2,
                         space_limit = c(0, 15),
                         separation_type = "Mahalanobis",
                         separation = 10)
plot(stream2)
