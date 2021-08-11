library(timeSeries)
library(xts)
library(forecast)
library(ggplot2)
#This is forecast 7.3 
data.stuff <- as.data.frame(c(
  + 0, 0, 1, 2, 12, 9, 23, 28, 16, 32, 28.36, 22, 50, 41, 44, 76, 50.81, 37.52, 14.28, 39.58, 28.01, 40.87, 36.86, 30.59, 54.14, 45.73,
  + 47.76, 74.85, 52.66, 40.78, 20.40))
colnames(data.stuff)[1] <- "Rawdata"
#In order to use a time series model we must transform data into a time series format
ts.data <- ts(data.stuff$Rawdata, start = c(2016,1), end = c(2018,7), frequency = 12)
plot(ts.data)
#Lets create data decomposition to analyze our data. 
stl.stuff <- stl(ts.data, s.window = "period")
plot(stl.stuff)
stl.stuff <- as.data.frame(stl.stuff$time.series)
HltWint.stuff <- as.data.frame(forecast::forecast(stats::HoltWinters(ts.stuff, beta = FALSE, gamma = TRUE, seasonal = c("additive")), h = (120) - length(ts.stuff), level = c(50)))
#Lets plot to review the output
plot(HltWint.stuff$`Point Forecast`, type='l')
#Lets visually see how our observed data matches up against Fitted data
plot(stats::HoltWinters(ts.stuff, beta = FALSE, gamma = TRUE, seasonal = c("additive")), h = (120) - length(ts.stuff), level = c(50))
#Now lets review our final stats summary
forecast::accuracy(forecast::forecast(stats::HoltWinters(ts.stuff, beta = FALSE, gamma = TRUE, seasonal = c("additive")), h = (120) - length(ts.stuff), level = c(50)))
