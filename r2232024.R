# Get the data points in form of a R vector.
rainfall <- c(799,1174.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)

# Convert it to a time series object.
rainfall.timeseries <- ts(rainfall,start = c(2012,1),frequency = 12)

# Print the timeseries data.
print(rainfall.timeseries)

# Give the chart file a name.
png(file = "rainfall.png")

# Plot a graph of the time series.
plot(rainfall.timeseries)

# Save the file.
dev.off()



# Get the data points in form of a R vector.
rainfall1 <- c(799,1174.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)
rainfall2 <- 
  c(655,1306.9,1323.4,1172.2,562.2,824,822.4,1265.5,799.6,1105.6,1106.7,1337.8)

# Convert them to a matrix.
combined.rainfall <-  matrix(c(rainfall1,rainfall2),nrow = 12)

# Convert it to a time series object.
rainfall.timeseries <- ts(combined.rainfall,start = c(2012,1),frequency = 12)

# Print the timeseries data.
print(rainfall.timeseries)

# Give the chart file a name.
png(file = "rainfall_combined.png")

# Plot a graph of the time series.
plot(rainfall.timeseries, main = "Multiple Time Series")

# Save the file.
dev.off()


EDraids2020<-c(125.0,1895.8,532.2,481.8,45.9,182.0,215.9,889,918,756,846.02,5189.89)
EDraids2022<-c(985,1025.85,582.78,1514.99,885.52,668,2524.0,18584.8,2186.7,218.5,252.5,8489)

combined.EDraids2020 <-  matrix(c(EDraids2020,EDraids2022),nrow = 12)

# Convert it to a time series object.
EDraids2020.timeseries <- ts(combined.EDraids2020,start = c(2020,1),frequency = 12)

# Print the timeseries data.
print(EDraids2020.timeseries)

# Give the chart file a name.
png(file = "ed1.png")

# Plot a graph of the time series.
plot(EDraids2020.timeseries, main = "Multiple Time Series")

# Save the file.
dev.off()

#####################################################3

# Load the dataset
data("AirPassengers")
# Print the first few rows
print(head(AirPassengers))
# Check the structure
str(AirPassengers)
# Plot the dataset
plot(AirPassengers)
# Decompose the time series data
decomposed <- decompose(AirPassengers)

# Plot the decomposed data
plot(decomposed)
# Install the 'forecast' package
install.packages('forecast')
# Load the 'forecast' package
library(forecast)
# Forecast future data
forecast_data <- forecast(AirPassengers, h = 24)
#Plot the forecasted data
plot(forecast_data)

start(AirPassengers)

end(AirPassengers)

cycle(AirPassenger)

AirPassengers.timeseries <- ts(AirPassengers,start = c(1960,1),frequency = 12)
print(AirPassengers.timeseries)
png(file = "AirPassengers.png")
plot(AirPassengers.timeseries)
dev.off()

library(datasets)
View(AirPassengers)

timeseries <- ts(AirPassengers,start = c(2012,1),frequency = 12)
print(timeseries)
png(file = "timeseries.png")
plot(timeseries, main = "Time Series")
dev.off()

timeseries1 <- ts(AirPassengers,start = c(2012,1),frequency = 4)
print(timeseries1)
png(file = "timeseries1.png")
plot(timeseries1, main = "Time Series")
dev.off()

timeseries2 <- ts(AirPassengers,start = c(2012,1),frequency = 6)
print(timeseries2)
png(file = "timeseries2.png")
plot(timeseries2, main = "Time Series")
dev.off()

timeseries3 <- ts(AirPassengers,start = c(2012,1),frequency = 24*6)
print(timeseries3)
png(file = "timeseries3.png")
plot(timeseries3, main = "Time Series")
dev.off()

start(AirPassengers)
end(AirPassengers)
cycle(AirPassengers)


################################################

####logistics regression

# Select some columns form mtcars.
input <- mtcars[,c("am","cyl","hp","wt")]

print(head(input))
input <- mtcars[,c("am","cyl","hp","wt")]

am.data = glm(formula = am ~ cyl + hp + wt, data = input, family = binomial)

print(summary(am.data))

relation<-glm(input)
a<-data.frame(cyl=0.2582,hp=184,wt=4.856)
results<-predict(relation,a)
print(results)


      