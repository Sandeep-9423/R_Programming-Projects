
##input_data sets

datasets=read.csv("C:\\Users\\Dell\\Desktop\\agesuciderates.csv")
head(datasets)
View(datasets)

######################################################
###PIE CHARTS

# Create data for the graph.(PIE CHART)
SuicideCount<- c(3624527,1665543,96330,748702,1363995,15592)
RegionName <- c("Europe", "Asia", "Oceania", "Central and South America",
                "North America and the Caribbean","Africa")

# Give the chart file a name.
png(file = "RegionName.png")

# Plot the chart.
pie(SuicideCount,RegionName)

# Save the file.
dev.off()

# Give the chart file a name.
png(file = "RegionName_colours.jpg")

# Plot the chart with title and rainbow color pallet.
pie(SuicideCount,RegionName, main = "RegionName pie chart", col = rainbow(length(x)))

# Save the file.
dev.off()

piepercent<- round(100*SuicideCount/sum(SuicideCount), 2)

# Give the chart file a name.
png(file = "RegionName_percentage_legends.jpg")

# Plot the chart.
pie(SuicideCount, labels = piepercent, main = "RegionName pie chart",col = rainbow(length(SuicideCount)))
legend("bottomleft",c("Europe", "Asia", "Oceania", "Central and South America",
                    "North America and the Caribbean","Africa"), cex = 0.70,
       fill = rainbow(length(x)))

# Save the file.
dev.off()

# Get the library.3d pie chart
library(plotrix)

SuicideCount<- c(3624527,1665543,96330,748702,1363995,15592)
RegionName <- c("Europe", "Asia", "Oceania", "Central and South America",
                "North America and the Caribbean","Africa")

# Give the chart file a name.
png(file = "3d_pie_chartRegionName.jpg")

# Plot the chart.
pie3D(SuicideCount,labels =RegionName ,explode = 0.2, main = "3dPie Chart of RegionName")

# Save the file.
dev.off()

#######################################################################
###Bar Chart


# Create data for the graph.(BAR CHART)

SuicideCount<- c(3624527,1665543,96330,748702,1363995,15592)
RegionName <- c("Europe", "Asia", "Oceania", "Central and South America",
                "North America and the Caribbean","Africa")

# Create the data for the chart
SuicideCount<- c(3624527,1665543,96330,748702,1363995,15592)

# Give the chart file a name
png(file = "SuicideCountbarchart.png")

# Plot the bar chart 
barplot(SuicideCount)

# Save the file
dev.off()


# Give the chart file a name
png(file = "barchart_SuicideCount.png")

# Plot the bar chart 
barplot(SuicideCount,names.arg=,xlab="RegionName",ylab="SuicideCoun",col="purple",
        main="SuicideCounchart",border="yellow")

# Add the legend to the chart
legend("topleft", RegionName, cex = 1.3, fill = colors)

# Save the file
dev.off()


##########################################################################

###Multiple Regressions

data=read.csv("C:\\Users\\Dell\\Desktop\\agesuciderates.csv")
head(data)
View(data)
print(data)

input<-data[,c("SuicideCount","CauseDeathPercentage","StdDeathRate",
               "DeathRatePer100K","Population" )]
print(head(input))

model<-lm(SuicideCount~CauseDeathPercentage*StdDeathRate*DeathRatePer100K*
            Population,data=input)
print(model)
relation<-lm(model)


a<-data.frame(CauseDeathPercentage=0.845255,StdDeathRate=3.525944,
              DeathRatePer100K=0.924854,Population=3599523)
result<-predict(relation,a)
print(result)

a<-data.frame(CauseDeathPercentage=0.65554,StdDeathRate=1.89498,
              DeathRatePer100K=0.751658,Population=18785)
result<-predict(relation,a)
print(result)


relation<-glm(model)
a<-data.frame(CauseDeathPercentage=0.65554,StdDeathRate=1.89498,
              DeathRatePer100K=0.751658,Population=18785)
results<-predict(relation,a)
print(results)


################################################################
###   TRAIN AND TEST 

datasets=read.csv("C:\\Users\\Dell\\Desktop\\agesuciderates.csv")
head(datasets)
View(datasets)

str(datasets)
datasets$RegionCode <- as.factor(datasets$RegionCode)
datasets$RegionName  <- as.factor(datasets$RegionName)
datasets$CountryCode <- as.factor(datasets$CountryCode)
datasets$CountryName <- as.factor(datasets$CountryName)
datasets$Sex <- as.factor(datasets$Sex)

xtabs(~SuicideCount + CauseDeathPercentage, data = datasets)

###to see null  values in data for data cleaning use this to see resuts
sum(is.na(datasets))  

# Partition data - train (80%) & test (20%)
set.seed(12345)
ind <- sample(2, nrow(datasets), replace = T, prob = c(0.8, 0.2))
train <- datasets[ind==1,]
test <- datasets[ind==2,]

# Logistic regression model

model<-glm(~SuicideCount+CauseDeathPercentage,
           data=train,family ='binomial')
print(model)
summary(model)


# Prediction
p1 <- predict(model, train, type = 'response')
head(p1)
head(train)

# Misclassification error - train data
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$SuicideCount)
tab1
1 - sum(diag(tab1))/sum(tab1)

# Misclassification error - test data
p2 <- predict(model, test, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$SuicideCount)
tab2

pred1
p1
View(datasets)
m1 <- data.frame(CauseDeathPercentage=0.64985,StdDeathRate=2.841844,
                 DeathRatePer100K=0.654854)
p2 <- predict(model,m1,type = 'response')
p2
m3 <- ifelse(p2>0.5, 1, 0)
m3


