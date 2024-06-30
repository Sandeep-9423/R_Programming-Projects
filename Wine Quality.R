wine <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"), header = TRUE, sep = ";") # This command is used to load the dataset
head(wine)
dim(wine)
barplot(table(wine$quality))
wine$taste <- ifelse(wine$quality < 5, "bad", "good")

wine$taste[wine$quality == 5] <- "normal"

wine$taste[wine$quality == 6] <- "normal"

wine$taste <- as.factor(wine$taste)

str(wine$taste)

barplot(table(wine$taste)) # Barplot to view the taste of wines. The output is shown below.

table(wine$taste)
set.seed(123)

samp <- sample(nrow(wine), 0.8 * nrow(wine))

train <- wine[samp, ]

test <- wine[-samp, ]
library(ggplot2)
ggplot(wine,aes(fixed.acidity,volatile.acidity))+ geom_point(aes(color=taste))
ggplot(wine,aes(alcohol)) + geom_histogram(aes(fill=taste),color='black',bins=50) 
dim(train)
dim(test)
library(randomForest)
model <- randomForest(taste ~ . - quality, data = train, ntree = 1000, mtry = 5)
model

model$confusion
prediction <- predict(model, newdata = test)

table(prediction, test$taste)

prediction
