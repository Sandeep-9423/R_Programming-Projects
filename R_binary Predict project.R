# Logistic Regression

# Read data file
mydata <- read.csv("C:\\Users\\Dell\\Desktop\\R\\New folder\\binary.csv")
str(mydata)
mydata$admit <- as.factor(mydata$admit)
mydata$rank <- as.factor(mydata$rank)

# Two-way table of factor variables
xtabs(~admit + rank, data = mydata)

# Partition data - train (80%) & test (20%)
set.seed(1234)
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.8, 0.2))
train <- mydata[ind==1,]
test <- mydata[ind==2,]

# Logistic regression model
mymodel <- glm(admit ~ gpa + rank, data = train, family = 'binomial')
summary(mymodel)

# Prediction
p1 <- predict(mymodel, train, type = 'response')
head(p1)
head(train)

# Misclassification error - train data
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$admit)
tab1
1 - sum(diag(tab1))/sum(tab1)

# Misclassification error - test data
p2 <- predict(mymodel, test, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$admit)
tab2

pred1
p1
View(mydata)
m1 <- data.frame(gpa=3.61,rank="3")
p2 <- predict(mymodel,m1,type = 'response')
p2
m3 <- ifelse(p2>0.5, 1, 0)
m3
