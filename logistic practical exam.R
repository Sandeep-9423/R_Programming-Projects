###imported data from folder
diabeticdata=read.csv("C:\\Users\\Dell\\Desktop\\R\\New folder\\diabetes.csv")
head(diabeticdata)
View(diabeticdata)

###Box plot outcomes according to age group
input<-diabeticdata[,c('Age','Outcome')]
print(head(input))
boxplot(Age ~ Outcome, data = diabeticdata, xlab = "Outcome",
        ylab = "Age", main = "Diabetes outcome vs Age group")
dev.off()

###model prepartion

str(diabeticdata)
diabeticdata$Age <- as.factor(diabeticdata$Age)
diabeticdata$Glucose <-as.factor(diabeticdata$Glucose)
diabeticdata$BloodPressure <-as.factor(diabeticdata$BloodPressure)

xtabs(~Outcome+ Glucose+BloodPressure, data = diabeticdata)

set.seed(1234567)
ind <- sample(2, nrow(diabeticdata), replace = T, prob = c(0.8, 0.2))
train <- diabeticdata[ind==1,]
test <- diabeticdata[ind==2,]
diabeticmodel <- glm(Outcome ~  Glucose+BloodPressure, data = train, family = 'binomial')
summary(diabeticmodel)

p1 <- predict(diabeticmodel, train, type = 'response')
head(p1)
head(train)

pred1 <- ifelse(p1>75, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$Outcome)
tab1
1 - sum(diag(tab1))/sum(tab1)

p2 <- predict(diabeticmodel, test, type = 'response')
pred2 <- ifelse(p2>75, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$Outcome)
tab2

pred1
p1
View(diabeticdata)
m1 <- data.frame( Glucose =158,BloodPressure=92 )
p2 <- predict(diabeticmodel,m1,type = 'response')
p2
m3 <- ifelse(p2>75, 1, 0)
m3

