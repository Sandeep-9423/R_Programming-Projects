###Input data

input=read.csv("C:\\Users\\Dell\\Desktop\\R\\New folder\\diabetes.csv")
head(input)
View(input)
input$Outcome <- as.factor(input$Outcome)

###Box plot outcomes according to age group with notch

print(head(input))
boxplot(Age ~ Outcome, data = input, xlab = "Outcome",
        ylab = "Age", main = "Diabetes outcome vs Age group")

###BOX plot with Notch 
boxplot(Age ~ Outcome, data = input,
        xlab = "Outcome",
        ylab = "Age",
        main = "Diabetes outcome vs Age group",
        notch = TRUE,
        varwidth = TRUE,
        col = c("Red","Green"))
dev.off()

###model prediction

input$Outcome <- as.factor(input$Outcome)
set.seed(123456)

A <- sample(2, size = nrow(input), replace = T, prob = c(0.8, 0.2))
train <- input[A==1,]
test <- input[A==1,]
head(train)
test
dim(train)
dim(test)
model <- glm(Outcome ~ BloodPressure + Glucose + BMI + Age , data=train , family = "binomial" )
model

p1 <- predict(model, test, type = 'response')
p1
pred1 <- ifelse(p1>0.60 , 1, 0)
pred1
head(p1)
head(test)
summary(p1)
input$confusion
