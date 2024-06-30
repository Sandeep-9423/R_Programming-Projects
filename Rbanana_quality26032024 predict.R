mydata <- read.csv("C:\\Users\\Dell\\Desktop\\R\\New folder\\banana_quality.csv")
str(mydata)
mydata$Quality <- as.factor(mydata$Quality)
set.seed(1234)
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.8, 0.2))
train <- mydata[ind==1,]
test <- mydata[ind==2,]

mymodel <- glm(Quality ~ Size + Weight + Sweetness + Softness + HarvestTime + Ripeness + Acidity,data = mydata,family = 'binomial')
p1 <- predict(mymodel, train, type = 'response')
head(p1)
head(train)               
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$Quality)
tab1
pred1
View(mydata)
a<-data.frame(Size=-1.9249682,	Weight=0.46807805,	Sweetness=3.0778325,	Softness=-1.4721768,	HarvestTime=0.2947986,	Ripeness=2.4355695,	Acidity=0.27129033)
p2 <- predict(mymodel,a,type = 'response')
head(p2)
pred2 <- ifelse(p2>0.5, 1, 0)
pred2
b<-data.frame(Size=-1.3280383,	Weight=1.0011656,	Sweetness=0.15192918,	Softness=--3.259573,	HarvestTime=-2.7055087,	Ripeness=1.6863799,	Acidity=-0.76770055)
p3 <- predict(mymodel,b,type = 'response')
head(p3)
pred3 <- ifelse(p3>0.5, 1, 0)
pred3

