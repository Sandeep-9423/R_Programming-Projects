dataset=read.csv("C:\\Users\\Dell\\Desktop\\bottle.csv")
head(dataset)
View(dataset)

dataset <- dataset[,c("Salnty","STheta","T_degC","T_prec")]

# Create the relationship model.
weather <- lm(Salnty~STheta+T_degC+T_prec, data = dataset)

# Show the model.
print(weather)

# Get the Intercept and coefficients as vector elements.
cat("# # # # The Coefficient Values # # # ","\n")

c<- coef(weather)[1]
print(c)

XSalnty <- coef(weatherl)[2]
XSTheta <- coef(weatherl)[3]
XT_degC <- coef(weather)[4]

print(XSalnty)
print(XSTheta)
print(XT_degC)
relation<- lm(XSalnty~XStheta+XT_degC)
c <- data.frame(XStheta=25,XT_degC=020)
results <-  predict(relation,c)
print(results)



dataset <- lm(Salnty+STheta+T_degC+T_prec, data = dataset)

# Show the model.
print(dataset)

# Get the Intercept and coefficients as vector elements.
cat("# # # # The Coefficient Values # # # ","\n")

a <- coef(model)[1]
print(a)
