# Uploading "CPS1988" dataset placed in AER library
install.packages("AER")
library(AER)
data("CPS1988")
dataset <- CPS1988
View(dataset)

#Defining new dummy variable "region2" for categorical variable "region". Value "1" stands for "south", "0" for other
dataset$region2 =ifelse(dataset$region=="south",1,0)
View(dataset)

#Linear Model explaining earnings relative to years of education, years of experience and variable region2
model1 <- lm(wage~education+experience+region2,data=dataset)
summary(model1)

#Linear Model explaining earnings relative to years of education, years of experience and variable region2 for 12 and more education years
model2 <- lm(wage~education+experience+region2,data=dataset, education>12)
summary(model2)

#Comparing evaluations and descriptive statistics of model1 and model2
summary(model1)
summary(model2)

install.packages(stargazer)
library(stargazer)
stargazer(model1,model2,type="text")

#Predicted values for wage variable
predictedValues <- model1$fitted.values
plot(predictedValues,dataset$wage)
  
#Bar chart of education variable, x axis limited from 0 to 20
H1=hist(dataset$education,xlim=c(0,20)) 

