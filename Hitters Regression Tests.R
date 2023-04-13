library(tidyverse)
dataset=read.csv("Hitters.csv")

# Exploring the dataset

head(dataset)
length(dataset)
glimpse(dataset)
summary(dataset)
names(dataset)
#Missing values
colSums(is.na(dataset))

ggplot(data=dataset, aes(Salary))+geom_histogram()
Salary_median=median(dataset$Salary, na.rm=TRUE)
dataset$Salary=ifelse(is.na(dataset$Salary), Salary_median, dataset$Salary)
colSums(is.na(dataset))

#splitting the data       Experiment 1

library('caTools')
names(dataset)
split=sample.split(dataset$Salary,SplitRatio = .8)
train=subset(dataset,split==TRUE)
test=subset(dataset,split==FALSE)

names(dataset)
mlr=lm(formula=Salary~.,data=train)
summary(mlr)
#R-squared = 0.4859 Meaning this is not a great model
#P-value < 2.2e-16

#Mean Squared Error
summ=summary(mlr)
mse=(mean(summ$residuals^2))
paste('Mean Squared Error:',mse)
#Mean Squared Error = 92640.5710

#Test Set
#testing set prediction
y_pred=predict(mlr,newdata = test)
data=data.frame(test$Salary,y_pred)
head(data)

#This model is semi accurate on some however on others it is very far off.
#For Example test.Salary=425, prediction = 1246.79

#Validation

new=read.csv('Hitters_Validation_CSV.csv')
head(new)
new_x=new[c(1:20)]
new_x
data.frame(new[c(19)], predict(mlr, newdata=new_x))
#This model has much stronger predictions, however they are not super close. 

#             Experiment 2
#Splitting the Data
names(dataset)
split=sample.split(dataset$Salary,SplitRatio = .9)
train=subset(dataset,split==TRUE)
test=subset(dataset,split==FALSE)

#Multiple Linear Regression 
names(dataset)
mlr=lm(formula=Salary~.,data=train)
summary(mlr)
# R-Squared is 0.4638 meaning this is not a good model to predict salary

#MSE
summ=summary(mlr)
mse=(mean(summ$residuals^2))
paste('MSE:',mse)
#MSE = 92364.03

#Test set 
y_pred=predict(mlr,newdata = test)
data=data.frame(test$Salary,y_pred)
head(data)

#Of these predictions, only two were good predictions. 

#Validation
new=read.csv('Hitters_Validation_CSV.csv')
head(new)
new_x=new[c(1:20)]
new_x
data.frame(new[c(19)], predict(mlr, newdata=new_x))

#These predictions were better

#                   Experiment 3
#Splitting the data
names(dataset)
split=sample.split(dataset$Salary,SplitRatio = .6)
train=subset(dataset,split==TRUE)
test=subset(dataset,split==FALSE)

#MLR
names(dataset)
mlr=lm(formula=Salary~.,data=train)
summary(mlr)

#R-squared = 0.4806 meaning this is not a good model for predicting salary. 

#MSE
summ=summary(mlr)
mse=(mean(summ$residuals^2))
paste('MSE:',mse)

#MSE = 106276.26

y_pred=predict(mlr,newdata = test)
data=data.frame(test$Salary,y_pred)
head(data)

#Only two of these were good predictions

#Validation
new=read.csv('Hitters_Validation_CSV.csv')
head(new)
new_x=new[c(1:20)]
new_x
data.frame(new[c(19)], predict(mlr, newdata=new_x))

#These predictions were also unsuccessful 

