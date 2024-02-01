#Train data
 
salary_train=read.csv(file.choose())
View(salary_train)
str(salary_train)
salary_train$Salary=as.factor(salary_train$Salary)
str(salary_train)
table(salary_train$Salary)
prop.table(table(salary_train$Salary))
salary_train$maritalstatus=as.factor(salary_train$maritalstatus)
salary_train$occupation=as.factor(salary_train$occupation)
salary_train$relationship=as.factor(salary_train$relationship)
salary_train$race=as.factor(salary_train$race)
salary_train$sex=as.factor(salary_train$sex)
salary_train$workclass=as.factor(salary_train$workclass)
salary_train$education=as.factor(salary_train$education)
salary_train$educationno=as.factor(salary_train$educationno)
salary_train$native=as.factor(salary_train$native)
str(salary_train)
plot(salary_train$Salary,salary_train$age,horizontal=TRUE)
plot(salary_train$Salary,salary_train$workclass)
plot(salary_train$Salary,salary_train$education)
plot(salary_train$Salary,salary_train$education)
plot(salary_train$Salary,salary_train$educationno)
plot(salary_train$Salary,salary_train$maritalstatus)
plot(salary_train$Salary,salary_train$occupation)
plot(salary_train$Salary,salary_train$relationship)
plot(salary_train$Salary,salary_train$race)
plot(salary_train$Salary,salary_train$sex)
plot(salary_train$Salary,salary_train$capitalgain,horizontal=TRUE)
plot(salary_train$Salary,salary_train$capitalloss,horizontal=TRUE)
plot(salary_train$Salary,salary_train$hoursperweek,horizontal=TRUE)
library(e1071)
classify=naiveBayes(Salary~.,data = salary_train)
train_pred=as.factor(predict(classify,tpye=c("response"),salary_train))
train_pred
str(train_pred);table(train_pred)
prop.table(table(train_pred))
library(caret)
conf_matrix_train=as.matrix(confusionMatrix(train_pred,salary_train$Salary))
accuracy_train=sum(diag(conf_matrix_train))/sum(conf_matrix_train)
accuracy_train
#Train Accuracy=82.20

#Test data

salary_test=read.csv(file.choose())
View(salary_test)
salary_test$Salary=as.factor(salary_test$Salary)
str(salary_test$Salary)
table(salary_test$Salary)
prop.table(table(salary_test$Salary))
salary_test$maritalstatus=as.factor(salary_test$maritalstatus)
salary_test$occupation=as.factor(salary_test$occupation)
salary_test$relationship=as.factor(salary_test$relationship)
salary_test$race=as.factor(salary_test$race)
salary_test$sex=as.factor(salary_test$sex)
salary_test$workclass=as.factor(salary_test$workclass)
salary_test$education=as.factor(salary_test$education)
salary_test$educationno=as.factor(salary_test$educationno)
salary_test$native=as.factor(salary_test$native)
str(salary_test)
#Predictions and accuracy
test_pred=as.factor(predict(classify,salary_test))
test_pred
table(test_pred)
prop.table(table(test_pred))
conf_matrix_test=as.matrix(confusionMatrix(test_pred,salary_test$Salary))
accuracy_test=sum(diag(conf_matrix_test))/sum(conf_matrix_test)
accuracy_test
#Cross Table
library(gmodels)
CrossTable(test_pred,salary_test$Salary,prop.chisq = FALSE,prop.r = FALSE,prop.t = FALSE)
(10549+1781)/15060
#The model wrongly classifies 1919 low salary as high salary.
#The model wrongly classifies 811 high salary as low salary.
#Train Accuracy=82.20%,Test Accuracy=81.87%
#Train accuracy and test accuracy of the model are close and therefore the model is a good model.