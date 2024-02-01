Train_data=read.csv(file.choose())
View(Train_data)
str(Train_data)
summary(Train_data)
Train_data$workclass=as.factor(Train_data$workclass)
Train_data$education=as.factor(Train_data$education)
Train_data$maritalstatus=as.factor(Train_data$maritalstatus)
Train_data$occupation=as.factor(Train_data$occupation)
Train_data$relationship=as.factor(Train_data$relationship)
Train_data$race=as.factor(Train_data$race)
Train_data$sex=as.factor(Train_data$sex)
Train_data$native=as.factor(Train_data$native)
Train_data$Salary=as.factor(Train_data$Salary)
str(Train_data)
library(Hmisc)
describe(Train_data)
plot(Train_data$Salary,Train_data$age)
plot(Train_data$workclass,Train_data$Salary)
plot(Train_data$education,Train_data$Salary)
plot(Train_data$educationno,Train_data$Salary)
plot(Train_data$maritalstatus,Train_data$Salary)
plot(Train_data$occupation,Train_data$Salary)
plot(Train_data$relationship,Train_data$Salary)
plot(Train_data$race,Train_data$Salary)
plot(Train_data$sex,Train_data$Salary)
plot(Train_data$capitalgain,Train_data$Salary)
plot(Train_data$capitalloss,Train_data$Salary)
plot(Train_data$hoursperweek,Train_data$hoursperweek)
plot(Train_data$native,Train_data$Salary)

library(kernlab)
model_linear_train=ksvm(Train_data$Salary~.,data=Train_data,kernel="vanilladot")
model_linear_train
pred_linear_train=predict(model_linear_train,newdata=Train_data)
pred_linear_train
library(caret)
confusionMatrix(Train_data$Salary,pred_linear_train)
#Accuracy_Train(kernel=linear)=84.83%
library(pROC)
roc(Train_data$Salary,as.numeric(pred_linear_train),plot=TRUE,legacy.axes=TRUE)
#AUC=0.7604

model_RBF_train=ksvm(Train_data$Salary~.,data=Train_data,kernel="rbfdot")
model_RBF_train
pred_RBF_train=predict(model_RBF_train,newdata=Train_data)
pred_RBF_train
confusionMatrix(Train_data$Salary,pred_RBF_train)
#Accuracy_Train(kernel=rbf(gaussian))=86.20%
roc(Train_data$Salary,as.numeric(pred_RBF_train),plot = TRUE,legacy.axes=TRUE)
#AUC=77.89

Test_data=read.csv(file.choose())
Test_data$Salary=as.factor(Test_data$Salary)

pred_linear_test=predict(model_linear_train,newdata=Test_data)
pred_linear_test
confusionMatrix(Test_data$Salary,pred_linear_test)
#Accuracy_Test(kernel=linear)=84.63%
roc(Test_data$Salary,as.numeric(pred_linear_test),plot = TRUE,legacy.axes=TRUE)
#AUC=75.65%

pred_RBF_test=predict(model_RBF_train,newdata=Test_data)
pred_RBF_test
confusionMatrix(Test_data$Salary,pred_RBF_test)
#Accuracy_Test(kernel=rbf(gaussian))=85.44%
roc(Test_data$Salary,as.numeric(pred_RBF_test),plot = TRUE,legacy.axes=TRUE)
#AUC=76.74%