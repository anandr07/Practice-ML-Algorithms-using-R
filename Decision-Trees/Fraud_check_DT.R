Fraud_data=read.csv(file.choose())
View(Fraud_data)
str(Fraud_data)
summary(Fraud_data)
library(moments)
skewness(Fraud_data$Taxable.Income)
kurtosis(Fraud_data$Taxable.Income)
hist(Fraud_data$Taxable.Income)
densityplot(Fraud_data$Taxable.Income)
Tax_income=as.data.frame(ifelse(Fraud_data$Taxable.Income>30000,"Good","Risky"))
New_data=cbind(Fraud_data,Tax_income)
str(New_data)
New_data$Undergrad=as.factor(New_data$Undergrad)
New_data$Marital.Status=as.factor(New_data$Marital.Status)
New_data$Urban=as.factor(New_data$Urban)
library(plyr)
names(New_data)
New_data=rename(New_data,c('ifelse(Fraud_data$Taxable.Income > 30000, \"Good\", \"Risky\")'='Tax_inc'))
New_data$Tax_inc=as.factor(New_data$Tax_inc)
New_data=New_data[,-3]
nrow(New_data)*0.75
Train_data=New_data[1:450,]
Test_data=New_data[451:600,]
library(tree)
Model_train=tree(Train_data$Tax_inc~.,data = Train_data)
plot(Model_train)
text(Model_train,pretty = 0)
summary(Model_train)
pred_train=as.data.frame(predict(Model_train,data=Train_data))
pred_train["Pred_train"]=NULL
for(i in 1:nrow(pred_train)){
  pred_train[i,"Pred_train"]=ifelse(pred_train[i,"Good"]>0.5,"Good","Risky")
}
library(gmodels)
CrossTable(Train_data$Tax_inc,pred_train$Pred_train)
library(caret)
pred_train$Pred_train=as.factor(pred_train$Pred_train)
confusionMatrix(pred_train$Pred_train,Train_data$Tax_inc)
#Accuracy=77.33%
pred_test=as.data.frame(predict(Model_train,newdata = Test_data))
pred_test["Pred_test"]=NULL
for(i in 1:nrow(pred_test)){
  pred_test[i,"Pred_test"]=ifelse(pred_test[i,"Good"]>0.5,"Good","Risky")
}
CrossTable(Test_data$Tax_inc,pred_test$Pred_test)
pred_test$Pred_test=as.factor(pred_test$Pred_test)
confusionMatrix(pred_test$Pred_test,Test_data$Tax_inc)
#Accuracy=85.33%