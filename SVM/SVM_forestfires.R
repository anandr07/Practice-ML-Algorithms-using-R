forestfires=read.csv(file.choose())
str(forestfires)
Data=forestfires[,-c(1,2)]
Data$size_category=as.factor(Data$size_category)
str(Data)
library(caTools)
x=sample.split(Data,SplitRatio = 0.8)
train_data=subset(Data,x==TRUE)
test_data=subset(Data,x==FALSE)
library(kernlab)
library(gmodels)
library(caret)

Model_linear=ksvm(train_data$size_category~.,data=train_data,kernel="vanilladot")
Model_linear
#Prediction on train data with kernel as linear
pred_linear_train=predict(Model_linear,newdata=train_data)
pred_linear_train
confusionMatrix(train_data$size_category,pred_linear_train)
#Prediction on test data with kernel as linear 
pred_linear_test=predict(Model_linear,newdata=test_data)
pred_linear_test
CrossTable(test_data$size_category,pred_linear_test)
confusionMatrix(test_data$size_category,pred_linear_test)
#Accuracy_Train(kernel is linear)=90.02%
#Accuracy_Test(kernel is linear)=87.74%

Model_poly=ksvm(train_data$size_category~.,data=train_data,kernel="polydot")
Model_poly
#Prediction on train data
pred_poly_train=predict(Model_poly,newdata=train_data)
confusionMatrix(train_data$size_category,pred_poly_train)
#Prediction on test data
pred_poly=predict(Model_poly,newdata=test_data)
CrossTable(test_data$size_category,pred_poly)
confusionMatrix(test_data$size_category,pred_poly)
#Accuracy_Train(kernel is polynomial)=90.02%
#Accuracy_Test(kernel is ploynomial)=87.74%

Model_RBF=ksvm(train_data$size_category~.,data=train_data,kernel="rbfdot")
Model_RBF
#Prediction on train data
pred_RBF_train=predict(Model_RBF,newdata=train_data)
pred_RBF_train
confusionMatrix(train_data$size_category,pred_RBF_train)
#Prediction on test data
pred_RBF_test=predict(Model_RBF,newdata=test_data)
CrossTable(test_data$size_category,pred_RBF_test)
confusionMatrix(test_data$size_category,pred_RBF_test)
#Accuracy_Train(kernel is RBF(Guassian))=78.59%
#Accuracy_Test(kernel is RBF(Gaussian))=74.53%

Model_Hyperbolic=ksvm(train_data$size_category~.,data=train_data,kernel="tanhdot")
Model_Hyperbolic
#Prediction on train data
pred_Hyperbolic_train=predict(Model_Hyperbolic,newdata=train_data)
confusionMatrix(train_data$size_category,pred_Hyperbolic_train)
#Prediction on test data
pred_Hyperbolic_test=predict(Model_Hyperbolic,newdata=test_data)
CrossTable(test_data$size_category,pred_Hyperbolic_test)
confusionMatrix(test_data$size_category,pred_Hyperbolic_test)
#Accuracy_Train(kernel is hyperbolic)=61.07%
#Accuracy_Test(kernel is hyperbolic)=63.21%

#The best accuracy for test and train data comes with kernel as linear i.e "vanilladot".
library(pROC)
roc(test_data$size_category,as.numeric(pred_linear_test),plot = TRUE,legacy.axes=TRUE)
#AUC i.e area under curve comes out to be 81.25%