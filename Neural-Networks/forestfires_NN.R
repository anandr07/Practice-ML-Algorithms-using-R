forest_data=read.csv(file.choose())
str(forest_data)
new_data=forest_data[,-c(1,2)]
new_data$size_category=as.factor(new_data$size_category)
new_data$size_category=as.numeric(new_data$size_category)
str(new_data)
normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
norm_data=as.data.frame(lapply(new_data,normalize))
library(caTools)
i=sample.split(norm_data,SplitRatio = 0.8)
Train_data=subset(norm_data,i=="TRUE")
Test_data=subset(norm_data,i=="FALSE")
library(neuralnet)
model_NN=neuralnet(Train_data$size_category~.,data = Train_data,hidden = 5)
summary(model_NN)
plot(model_NN)
result=NULL
result=compute(model_NN,Train_data)
pred_train=result$net.result
cor(pred_train,Train_data$size_category)
#Test data
result_test=compute(model_NN,Test_data)
pred_test=result_test$net.result
cor(pred_test,Test_data$size_category)
library(pROC)
roc(Test_data$size_category,pred_test,plot = TRUE,legacy.axes=TRUE)
