Data_startups=read.csv(file.choose())
str(Data_startups)
summary(Data_startups)
Data_startups$State=as.factor(Data_startups$State)
Data_startups$State=as.numeric(Data_startups$State)
plot(Data_startups$Profit,Data_startups$R.D.Spend)
plot(Data_startups$Profit,Data_startups$Administration)
plot(Data_startups$Profit,Data_startups$Marketing.Spend)
plot(Data_startups$Profit,Data_startups$State)
pairs(Data_startups)
normalize=function(x)  {
  return((x-min(x))/(max(x)-min(x)))
}
norm_data=as.data.frame(lapply(Data_startups,normalize))
Train_data=norm_data[c(1:40),]
Test_data=norm_data[c(41:50),]
library(neuralnet)
set.seed(100)
model_NN_1=neuralnet(Train_data$Profit~.,data = Train_data)
plot(model_NN_1)
pred_model_1=as.data.frame(predict(model_NN_1,newdata=Train_data))
pred_model_1
summary(model_NN_1)
cor(Train_data$Profit,pred_model_1)
model_NN_2=neuralnet(Train_data$Profit~.,data = Train_data,hidden = 2)
plot(model_NN_2)
pred_model_2=as.data.frame(predict(model_NN_2,newdata=Train_data))
pred_model_2
summary(model_NN_2)
cor(Train_data$Profit,pred_model_2)
model_NN_3=neuralnet(Train_data$Profit~.,data = Train_data,hidden = 3)
plot(model_NN_3)
pred_model_3=as.data.frame(predict(model_NN_3,newdata=Train_data))
pred_model_3
summary(model_NN_3)
cor(Train_data$Profit,pred_model_3)
#Final model with hidden layers = 5
Final_model=neuralnet(Train_data$Profit~.,data = Train_data,hidden = 5)
plot(Final_model)
pred_final_traindata=as.data.frame(predict(Final_model,newdata = Train_data))
pred_final_traindata
cor(Train_data$Profit,pred_final_traindata)
#Prediction on test data
pred_test_data=as.data.frame(predict(Final_model,newdata = Test_data))
pred_test_data
cor(Test_data$Profit,pred_test_data)
