MLRcompurterdata=read.csv(file.choose())
Data=MLRcompurterdata[,-1]
View(Data)
library(caTools)
summary(Data)
library(dummies)
CD=dummy(Data$cd)
M=dummy(Data$multi)
P=dummy(Data$premium)
New_data=cbind(Data,as.data.frame(CD),as.data.frame(M),as.data.frame(P))
New_Data=New_data[,-c(6,7,8)]
i=sample.split(Data,SplitRatio = 0.8)
Train_Data=subset(New_Data,i== "TRUE")                 
Test_data=subset(New_Data,i=="FALSE")
library(moments)
skewness(Train_Data)
kurtosis(Train_Data)
hist(Train_Data$price)
boxplot(Train_Data)#their are outliers in price and hd
cor(Train_Data)
pairs(Train_Data)
plot(Train_Data$ads,Train_Data$trend)
attach(Train_Data)
#Their is no collinearity problem except a little collinearity seen in ads and trend
#Model with all variables included
model_1=lm(Train_Data$price~.,data = Train_Data)
summary(model_1)
#All the variables are significant  except multino,cdno,premiumno and the r squared value is 0.7744
library(HH)
vif(model_1)
AIC(model_1)
stepAIC(model_1)
#All varience infliation factor are less than 10
library(car)
avPlots(model_1)
#from avPlots we can conclude that cd and multi can be removed 
plot(model_1)
influencePlot(model_1)
#Their are some outliers (1441,1701,5961,3784)
Data2=Train_Data[c(-1441,-1701,-5961,-3784),-c(8,10,12)]
Final_model=lm(Data2$price~.,data = Data2)
summary(Final_model)
#Their is much differrence in the R squared value afer removing the outliers
AIC(Final_model)
stepAIC(Final_model)
vif(Final_model)
avPlots(Final_model)
#Root mean square error
confint(Final_model,level = 0.95)
pred_value_train=as.data.frame(predict(Final_model,interval = "predict"))
error_train=pred_value_train$fit-Data2$price
sqrt(mean(error_train^2))
cor(pred_value_train$fit,Data2$price)#Accuracy on train data=88.06%
pred_value_test=as.data.frame(predict.lm(Final_model,newdata=Test_data))
cor(pred_value_test$`predict.lm(Final_model, newdata = Test_data)`,Test_data$price)#Accuracy on test data=88.04%
