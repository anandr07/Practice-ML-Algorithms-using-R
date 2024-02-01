Company_Data=read.csv(file.choose())
View(Company_Data)
str(Company_Data)
summary(Company_Data)
library(moments)
skewness(Company_Data$Sales)
kurtosis(Company_Data$Sales)
hist(Company_Data$Sales)
plot(density(Company_Data$Sales))
sales_category=as.data.frame(ifelse(Company_Data$Sales>10,"High","Low"))
New_data=cbind(Company_Data,sales_category)
library(plyr)
names(New_data)
New_data=rename(New_data,c('ifelse(Company_Data$Sales > 10, \"High\", \"Low\")'='Sales_category'))
nrow(New_data)*0.7
New_data$ShelveLoc=as.factor(New_data$ShelveLoc)
New_data$Urban=as.factor(New_data$Urban)
New_data$US=as.factor(New_data$US)
New_data$Sales_category=as.factor(New_data$Sales_category)
New_data=New_data[,-1]
Train_data=New_data[1:280,]
Test_data=New_data[281:400,]
str(Train_data)
library(tree)
Model=tree(Sales_category~.,data = Train_data)
plot(Model)
text(Model,pretty = 0)
summary(Model)
pred_class=as.data.frame(predict(Model,data=Train_data))
pred_class["Predicted_class"]=NULL
for(i in 1:nrow(pred_class)){
  pred_class[i,"Predicted_class"]=ifelse(pred_class[i,"High"]>0.5,"High","Low")
}
library(gmodels)
CrossTable(Train_data$Sales_category,pred_class$Predicted_class)
library(caret)
pred_class$Predicted_class=as.factor(pred_class$Predicted_class)
confusionMatrix(pred_class$Predicted_class,Train_data$Sales_category)
#Accuracy=93.93
pred_Test=as.data.frame(predict(Model,newdata=Test_data))
pred_Test["Predicted_Test"]=NULL
for(x in 1:nrow(pred_Test)){
  pred_Test[x,"Predicted_Test"]=ifelse(pred_Test[x,"High"]>0.5,"High","Low")
}
CrossTable(Test_data$Sales_category,pred_Test$Predicted_Test)
pred_Test$Predicted_Test=as.factor(pred_Test$Predicted_Test)
confusionMatrix(pred_Test$Predicted_Test,Test_data$Sales_category)
#Accuracy=75%