glass_data=read.csv(file.choose())
View(glass_data)
str(glass_data)
normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
New_data=as.data.frame(lapply(glass_data[,-10], normalize))
summary(New_data)
str(New_data)
library(caTools)
set.seed(100)

#Train and test data partition
i=sample.split(New_data,SplitRatio = 0.70)
Train_data=subset(New_data[,-10],i=="TRUE")
Test_data=subset(New_data[,-10],i=="FALSE")
Train_type=subset(glass_data[,10],i=="TRUE")
Test_type=subset(glass_data[,10],i=="FALSE")

library(class)
library(pROC)
library(caret)
library(gmodels)

#Thumb rule : sqrt(N)=sqrt(214)=14.62(lets take k values 14 and 15)
glass_pred_14=knn(train=Train_data,test=Test_data,cl=Train_type,k=14)
#Crosstable
CrossTable(x=Test_type,y=glass_pred_14,prop.chisq = FALSE)
#Accuracy=57.75
confusionMatrix(glass_pred_14,as.factor(Test_type))
roc(as.numeric(Test_type),as.numeric(glass_pred_14),plot = TRUE,legacy.axes=TRUE)
#AUC=68.96

glass_pred_15=knn(train=Train_data,test=Test_data,cl=Train_type,k=3)
#Crosstable
CrossTable(x=Test_type,y=glass_pred_15,prop.chisq = FALSE)
#Accuracy=63.38
confusionMatrix(glass_pred_15,as.factor(Test_type))
roc(as.numeric(Test_type),as.numeric(glass_pred_15),plot = TRUE,legacy.axes=TRUE)
#AUC=64.78

#for loop to find accuracy for k values from 1 to 15
i=1                        
k_opt=1
for (i in 1:15){ 
  knn_glass <-  knn(train=Train_data, test=Test_data, cl=Train_type, k=i)
  k_opt[i] <- sum(Test_type == knn_glass)/NROW(Test_type)*100
  k=i  
  cat(k,'=',k_opt[i],'\n')   
}
#Plot for k values
plot(k_opt, type="b", xlab="K-Values",ylab="Accuracy")

#As per accuracy k=1 has the highest accuracy,therefore model with k=1 is choosen.

glass_pred_1=knn(train=Train_data,test=Test_data,cl=Train_type,k=1)
#Crosstable
CrossTable(x=Test_type,y=glass_pred_1,prop.chisq = FALSE)
#Accuracy=66.20%
confusionMatrix(glass_pred_1,as.factor(Test_type))
roc(as.numeric(Test_type),as.numeric(glass_pred_1),plot = TRUE,legacy.axes=TRUE)
#AUC=69.91
#The accuracy for k=1 and also AUC is highest for k=1.