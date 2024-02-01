Zoo_data=read.csv(file.choose())
View(Zoo_data)
summary(Zoo_data)
str(Zoo_data)
normalize=function(i){
  return((i-min(i))/(max(i)-min(i)))
}
New_data=as.data.frame(lapply(Zoo_data[,-c(1,18)],normalize))
summary(New_data)
str(New_data)
Train_data=New_data[c(1:81),]
Test_data=New_data[c(82:101),]
Train_type=as.data.frame(Zoo_data[c(1:81),18])
Test_type=as.data.frame(Zoo_data[c(82:101),18])
library(class)
Zoo_pred=knn(train=Train_data,test=Test_data,cl=Train_type[,1],k=1)
library(gmodels)
CrossTable(x=Test_type[,1],y=Zoo_pred,prop.chisq = FALSE)
#Accuracy
(5+4+1+3+1+2+1)/20
#Test accuracy=85%
#Their are also 3 misclassifications
Zoo_pred_1=knn(train=Train_data,test=Test_data,cl=Train_type[,1],k=2)
library(gmodels)
CrossTable(x=Test_type[,1],y=Zoo_pred_1,prop.chisq = FALSE)
#Accuracy=
Zoo_pred_2=knn(train=Train_data,test=Test_data,cl=Train_type[,1],k=3)
library(gmodels)
CrossTable(x=Test_type[,1],y=Zoo_pred_2,prop.chisq = FALSE)
#Accuracy=
Zoo_pred_3=knn(train=Train_data,test=Test_data,cl=Train_type[,1],k=5)
library(gmodels)
CrossTable(x=Test_type[,1],y=Zoo_pred_3,prop.chisq = FALSE)
#Accuracy=
Zoo_pred_4=knn(train=Train_data,test=Test_data,cl=Train_type[,1],k=7)
library(gmodels)
CrossTable(x=Test_type[,1],y=Zoo_pred_4,prop.chisq = FALSE)
#Accuracy=
Zoo_pred_5=knn(train=Train_data,test=Test_data,cl=Train_type[,1],k=11)
library(gmodels)
CrossTable(x=Test_type[,1],y=Zoo_pred_5,prop.chisq = FALSE)
#Accuracy=
Zoo_pred_6=knn(train=Train_data,test=Test_data,cl=Train_type[,1],k=15)
library(gmodels)
CrossTable(x=Test_type[,1],y=Zoo_pred_6,prop.chisq = FALSE)
#Accuracy=
Zoo_pred_7=knn(train=Train_data,test=Test_data,cl=Train_type[,1],k=19)
library(gmodels)
CrossTable(x=Test_type[,1],y=Zoo_pred_7,prop.chisq = FALSE)
#Accuracy=
Zoo_pred_8=knn(train=Train_data,test=Test_data,cl=Train_type[,1],k=23)
library(gmodels)
CrossTable(x=Test_type[,1],y=Zoo_pred_8,prop.chisq = FALSE)
#Accuracy=