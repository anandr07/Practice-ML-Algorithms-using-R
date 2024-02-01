Sms_raw_NB=read.csv(file.choose())
View(Sms_raw_NB)
str(Sms_raw_NB)
Sms_raw_NB$type=factor(Sms_raw_NB$type)
str(Sms_raw_NB$type)
table(Sms_raw_NB$type)
library(tm)
Data_cor=Corpus(VectorSource(Sms_raw_NB$text))
corpus=tm_map(Data_cor,tolower)
corpus=tm_map(Data_cor,removeNumbers)
corpus=tm_map(Data_cor,removePunctuation)
corpus=tm_map(Data_cor,stripWhitespace)
corpus=tm_map(Data_cor,removeWords,stopwords())
Data_DTM=DocumentTermMatrix(corpus)
5559*0.8
Train_raw_data=Sms_raw_NB[1:4447,]
Test_raw_data=Sms_raw_NB[4448:5559,]
Train_dtm_data=Data_DTM[1:4447,]
Test_dtm_data=Data_DTM[4448:5559,]
Train_cor_data=Data_cor[1:4447]
Test_cor_data=Data_cor[4448:5559]
table(Train_raw_data$type)
prop.table(table(Train_raw_data$type))
prop.table(table(Test_raw_data$type))
sms_freq=findFreqTerms(Train_dtm_data,3)
sms_train_dtm=DocumentTermMatrix(Train_cor_data,list(dictionary=sms_freq))
sms_test_dtm=DocumentTermMatrix(Test_cor_data,list(dictionary=sms_freq))
Counts=function(i){
  i=ifelse(i>0,1,0)
  i=factor(i,levels = c(0,1),labels = c("No","Yes"))
}
sms_train_dtm=apply(sms_train_dtm,MARGIN = 2,Counts)
sms_test_dtm=apply(sms_test_dtm,MARGIN = 2,Counts)
library(e1071)
sms_classifier=naiveBayes(sms_train_dtm,Train_raw_data$type)
pred=predict(sms_classifier,sms_test_dtm)
pred
library(gmodels)
table(Test_raw_data$type)
table(pred)
CrossTable(pred,Test_raw_data$type,prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,dmn=c("predicted","actual"))
#Accuracy
(958+125)/1112
#Accuracy=97.39
#The being very high the model could not be implemented as it wrongly classifies 4 ham mails as spam.