attach(`50_Startups`)
View(`50_Startups`)
library(dummies)
S=dummy(`50_Startups`$State)
DATA=cbind(`50_Startups`,S)
View(DATA)
summary(DATA)
library(moments)
attach(DATA)
skewness(Profit)
kurtosis(Profit)
plot(density(Profit))
#Looking at skewness,kurtosis and density plot we can say that y is very close to normal distribution 
boxplot(Profit)
#Boxplot tells us their are no outlier in the response variable
pairs(DATA[,-4])
cor(DATA[,-4])
LM_1=lm(Profit~.,data = DATA[,-4])
summary(LM_1)
library(HH)
vif(LM_1)
library(car)
avPlots(LM_1)
influencePlot(LM_1)
plot(LM_1)
LM_A=lm(Profit~Administration)
summary(LM_A)
LM_S=lm(Profit~Marketing.Spend)
summary(LM_S)
#State is insignificant
Newdata=DATA[,-c(4,6,7,8)]
LM_2=lm(Profit~R.D.Spend+Marketing.Spend+Administration,data = Newdata[c(-50,-46,-47,-49),])
summary(LM_2)
avPlots(LM_2)
vif(LM_2)
plot(LM_2)
#Administration is to be removed from the model as it is not significant
LM_3=lm(Profit~R.D.Spend+Marketing.Spend,data = Newdata[c(-50,-46,-47,-49,-20,-28,-7),])
summary(LM_3)
avPlots(LM_3)
vif(LM_3)
#Marketing.spend is now significant.
influenceIndexPlot(LM_3)
plot(LM_3)
#final model
LM_4=lm(Profit~R.D.Spend+Marketing.Spend,data = Newdata[c(-50,-46,-47,-28),])
summary(LM_4)
avPlots(LM_4)
vif(LM_4)
plot(LM_4)
#In the above model Marketing.spend comes out to be significant and also the R square value is 0.964 close to adjusted R square value.
confint(LM_4,level=0.95)
Pred_val=as.data.frame(predict(LM_4,interval = "predict"))
#mean square error
DATA1=Newdata[-c(50,46,47,28)]
Error=Pred_val$fit-DATA1$Profit
mean(Error^2)
sqrt(mean(Error^2))
