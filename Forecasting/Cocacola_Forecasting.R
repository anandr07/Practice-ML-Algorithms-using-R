library(readxl)
library(forecast)
library(fpp)
library(tseries)
library(smooth)
library(Hmisc)
#Reading data
Coca_cola=read_xlsx("C://Users//priti//Desktop//Anand//Assignment//Forecasting//CocaCola_Sales_Rawdata.xlsx")
View(Coca_cola)
describe(Coca_cola)

#Visualizing sales and converting to time series data
hist(Coca_cola$Sales)
plot.ts(Coca_cola$Sales)
plot(Coca_cola$Sales,type = "o")
ts_sales=ts(Coca_cola$Sales,frequency = 4,start = c(42))
View(ts_sales)
decompose_sales=decompose(ts_sales,type = "additive")
seasonal_sales=as.ts(decompose_sales$seasonal)
Trend_sales=as.ts(decompose_sales$trend)
Random_sales=as.ts(decompose_sales$random)
par(mfrow=c(3,1))
#Seasonality in sales
plot.ts(seasonal_sales)
#Trend in sales
plot.ts(Trend_sales)
#Random component in sales
plot.ts(Random_sales)

#Data partition
Train_data=ts_sales[1:29]
Test_data=ts_sales[30:42]
Train_data=ts(Train_data,frequency = 4)
Test_data=ts(Test_data,frequency = 4)

#Holt winters model assuming only trend as parameter (Alpha=0.2)
model_HW_1=HoltWinters(Train_data,alpha = 0.2,beta = F,gamma = F)
pred_value_1=data.frame(predict(model_HW_1,n.ahead = 13))
plot(forecast(model_HW_1,h=4))
HW_MAPE_1=MAPE(pred_value_1$fit,Test_data)*100
HW_MAPE_1
#37.22549

#Holt winters model with trend and level as parameter (Alpha=0.2,Beta=0.3)
model_HW_2=HoltWinters(Train_data,alpha = 0.2,beta = 0.3,gamma = F)
pred_value_2=data.frame(predict(model_HW_2,n.ahead = 13))
plot(forecast(model_HW_2,h=4))
HW_MAPE_2=MAPE(pred_value_2$fit,Test_data)*100
HW_MAPE_2

#Holt winters model with trend, level and seasonality as a parameter (Alpha=0.2,Beta=0.3,Gamma=0.3)
model_HW_3=HoltWinters(Train_data,alpha = 0.2,beta = 0.3,gamma = 0.3)
pred_value_3=data.frame(predict(model_HW_3,n.ahead = 13))
plot(forecast(model_HW_3,h=4))
#From the plot it is seen the forecasted values follow past data
HW_MAPE_3=MAPE(pred_value_3$fit,Test_data)*100
HW_MAPE_3

#Holt winters model with default values of Alpha,Beta and Gamma
model_HW_4=HoltWinters(Train_data,beta=F,gamma = F)
pred_value_4=data.frame(predict(model_HW_4,n.ahead = 13))
plot(forecast(model_HW_4,h=4))
HW_MAPE_4=MAPE(pred_value_4$fit,Test_data)*100
HW_MAPE_4

#Table of MAPE values
table_MAPE=data.frame(c("model_HW_1","model_HW_2","model_HW_3","model_HW_4"),c(HW_MAPE_1,HW_MAPE_2,HW_MAPE_3,HW_MAPE_4))
View(table_MAPE)

#Model 4 with parameters (Alpha=0.2,Beta=0.3,Gamma=0.3) has least values of MAPE
