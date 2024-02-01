library(readxl)
library(forecast)
library(fpp)
Airlinesdata=read_xlsx("C://Users//priti//Desktop//Anand//Assignment//Forecasting//Airlines+Data.xlsx")
View(Airlinesdata)
library(Hmisc)
describe(Airlinesdata)
plot(Airlinesdata$Passengers,type = "b")
plot(Airlinesdata$Passengers,type = "o")
dummy_time=data.frame(outer(rep(month.abb,length=96),month.abb,"==")+0)
colnames(dummy_time)=month.abb
Airlinesdata=cbind(Airlinesdata,dummy_time)
Airlinesdata["t"]=c(1:96)
Airlinesdata["log_Y"]=log(Airlinesdata$Passengers)
Airlinesdata["t_square"]=Airlinesdata$t*Airlinesdata$t
#Train and Test data
Train_data=Airlinesdata[c(1:67),]
Test_data=Airlinesdata[c(68:96),]

    #Linear model
model_linear=lm(Train_data$Passengers ~ t,data = Train_data)
summary(model_linear)
pred_linear=predict(model_linear,newdata = Test_data)
RMSE_linear=sqrt(mean((pred_linear-Test_data$Passengers)^2))
RMSE_linear

    #Exponential model
model_expo=lm(Train_data$log_Y ~ t,data =Train_data)
summary(model_expo)
pred_expo=predict(model_expo,newdata = Test_data)
RMSE_expo=sqrt(mean((exp(pred_expo)-Test_data$Passengers)^2))                  
RMSE_expo

    #Quadratic model
model_quad=lm(Train_data$Passengers ~ t+t_square,data = Train_data)
summary(model_quad)
pred_quad=predict(model_quad,newdata = Test_data)
RMSE_quad=sqrt(mean((pred_quad-Test_data$Passengers)^2))
RMSE_quad

    #Additive Seasonlity
model_add_quad=lm(Train_data$Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = Train_data)
summary(model_add_quad)
pred_add_quad=predict(model_add_quad,newdata = Test_data)
RMSE_add_quad=sqrt(mean((pred_add_quad-Test_data$Passengers)^2))
RMSE_add_quad

    #Multiplicative Seasonality
model_mul=lm(Train_data$log_Y ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = Train_data)
summary(model_mul)
pred_mul=predict(model_mul,newdata = Test_data)
RMSE_mul=sqrt(mean((exp(pred_mul)-Test_data$Passengers)^2))
RMSE_mul
table_RMSE=data.frame(c("RMSE_linear","RMSE_expo","RMSE_quad","RMSE_add_quad","RMSE_mul"),c(RMSE_linear,RMSE_expo,RMSE_quad,RMSE_add_quad,RMSE_mul))
View(table_RMSE)

#Model with additive seasonality with quadratic trend has least RMSE
model_final=lm(Airlinesdata$Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = Airlinesdata)
summary(model_final)
pred_final=predict(model_final,newdata = Test_data)
RMSE_final=sqrt(mean((pred_final-Test_data$Passengers)^2))
RMSE_final

#Predicting on test data using final model
pred_values=as.data.frame(round(pred_final))
Airlinesdata=cbind(Airlinesdata,pred_values)
plot(model_final)
#ACF plot
acf(model_final$residuals,log.max=10)

#errors 
errors=arima(model_final$residuals,order = c(1,0,0))
errors$residuals
ARerrors=errors$residuals
acf(ARerrors,lag.max = 10)

#Predicting future errors
future_errors=forecast(errors,h=12)
future_errors=data.frame(future_errors)
View(future_errors)
future_errors=future_errors$Point.Forecast

#Final predictions on test data including errors
pred_new_value=pred_values+future_errors
 
#Test data and predicted values
Pred_test=cbind(Test_data$Passengers,round(pred_new_value))
