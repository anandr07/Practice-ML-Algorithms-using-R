Plastic_sales=read.csv(file.choose())
plot(Plastic_sales$Sales)
plot(Plastic_sales$Sales,type = "o")
x=data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
colnames(x)=month.abb
Plastic_sales=cbind(Plastic_sales,x)
Plastic_sales["t"]=c(1:60)
Plastic_sales["log_sales"]=log(Plastic_sales$Sales)
Plastic_sales["t_square"]=Plastic_sales$t*Plastic_sales$t

#Train data & Test data
Train_data=Plastic_sales[c(1:42),]
Test_data=Plastic_sales[c(43:60),]

    # Model_Linear
model_linear=lm(Train_data$Sales~t,data = Train_data )
summary(model_linear)
pred_linear=predict(model_linear,newdata = Test_data)
pred_linear
RMSE_linear=sqrt(mean(pred_linear-Test_data$Sales)^2)
RMSE_linear

   #Model_Exponential
model_expo=lm(Train_data$log_sales~t,data = Train_data)
summary(model_expo)
pred_expo=predict(model_expo,newdata = Test_data)
pred_expo
RMSE_expo=sqrt(mean(exp(pred_expo)-Test_data$Sales)^2)
RMSE_expo

   #Model_Quadratic
model_quad=lm(Train_data$Sales~t+t_square,data = Train_data)
summary(model_quad)
pred_quad=predict(model_quad,newdata = Test_data)
pred_quad
RMSE_quad=sqrt(mean(pred_quad,newdata=Test_data))
RMSE_quad
  
 #Model_Additive seasonality
model_add=lm(Train_data$Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = Train_data)
summary(model_add)
pred_add=predict(model_add,newdata = Test_data)
pred_add
RMSE_add=sqrt(mean(pred_add-Test_data$Sales)^2)
RMSE_add

  #Model_Additive Seasonality with quadratic trend
model_add_quad=lm(Train_data$Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = Train_data)
summary(model_add_quad)
pred_add_quad=predict(model_add_quad,newdata = Test_data)
pred_add_quad
RMSE_add_quad=sqrt(mean(pred_add_quad-Test_data$Sales)^2)
RMSE_add_quad

  #Model Multiplicative Seasonality
model_mul=lm(Train_data$log_sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = Train_data)
summary(model_mul)
pred_mul=predict(model_mul,newdata = Test_data)
pred_mul
RMSE_mul=sqrt(mean(exp(pred_mul)-Test_data$Sales)^2)
RMSE_mul

table_RMSE=data.frame(c("RMSE_linear","RMSE_expo","RMSE_quad","RMSE_add_quad","RMSE_mul"),c(RMSE_linear,RMSE_expo,RMSE_quad,RMSE_add_quad,RMSE_mul))

# Additive seasonality with quadratic trend model has least RMSE value

model_final=lm(Plastic_sales$Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = Plastic_sales)
summary(model_final)
pred_final=predict(model_final,newdata = Test_data)
pred_final
RMSE_final=sqrt(mean(pred_final-Test_data$Sales)^2)
RMSE_final

#ACF plot
plot(model_final)
acf(model_final$residuals,lag.max=10)

errors=arima(model_final$residuals,order = c(1,0,0))
errors$residuals
ARerrors=errors$residuals
acf(ARerrors,lag.max = 10)

#Predicting future errors
library(forecast)
future_errors=forecast(errors,h=12)
future_errors=data.frame(future_errors)
View(future_errors)
future_errors=future_errors$Point.Forecast

#Final predicted values with errors included
pred_values=data.frame(pred_final)+future_errors
