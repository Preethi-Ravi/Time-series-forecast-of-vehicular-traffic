library(readr)
library(forecast)
library(xts)
sample_submission <- read_csv("C:/Users/preethi/Downloads/sample_submission.csv")
#View(sample_submission)
train <- read_csv("C:/Users/preethi/Downloads/train.csv")
test <- read_csv("C:/Users/preethi/Downloads/test.csv")
plot(train$DateTime,train$Vehicles)
train_xts<-xts(train$Vehicles,train$DateTime)
plot(train_xts)
#Since training data contains duplicates for datetime values for each junction, 
#let us split the training set into 4 based on junction
train1<-train[train$Junction==1,3]
train2<-train[train$Junction==2,3]
train3<-train[train$Junction==3,3]
train4<-train[train$Junction==4,3]
test1<-test[test$Junction==1,]
test2<-test[test$Junction==2,]
test3<-test[test$Junction==3,]
test4<-test[test$Junction==4,]
#creating a time series object for junction 1
data_msts1<-msts(train1,seasonal.periods = c(24,168))
plot(data_msts1[1:168],type="l")
#from the plot we can see that it is a double seasonal (daily, weekly) time series
#The traffic reaches peak during the day and during the night it falls and picks up again the next day
#During the weekends there is a consistent dip
acf(data_msts1,na.action = na.pass,main='ACF vehicle traffic')
pacf(data_msts1,na.action = na.pass,main='PACF vehicle traffic')
#fitting a model for junction 1 using tbats function
#tbats function takes care of exponential smoothening, trend and seasonality when the multiseasonal periods are mentioned
fit1<-tbats(data_msts1)
summary(fit1)
plot(fit1$errors)
acf(fit1$errors,na.action = na.pass,main='ACF vehicle traffic model errors')
pacf(fit1$errors,na.action = na.pass,main='PACF vehicle traffic model errors')
rmse<-sqrt(fit1$variance)
rmse
# rmse = 0.1195002
test1_out<-forecast(fit1)
out1<-round(test1_out$fitted)+50
out1<-data.frame(out1)
out1<-out1[1:2952,]
test1_output<-cbind(test1,Vehicles=out1)
plot(test1_output$Vehicles,type="l", main="Forecasted vehicular traffic for junction 1")


#creating a time series object for junction 2
data_msts2<-msts(train2,seasonal.periods = c(24,168))
plot(data_msts2[1:168],type="l")

#from the plot we can see that it is a double seasonal (daily, weekly) time series
#The traffic reaches peak during the day and during the night it falls and picks up again the next day
#During the weekends there is a consistent dip
acf(data_msts2,na.action = na.pass,main='ACF vehicle traffic')
pacf(data_msts2,na.action = na.pass,main='PACF vehicle traffic')
#fitting a model for junction 2 using arima function
fit2<-auto.arima(data_msts2)
summary(fit2)
#rmse = 2.793329
plot(fit2$residuals)
acf(fit2$residuals,na.action = na.pass,main='ACF vehicle traffic model errors')
pacf(fit2$residuals,na.action = na.pass,main='PACF vehicle traffic model errors')
rmse<-sqrt(fit2$var.coef)
rmse
test2_out<-forecast(fit2)
out2<-round(test2_out$fitted)+20
out2<-data.frame(out2)
out2<-out2[1:2952,]
test2_output<-cbind(test2,Vehicles=out2)
plot(test2_output$Vehicles,type="l", main="Forecasted vehicular traffic for junction 2")

#creating a time series object for junction 3
data_msts3<-msts(train3,seasonal.periods = c(24,168))
plot(data_msts3[1:168],type="l")

acf(data_msts3,na.action = na.pass,main='ACF vehicle traffic')
pacf(data_msts3,na.action = na.pass,main='PACF vehicle traffic')
#fitting a model for junction 3 using tbats function
fit3<-tbats(data_msts3)
summary(fit3)
plot(fit3$errors)
acf(fit3$errors,na.action = na.pass,main='ACF vehicle traffic model errors')
pacf(fit3$errors,na.action = na.pass,main='PACF vehicle traffic model errors')
rmse<-sqrt(fit3$variance)
rmse
#rmse = 0.4775908
test3_out<-forecast(fit3)
out3<-round(test3_out$fitted)+20
out3<-data.frame(out3)
out3<-out3[1:2952,]
test3_output<-cbind(test3,Vehicles=out3)
plot(test3_output$Vehicles,type="l", main="Forecasted vehicular traffic for junction 3")

#creating a time series object for junction 4
data_msts4<-msts(train4,seasonal.periods = c(24,168))
plot(data_msts4[1:168],type="l")

acf(data_msts4,na.action = na.pass,main='ACF vehicle traffic')
pacf(data_msts4,na.action = na.pass,main='PACF vehicle traffic')
#fitting a model for junction 4 using tbats function
fit4<-tbats(data_msts4)
summary(fit4)
plot(fit4$errors)
acf(fit4$errors,na.action = na.pass,main='ACF vehicle traffic model errors')
pacf(fit4$errors,na.action = na.pass,main='PACF vehicle traffic model errors')
rmse<-sqrt(fit4$variance)
rmse
#rmse = 0.7402764
test4_out<-forecast(fit4)
out4<-round(test4_out$fitted)+10
out4<-data.frame(out4)
out4<-out4[1:2952,]
test4_output<-cbind(test4,Vehicles=out4)
plot(test4_output$Vehicles,type="l", main="Forecasted vehicular traffic for junction 4")
#SOlution file
submission<-rbind(test1_output,test2_output,test3_output,test4_output)
submission<-submission[,3:4]
write.csv(submission,file="C:/Users/preethi/Downloads/submission.csv",row.names=F)

rmse<-c(0.1195002,2.793329, 0.4775908, 0.7402764)
rmse
#Inference: Nature of input time series
#Double seasonal: peaks per day, peaks per week (higher during week days, lower during weekends)
#Increasing trend: Since the number of vehicles owned by total population increases over the years (new vehicles manufactured and bought by people)
#There is an increasing trend in the data
#Best way to handle such data is using tbats function. But then again, trend component is not caught by this function very well
#Further work: Decomposing the time series into components and using a multiplicative model might yield better results
# Interesting reads: https://robjhyndman.com/papers/multiseasonal.pdf
#https://robjhyndman.com/publications/complex-seasonality/
