#http://www.taiwanhouse.org.tw/a/blogs/show/3092702
#load library
library(readxl)
library(tseries)
library(forecast)
library(astsa)
op <- par(family='AdobeFanHeitiStd-Bold')

# read data / data preparation
house <- read_excel("/Volumes/GoogleDrive/我的雲端硬碟/NTPU/10_時間數列分析/final/house.xlsx")
house$cnt <- house$`第一次登記-棟數` + house$`移轉登記-合計-棟數`
train <- house$cnt[1:240]
test <- house$cnt[241:252]

# summary / graph
summary(train)
plot(train,type='l',xlab = "期數",ylab = "棟數")
abline(h=seq(30000,80000,10000),col="lightgray",lty=3)

# ADF(Augmented Dickey-Fuller)
adf.test(train) # p-value = 0.1257 > 0.05 非定態
## 一階差分
adf.test(diff(train)) # p-value = 0.01 < 0.05 定態
plot(diff(train),type='l',xlab = "期數",ylab = "一階差分之棟數")
abline(h=seq(-40000,30000,10000),col="lightgray",lty=3)

# ACF/PACF
acf2(diff(train),main='')


# ARIMA

## ARIMA(0,1,1)
mod1 <- Arima(train,order=c(0,1,1));summary(mod1)
Box.test(mod1$residuals,type="Ljung") # p-value = 0.5249 > 0.05 white noise
checkresiduals(mod1)
plot(forecast(mod1,h=12),main="")
mod1.f <- data.frame(forecast(mod1,h=12))$Point.Forecast
mod1.r <- test - mod1.f


## ARIMA(0,1,1)(0,1,1)[12]
mod2 <- Arima(train,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12));summary(mod2)
Box.test(mod2$residuals,type="Ljung") # p-value = 0.5407 > 0.05 white noise
checkresiduals(mod2)
plot(forecast(mod2,h=12),main="")
mod2.f <- data.frame(forecast(mod2,h=12))$Point.Forecast
mod2.r <- test - mod2.f


# accuracy
acc <- function(pred,actual) {
  error <- actual - pred
  rmse <- sqrt(mean(error^2))
  mae <- mean(abs(error))
  mape <- mean(abs(error/actual))*100
  
  cat("RMSE:",rmse," MAE:",mae," MAPE:",mape)
}

acc(mod1.f,test)
acc(mod2.f,test)

## combind model result
result <- data.frame(test)
result <- cbind(result,mod1.f)
result <- cbind(result,mod1.r)
result <- cbind(result,mod2.f)
result <- cbind(result,mod2.r)
colnames(result) <- c('actule','mod1_fit','mod1_residuals','mod2_fit','mod2_residuals')

## output / graph
result

plot(result$actule,type='l',ylim=c(40000,65000),lwd= 2,xlab = "期數",ylab = "棟數")
lines(c(1:12),result$mod1_fit,col="blue",lwd= 2)
lines(c(1:12),result$mod2_fit,col="red",lwd= 2)
abline(h=seq(40000,65000,5000),col="lightgray",lty=3)
legend("topleft",legend=c('實際值','ARIMA(0,1,1)','ARIMA(0,1,1)(0,1,1)[12]'),
       col=c('black','blue','red'),lty=1) 

par(op)
