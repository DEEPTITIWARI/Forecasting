library(forecast)

#Function for residual histogram
plotForecastErrors <- function(forecasterrors,modeltype="Residual Histogram")
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins, main =modeltype )
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

soups <- read.csv("Campbells_233779_data.csv")




soupstimeseries<- ts(soups)

findfrequency(soupstimeseries)

plot.ts(soupstimeseries)




# additional plots
monthplot(soupstimeseries)
seasonplot(soupstimeseries)
ggtsdisplay(soups1tm)

#Data Partitioning
souptraining<- ts(soupstimeseries[c(1:48),])
soupvalidation<- ts(soupstimeseries[c(49:52),])



soupfit1 <- meanf(soupstimeseries,h=4)
soupfit2 <- naive(soupstimeseries,h=4)


plot(soupfit1, plot.conf=FALSE,
     main="Forecasts for quarterly beer production")
lines(soupfit2$mean,col=2)

lines(soupstimeseries)
legend("topright", lty=1, col=c(1,2),
       legend=c("Mean method","Naive method"))


a<-accuracy(soupfit1)
print("Result of Mean method")
a
accuracy(soupfit2)


#Residuals from mean and naive method

res1<- residuals(soupfit1)
res2<- residuals(soupfit2)

#Plotting residuals from both te methods
plot(res1, main="Residuals from mean method",ylab="", xlab="Week")

plot(res2, main="Residuals from naive method", ylab="", xlab="Week")

#ACF and histogram from both the methods
Acf(res1, main="ACF of mean residuals")
Acf(res2, main="ACF of naive residuals")
plotForecastErrors(res1)
plotForecastErrors(res2)


#Liner Regression-Linear Trend
soup.fit3 <- tslm(souptraining~trend)
f3 <- forecast(soup.fit3, h=4, level=c(80,95))
plot(f3, ylab="Units sold per week",xlab="Week",main="Forecast from Linear Model")
lines(fitted(soup.fit3),col="blue")
res3 <- residuals(soup.fit3)
summary(soup.fit3)
accuracy(f3)
plot(res3, main="Residuals from linear regression method", ylab="residuals", xlab="Week")
abline(0,0,col="gray60")
Acf(res3, main="ACF of linear regression residuals")
plotForecastErrors(res3)



#Linear Regression-Quadratic Trend
soup.fit4 <- tslm(souptraining~trend+I(trend^2))
f4 <- forecast(soup.fit4, h=4, level=c(80,95))
plot(f4, ylab="Units sold per week",xlab="Week", main="Forecast from Quadratic Model")
lines(fitted(soup.fit4),col="blue")
res4 <- residuals(soup.fit4)
summary(soup.fit4)
accuracy(f4)
plot(res4, main="Residuals from Quadratic Model", ylab="residuals", xlab="Week")
abline(0,0,col="gray60")
Acf(res4, main="ACF of Quadratic Model residuals")
plotForecastErrors(res4,"Histograms of Quadratic Model Residuals")

#Fitting quadratic model on validation data

soup.fitv4 <- tslm(soupvalidation~trend+I(trend^2))
fv4 <- forecast(soup.fitv4, h=4, level=c(80,95))
resv4 <- residuals(soup.fitv4)
summary(soup.fitv4)
accuracy(fv4)
plot(resv4, main="Residuals from Quadratic Model", ylab="residuals", xlab="Week")
abline(0,0,col="gray60")
Acf(res4, main="ACF of Quadratic Model residuals")



#Data Driven approach

#Smoothing using centered moving average to see trend

plot(soupstimeseries, main="Weekly Units sold",
     ylab="units#", xlab="Week")
lines(ma(soupstimeseries,8),col="red")

#Smoothing using weighted moving average

plot(soupstimeseries, main="Weekly Units sold",
     ylab="units#", xlab="Week")
lines(ma(ma(soupstimeseries,4),2),col="red")

#Simple exponential method
fit5 <- ses(souptraining, alpha=0.2, initial="optimal", h=4)
fit6 <- ses(souptraining, alpha=0.6, initial="optimal", h=4)
fit7 <- ses(souptraining, alpha=0.89,initial="optimal", h=4)
plot(fit5, plot.conf=FALSE, ylab="Units Sold per week",
     xlab="Week", main="", fcol="white", type="o")
lines(fitted(fit5), col="blue", type="o")
lines(fitted(fit6), col="red", type="o")
lines(fitted(fit7), col="green", type="o")
lines(fit5$mean, col="blue", type="o")
lines(fit6$mean, col="red", type="o")
lines(fit7$mean, col="green", type="o")
legend("topright",lty=1, col=c(1,"blue","red","green"), 
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.89)),pch=1)

accuracy(fit5)
accuracy(fit6)
accuracy(fit7)

#Fitting Simple exponential model on validation data with alpha=.2
fitv5 <- ses(soupvalidation, alpha=0.2, initial="optimal", h=4)
accuracy(fitv5)



#Holt's method
 
#On Training set
fit8 <- holt(souptraining, alpha=0.2, beta=0.2, damped=TRUE, initial="optimal", h=4) 
plot(fit8, type="o", ylab="Units sold per week", xlab="Week", 
     fcol="white", plot.conf=FALSE)
lines(fitted(fit8), col="green") 
lines(fit8$mean, col="green", type="o") 
legend("topright", lty=1, col=c("black","green"), 
       c("Data","Additive damped trend"))

accuracy(fit8)


#Holt's Winter method
#On Training set
fit9 <- HoltWinters(souptraining,alpha=0.6,beta=FALSE,gamma=FALSE)
fit9_1 <- forecast.HoltWinters(fit9, h=4)
fit10 <- HoltWinters(souptraining,alpha=0.2,beta=FALSE,gamma=FALSE)
fit10_1 <- forecast.HoltWinters(fit10, h=4)
fit11 <- HoltWinters(souptraining,alpha=0.2,beta=0.7,gamma=FALSE)
fit11_1 <- forecast.HoltWinters(fit11, h=4)
plot(fit9_1, type="o", ylab="Units sold per week", xlab="Week", fcol="white", plot.conf=FALSE)
lines(fitted(fit9_1), col="green") 
lines(fit9_1$mean, col="green", type="o") 
lines(fitted(fit10_1), col="blue") 
lines(fit10_1$mean, col="blue", type="o")
lines(fitted(fit11_1), col="red") 
lines(fit11_1$mean, col="red", type="o")

legend("topright", lty=1, col=c("black","green","blue","red"), 
       c("Data",expression(alpha == 0.6),expression(alpha == 0.2),expression(alpha == 0.6 & beta == 0.7)))
accuracy(fit9_1)
accuracy(fit10_1)
accuracy(fit11_1)

#On validation set
fitv10 <- HoltWinters(soupvalidation,alpha=0.2,beta=FALSE,gamma=FALSE)
fitv10_1 <- forecast.HoltWinters(fitv10, h=4)
accuracy(fitv10_1)



#Fitting quadratic model on complete series to forecast

soup.fitfinal <- tslm(soupstimeseries~trend+I(trend^2))
ffinal <- forecast(soup.fitfinal, h=4, level=c(80,95))
ffinal
plot(ffinal, ylab="Units sold per week",xlab="Week", main="Forecast from Quadratic Model")
lines(fitted(soup.fitfinal),col="blue")
res <- residuals(soup.fitfinal)
summary(soup.fitfinal)
accuracy(ffinal)
plot(res, main="Residuals from Quadratic Model", ylab="residuals", xlab="Week")
abline(0,0,col="gray60")
Acf(res, main="ACF of Quadratic Model residuals")
plotForecastErrors(res,"Histograms of Quadratic Model Residuals")


