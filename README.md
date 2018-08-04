# R-stock-prediction
 library(forecast)



 library(fpp)



 Stock=read.table("C:/Users/SHIVAM-DELL/Downloads/r-stockPrediction-master/input/ABC.csv", sep=",", header=TRUE)

 tsStock = ts(Stock$Close, start = c(2000,1),frequency = 12)

 tsStock

 plot(tsStock)





 t1 = seq(2000,2013,length=length(tsStock))

 t12 = t1^7

 polystock = lm(tsStock ~ t1 + t12)



 tsStocktrend1=ts(polystock$fit,start = c(2000,1), frequency = 12)



 plot(tsStock,lw=2,col="blue",xlim=c(2000,2013))



 lines(tsStocktrend1,lw=2,col="red")



 abline(v=2013.25,lty=3)



 stlStock = stl(tsStock,s.window = "periodic")



 plot(stlStock)



 plot(stlStock,col="blue",lw=2)



 tsStocktrend2 = stlStock$time.series[,2]



 plot(forecast(stlStock))





 abline(v=2013.25,lty= 3)

 

 

 plot(tsStock,lw=3)



 lines(tsStocktrend1, col='purple',lw=2)



 lines(tsStocktrend2, col='red',lw=2)



 abline(v=2013.25, lty= 3)





**** legend("bottomleft",legend = c(" "), col=c(" "))





 legend("bottomleft",legend = c("Actual Function","STL Trend","Polynomial Trend"), col=c("black","red","purple"), lw=2)



 HWStock1_ng = HoltWinters(tsStocktrend1, gamma = FALSE)



 HWStock1= HoltWinters(tsStocktrend1)



 NETfit1 <- nnetar(tsStocktrend1)



 autofit1=auto.arima(tsStocktrend1)







**fit12 <- arima(tsStocktrend1,order=c(1,0,0),seasonal=list(order=c(2,1,0),period=12),xreg=1:length(tsStocktrend1),method="CSS")







 fitl1 <- tslm(tsStocktrend1 ~ trend + season, lambda = 0)



 stlStock1=stl(tsStocktrend1, s.window = "periodic")



 plot(forecast(autofit1, h=24),xlim=c(2000,2015.2),ylim=c(-50,100),lw=2,col="red",xlab="Time",ylab="Stock Price", main="Predictions of the polynomial Trend")





 lines(forecast(stlStock1, h=24)$mean,col="red",lw=2)



 lines(tsStock, lw=3)



 lines(forecast(fitl1, h=24)$mean,col="orange")



 lines(forecast(NETfit1,h=24)$mean,lw=3,lty="longdash",col="brown")



 lines(predict(HWStock1_ng,n.ahead = 24),lw=2,col="green")



 lines(forecast(fit12,h=24)$mean, lw=2, col="purple")





 lines(predict(HWStock1,n.ahead=24, prediction.interval =  T, level = 0.95)[,1],lw=2, col="green")

 lines(predict(HWStock1,n.ahead=24, prediction.interval =  T, level = 0.95)[,2],lw=2, col="green")

 lines(predict(HWStock1,n.ahead=24, prediction.interval =  T, level = 0.95)[,3],lw=2, col="green")



 legend("bottomleft",legend = c("Actual Function","Polynomial Trend","Prediction-Holt Winters","Prediction -Arima(auto)", "Prediction-Arima(fixed)","Prediction-Neutral nets","Prediction-Linear Model"), col=c("black","red","green","blue","purple","brown","orange"), lw=2)



 abline(v=2013.25, lty=3)







--------------------------------------------------------------

HWStock2_ng = HoltWinters(tsSocktrend2, gamma=FALSE)

HWStock2 - HoltWinters(tsStocktrend2)

NETfit2 <- nnetar(tsStocktrend2)

autofit2 = auto.arima(tsStocktrend2)



fit2 <- Arima(tsStocktrend2, order=c(15,3,3))

fit12 <- tslm(tsStocktrend2  ~ trend + season, lambda=0)

stlStock = stl(tsStocktrend1,s.window"periodic")



plot(forecast(autofit2,h=24),xlim=c(2000,2015.2),ylim=c(-50,100),lw=2,col="blue",xlab="Time",ylab="Stock Price",main="Prediction of STL Trend")



lines(tsStock, lw=3)

lines(forecast(stlStock2, h=24)$mean,col="red",lw=2)

lines(forecast(fit12, h=24)$mean,col="orange")

lines(forecast(fit2,h=24)$mean, lw=2 , col="purple")



lines(tsStocktrend2, lw=2, col="red")

lines(forecast(NETfit2,h=24)$mean,lw=3,lty="longdash",col="brown")

lines(predict(HWStock2,n,ahead=24),lw=2,col="green")

lines(predict(HWStock2_ng,n,ahead=24),lw=2,col="green")

lines(predict(HWStock2,n,ahead =24 , prediction,interval =T , level = 0.95 )[,2],col="orange")

lines(predict(HWStock2,n,ahead =24 , prediction,interval =T , level = 0.95)[,3],col="orange ")

legend("bottomleft", legend=c("actual function","STL Trend","Prediction=Holt Winters", "Prediction =Arima(auto)","Prediction=Arima(fixed)","Prediction=Neutral Nets", "Prediction=Linear Model"),col=c("black","red","green","blue","purple","brown","orange"),lw=2)

abline(v=2013.25,lty=3)



HWStockr_ng=HoltWinters(tsStock,gamma=FALSE)

HWStockr=HoltWinters(tsStock)

NETfitr <- nnetr(tsStock)

autofitr =auto.arima(tsStock)

fitr <- Arima(tsStock, order=c(15,3,3))

fit <-arima(tsStock, order=c(1,0,0), list(order=c(2,1,0),period=12))

fitlr <-tslm(tsStock ~ trend +season, lambda=0)

stlStockr = stl(tsSock,s.window="periodic")

plot(forecast(autofitr,h=24),xlim=c(2000,2015.2),ylim=c(=50,100),l==2,col="blue",xlab="Time",ylab="Stock Price",main="prediction of the actual model ")

lines(forecast(fitlr,h=24)$mean,col="orange")

lines(forecast(stlStockr, h=24)$mean,col="red",lw=2)

lines(forecast(fitr,h=24)$mean, lw=2, col="purple")

lines(forecast(fitr2,h=24)$mean, lw=2, col="purple")

lines(tsStock, lw=3)

lines(forecast(NETfitr,h=24)$mean ,lw=3,lty= "longdash",col="brown");

lines(predict(HWStockr,n.ahead=24),lw=2,col="green")

abline(v=2013.25,lty=3)

legend("bottomleft",legend=c("Actual Functions ","Prediction = HoltWinters","Prediction - Arima (auto)","Prediction - Arima (fixed)","Prediction - Neural Nets","Prediction - Linear model"),col=c("black","green","blue","purple","brown","orange",),lw=2)





#Add all lines if curious

plot(forecast(autofitr,h=24),lw=2,xlis=c(2000,2015,2),ylim=c(-50,100) , col="blue",xlab="time", ylab="Stock Price", main= "All 24 Predictions competing"



lines(forecast(fitr, h=24)$mean, lw=2, col="purple")

lines(forecast(fitr2, h=24)$mean, lw=2, col="purple")

lines(tsStock,lw=3)

lines(forecast(NETfitr,h=24)$mean,lw=3,lty= "longdash", col ="brown");

lines(predict(HWstockr,n.ahead=24),lw=2,col="green")

lines(predict(HWstockr_ng,n.ahead=24),lw=2,col="green")

lines(forecast(autofit2,h=24)$mean,lw=2,col="blue")

lines(forecast(fit12,h=24)$mean,lw=2,col="purple")

lines(tsStock,lw=3)

lines(forecast(stlStock1,h=24) $mean,col="yellow", lw=3)

lines(forecast(stlStock2,h=24) $mean,col="yellow", lw=3)

lines(forecast(stlStockr,h=24) $mean,col="yellow", lw=3)

lines(forecast(fit2,h=24)$mean, lw=2,col="purple")



lines(tsStocktrend2, lw=2,col="red")

lines(tsStocktrend1, lw=2,col="red")

lines(forecast(NETfit2,h=24)$mean,lw=3,lty= "longdash",col= "brown")

lines(predict(HWStock2,n.ahead=24),lw=2,col="green")

lines(predict(HWStock2_ng,n.ahead=24),lw=2,col="green")

lines(forecast(autofit1,h=24)$mean,lw=2,col="blue")

lines(forecast(fitlr,h=24)$mean,lw=2,col="orange")

lines(forecast(fitl1,h=24)$mean,lw=2,col="orange")

lines(forecast(fitl2,h=24)$mean,lw=2,col="orange")

lines(tsStock,lw=3)

lines(forecast(NETfit1,h=24)$mean,lw=3,lty="longdash",col ="brown")

lines(predict(HWStock1_ng,n.ahead=24),lw=2,col="green")

lines(predict(HWStock1,n.ahead=24,prediction,interval=T, level=0.95)[.1],lw=2,col="green")

lines(predict(HWStock1,n.ahead=24,prediction,interval=T, level=0.95)[.2],col="green")

lines(predict(HWStock1,n.ahead=24,prediction,interval=T, level=0.95)[.3],col="green")

legend("bottomleft",legend=c("Actual Function","Prediction=Holt winters","Prediction=Arima(auto)","Prediction = Arima (fixed)","Prediction = Neural Nets","Prediction= Linear Model"),col=c("black","green","blue","purple","brown","orange"),lw=2)

abline(v=2013.25,lty=3) 
