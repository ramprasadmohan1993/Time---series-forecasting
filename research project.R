getwd()
setwd("D:/R_wd")

pacman::p_load(readxl,ggplot2,forecast,tseries,caret,lattice,ggfortify,plotly,MLmetrics)
library(zoo)
library(xts)

BSE <- read_excel(file.choose())

BSE <- as.data.frame(BSE)

str(BSE)

tsBSE<-ts(BSE$Sales,start = c(2015,1,1),frequency = 365.25)

class(tsBSE)

BSE$Sales <- as.Date(BSE$Sales)
Q <- zoo(BSE$Sales)
window(Q, start = as.Date('2015-01-01'), end=as.Date('2016-31-12'))


autoplot(tsBSE,xlab = "Year",ylab = "Sensex index",ts.colour = "blue",main="BSE Sensex Jan'2009 - Jun'2020",ts.size = 1)

tseries::pp.test(tsBSE)

  ## test stationarity ####

adf.test(tsBSE)
tseries::adf.test(tsBSE)
# Phillips-Perron Test 
pp.test(tsBSE,alternative="stationary")

### DEcompose all plot #####

plot(stl(tsBSE,s.window = "periodic"),col='blue')
monthplot(tsBSE,col = 'red',main="Monthly beakup sparkline plot",ylab="BSE - Sensex points")
seasonplot(tsBSE,col=c("red","blue","green","grey","pink","orange","gold3","magenta3","turquoise2","midnightblue","navajowhite2","red2"),year.labels = TRUE)


decmp<- decompose(tsBSE)
autoplot(object = decmp,ts.colour = "Blue",ts.size = 1)

acf(tsBSE,main="ACF Plot")
pacf(tsBSE,main="PACF Plot")
tsmodel <- auto.arima(tsBSE,ic = "aic",trace = TRUE)
summary(tsmodel)

acf(ts(tsmodel$residuals),main="Residual ACF plot")
pacf(ts(tsmodel$residuals),main="Residual PACF plot")

checkresiduals(tsmodel$residuals,plot = TRUE,col = "red")
checkre

adf.test(ts(tsmodel$residuals))

qqnorm(tsmodel$residuals,col="Red")
qqline(tsmodel$residuals,col="Blue")

## Forecasting after changing stationarity ##
myforecast1 <- forecast(tsmodel,h = 12,level = c(95))
myforecast2 <- forecast(tsmodel,h = 60,level = c(95))
myforecast3 <- forecast(tsmodel,h = 120,level = c(95))
myforecast4 <- forecast(tsmodel,h = 6,level = c(95))

print(myforecast1)
summary(myforecast1)
plot(myforecast)
autoplot(myforecast1,predict.colour = "red",predict.size = 1,predict.linetype = 5,conf.int.fill = "darkolivegreen2",colour="Blue")
autoplot(myforecast2,predict.colour = "red",predict.size = 1,predict.linetype = 5,conf.int.fill = "darkolivegreen2",colour="Blue")
autoplot(myforecast3,predict.colour = "red",predict.size = 1,predict.linetype = 5,conf.int.fill = "darkolivegreen2",colour="Blue")

autoplot(myforecast4,predict.colour = "red",predict.size = 1,predict.linetype = 5,conf.int.fill = "darkolivegreen2",colour="Blue")

class(myforecast1$x)
class(myforecast1$fitted)

y <- c(1:50)

for(i in y)
{
x <- Box.test(myforecast1$residuals, lag = i, type = "Ljung-Box")
out[i] <- x$p.value
}

pval <- as.data.frame(out)
lagng <- c(1:50)
ljung <- cbind(lagng,pval)

plot(ljung,xlab="Lag",ylab="p-value",main="p - values for Ljung Box statistics",col="red")
abline(h = 0.4,lty=2,col="blue")


### merging actual and predicted data ---------####

mm <- as.data.frame(myforecast1$x)
ss <- as.data.frame(myforecast1$fitted)

final <- cbind(mm,ss)
names(final)[1] <- "Actual"
names(final)[2] <- "Predicted"
names(final)

### backtrack ###

ts.plot(final,col=c("blue","red"),lty=c(1,2))
legend("topleft",legend = c("Actual","Predicted"),col = c("blue","red"),lty=1:2 )

accuracy(tsmodel)

### ------- Comparing the latest forecast with the actual sensex points till september ----#####

myforecast1$fitted
myforecast1$mean


forecast::accuracy(tsmodel)

