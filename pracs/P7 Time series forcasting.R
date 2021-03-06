library(hflights)
library(data.table)
dt<-data.table(hflights)
dt[,date:=ISOdate(Year, Month, DayofMonth)]
daily<-dt[,list(N=.N,Delays=sum(ArrDelay,na.rm=TRUE),Cancelled=sum(Cancelled),Distance=mean(Distance)),by=date]
str(daily)
nts<-ts(daily$N,frequency = 7)
plot(nts)
#arima
auto.arima(nts)
auto.arima(nts, approximation=FALSE)
#HoltWinters
fit<-HoltWinters(nts)
plot(fit)
library(forecast)
forecast(fit)
f=forecast(HoltWinters(nts),5)
f
plot(f)
#outliers
cts<-ts(daily$Cancelled)
fit<-auto.arima(cts)
auto.arima(cts)
library(tsoutliers)
outliers<-tso(cts, tsmethod='arima', args.tsmethod=list(order=c(1,1,2)))
outliers
plot(outliers)
plot(tso(ts(daily$Cancelled)))