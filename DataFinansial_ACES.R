

library(tseries)
library(forecast)
library(readxl)
library(FinTS)
library(TSA)
library(fGarch)

#import data#
data<-read_excel("C:/Users/Nyoman Sukma/Downloads/ACES.JK.xlsx")
ace_close=data$Close

#ubah ke dalam time series plot#
close_ts<-ts(ace_close,frequency = 365)
close_ts

#plot time series harga penutupan#
plot.ts(close_ts,main="Plot Harga Penutupan Saham ACES.JK Periode 2020-2022")

#uji stasioner dalam rataan pada harga penutupan#
adf.test(close_ts) #data belum stasioner

#Tranformasi data tidak stasioner dengan menggunakan log returns dan differencing#
log_close<-diff(log(close_ts), lag=1)
adf.test(log_close) ##data sudah stasioner

#plot return 
plot.ts(log_close,main="Plot Log Return Harga Penutupan Saham ACES.JK Periode 2020-2022",xlab="Harga",ylab="Waktu")
abline(h=0,col="red")

#plot ACF dan PACF dari log return
acf(as.vector(log_close),main="Plot ACF dari Log Return Saham ACES.JK")
pacf(as.vector(log_close),main="Plot PACF dari Log Return Saham ACES.JK")

#plot ACF dan PACF dari log return yang dikuadratkan
acf(as.vector(log_close^2),main="Plot ACF dari Log Return Saham ACES.JK yang Dikuadratkan")
pacf(as.vector(log_close^2),main="Plot PACF dari Log Return Saham ACES.JK yang Dikuadratkan")

#plot ACF dan PACF dari log return yang dikuadratkan
acf(as.vector(abs(log_close)),main="Plot ACF dari Log Return Saham ACES.JK yang Dimutlakkan")
pacf(as.vector(abs(log_close)),main="Plot PACF dari Log Return Saham ACES.JK yang Dimutlakkan")

#uji efek ARCH
ArchTest(log_close) #tolak H0 (terdapat efek ARCH)

#Identifikasi model ARCH
acf(as.vector(log_close^2),main="Plot ACF dari Log Return Saham ACES.JK yang Dikudaratkan")
pacf(as.vector(log_close^2),main="Plot ACF dari Log Return Saham ACES.JK yang Dikuadratkan")
eacf(log_close^2)

arch9<-garchFit(formula=~garch(9,0), data=log_close) #aic -4.441203 #aic terkecil
summary(arch9)
arch6<-garchFit(formula=~garch(6,0), data=log_close) #aic -4.436562
summary(arch6)
arch4<-garchFit(formula=~garch(4,0), data=log_close) #aic -4.427611
summary(arch4)
arch2<-garchFit(formula=~garch(2,0), data=log_close) #aic -4.430621
summary(arch2)
arch1<-garchFit(formula=~garch(1,0), data=log_close) #aic -4.426210
summary(arch1)

garch11<-garchFit(formula=~garch(1,1), data=log_close) #aic -4.432363
summary(garch11)
garch12<-garchFit(formula=~garch(1,2), data=log_close) #aic -4.432255
summary(garch12)
garch22<-garchFit(formula=~garch(2,2), data=log_close) #aic -4.429530
summary(garch22)
garch21<-garchFit(formula=~garch(2,1), data=log_close) #aic -4.430323
summary(garch21)


#Diagnostik Model untuk ARCH(9)
#plot dari ARCH(9,0)
plot(arch9)

res.arch9<-arch9@residuals

#1_residual berdistribusi normal
#plot q-q
qqnorm(res.arch9)
qqline(res.arch9,col='red')
#statistik uji
jarque.bera.test(res.arch9) #residual tidak berditribusi normal

#2_uji kebebasan galat/autokolerasi
Box.test(res.arch9)

#peramalan
prediksi=predict(arch9,6)
prediksi
stdev_arch9=prediksi$standardDeviation
plot.ts(stdev_arch9,main="Plot Prediksi Volatilitas dengan ARCH(9)",ylab="St, Deviasi")
