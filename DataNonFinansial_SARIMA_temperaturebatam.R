

library(lubridate)
library(readxl)
library(tseries)
library(TSA)
library(forecast)
library(lmtest)
library(MASS)

#Import data#
data<-read_excel("C:/Users/Nyoman Sukma/Downloads/Rata-Rata Suhu Hang Nadim-Batam.xlsx")
suhu<-data$suhu

#ubah ke dalam time series plot#
suhu_ts<-ts(suhu,start=c(2017,1),frequency = 12)
suhu_ts

#plot suhu#
plot(suhu_ts,col='Blue',main="Rata-Rata Suhu di Hang Nadim-Kota Batam Tahun 2017-2022",xlab="Tahun",ylab="suhu")
points(y=suhu_ts,x=time(suhu_ts),pch=as.vector(season(suhu_ts)))

#plot ACF dan PACF#
acf(as.vector(suhu_ts),main="Plot ACF Rata-Rata Suhu di Hang Nadim-Kota Batam Tahun 2017-2022")
pacf(as.vector(suhu_ts),main="Plot PACF Rata-Rata Suhu di Hang Nadim-Kota Batam Tahun 2017-2022")

#uji stasioner dalam varians#
u1=boxcox(lm(suhu_ts~1))
(lambda1<-u1$x[which.max(u1$y)])
u1$x[u1$y>max(u1$y)-1/2*qchisq(.95,1)]

#uji stasioner dalam rataan#
adf.test(suhu_ts)

#karena data tidak stasioner dan terdapat indikasi musiman pada lag ke-12 pada plot ACF
#Differencing data musiman orde 1#
suhu_musiman=diff(suhu,differences=1,lag=12)
plot.ts(suhu_musiman,main="Plot Data Musiman Setelah di D=1 ") 
adf.test(suhu_musiman) #data musiman tidak stasioner

#Differencing data non-musiman order 1#
suhu_non_musiman=diff(suhu_musiman,differences = 1)
plot.ts(suhu_non_musiman,main="Plot Data Non-Musiman setelah di d=1")
adf.test(suhu_non_musiman) #data non-musiman sudah stasiioner

#Plot ACF dan PACF#
acf(as.vector(suhu_non_musiman),lag.max=48,main="Plot ACF Data Non-Musiman")
pacf(as.vector(suhu_non_musiman),lag.max=48,main="Plot PACF Data Non-Musiman")
eacf(suhu_non_musiman)

#Indentifikasi model SARIMA (p,d,q)x(P,D,Q)_12#
#SARIMA (1,1,1)x(0,1,1)_12
#SARIMA (0,1,1)x(0,1,1)_12
#SARIMA (2,1,2)x(0,1,1)_12
#SARIMA (3,1,1)x(0,1,1)_12
Sarima.1=Arima(suhu,order = c(1,1,1),seasonal = list(order=c(0,1,1),period=12))
summary(Sarima.1)
Sarima.2=Arima(suhu,order = c(0,1,1),seasonal = list(order=c(0,1,1),period=12))
summary(Sarima.2)
Sarima.3=Arima(suhu,order = c(2,1,2),seasonal = list(order=c(0,1,1),period=12))
summary(Sarima.3)
Sarima.4=Arima(suhu,order = c(3,1,1),seasonal = list(order=c(0,1,1),period=12))
summary(Sarima.4)
#Menampilkan List AIC Dari ke-12 Model#
Sarima.1$aic
Sarima.2$aic #aic terkecil
Sarima.3$aic
Sarima.4$aic

Sarima.aic=c(Sarima.1$aic,Sarima.2$aic,Sarima.3$aic,Sarima.4$aic)
Model.Sarima=c("SARIMA (1,1,1)x(0,1,1)_12","SARIMA (0,1,1)x(0,1,1)_12",
               "SARIMA (2,1,2)x(0,1,1)_12","SARIMA (3,1,1)x(0,1,1)_12")
List.Sarima=cbind(Sarima.aic,Model.Sarima)
List.Sarima=data.frame(List.Sarima)
List.Sarima

#Diagnostik Model Untuk Sarima.2 (SARIMA(0,1,1)x(0,1,1)_12)#
#1_Uji kesignifikanan parameter : uji t
coeftest(Sarima.2) #Parameter sudah signifikan

#plot sisaan (residual)
res<-residuals(Sarima.2)
plot(res,main="Plot Residual")
abline(h=0,col='red')
#plot residual terstandarkan
res.std<-rstandard(Sarima.2)
plot(res.std, main="Plot Residual Standar")
abline(h=0,col='red')

#2_Residual berdistribusi normal
#plot q-q
qqnorm(res)
qqline(res,col='red')
#uji statistik
jarque.bera.test(res)

#3_uji asumsi kebebasan galat/kebebasan autokorelasi residual pada model
#plot acf
acf(res)
#uji statistik dengan uji Ljung-box
Box.test(res,type="Ljung")

#Peramalan#
#Hasil Prediksi Untuk 12 Bulan Ke Depan
prediksi=forecast(Sarima.2,12)
prediksi
plot(prediksi,ylab="Suhu",col=1:2,xlab = "Data",main="Plot Prediksi dengan SARIMA(0,1,1)x(0,1,1)_12")

#Perbandingan Plot Asli dengan Plot Peramalan
data_suhu<-data$suhu
fit.data=fitted(Sarima.2)
ts.plot(data_suhu,ylab="suhu",xlab="Data",main="Perbandingan Plot Asli dengan Plot Peramalan")
lines(fit.data,col='red')

#Model Matematis dari Sarima.2(SARIMA (0,1,1)x(0,1,1)_12)#
summary(Sarima.2)
#persamaan Sarima : (1-B)(1-B^12)y_t=(1-0.7291B)(1-0.9997B^12)e_t
#dengan MAPE 1.164607