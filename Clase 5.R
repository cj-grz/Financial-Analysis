library(tseries)
library(forecast)
library(car)



library(readxl)
pib <- read_excel("~/Desktop/Tec/SeriesT/pib.xls")
View(pib)

#Convertir a Series de Tiempo
pibts <- ts(pib$pib, start= c(1993,1), end = c(2020,4), frequency = 4)
autoplot(pibts)

lpib <-log(pibts)
lpib
autoplot(lpib)

adf.test(lpib)
acf(lpib)
pp.test(lpib)
#Aqui se comprueba que la serie no es estacionaria

#Cuantas diferencias necesitamos?
ndiffs(lpib)

#Calculamos la primera diferencia
d1.pib<-diff(lpib)
autoplot(d1.pib, )
adf.test(d1.pib)
acf(d1.pib)
pp.test(d1.pib)

#El pib es estacionario en primeras diferencias
#Ahora ya se puede hacer el modelo ARIMA

par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
#correlograma
acf(d1.pib)
pacf(d1.pib)

#Primera propuesta del modelo ARIMA
M1<-arima(lpib, order=c(2,1,1))
summary(M1)
tsdiag(M1)

#Prueba Ljung-Box
Box.test(M1$residuals, type = "Ljung-Box")

#La hipotesis nula es que los residuos son ruidos blancos -> El modelo ARIMA 
#tiene buen ajuste, es este caso se acepta la H0

#Segunda propuesta del modelo Arima
M2 <- arima(lpib, order = c(3,1,1))
summary(M2)
tsdiag(M2)
#Prueba Ljung-Box
Box.test(M2$residuals, type = "Ljung-Box")

#Podríamos haber hecho un modelo ARMA(2,1) -> sobre la serie d1.pib
#La hipotesis nula es que los residuos son ruido blanco -> El modelo ARIMA tiene
#buen ajuste"
#En este caso, se acepta la Ho
#El modelo es un ARIMA(3,1,1) y significa que el lpbib depende de sun valor de hasta 3 periodos anteriores y
#del error de pronostico de 1 pariodo anterior

#¿Cómo puedo determinar o encontrar el mejor ARIMA para el lpib?

M3<-auto.arima(lpib)
summary(M3)
tsdiag(M3)
#Prueba Ljung-Box
Box.test(M3$residuals, type = "Ljung-Box")
#LA Ho es que los residuos son ruido blanco, se busca aceptar y por la Ho se acepta

autoplot(M3$residuals)
hist(M3$residuals, nclass = 20, col="darkred", main="Histograma de los residuos")

pronostico<-forecast(M3, h=4)
autoplot(pronostico)
pronostico
