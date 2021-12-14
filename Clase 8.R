library(quantmod)
library(tseries)
library(forecast)
library(car)
library(FinTS) #Para la prueba ArchTest
library(dynlm) #Para la funcion dynlm

#obtener las cotizaciones de Apple, Twitter, Tesla y BBVA, diaras, desde 2000
#hasta la fecha. Lo vamos a bajar de yahoo
getSymbols(Symbols= c("aapl", "twtr", "tsla", "bbva"), from="2000-01-01", 
           src = "yahoo")

# We merge the datasets into a new R object called prices:
prices<- merge(AAPL, TWTR, TSLA, BBVA)
# We only keep the adjusted price columns:
prices = na.omit(log(Ad(prices)))
# We rename the columns with simpler names:
names(prices)<-c("Apple", "Twitter", "Tesla", "BBVA")

#graphs of the series
chartSeries(TWTR, theme=("white"))

#Graphs of the logs of the adjusted prices
chartSeries(prices$Twitter, name="Cotizacion Twitter en logs", theme=("white"))

#Daily returns
d1.twitter<-diff(prices$Twitter)

#Graphs of the daily returns:
chartSeries(d1.twitter, name = "Rendimiento diario de Twitter", theme=("white"))

#Vamos a revisar si hay efectos ARCH:
###(proceso autoregresivo donde tenemos errores al cuadrado sobre si mismos)

#1. estimamos el modelo ARIMA
M2<-auto.arima(d1.twitter)
summary(M2)
autoplot(M2)
checkresiduals(M2)

#2. Calculamos los residuos al cuadrado:
resM2<-M2$residuals
resM2.2<-resM2*resM2
chartSeries(resM2.2, name = "Residuos al cuadrado AR(1,2) de Twitter", theme=("white"))

#3. Regresion de los residuos al cuadrado sobre ellos mismos con 1 rezago:
twitter.arch<- dynlm(resM2.2~L(resM2.2))
summary(twitter.arch)
checkresiduals(twitter.arch)

#tambien se puede aplicar directamente la prueba ArchTest:
twitter.archtest.1<-ArchTest(d1.twitter, lags = 1)
twitter.archtest.1

#Si hay efectos ARCH ya que el p-value nos dice que es estadisticamente significativo.


#Se rechaza la H0, -> sí hay efectos ARCH por la P-value.
#Significa que la volatividad o la heterosteracidad si es importante en la 
#estimacion de los modelos de twitter. 


#Verificamos con el correlograma:
par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(resM2.2, main="ACF Residuos al cuadrado")
pacf(resM2.2, main="PACF Residuos al cuadrado")

#Los residuos son ruido blanco en las gráficas, 

#Todavia hay muchos rezagos significativos. Probemos un ARCH(2)
twitter.archtest.2<-ArchTest(d1.twitter, lags = 7)
twitter.archtest.2

#Aquí los rezagos siguien siendo significativos, con un lag de 7, la volatividad
#en los precios de twitter es importante para los rendimientos de hasta 
#6 periodos hacia adelante. 

### Ahora buscaremso una mejor manera de representar o modelar la volatividad.

#Ahora vamos a probar un modelo GARCH incorporando a la ecuación la varianza de los
#residuos (ARCH + varianza de los residuos con periodos resagados)
library(rugarch)
#eliminamos el primer NA en la serie d1.apple
r.twitter<-na.omit(d1.twitter)

#especificamos el tipo de modelo ARIMA del GARCH
garch.base<-ugarchspec()
garch.base

#Primer modelo GARCH
garch.twitter1<-ugarchfit(spec=garch.base, data=r.twitter)
garch.twitter1

par(mfrow=c(1,1))
plot(garch.twitter1@fit$residuals, type ="l", col="darkblue")

#El mejor ARIMA para Twitter es un ARMA(1,2), vamos a especificar el GARCH con 
#esta caracteristica: 
garch.twitter.arma12<-ugarchspec(mean.model=list(armaOrder=c(1,2)))
garch.twitter.arma12


garch.twitter2<-ugarchfit(spec=garch.twitter.arma12, data=r.twitter)
garch.twitter2
par(mflow=c(1,1))
plot(garch.twitter2@fit$residuals, type="l", col="darkblue")

#Coeficientes del modelo
garch.twitter1@fit$matcoef

#Estimacion de la varinza del modelo:
var.garch.twitter<- garch.twitter1@fit$var

#Graficar la varinza con los residuos al cuadrado:
res.garch.twitter<-(garch.twitter1@fit$residuals)^2
plot(res.garch.twitter, type="l")
lines(var.garch.twitter, col="blue")

#Pronostico del modelo:
twitter.forecast<-ugarchforecast(garch.twitter1, n.ahead = 10)
twitter.forecast






##############BBVA################




#graphs of the series
chartSeries(BBVA, theme=("white"))

#Graphs of the logs of the adjusted prices
chartSeries(prices$BBVA, name="Cotizacion BBVA en logs", theme=("white"))

#Daily returns
d1.bbva<-diff(prices$BBVA)

#Graphs of the daily returns:
chartSeries(d1.bbva, name = "Rendimiento diario de BBVA", theme=("white"))

#Vamos a revisar si hay efectos ARCH:
###(proceso autoregresivo donde tenemos errores al cuadrado sobre si mismos)

#1. estimamos el modelo ARIMA
M3<-auto.arima(d1.bbva)
summary(M3)
autoplot(M3)
checkresiduals(M3)

###El mejor modelo es el ARIMA(2.0.3)

#2. Calculamos los residuos al cuadrado:
resM3<-M3$residuals
resM3.2<-resM3*resM3
chartSeries(resM3.2, name = "Residuos al cuadrado AR(1,2) de BBVA", theme=("white"))

#3. Regresion de los residuos al cuadrado sobre ellos mismos con 1 rezago:
bbva.arch<- dynlm(resM3.2~L(resM3.2))
summary(bbva.arch)
checkresiduals(bbva.arch)

#tambien se puede aplicar directamente la prueba ArchTest:
bbva.archtest.1<-ArchTest(d1.bbva, lags = 1)
bbva.archtest.1

#Si hay efectos ARCH ya que el p-value nos dice que es estadisticamente significativo.


#Se rechaza la H0, -> sí hay efectos ARCH por la P-value.
#Significa que la volatividad o la heterosteracidad si es importante en la 
#estimacion de los modelos de twitter. 


#Verificamos con el correlograma:
par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(resM3.2, main="ACF Residuos al cuadrado")
pacf(resM3.2, main="PACF Residuos al cuadrado")

#Los residuos no son ruido blanco en las gráficas, 

#Todavia hay muchos rezagos significativos. Probemos un ARCH(2)
bbva.archtest.2<-ArchTest(d1.bbva, lags = 2)
bbva.archtest.2

#Aquí los rezagos siguien siendo significativos, con un lag de 7, la volatividad
#en los precios de twitter es importante para los rendimientos de hasta 
#2 periodos hacia adelante. 

### Ahora buscaremso una mejormanera de representar o modelar la volatividad.

#Ahora vamos a probar un modelo GARCH incorporando a la ecuación la varianza de los
#residuos (ARCH + varianza de los residuos con periodos resagados)
library(rugarch)
#eliminamos el primer NA en la serie d1.apple
r.bbva<-na.omit(d1.bbva)

#especificamos el tipo de modelo ARIMA del GARCH
garch.base<-ugarchspec()
garch.base

#Primer modelo GARCH
garch.bbva1<-ugarchfit(spec=garch.base, data=r.bbva)
garch.bbva1

par(mfrow=c(1,1))
plot(garch.bbva1@fit$residuals, type ="l", col="darkblue")

#El mejor ARIMA para BBVA es un ARMA(1,1), vamos a especificar el GARCH con 
#esta caracteristica: 
garch.bbva.arma12<-ugarchspec(mean.model=list(armaOrder=c(1,1)))
garch.bbva.arma12


garch.bbva2<-ugarchfit(spec=garch.bbva.arma12, data=r.bbva)
garch.bbva2
par(mflow=c(1,1))
plot(garch.twitter2@fit$residuals, type="l", col="darkblue")

#Coeficientes del modelo
garch.bbva1@fit$matcoef

#Estimacion de la varinza del modelo:
var.garch.bbva<- garch.bbva1@fit$var

#Graficar la varinza con los residuos al cuadrado:
res.garch.bbva<-(garch.bbva1@fit$residuals)^2
plot(res.garch.bbva, type="l")
lines(var.garch.bbva, col="blue")

#Pronostico del modelo:
bbva.forecast<-ugarchforecast(garch.bbva1, n.ahead = 10)
bbva.forecast



