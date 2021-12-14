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
chartSeries(AAPL, theme=("white"))
chartSeries(TWTR, theme=("white"))
chartSeries(TSLA, theme=("white"))
chartSeries(BBVA, theme=("white"))

#Graphs of the logs of the adjusted prices
chartSeries(prices$Apple, name="Cotizacion Apple en logs",theme=("white"))
chartSeries(prices$Twitter, name="Cotizacion Twitter en logs", theme=("white"))
chartSeries(prices$Tesla, name = "Cotizacion Tesla en logs", theme=("white"))
chartSeries(prices$BVA, name = "Cotizacion BBVA en logs", theme=("white"))

#Daily returns
d1.apple<-diff(prices$Apple)
d1.twitter<-diff(prices$Twitter)
d1.tesla<-diff(prices$Tesla)
d1.bbva<-diff(prices$BVA)



#Apple
#Vamos a revisar si hay efectos ARCH:
#1. estimamos el modelo ARIMA
M1<-auto.arima(d1.apple)
summary(M1)
autoplot(M1)
checkresiduals(M1)

#2. Calculamos los residuos al cuadrado:
resM1<-M1$residuals
resM1.2<-resM1*resM1
chartSeries(resM1.2, name = "Residuos al cuadrado AR(1) de Apple", theme=("white"))

#3. Regresion de los residuos al cuadrado sobre ellos mismos con 1 rezago:
apple.arch<- dynlm(resM1.2~L(resM1.2))
summary(apple.arch)
checkresiduals(apple.arch)

#La conclusión de este análisis es que si hay efectos ARCH(1)

#Vamos a realizar la prueba ARCH de otra manera:

#tambien se puede aplicar directamente la prueba ArchTest:
apple.archtest.1<-ArchTest(d1.apple, lags = 1)
apple.archtest.1

#La Ho: es que no hay efectos ARCH. Con los resultados de la prueba de
#LM (Multiplicador de Lagrange) se rechaza la Ho,
#Por lo que sí hay efectos ARCH de primer orden.

#Verificamos con el correlograma:
par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(resM1.2, main="ACF Residuos al cuadrado")
pacf(resM1.2, main="PACF Residuos al cuadrado")

#Todavia hay muchos rezagos significativos. Probemos un ARCH(2)

apple.archtest.2<-ArchTest(d1.apple, lags = 2)
apple.archtest.2




#Analisis de Twitter...
#1. estimamos el modelo ARIMA
M2<-auto.arima(d1.twitter)
summary(M2)
autoplot(M2)
checkresiduals(M2)

#2. Calculamos los residuos al cuadrado:
resM2<-M2$residuals
resM2.2<-resM2*resM2
chartSeries(resM2.2, name = "Residuos al cuadrado AR(2) de Twitter", theme=("white"))

#3. Regresion de los residuos al cuadrado sobre ellos mismos con 1 rezago:
twitter.arch<- dynlm(resM2.2~L(resM2.2))
summary(twitter.arch)
checkresiduals(twitter.arch)

#tambien se puede aplicar directamente la prueba ArchTest:
twitter.archtest.1<-ArchTest(d1.twitter, lags = 1)
twitter.archtest.1

#Si hay efectos ARCH
#Verificamos con el correlograma:

par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(resM2.2, main="ACF Residuos al cuadrado")
pacf(resM2.2, main="PACF Residuos al cuadrado")

#Los residuos al cuadrado parecen ruido blanco. De todas maneras,
# probemos un ARCH(2)

twitter.archtest.2<-ArchTest(d1.twitter, lags = 2)
twitter.archtest.2

###########################################################
#Análisis para Tesla y BBVA
###########################################################


#Analisis de Tesla...
#1. estimamos el modelo ARIMA
M3<-auto.arima(d1.tesla)
summary(M3)
autoplot(M3)
checkresiduals(M3)

#2. Calculamos los residuos al cuadrado:
resM3<-M3$residuals
resM3.2<-resM3*resM3
chartSeries(resM3.2, name = "Residuos al cuadrado AR(2) de Tesla", theme=("white"))

#3. Regresion de los residuos al cuadrado sobre ellos mismos con 1 rezago:
tesla.arch<- dynlm(resM3.2~L(resM3.2))
summary(tesla.arch)
checkresiduals(tesla.arch)

#tambien se puede aplicar directamente la prueba ArchTest:
tesla.archtest.1<-ArchTest(d1.tesla, lags = 1)
tesla.archtest.1

#No hay efectos ARCH
#Verificamos con el correlograma:

par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(resM2.2, main="ACF Residuos al cuadrado")
pacf(resM2.2, main="PACF Residuos al cuadrado")

#Los residuos al cuadrado parecen ruido blanco. De todas maneras,
# probemos un ARCH(2)

tesla.archtest.2<-ArchTest(d1.tesla, lags = 2)
tesla.archtest.2

#Aqui se comprueba que hay efectos de ARCH en tesla ya que el p-value
#permite rechazar la hipotesis nula 





#Analisis de BBVA...
#1. estimamos el modelo ARIMA
M4<-auto.arima(d1.bbva)
summary(M4)
autoplot(M4)
checkresiduals(M4)

#2. Calculamos los residuos al cuadrado:
resM4<-M4$residuals
resM4.2<-resM4*resM4
chartSeries(resM4.2, name = "Residuos al cuadrado AR(2) de BBVA", theme=("white"))

#3. Regresion de los residuos al cuadrado sobre ellos mismos con 1 rezago:
bbva.arch<- dynlm(resM4.2~L(resM4.2))
summary(bbva.arch)
checkresiduals(bbva.arch)

#tambien se puede aplicar directamente la prueba ArchTest:
bbva.archtest.1<-ArchTest(d1.bbva, lags = 1)
bbva.archtest.1

#Si hay efectos ARCH
#Verificamos con el correlograma:

par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(resM2.2, main="ACF Residuos al cuadrado")
pacf(resM2.2, main="PACF Residuos al cuadrado")

#Los residuos al cuadrado parecen ruido blanco. De todas maneras,
# probemos un ARCH(2)

bbva.archtest.2<-ArchTest(d1.bbva, lags = 2)
bbva.archtest.2

#Con la prueba de archt, sale que bbva tiene efectos ARCH ya que se rechaza la
#Ho

