library(tseries)
library(forecast)
library(car)

#Let's try with CO2 emissions, inflation, exports, external debt and GDP per capita...
#download data from World's Bank web site:
#https://datos.bancomundial.org/

library(wbstats)

emisiones<-wb_data(indicator = c("EN.ATM.CO2E.PC", "NY.GDP.PCAP.CD", "FP.CPI.TOTL.ZG", 
                                 "TX.VAL.MRCH.CD.WT", "DT.DOD.DECT.CD"), 
                   country="MEX", start_date = 1960, end_date = 2020)
co2<-log(emisiones$EN.ATM.CO2E.PC)
gdp<-log(emisiones$NY.GDP.PCAP.CD)
inf<-log(emisiones$FP.CPI.TOTL.ZG)
expor<-log(emisiones$TX.VAL.MRCH.CD.WT)
deuda<-log(emisiones$DT.DOD.DECT.CD)

#Comenzamos a trabajar con las emisiones de CO2
co2ts<-ts(co2, start = c(1960))
autoplot(co2ts)
par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(na.omit(co2ts))
pacf(na.omit(co2ts))
adf.test(na.omit(co2ts))
#La serie no es estacionaria. Procedemos a las primeras diferencias:
d1.co2<-diff(na.omit(co2ts))
autoplot(d1.co2)
par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(d1.co2)
pacf(d1.co2)
adf.test(na.omit(co2ts))

adf.test(na.omit(co2ts))
#La serie no es estacionaria. Procedemos a las primeras diferencias:
d1.co2<-diff(na.omit(co2ts))
autoplot(d1.co2)
par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(d1.co2)
pacf(d1.co2)
adf.test(d1.co2)
pp.test(d1.co2)

#la serie parece ser estacionaria en primeras diferencias
#el correlograma muestra que las correlaciones con los rezagos no son significativos
#Vamos a revisar con el algoritmo de R cual es el mejor modelo que propone:
M1<-auto.arima(na.omit(co2ts))
summary(M1)

#El algoritmo de R arroja que el mejor modelo es un IMA(2,1), es decir
#Un modelo de promedio movil de orden 1 con dos diferencias.
#vamos a realizar las pruebas formales:

tsdiag(M1)
#Prueba Ljung-Box:
Box.test(M1$residuals, type = "Ljung-Box")
#La hipotesis nula es que los residuos son ruido blanco -> El modelo ARIMA tiene
#buen ajuste". En este caso, el p-value es de 0.209 > 0.05
#por lo tanto, se acepta la Ho -> los residuos del modelo ARIMA son
#ruido blanco -> El modelo ARIMA hace un trabajo para explicar el 
#comportamiento de las emisiones de CO2 en Mexico
 
par(mfrow=c(1,1))
autoplot(M1$residuals)
hist(M1$residuals, nclass = 20, col = "darkred", main="Histograma de los residuos")

#realicemos ahora los pronosticos:
pronostico.co2<-forecast(M1, h=4)
pronostico.co2
autoplot(pronostico.co2)


################################################################################
#Analisis del PIB per capita:
gdpts<-ts(gdp, start = c(1960))
autoplot(gdpts)
par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(na.omit(gdpts))
pacf(na.omit(gdpts))
adf.test(na.omit(gdpts))
#La serie no es estacionaria. Procedemos a las primeras diferencias:
d1.gdp<-diff(na.omit(gdpts))
autoplot(d1.gdp)
par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(d1.gdp)
pacf(d1.gdp)
adf.test(d1.gdp)
pp.test(d1.gdp)

#la serie parece ser estacionaria en primeras diferencias
#el correlograma muestra que las correlaciones con los rezagos no son significativos
#Vamos a revisar con el algoritmo de R cual es el mejor modelo que propone:
M2<-auto.arima(na.omit(gdpts))
summary(M2)
#AR(2) -> depende de los valores del pibpc de dos periodos anteriores
#I(1) -> Estacionaria en primeras diferencias
#MA(2) -> Depende de los errores de dos periodos anteriores 
#Tambien tiene una deriva que es significativa (drift)

tsdiag(M2)
#Prueba Ljung-Box:
Box.test(M2$residuals, type = "Ljung-Box")
#La hipotesis nula es que los residuos son ruido blanco -> El modelo ARIMA tiene
#buen ajuste. En este caso, se acepta la Ho

par(mfrow=c(1,1))
autoplot(M2$residuals)
hist(M2$residuals, nclass = 20, col = "darkred", main="Histograma de los residuos")

#realicemos ahora los pronosticos:
pronostico.gdp<-forecast(M2, h=4)
autoplot(pronostico.gdp)  
print(pronostico.gdp)
