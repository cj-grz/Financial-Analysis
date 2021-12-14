library(quantmod)
library(tseries)
library(forecast)
library(car)

#Obtener el tipo de cambio, el indice de la BMV y el indice de la S&P por mes
#desde 2004 hasta la fecha de yahoo.

getSymbols(Symbols= c("USDMXN=X", "^MXX", "^GSPC"), from="2004-01-01", 
           periodicity = "monthly", src = "yahoo")

#Cambiamos el nombre de la serie 
TC<-`USDMXN=X`

#We merge the datasets into a new R object called prices.
prices<- merge (TC,MXX,GSPC)
#Hizo una nueva tabla

#We only keep the adjusted price column 
prices = Ad(prices)

#We rename de columns with simpler names
names(prices)<-c("TC","MXX", "GSPC")

#graphs of the series
chartSeries(MXX, theme=("white"))
chartSeries(TC, theme=("white"))
chartSeries(GSPC, theme=("white"))

#logarithmic transformation
lnprices=log(prices)

#now do a time plot for a natural log price of the MXX
plot(lnprices$MXX, main = "Log of the Mexican Index over time")
plot(lnprices$TC, main = "Log of the peso/dolar exchange rate over time")
plot(lnprices$GSPC, main = "Log of the S&P index over time")
#Solo grafíca cada logaritmo

#stacionarety tests:
ndiffs(lnprices$TC)
ndiffs(lnprices$MXX)
ndiffs(lnprices$GSPC)

#define the three series as ts:
TCts<-ts(lnprices$TC, start=c(2004,1), frequency = 12)
MXXts<-ts(lnprices$MXX, start=c(2004,1), frequency = 12)
GSPCts<-ts(lnprices$GSPC, start=c(2004,1), frequency = 12)

#the three series are I(1)
#Does the exchange rate explain the MXX behavor?
M1<-lm(MXXts~TCts)
summary(M1)

#residuals series;
residuosm1<-na.omit(M1$residuals)
residualPlot(M1)
plot(residuosm1)

acf(residuosm1)
adf.test(residuosm1)

 #Los resultados de la regresión no son estacionarios -> las
#series no estan cointegradas -> regresión espuria

#Do the S&P index and exchange rate influence in the MXX Behavior?
M3<-lm(MXXts~GSPCts+TCts)
summary(M3)

#Residual series
residuosm3<-na.omit(M3$residuals)
residualPlot(M3)
plot(residuosm3)
acf(residuosm3)
adf.test(residuosm3)

#Los residuos de la regresión no son estacionarios ->
#las series no esta cointegradas -> regresión espuria
