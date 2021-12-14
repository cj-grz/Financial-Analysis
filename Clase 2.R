#importar librería
library(ggfortify)
library(forecast)
library(tseries)
library(readxl)

#Importar los datos
inpc <- read_excel("~/Desktop/Tec/SeriesT/inpc.xls")

#Convertir a serie de tiempo
inpcts <-  ts(inpc$inpc, start=c(1982.1), frequency = 12)
print(inpcts)

autoplot(inpcts, colour="red", linetype="dashed")

#Descomponer la serie de tiempo en sus componentes estructurales:
#tendencia + estacionalidad + residuos
autoplot(stl(inpcts, s.window="periodic"))

#descomposición de la inflación por mes
boxplot(inpcts~cycle(inpcts))





#Transformación logarítmica de la serie:
linpc <- log(inpcts)
autoplot(linpc, colour="red", linetype="dashed")

#Eliminación de la tendencia:
d1.inpc <- diff(linpc)
autoplot(d1.inpc, colour="blue", linetype="dashed")

#Ya es estacionaria?
#una forma de probar es usando la función de autocorrelación (acf)
acf(d1.inpc)

#¿Cuántas veces debemos de diferenciar una serie para que sea estacionaria?
#prueba para ver 
ndiffs(linpc)
d2.d1.inpc <-diff(d1.inpc)
autoplot(d2.d1.inpc, colour="blue", linetype="dashed")
acf (d2.d1.inpc)

#¿Es relevante la estacionariedad?
nsdiffs(linpc)


d2.inpc <-diff(linpc, lag=2)
autoplot(d2.inpc)

