library(quantmod)
library(tseries)
library(forecast)

#obtener el tipo de cambio dolar/peso, por mes, desde 2003 hasta la fecha.
#utilicar de yahoo
getSymbols(Symbols= c("USDMXN=X"), from="2003-01-01", 
           periodicity = "monthly", src ="yahoo")

#Cambiar el nombre de la sere por TC
TC<- `USDMXN=X`

#Graficar tipo de cambio
chartSeries(TC, name="Tipo de cambio peso/dólar mensual", theme=("white"))

#de todos los precios que tenemos del TC nos quedamos solamente con el ajustado
TC.adj = Ad(TC)

#Grafica del TC ajustado
chartSeries(TC.adj, name="Tipo de cambio ajustado", theme=("white"))

#Transformación de la serie a logaritmo
ldolar<-log(TC.adj)
plot(ldolar, col="darkmagenta", main="Log. del TC")

#¿Es estacionaria la serie?
acf(ldolar)
#----- La serie no es estacionaria





#Calculo de rendimiento
R=TC.adj / lag(TC.adj,n=1)-1
View(R)
plot(R, col="darkblue", main="Rendimientos mensuales de TC")

#Rendimientos continuos
r=ldolar - lag (ldolar)
View(r)
plot(r,col="blue", main="Rendimientos continuos del TC")

#Tambien se puede calcular de esta manera
r.d1=diff(ldolar)
plot(r.d1, col="red", main="Rendiminetos continuos del TC")




#¿Son estacionarias las series?
acf(ldolar)
acr(R, na.action = na.pass)
acr(r, na.action = na.pass)
acr(r.d1, na.action = na.pass)

#Prueba de raíces unitarias
#Augmented Fickey-Fuller
adf.test(ldolar)
#Con esta prueba de ADF concluimos que el log del tipo de cambio 
#no es estacionaria

#Se hizo la tranformación de la serie con primeras diferencias
#Es decir ldolar(t) - ldolar(t-1), que es lo mismo que el 
#Rendimiento contunio del dolar

#Hacemos prueba de ADF para r.d1:
#adf.test(r.d1)
#El error que nos aparece es porque la serie r.d1 tiene un valor na dado a 
#que eliminamos una observación de la serie


#na dado que eliminamos una observación de la serie
r.d1.na<-na.omit(r.d1)

#Necesitamos eliminar el NA de la serie
r.d1.na<-na.omit(r.d1)
View(r.d1.na)
#Ya podemos hacer la prueba ADF
adf.test(r.d1.na)
#Con el estadistico de la prueba de ADF concluimos ue la serie es estacionaria
plot(r.d1.na, col="blue", main="Rendimientos continuos del TC")

#Prueba de Phillips-Perron:
pp.test(ldolar)
pp.test(r.d1.na)

#Con estas dos pruebas se confirma que lo rendimientos contínuos 
#del dólar son estacionarios 







