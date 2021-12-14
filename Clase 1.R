library(readxl)
inpc <- read_excel("~/Desktop/Tec/SeriesT/inpc.xls")

library(forecast)
library(tseries)

inpc2<-ts(inpc$inpc, start=c(1982,1), frequency = 12)
print(inpc2)
#----- Ts significa serie de tiempo


library(ggfortify)
autoplot(inpc2, ts.colour="red", ts.linetipe="dashed")
#----- autoplot es para generar la gráfica

autoplot(stl(inpc2, s.window ="periodic"), ts.colour = "blue")
#----- descomponer la serie de tiempo en sus 3 componentes fundamentales:
#Tendencia, estacionalidad y residuos. 



inpc.desc<-decompose(inpc2)
plot(inpc.desc)
#----- Forma máss facil de descomponer la serie de 
#tiempo en sus 3 componentes fundamentales:
#Tendencia, estacionalidad y residuos. 


