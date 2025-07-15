#Proyecto INPC####

library(TSA)
library(forecast)
library(urca) #funcion ur.df
library(lmtest) #coeftest
library(reshape2) #Funcion melt

INPC  <- read.csv("INPC.csv", header=T, stringsAsFactors = T)
INPC <- INPC[1:72,]

INPC.ts<- ts(INPC$INPC, start = c(2010,01), end = c(2015,12), frequency = 12) #Fin, 1 de dic 2015
INPC.ts
plot(INPC.ts, xlab="Fecha", ylab="INPC")  #No parece haber media ni varianza constante
plot(diff(INPC.ts),xlab="Fecha", ylab="diff(INPC)") #media y varianza constante con una primera diferencia

#Haremos el acf y el pacf de la primera diferencia
par(mfrow=c(1,2))
acf(as.vector(diff(INPC.ts)), main="ACF")
pacf(as.vector(diff(INPC.ts)), main="PACF") #No parece haber un comportamiento
par(mfrow=c(1,1))

eacf(as.vector(diff(INPC.ts))) #ARIMA(1,1,1), ARIMA(2,1,1)
auto.arima(as.vector(INPC.ts)) #Esta funcion nos indica un ARIMA(2,1,2)

modelo1 <- arima(INPC.ts, order=c(1,1,1))
modelo2 <- arima(INPC.ts, order=c(2,1,1))
modelo3 <- arima(INPC.ts, order=c(2,1,2))
modelo1;modelo2;modelo3

AIC(modelo1, modelo2, modelo3) #Parece ser mejor el modelo3 ARIMA(2,1,2)

#Usaremos el modelo3
residuos <- modelo3$residuals
plot(residuos, xlab="Fecha", ylab="Residuales")

#Checamos normalidad
qqnorm((residuos-mean(residuos))/sqrt(var(residuos))) #con graficas
ks.test((residuos-mean(residuos))/sqrt(var(residuos)), "pnorm",0,1)
#Como p-valor=0.9664 > 0.05, no se rechaza la hipotesis nula, 
#decimos que la muestra no presenta evidencia suficiente en contra de la normalidad

#¿Es un ruido blanco? hay unas lineas fuera de las bandas, sin embargo, haremos la prueba
par(mfrow=c(1,2))
acf(as.vector(residuos), main="ACF")
pacf(as.vector(residuos), main="PACF")
par(mfrow=c(1,1))

#Prueba para saber si es ruido blanco
prueba <- Box.test(residuos, lag=20, type = "Ljung-Box", fitdf = 1)
prueba #El p-valor de la prueba Lung Box es 0.067 > 0.05, por lo que no se rechaza
#H0, es decir, no hay evidencia en contra del ruido blanco.

#Prueba de si los coeficientes del modelo son significativamente diferentes de cero, 
#esto se debe realizar despues de validar el modelo 
coeftest(modelo3)
confint(modelo3)

#Predicciones
modelo<-Arima(INPC.ts, order=c(2,1,2))
forecast(modelo, h=5)

#Grafico con datos observados, los estimados y pronósticos.
plot(forecast(modelo, h=5), xlab="Fecha", ylab="INPC")
lines(modelo$fitted, lty=2, col="blue") #modelo

