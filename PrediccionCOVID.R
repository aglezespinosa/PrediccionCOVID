##############################################################################
# Analisis, Prediccion y Graficación de COVID Mexico                         #
# (c) Angel Gonzalez Espinosa 2021                                           #
# aglezespinosa@gmail.com                                                    #
##############################################################################


library(tidyverse)
library(tseries)
library(lubridate)
library(forecast)

# Funcion Formatear Decimal
FormatDecimal <- function(x, k) {
  return(format(round(as.numeric(x), k), nsmall=k))
}

# Pronostico Fallecidos
# Carga de datos de Fallecidos en una columna
#arch <- "Aguascalientes.csv"
#arch <- "CDMX.csv"
#arch <- "Nacional.csv"

#arch4 <- "LigaTurEdos.csv"
#datos4 <- read.csv(arch4, header=TRUE, sep=",")

#arch3 <- "PoblacionEdos.csv"
#datos3 <- read.csv(arch3, header=TRUE, sep=",")
#p <- 1
#poblacion <- datos3[p]

arch <- "FallecidosNacional.csv"
datos <- read.csv(arch, header=TRUE, sep=",")
summary(datos)

arch6 <- "NCLugares.csv"        # Confirmados Lugares
arch7 <- "FallecidosLugares.csv"        # Confirmados Lugares
datos6 <- read.csv(arch6, header=TRUE, sep=",")
datos7 <- read.csv(arch7, header=TRUE, sep=",")

# Convertimos a serie de datos iniciando en Abril de 2020
i = 33
#i = 7
# Elegimos Nacional
covid.ts=ts(datos[i], start = c(2020, 4), freq = 365)
covid.ts
plot(covid.ts)
nomedo = names(datos[i])
sumacasos = sum(datos[i])


# Histograma
hist(covid.ts, col="blue")
qqnorm(covid.ts, pch=19, col="blue")
qqline(covid.ts, col = "red", lwd=2)
shapirocovid.test <- shapiro.test(covid.ts)
# el pvalue es menor a 0.05 por lo que no tiene distribucion normal
pvalue <- shapirocovid.test[["p.value"]]
print(FormatDecimal(pvalue,12))


# Grafica serie de tiempo e histograma
plot(covid.ts)
plot(diff(covid.ts, type="o")) # Muestra posibles atipicidades
hist(covid.ts, main = "Histograma de Fallecidos COVID CDMX", xlab = "Cantidad", ylab = "Frecuencia")

# Descompone la serie de tiempo en componentes irregulares (temporalidad, tendencias)
descomponer <- decompose(covid.ts, type='additive')
# Grafica la descomposicion
plot(descomponer)
# Grafica temporalidad por años
titgraf <- paste("Temporalidad Fallecidos COVID de: ", nomedo)
ggseasonplot(covid.ts, main = titgraf, xlab = "Fechas", ylab = "Cantidad")


# Pronosticos
m1 <- snaive(covid.ts, h=96)
plot(m1)
# regresion <- tslm(covid.ts ~ trend + season)
# Regresion
regresion <- tslm(covid.ts ~ trend + season)
m2 <- forecast(regresion, h=60)
plot(m2)
titgraf <- paste("Pronostico Fallecidos Covid con Regresión: ", nomedo)
autoplot(m2)+labs(title=titgraf,x="Tiempo",y="Fallecidos",colour="#00a0dc")
checkresiduals(m2)


# ARIMA
modelo_arima <- auto.arima(covid.ts)
m4 <- forecast(modelo_arima, h=60)
titgraf <- paste("Pronostico Fallecidos Covid con ARIMA: ", nomedo)
autoplot(m4)+labs(title=titgraf,x="Tiempo",y="Fallecidos",colour="#00a0dc")
checkresiduals(m4)

# Red Neuronal
red_neuronal <- nnetar(covid.ts)
m5 <- forecast(red_neuronal, h=60)
titgraf <- paste("Pronostico Fallecidos Covid con Red Neuronal: ", nomedo)
autoplot(m5)+labs(title=titgraf,x="Tiempo",y="Fallecidos",colour="#00a0dc")
autoplot(m5, ts.colour = 'firebrick1', predict.colour = 'red',
         predict.linetype = 'dashed', conf.int = FALSE)
autoplot(m5, ts.geom = 'bar', fill = 'blue')

