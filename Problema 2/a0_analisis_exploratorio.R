#Estudio de la tendencia del riesgo sobre la mortalidad por cáncer de pulmón en Ohio

#Cargar de librerias
library(sf)
library(gstat)
library(sp)

#Establecer el directorio activo#
setwd("~/Home/Codes/Cuatrimestre_02/AnalisisRegresionModerno/Examen")

#Cargar datos
cancer <- read.csv("Ohio_data/OhioRespMort.csv")

#Visualizar los datos
head(cancer)

#Construir modelo de regresión lineal
modelo_regresion <- lm(y ~ county + year + n, data = cancer)
summary(modelo_regresion)

#Filtrar solo los datos del county 1 y graficarlos por year
cancer_county1 <- cancer[cancer$county == 4,]
plot(cancer_county1$year, cancer_county1$n)

#Construir las 88 graficas de los 88 condados year vs n
par(mfrow = c(4, 6))
for (j in 1:4){
  ini = 1 + 22*(j-1)
  fin = 22*j
  for (i in ini:fin) {
    cancer_county <- cancer[cancer$county == i,]
    plot(cancer_county$year, cancer_county$n, main = paste("County", i))
  }
}

