#Estudio de la tendencia del riesgo sobre la mortalidad por cáncer de pulmón en Ohio

#Cargar de librerias
library(sf)
library(gstat)
library(sp)

#Establecer el directorio activo#
setwd("~/Home/Codes/Cuatrimestre_02/AnalisisRegresionModerno/Examen")

#Cargar datos
cancer <- read.csv("Ohio_data/ohio_data.csv")

#Generar un subgrupo
cancer <- subset(cancer, select = -county_name)

#Converir a factor las columnas: gender, race, county, year
cancer$gender <- as.factor(cancer$gender)
cancer$race <- as.factor(cancer$race)
cancer$county <- as.factor(cancer$county)
cancer$year <- as.factor(cancer$year)

#Visualizar los datos
head(cancer)
str(cancer)

#Construir modelo de regresión lineal
modelo_regresion <- lm(y ~ ., data = cancer)
summary(modelo_regresion)

#Seleccionar el mejor modelo por Stepwise
modelo_regresion_step <- step(modelo_regresion, direction = "both")
summary(modelo_regresion_step)


#Grafica del mejor modelo
modelo_regresion_sel <- lm(y ~ county + gender + year + n, data = cancer)
summary(modelo_regresion_sel)

#Graficas de diagnostico
par(mfrow=c(2,2))
plot(modelo_regresion)


