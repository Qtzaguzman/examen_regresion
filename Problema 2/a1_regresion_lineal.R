# Información: Regresión lineal de casos de cancer de pulmón en Ohio =================================
    # Estudio de la tendencia del riesgo sobre la mortalidad por cáncer de pulmón en Ohio
    # Regresión Lineal 
    # Variables respuesta: rate = Y/n
    # Variables predictoras: county, year, n



# Cargar librerias =====================================
    
    load <- c("sf", "gstat", "sp")
    lapply(load, require, character.only = TRUE)

    # Establecer el directorio activo
    setwd("~/Projects/examen_regresion/Problema 2")



# Cargar y preparar datos ========================================

    # Cargar de datos
    cancer <- read.csv("data/OhioRespMort.csv")

    # Generar un subgrupo
    cancer <- subset(cancer, select = c(y, n, county, year, NAME))

    # Calcular tasa de mortalidad por condado
    cancer$rate <- cancer$y / cancer$n

    # Visualizar los datos
    head(cancer)
    str(cancer)



# Ajustar Modelo de Regresión Lineal ========================================
    
    # Modelo de regresión lineal
    modelo_regresion <- lm(rate ~ county + year + n, data = cancer)
    summary(modelo_regresion)



# Analisis de supuestos ========================================

    # Graficas de diagnostico
    par(mfrow=c(2,2))
    plot(modelo_regresion)


