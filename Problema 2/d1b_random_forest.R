# Información: Modelo Random Forest para tasa de mortalidad por condado de cancer de pulmón en Ohio =================================
    
    # Estudio de la tendencia del riesgo sobre la mortalidad por cáncer de pulmón en Ohio
    # Variables respuesta: rate = Y/n
    # Variables predictoras: county, year, n



# Cargar librerias =====================================
    
    # Limpiar memoria del entorno
    rm(list = ls())
    cat("\014")

    # Cargar librerias
    load <- c("sf", "sp", "spdep", "ggplot2", "dplyr", "tidyr", "spatialreg", "mgcv", "lmtest", "randomForest", "MASS", "randomForestExplainer")
    lapply(load, require, character.only = TRUE)

    # Establecer el directorio activo
    setwd("~/Projects/examen_regresion/Problema 2")



# Cargar y preparar datos ========================================

    # Cargar de datos
    ohio_data <- read.csv("data/OhioRespMort.csv")

    # Generar un subgrupo
    ohio_data <- subset(ohio_data, select = c(y, n, county, year, NAME))

    # Calcular tasa de mortalidad por condado
    ohio_data$rate <- ohio_data$y / ohio_data$n

    # Convertir year a factor
    ohio_data$year <- as.factor(ohio_data$year)

    # Visualizar los datos
    head(ohio_data)
    str(ohio_data)


# Construir modelo Random Forest ========================================

    #Construir el modelo de clasificación con Random Forest
    rf <- randomForest(Variety ~ ., data = datos, ntree = 50, mtry = 6, importance = TRUE)
    rf

plot(rf)

table(datos$Variety)

#Importancia de las variables ordenadas
importance(rf)
  
#Importancia de las variables graficadas
varImpPlot(rf)

datos_pred <- subset(datos, select = -Variety)

#Predecir
pred <- predict(rf, newdata = datos_pred)
table(pred, datos$Variety)






# Asegúrate de que los datos estén preparados adecuadamente
# Convertir las variables categóricas a factor si es necesario
ohio_map_data$county <- as.factor(ohio_map_data$county)

# Ajustar el modelo Random Forest
# rate es la variable de respuesta, las demás son predictoras
modelo_rf <- randomForest(rate ~ year + n + county, 
                          data = ohio_map_data, 
                          ntree = 500,  # Número de árboles
                          mtry = 3,    # Número de variables consideradas en cada split
                          importance = TRUE)  # Calcular la importancia de las variables

# Resumen del modelo
print(modelo_rf)

# Importancia de las variables
importance(modelo_rf)
varImpPlot(modelo_rf)  # Gráfico de importancia de variables

# Predicciones con el modelo Random Forest
predicciones_rf <- predict(modelo_rf, newdata = ohio_map_data)

# Comparación de valores observados vs predichos
plot(ohio_map_data$rate, predicciones_rf, 
     xlab = "Valores Observados", ylab = "Valores Predichos", 
     main = "Comparación Observados vs Predichos (Random Forest)",
     pch = 16, col = "blue")
abline(0, 1, col = "red", lwd = 2)  # Línea de referencia y = x
