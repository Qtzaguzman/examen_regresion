# Información: Modelo LASSO y RIDGE para tasa de mortalidad por condado de cancer de pulmón en Ohio =================================
    
  # Estudio de la tendencia del riesgo sobre la mortalidad por cáncer de pulmón en Ohio
  # Variables respuesta: N_g_m2
  # Variables predictoras: Wave_500 , …, Wave_2400 

  # Definir el tipo de regresión a realizar 1 = LASSO, 0 = RIDGE
  tipo_regresion <- 0

  # Definir titulo regresion
  titulo_regresion <- ifelse(tipo_regresion == 1, "LASSO", "RIDGE")

# Cargar librerias =====================================
    
  # Limpiar memoria del entorno
  rm(list = ls())
  cat("\014")

  # Cargar librerias
  load <- c("gstat", "ggplot2", "dplyr", "tidyr", "spdep", "pls", "caret", "car", "glmnet", "islasso")
  lapply(load, require, character.only = TRUE)

  # Establecer el directorio activo
  setwd("~/Projects/examen_regresion/Problema 1")



# Cargar y preparar datos ========================================

  # Cargar datos
  datos <- read.csv("data/ely_plsr_data.csv")

  # Definir las variables de respuesta y predictoras
  Y <- as.matrix(datos[, c("N_g_m2")])                   # Variable respuesta
  X <- as.matrix(datos[, grep("^Wave_", names(datos))])  # Variables predictoras

  # Ajustar el modelo LASSO y RIDGE con validación cruzada para seleccionar el valor óptimo de lambda
  set.seed(1)                                                            # Asegurar reproducibilidad de los resultados a partir de la misma semilla
  lambda_optimo <- cv.glmnet(X, Y, alpha = tipo_regresion, nfolds = 10)  # Ajuste con 10-fold cross-validation



# Analisis de lambda óptimo ========================================

  # Obtener el valor óptimo de lambda
  lambda_mejor <- lambda_optimo$lambda.min
  print(paste("Lambda óptimo: ", lambda_mejor))

  # Visualizar el resultado de la validación cruzada
  plot(lambda_optimo)



# Ajustar el modelo LASSO y RIDGE final ========================================

  # Ajustar el modelo final usando el lambda óptimo
  modelo_ridge_final <- glmnet(X, Y, alpha = tipo_regresion, lambda = lambda_mejor)
  summary(modelo_ridge_final)

  # Mostrar los coeficientes del modelo final
  print(coef(modelo_ridge_final))



# Predicciones y evaluación del modelo ========================================

  # Realizar predicciones utilizando el modelo ajustado
  predicciones <- predict(modelo_ridge_final, s = lambda_mejor, newx = X)
  print(predicciones)

  # Calcular R^2
  r2_ridge <- 1 - sum((Y - predicciones)^2) / sum((Y - mean(Y))^2)
  
  # Mostrar los resultados
  cat(paste("Lambda óptimo", titulo_regresion, ": "), r2_ridge, "\n")


