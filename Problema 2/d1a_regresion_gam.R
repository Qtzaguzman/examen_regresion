# Información: Regresion GAM de tasa de mortalidad por condado de cancer de pulmón en Ohio =================================
  # Estudio de la tendencia del riesgo sobre la mortalidad por cáncer de pulmón en Ohio
  # Variables respuesta: rate = Y/n
  # Variables predictoras: county, year, n



# Cargar librerias =====================================
    
  # Limpiar memoria del entorno
  rm(list = ls())
  cat("\014")

  # Cargar librerias
  load <- c("sf", "sp", "spdep", "ggplot2", "dplyr", "tidyr", "spatialreg", "mgcv", "lmtest")
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

  # Visualizar los datos
  head(ohio_data)
  str(ohio_data)



# Preparar datos para construcción de mapas ========================================
  # de tasas de mortalidad por condado

  # Convertir el código del año a un año real
  activar_year_real = FALSE
  if (activar_year_real) {
    ohio_data <- ohio_data %>% 
      mutate(year = 1967 + year) # Suma 1967 al código para convertir 1 a 1968, 2 a 1969, etc.
  }


  # Visualizar los datos
  head(ohio_data)

  # Cargar shapefile
  ohio_map_org <- st_read("data/shape/tl_2010_39_county00.shp")

  # Visualizar capa de condados
  ohio_map_org

  # Seleccionar columnas necesarias y renombrar la columna en común
  ohio_map <- ohio_map_org %>%
    select("NAME" = "NAME00", geometry) # Selecciona la columna renombrada y 'geometry'

  # Unir los datos con el shapefile usando una columna común
  ohio_map_data <- ohio_map %>%
    left_join(ohio_data, by="NAME")



# Preparacion de datos para el modelo GAM ========================================

  # Convertir geometrías a coordenadas X, Y
  coords <- st_coordinates(st_centroid(st_geometry(ohio_map_data)))
  ohio_map_data$X <- coords[, 1]
  ohio_map_data$Y <- coords[, 2]



# Modelo GAM para tasa de mortalidad por condado de cáncer de pulmón en Ohio =================================

  # Ajustar un modelo GAM con términos suavizados para las coordenadas espaciales
  # y utilizando las variables predictoras especificadas
  modelo_gam <- gam(rate ~ s(X, Y) + s(year) + n, data = ohio_map_data, method = "REML")

  # Resumen del modelo
  summary(modelo_gam)

  # Gráficos de diagnóstico del modelo
  par(mfrow = c(2, 2))
  gam.check(modelo_gam)



# Modelo GAM alternativo =================================

  # Comparar con un suavizado diferente para las coordenadas espaciales
  modelo_gam_alt <- gam(rate ~ s(X, Y, bs = "tp") + s(year) + n, data = ohio_map_data, method = "REML")

  # Resumen del modelo alternativo
  summary(modelo_gam_alt)

  # Gráficos de diagnóstico del modelo
  par(mfrow = c(2, 2))
  gam.check(modelo_gam_alt)



# Comparación de modelos =================================

  # Comparar AIC para ambos modelos
  aic_gam <- AIC(modelo_gam)
  aic_gam_alt <- AIC(modelo_gam_alt)
  print(paste("AIC Modelo GAM:", aic_gam))
  print(paste("AIC Modelo GAM Alternativo:", aic_gam_alt))

  # Gráfico de comparación de valores predichos vs observados
  ohio_map_data$predicted_gam <- predict(modelo_gam)

  # Scatter plot de observados vs predichos
  plot(ohio_map_data$rate, ohio_map_data$predicted_gam, 
       xlab = "Valores Observados", ylab = "Valores Predichos", 
       main = "Comparación Observados vs Predichos (GAM)",
       pch = 16, col = "blue")
  abline(0, 1, col = "red", lwd = 2)  # Línea de referencia y = x

  # Prueba de homocedasticidad de Breusch-Pagan
  bptest_gam <- bptest(modelo_gam)
  print(bptest_gam)



# Calculo de coeficiente de determinación R2 ========================================

  # Calcular la proporción de devianza explicada como un pseudo-R^2
  deviance_explicada <- summary(modelo_gam)$dev.expl
  pseudo_R2 <- deviance_explicada

  # Mostrar el pseudo-R^2
  print(paste("Pseudo R^2:", pseudo_R2))

