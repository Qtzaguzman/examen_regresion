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
    library(randomForest)

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
    activar_year_real = TRUE
    if (activar_year_real) {
        ohio_data <- ohio_data %>% 
            mutate(year = 1967 + year) # Suma 1967 al código para convertir 1 a 1968, 2 a 1969, etc.

        # Convertir year a factor
        #ohio_data$year <- as.factor(ohio_data$year)
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
    
    # Revisar si hay valores faltantes en las columnas relevantes
    colSums(is.na(ohio_map_data))
    


# Construir modelo Random Forest ========================================

    # Construir el modelo de clasificación con Random Forest
    modelo_rf <- randomForest(rate ~ X + Y + year + n, 
                          data = ohio_map_data, 
                          ntree = 500,        # Número de árboles
                          mtry = 3,           # Número de variables consideradas en cada split
                          importance = TRUE)  # Calcular la importancia de las variables

    modelo_rf
    plot(modelo_rf)

    # Importancia de las variables del modelo
    importance(modelo_rf)
    
    # Importancia de las variables graficadas
    varImpPlot(modelo_rf)

    datos_pred <- subset(ohio_map_data, select = -rate)

    # Predicciones con el modelo Random Forest
    predicciones_rf <- predict(modelo_rf, newdata = ohio_map_data)

    # Comparación de valores observados vs predichos
    plot(ohio_map_data$rate, predicciones_rf, xlab = "Valores Observados", ylab = "Valores Predichos", 
    main = "Comparación Observados vs Predichos (Random Forest)", 
    pch = 16, col = "blue")
    abline(0, 1, col = "red", lwd = 2)  

    table(predicciones_rf, ohio_map_data$rate)

