# Información: Regresion lineal de tasa de mortalidad por condado de cancer de pulmón en Ohio =================================
  # Estudio de la tendencia del riesgo sobre la mortalidad por cáncer de pulmón en Ohio
  # Mapas de tasas de mortalidad por condados 
  # Variables respuesta: rate = Y/n
  # Variables predictoras: county, year, n



# Cargar librerias =====================================
    
  # Limpiar memoria del entorno
  rm(list = ls())
  cat("\014")

  # Cargar librerias
  load <- c("sf", "sp", "spdep", "ggplot2", "dplyr", "tidyr", "spatialreg")
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



# Matriz de ponderación espacial ========================================

  # Crear una lista de vecinos basada en contigüidad de los polígonos
  list_nb <- poly2nb(ohio_map_data)

  # Convertir la lista de vecinos a una matriz de ponderación espacial
  spat_m_pond <- nb2listw(list_nb, style = "W")



# Análisis de autocorrelación espacial ========================================

  # Prueba de Moran para la variable de respuesta 'rate'
  moran_test_rate <- moran.test(ohio_map_data$rate, spat_m_pond, alternative = "two.sided")
  print(moran_test_rate)

  # Prueba de Monte Carlo para la variable de respuesta 'rate'
  moran_mc_rate <- moran.mc(ohio_map_data$rate, spat_m_pond, nsim = 999)
  print(moran_mc_rate)



# Modelo de regresión error espacial (Spatial Error Model) ========================================

  # Ajustar un modelo de regresión de retardo espacial (Spatial Lag Model)
  modelo_sar <- lagsarlm(rate ~ year + n, data = ohio_map_data, listw = spat_m_pond)
  summary(modelo_sar)

  # Ajustar un modelo de regresión de error espacial (Spatial Error Model)
  modelo_sem <- errorsarlm(rate ~ year + n, data = ohio_map_data, listw = spat_m_pond)
  summary(modelo_sem)



# Análisis de residuales ========================================

  # Evaluación de los residuales del modelo de retardo espacial
  ohio_map_data$res_sar <- residuals(modelo_sar)
  moran.mc(ohio_map_data$res_sar, spat_m_pond, nsim = 999)

  # Evaluación de los residuales del modelo de error espacial
  ohio_map_data$res_sem <- residuals(modelo_sem)
  moran.mc(ohio_map_data$res_sem, spat_m_pond, nsim = 999)

  # Visualización de los residuales del modelo SAR en el mapa
  ggplot(ohio_map_data) +
    geom_sf(aes(fill = res_sar)) +
    theme_minimal() +
    labs(title = "Residuales del Modelo de Retardo Espacial (SAR)")

  # Visualización de los residuales del modelo SEM en el mapa
  ggplot(ohio_map_data) +
    geom_sf(aes(fill = res_sem)) +
    theme_minimal() +
    labs(title = "Residuales del Modelo de Error Espacial (SEM)")



