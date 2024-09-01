# Información: Mapas de casos de cancer de pulmón en Ohio =================================
  # Estudio de la tendencia del riesgo sobre la mortalidad por cáncer de pulmón en Ohio
  # Mapas de tasas de mortalidad por condados 
  # Variables respuesta: rate = Y/n
  # Variables predictoras: county, year, n



# Cargar librerias =====================================
    
  # Limpiar memoria del entorno
  rm(list = ls())
  cat("\014")

  # Cargar librerias
  load <- c("sf", "sp", "spdep", "ggplot2", "dplyr", "tidyr")
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
  ohio_data <- ohio_data %>% 
    mutate(year = 1967 + year) # Suma 1967 al código para convertir 1 a 1968, 2 a 1969, etc.

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



# Creación de mapas de tasas de mortalidad ========================================

  # Crear el mapa de casos por año
  graficar = TRUE
  if (graficar == TRUE) {
    ggplot(ohio_map_data) +
      geom_sf(aes(fill = rate)) +                                                              # Rellenar el mapa con la tasa de mortalidad
      facet_wrap(~ year, ncol = 7) +                                                           # Crear un panel por cada año
      scale_fill_gradient(low = "white", high = "red", na.value = "grey50", name = "Casos") +
      labs(title = "Mapa de Mortalidad por Cáncer de Pulmón en Ohio (1968-1988)",
          subtitle = "Tasa de muertes por condado por año",
          caption = "Datos: Ohio Lung Cancer Mortality") +
      theme_minimal()
  }



# Modelo de Regresión espacial

  # Crear una lista de vecinos basada en contigüidad de los polígonos
  list_nb <- poly2nb(ohio_map_data)

  # Convertir la lista de vecinos a una matriz de ponderación espacial
  spat_m_pond <- nb2listw(list_nb, style = "W")

  # Predicción (Código de ejemplo de clase)
  #TheSurface = predict(TheGStat, model=FittedModel, newdata = TheGrid)



# Análisis de autocorrelación espacial ========================================

  # Prueba de Moran
  moran_test <- moran.test(ohio_map_data$res, spat_m_pond)

  # Ver resultados de la prueba
  print(moran_test)

