#Estudio de la tendencia del riesgo sobre la mortalidad por cáncer de pulmón en Ohio
install.packages("spdep")
install.packages("spData")
install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')

#Cargar de librerias
library(sf)
library(gstat)
library(sp)
library(ggplot2)
library(dplyr)
library(tidyr)
library(spdep)

#Establecer el directorio activo#
setwd("~/Home/Codes/Cuatrimestre_02/AnalisisRegresionModerno/Examen")

#Cargar datos
ohio_data <- read.csv("Ohio_data/ohio_data.csv")

#Renombrar columna county_name a NAME
ohio_data <- ohio_data %>%
  rename(NAME = county_name)

#Mostrar tabla
head(ohio_data)

#Generar un subset de los datos
ohio_data <- ohio_data %>%
  select(NAME, county, gender, year, y, n)

# Convertir el código del año a un año real
ohio_data <- ohio_data %>%
  mutate(year = 1967 + year) # Suma 1967 al código para convertir 1 a 1968, 2 a 1969, etc.

#Visualizar los datos
head(ohio_data)

#Cargar shapefile
ohio_map_org <- st_read("Ohio_data/tl_2010_39_county00.shp")

#Visualizar los datos
ohio_map_org

# Seleccionar columnas necesarias y renombrar la columna en común
ohio_map <- ohio_map_org %>%
  select("NAME" = "NAME00", geometry) # Selecciona la columna renombrada y 'geometry'

# Unir los datos con el shapefile usando una columna común
ohio_map_data <- ohio_map %>%
  left_join(ohio_data, by="NAME")

#Graficar el mapa
#plot(ohio_map_data)

#Generar el mapa para 1968 a partir de un subconjunto
ohio_map_data_1968 <- ohio_map_data %>%
  filter(year_real == 1968)

#Crear el mapa de casos por año
graficar = FALSE
if (graficar == TRUE) {
  ggplot(ohio_map_data_1968) +
    geom_sf(aes(fill = y)) +  # Rellenar el mapa con el número de casos 'y'
    facet_wrap(~ year_real) +  # Crear un panel por cada año
    scale_fill_gradient(low = "white", high = "red", na.value = "grey50", name = "Casos") +
    labs(title = "Mapa de Mortalidad por Cáncer de Pulmón en Ohio (1968-1988)",
         subtitle = "Número de muertes por condado por año",
         caption = "Datos: Ohio Lung Cancer Mortality") +
    theme_minimal()
}

# Crear una lista de vecinos basada en contigüidad de los polígonos
list_nb <- poly2nb(ohio_map_data)

# Convertir la lista de vecinos a una matriz de ponderación espacial
spat_m_pond <- nb2listw(list_nb, style = "W")

# Prueba de Moran
moran_test <- moran.test(ohio_map_data$res, spat_m_pond)

# Ver resultados de la prueba
print(moran_test)



#Ajuste de modelo
FittedModel <- fit.variogram(TheVariogram, model = TheVariogramModel)
plot(TheVariogram, model = FittedModel)
TheGStat <- gstat(id = "Sine", formula = logAs ~ 1, data = arsenic)

#Crear columnas de pixeles de un raster
Columns = seq(from = 0, to = 500, by = 10)

#Agregar las filas de pixeles
Rows = seq(from = 0, to = 500, by = 10)

#Crear la grilla
TheGrid = expand.grid(x = Columns, y = Rows)

#Convertir a pixeles espaciales
coordinates(TheGrid) <- ~x + y
gridded(TheGrid) <- TRUE

#Graficar malla y puntos
plot(TheGrid, cex = 0.5)

#Predicción
TheSurface = predict(TheGStat, model=FittedModel, newdata = TheGrid)

#Establecer los margenes a 2
par(mar = c(2, 2, 2, 2))

#Agregar surface kriggin
image(TheSurface, col = terrain.colors(100), axes = FALSE)

#Agregar curvas de nivel
contour(TheSurface, add = TRUE)

#Agregar puntos
points(arsenic, pch = 20, col = "red")