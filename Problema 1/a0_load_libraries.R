#Instalar librerias
instalar = FALSE
if(instalar) {
    install.packages("ggplot2")
    install.packages("dplyr")
    install.packages("tidyr")
    install.packages("spdep")
    install.packages("pls")
    install.packages("caret")
    install.packages("raster")
    install.packages("rgdal")
    install.packages("rgeos")
    install.packages("sp")
    install.packages("zoo")
    install.packages("fields")
    install.packages("gstat", dependencies=TRUE)
    install.packages("gridExtra")
    install.packages("patchwork")
}


#Cargar librerias
load = FALSE
if(load) {
    library(gstat)
    library(raster)
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    library(spdep)
    library(pls)
    library(caret)
    library(rgdal)
    library(rgeos)
    library(sp)
    library(zoo)
    library(fields)
    library(gridExtra)
    library(patchwork)
    library(caret)
}