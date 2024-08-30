#Cargar librerias
load_librerias = FALSE
if (load_librerias == TRUE) {
  library(gstat)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(spdep)
  library(pls)
  library(caret)
  library(car)
  library(glmnet)
  library(islasso)
  #install.packages("glmnet")
  #install.packages("islasso")
}

#Establecer el directorio activo
setwd("~/Home/Codes/Cuatrimestre_02/AnalisisRegresionModerno/Examen")

#Cargar datos
datos <- read.csv("ely_data/ely_plsr_data.csv")

#Variables respuesta: N_g_m2
#Variables predictoras: Wave_500 , …, Wave_2400 

# Definir las variables de respuesta y predictoras
Y <- as.matrix(datos[, c("N_g_m2")])                   # Variable respuesta
X <- as.matrix(datos[, grep("^Wave_", names(datos))])  # Variables predictoras

# Ajustar el modelo LASSO con validación cruzada para seleccionar el valor óptimo de lambda
set.seed(3456)                                            # Asegurar reproducibilidad de los resultados a partir de la misma semilla
lambda_optimo <- cv.glmnet(X, Y, alpha = 1, nfolds = 10)  # Ajuste con 10-fold cross-validation

# Obtener el valor óptimo de lambda
lambda_mejor <- lambda_optimo$lambda.min
print(paste("Lambda óptimo: ", lambda_mejor))

# Visualizar el resultado de la validación cruzada
plot(lambda_optimo)

# Ajustar el modelo LASSO final usando el lambda óptimo
modelo_lasso_final <- glmnet(X, Y, alpha = 1, lambda = lambda_mejor)

# Mostrar los coeficientes del modelo final
print(coef(modelo_lasso_final))

# Realizar predicciones utilizando el modelo ajustado
predicciones <- predict(modelo_lasso_final, s = lambda_mejor, newx = X)
print(predicciones)

# Calcular R^2
r2_lasso <- 1 - sum((Y - predicciones)^2) / sum((Y - mean(Y))^2)

# Mostrar los resultados
cat("R^2 para LASSO:", r2_lasso, "\n")
