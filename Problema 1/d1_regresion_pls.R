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
}

#Establecer el directorio activo
setwd("~/Home/Codes/Cuatrimestre_02/AnalisisRegresionModerno/Examen")

#Cargar datos
datos <- read.csv("ely_data/ely_plsr_data.csv")

#Variables respuesta: N_g_m2
#Variables predictoras: Wave_500 , …, Wave_2400 

#Visualizar los datos
#head(datos)
#str(datos)
set.seed(3456)

##Ajustar modelo PLS

# Definir las variables de respuesta
Y <- as.matrix(datos[, c("N_g_m2")])

# Definir las variables predictoras
X <- as.matrix(datos[, grep("^Wave_", names(datos))])

# Ajustar el modelo de regresión PLS considerando 5 componentes y escalando
modelo_pls <- plsr(Y ~ X, ncomp = 5, scale = TRUE, validation = "CV")

# Mostrar un resumen del modelo ajustado
summary(modelo_pls)

# Generar el gráfico Elbow para evaluar el número óptimo de componentes
rmsep_values <- RMSEP(modelo_pls, validation = "CV")
plot(rmsep_values, main = "Elbow - RMSEP vs Número de componentes", 
     xlab = "Número de componentes", ylab = "RMSEP", type = "b")

#Coeficientes del modelo por componente
coef(modelo_pls, ncomp = 5, intercept = TRUE)

#Graficar coeficientes de regresión
coefplot(modelo_pls, ncomp = 1:5)

#Predicciones
predicciones <- predict(modelo_pls, ncomp = 5, newdata = X)
predicciones

#Generar gráfico de predicciones vs observaciones con ggplot
datos_predicciones <- data.frame(Observado = Y, Predicho = predicciones)
names(datos_predicciones)

# Grafica de predicciones vs observaciones C_N_mass
ggplot(datos_predicciones, aes(x = datos_predicciones$Observado.N_g_m2, y = datos_predicciones$Predicho.N_g_m2.comps)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicciones vs Observaciones (N_g_m2)", x = "Observado N_g_m2", y = "Predicho N_g_m2")

# Calcular los coeficientes de determinación (R^2) para cada variable de respuesta
R2_values <- sapply(1:ncol(Y), function(i) {
  y_true <- Y[, i]
  y_pred <- predicciones[, i, 1]
  1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
})

# Mostrar los valores de R^2
names(R2_values) <- colnames(Y)
print(R2_values)





# VALIDACION CRUZADA

# Crear un índice para dividir los datos en 70% entrenamiento y 30% prueba
set.seed(345)
index_train <- createDataPartition(Y[,1], p = 0.7, list = FALSE)

# Dividir los datos en conjunto de entrenamiento y prueba
X_train <- X[index_train, ]
Y_train <- Y[index_train, ]
X_test <- X[-index_train, ]
Y_test <- Y[-index_train, ]

# Ajustar el modelo de regresión PLS con 5 componentes
modelo_pls_train <- plsr(Y_train ~ X_train, ncomp = 5, scale = TRUE, validation = "CV")

# Mostrar un resumen del modelo ajustado, incluyendo la varianza explicada
summary(modelo_pls_train)

# Gráfico Elbow para evaluar el número óptimo de componentes en el conjunto de entrenamiento
rmsep_values <- RMSEP(modelo_pls_train, validation = "CV")
plot(rmsep_values, main = "Elbow Plot - RMSEP vs Número de componentes (Train Set)", 
     xlab = "Número de componentes", ylab = "RMSEP", type = "b")

# Grafica los coeficientes del modelo para los componentes especificados
coefplot(modelo_pls_train, ncomp = 1:5)

# Hacer predicciones sobre el conjunto de prueba usando el modelo PLS ajustado
pred_test <- predict(modelo_pls, X_test, ncomp = 5)

# Calcular los coeficientes de determinación (R^2) para cada variable de respuesta
R2_values_test <- sapply(1:ncol(Y_test), function(i) {
  y_true <- Y_test[, i]
  y_pred <- pred_test[, i, 1]  # Accede a las predicciones en la tercera dimensión
  1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
})

# Mostrar los valores de R^2 para el conjunto de prueba
names(R2_values_test) <- colnames(Y_test)
print(R2_values_test)