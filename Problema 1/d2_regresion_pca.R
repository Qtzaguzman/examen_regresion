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

# LOO (Leave-One-Out Cross-Validation) 
# Tipo de validación cruzada que se utiliza para evaluar el rendimiento de un modelo predictivo. 
# En esta técnica, se utiliza un solo dato del conjunto de datos como conjunto de prueba 
# y el resto como conjunto de entrenamiento. 
# Este proceso se repite de tal manera que cada observación se utiliza una vez como conjunto de prueba.

# Ajustar el modelo de regresión PCA considerando 5 componentes
modelo_pca <- pcr(Y ~ X, ncomp = 5, validation = "LOO", jacknite=T) 

# Mostrar un resumen del modelo ajustado, incluyendo la varianza explicada
summary(modelo_pca)

# Intervalos de confianza de los coeficientes
ci_coef <- confint(modelo_pca, parm = "coefficients", ncomp = 5)
print(ci_coef)

# Generar el gráfico Elbow para evaluar el número óptimo de componentes
rmsep_values <- RMSEP(modelo_pca, validation = "LOO")
plot(rmsep_values, main = "Elbow - RMSEP vs Número de componentes", 
     xlab = "Número de componentes", ylab = "RMSEP", type = "b", col = "blue", lwd = 2, pch = 19)

# Mostrar los coeficientes del modelo para los componentes principales
coef(modelo_pca, ncomp = 5, intercept = TRUE)

# Graficar los coeficientes del modelo para los componentes 1 a 5
coefplot(modelo_pca, ncomp = 1:5)

# Hacer predicciones sobre el conjunto de datos X usando el modelo PCA ajustado
pred <- predict(modelo_pca, X, ncomp = 5)
pred

# Calcular el coeficiente de determinación (R^2) para la variable de respuesta
y_true <- Y[, 1]         # Extraer los valores reales de la variable de respuesta
y_pred <- pred[, 1, 1]   # Extraer los valores predichos

# Calcular R^2 directamente
R2_value <- 1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)

# Mostrar el valor de R^2
print(paste("R^2:", round(R2_value, 4)))

barplot(R2_value, 
        main = expression(R^2 ~ " variable de respuesta"), 
        ylab = expression(R^2), 
        xlab = "Variable Respuesta", 
        col = "dodgerblue",  # Color de la barra
        border = "white",    # Borde de la barra
        names.arg = colnames(Y),  # Nombre de la variable de respuesta
        ylim = c(0, 1),  # Escalar el eje y de 0 a 1 para R^2
        cex.main = 1.5,   # Tamaño de la fuente del título
        cex.lab = 1.2,    # Tamaño de la fuente de las etiquetas
        cex.names = 1.2)  # Tamaño de la fuente de los nombres de las barras

# Añadir valores numéricos de R^2 sobre la barra
text(x = 1, y = R2_value, labels = round(R2_value, 3), pos = 3, cex = 1.2, col = "black")
