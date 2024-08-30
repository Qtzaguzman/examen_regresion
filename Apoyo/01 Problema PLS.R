
# Leer el archivo CSV en un data frame
datos <- read.csv("ely_plsr_data.csv", header = TRUE)

# Mostrar las primeras filas del data frame para verificar que se ha leído correctamente
head(datos)
str(datos)
set.seed(1)
###################################################################################################################################
###################################################################################################################################
#a)	Ajuste un modelo de regresión PLS  considerando  las variables :C_N_mass,C_g_m2, H20_g_m2, LMA_g_m2	, N_g_m2 como respuestas y 
#las variables Wave_500 , …, Wave_2400 como predictoras.
###################################################################################################################################
###################################################################################################################################
#Datos completos
###################################################################################################################################
set.seed(1)

# Cargar la librería pls para realizar la regresión PLS
library(pls)

# Definir las variables de respuesta (C_N_mass, C_g_m2, H20_g_m2, LMA_g_m2, N_g_m2)
Y <- as.matrix(datos[, c("C_N_mass", "C_g_m2", "H20_g_m2", "LMA_g_m2", "N_g_m2")])

# Definir las variables predictoras (Wave_500 a Wave_2400)
X <- as.matrix(datos[, grep("^Wave_", names(datos))])
dimensiones_X <- dim(X)

# Ajustar el modelo de regresión PLS considerando 2 componentes y escalando (media de 0 y una desviación estándar de 1) las variables
modelo_pls <- plsr(Y ~ X, ncomp = 5, scale = TRUE, validation = "CV")

# Mostrar un resumen del modelo ajustado, incluyendo la varianza explicada
summary(modelo_pls)

# Generar el gráfico Elbow para evaluar el número óptimo de componentes
rmsep_values <- RMSEP(modelo_pls, validation = "CV")
plot(rmsep_values, main = "Elbow Plot - RMSEP vs Número de componentes", 
     xlab = "Número de componentes", ylab = "RMSEP", type = "b")

# Opcional: Mostrar los coeficientes del modelo para los componentes especificados
coef(modelo_pls, ncomp = 5, intercept = TRUE)

coefplot(modelo_pls, ncomp = 1:5)

# Hacer predicciones sobre el conjunto de datos X usando el modelo PLS ajustado
# El resultado de `predict` tiene una estructura de 3 dimensiones: [n_samples, n_components, n_responses]
pred <- predict(modelo_pls, X, ncomp = 5)
pred

# Mostrar los valores originales de Y
Y

dim(Y)

# Como `pred` tiene la forma [178, 5, 1], se accede a las predicciones para cada variable de respuesta
R2_values <- sapply(1:ncol(Y), function(i) {
  y_true <- Y[, i]
  y_pred <- pred[, i, 1]  # Accede a las predicciones en la tercera dimensión
  1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
})

# Mostrar los valores de R^2
names(R2_values) <- colnames(Y)
print(R2_values)


# Graficar los valores de R^2 para cada variable de respuesta

barplot(R2_values, main = "R^2 para cada variable respuesta", ylab = "R^2", xlab = "Variables Respuesta")
###################################################################################################################################
#Validación cruzada (a)
###################################################################################################################################
set.seed(1)

# Definir las variables de respuesta (C_N_mass, C_g_m2, H20_g_m2, LMA_g_m2, N_g_m2)
Y <- as.matrix(datos[, c("C_N_mass", "C_g_m2", "H20_g_m2", "LMA_g_m2", "N_g_m2")])

# Definir las variables predictoras (Wave_500 a Wave_2400)
X <- as.matrix(datos[, grep("^Wave_", names(datos))])
dimensiones_X <- dim(X)

# Crear un índice para dividir los datos en 70% entrenamiento y 30% prueba
set.seed(1)
Indice_entrenamiento <- createDataPartition(Y[,1], p = 0.7, list = FALSE)

# Dividir los datos en conjunto de entrenamiento y prueba
X_train <- X[Indice_entrenamiento, ]
Y_train <- Y[Indice_entrenamiento, ]
X_test <- X[-Indice_entrenamiento, ]
Y_test <- Y[-Indice_entrenamiento, ]

# Ajustar el modelo de regresión PLS considerando 4 componentes y escalando (media de 0 y una desviación estándar de 1) las variables
modelo_pls <- plsr(Y_train ~ X_train, ncomp = 5, scale = TRUE, validation = "CV")

# Mostrar un resumen del modelo ajustado, incluyendo la varianza explicada
summary(modelo_pls)

# Generar el gráfico Elbow para evaluar el número óptimo de componentes en el conjunto de entrenamiento
rmsep_values <- RMSEP(modelo_pls, validation = "CV")
plot(rmsep_values, main = "Elbow Plot - RMSEP vs Número de componentes (Train Set)", 
     xlab = "Número de componentes", ylab = "RMSEP", type = "b")

# Mostrar los coeficientes del modelo para los componentes especificados
coef(modelo_pls, ncomp = 5, intercept = TRUE)
coefplot(modelo_pls, ncomp = 1:5)

# Hacer predicciones sobre el conjunto de prueba usando el modelo PLS ajustado
pred_test <- predict(modelo_pls, X_test, ncomp = 5)

# Calcular el coeficiente de determinación (R^2) para cada variable de respuesta en el conjunto de prueba
R2_values_test <- sapply(1:ncol(Y_test), function(i) {
  y_true <- Y_test[, i]
  y_pred <- pred_test[, i, 1]  # Accede a las predicciones en la tercera dimensión
  1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
})

# Mostrar los valores de R^2 para el conjunto de prueba
names(R2_values_test) <- colnames(Y_test)
print(R2_values_test)

# Graficar los valores de R^2 para cada variable de respuesta en el conjunto de prueba
barplot(R2_values_test, main = "R^2 para cada variable respuesta (Conjunto de Prueba)", ylab = "R^2", xlab = "Variables Respuesta")

###################################################################################################################################
###################################################################################################################################
#b)	Es posible incluir Species como otra variable (usando variables dummy) como predictora? Si lo es mejora el modelo?
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
#Datos completos
###################################################################################################################################
set.seed(1)

# Cargar la librería pls para realizar la regresión PLS
library(pls)

# Definir las variables de respuesta (C_N_mass, C_g_m2, H20_g_m2, LMA_g_m2, N_g_m2)
Y <- as.matrix(datos[, c("C_N_mass", "C_g_m2", "H20_g_m2", "LMA_g_m2", "N_g_m2")])

# Convertir la columna 'Species' en variables dummy
species_dummies <- model.matrix(~ Species_Code - 1, data = datos)

# Definir las variables predictoras (Wave_500 a Wave_2400)
X_W <- as.matrix(datos[, grep("^Wave_", names(datos))])

# Combinar las variables predictoras originales con las variables dummy de 'Species'
X <- cbind(X_W, species_dummies)

# Verificar las dimensiones de las nuevas variables predictoras
dimensiones_X <- dim(X)

# Ajustar el modelo de regresión PLS considerando 5 componentes y escalando las variables
modelo_pls <- plsr(Y ~ X, ncomp = 5, scale = TRUE, validation = "CV")

# Mostrar un resumen del modelo ajustado, incluyendo la varianza explicada
summary(modelo_pls)

# Generar el gráfico Elbow para evaluar el número óptimo de componentes
rmsep_values <- RMSEP(modelo_pls, validation = "CV")
plot(rmsep_values, main = "Elbow Plot - RMSEP vs Número de componentes", 
     xlab = "Número de componentes", ylab = "RMSEP", type = "b")

# Opcional: Mostrar los coeficientes del modelo para los componentes especificados
coef(modelo_pls, ncomp = 5, intercept = TRUE)

coefplot(modelo_pls, ncomp = 1:5)

# Hacer predicciones sobre el conjunto de datos X usando el modelo PLS ajustado
# El resultado de `predict` tiene una estructura de 3 dimensiones: [n_samples, n_components, n_responses]
pred <- predict(modelo_pls, X, ncomp = 5)
pred

# Mostrar los valores originales de Y
Y

dim(Y)

# Como `pred` tiene la forma [178, 5, 1], se accede a las predicciones para cada variable de respuesta
R2_values <- sapply(1:ncol(Y), function(i) {
  y_true <- Y[, i]
  y_pred <- pred[, i, 1]  # Accede a las predicciones en la tercera dimensión
  1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
})

# Mostrar los valores de R^2
names(R2_values) <- colnames(Y)
print(R2_values)

# Graficar los valores de R^2 para cada variable de respuesta

barplot(R2_values, main = "R^2 para cada variable respuesta", ylab = "R^2", xlab = "Variables Respuesta")
###################################################################################################################################
#Validación cruzada (b)
###################################################################################################################################
set.seed(1)

# Definir las variables de respuesta (C_N_mass, C_g_m2, H20_g_m2, LMA_g_m2, N_g_m2)
Y <- as.matrix(datos[, c("C_N_mass", "C_g_m2", "H20_g_m2", "LMA_g_m2", "N_g_m2")])

# Convertir la columna 'Species' en variables dummy
species_dummies <- model.matrix(~ Species_Code - 1, data = datos)

# Definir las variables predictoras (Wave_500 a Wave_2400)
X_W <- as.matrix(datos[, grep("^Wave_", names(datos))])

# Combinar las variables predictoras originales con las variables dummy de 'Species'
X <- cbind(X_W, species_dummies)

# Crear un índice para dividir los datos en 70% entrenamiento y 30% prueba
set.seed(1)
Indice_entrenamiento <- createDataPartition(Y[,1], p = 0.7, list = FALSE)

# Dividir los datos en conjunto de entrenamiento y prueba
X_train <- X[Indice_entrenamiento, ]
Y_train <- Y[Indice_entrenamiento, ]
X_test <- X[-Indice_entrenamiento, ]
Y_test <- Y[-Indice_entrenamiento, ]

# Ajustar el modelo de regresión PLS considerando 4 componentes y escalando (media de 0 y una desviación estándar de 1) las variables
modelo_pls <- plsr(Y_train ~ X_train, ncomp = 5, scale = TRUE, validation = "CV")

# Mostrar un resumen del modelo ajustado, incluyendo la varianza explicada
summary(modelo_pls)

# Generar el gráfico Elbow para evaluar el número óptimo de componentes en el conjunto de entrenamiento
rmsep_values <- RMSEP(modelo_pls, validation = "CV")
plot(rmsep_values, main = "Elbow Plot - RMSEP vs Número de componentes (Train Set)", 
     xlab = "Número de componentes", ylab = "RMSEP", type = "b")

# Mostrar los coeficientes del modelo para los componentes especificados
coef(modelo_pls, ncomp = 5, intercept = TRUE)
coefplot(modelo_pls, ncomp = 1:5)

# Hacer predicciones sobre el conjunto de prueba usando el modelo PLS ajustado
pred_test <- predict(modelo_pls, X_test, ncomp = 5)

# Calcular el coeficiente de determinación (R^2) para cada variable de respuesta en el conjunto de prueba
R2_values_test <- sapply(1:ncol(Y_test), function(i) {
  y_true <- Y_test[, i]
  y_pred <- pred_test[, i, 1]  # Accede a las predicciones en la tercera dimensión
  1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
})

# Mostrar los valores de R^2 para el conjunto de prueba
names(R2_values_test) <- colnames(Y_test)
print(R2_values_test)

# Graficar los valores de R^2 para cada variable de respuesta en el conjunto de prueba
barplot(R2_values_test, main = "R^2 para cada variable respuesta (Conjunto de Prueba)", ylab = "R^2", xlab = "Variables Respuesta")
###################################################################################################################################
###################################################################################################################################
#c)	Realice un reporte y compare con algunos resultados del articulo.
###################################################################################################################################
###################################################################################################################################

###################################################################################################################################
###################################################################################################################################
#d)	Suponga ahora N_g_m2 como variable respuesta; ajuste PLS, PCA, LASSO y RIDGE e indique cual genera el mejor modelo predictivo.
###################################################################################################################################
###################################################################################################################################
# Leer el archivo CSV en un data frame
datos <- read.csv("ely_plsr_data.csv", header = TRUE)
#---------------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------                       PLS                             ----------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------#
set.seed(1)

# Cargar la librería pls para realizar la regresión PLS
library(pls)

# Definir las variables de respuesta (C_N_mass, C_g_m2, H20_g_m2, LMA_g_m2, N_g_m2)
Y <- as.matrix(datos[, c("N_g_m2")])

# Definir las variables predictoras (Wave_500 a Wave_2400)
X <- as.matrix(datos[, grep("^Wave_", names(datos))])
dimensiones_X <- dim(X)

# Ajustar el modelo de regresión PLS considerando 2 componentes y escalando (media de 0 y una desviación estándar de 1) las variables
modelo_pls <- plsr(Y ~ X, ncomp = 5, scale = TRUE, validation = "CV")

# Mostrar un resumen del modelo ajustado, incluyendo la varianza explicada
summary(modelo_pls)

# Generar el gráfico Elbow para evaluar el número óptimo de componentes
rmsep_values <- RMSEP(modelo_pls, validation = "CV")
plot(rmsep_values, main = "Elbow Plot - RMSEP vs Número de componentes", 
     xlab = "Número de componentes", ylab = "RMSEP", type = "b")

# Opcional: Mostrar los coeficientes del modelo para los componentes especificados
coef(modelo_pls, ncomp = 5, intercept = TRUE)

coefplot(modelo_pls, ncomp = 1:5)

# Hacer predicciones sobre el conjunto de datos X usando el modelo PLS ajustado
# El resultado de `predict` tiene una estructura de 3 dimensiones: [n_samples, n_components, n_responses]
pred <- predict(modelo_pls, X, ncomp = 5)
pred

# Mostrar los valores originales de Y
Y

dim(Y)

# Calcular el coeficiente de determinación (R^2) para la variable de respuesta
y_true <- Y[, 1]         # Extraer los valores reales de la variable de respuesta
y_pred <- pred[, 1, 1]   # Extraer los valores predichos de la estructura de 3 dimensiones

# Calcular R^2 directamente
R2_value <- 1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)

# Mostrar el valor de R^2
print(paste("R^2:", round(R2_value, 4)))


barplot(R2_value, 
        main = expression(R^2 ~ "para la variable de respuesta"), 
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

#---------------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------                   COMPONENTES PRINCIPALES              ---------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------#

# Cargar la librería pls para realizar la regresión PCR
library(pls)
set.seed(1)

# Definir la variable de respuesta (N_g_m2)
Y <- as.matrix(datos[, c("N_g_m2")])

# Definir las variables predictoras (Wave_500 a Wave_2400)
X <- as.matrix(datos[, grep("^Wave_", names(datos))])
dimensiones_X <- dim(X)

# Ajustar el modelo de regresión PCR considerando 5 componentes y escalando las variables
modelo_pcr <- pcr(Y ~ X, ncomp = 5, validation = "LOO", jacknite=T) #LOO (Leave-One-Out Cross-Validation) es un tipo de validación cruzada que se utiliza para evaluar el rendimiento de un modelo predictivo. En esta técnica, se utiliza un solo dato del conjunto de datos como conjunto de prueba y el resto como conjunto de entrenamiento. Este proceso se repite de tal manera que cada observación se utiliza una vez como conjunto de prueba.

# Mostrar un resumen del modelo ajustado, incluyendo la varianza explicada
summary(modelo_pcr)

# Extraer los intervalos de confianza de los coeficientes
ci_coef <- confint(modelo_pcr, parm = "coefficients", ncomp = 2)
print(ci_coef)

# Generar el gráfico Elbow para evaluar el número óptimo de componentes
rmsep_values <- RMSEP(modelo_pcr, validation = "LOO")
plot(rmsep_values, main = "Elbow Plot - RMSEP vs Número de componentes", 
     xlab = "Número de componentes", ylab = "RMSEP", type = "b", col = "blue", lwd = 2, pch = 19)

# Opcional: Mostrar los coeficientes del modelo para los componentes especificados
coef(modelo_pcr, ncomp = 5, intercept = TRUE)

# Graficar los coeficientes del modelo para los componentes 1 a 5
coefplot(modelo_pcr, ncomp = 1:5)

# Hacer predicciones sobre el conjunto de datos X usando el modelo PCR ajustado
pred <- predict(modelo_pcr, X, ncomp = 5)
pred

dim(pred)
# Mostrar los valores originales de Y
Y

dim(Y)

# Calcular el coeficiente de determinación (R^2) para la variable de respuesta
y_true <- Y[, 1]         # Extraer los valores reales de la variable de respuesta
y_pred <- pred[, 1, 1]   # Extraer los valores predichos de la estructura de 3 dimensiones

# Calcular R^2 directamente
R2_value <- 1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)

# Mostrar el valor de R^2
print(paste("R^2:", round(R2_value, 4)))

barplot(R2_value, 
        main = expression(R^2 ~ "para la variable de respuesta"), 
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
