
# Información: PLS con variables categóricas =================================
 
  # Regresión PLS que incluye variable categorica Species como predictor 
  # Variables respuesta: N_g_m2
  # Variables predictoras: Wave_500 , …, Wave_2400



# Cargar librerias =====================================
  load_librerias = FALSE
  if (load_librerias == TRUE) {
    load <- c("gstat", "ggplot2", "dplyr", "tidyr", "spdep", "pls", "caret")
    lapply(load, require, character.only = TRUE)
  }

  # Establecer el directorio activo
  setwd("~/Projects/examen_regresion/Problema 1")



# Cargar datos ========================================
  datos <- read.csv("data/ely_plsr_data.csv")

  # Visualizar los datos
  head(datos)
  str(datos)

  # Establecer la semilla para la reproducibilidad
  set.seed(3456)



# Ajustar modelo PCA ===================================

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



# Explorar el modelo PCA =================================

  # Generar el gráfico Elbow para evaluar el número óptimo de componentes
  rmsep_values <- RMSEP(modelo_pca, validation = "LOO")
  plot(rmsep_values, main = "Elbow - RMSEP vs Número de componentes", 
      xlab = "Número de componentes", ylab = "RMSEP", type = "b", col = "blue", lwd = 2, pch = 19)

  # Mostrar los coeficientes del modelo para los componentes principales
  coef(modelo_pca, ncomp = 5, intercept = TRUE)

  # Graficar los coeficientes del modelo para los componentes 1 a 5
  coefplot(modelo_pca, ncomp = 1:5)



# Predicción del modelo PCA ==============================

  # Hacer predicciones sobre el conjunto de datos X usando el modelo PCA ajustado
  predicciones <- predict(modelo_pca, X, ncomp = 5)

  # Generar gráfico de predicciones vs observaciones con ggplot
  datos_predicciones <- data.frame(Observado = Y, Predicho = predicciones)
  names(datos_predicciones)

  # Crear el gráfico
  p <- ggplot(datos_predicciones, aes(x = Observado, y = Y.5.comps)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    labs(title = "N_g_m2: Predicciones vs Observaciones", x = "Observado", y = "Y.5.comps")

  print(p)



# Calcular coeficiente de determinación (R^2) ============

  # Definir función para calcular R^2 para cada variable
  get_listaR2 <- function(Y, predicciones) {
    R2_values <- sapply(1:ncol(Y), function(i) {
      y_true <- Y[, i]
      y_pred <- predicciones[, i, 1]  # Accede a las predicciones en la tercera dimensión
      1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
    })
    
    # Asignar nombres a los valores de R^2 basados en los nombres de las columnas de Y
    names(R2_values) <- colnames(Y)
    return(R2_values)
  }

  # Mostrar los valores de R^2
  R2_values <- get_listaR2(Y, predicciones)
  print(R2_values)

  # Visualizar los valores de R^2 en un gráfico de barras
  barplot(R2_value, 
    main = expression(R^2 ~ " variable de respuesta"), 
    ylab = expression(R^2), 
    xlab = "Variable Respuesta", 
    col = "dodgerblue",  
    border = "white",    
    names.arg = colnames(Y),  
    ylim = c(0, 1),  
    cex.main = 1.5,   
    cex.lab = 1.2,    
    cex.names = 1.2)  

  # Añadir valores numéricos de R^2 sobre la barra
  text(x = 1, y = R2_value, labels = round(R2_value, 3), pos = 3, cex = 1.2, col = "black")
