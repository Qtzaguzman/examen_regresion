# Información: PLS con variables categóricas =================================
 
  # Regresión PLS que incluye variable categorica Species como predictor 
  # Variables respuesta: N_g_m2
  # Variables predictoras: Wave_500 , …, Wave_2400



# Cargar librerias =====================================

  cargar = TRUE
  if(cargar) {
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
  set.seed(3456)



# Ajustar modelo PLS ===================================

  # Definir las variables de respuesta
  variables <- c("N_g_m2")
  Y <- as.matrix(datos[, variables])

  # Definir las variables predictoras
  X <- as.matrix(datos[, grep("^Wave_", names(datos))])

  # Ajustar el modelo de regresión PLS considerando 5 componentes y escalando
  modelo_pls <- plsr(Y ~ X, ncomp = 5, scale = TRUE, validation = "CV")

  # Mostrar un resumen del modelo ajustado
  summary(modelo_pls)



# Explorar el modelo PLS =================================

  # Generar el gráfico Elbow para evaluar el número óptimo de componentes
  rmsep_values <- RMSEP(modelo_pls, validation = "CV")
  plot(rmsep_values, main = "Elbow - RMSEP vs Número de componentes", 
      xlab = "Número de componentes", ylab = "RMSEP", type = "b")

  # Coeficientes del modelo por componente
  coef(modelo_pls, ncomp = 5, intercept = TRUE)

  # Graficar coeficientes de regresión
  coefplot(modelo_pls, ncomp = 1:5)



# Predicción del modelo PLS ==============================

  predicciones <- predict(modelo_pls, ncomp = 5, newdata = X)

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



# Validación cruzada ====================================

  runValidacion = TRUE
  if (runValidacion) {
    # Crear un índice para dividir los datos en 70% entrenamiento y 30% prueba
    set.seed(345)
    index_train <- createDataPartition(Y[,1], p = 0.7, list = FALSE)

    # Dividir los datos en conjunto de entrenamiento y prueba
    X_train <- X[index_train, ]
    Y_train <- Y[index_train, ]
    X_test  <- X[-index_train, ]
    Y_test  <- Y[-index_train, ]

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

    #Función para calcular R^2 
    calcular_R2 <- function(y_true, y_pred) {
      # Calcular R^2 para un conjunto de predicciones y valores observados
      R2 <- 1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
      return(R2)
    }
    
    # Mostrar los valores de R^2
    print("R^2 para el conjunto de prueba:")
    R2_N_g_m2 <- calcular_R2(Y_test, pred_test)
    print(R2_N_g_m2)
  }

  