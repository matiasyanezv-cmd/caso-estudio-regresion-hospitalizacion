# CAMBIO DE INTEGRACION MATIAS: modelos_avanzados_A/B no pisan modelos lineales.
# ==========================================
# 1. LIBRERIAS
# ==========================================

library(caret)
library(gbm)
library(rpart)
library(ranger)
library(gam)

set.seed(2026)

# Carpetas de resultados separadas
carpeta_A <- "resultados/modelos_avanzados_A"
carpeta_B <- "resultados/modelos_avanzados_B"

dir.create(carpeta_A, recursive = TRUE, showWarnings = FALSE)
dir.create(carpeta_B, recursive = TRUE, showWarnings = FALSE)

# 2. CARGAR PARTICIONES A Y B
# ==========================================

objetos <- load("datos/particiones_AyB.RData")

requeridos <- c(
  "datos_train_A",
  "datos_val_A",
  "datos_train_B",
  "datos_val_B",
  "mis_folds"
)

if (!all(requeridos %in% objetos)) {
  stop("Faltan objetos en particiones_AyB.RData.")
}

# Crear copias independientes
entrenamiento_A <- datos_train_A
validacion_A <- datos_val_A

entrenamiento_B <- datos_train_B
validacion_B <- datos_val_B

# Comprobar que A y B tengan las mismas filas
stopifnot(
  nrow(entrenamiento_A) == nrow(entrenamiento_B),
  nrow(validacion_A) == nrow(validacion_B),
  identical(
    entrenamiento_A$lengthofstay,
    entrenamiento_B$lengthofstay
  ),
  identical(
    validacion_A$lengthofstay,
    validacion_B$lengthofstay
  )
)
stopifnot(
  identical(rownames(entrenamiento_A), rownames(entrenamiento_B)),
  identical(rownames(validacion_A), rownames(validacion_B))
)
# Comprobar que los folds correspondan al entrenamiento
if (any(unlist(mis_folds) > nrow(entrenamiento_A)) ||
    anyNA(unlist(mis_folds))) {
  stop("Los folds no coinciden con las filas de entrenamiento.")
}

cat("Particiones A y B cargadas correctamente.\n")
cat("Filas entrenamiento A:", nrow(entrenamiento_A), "\n")
cat("Filas validación A:", nrow(validacion_A), "\n")
cat("Filas entrenamiento B:", nrow(entrenamiento_B), "\n")
cat("Filas validación B:", nrow(validacion_B), "\n")
cat("Folds:", length(mis_folds), "\n")

# Guardar cantidad de filas originales
filas_originales_train_A <- nrow(entrenamiento_A)
filas_originales_train_B <- nrow(entrenamiento_B)

# 3. PREPARAR LOS CONJUNTOS A Y B
# ==========================================

# Función para comprobar que la variable objetivo sea válida
comprobar_objetivo <- function(datos, nombre) {
  
  objetivo <- datos$lengthofstay
  
  if (anyNA(objetivo) ||
      any(!is.finite(objetivo)) ||
      any(objetivo <= 0)) {
    
    stop(paste(
      "Hay valores invalidos en lengthofstay en",
      nombre
    ))
  }
}

# Preparar cada enfoque por separado
preparar_enfoque <- function(entrenamiento, validacion, nombre) {
  
  # Comprobar que exista la variable objetivo
  if (!"lengthofstay" %in% names(entrenamiento) ||
      !"lengthofstay" %in% names(validacion)) {
    stop(paste("Falta lengthofstay en el enfoque", nombre))
  }
  
  # Eliminar identificadores y variables no utilizadas
  variables_quitar <- c("eid", "discharged", "vdate")
  
  entrenamiento <- entrenamiento[
    , !(names(entrenamiento) %in% variables_quitar),
    drop = FALSE
  ]
  
  validacion <- validacion[
    , !(names(validacion) %in% variables_quitar),
    drop = FALSE
  ]
  
  # Convertir la variable objetivo a numérica
  entrenamiento$lengthofstay <- as.numeric(
    as.character(entrenamiento$lengthofstay)
  )
  
  validacion$lengthofstay <- as.numeric(
    as.character(validacion$lengthofstay)
  )
  
  # Comprobar que la variable objetivo sea válida
  comprobar_objetivo(entrenamiento, paste("entrenamiento", nombre))
  comprobar_objetivo(validacion, paste("validacion", nombre))
  
  # Tratar glucosa negativa como valor faltante
  
  if ("glucose" %in% names(entrenamiento) &&
      is.numeric(entrenamiento$glucose)) {
    
    entrenamiento$glucose[
      !is.na(entrenamiento$glucose) &
        entrenamiento$glucose < 0
    ] <- NA
  }
  
  if ("glucose" %in% names(validacion) &&
      is.numeric(validacion$glucose)) {
    
    validacion$glucose[
      !is.na(validacion$glucose) &
        validacion$glucose < 0
    ] <- NA
  }
  
  # Devolver ambos conjuntos preparados
  return(list(
    entrenamiento = entrenamiento,
    validacion = validacion
  ))
}

# Preparar enfoque A
datos_A <- preparar_enfoque(
  entrenamiento_A,
  validacion_A,
  "A"
)

entrenamiento_A <- datos_A$entrenamiento
validacion_A <- datos_A$validacion

# Preparar enfoque B
datos_B <- preparar_enfoque(
  entrenamiento_B,
  validacion_B,
  "B"
)

entrenamiento_B <- datos_B$entrenamiento
validacion_B <- datos_B$validacion

# Comprobar que no se hayan eliminado filas
if (nrow(entrenamiento_A) != filas_originales_train_A ||
    nrow(entrenamiento_B) != filas_originales_train_B) {
  
  stop("Se modificó la cantidad de filas de entrenamiento.")
}

cat("Enfoques A y B preparados correctamente.\n")
cat("Filas entrenamiento A:", nrow(entrenamiento_A), "\n")
cat("Filas validación A:", nrow(validacion_A), "\n")
cat("Filas entrenamiento B:", nrow(entrenamiento_B), "\n")
cat("Filas validación B:", nrow(validacion_B), "\n")

# 4. PREPARAR VARIABLES CATEGORICAS
# ==========================================
preparar_categoricas <- function(entrenamiento, validacion) {
  
  # Identificar variables categóricas del entrenamiento
  variables_categoricas <- names(entrenamiento)[
    sapply(
      entrenamiento,
      function(x) is.character(x) || is.factor(x)
    )
  ]
  
  variables_categoricas <- setdiff(
    variables_categoricas,
    "lengthofstay"
  )
  
  # Preparar cada variable categórica
  for (variable in variables_categoricas) {
    
    valores_train <- as.character(entrenamiento[[variable]])
    
    valores_train[
      is.na(valores_train) | trimws(valores_train) == ""
    ] <- "Sin_dato"
    
    niveles <- unique(valores_train)
    
    if (!"Sin_dato" %in% niveles) {
      niveles <- c(niveles, "Sin_dato")
    }
    
    entrenamiento[[variable]] <- factor(
      valores_train,
      levels = niveles
    )
    
    valores_val <- as.character(validacion[[variable]])
    
    valores_val[
      is.na(valores_val) | trimws(valores_val) == ""
    ] <- "Sin_dato"
    
    valores_val[
      !valores_val %in% niveles
    ] <- "Sin_dato"
    
    validacion[[variable]] <- factor(
      valores_val,
      levels = niveles
    )
  }
  
  return(list(
    entrenamiento = entrenamiento,
    validacion = validacion
  ))
}

# Preparar variables categóricas de A
datos_A <- preparar_categoricas(
  entrenamiento_A,
  validacion_A
)

entrenamiento_A <- datos_A$entrenamiento
validacion_A <- datos_A$validacion

# Preparar variables categóricas de B
datos_B <- preparar_categoricas(
  entrenamiento_B,
  validacion_B
)

entrenamiento_B <- datos_B$entrenamiento
validacion_B <- datos_B$validacion

cat("Variables categóricas de A y B preparadas.\n")

# 5. SELECCIONAR PREDICTORES
# ==========================================

# Identificar predictores de A
predictores_A <- setdiff(
  names(entrenamiento_A),
  "lengthofstay"
)

# Identificar predictores de B
predictores_B <- setdiff(
  names(entrenamiento_B),
  "lengthofstay"
)

# Comprobar que existan predictores
if (length(predictores_A) == 0 ||
    length(predictores_B) == 0) {
  
  stop("No quedaron predictores utilizables en A o B.")
}

# Comprobar que entrenamiento y validación tengan
# las mismas columnas en cada enfoque

if (!all(predictores_A %in% names(validacion_A)) ||
    !all(predictores_B %in% names(validacion_B))) {
  
  stop("Faltan predictores en los conjuntos de validación.")
}

# Conservar las mismas columnas y el mismo orden en A
entrenamiento_A <- entrenamiento_A[
  , c(predictores_A, "lengthofstay"),
  drop = FALSE
]

validacion_A <- validacion_A[
  , c(predictores_A, "lengthofstay"),
  drop = FALSE
]

# Conservar las mismas columnas y el mismo orden en B
entrenamiento_B <- entrenamiento_B[
  , c(predictores_B, "lengthofstay"),
  drop = FALSE
]

validacion_B <- validacion_B[
  , c(predictores_B, "lengthofstay"),
  drop = FALSE
]

cat("Predictores seleccionados correctamente.\n")
cat("Cantidad de predictores A:", length(predictores_A), "\n")
cat("Cantidad de predictores B:", length(predictores_B), "\n")

# ==========================================
# 6. LIMPIAR VALORES NUMERICOS
# ==========================================

# Función para limpiar valores numéricos
limpiar_numericos <- function(datos) {
  
  for (columna in names(datos)) {
    
    if (is.numeric(datos[[columna]]) &&
        columna != "lengthofstay") {
      
      datos[[columna]][
        !is.finite(datos[[columna]])
      ] <- NA
    }
  }
  
  return(datos)
}


# Limpiar los cuatro conjuntos
entrenamiento_A <- limpiar_numericos(entrenamiento_A)
validacion_A <- limpiar_numericos(validacion_A)

entrenamiento_B <- limpiar_numericos(entrenamiento_B)
validacion_B <- limpiar_numericos(validacion_B)


# Identificar predictores completamente vacíos
# utilizando solamente entrenamiento

eliminar_vacias <- function(entrenamiento, validacion, predictores) {
  
  vacias <- predictores[
    sapply(
      entrenamiento[, predictores, drop = FALSE],
      function(x) all(is.na(x))
    )
  ]
  
  predictores <- setdiff(predictores, vacias)
  
  if (length(predictores) == 0) {
    stop("Todos los predictores están vacíos.")
  }
  
  entrenamiento <- entrenamiento[
    , c(predictores, "lengthofstay"),
    drop = FALSE
  ]
  
  validacion <- validacion[
    , c(predictores, "lengthofstay"),
    drop = FALSE
  ]
  
  return(list(
    entrenamiento = entrenamiento,
    validacion = validacion,
    predictores = predictores
  ))
}


# Limpiar predictores del enfoque A
datos_A <- eliminar_vacias(
  entrenamiento_A,
  validacion_A,
  predictores_A
)

entrenamiento_A <- datos_A$entrenamiento
validacion_A <- datos_A$validacion
predictores_A <- datos_A$predictores


# Limpiar predictores del enfoque B
datos_B <- eliminar_vacias(
  entrenamiento_B,
  validacion_B,
  predictores_B
)

entrenamiento_B <- datos_B$entrenamiento
validacion_B <- datos_B$validacion
predictores_B <- datos_B$predictores


# Completar NA numéricos de validación
# usando las medianas del entrenamiento

imputar_validacion <- function(entrenamiento, validacion, predictores) {
  
  for (variable in predictores) {
    
    if (is.numeric(entrenamiento[[variable]])) {
      
      mediana <- median(
        entrenamiento[[variable]],
        na.rm = TRUE
      )
      
      if (is.finite(mediana)) {
        
        validacion[[variable]][
          is.na(validacion[[variable]])
        ] <- mediana
        
      }
    }
  }
  
  return(validacion)
}


# Aplicar imputación a A y B
validacion_A <- imputar_validacion(
  entrenamiento_A,
  validacion_A,
  predictores_A
)

validacion_B <- imputar_validacion(
  entrenamiento_B,
  validacion_B,
  predictores_B
)


if (nrow(entrenamiento_A) != filas_originales_train_A ||
    nrow(entrenamiento_B) != filas_originales_train_B ||
    nrow(validacion_A) != nrow(datos_val_A) ||
    nrow(validacion_B) != nrow(datos_val_B)) {

  stop("Se modificó la cantidad de filas.")

}


cat("Limpieza numérica completada.\n")
cat("Predictores A:", length(predictores_A), "\n")
cat("Predictores B:", length(predictores_B), "\n")
cat("Filas entrenamiento A:", nrow(entrenamiento_A), "\n")
cat("Filas entrenamiento B:", nrow(entrenamiento_B), "\n")
cat("Filas validación A:", nrow(validacion_A), "\n")
cat("Filas validación B:", nrow(validacion_B), "\n")

# 7. VALIDACION CRUZADA
# ==========================================

# Reutilizar los folds creados por el Integrante 2
grupos_cv <- mis_folds

# Comprobar que existan suficientes grupos
if (!is.list(grupos_cv) || length(grupos_cv) < 2) {
  stop("mis_folds debe contener al menos dos grupos.")
}

# Comprobar los índices
indices_cv <- unlist(grupos_cv)

if (length(indices_cv) == 0 ||
    anyNA(indices_cv) ||
    any(indices_cv < 1) ||
    any(indices_cv > nrow(entrenamiento_A)) ||
    any(indices_cv != as.integer(indices_cv))) {
  
  stop("Los índices de mis_folds no coinciden con entrenamiento.")
}

# Comprobar que A y B tengan la misma cantidad de filas
if (nrow(entrenamiento_A) != nrow(entrenamiento_B)) {
  stop("Los enfoques A y B tienen distintas cantidades de filas.")
}

# Cantidad de grupos
numero_grupos <- length(grupos_cv)

# Configurar validación cruzada
control_cv <- trainControl(
  method = "cv",
  number = numero_grupos,
  index = grupos_cv,
  savePredictions = "final"
)

# Preprocesamiento dentro de cada fold
preparacion <- c("zv", "medianImpute")

# Guardar los grupos utilizados
saveRDS(
  grupos_cv,
  file.path(carpeta_A, "grupos_validacion_cruzada.rds")
)

saveRDS(
  grupos_cv,
  file.path(carpeta_B, "grupos_validacion_cruzada.rds")
)

cat("Validación cruzada configurada correctamente.\n")
cat("Cantidad de grupos:", numero_grupos, "\n")


# 8. MODELO GAM CON SPLINES
# ==========================================

set.seed(2026)

#GAM para el enfoque A
modelo_gam_A <- train(
  lengthofstay ~ .,
  data = entrenamiento_A,
  method = "gamSpline",
  trControl = control_cv,
  preProcess = preparacion,
  tuneGrid = expand.grid(
    df = c(3, 5, 7)
  ),
  metric = "RMSE",
  maximize = FALSE
)

#GAM para el enfoque B
set.seed(2026)
modelo_gam_B <- train(
  lengthofstay ~ .,
  data = entrenamiento_B,
  method = "gamSpline",
  trControl = control_cv,
  preProcess = preparacion,
  tuneGrid = expand.grid(
    df = c(3, 5, 7)
  ),
  metric = "RMSE",
  maximize = FALSE
)
cat("Modelos GAM entrenados para A y B.\n")

# 9. ARBOL DE REGRESION
# ==========================================
#Arbol de regresion para A
set.seed(2026)

modelo_arbol_A <- train(
  lengthofstay ~ .,
  data = entrenamiento_A,
  method = "rpart",
  trControl = control_cv,
  preProcess = preparacion,
  tuneGrid = expand.grid(
    cp = c(0.001, 0.01, 0.05)
  ),
  metric = "RMSE",
  maximize = FALSE
)

#Arbol de regresion para B
set.seed(2026)

modelo_arbol_B <- train(
  lengthofstay ~ .,
  data = entrenamiento_B,
  method = "rpart",
  trControl = control_cv,
  preProcess = preparacion,
  tuneGrid = expand.grid(
    cp = c(0.001, 0.01, 0.05)
  ),
  metric = "RMSE",
  maximize = FALSE
)
cat("Árboles de regresión entrenados para A y B.\n")

# 10. RANDOM FOREST
# ==========================================

# Calcular cantidad de predictores para A
cantidad_predictores_A <- length(predictores_A)

valores_mtry_A <- unique(
  pmax(
    1,
    pmin(
      cantidad_predictores_A,
      c(
        floor(sqrt(cantidad_predictores_A)),
        floor(cantidad_predictores_A / 3),
        floor(cantidad_predictores_A / 2)
      )
    )
  )
)

grilla_rf_A <- expand.grid(
  mtry = valores_mtry_A,
  splitrule = "variance",
  min.node.size = 5
)

# Entrenar Random Forest para A
set.seed(2026)

modelo_rf_A <- train(
  lengthofstay ~ .,
  data = entrenamiento_A,
  method = "ranger",
  trControl = control_cv,
  preProcess = preparacion,
  tuneGrid = grilla_rf_A,
  num.trees = 100,
  importance = "permutation",
  metric = "RMSE",
  maximize = FALSE
)


# Calcular cantidad de predictores para B
cantidad_predictores_B <- length(predictores_B)

valores_mtry_B <- unique(
  pmax(
    1,
    pmin(
      cantidad_predictores_B,
      c(
        floor(sqrt(cantidad_predictores_B)),
        floor(cantidad_predictores_B / 3),
        floor(cantidad_predictores_B / 2)
      )
    )
  )
)

grilla_rf_B <- expand.grid(
  mtry = valores_mtry_B,
  splitrule = "variance",
  min.node.size = 5
)

# Entrenar Random Forest para B
set.seed(2026)

modelo_rf_B <- train(
  lengthofstay ~ .,
  data = entrenamiento_B,
  method = "ranger",
  trControl = control_cv,
  preProcess = preparacion,
  tuneGrid = grilla_rf_B,
  num.trees = 100,
  importance = "permutation",
  metric = "RMSE",
  maximize = FALSE
)

cat("Modelos Random Forest entrenados para A y B.\n")

# 11. GRADIENT BOOSTING
# ==========================================

set.seed(2026)

grilla_gbm <- expand.grid(
  interaction.depth = c(1, 3),
  n.trees = c(100, 200),
  shrinkage = c(0.05, 0.1),
  n.minobsinnode = 10
)

modelo_gbm_A <- train(
  lengthofstay ~ .,
  data = entrenamiento_A,
  method = "gbm",
  trControl = control_cv,
  preProcess = preparacion,
  tuneGrid = grilla_gbm,
  verbose = FALSE,
  metric = "RMSE",
  maximize = FALSE
)
set.seed(2026)
modelo_gbm_B <- train(
  lengthofstay ~ .,
  data = entrenamiento_B,
  method = "gbm",
  trControl = control_cv,
  preProcess = preparacion,
  tuneGrid = grilla_gbm,
  verbose = FALSE,
  metric = "RMSE",
  maximize = FALSE
)

# 12. GUARDAR LOS CUATRO MODELOS
# ==========================================

modelos_avanzados_A <- list(
  GAM = modelo_gam_A,
  Arbol = modelo_arbol_A,
  RandomForest = modelo_rf_A,
  GradientBoosting = modelo_gbm_A
)

modelos_avanzados_B <- list(
  GAM = modelo_gam_B,
  Arbol = modelo_arbol_B,
  RandomForest = modelo_rf_B,
  GradientBoosting = modelo_gbm_B
)

# Comprobar que estén los cuatro modelos de cada enfoque
if (length(modelos_avanzados_A) != 4 ||
    any(sapply(modelos_avanzados_A, is.null)) ||
    length(modelos_avanzados_B) != 4 ||
    any(sapply(modelos_avanzados_B, is.null))) {
  
  stop("No se entrenaron correctamente los cuatro modelos de A y B.")
}

# Guardar modelos del enfoque A
for (nombre in names(modelos_avanzados_A)) {
  
  saveRDS(
    modelos_avanzados_A[[nombre]],
    file.path(
      carpeta_A,
      paste0("modelo_", nombre, ".rds")
    )
  )
}

# Guardar modelos del enfoque B
for (nombre in names(modelos_avanzados_B)) {
  
  saveRDS(
    modelos_avanzados_B[[nombre]],
    file.path(
      carpeta_B,
      paste0("modelo_", nombre, ".rds")
    )
  )
}

cat("Los ocho modelos fueron guardados correctamente.\n")
# 13. FUNCION PARA CALCULAR METRICAS
# ==========================================

calcular_metricas <- function(real, prediccion) {
  
  if (length(real) != length(prediccion)) {
    stop("La cantidad de predicciones no coincide con los valores reales.")
  }
  
  if (any(!is.finite(real)) ||
      any(!is.finite(prediccion))) {
    stop("Existen predicciones o valores reales invalidos.")
  }
  
  rmse <- sqrt(mean((real - prediccion)^2))
  
  mae <- mean(abs(real - prediccion))
  
  if (length(real) > 1 &&
      sum((real - mean(real))^2) > 0) {
    
    r2 <- 1 -
      sum((real - prediccion)^2) /
      sum((real - mean(real))^2)
    
  } else {
    
    r2 <- NA_real_
  }
  
  data.frame(
    RMSE = rmse,
    MAE = mae,
    R2 = r2
  )
}


# 14. PREDICCIONES Y METRICAS DE VALIDACION
# ==========================================

# Función para evaluar los cuatro modelos de un enfoque
evaluar_modelos <- function(modelos, validacion, predictores) {
  
  # Guardar valores reales
  predicciones <- data.frame(
    lengthofstay_real = validacion$lengthofstay
  )
  
  metricas_validacion <- data.frame()
  
  # Evaluar cada modelo
  for (nombre in names(modelos)) {
    
    cat("Evaluando modelo:", nombre, "\n")
    
    prediccion <- predict(
      modelos[[nombre]],
      newdata = validacion[, predictores, drop = FALSE]
    )
    
    prediccion <- as.numeric(prediccion)
    
    # Comprobar cantidad de predicciones
    if (length(prediccion) != nrow(validacion)) {
      stop(paste(
        "Cantidad incorrecta de predicciones para",
        nombre
      ))
    }
    
    # La predicción no puede ser menor a un día
    prediccion <- pmax(1, prediccion)
    
    # Guardar predicciones
    columna <- paste0("pred_", nombre)
    predicciones[[columna]] <- prediccion
    
    # Calcular métricas
    metricas <- calcular_metricas(
      validacion$lengthofstay,
      prediccion
    )
    
    metricas$Modelo <- nombre
    
    metricas_validacion <- rbind(
      metricas_validacion,
      metricas[, c("Modelo", "RMSE", "MAE", "R2")]
    )
  }
  
  # Mantener el orden exacto de las columnas
  predicciones <- predicciones[
    ,
    c(
      "lengthofstay_real",
      "pred_GAM",
      "pred_Arbol",
      "pred_RandomForest",
      "pred_GradientBoosting"
    ),
    drop = FALSE
  ]
  
  # Ordenar métricas por RMSE
  metricas_validacion <- metricas_validacion[
    order(metricas_validacion$RMSE),
    ,
    drop = FALSE
  ]
  
  return(list(
    predicciones = predicciones,
    metricas = metricas_validacion
  ))
}

# Evaluar enfoque A
resultados_A <- evaluar_modelos(
  modelos_avanzados_A,
  validacion_A,
  predictores_A
)

# Evaluar enfoque B
resultados_B <- evaluar_modelos(
  modelos_avanzados_B,
  validacion_B,
  predictores_B
)


# Guardar resultados del enfoque A
write.csv(
  resultados_A$predicciones,
  file.path(carpeta_A, "predicciones_validacion.csv"),
  row.names = FALSE
)

write.csv(
  resultados_A$metricas,
  file.path(carpeta_A, "metricas_validacion.csv"),
  row.names = FALSE
)


# Guardar resultados del enfoque B
write.csv(
  resultados_B$predicciones,
  file.path(carpeta_B, "predicciones_validacion.csv"),
  row.names = FALSE
)

write.csv(
  resultados_B$metricas,
  file.path(carpeta_B, "metricas_validacion.csv"),
  row.names = FALSE
)

cat("Predicciones y métricas de validación guardadas para A y B.\n")

# 15. METRICAS DE VALIDACION CRUZADA
# ==========================================

# Función para obtener las métricas CV de los modelos
obtener_metricas_cv <- function(modelos) {
  
  metricas_cv <- data.frame()
  
  for (nombre in names(modelos)) {
    
    resultados <- modelos[[nombre]]$results
    mejores <- modelos[[nombre]]$bestTune
    
    # Identificar la fila correspondiente al mejor modelo
    fila_mejor <- rep(TRUE, nrow(resultados))
    
    for (columna in names(mejores)) {
      
      fila_mejor <- fila_mejor &
        resultados[[columna]] == mejores[[columna]][1]
    }
    
    fila <- resultados[fila_mejor, , drop = FALSE]
    
    if (nrow(fila) != 1) {
      stop(paste(
        "No se encontraron métricas CV para",
        nombre
      ))
    }
    
    # Guardar métricas
    metricas_cv <- rbind(
      metricas_cv,
      data.frame(
        Modelo = nombre,
        RMSE_CV = fila$RMSE,
        MAE_CV = if ("MAE" %in% names(fila)) {
          fila$MAE
        } else {
          NA_real_
        },
        R2_CV = if ("Rsquared" %in% names(fila)) {
          fila$Rsquared
        } else {
          NA_real_
        }
      )
    )
  }
  
  return(metricas_cv)
}


# Obtener métricas del enfoque A
metricas_cv_A <- obtener_metricas_cv(modelos_avanzados_A)

# Obtener métricas del enfoque B
metricas_cv_B <- obtener_metricas_cv(modelos_avanzados_B)


# Ordenar por RMSE
metricas_cv_A <- metricas_cv_A[
  order(metricas_cv_A$RMSE_CV),
  ,
  drop = FALSE
]

metricas_cv_B <- metricas_cv_B[
  order(metricas_cv_B$RMSE_CV),
  ,
  drop = FALSE
]


# Guardar métricas del enfoque A
write.csv(
  metricas_cv_A,
  file.path(carpeta_A, "metricas_validacion_cruzada.csv"),
  row.names = FALSE
)

# Guardar métricas del enfoque B
write.csv(
  metricas_cv_B,
  file.path(carpeta_B, "metricas_validacion_cruzada.csv"),
  row.names = FALSE
)

cat("Métricas de validación cruzada guardadas para A y B.\n")

# 16. IMPORTANCIA DE VARIABLES
# ==========================================

# Función para calcular y guardar la importancia
guardar_importancia <- function(modelos, carpeta) {
  
  for (nombre in names(modelos)) {
    
    cat("Calculando importancia de variables:", nombre, "\n")
    
    importancia <- tryCatch(
      varImp(modelos[[nombre]], scale = FALSE)$importance,
      error = function(e) {
        cat("No se pudo calcular la importancia para", nombre, "\n")
        NULL
      }
    )
    
    if (!is.null(importancia)) {
      
      importancia$Variable <- rownames(importancia)
      rownames(importancia) <- NULL
      
      write.csv(
        importancia,
        file.path(
          carpeta,
          paste0("importancia_", nombre, ".csv")
        ),
        row.names = FALSE
      )
    }
  }
}


# Guardar importancia de variables del enfoque A
guardar_importancia(
  modelos_avanzados_A,
  carpeta_A
)

# Guardar importancia de variables del enfoque B
guardar_importancia(
  modelos_avanzados_B,
  carpeta_B
)

cat("Importancia de variables guardada para A y B.\n")

# 17. MOSTRAR RESULTADOS
# ==========================================

cat("\n====================================\n")
cat("RESULTADOS DEL ENFOQUE A\n")
cat("====================================\n")

cat("\n===== METRICAS DE VALIDACION A =====\n")
print(resultados_A$metricas)

cat("\n===== METRICAS DE VALIDACION CRUZADA A =====\n")
print(metricas_cv_A)


cat("\n====================================\n")
cat("RESULTADOS DEL ENFOQUE B\n")
cat("====================================\n")

cat("\n===== METRICAS DE VALIDACION B =====\n")
print(resultados_B$metricas)

cat("\n===== METRICAS DE VALIDACION CRUZADA B =====\n")
print(metricas_cv_B)


cat("\n====================================\n")
cat("ARCHIVOS GUARDADOS\n")
cat("====================================\n")

cat("Enfoque A:", carpeta_A, "\n")
cat("Enfoque B:", carpeta_B, "\n")


# ==========================================
# 18. VISUALIZAR Y GUARDAR GRAFICOS
# ==========================================

library(rpart.plot)

# ------------------------------------------
# ENFOQUE A
# ------------------------------------------

# 1. GAM
png(
  file.path(carpeta_A, "grafico_GAM.png"),
  width = 1600,
  height = 1200
)

plot(modelos_avanzados_A$GAM$finalModel)

dev.off()


# 2. ARBOL DE REGRESION
png(
  file.path(carpeta_A, "grafico_Arbol.png"),
  width = 1800,
  height = 1400
)

rpart.plot(
  modelos_avanzados_A$Arbol$finalModel,
  type = 2,
  extra = 101,
  fallen.leaves = TRUE
)

dev.off()


# 3. RANDOM FOREST
png(
  file.path(carpeta_A, "grafico_RandomForest.png"),
  width = 1600,
  height = 1200
)

plot(
  varImp(modelos_avanzados_A$RandomForest, scale = FALSE),
  top = 15
)

dev.off()


# 4. GRADIENT BOOSTING
png(
  file.path(carpeta_A, "grafico_GradientBoosting.png"),
  width = 1600,
  height = 1200
)

plot(
  varImp(modelos_avanzados_A$GradientBoosting, scale = FALSE),
  top = 15
)

dev.off()


# ------------------------------------------
# ENFOQUE B
# ------------------------------------------

# 1. GAM
png(
  file.path(carpeta_B, "grafico_GAM.png"),
  width = 1600,
  height = 1200
)

plot(modelos_avanzados_B$GAM$finalModel)

dev.off()


# 2. ARBOL DE REGRESION
png(
  file.path(carpeta_B, "grafico_Arbol.png"),
  width = 1800,
  height = 1400
)

rpart.plot(
  modelos_avanzados_B$Arbol$finalModel,
  type = 2,
  extra = 101,
  fallen.leaves = TRUE
)

dev.off()


# 3. RANDOM FOREST
png(
  file.path(carpeta_B, "grafico_RandomForest.png"),
  width = 1600,
  height = 1200
)

plot(
  varImp(modelos_avanzados_B$RandomForest, scale = FALSE),
  top = 15
)

dev.off()


# 4. GRADIENT BOOSTING
png(
  file.path(carpeta_B, "grafico_GradientBoosting.png"),
  width = 1600,
  height = 1200
)

plot(
  varImp(modelos_avanzados_B$GradientBoosting, scale = FALSE),
  top = 15
)

dev.off()


cat("\n====================================\n")
cat("GRAFICOS GUARDADOS CORRECTAMENTE\n")
cat("====================================\n")

cat("Enfoque A:", carpeta_A, "\n")
cat("Enfoque B:", carpeta_B, "\n")

