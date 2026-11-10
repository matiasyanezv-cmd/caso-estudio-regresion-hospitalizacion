# ==========================================
# PROYECTO: DIAS DE HOSPITALIZACION
# ESTADISTICA APLICADA - CASO DE ESTUDIO 1
# SCRIPT 02: PREPARACION Y MODELOS LINEALES (ENFOQUES A Y B)
# INTEGRANTE 2: JOAQUIN
# ==========================================

library(caret)
library(glmnet)
library(dplyr)
library(car)

# ==========================================
# 1. CARGA DE DATOS Y ENFOQUES (DE FRANCISCA)
# ==========================================
# Cargar los datos crudos
datos <- read.csv("datos/LengthOfStay.csv")

# Cargar la definición de variables que guardó Francisca
enfoques <- readRDS("datos/definicion_enfoques.rds")

variable_objetivo <- enfoques$objetivo
predictores_A <- enfoques$enfoque_A
predictores_B <- enfoques$enfoque_B

# ==========================================
# 2. LIMPIEZA BÁSICA
# ==========================================
# Filtrar glucosa negativa
datos$glucose[datos$glucose < 0] <- NA

# Ajuste de tipos
datos$gender <- as.factor(datos$gender)
if("facid" %in% names(datos)) {
  datos$facid <- as.factor(datos$facid)
}

# Corrección de variable ordinal (rcount)
datos$rcount <- as.numeric(sub("+", "", datos$rcount, fixed = TRUE))

# ==========================================
# 3. CREACIÓN DE CONJUNTOS A Y B
# ==========================================
# Enfoque A: Solo clínicas y generales
datos_A <- datos %>% select(all_of(predictores_A), all_of(variable_objetivo))

# Enfoque B: Clínicas, generales y enfermedades
datos_B <- datos %>% select(all_of(predictores_B), all_of(variable_objetivo))

# ==========================================
# 4. PARTICIÓN DE LOS DATOS Y FOLDS
# ==========================================
# Hacemos la partición usando los índices para que A y B tengan las mismas filas exactas
set.seed(2026) 
indice_test <- createDataPartition(datos[[variable_objetivo]], p = 0.15, list = FALSE)

# Test congelado
datos_test_A <- datos_A[indice_test, ]
datos_test_B <- datos_B[indice_test, ]

# Resto de los datos
resto_A <- datos_A[-indice_test, ]
resto_B <- datos_B[-indice_test, ]

# Validación interna
set.seed(2026)
indice_val <- createDataPartition(resto_A[[variable_objetivo]], p = 0.1765, list = FALSE)

datos_val_A <- resto_A[indice_val, ]
datos_val_B <- resto_B[indice_val, ]

datos_train_A <- resto_A[-indice_val, ]
datos_train_B <- resto_B[-indice_val, ]

# Folds de Validación Cruzada
set.seed(2026)
mis_folds <- createFolds(datos_train_A[[variable_objetivo]], k = 5, returnTrain = TRUE)

# Verificación de fugas
stopifnot(
  "Fuga Train-Val" = length(intersect(rownames(datos_train_A), rownames(datos_val_A))) == 0,
  "Fuga Train-Test" = length(intersect(rownames(datos_train_A), rownames(datos_test_A))) == 0,
  "Fuga Val-Test" = length(intersect(rownames(datos_val_A), rownames(datos_test_A))) == 0
)

# Exportación segura de las particiones conjuntas
save(datos_train_A, datos_val_A, datos_train_B, datos_val_B, mis_folds, file = "datos/particiones_AyB.RData")
save(datos_test_A, datos_test_B, file = "datos/test_congelado_AyB.RData")
cat("\n¡Particiones A y B aisladas y guardadas sin fugas!\n")

# ==========================================
# 5. MODELO BASELINE (Igual para A y B)
# ==========================================
pred_media_train <- mean(datos_train_A[[variable_objetivo]])
pred_baseline_val <- rep(pred_media_train, nrow(datos_val_A))

rmse_baseline <- sqrt(mean((datos_val_A[[variable_objetivo]] - pred_baseline_val)^2))
mae_baseline <- mean(abs(datos_val_A[[variable_objetivo]] - pred_baseline_val))

# ==========================================
# 6. CONFIGURACIÓN CV Y ENTRENAMIENTO
# ==========================================
control_cv <- trainControl(method = "cv", index = mis_folds, savePredictions = "final")
preproc_steps <- c("zv", "medianImpute", "center", "scale") 
grid_lambda <- 10^seq(-5, -1, length = 20)

# Función para entrenar la familia lineal sin repetir código
entrenar_familia_lineal <- function(train_data) {
  
  set.seed(2026)
  ols <- train(lengthofstay ~ ., data = train_data, method = "lm",
               preProcess = preproc_steps, trControl = control_cv, na.action = na.pass)
  
  set.seed(2026)
  ridge <- train(lengthofstay ~ ., data = train_data, method = "glmnet",
                 preProcess = preproc_steps, tuneGrid = expand.grid(alpha = 0, lambda = grid_lambda),
                 trControl = control_cv, na.action = na.pass)
  
  set.seed(2026)
  lasso <- train(lengthofstay ~ ., data = train_data, method = "glmnet",
                 preProcess = preproc_steps, tuneGrid = expand.grid(alpha = 1, lambda = grid_lambda),
                 trControl = control_cv, na.action = na.pass)
  
  set.seed(2026)
  en <- train(lengthofstay ~ ., data = train_data, method = "glmnet",
              preProcess = preproc_steps, tuneGrid = expand.grid(alpha = c(0.25, 0.5, 0.75), lambda = grid_lambda),
              trControl = control_cv, na.action = na.pass)
  
  list(OLS = ols, Ridge = ridge, Lasso = lasso, ElasticNet = en)
}

cat("\nEntrenando modelos para Enfoque A...\n")
modelos_A <- entrenar_familia_lineal(datos_train_A)

cat("Entrenando modelos para Enfoque B...\n")
modelos_B <- entrenar_familia_lineal(datos_train_B)

# Diagnóstico de multicolinealidad obligatorio
cat("\n=== DIAGNÓSTICO OLS: MULTICOLINEALIDAD VIF (ENFOQUE B) ===\n")
print(car::vif(modelos_B$OLS$finalModel))

# ==========================================
# 7. EVALUACIÓN DE PREDICCIONES
# ==========================================
# Función para predecir con límite clínico mínimo de 1 día
obtener_predicciones <- function(lista_modelos, datos_eval) {
  data.frame(
    real_lengthofstay = datos_eval[[variable_objetivo]],
    pred_ols = pmax(as.numeric(predict(lista_modelos$OLS, newdata = datos_eval, na.action = na.pass)), 1),
    pred_ridge = pmax(as.numeric(predict(lista_modelos$Ridge, newdata = datos_eval, na.action = na.pass)), 1),
    pred_lasso = pmax(as.numeric(predict(lista_modelos$Lasso, newdata = datos_eval, na.action = na.pass)), 1),
    pred_elastic = pmax(as.numeric(predict(lista_modelos$ElasticNet, newdata = datos_eval, na.action = na.pass)), 1)
  )
}

preds_val_A <- obtener_predicciones(modelos_A, datos_val_A)
preds_val_B <- obtener_predicciones(modelos_B, datos_val_B)

# Guardar predicciones de validación
write.csv(preds_val_A, "resultados/predicciones_validacion_lineales_A.csv", row.names = FALSE)
write.csv(preds_val_B, "resultados/predicciones_validacion_lineales_B.csv", row.names = FALSE)

# ==========================================
# 8. MÉTRICAS CV
# ==========================================
extraer_metricas_cv <- function(modelo, nombre, enfoque) {
  resumen <- modelo$results
  if("alpha" %in% names(resumen)) {
    resumen <- resumen[resumen$alpha == modelo$bestTune$alpha & resumen$lambda == modelo$bestTune$lambda, ]
  }
  data.frame(
    Enfoque = enfoque,
    Modelo = nombre,
    RMSE_CV = resumen$RMSE,
    SD_RMSE = resumen$RMSESD,
    MAE_CV = resumen$MAE,
    SD_MAE = resumen$MAESD,
    R2_CV = resumen$Rsquared,
    SD_R2 = resumen$RsquaredSD
  )
}

tabla_cv <- rbind(
  extraer_metricas_cv(modelos_A$OLS, "OLS", "Enfoque A"),
  extraer_metricas_cv(modelos_A$Ridge, "Ridge", "Enfoque A"),
  extraer_metricas_cv(modelos_A$Lasso, "Lasso", "Enfoque A"),
  extraer_metricas_cv(modelos_A$ElasticNet, "Elastic Net", "Enfoque A"),
  
  extraer_metricas_cv(modelos_B$OLS, "OLS", "Enfoque B"),
  extraer_metricas_cv(modelos_B$Ridge, "Ridge", "Enfoque B"),
  extraer_metricas_cv(modelos_B$Lasso, "Lasso", "Enfoque B"),
  extraer_metricas_cv(modelos_B$ElasticNet, "Elastic Net", "Enfoque B")
)

write.csv(tabla_cv, "resultados/metricas_cv_lineales_AyB.csv", row.names = FALSE)
cat("\n¡Predicciones y métricas CV exportadas exitosamente!\n")

