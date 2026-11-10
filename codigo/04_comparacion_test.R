# PROYECTO: DIAS DE HOSPITALIZACION
# ESTADISTICA APLICADA - CASO DE ESTUDIO 1
# SCRIPT 04: COMPARACION Y EVALUACION DE MODELOS
# INTEGRANTE 4: ISIDORA

# Este script debe ejecutarse desde la raiz del proyecto,
# despues de 01_auditoria_exploracion.R,
# 02_preparacion_lineales.R y 03_modelos_avanzados.R.

# 1. CARGAR PARTICIONES A Y B

load("datos/particiones_AyB.RData")

stopifnot(
  exists("datos_train_A"),
  exists("datos_val_A"),
  exists("datos_train_B"),
  exists("datos_val_B"),
  exists("mis_folds")
)

stopifnot(
  nrow(datos_train_A) == nrow(datos_train_B),
  nrow(datos_val_A) == nrow(datos_val_B),
  identical(datos_train_A$lengthofstay, datos_train_B$lengthofstay),
  identical(datos_val_A$lengthofstay, datos_val_B$lengthofstay)
)

# 2. FUNCIONES DE EVALUACION

ajustar_predicciones <- function(prediccion) {
  pmax(as.numeric(prediccion), 1)
}

funcion_mae <- function(real, predicho) {
  mean(abs(real - predicho), na.rm = TRUE)
}

funcion_rmse <- function(real, predicho) {
  sqrt(mean((real - predicho)^2, na.rm = TRUE))
}

funcion_r2 <- function(real, predicho) {
  error_modelo <- sum((real - predicho)^2, na.rm = TRUE)
  error_promedio <- sum(
    (real - mean(real, na.rm = TRUE))^2,
    na.rm = TRUE
  )
  1 - (error_modelo / error_promedio)
}

tabla <- function(nombre_modelo, enfoque, real, predicho) {
  predicho <- ajustar_predicciones(predicho)
  
  data.frame(
    Enfoque = enfoque,
    Modelo = nombre_modelo,
    RMSE = funcion_rmse(real, predicho),
    MAE = funcion_mae(real, predicho),
    R2 = funcion_r2(real, predicho),
    stringsAsFactors = FALSE
  )
}

# 3. BASELINE

promedio_train <- mean(
  datos_train_A$lengthofstay,
  na.rm = TRUE
)

pred_baseline_val <- rep(
  promedio_train,
  nrow(datos_val_A)
)

resultado_baseline_A <- tabla(
  "Baseline",
  "A",
  datos_val_A$lengthofstay,
  pred_baseline_val
)

resultado_baseline_B <- tabla(
  "Baseline",
  "B",
  datos_val_B$lengthofstay,
  pred_baseline_val
)

rmse_baseline <- resultado_baseline_A$RMSE
mae_baseline <- resultado_baseline_A$MAE

# 4. CARGAR PREDICCIONES LINEALES A Y B

pred_lineales_A <- read.csv(
  "resultados/predicciones_validacion_lineales_A.csv"
)

pred_lineales_B <- read.csv(
  "resultados/predicciones_validacion_lineales_B.csv"
)

columnas_lineales <- c(
  "real_lengthofstay",
  "pred_ols",
  "pred_ridge",
  "pred_lasso",
  "pred_elastic"
)

stopifnot(
  all(columnas_lineales %in% names(pred_lineales_A)),
  all(columnas_lineales %in% names(pred_lineales_B)),
  nrow(pred_lineales_A) == nrow(datos_val_A),
  nrow(pred_lineales_B) == nrow(datos_val_B)
)

evaluar_lineales <- function(predicciones, enfoque) {
  rbind(
    tabla(
      "OLS",
      enfoque,
      predicciones$real_lengthofstay,
      predicciones$pred_ols
    ),
    tabla(
      "Ridge",
      enfoque,
      predicciones$real_lengthofstay,
      predicciones$pred_ridge
    ),
    tabla(
      "Lasso",
      enfoque,
      predicciones$real_lengthofstay,
      predicciones$pred_lasso
    ),
    tabla(
      "Elastic Net",
      enfoque,
      predicciones$real_lengthofstay,
      predicciones$pred_elastic
    )
  )
}

resultados_lineales_A <- evaluar_lineales(
  pred_lineales_A,
  "A"
)

resultados_lineales_B <- evaluar_lineales(
  pred_lineales_B,
  "B"
)

# 5. CARGAR PREDICCIONES AVANZADAS A Y B

pred_avanzados_A <- read.csv(
  "resultados/modelos_avanzados_A/predicciones_validacion.csv"
)

pred_avanzados_B <- read.csv(
  "resultados/modelos_avanzados_B/predicciones_validacion.csv"
)

columnas_avanzados <- c(
  "lengthofstay_real",
  "pred_GAM",
  "pred_Arbol",
  "pred_RandomForest",
  "pred_GradientBoosting"
)

stopifnot(
  all(columnas_avanzados %in% names(pred_avanzados_A)),
  all(columnas_avanzados %in% names(pred_avanzados_B)),
  nrow(pred_avanzados_A) == nrow(datos_val_A),
  nrow(pred_avanzados_B) == nrow(datos_val_B)
)

evaluar_avanzados <- function(predicciones, enfoque) {
  rbind(
    tabla(
      "GAM",
      enfoque,
      predicciones$lengthofstay_real,
      predicciones$pred_GAM
    ),
    tabla(
      "Arbol",
      enfoque,
      predicciones$lengthofstay_real,
      predicciones$pred_Arbol
    ),
    tabla(
      "Random Forest",
      enfoque,
      predicciones$lengthofstay_real,
      predicciones$pred_RandomForest
    ),
    tabla(
      "Gradient Boosting",
      enfoque,
      predicciones$lengthofstay_real,
      predicciones$pred_GradientBoosting
    )
  )
}

resultados_avanzados_A <- evaluar_avanzados(
  pred_avanzados_A,
  "A"
)

resultados_avanzados_B <- evaluar_avanzados(
  pred_avanzados_B,
  "B"
)

# 6. AVERAGING DE MODELOS AVANZADOS PARA A Y B

crear_averaging <- function(predicciones, enfoque) {
  pred_promedio <- rowMeans(
    predicciones[, c(
      "pred_GAM",
      "pred_Arbol",
      "pred_RandomForest",
      "pred_GradientBoosting"
    )],
    na.rm = TRUE
  )
  
  pred_promedio <- ajustar_predicciones(pred_promedio)
  
  resultado <- tabla(
    "Promedio avanzados",
    enfoque,
    predicciones$lengthofstay_real,
    pred_promedio
  )
  
  list(
    predicciones = pred_promedio,
    resultado = resultado
  )
}

averaging_A <- crear_averaging(
  pred_avanzados_A,
  "A"
)

averaging_B <- crear_averaging(
  pred_avanzados_B,
  "B"
)

pred_promedio_A <- averaging_A$predicciones
pred_promedio_B <- averaging_B$predicciones

resultado_promedio_A <- averaging_A$resultado
resultado_promedio_B <- averaging_B$resultado

# 7. TABLA COMPLETA DE VALIDACION A Y B

tabla_comparacion <- rbind(
  resultado_baseline_A,
  resultados_lineales_A,
  resultados_avanzados_A,
  resultado_promedio_A,
  resultado_baseline_B,
  resultados_lineales_B,
  resultados_avanzados_B,
  resultado_promedio_B
)

tabla_comparacion$Mejora_RMSE <-
  rmse_baseline - tabla_comparacion$RMSE

tabla_comparacion$Mejora_MAE <-
  mae_baseline - tabla_comparacion$MAE

tabla_comparacion <- tabla_comparacion[
  order(tabla_comparacion$RMSE),
]

rownames(tabla_comparacion) <- NULL

print(tabla_comparacion)

write.csv(
  tabla_comparacion,
  "resultados/tabla_comparacion_AyB.csv",
  row.names = FALSE
)

# 8. VALIDACION CRUZADA DE MODELOS LINEALES

metricas_cv_lineales <- read.csv(
  "resultados/metricas_cv_lineales_AyB.csv"
)

metricas_cv_lineales$Enfoque <- ifelse(
  grepl("A", metricas_cv_lineales$Enfoque),
  "A",
  "B"
)

# 9. VALIDACION CRUZADA DE MODELOS AVANZADOS

metricas_cv_avanzados_A <- read.csv(
  "resultados/modelos_avanzados_A/metricas_validacion_cruzada.csv"
)

metricas_cv_avanzados_B <- read.csv(
  "resultados/modelos_avanzados_B/metricas_validacion_cruzada.csv"
)

metricas_cv_avanzados_A$Enfoque <- "A"
metricas_cv_avanzados_B$Enfoque <- "B"

metricas_cv_avanzados <- rbind(
  metricas_cv_avanzados_A,
  metricas_cv_avanzados_B
)

metricas_cv_avanzados$Modelo[
  metricas_cv_avanzados$Modelo == "RandomForest"
] <- "Random Forest"

metricas_cv_avanzados$Modelo[
  metricas_cv_avanzados$Modelo == "GradientBoosting"
] <- "Gradient Boosting"

# 10. VARIABILIDAD ENTRE FOLDS DE MODELOS AVANZADOS

obtener_variabilidad <- function(carpeta, enfoque) {
  
  nombres_archivo <- c(
    GAM = "modelo_GAM.rds",
    Arbol = "modelo_Arbol.rds",
    `Random Forest` = "modelo_RandomForest.rds",
    `Gradient Boosting` = "modelo_GradientBoosting.rds"
  )
  
  resultado <- data.frame()
  
  for (nombre in names(nombres_archivo)) {
    
    modelo <- readRDS(
      file.path(
        carpeta,
        nombres_archivo[[nombre]]
      )
    )
    
    fila <- data.frame(
      Enfoque = enfoque,
      Modelo = nombre,
      SD_RMSE = sd(
        modelo$resample$RMSE,
        na.rm = TRUE
      ),
      SD_MAE = sd(
        modelo$resample$MAE,
        na.rm = TRUE
      ),
      SD_R2 = sd(
        modelo$resample$Rsquared,
        na.rm = TRUE
      )
    )
    
    resultado <- rbind(
      resultado,
      fila
    )
  }
  
  resultado
}

variabilidad_avanzados_A <- obtener_variabilidad(
  "resultados/modelos_avanzados_A",
  "A"
)

variabilidad_avanzados_B <- obtener_variabilidad(
  "resultados/modelos_avanzados_B",
  "B"
)

variabilidad_avanzados <- rbind(
  variabilidad_avanzados_A,
  variabilidad_avanzados_B
)

metricas_cv_avanzados <- merge(
  metricas_cv_avanzados,
  variabilidad_avanzados,
  by = c("Enfoque", "Modelo"),
  all.x = TRUE
)

# 11. UNIR VALIDACION Y CV

validacion_sin_baseline <- tabla_comparacion[
  tabla_comparacion$Modelo != "Baseline" &
    tabla_comparacion$Modelo != "Promedio avanzados",
  c("Enfoque", "Modelo", "RMSE", "MAE", "R2")
]

cv_lineales_final <- metricas_cv_lineales[
  ,
  c(
    "Enfoque",
    "Modelo",
    "RMSE_CV",
    "SD_RMSE",
    "MAE_CV",
    "SD_MAE",
    "R2_CV",
    "SD_R2"
  )
]

cv_avanzados_final <- metricas_cv_avanzados[
  ,
  c(
    "Enfoque",
    "Modelo",
    "RMSE_CV",
    "SD_RMSE",
    "MAE_CV",
    "SD_MAE",
    "R2_CV",
    "SD_R2"
  )
]

metricas_cv_todos <- rbind(
  cv_lineales_final,
  cv_avanzados_final
)

comparacion_validacion_cv <- merge(
  validacion_sin_baseline,
  metricas_cv_todos,
  by = c("Enfoque", "Modelo"),
  all.x = TRUE
)

comparacion_validacion_cv$Diferencia_RMSE <-
  abs(
    comparacion_validacion_cv$RMSE -
      comparacion_validacion_cv$RMSE_CV
  )

comparacion_validacion_cv$Diferencia_MAE <-
  abs(
    comparacion_validacion_cv$MAE -
      comparacion_validacion_cv$MAE_CV
  )

comparacion_validacion_cv$Diferencia_R2 <-
  abs(
    comparacion_validacion_cv$R2 -
      comparacion_validacion_cv$R2_CV
  )

comparacion_validacion_cv <- comparacion_validacion_cv[
  order(comparacion_validacion_cv$RMSE),
]

rownames(comparacion_validacion_cv) <- NULL

print(comparacion_validacion_cv)

write.csv(
  comparacion_validacion_cv,
  "resultados/comparacion_validacion_cv_AyB.csv",
  row.names = FALSE
)

# 12. COMPARAR ENFOQUE A VS ENFOQUE B

candidatos_enfoque <- tabla_comparacion[
  tabla_comparacion$Modelo != "Baseline",
]

mejor_A <- candidatos_enfoque[
  candidatos_enfoque$Enfoque == "A",
]

mejor_A <- mejor_A[
  order(mejor_A$RMSE),
][1, ]

mejor_B <- candidatos_enfoque[
  candidatos_enfoque$Enfoque == "B",
]

mejor_B <- mejor_B[
  order(mejor_B$RMSE),
][1, ]

comparacion_enfoques <- rbind(
  mejor_A,
  mejor_B
)

comparacion_enfoques <- comparacion_enfoques[
  order(comparacion_enfoques$RMSE),
]

rownames(comparacion_enfoques) <- NULL

print(comparacion_enfoques)

write.csv(
  comparacion_enfoques,
  "resultados/comparacion_enfoques_A_vs_B.csv",
  row.names = FALSE
)

enfoque_ganador <- comparacion_enfoques$Enfoque[1]

cat(
  "\nENFOQUE SELECCIONADO EN VALIDACION:",
  enfoque_ganador,
  "\n"
)

# 13. COMPARACION FINAL SOLO DENTRO DEL ENFOQUE GANADOR

tabla_enfoque_ganador <- tabla_comparacion[
  tabla_comparacion$Enfoque == enfoque_ganador,
]

tabla_enfoque_ganador <- tabla_enfoque_ganador[
  order(tabla_enfoque_ganador$RMSE),
]

rownames(tabla_enfoque_ganador) <- NULL

print(tabla_enfoque_ganador)

write.csv(
  tabla_enfoque_ganador,
  "resultados/tabla_modelos_enfoque_ganador.csv",
  row.names = FALSE
)

candidatos_finales <- tabla_enfoque_ganador[
  tabla_enfoque_ganador$Modelo != "Baseline",
]

mejor_sistema <- candidatos_finales[
  order(candidatos_finales$RMSE),
][1, ]

nombre_mejor <- mejor_sistema$Modelo

cat(
  "\nSISTEMA FINAL SELECCIONADO EN VALIDACION:",
  nombre_mejor,
  "- ENFOQUE",
  enfoque_ganador,
  "\n"
)

write.csv(
  mejor_sistema,
  "resultados/mejor_sistema_validacion.csv",
  row.names = FALSE
)

# 14. OBTENER PREDICCIONES DE VALIDACION DEL SISTEMA FINAL

if (enfoque_ganador == "A") {
  pred_lineales_ganador <- pred_lineales_A
  pred_avanzados_ganador <- pred_avanzados_A
  pred_promedio_ganador <- pred_promedio_A
  datos_val_ganador <- datos_val_A
} else {
  pred_lineales_ganador <- pred_lineales_B
  pred_avanzados_ganador <- pred_avanzados_B
  pred_promedio_ganador <- pred_promedio_B
  datos_val_ganador <- datos_val_B
}

if (nombre_mejor == "OLS") {
  pred_final_val <- pred_lineales_ganador$pred_ols
} else if (nombre_mejor == "Ridge") {
  pred_final_val <- pred_lineales_ganador$pred_ridge
} else if (nombre_mejor == "Lasso") {
  pred_final_val <- pred_lineales_ganador$pred_lasso
} else if (nombre_mejor == "Elastic Net") {
  pred_final_val <- pred_lineales_ganador$pred_elastic
} else if (nombre_mejor == "GAM") {
  pred_final_val <- pred_avanzados_ganador$pred_GAM
} else if (nombre_mejor == "Arbol") {
  pred_final_val <- pred_avanzados_ganador$pred_Arbol
} else if (nombre_mejor == "Random Forest") {
  pred_final_val <- pred_avanzados_ganador$pred_RandomForest
} else if (nombre_mejor == "Gradient Boosting") {
  pred_final_val <- pred_avanzados_ganador$pred_GradientBoosting
} else if (nombre_mejor == "Promedio avanzados") {
  pred_final_val <- pred_promedio_ganador
} else {
  stop("No se reconoce el sistema final seleccionado.")
}

pred_final_val <- ajustar_predicciones(
  pred_final_val
)

# 15. ERROR POR SUBGRUPOS

evaluar_subgrupos <- function(real, predicho, grupo) {
  
  grupos <- unique(grupo)
  resultado <- data.frame()
  
  for (g in grupos) {
    
    indice <- grupo == g
    
    fila <- data.frame(
      Subgrupo = as.character(g),
      N = sum(indice, na.rm = TRUE),
      RMSE = funcion_rmse(
        real[indice],
        predicho[indice]
      ),
      MAE = funcion_mae(
        real[indice],
        predicho[indice]
      )
    )
    
    resultado <- rbind(
      resultado,
      fila
    )
  }
  
  resultado
}

error_genero <- evaluar_subgrupos(
  datos_val_ganador$lengthofstay,
  pred_final_val,
  datos_val_ganador$gender
)

error_facid <- evaluar_subgrupos(
  datos_val_ganador$lengthofstay,
  pred_final_val,
  datos_val_ganador$facid
)

error_rcount <- evaluar_subgrupos(
  datos_val_ganador$lengthofstay,
  pred_final_val,
  datos_val_ganador$rcount
)

write.csv(
  error_genero,
  "resultados/error_subgrupo_genero.csv",
  row.names = FALSE
)

write.csv(
  error_facid,
  "resultados/error_subgrupo_facid.csv",
  row.names = FALSE
)

write.csv(
  error_rcount,
  "resultados/error_subgrupo_rcount.csv",
  row.names = FALSE
)

# 16. INCERTIDUMBRE EN VALIDACION

errores_validacion <-
  datos_val_ganador$lengthofstay -
  pred_final_val

limites_error <- quantile(
  errores_validacion,
  probs = c(0.025, 0.975),
  na.rm = TRUE
)

print(limites_error)

# 17. IMPORTANCIA DE VARIABLES DEL ENFOQUE GANADOR

if (enfoque_ganador == "A") {
  carpeta_importancia <-
    "resultados/modelos_avanzados_A"
} else {
  carpeta_importancia <-
    "resultados/modelos_avanzados_B"
}

archivos_importancia <- c(
  GAM = "importancia_GAM.csv",
  Arbol = "importancia_Arbol.csv",
  `Random Forest` = "importancia_RandomForest.csv",
  `Gradient Boosting` = "importancia_GradientBoosting.csv"
)

top_importancias <- list()

for (nombre in names(archivos_importancia)) {
  
  ruta <- file.path(
    carpeta_importancia,
    archivos_importancia[[nombre]]
  )
  
  if (file.exists(ruta)) {
    
    importancia <- read.csv(ruta)
    
    if ("Overall" %in% names(importancia)) {
      
      importancia <- importancia[
        order(-importancia$Overall),
      ]
      
      top_importancias[[nombre]] <-
        head(importancia, 10)
    }
  }
}

# CAMBIO DE INTEGRACION MATIAS: El test esta desactivado por defecto.
# Activarlo solo cuando el equipo confirme que la seleccion quedo congelada.
if (isTRUE(EJECUTAR_TEST_FINAL)) {

# 18. CARGAR TEST SOLO DESPUES DE LA SELECCION

load("datos/test_congelado_AyB.RData")

stopifnot(
  exists("datos_test_A"),
  exists("datos_test_B")
)

if (enfoque_ganador == "A") {
  datos_test_ganador <- datos_test_A
  carpeta_modelos_ganador <-
    "resultados/modelos_avanzados_A"
} else {
  datos_test_ganador <- datos_test_B
  carpeta_modelos_ganador <-
    "resultados/modelos_avanzados_B"
}

# Preparacion consistente del test (sin recalcular estadisticos del test)
if ("glucose" %in% names(datos_test_ganador)) {
  datos_test_ganador$glucose[!is.na(datos_test_ganador$glucose) &
                              datos_test_ganador$glucose < 0] <- NA
}
if ("rcount" %in% names(datos_test_ganador)) {
  datos_test_ganador$rcount <- as.numeric(sub("+", "", as.character(datos_test_ganador$rcount), fixed = TRUE))
}
# Usar niveles categóricos obtenidos en entrenamiento, nunca del test
train_ref <- if (enfoque_ganador == "A") datos_train_A else datos_train_B
for (col in c("gender", "facid")) {
  if (col %in% names(datos_test_ganador) && col %in% names(train_ref)) {
    niveles <- levels(train_ref[[col]])
    if (is.null(niveles)) niveles <- sort(unique(as.character(train_ref[[col]])))
    datos_test_ganador[[col]] <- factor(as.character(datos_test_ganador[[col]]), levels = niveles)
    if (anyNA(datos_test_ganador[[col]])) {
      stop(paste("Nivel no visto o NA en test para", col,
                 "- revisar tratamiento de categorias antes de evaluar."))
    }
  }
}

# 19. GENERAR PREDICCION FINAL EN TEST

if (nombre_mejor %in%
    c("OLS", "Ridge", "Lasso", "Elastic Net")) {
  
  if (!exists("modelos_A") ||
      !exists("modelos_B")) {
    stop(
      paste(
        "Para evaluar un modelo lineal en test,",
        "primero debe ejecutarse",
        "02_preparacion_lineales.R",
        "en la misma sesion."
      )
    )
  }
  
  if (enfoque_ganador == "A") {
    modelos_lineales_ganador <- modelos_A
  } else {
    modelos_lineales_ganador <- modelos_B
  }
  
  if (nombre_mejor == "OLS") {
    modelo_final <- modelos_lineales_ganador$OLS
  } else if (nombre_mejor == "Ridge") {
    modelo_final <- modelos_lineales_ganador$Ridge
  } else if (nombre_mejor == "Lasso") {
    modelo_final <- modelos_lineales_ganador$Lasso
  } else {
    modelo_final <- modelos_lineales_ganador$ElasticNet
  }
  
  pred_final_test <- predict(
    modelo_final,
    newdata = datos_test_ganador,
    na.action = na.pass
  )
  
} else {
  
  modelo_gam_final <- readRDS(
    file.path(
      carpeta_modelos_ganador,
      "modelo_GAM.rds"
    )
  )
  
  modelo_arbol_final <- readRDS(
    file.path(
      carpeta_modelos_ganador,
      "modelo_Arbol.rds"
    )
  )
  
  modelo_rf_final <- readRDS(
    file.path(
      carpeta_modelos_ganador,
      "modelo_RandomForest.rds"
    )
  )
  
  modelo_gbm_final <- readRDS(
    file.path(
      carpeta_modelos_ganador,
      "modelo_GradientBoosting.rds"
    )
  )
  
  if (nombre_mejor == "GAM") {
    
    pred_final_test <- predict(
      modelo_gam_final,
      newdata = datos_test_ganador
    )
    
  } else if (nombre_mejor == "Arbol") {
    
    pred_final_test <- predict(
      modelo_arbol_final,
      newdata = datos_test_ganador
    )
    
  } else if (nombre_mejor == "Random Forest") {
    
    pred_final_test <- predict(
      modelo_rf_final,
      newdata = datos_test_ganador
    )
    
  } else if (nombre_mejor == "Gradient Boosting") {
    
    pred_final_test <- predict(
      modelo_gbm_final,
      newdata = datos_test_ganador
    )
    
  } else if (nombre_mejor == "Promedio avanzados") {
    
    pred_test_gam <- predict(
      modelo_gam_final,
      newdata = datos_test_ganador
    )
    
    pred_test_arbol <- predict(
      modelo_arbol_final,
      newdata = datos_test_ganador
    )
    
    pred_test_rf <- predict(
      modelo_rf_final,
      newdata = datos_test_ganador
    )
    
    pred_test_gbm <- predict(
      modelo_gbm_final,
      newdata = datos_test_ganador
    )
    
    pred_final_test <- rowMeans(
      data.frame(
        pred_test_gam,
        pred_test_arbol,
        pred_test_rf,
        pred_test_gbm
      ),
      na.rm = TRUE
    )
  }
}

pred_final_test <- ajustar_predicciones(
  pred_final_test
)

# 20. EVALUACION FINAL EN TEST

resultado_test <- tabla(
  nombre_mejor,
  enfoque_ganador,
  datos_test_ganador$lengthofstay,
  pred_final_test
)

pred_baseline_test <- rep(
  promedio_train,
  nrow(datos_test_ganador)
)

resultado_baseline_test <- tabla(
  "Baseline",
  enfoque_ganador,
  datos_test_ganador$lengthofstay,
  pred_baseline_test
)

comparacion_test <- rbind(
  resultado_baseline_test,
  resultado_test
)

comparacion_test$Mejora_RMSE <-
  resultado_baseline_test$RMSE -
  comparacion_test$RMSE

comparacion_test$Mejora_MAE <-
  resultado_baseline_test$MAE -
  comparacion_test$MAE

print(comparacion_test)

write.csv(
  comparacion_test,
  "resultados/resultado_final_test.csv",
  row.names = FALSE
)

# 21. INTERVALO EMPIRICO APROXIMADO EN TEST

limite_inferior_test <-
  pred_final_test +
  limites_error[1]

limite_superior_test <-
  pred_final_test +
  limites_error[2]

limite_inferior_test <- pmax(
  limite_inferior_test,
  1
)

cobertura_test <- mean(
  datos_test_ganador$lengthofstay >=
    limite_inferior_test &
    datos_test_ganador$lengthofstay <=
    limite_superior_test,
  na.rm = TRUE
)

predicciones_finales_test <- data.frame(
  lengthofstay_real =
    datos_test_ganador$lengthofstay,
  prediccion_final =
    pred_final_test,
  limite_inferior =
    limite_inferior_test,
  limite_superior =
    limite_superior_test
)

write.csv(
  predicciones_finales_test,
  "resultados/predicciones_sistema_final_test.csv",
  row.names = FALSE
)

# 22. GRAFICO DE IMPORTANCIA DEL MODELO GANADOR

if (nombre_mejor %in%
    names(top_importancias)) {
  
  importancia_grafico <-
    top_importancias[[nombre_mejor]]
  
  importancia_grafico <-
    importancia_grafico[
      order(importancia_grafico$Overall),
    ]
  
  png(
    "resultados/importancia_modelo_ganador.png",
    width = 1200,
    height = 800,
    res = 120
  )
  
  par(
    mar = c(5, 12, 4, 2)
  )
  
  barplot(
    importancia_grafico$Overall,
    names.arg =
      importancia_grafico$Variable,
    horiz = TRUE,
    las = 1,
    main = paste(
      "Importancia de variables -",
      nombre_mejor,
      "- Enfoque",
      enfoque_ganador
    ),
    xlab = "Importancia"
  )
  
  dev.off()
}

# 23. RESUMEN FINAL

cat("\n========================================\n")
cat("RESULTADO FINAL\n")
cat("========================================\n")

cat(
  "Enfoque seleccionado:",
  enfoque_ganador,
  "\n"
)

cat(
  "Sistema seleccionado:",
  nombre_mejor,
  "\n"
)

cat(
  "RMSE validacion:",
  round(mejor_sistema$RMSE, 4),
  "\n"
)

cat(
  "MAE validacion:",
  round(mejor_sistema$MAE, 4),
  "\n"
)

cat(
  "R2 validacion:",
  round(mejor_sistema$R2, 4),
  "\n"
)

cat(
  "\nRMSE test:",
  round(resultado_test$RMSE, 4),
  "\n"
)

cat(
  "MAE test:",
  round(resultado_test$MAE, 4),
  "\n"
)

cat(
  "R2 test:",
  round(resultado_test$R2, 4),
  "\n"
)

cat(
  "Mejora RMSE frente al baseline en test:",
  round(
    resultado_baseline_test$RMSE -
      resultado_test$RMSE,
    4
  ),
  "dias\n"
)

cat(
  "Cobertura aproximada del intervalo:",
  round(cobertura_test * 100, 2),
  "%\n"
)

cat("========================================\n")
} else {
  cat("\nTEST NO EJECUTADO. Seleccion completada exclusivamente con validacion.\n")
}
