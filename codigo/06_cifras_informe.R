# =============================================================
# PROYECTO: DIAS DE HOSPITALIZACION
# SCRIPT 06: CIFRAS Y TABLAS DE RESPALDO PARA INFORME Y PPT
# Lo ejecuta main.R despues de 00-04. Tambien puede correrse solo,
# desde la raiz del proyecto, una vez que 01-04 ya se ejecutaron.
# No modifica ningun resultado previo ni participa en la seleccion:
# el test solo se usa para REPORTAR el sistema ya elegido en 04.
# Todo se guarda en resultados/cifras_informe/
# =============================================================

invisible(lapply(c("caret", "glmnet", "car", "gam", "rpart", "ranger", "gbm"),
                 library, character.only = TRUE))

set.seed(2026)
dir_out <- "resultados/cifras_informe"
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
guardar <- function(x, nombre) {
  write.csv(x, file.path(dir_out, nombre), row.names = FALSE)
}

# -------------------------------------------------------------
# 1. DESCRIPTIVOS DEL DATASET COMPLETO
# -------------------------------------------------------------
datos <- read.csv("datos/LengthOfStay.csv")
datos$rcount_num <- as.numeric(sub("+", "", datos$rcount, fixed = TRUE))

guardar(data.frame(
  n_filas        = nrow(datos),
  n_columnas     = ncol(datos) - 1,
  faltantes      = sum(is.na(datos)),
  filas_duplic   = sum(duplicated(datos[, setdiff(names(datos), "rcount_num")])),
  eid_duplic     = sum(duplicated(datos$eid)),
  glucosa_neg    = sum(datos$glucose < 0, na.rm = TRUE),
  los_media      = mean(datos$lengthofstay),
  los_mediana    = median(datos$lengthofstay),
  los_sd         = sd(datos$lengthofstay),
  los_min        = min(datos$lengthofstay),
  los_max        = max(datos$lengthofstay),
  vdate_min      = tryCatch(as.character(min(as.Date(datos$vdate, tryFormats = c("%m/%d/%Y", "%Y-%m-%d")))), error = function(e) NA),
  vdate_max      = tryCatch(as.character(max(as.Date(datos$vdate, tryFormats = c("%m/%d/%Y", "%Y-%m-%d")))), error = function(e) NA)
), "01_descriptivos.csv")

guardar(as.data.frame(table(dias = datos$lengthofstay)), "02_histograma_los.csv")

resumen_grupo <- function(var) {
  g <- datos[[var]]
  data.frame(
    variable = var,
    nivel    = names(tapply(datos$lengthofstay, g, mean)),
    n        = as.vector(table(g)),
    media    = as.vector(tapply(datos$lengthofstay, g, mean)),
    sd       = as.vector(tapply(datos$lengthofstay, g, sd))
  )
}
vars_grupo <- c("gender", "facid", "rcount_num",
                "dialysisrenalendstage", "asthma", "irondef", "pneum",
                "substancedependence", "psychologicaldisordermajor",
                "depress", "psychother", "fibrosisandother",
                "malnutrition", "hemo")
guardar(do.call(rbind, lapply(vars_grupo, resumen_grupo)),
        "03_media_los_por_grupo.csv")

# -------------------------------------------------------------
# 2. RELACION ESTIMADA CON INTERVALO (genero, igual que 01)
# -------------------------------------------------------------
tt <- t.test(datos$lengthofstay[datos$gender == "M"],
             datos$lengthofstay[datos$gender == "F"],
             conf.level = 0.95)
dif <- unname(tt$estimate[1] - tt$estimate[2])
guardar(data.frame(
  media_M = unname(tt$estimate[1]), media_F = unname(tt$estimate[2]),
  diferencia = dif, ic_inf = tt$conf.int[1], ic_sup = tt$conf.int[2],
  p_valor = tt$p.value,
  d_cohen = dif / sd(datos$lengthofstay)
), "04_ic_genero.csv")

# -------------------------------------------------------------
# 3. PARTICIONES ACTUALES
# -------------------------------------------------------------
load("datos/particiones_AyB.RData")
load("datos/test_congelado_AyB.RData")
guardar(data.frame(
  conjunto = c("train", "validacion", "test"),
  n = c(nrow(datos_train_B), nrow(datos_val_B), nrow(datos_test_B)),
  media_los = c(mean(datos_train_B$lengthofstay),
                mean(datos_val_B$lengthofstay),
                mean(datos_test_B$lengthofstay)),
  n_folds = length(mis_folds)
), "05_particiones.csv")

# -------------------------------------------------------------
# 4. OLS ENFOQUE B: coeficientes con IC, VIF y residuos
#    (imputacion con mediana del TRAIN; mismo dato que 02)
# -------------------------------------------------------------
preparar_lm <- function(d, ref) {
  d$glucose[!is.na(d$glucose) & d$glucose < 0] <- NA
  for (v in names(d)) if (is.numeric(d[[v]]) && v != "lengthofstay") {
    d[[v]][is.na(d[[v]])] <- median(ref[[v]], na.rm = TRUE)
  }
  d
}
train_lm <- preparar_lm(datos_train_B, datos_train_B)
val_lm   <- preparar_lm(datos_val_B, datos_train_B)
ols_B <- lm(lengthofstay ~ ., data = train_lm)

coefs <- cbind(coef(ols_B), confint(ols_B))
guardar(data.frame(termino = rownames(coefs), coef = coefs[, 1],
                   ic_inf = coefs[, 2], ic_sup = coefs[, 3],
                   p_valor = summary(ols_B)$coefficients[rownames(coefs), 4]),
        "06_ols_B_coeficientes.csv")

v <- car::vif(ols_B)
if (is.matrix(v)) {
  vif_df <- data.frame(variable = rownames(v), gvif_ajustado = v[, ncol(v)]^2)
} else {
  vif_df <- data.frame(variable = names(v), gvif_ajustado = v)
}
guardar(vif_df[order(-vif_df$gvif_ajustado), ], "07_vif_ols_B.csv")

pred_ols_val <- pmax(1, predict(ols_B, newdata = val_lm))
res_ols <- val_lm$lengthofstay - pred_ols_val
guardar(data.frame(
  ols_r2_ajustado = summary(ols_B)$adj.r.squared,
  shapiro_p_muestra5000 = shapiro.test(sample(residuals(ols_B), 5000))$p.value,
  bp_cor_absres_ajustado = cor(abs(residuals(ols_B)), fitted(ols_B))
), "08_diagnostico_ols.csv")

png(file.path(dir_out, "diagnostico_residuos_ols_B.png"), width = 1600, height = 800, res = 150)
par(mfrow = c(1, 2))
plot(fitted(ols_B), residuals(ols_B), pch = ".", col = rgb(0, 0, 0, 0.2),
     main = "OLS (B): residuos vs ajustados", xlab = "Ajustado (dias)", ylab = "Residuo (dias)")
abline(h = 0, col = "red")
qqnorm(sample(residuals(ols_B), 5000), pch = ".", main = "QQ-plot residuos (muestra 5000)")
qqline(residuals(ols_B), col = "red")
dev.off()

# -------------------------------------------------------------
# 5. HIPERPARAMETROS SELECCIONADOS POR CV
# -------------------------------------------------------------
control_cv <- trainControl(method = "cv", index = mis_folds)
prep <- c("zv", "medianImpute", "center", "scale")
grid_lambda <- 10^seq(-5, -1, length = 20)
set.seed(2026)
ridge_B <- train(lengthofstay ~ ., data = datos_train_B, method = "glmnet",
                 preProcess = prep, trControl = control_cv, na.action = na.pass,
                 tuneGrid = expand.grid(alpha = 0, lambda = grid_lambda))
set.seed(2026)
lasso_B <- train(lengthofstay ~ ., data = datos_train_B, method = "glmnet",
                 preProcess = prep, trControl = control_cv, na.action = na.pass,
                 tuneGrid = expand.grid(alpha = 1, lambda = grid_lambda))
set.seed(2026)
en_B <- train(lengthofstay ~ ., data = datos_train_B, method = "glmnet",
              preProcess = prep, trControl = control_cv, na.action = na.pass,
              tuneGrid = expand.grid(alpha = c(0.25, 0.5, 0.75), lambda = grid_lambda))

hp <- function(nombre, modelo) {
  bt <- modelo$bestTune
  data.frame(modelo = nombre,
             hiperparametros = paste(names(bt), signif(unlist(bt), 4), sep = "=", collapse = "; "))
}
dir_B <- "resultados/modelos_avanzados_B"
gam_B   <- readRDS(file.path(dir_B, "modelo_GAM.rds"))
arbol_B <- readRDS(file.path(dir_B, "modelo_Arbol.rds"))
rf_B    <- readRDS(file.path(dir_B, "modelo_RandomForest.rds"))
gbm_B   <- readRDS(file.path(dir_B, "modelo_GradientBoosting.rds"))

guardar(rbind(hp("Ridge", ridge_B), hp("Lasso", lasso_B), hp("Elastic Net", en_B),
              hp("GAM", gam_B), hp("Arbol", arbol_B),
              hp("Random Forest", rf_B), hp("Gradient Boosting", gbm_B)),
        "09_hiperparametros_B.csv")

# Lasso: cuantos coeficientes quedan en cero
coef_lasso <- as.matrix(coef(lasso_B$finalModel, s = lasso_B$bestTune$lambda))
guardar(data.frame(coef_no_cero = sum(coef_lasso[-1, 1] != 0),
                   coef_total = nrow(coef_lasso) - 1), "10_lasso_seleccion.csv")

# -------------------------------------------------------------
# 6. RMSE POR SUBGRUPO (facid) PARA TODOS LOS MODELOS (validacion B)
# -------------------------------------------------------------
lin_B <- read.csv("resultados/predicciones_validacion_lineales_B.csv")
av_B  <- read.csv(file.path(dir_B, "predicciones_validacion.csv"))
stopifnot(nrow(lin_B) == nrow(datos_val_B), nrow(av_B) == nrow(datos_val_B),
          isTRUE(all.equal(lin_B$real_lengthofstay, datos_val_B$lengthofstay)))

preds <- list(
  Baseline = rep(mean(datos_train_B$lengthofstay), nrow(datos_val_B)),
  OLS = lin_B$pred_ols, Ridge = lin_B$pred_ridge, Lasso = lin_B$pred_lasso,
  `Elastic Net` = lin_B$pred_elastic, GAM = av_B$pred_GAM, Arbol = av_B$pred_Arbol,
  `Random Forest` = av_B$pred_RandomForest,
  `Gradient Boosting` = av_B$pred_GradientBoosting,
  `Promedio avanzados` = rowMeans(av_B[, c("pred_GAM", "pred_Arbol",
                                           "pred_RandomForest", "pred_GradientBoosting")])
)
y <- datos_val_B$lengthofstay
rmse <- function(a, b) sqrt(mean((a - b)^2))
filas <- list()
for (m in names(preds)) {
  p <- pmax(1, preds[[m]])
  for (g in sort(unique(as.character(datos_val_B$facid)))) {
    i <- as.character(datos_val_B$facid) == g
    filas[[length(filas) + 1]] <- data.frame(modelo = m, facid = g, n = sum(i),
                                             rmse = rmse(y[i], p[i]))
  }
}
guardar(do.call(rbind, filas), "11_rmse_facid_todos_modelos_B.csv")

# -------------------------------------------------------------
# 7. RESIDUOS DEL SISTEMA FINAL (GB, B) EN VALIDACION, POR DECIL
# -------------------------------------------------------------
p_gb <- pmax(1, av_B$pred_GradientBoosting)
r_gb <- y - p_gb
dec <- cut(p_gb, breaks = unique(quantile(p_gb, 0:10 / 10)), include.lowest = TRUE)
guardar(data.frame(
  pred_media = tapply(p_gb, dec, mean), real_media = tapply(y, dec, mean),
  residuo_medio = tapply(r_gb, dec, mean), residuo_sd = tapply(r_gb, dec, sd),
  n = as.vector(table(dec))
), "12_residuos_gb_por_decil.csv")
idx <- sample(seq_along(y), 2000)
guardar(data.frame(real = y[idx], pred = p_gb[idx]), "13_muestra_real_vs_pred_val.csv")
guardar(data.frame(q025 = quantile(r_gb, 0.025), q975 = quantile(r_gb, 0.975),
                   prop_pred_menor1_antes_ajuste = mean(av_B$pred_GradientBoosting < 1)),
        "14_cuantiles_error_val.csv")

# -------------------------------------------------------------
# 8. TEST: COBERTURA DEL INTERVALO Y ERROR POR SUBGRUPO (solo reporte)
# -------------------------------------------------------------
# Si el test se ejecuto (EJECUTAR_TEST_FINAL = TRUE) se usa ese archivo;
# si no, la evaluacion previa del grupo guardada en referencia_test_previo/.
ruta_test <- "resultados/predicciones_sistema_final_test.csv"
if (!file.exists(ruta_test)) ruta_test <- "referencia_test_previo/predicciones_sistema_final_test.csv"
pt <- read.csv(ruta_test)
stopifnot(nrow(pt) == nrow(datos_test_B))
guardar(data.frame(
  n_test = nrow(pt),
  cobertura = mean(pt$lengthofstay_real >= pt$limite_inferior &
                   pt$lengthofstay_real <= pt$limite_superior),
  ancho_medio = mean(pt$limite_superior - pt$limite_inferior)
), "15_intervalo_test.csv")
filas <- list()
for (var in c("facid", "gender", "rcount")) {
  for (g in sort(unique(as.character(datos_test_B[[var]])))) {
    i <- as.character(datos_test_B[[var]]) == g
    filas[[length(filas) + 1]] <- data.frame(
      variable = var, nivel = g, n = sum(i),
      rmse = rmse(pt$lengthofstay_real[i], pt$prediccion_final[i]),
      mae = mean(abs(pt$lengthofstay_real[i] - pt$prediccion_final[i])))
  }
}
guardar(do.call(rbind, filas), "16_error_subgrupo_test.csv")

# -------------------------------------------------------------
# 9. PDP e ICE del sistema final (GB, B) sobre muestra de TRAIN
# -------------------------------------------------------------
muestra <- datos_train_B[sample(nrow(datos_train_B), 1000), ]
pdp_ice <- function(var, grilla) {
  out <- list()
  for (val in grilla) {
    d <- muestra
    d[[var]] <- val
    p <- pmax(1, predict(gbm_B, newdata = d))
    out[[length(out) + 1]] <- data.frame(variable = var, valor = val,
                                         id = seq_len(nrow(d)), pred = p)
  }
  do.call(rbind, out)
}
ice <- rbind(
  pdp_ice("rcount", 0:5),
  pdp_ice("hematocrit", seq(quantile(datos_train_B$hematocrit, .05),
                            quantile(datos_train_B$hematocrit, .95), length = 15)),
  pdp_ice("respiration", seq(quantile(datos_train_B$respiration, .05),
                             quantile(datos_train_B$respiration, .95), length = 15))
)
guardar(aggregate(pred ~ variable + valor, data = ice, FUN = mean), "17_pdp_gb_B.csv")
guardar(ice[ice$id <= 60, ], "18_ice_gb_B_60curvas.csv")

writeLines(capture.output(sessionInfo()), file.path(dir_out, "19_session_info.txt"))
cat("\nListo. Archivos en", dir_out, "\n")
