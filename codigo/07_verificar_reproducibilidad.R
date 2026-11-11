# =============================================================
# SCRIPT 07: VERIFICACION DE REPRODUCIBILIDAD
# Compara lo que se acaba de generar con las cifras declaradas
# en el informe y la presentacion. Ejecutar despues de main.R.
# Resultado: tabla OK / FALLA en consola y en
# resultados/verificacion_reproducibilidad.csv
# =============================================================

TOL <- 0.001  # tolerancia absoluta para metricas en dias
filas <- list()
chequear <- function(item, obtenido, esperado, tol = TOL, fuente = "") {
  ok <- if (is.character(esperado)) identical(as.character(obtenido), esperado) else
    isTRUE(abs(as.numeric(obtenido) - esperado) <= tol)
  filas[[length(filas) + 1]] <<- data.frame(
    item = item, esperado = as.character(esperado),
    obtenido = as.character(if (is.numeric(obtenido)) signif(obtenido, 7) else obtenido),
    estado = ifelse(ok, "OK", "FALLA"), fuente = fuente)
}
leer <- function(ruta) {
  if (!file.exists(ruta)) stop("Falta ", ruta, ": ejecuta primero source(\"codigo/main.R\")")
  read.csv(ruta)
}

# ---- 1. Seleccion y comparacion de modelos (validacion) ----
tab <- leer("resultados/tabla_comparacion_AyB.csv")
val <- function(enf, mod, col) tab[tab$Enfoque == enf & tab$Modelo == mod, col]
f1 <- "tabla_comparacion_AyB.csv"
chequear("Validacion B, Gradient Boosting: RMSE", val("B", "Gradient Boosting", "RMSE"), 0.6023066, fuente = f1)
chequear("Validacion B, Gradient Boosting: MAE",  val("B", "Gradient Boosting", "MAE"),  0.4325448, fuente = f1)
chequear("Validacion B, Gradient Boosting: R2",   val("B", "Gradient Boosting", "R2"),   0.9353329, fuente = f1)
chequear("Validacion A, Gradient Boosting: RMSE", val("A", "Gradient Boosting", "RMSE"), 0.6434635, fuente = f1)
chequear("Validacion B, Random Forest: RMSE",     val("B", "Random Forest", "RMSE"),     0.6533188, fuente = f1)
chequear("Validacion B, Random Forest: MAE",      val("B", "Random Forest", "MAE"),      0.4038958, fuente = f1)
chequear("Validacion B, GAM: RMSE",               val("B", "GAM", "RMSE"),               0.8776598, fuente = f1)
chequear("Validacion B, Arbol: RMSE",             val("B", "Arbol", "RMSE"),             1.1040357, fuente = f1)
chequear("Validacion B, OLS: RMSE",               val("B", "OLS", "RMSE"),               1.1697549, fuente = f1)
chequear("Validacion B, Averaging: RMSE",         val("B", "Promedio avanzados", "RMSE"), 0.7072839, fuente = f1)
chequear("Validacion, Baseline: RMSE",            val("B", "Baseline", "RMSE"),          2.3685151, fuente = f1)

mejor <- leer("resultados/mejor_sistema_validacion.csv")
chequear("Sistema elegido: enfoque", mejor$Enfoque[1], "B", fuente = "mejor_sistema_validacion.csv")
chequear("Sistema elegido: modelo",  mejor$Modelo[1], "Gradient Boosting", fuente = "mejor_sistema_validacion.csv")

cv <- leer("resultados/comparacion_validacion_cv_AyB.csv")
gb <- cv[cv$Enfoque == "B" & cv$Modelo == "Gradient Boosting", ]
chequear("CV B, Gradient Boosting: RMSE_CV", gb$RMSE_CV, 0.5968774, fuente = "comparacion_validacion_cv_AyB.csv")
chequear("CV B, Gradient Boosting: DE entre folds", gb$SD_RMSE, 0.0056865, fuente = "comparacion_validacion_cv_AyB.csv")

fac <- leer("resultados/error_subgrupo_facid.csv")
chequear("Subgrupo establecimiento D: n", fac$N[fac$Subgrupo == "D"], 725, tol = 0, fuente = "error_subgrupo_facid.csv")
chequear("Subgrupo establecimiento D: RMSE", fac$RMSE[fac$Subgrupo == "D"], 0.7811623, fuente = "error_subgrupo_facid.csv")

# ---- 2. Cifras de respaldo del informe (script 06) ----
ci <- "resultados/cifras_informe/"
d1 <- leer(paste0(ci, "01_descriptivos.csv"))
chequear("Dataset: filas", d1$n_filas, 100000, tol = 0, fuente = "01_descriptivos.csv")
chequear("Dataset: media de estadia (dias)", d1$los_media, 4.00103, fuente = "01_descriptivos.csv")
d4 <- leer(paste0(ci, "04_ic_genero.csv"))
chequear("Genero: diferencia de medias (dias)", d4$diferencia, 0.3323404, fuente = "04_ic_genero.csv")
chequear("Genero: IC 95 % inferior", d4$ic_inf, 0.3027217, fuente = "04_ic_genero.csv")
chequear("Genero: IC 95 % superior", d4$ic_sup, 0.3619592, fuente = "04_ic_genero.csv")
d5 <- leer(paste0(ci, "05_particiones.csv"))
chequear("Particion: filas entrenamiento", d5$n[d5$conjunto == "train"], 69993, tol = 0, fuente = "05_particiones.csv")
chequear("Particion: filas validacion", d5$n[d5$conjunto == "validacion"], 15005, tol = 0, fuente = "05_particiones.csv")
chequear("Particion: filas test", d5$n[d5$conjunto == "test"], 15002, tol = 0, fuente = "05_particiones.csv")
d6 <- leer(paste0(ci, "06_ols_B_coeficientes.csv"))
chequear("OLS: coeficiente reingresos previos", d6$coef[d6$termino == "rcount"], 1.152, tol = 0.001, fuente = "06_ols_B_coeficientes.csv")
d7 <- leer(paste0(ci, "07_vif_ols_B.csv"))
chequear("OLS: VIF maximo", max(d7$gvif_ajustado), 3.2407753, tol = 0.01, fuente = "07_vif_ols_B.csv")
d9 <- leer(paste0(ci, "09_hiperparametros_B.csv"))
hp <- function(m) d9$hiperparametros[d9$modelo == m]
chequear("Hiperparametros Gradient Boosting", hp("Gradient Boosting"),
         "n.trees=200; interaction.depth=3; shrinkage=0.1; n.minobsinnode=10", fuente = "09_hiperparametros_B.csv")
chequear("Hiperparametros GAM", hp("GAM"), "df=7", fuente = "09_hiperparametros_B.csv")
chequear("Hiperparametros Arbol", hp("Arbol"), "cp=0.001", fuente = "09_hiperparametros_B.csv")
chequear("Hiperparametros Lasso", hp("Lasso"), "alpha=1; lambda=0.00336", fuente = "09_hiperparametros_B.csv")
d10 <- leer(paste0(ci, "10_lasso_seleccion.csv"))
chequear("Lasso: coeficientes distintos de cero", d10$coef_no_cero, 22, tol = 0, fuente = "10_lasso_seleccion.csv")
d14 <- leer(paste0(ci, "14_cuantiles_error_val.csv"))
chequear("Intervalo: limite inferior (dias)", d14$q025, -1.1008724, fuente = "14_cuantiles_error_val.csv")
chequear("Intervalo: limite superior (dias)", d14$q975, 1.3638926, fuente = "14_cuantiles_error_val.csv")

# ---- 3. Test (desde resultados/ si se ejecuto; si no, referencia previa) ----
rt <- if (file.exists("resultados/resultado_final_test.csv")) "resultados/resultado_final_test.csv" else
  "referencia_test_previo/resultado_final_test.csv"
te <- read.csv(rt)
chequear("Test, Gradient Boosting: RMSE", te$RMSE[te$Modelo == "Gradient Boosting"], 0.6054736, fuente = rt)
chequear("Test, Gradient Boosting: MAE",  te$MAE[te$Modelo == "Gradient Boosting"],  0.4346074, fuente = rt)
chequear("Test, Baseline: RMSE",          te$RMSE[te$Modelo == "Baseline"],          2.3731665, fuente = rt)
d15 <- leer(paste0(ci, "15_intervalo_test.csv"))
chequear("Test: cobertura del intervalo", d15$cobertura, 0.9470737, fuente = "15_intervalo_test.csv")

# ---- Resumen ----
res <- do.call(rbind, filas)
write.csv(res, "resultados/verificacion_reproducibilidad.csv", row.names = FALSE)
print(res[, c("estado", "item", "esperado", "obtenido")], row.names = FALSE)
n_falla <- sum(res$estado == "FALLA")
cat("\n", nrow(res) - n_falla, "de", nrow(res), "cifras coinciden con el informe.\n")
if (!grepl("^resultados/", rt)) cat("Nota: las cifras de test se leyeron de referencia_test_previo/ (test no re-ejecutado).\n")
if (n_falla == 0) cat("REPRODUCIBILIDAD VERIFICADA.\n") else
  cat("ATENCION: revisar las filas con FALLA (version de R/paquetes o archivos desactualizados).\n")
