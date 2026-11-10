# INTEGRACION MATIAS - Proyecto A/B
# Ejecucion desde la raiz del proyecto RStudio (abrir .Rproj)
# Se ejecutan los cuatro scripts en un entorno nuevo para evitar variables antiguas.
archivos <- file.path("codigo", c("00_configuracion.R", "01_auditoria_exploracion.R",
                                     "02_preparacion_lineales.R", "03_modelos_avanzados.R",
                                     "04_comparacion_test.R"))
if (!all(file.exists(archivos))) stop("Faltan scripts: ", paste(archivos[!file.exists(archivos)], collapse=", "))
entorno_integracion <- new.env(parent = globalenv())
for (i in seq_along(archivos)) {
  cat("\n========================================\n[", i-1, "/4] ", archivos[i], "\n========================================\n", sep="")
  source(archivos[i], local = entorno_integracion, echo = FALSE, encoding = "UTF-8")
}
# Verificaciones de integracion en validacion
stopifnot(identical(entorno_integracion$datos_train_A$lengthofstay,
                    entorno_integracion$datos_train_B$lengthofstay))
stopifnot(identical(entorno_integracion$datos_val_A$lengthofstay,
                    entorno_integracion$datos_val_B$lengthofstay))
stopifnot(all(c("Enfoque", "Modelo", "RMSE", "MAE", "R2") %in%
              names(entorno_integracion$tabla_comparacion)))
stopifnot(all(is.finite(entorno_integracion$tabla_comparacion$RMSE)))
stopifnot(all(c("OLS", "Ridge", "Lasso", "ElasticNet") %in%
              names(entorno_integracion$modelos_A)))
stopifnot(all(c("GAM", "Arbol", "RandomForest", "GradientBoosting") %in%
              names(entorno_integracion$modelos_avanzados_A)))
writeLines(capture.output(sessionInfo()), "resultados/session_info_integracion.txt", useBytes = TRUE)
cat("\nINTEGRACION FINALIZADA: validaciones estructurales superadas.\n")
cat("Enfoque seleccionado:", entorno_integracion$enfoque_ganador, "\n")
cat("Modelo seleccionado:", entorno_integracion$nombre_mejor, "\n")
if (!isTRUE(entorno_integracion$EJECUTAR_TEST_FINAL)) cat("Test final: NO ejecutado.\n")
