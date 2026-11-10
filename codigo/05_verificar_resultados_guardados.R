# Comprobacion rapida de archivos de validacion ya generados.
# No entrena modelos ni accede a test.
archivos <- c("resultados/comparacion_enfoques_A_vs_B.csv",
              "resultados/mejor_sistema_validacion.csv",
              "resultados/comparacion_validacion_cv_AyB.csv",
              "resultados/supuesto_temporal.txt",
              "resultados/session_info_integracion.txt")
stopifnot(all(file.exists(archivos)))
comp <- read.csv(archivos[1]); mejor <- read.csv(archivos[2]); cv <- read.csv(archivos[3])
stopifnot(nrow(comp) == 2, all(c("A", "B") %in% comp$Enfoque))
stopifnot(mejor$Enfoque[1] == "B", mejor$Modelo[1] == "Gradient Boosting")
stopifnot(abs(mejor$RMSE[1] - 0.6023066) < 0.0001)
stopifnot(abs(mejor$MAE[1] - 0.4325448) < 0.0001)
stopifnot(abs(mejor$R2[1] - 0.9353329) < 0.0001)
stopifnot(all(c("RMSE_CV", "SD_RMSE", "MAE_CV", "R2_CV") %in% names(cv)))
cat("OK: tablas A/B y metricas guardadas coinciden con la referencia.\n")
cat("NOTA: verificar CSV guardados NO equivale a reejecutar modelos desde cero.\n")
