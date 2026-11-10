# Configuracion del proyecto integrada por Matias
EJECUTAR_TEST_FINAL <- FALSE  # NO cambiar durante la comprobacion inicial
paquetes <- c("caret", "glmnet", "dplyr", "car", "gbm", "rpart", "ranger", "gam", "rpart.plot", "foreach")
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes)) stop("Instala paquetes faltantes: ", paste(faltantes, collapse = ", "))
if (!file.exists("datos/LengthOfStay.csv")) stop("Abre el .Rproj antes de ejecutar: falta datos/LengthOfStay.csv")
dir.create("resultados", recursive = TRUE, showWarnings = FALSE)
cat("Configuracion inicial correcta.\n")
