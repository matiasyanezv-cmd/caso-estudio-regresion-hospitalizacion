
# ==========================================
# PROYECTO: DIAS DE HOSPITALIZACION
# ESTADISTICA APLICADA
#Integrante:Francisca
# ==========================================

# 1. CARGAR LOS DATOS

datos <- read.csv("datos/LengthOfStay.csv")

# 2. EXPLORACION INICIAL

# Cantidad de filas y columnas
dim(datos)

# Nombres de las variables
names(datos)

# Primeras 6 observaciones
head(datos)

# Resumen estadistico
summary(datos)

# 3. AUDITORIA DE CALIDAD DE LOS DATOS

# Revisar los tipos de variables
str(datos)

# Contar valores faltantes por columna
colSums(is.na(datos))

# Contar filas completamente duplicadas
sum(duplicated(datos))

# Revisar identificadores repetidos
sum(duplicated(datos$eid))

# Comprobar si hay dias de hospitalizacion no positivos
sum(datos$lengthofstay <= 0)


# ==========================================
# 4. ANALISIS DESCRIPTIVO
# ==========================================

# 4.1. ANALISIS GENERAL DE HOSPITALIZACION

# Resumen de los dias de hospitalizacion
summary(datos$lengthofstay)

# Promedio de dias
mean(datos$lengthofstay)

# Mediana de dias
median(datos$lengthofstay)

# Desviacion estandar
sd(datos$lengthofstay)


# 4.2. ANALISIS SEGUN GENERO

# Cantidad de registros por genero
table(datos$gender)

# Promedio de dias segun genero
tapply(datos$lengthofstay, datos$gender, mean)

# Mediana de dias segun genero
tapply(datos$lengthofstay, datos$gender, median)


# 4.3. ANALISIS SEGUN ENFERMEDADES

# Lista de variables de enfermedades y condiciones
enfermedades <- c(
  "dialysisrenalendstage",
  "asthma",
  "irondef",
  "pneum",
  "substancedependence",
  "psychologicaldisordermajor",
  "depress",
  "psychother",
  "fibrosisandother",
  "malnutrition"
)

# Cantidad de registros con y sin cada condicion
lapply(datos[enfermedades], table)

# Promedio de dias de hospitalizacion por condicion
lapply(datos[enfermedades], function(x) {
  tapply(datos$lengthofstay, x, mean)
})

# Mediana de dias de hospitalizacion por condicion
lapply(datos[enfermedades], function(x) {
  tapply(datos$lengthofstay, x, median)
})


# 4.4. RESUMEN DE TODAS LAS VARIABLES

# Estadisticas descriptivas generales
summary(datos)


# 5. REVISION DE FUGA DE INFORMACION


# 5.1. Variables excluidas de ambos enfoques

variables_excluir <- c(
  "eid",
  "discharged",
  "vdate"
)

# Variable que queremos predecir
variable_objetivo <- "lengthofstay"

print(variables_excluir)



# 5.2. Revisar variables que requieren investigacion

variables_revisar <- c(
  "vdate",
  "rcount",
  "secondarydiagnosisnonicd9"
)

# Mostrar las variables pendientes de revision
variables_revisar


# 5.3. Revisar las variables de fechas

# Primeros valores de la fecha de ingreso o visita
head(datos$vdate)

# Primeros valores de la fecha de alta
head(datos$discharged)


# 5.4. Verificar que las variables existan
# en el dataset

variables_excluir %in% names(datos)

variables_revisar %in% names(datos)


# 6. GRAFICOS EXPLORATORIOS

# 6.1. HISTOGRAMA DE DIAS DE HOSPITALIZACION
hist(datos$lengthofstay,
     main = "Distribucion de dias de hospitalizacion",
     xlab = "Dias de hospitalizacion",
     ylab = "Cantidad de registros",
     col = "lightblue",
     breaks = seq(0.5, 17.5, by = 1))


# 6.2. BOXPLOT DE DIAS SEGUN GENERO

boxplot(lengthofstay ~ gender,
        data = datos,
        main = "Dias de hospitalizacion segun genero",
        xlab = "Genero",
        ylab = "Dias de hospitalizacion",
        col = c("lightpink", "lightblue"))


# 6.3. PROMEDIO DE DIAS SEGUN GENERO

# 6.3. PROMEDIO DE DIAS SEGUN GENERO

promedio_genero <- tapply(
  datos$lengthofstay,
  datos$gender,
  mean
)

barplot(promedio_genero,
        main = "Promedio de dias segun genero",
        xlab = "Genero",
        ylab = "Promedio de dias",
        col = c("lightpink", "lightblue"),
        ylim = c(0, max(promedio_genero) * 1.2))


# 6.4. PROMEDIO DE DIAS SEGUN ENFERMEDADES

# Crear la carpeta exploratorios dentro de resultados
dir.create("resultados/exploratorios",
           recursive = TRUE,
           showWarnings = FALSE)

# Crear y guardar un grafico por enfermedad
for (enfermedad in enfermedades) {
  
  # Calcular promedio de dias por categoria
  promedios <- tapply(
    datos$lengthofstay,
    datos[[enfermedad]],
    mean
  )
  
  # Abrir archivo PNG
  png(
    filename = paste0(
      "resultados/exploratorios/07_",
      enfermedad,
      ".png"
    ),
    width = 1000,
    height = 700
  )
  
  # Crear grafico
  barplot(
    promedios,
    main = enfermedad,
    xlab = "Categoria (0 o 1)",
    ylab = "Promedio de dias",
    col = c("lightblue", "lightpink"),
    ylim = c(0, max(promedios) * 1.2)
  )
  
  # Guardar y cerrar imagen
  dev.off()
}


# 7. INTERVALO DE CONFIANZA

# 7.1. PROMEDIO DE DIAS SEGUN GENERO

# Calcular los promedios por genero
promedios_genero <- tapply(
  datos$lengthofstay,
  datos$gender,
  mean
)

# Mostrar los promedios
promedios_genero


# 7.2. DIFERENCIA DE PROMEDIOS

# Calcular diferencia entre hombres y mujeres
diferencia <- promedios_genero["M"] -
  promedios_genero["F"]

# Mostrar la diferencia
diferencia



# 7.3. Intervalo de confianza del 95%
# Comparacion: hombres menos mujeres

prueba_genero <- t.test(
  datos$lengthofstay[datos$gender == "M"],
  datos$lengthofstay[datos$gender == "F"],
  conf.level = 0.95
)

print(prueba_genero)

# 7.4. Extraer intervalo
intervalo_confianza <- prueba_genero$conf.int

print(intervalo_confianza)

# 7.5. Verificar si incluye cero
incluye_cero <- (
  intervalo_confianza[1] <= 0 &&
    intervalo_confianza[2] >= 0
)

print(incluye_cero)


# ==========================================
# 8. REVISION FINAL DE CALIDAD Y FUGAS
# ==========================================

# 8.1. COMPROBACION DE CALIDAD DE DATOS

# Cantidad total de valores faltantes
sum(is.na(datos))

# Cantidad de filas completamente duplicadas
sum(duplicated(datos))

# Cantidad de identificadores de registro repetidos
sum(duplicated(datos$eid))

# Cantidad de dias de hospitalizacion no positivos
sum(datos$lengthofstay <= 0, na.rm = TRUE)



# 8.2. VARIABLES QUE DEBEN EXCLUIRSE

# Variables que no deben utilizarse como predictoras
variables_excluir <- c(
  "eid",
  "discharged",
  "vdate"
)

# Mostrar variables a excluir
variables_excluir

# 8.3. Variables cuya disponibilidad
# temporal requiere justificacion

variables_revisar <- c(
  "rcount",
  "secondarydiagnosisnonicd9"
)

print(variables_revisar)


# 8.4. COMPROBAR QUE LAS VARIABLES EXISTEN

# Revisar si las columnas estan presentes en el dataset
variables_excluir %in% names(datos)

variables_revisar %in% names(datos)


# 8.5. REVISAR VALORES DE VARIABLES SOSPECHOSAS

# Mostrar algunos valores de las columnas a revisar
head(datos[variables_revisar])

# Revisar distribucion de rcount
table(datos$rcount, useNA = "ifany")

# Revisar distribucion del diagnostico secundario
table(datos$secondarydiagnosisnonicd9,
      useNA = "ifany")


# 8.6. COMPROBAR VARIABLES CLINICAS

# Revisar que las enfermedades tengan valores 0 y 1
lapply(datos[enfermedades], function(x) {
  sort(unique(x))
})


# 8.7. CONCLUSIONES DE LA AUDITORIA

cat("\nREVISION FINAL DEL DATASET\n")

cat("Observaciones:", nrow(datos), "\n")

cat("Variables:", ncol(datos), "\n")

cat("Valores faltantes:", sum(is.na(datos)), "\n")

cat("Filas duplicadas:", sum(duplicated(datos)), "\n")

cat("Identificadores repetidos:",
    sum(duplicated(datos$eid)), "\n")

cat("Dias no positivos:",
    sum(datos$lengthofstay <= 0, na.rm = TRUE), "\n")

cat("Variables a excluir:",
    paste(variables_excluir, collapse = ", "), "\n")

cat("Variables a revisar:",
    paste(variables_revisar, collapse = ", "), "\n")


# 8.8. REVISION DE VALORES EXTREMOS

# Seleccionar variables clinicas numericas
variables_clinicas <- c(
  "hemo",
  "hematocrit",
  "neutrophils",
  "sodium",
  "glucose",
  "bloodureanitro",
  "creatinine",
  "bmi",
  "pulse",
  "respiration"
)

# Obtener valores minimos y maximos
extremos_clinicos <- data.frame(
  variable = variables_clinicas,
  minimo = sapply(datos[variables_clinicas], min, na.rm = TRUE),
  maximo = sapply(datos[variables_clinicas], max, na.rm = TRUE)
)

# Mostrar resultados
print(extremos_clinicos)

# 8.9. REVISION DE POSIBLES INCONSISTENCIAS

# Contar valores negativos en variables clinicas
negativos_clinicos <- sapply(
  datos[variables_clinicas],
  function(x) sum(x < 0, na.rm = TRUE)
)

# Mostrar cantidad de valores negativos
print(negativos_clinicos)

# Contar valores iguales a cero
ceros_clinicos <- sapply(
  datos[variables_clinicas],
  function(x) sum(x == 0, na.rm = TRUE)
)

# Mostrar cantidad de ceros
print(ceros_clinicos)

# 8.10. DISPONIBILIDAD DE VARIABLES

# Crear tabla para documentar las variables
revision_temporal <- data.frame(
  variable = c(
    "discharged",
    "eid",
    "vdate",
    "rcount",
    "secondarydiagnosisnonicd9",
    "gender"
  ),
  
  recomendacion = c(
    "Excluir",
    "Excluir",
    "Revisar",
    "Revisar",
    "Revisar",
    "Candidata a predictora"
  ),
  
  motivo = c(
    "Fecha de alta posterior al ingreso",
    "Identificador del registro",
    "Confirmar significado y disponibilidad",
    "Confirmar significado y disponibilidad",
    "Confirmar momento del diagnostico",
    "Verificar disponibilidad al ingreso"
  )
)

# Mostrar tabla
print(revision_temporal)


# =====================================================
# 9. DEFINICION DE VARIABLES Y ENFOQUES A Y B
# =====================================================

# Objetivo:
# Predecir la duracion de la hospitalizacion
# a partir de una evaluacion clinica inicial.
#
# SUPUESTO DEL ESTUDIO:
# Se interpretan las variables clinicas como
# informacion disponible en dicha evaluacion.
#
# LIMITACION:
# La documentacion del dataset indica que
# varias mediciones son promedios del episodio.
# No esta demostrado que estos promedios
# estuvieran disponibles inicialmente.
# Por tanto, persiste riesgo de fuga temporal.

# 9.1. Definir variable respuesta

variable_objetivo <- "lengthofstay"

# 9.2. Variables clinicas

variables_clinicas <- c(
  "hemo",
  "hematocrit",
  "neutrophils",
  "sodium",
  "glucose",
  "bloodureanitro",
  "creatinine",
  "bmi",
  "pulse",
  "respiration"
)

# 9.3. Variables generales

variables_generales <- c(
  "gender",
  "facid",
  "rcount"
)

# 9.4. Enfermedades y diagnosticos

variables_diagnosticas <- c(
  "dialysisrenalendstage",
  "asthma",
  "irondef",
  "pneum",
  "substancedependence",
  "psychologicaldisordermajor",
  "depress",
  "psychother",
  "fibrosisandother",
  "malnutrition",
  "secondarydiagnosisnonicd9"
)

# 9.5. Variables excluidas

variables_excluir <- c(
  "eid",
  "discharged",
  "vdate"
)

# 9.6. Construir los dos enfoques

# A: clinicas + generales
predictores_A <- c(
  variables_clinicas,
  variables_generales
)

# B: clinicas + generales + diagnosticas
predictores_B <- c(
  predictores_A,
  variables_diagnosticas
)

# 9.7. Comprobar existencia de columnas

columnas_necesarias <- unique(c(
  predictores_B,
  variables_excluir,
  variable_objetivo
))

faltantes <- setdiff(
  columnas_necesarias,
  names(datos)
)

if (length(faltantes) > 0) {
  stop(paste(
    "Faltan columnas:",
    paste(faltantes, collapse = ", ")
  ))
}

# Verificar que no se repitan predictores
stopifnot(
  !anyDuplicated(predictores_A),
  !anyDuplicated(predictores_B)
)

# Verificar que las variables excluidas
# no aparezcan entre los predictores
stopifnot(
  length(intersect(
    predictores_B,
    variables_excluir
  )) == 0
)

# Verificar que B contenga todo A
stopifnot(
  all(predictores_A %in% predictores_B)
)

# 9.8. Comprobar que todas las columnas
# del dataset estan clasificadas

columnas_sin_clasificar <- setdiff(
  names(datos),
  columnas_necesarias
)

if (length(columnas_sin_clasificar) > 0) {
  stop(paste(
    "Columnas sin clasificar:",
    paste(columnas_sin_clasificar,
          collapse = ", ")
  ))
}

# 9.9. Crear tabla de clasificacion

clasificacion <- data.frame(
  variable = names(datos),
  grupo = NA_character_,
  stringsAsFactors = FALSE
)

clasificacion$grupo[
  clasificacion$variable %in% variables_clinicas
] <- "Clinica"

clasificacion$grupo[
  clasificacion$variable %in% variables_generales
] <- "General"

clasificacion$grupo[
  clasificacion$variable %in% variables_diagnosticas
] <- "Enfermedad/diagnostico"

clasificacion$grupo[
  clasificacion$variable %in% variables_excluir
] <- "Excluida"

clasificacion$grupo[
  clasificacion$variable == variable_objetivo
] <- "Variable respuesta"

# 9.10. Documentar disponibilidad temporal

clasificacion$disponibilidad <- "No confirmada"

clasificacion$disponibilidad[
  clasificacion$variable == "gender"
] <- "Conocida en evaluacion inicial"

clasificacion$disponibilidad[
  clasificacion$variable == "facid"
] <- "Establecimiento identificado"

clasificacion$disponibilidad[
  clasificacion$variable == "rcount"
] <- "Historial previo, sujeto a verificacion"

clasificacion$disponibilidad[
  clasificacion$variable == "hemo"
] <- "Momento de registro no confirmado"

promedios_episodio <- setdiff(
  variables_clinicas,
  "hemo"
)

clasificacion$disponibilidad[
  clasificacion$variable %in% promedios_episodio
] <- "Promedio del episodio; no inicial verificado"

clasificacion$disponibilidad[
  clasificacion$variable %in% variables_diagnosticas
] <- "Momento del diagnostico no confirmado"

clasificacion$disponibilidad[
  clasificacion$variable == "discharged"
] <- "Disponible al finalizar el episodio"

clasificacion$disponibilidad[
  clasificacion$variable == variable_objetivo
] <- "Disponible al finalizar el episodio"

# 9.11. Identificar pertenencia a enfoques

clasificacion$enfoque_A <-
  clasificacion$variable %in% predictores_A

clasificacion$enfoque_B <-
  clasificacion$variable %in% predictores_B

# 9.12. Crear carpeta de resultados

dir.create(
  "resultados",
  recursive = TRUE,
  showWarnings = FALSE
)

# 9.13. Guardar clasificacion

write.csv(
  clasificacion,
  "resultados/clasificacion_variables.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 9.14. Guardar listas de predictores

saveRDS(
  list(
    objetivo = variable_objetivo,
    enfoque_A = predictores_A,
    enfoque_B = predictores_B,
    excluidas = variables_excluir
  ),
  "datos/definicion_enfoques.rds"
)

# 9.15. Guardar documento del supuesto

supuesto <- c(
  "OBJETIVO DEL ESTUDIO",
  paste(
    "Predecir la duracion de la hospitalizacion",
    "a partir de una evaluacion clinica inicial."
  ),
  "",
  "SUPUESTO TEMPORAL",
  paste(
    "Se interpretan las variables clinicas",
    "como informacion disponible en la",
    "evaluacion inicial del paciente."
  ),
  "",
  "LIMITACION METODOLOGICA",
  paste(
    "La documentacion identifica varias",
    "mediciones como promedios del episodio.",
    "Su disponibilidad inicial no esta",
    "verificada y existe riesgo de fuga",
    "temporal. Los resultados deben",
    "interpretarse bajo esta limitacion."
  ),
  "",
  "COMPARACION",
  "A: variables clinicas + generales.",
  paste(
    "B: variables clinicas + generales",
    "+ enfermedades/diagnosticos."
  )
)

writeLines(
  supuesto,
  "resultados/supuesto_temporal.txt",
  useBytes = TRUE
)

# 9.16. Mostrar resumen

cat("\n=== DEFINICION DE ENFOQUES ===\n")

cat(
  "Enfoque A:",
  length(predictores_A),
  "predictores\n"
)

cat(
  "Enfoque B:",
  length(predictores_B),
  "predictores\n"
)

cat("\nVariables adicionales de B:\n")
print(setdiff(predictores_B, predictores_A))

cat("\n=== CLASIFICACION ===\n")
print(clasificacion)

cat("\nArchivos guardados correctamente.\n")
