# Caso de estudio 1 · Regresión: predicción de los días de hospitalización

Proyecto integrador de Estadística Aplicada (Ingeniería Industrial, USACH).
Se comparan nueve modelos de regresión para predecir la duración de una hospitalización (días)
con dos conjuntos de variables:

- **Enfoque A:** variables clínicas y generales.
- **Enfoque B:** enfoque A más enfermedades y diagnósticos.

**Resultado:** el sistema elegido con validación es **Gradient Boosting con el enfoque B**:
RMSE de 0,602 días en validación y de 0,605 días en test, frente a 2,37 días del baseline.

**Supuesto del estudio:** para efectos del presente estudio se asume que las variables clínicas
registradas representan la información disponible en la evaluación inicial del paciente.

---

## Cómo reproducir los resultados (paso a paso)

**Requisitos:** R 4.6.1 (o una versión cercana) y RStudio.

1. **Descargar el proyecto.** En esta página, botón verde **Code → Download ZIP**.
   Descomprimir la carpeta completa.
2. **Abrir el proyecto.** Hacer doble clic en `proyecto_hospitalizacion.Rproj`.
   Esto abre RStudio con la carpeta del proyecto como directorio de trabajo.
3. **Instalar los paquetes** (solo la primera vez). Copiar en la consola de RStudio:
   ```r
   install.packages(c("caret", "glmnet", "dplyr", "car", "gbm", "rpart",
                      "ranger", "gam", "rpart.plot", "foreach"))
   ```
4. **Reiniciar R:** menú **Session → Restart R**.
5. **Ejecutar todo el proyecto.** Copiar en la consola:
   ```r
   source("codigo/main.R")
   ```
   Ejecuta en orden los scripts `00` a `07`. Entrena todos los modelos, así que puede tardar
   bastante según el computador. No cerrar RStudio mientras corre.
6. **Revisar el resultado.** Al final, la consola muestra una tabla con cada cifra del informe
   y su estado (**OK** o **FALLA**), y el mensaje `REPRODUCIBILIDAD VERIFICADA.` si todas coinciden.
   La misma tabla queda en `resultados/verificacion_reproducibilidad.csv`.

### Opcional: volver a evaluar el test

Por defecto el test **no se ejecuta** (`EJECUTAR_TEST_FINAL <- FALSE` en `codigo/00_configuracion.R`)
y las cifras de test se comparan con la evaluación guardada en `referencia_test_previo/`.
Para regenerarlas, después del paso 5 cambiar esa línea a `TRUE` y ejecutar:

```r
source("codigo/00_configuracion.R"); source("codigo/04_comparacion_test.R")
source("codigo/06_cifras_informe.R"); source("codigo/07_verificar_reproducibilidad.R")
```

El conjunto de test ya fue consultado en una versión anterior del proyecto (sin enfoques A/B);
la selección del sistema se hizo solo con validación cruzada y validación.

---

## Qué hace cada script

| Script | Qué hace | Archivos que genera |
|---|---|---|
| `00_configuracion.R` | Revisa paquetes y rutas; interruptor del test | — |
| `01_auditoria_exploracion.R` | Auditoría de calidad, EDA y definición de los enfoques A y B | `resultados/clasificacion_variables.csv`, `datos/definicion_enfoques.rds` |
| `02_preparacion_lineales.R` | Partición 70/15/15 (semilla 2026), 5 folds, OLS, Ridge, Lasso y Elastic Net | `datos/particiones_AyB.RData`, `datos/test_congelado_AyB.RData`, predicciones y métricas lineales |
| `03_modelos_avanzados.R` | GAM, árbol, Random Forest y Gradient Boosting con validación cruzada | `resultados/modelos_avanzados_A/` y `_B/` |
| `04_comparacion_test.R` | Comparación A vs. B, selección del sistema, subgrupos y test (si está activado) | `tabla_comparacion_AyB.csv`, `mejor_sistema_validacion.csv`, `error_subgrupo_*.csv` |
| `06_cifras_informe.R` | Cifras de respaldo: IC por género, VIF, hiperparámetros, intervalo, PDP/ICE | `resultados/cifras_informe/` |
| `07_verificar_reproducibilidad.R` | Compara lo generado con las cifras del informe | `resultados/verificacion_reproducibilidad.csv` |
| `05_verificar_resultados_guardados.R` | Revisión rápida de los CSV guardados, sin entrenar | — |

## Dónde está cada cifra del informe

| Cifra del informe | Archivo |
|---|---|
| Tabla de modelos (RMSE, MAE, R²) | `resultados/tabla_comparacion_AyB.csv` |
| RMSE de CV ± DE entre folds | `resultados/comparacion_validacion_cv_AyB.csv` |
| Enfoque A vs. B | `resultados/comparacion_enfoques_A_vs_B.csv` |
| Error por establecimiento, género y reingresos | `resultados/error_subgrupo_*.csv` |
| Diferencia por género con IC 95 % | `resultados/cifras_informe/04_ic_genero.csv` |
| Coeficientes OLS y VIF | `resultados/cifras_informe/06_ols_B_coeficientes.csv`, `07_vif_ols_B.csv` |
| Hiperparámetros elegidos | `resultados/cifras_informe/09_hiperparametros_B.csv` |
| Intervalo de predicción y cobertura | `resultados/cifras_informe/14_cuantiles_error_val.csv`, `15_intervalo_test.csv` |
| RMSE por establecimiento de todos los modelos | `resultados/cifras_informe/11_rmse_facid_todos_modelos_B.csv` |
| PDP e ICE | `resultados/cifras_informe/17_pdp_gb_B.csv`, `18_ice_gb_B_60curvas.csv` |
| Resultado en test | `referencia_test_previo/resultado_final_test.csv` (o `resultados/` si se re-ejecutó) |

## Datos

`datos/LengthOfStay.csv`: *Hospital Length of Stay*, Microsoft (repositorio público
`r-server-hospital-length-of-stay`). 100.000 hospitalizaciones y 28 columnas.

## Equipo

Julianna Arriaza Sánchez · Joaquín Navarro Aguilera · Francisca Ulloa Avendaño ·
Isidora Vega Cordero · Matías Yáñez Vergara
