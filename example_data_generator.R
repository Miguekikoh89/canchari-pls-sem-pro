# ============================================================================
# Generador de Datos de Ejemplo para PLS-SEM
# ============================================================================
# Este script genera un archivo Excel de ejemplo para probar la aplicación
# Los datos simulan respuestas Likert de un estudio de satisfacción de clientes

library(openxlsx)
set.seed(123)

# Parámetros de simulación
n_respondents <- 200
n_items <- 11

# Crear matriz de correlación base (estructura teórica)
# Constructos: COMP (competencia), LIKE (simpatía), CUSA (satisfacción), CUSL (lealtad)

# Generar datos latentes (constructos)
COMP <- rnorm(n_respondents, mean = 4, sd = 1)  # Competencia (1-5)
LIKE <- rnorm(n_respondents, mean = 4, sd = 1)  # Simpatía (1-5)
CUSA <- 0.4 * COMP + 0.5 * LIKE + rnorm(n_respondents, mean = 0, sd = 0.5)  # Satisfacción
CUSL <- 0.6 * CUSA + rnorm(n_respondents, mean = 0, sd = 0.5)  # Lealtad

# Normalizar a escala Likert 1-5
normalize_likert <- function(x, min_val = 1, max_val = 5) {
  x_scaled <- (x - min(x)) / (max(x) - min(x))
  round(x_scaled * (max_val - min_val) + min_val)
}

COMP <- normalize_likert(COMP)
LIKE <- normalize_likert(LIKE)
CUSA <- normalize_likert(CUSA)
CUSL <- normalize_likert(CUSL)

# Crear ítems observados con error de medición
# Constructo COMP (Competencia) - 3 ítems
comp_1 <- COMP + rnorm(n_respondents, mean = 0, sd = 0.3)
comp_2 <- COMP + rnorm(n_respondents, mean = 0, sd = 0.3)
comp_3 <- COMP + rnorm(n_respondents, mean = 0, sd = 0.3)

# Constructo LIKE (Simpatía) - 3 ítems
like_1 <- LIKE + rnorm(n_respondents, mean = 0, sd = 0.3)
like_2 <- LIKE + rnorm(n_respondents, mean = 0, sd = 0.3)
like_3 <- LIKE + rnorm(n_respondents, mean = 0, sd = 0.3)

# Constructo CUSA (Satisfacción) - 1 ítem
cusa <- CUSA + rnorm(n_respondents, mean = 0, sd = 0.3)

# Constructo CUSL (Lealtad) - 3 ítems
cusl_1 <- CUSL + rnorm(n_respondents, mean = 0, sd = 0.3)
cusl_2 <- CUSL + rnorm(n_respondents, mean = 0, sd = 0.3)
cusl_3 <- CUSL + rnorm(n_respondents, mean = 0, sd = 0.3)

# Normalizar todos los ítems a escala Likert 1-5
normalize_likert_5 <- function(x) {
  pmax(1, pmin(5, round(x)))
}

comp_1 <- normalize_likert_5(comp_1)
comp_2 <- normalize_likert_5(comp_2)
comp_3 <- normalize_likert_5(comp_3)
like_1 <- normalize_likert_5(like_1)
like_2 <- normalize_likert_5(like_2)
like_3 <- normalize_likert_5(like_3)
cusa <- normalize_likert_5(cusa)
cusl_1 <- normalize_likert_5(cusl_1)
cusl_2 <- normalize_likert_5(cusl_2)
cusl_3 <- normalize_likert_5(cusl_3)

# Crear data frame
data <- data.frame(
  ID = 1:n_respondents,
  comp_1 = comp_1,
  comp_2 = comp_2,
  comp_3 = comp_3,
  like_1 = like_1,
  like_2 = like_2,
  like_3 = like_3,
  cusa = cusa,
  cusl_1 = cusl_1,
  cusl_2 = cusl_2,
  cusl_3 = cusl_3
)

# Mostrar resumen de datos
cat("========================================\n")
cat("Datos de Ejemplo Generados\n")
cat("========================================\n\n")
cat("Dimensiones:", nrow(data), "observaciones x", ncol(data), "variables\n")
cat("Variables:\n")
print(names(data))
cat("\nPrimeras 10 observaciones:\n")
print(head(data, 10))
cat("\nEstadísticas descriptivas:\n")
print(summary(data[, -1]))  # Excluir ID

# Guardar en Excel
output_file <- "/home/ubuntu/example_data.xlsx"
write.xlsx(data, output_file, sheetName = "Corporate_Reputation", rowNames = FALSE)

cat("\n========================================\n")
cat("✓ Archivo guardado en:", output_file, "\n")
cat("========================================\n\n")

cat("Estructura del Modelo de Ejemplo:\n")
cat("================================\n")
cat("Constructos:\n")
cat("  - COMP (Competencia): comp_1, comp_2, comp_3\n")
cat("  - LIKE (Simpatía): like_1, like_2, like_3\n")
cat("  - CUSA (Satisfacción): cusa\n")
cat("  - CUSL (Lealtad): cusl_1, cusl_2, cusl_3\n\n")
cat("Paths (Relaciones):\n")
cat("  - COMP -> CUSA\n")
cat("  - LIKE -> CUSA\n")
cat("  - CUSA -> CUSL\n\n")

cat("Instrucciones para usar en la aplicación:\n")
cat("1. Abre la aplicación Shiny\n")
cat("2. Ve a 'Cargar Datos'\n")
cat("3. Selecciona el archivo 'example_data.xlsx'\n")
cat("4. Haz clic en 'Cargar Datos'\n")
cat("5. Ve a 'Definir Modelo' y configura los constructos y paths\n")
cat("6. Ejecuta el análisis\n")
