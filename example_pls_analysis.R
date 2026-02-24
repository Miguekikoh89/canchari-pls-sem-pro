# ============================================================================
# Ejemplo Completo de Análisis PLS-SEM con seminr
# ============================================================================
# Este script demuestra cómo ejecutar un análisis PLS-SEM completo
# Siguiendo los lineamientos de Hair et al. (2022)
# Puedes ejecutar este script directamente en RStudio

library(seminr)
library(tidyverse)

cat("\n")
cat("=" %+% rep("=", 70) %+% "=\n")
cat("EJEMPLO COMPLETO: ANÁLISIS PLS-SEM CON SEMINR\n")
cat("=" %+% rep("=", 70) %+% "=\n\n")

# ============================================================================
# PASO 1: CARGAR DATOS
# ============================================================================

cat("PASO 1: Cargando datos...\n")
cat("-" %+% rep("-", 70) %+% "-\n\n")

# Usar datos de ejemplo incluidos en seminr
data(corp_rep_data)

cat("✓ Datos cargados: Corporate Reputation Dataset\n")
cat("  Dimensiones:", nrow(corp_rep_data), "observaciones ×", ncol(corp_rep_data), "variables\n")
cat("  Variables:", paste(names(corp_rep_data), collapse = ", "), "\n\n")

# Explorar datos
cat("Primeras 10 observaciones:\n")
print(head(corp_rep_data, 10))
cat("\nEstadísticas descriptivas:\n")
print(summary(corp_rep_data))

# ============================================================================
# PASO 2: DEFINIR MODELO DE MEDICIÓN
# ============================================================================

cat("\n\nPASO 2: Definiendo modelo de medición...\n")
cat("-" %+% rep("-", 70) %+% "-\n\n")

# Crear constructos reflectivos
measurement_model <- constructs(
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
)

cat("✓ Modelo de medición definido:\n")
cat("  - COMP (Competencia): comp_1, comp_2, comp_3\n")
cat("  - LIKE (Simpatía): like_1, like_2, like_3\n")
cat("  - CUSA (Satisfacción): cusa\n")
cat("  - CUSL (Lealtad): cusl_1, cusl_2, cusl_3\n\n")

# ============================================================================
# PASO 3: DEFINIR MODELO ESTRUCTURAL
# ============================================================================

cat("PASO 3: Definiendo modelo estructural...\n")
cat("-" %+% rep("-", 70) %+% "-\n\n")

# Crear paths (relaciones entre constructos)
structural_model <- relationships(
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = "CUSA", to = "CUSL")
)

cat("✓ Modelo estructural definido:\n")
cat("  - COMP → CUSA, CUSL\n")
cat("  - LIKE → CUSA, CUSL\n")
cat("  - CUSA → CUSL\n\n")

# ============================================================================
# PASO 4: ESTIMAR MODELO PLS-SEM
# ============================================================================

cat("PASO 4: Estimando modelo PLS-SEM...\n")
cat("-" %+% rep("-", 70) %+% "-\n\n")

# Estimar modelo
pls_model <- estimate_pls(
  data = corp_rep_data,
  measurement_model = measurement_model,
  structural_model = structural_model,
  inner_weights = path_weighting(),
  missing = mean_replacement()
)

cat("✓ Modelo estimado exitosamente\n\n")

# Obtener resumen
model_summary <- summary(pls_model)

# ============================================================================
# PASO 5: EVALUAR MODELO DE MEDICIÓN
# ============================================================================

cat("PASO 5: Evaluando modelo de medición...\n")
cat("-" %+% rep("-", 70) %+% "-\n\n")

# Confiabilidad y validez convergente
cat("5.1 CONFIABILIDAD Y VALIDEZ CONVERGENTE\n")
cat("    (Alpha, rhoA, Composite Reliability, AVE)\n\n")

reliability <- model_summary$reliability
print(reliability)

cat("\nInterpretación:\n")
cat("  ✓ Alpha > 0.70: Fiabilidad interna aceptable\n")
cat("  ✓ rhoC > 0.70: Fiabilidad compuesta aceptable\n")
cat("  ✓ AVE > 0.50: Validez convergente aceptable\n\n")

# Cargas externas
cat("5.2 CARGAS EXTERNAS (OUTER LOADINGS)\n\n")

loadings <- model_summary$loadings
print(loadings)

cat("\nInterpretación:\n")
cat("  ✓ Cargas > 0.708: Ítems significativos en constructos\n")
cat("  ✓ Cargas² > 0.50: Varianza explicada > 50%\n\n")

# Validez discriminante
cat("5.3 VALIDEZ DISCRIMINANTE (HTMT)\n\n")

htmt <- model_summary$validity$htmt
print(htmt)

cat("\nInterpretación:\n")
cat("  ✓ HTMT < 0.90: Validez discriminante aceptable\n")
cat("  ✓ HTMT < 0.85: Criterio más estricto\n\n")

# ============================================================================
# PASO 6: EVALUAR MODELO ESTRUCTURAL
# ============================================================================

cat("PASO 6: Evaluando modelo estructural...\n")
cat("-" %+% rep("-", 70) %+% "-\n\n")

# Coeficientes de ruta y R²
cat("6.1 COEFICIENTES DE RUTA Y R²\n\n")

paths <- model_summary$paths
print(paths)

cat("\nInterpretación:\n")
cat("  ✓ R² > 0.26: Efecto pequeño\n")
cat("  ✓ R² > 0.13: Efecto medio\n")
cat("  ✓ R² > 0.26: Efecto grande\n\n")

# ============================================================================
# PASO 7: BOOTSTRAPPING
# ============================================================================

cat("PASO 7: Ejecutando bootstrapping (5,000 submuestras)...\n")
cat("-" %+% rep("-", 70) %+% "-\n\n")

# Ejecutar bootstrapping
boot_model <- bootstrap_model(
  seminr_model = pls_model,
  nboot = 5000,
  cores = 1,
  seed = 123
)

cat("✓ Bootstrapping completado\n\n")

# Obtener resumen de bootstrap
boot_summary <- summary(boot_model)

# Coeficientes de ruta con bootstrap
cat("7.1 COEFICIENTES DE RUTA CON BOOTSTRAP\n")
cat("    (β, t-value, p-value, CI 95%)\n\n")

bootstrapped_paths <- boot_summary$bootstrapped_paths
print(bootstrapped_paths)

cat("\nInterpretación:\n")
cat("  ✓ p-value < 0.05: Relación estadísticamente significativa\n")
cat("  ✓ CI 95% no incluye cero: Significancia confirmada\n")
cat("  ✓ t-value > 1.96: Significancia bilateral (α = 0.05)\n\n")

# ============================================================================
# PASO 8: VALIDACIONES METODOLÓGICAS
# ============================================================================

cat("PASO 8: Validaciones metodológicas (Hair et al. 2022)...\n")
cat("-" %+% rep("-", 70) %+% "-\n\n")

# Verificar criterios
cat("8.1 CONFIABILIDAD\n")
alpha_ok <- all(reliability[, "alpha"] > 0.70, na.rm = TRUE)
rhoc_ok <- all(reliability[, "rhoC"] > 0.70, na.rm = TRUE)
cat("  Alpha > 0.70:", ifelse(alpha_ok, "✓ CUMPLE", "✗ NO CUMPLE"), "\n")
cat("  rhoC > 0.70:", ifelse(rhoc_ok, "✓ CUMPLE", "✗ NO CUMPLE"), "\n\n")

cat("8.2 VALIDEZ CONVERGENTE\n")
ave_ok <- all(reliability[, "AVE"] > 0.50, na.rm = TRUE)
cat("  AVE > 0.50:", ifelse(ave_ok, "✓ CUMPLE", "✗ NO CUMPLE"), "\n\n")

cat("8.3 VALIDEZ DISCRIMINANTE\n")
htmt_ok <- all(htmt < 0.90, na.rm = TRUE)
cat("  HTMT < 0.90:", ifelse(htmt_ok, "✓ CUMPLE", "✗ NO CUMPLE"), "\n\n")

cat("8.4 CARGAS EXTERNAS\n")
weak_items <- sum(loadings < 0.708 & loadings > 0)
cat("  Ítems con cargas < 0.708:", weak_items, "\n")
cat("  Status:", ifelse(weak_items == 0, "✓ CUMPLE", "✗ REVISAR"), "\n\n")

cat("8.5 SIGNIFICANCIA DE PATHS\n")
p_values <- bootstrapped_paths[, , 3]
significant_paths <- sum(p_values < 0.05, na.rm = TRUE)
total_paths <- sum(!is.na(p_values) & p_values != 0)
cat("  Paths significativos:", significant_paths, "/", total_paths, "\n")
cat("  Porcentaje:", round(significant_paths / total_paths * 100, 1), "%\n\n")

# ============================================================================
# PASO 9: RESUMEN GENERAL
# ============================================================================

cat("PASO 9: Resumen general del análisis\n")
cat("-" %+% rep("-", 70) %+% "-\n\n")

cat("MODELO DE MEDICIÓN:\n")
cat("  Confiabilidad:", ifelse(alpha_ok && rhoc_ok, "✓ BUENA", "✗ REVISAR"), "\n")
cat("  Validez Convergente:", ifelse(ave_ok, "✓ BUENA", "✗ REVISAR"), "\n")
cat("  Validez Discriminante:", ifelse(htmt_ok, "✓ BUENA", "✗ REVISAR"), "\n\n")

cat("MODELO ESTRUCTURAL:\n")
cat("  Paths Significativos:", significant_paths, "/", total_paths, "\n")
cat("  R² promedio:", round(mean(paths[, "R²"], na.rm = TRUE), 4), "\n\n")

overall_quality <- ifelse(
  alpha_ok && rhoc_ok && ave_ok && htmt_ok && significant_paths > 0,
  "BUENA",
  "NECESITA REVISIÓN"
)

cat("EVALUACIÓN GENERAL:", overall_quality, "\n\n")

# ============================================================================
# PASO 10: EXPORTAR RESULTADOS
# ============================================================================

cat("PASO 10: Exportando resultados...\n")
cat("-" %+% rep("-", 70) %+% "-\n\n")

# Crear tabla de confiabilidad
reliability_table <- data.frame(
  Construct = rownames(reliability),
  Alpha = round(reliability[, "alpha"], 4),
  rhoA = round(reliability[, "rhoA"], 4),
  rhoC = round(reliability[, "rhoC"], 4),
  AVE = round(reliability[, "AVE"], 4)
)

cat("Tabla 1: Confiabilidad y Validez Convergente\n")
print(reliability_table)
cat("\n")

# Crear tabla de cargas
loadings_long <- data.frame()
for (i in 1:nrow(loadings)) {
  for (j in 1:ncol(loadings)) {
    if (loadings[i, j] != 0) {
      loadings_long <- rbind(loadings_long, data.frame(
        Item = rownames(loadings)[i],
        Construct = colnames(loadings)[j],
        Loading = round(loadings[i, j], 4),
        Loading_Squared = round(loadings[i, j]^2, 4)
      ))
    }
  }
}

cat("Tabla 2: Cargas Externas\n")
print(loadings_long)
cat("\n")

# Crear tabla de HTMT
htmt_long <- data.frame()
for (i in 1:(nrow(htmt)-1)) {
  for (j in (i+1):ncol(htmt)) {
    htmt_long <- rbind(htmt_long, data.frame(
      Construct_1 = rownames(htmt)[i],
      Construct_2 = colnames(htmt)[j],
      HTMT = round(htmt[i, j], 4)
    ))
  }
}

cat("Tabla 3: Validez Discriminante (HTMT)\n")
print(htmt_long)
cat("\n")

# Crear tabla de paths
paths_long <- data.frame()
for (i in 1:nrow(bootstrapped_paths)) {
  for (j in 1:ncol(bootstrapped_paths)) {
    if (bootstrapped_paths[i, j, 1] != 0) {
      paths_long <- rbind(paths_long, data.frame(
        From = rownames(bootstrapped_paths)[i],
        To = colnames(bootstrapped_paths)[j],
        Beta = round(bootstrapped_paths[i, j, 1], 4),
        t_value = round(bootstrapped_paths[i, j, 2], 4),
        p_value = round(bootstrapped_paths[i, j, 3], 4),
        CI_Lower = round(bootstrapped_paths[i, j, 4], 4),
        CI_Upper = round(bootstrapped_paths[i, j, 5], 4),
        Significant = ifelse(bootstrapped_paths[i, j, 3] < 0.05, "Yes", "No")
      ))
    }
  }
}

cat("Tabla 4: Coeficientes de Ruta (Bootstrap)\n")
print(paths_long)
cat("\n")

# ============================================================================
# PASO 11: VISUALIZACIÓN DEL MODELO
# ============================================================================

cat("PASO 11: Generando visualización del modelo...\n")
cat("-" %+% rep("-", 70) %+% "-\n\n")

# Crear gráfico
png("pls_model_diagram.png", width = 1200, height = 800)
plot(pls_model)
dev.off()

cat("✓ Gráfico guardado como: pls_model_diagram.png\n\n")

# ============================================================================
# FIN DEL ANÁLISIS
# ============================================================================

cat("=" %+% rep("=", 70) %+% "=\n")
cat("✓ ANÁLISIS COMPLETADO EXITOSAMENTE\n")
cat("=" %+% rep("=", 70) %+% "=\n\n")

cat("Resultados disponibles en:\n")
cat("  - Tabla 1: Confiabilidad y Validez Convergente\n")
cat("  - Tabla 2: Cargas Externas\n")
cat("  - Tabla 3: Validez Discriminante (HTMT)\n")
cat("  - Tabla 4: Coeficientes de Ruta (Bootstrap)\n")
cat("  - Gráfico: pls_model_diagram.png\n\n")

cat("Próximos pasos:\n")
cat("  1. Revisar todas las tablas de resultados\n")
cat("  2. Verificar que se cumplen los criterios de Hair et al.\n")
cat("  3. Interpretar los coeficientes de ruta\n")
cat("  4. Integrar resultados en manuscrito académico\n")
cat("  5. Guardar datos para replicabilidad (seed = 123)\n\n")
