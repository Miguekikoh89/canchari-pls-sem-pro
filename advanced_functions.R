# ============================================================================
# Funciones Avanzadas para PLS-SEM Academic Analyzer
# ============================================================================
# Funciones para manejo dinámico de constructos, paths y análisis
# Pueden integrarse en versiones futuras de la aplicación

library(seminr)
library(tidyverse)

# ============================================================================
# 1. FUNCIONES PARA MANEJO DE CONSTRUCTOS
# ============================================================================

# Función para crear constructos dinámicamente desde lista
create_constructs_from_list <- function(construct_list) {
  #' Crea un objeto de medición de seminr desde una lista de constructos
  #' 
  #' @param construct_list Lista con estructura:
  #'   list(
  #'     list(name = "COMP", items = c("Q1", "Q2", "Q3")),
  #'     list(name = "LIKE", items = "Q4"),
  #'     ...
  #'   )
  #'
  #' @return Objeto measurement_model de seminr
  
  constructs_expr <- list()
  
  for (i in seq_along(construct_list)) {
    construct <- construct_list[[i]]
    name <- construct$name
    items <- construct$items
    
    if (length(items) == 1) {
      # Variable de un solo ítem
      constructs_expr[[i]] <- substitute(
        composite(NAME, single_item(ITEM)),
        list(NAME = name, ITEM = items[1])
      )
    } else {
      # Constructo reflectivo multi-ítem
      constructs_expr[[i]] <- substitute(
        composite(NAME, multi_items(PREFIX, RANGE)),
        list(NAME = name, PREFIX = "", RANGE = items)
      )
    }
  }
  
  # Combinar en un único objeto constructs()
  constructs_call <- as.call(c(list(quote(constructs)), constructs_expr))
  eval(constructs_call)
}

# Función para validar nombres de constructos
validate_construct_names <- function(names) {
  #' Valida que los nombres de constructos sean válidos
  #' 
  #' @param names Vector de nombres de constructos
  #' @return Logical: TRUE si todos son válidos
  
  # No deben estar vacíos
  if (any(names == "")) return(FALSE)
  
  # Deben ser únicos
  if (length(names) != length(unique(names))) return(FALSE)
  
  # Deben ser válidos como nombres de R
  valid <- all(make.names(names) == names)
  
  return(valid)
}

# ============================================================================
# 2. FUNCIONES PARA MANEJO DE PATHS
# ============================================================================

# Función para crear modelo estructural dinámicamente
create_structural_model_from_list <- function(paths_list) {
  #' Crea un modelo estructural desde una lista de paths
  #'
  #' @param paths_list Lista con estructura:
  #'   list(
  #'     list(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  #'     list(from = "CUSA", to = "CUSL"),
  #'     ...
  #'   )
  #'
  #' @return Objeto structural_model de seminr
  
  paths_expr <- list()
  
  for (i in seq_along(paths_list)) {
    path <- paths_list[[i]]
    from <- path$from
    to <- path$to
    
    paths_expr[[i]] <- substitute(
      paths(from = FROM, to = TO),
      list(FROM = from, TO = to)
    )
  }
  
  # Combinar en un único objeto relationships()
  relationships_call <- as.call(c(list(quote(relationships)), paths_expr))
  eval(relationships_call)
}

# Función para validar paths
validate_paths <- function(paths_list, construct_names) {
  #' Valida que los paths sean válidos
  #'
  #' @param paths_list Lista de paths
  #' @param construct_names Vector de nombres de constructos válidos
  #' @return Logical: TRUE si todos los paths son válidos
  
  for (path in paths_list) {
    from <- path$from
    to <- path$to
    
    # Verificar que todos los constructos existan
    if (!all(from %in% construct_names) || !all(to %in% construct_names)) {
      return(FALSE)
    }
    
    # Verificar que no haya auto-loops
    if (any(from %in% to)) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

# ============================================================================
# 3. FUNCIONES PARA ANÁLISIS AVANZADO
# ============================================================================

# Función para detectar multicolinealidad
detect_multicollinearity <- function(data, threshold = 0.9) {
  #' Detecta multicolinealidad entre variables
  #'
  #' @param data Data frame con variables
  #' @param threshold Umbral de correlación
  #' @return Data frame con pares de variables altamente correlacionadas
  
  cor_matrix <- cor(data, use = "complete.obs")
  
  # Obtener pares con correlación > threshold
  high_cor <- data.frame()
  for (i in 1:(ncol(cor_matrix)-1)) {
    for (j in (i+1):ncol(cor_matrix)) {
      if (abs(cor_matrix[i, j]) > threshold) {
        high_cor <- rbind(high_cor, data.frame(
          Var1 = colnames(cor_matrix)[i],
          Var2 = colnames(cor_matrix)[j],
          Correlation = round(cor_matrix[i, j], 4)
        ))
      }
    }
  }
  
  return(high_cor)
}

# Función para evaluar calidad del modelo
evaluate_model_quality <- function(model_summary, boot_summary) {
  #' Evalúa la calidad general del modelo según Hair et al.
  #'
  #' @param model_summary Resumen del modelo PLS-SEM
  #' @param boot_summary Resumen del modelo bootstrapped
  #' @return Lista con evaluación de calidad
  
  quality_report <- list()
  
  # 1. Evaluación del modelo de medición
  reliability <- model_summary$reliability
  
  # Confiabilidad
  alpha_ok <- all(reliability[, "alpha"] > 0.70, na.rm = TRUE)
  rhoc_ok <- all(reliability[, "rhoC"] > 0.70, na.rm = TRUE)
  
  quality_report$measurement_model <- list(
    alpha_ok = alpha_ok,
    rhoc_ok = rhoc_ok,
    overall = alpha_ok && rhoc_ok
  )
  
  # Validez convergente
  ave_ok <- all(reliability[, "AVE"] > 0.50, na.rm = TRUE)
  quality_report$convergent_validity <- list(
    ave_ok = ave_ok
  )
  
  # Validez discriminante
  htmt <- model_summary$validity$htmt
  htmt_ok <- all(htmt < 0.90, na.rm = TRUE)
  quality_report$discriminant_validity <- list(
    htmt_ok = htmt_ok
  )
  
  # 2. Evaluación del modelo estructural
  paths <- boot_summary$bootstrapped_paths
  
  # Significancia de paths
  p_values <- paths[, , 3]  # Tercera dimensión contiene p-values
  significant_paths <- sum(p_values < 0.05, na.rm = TRUE)
  total_paths <- sum(!is.na(p_values) & p_values != 0)
  
  quality_report$structural_model <- list(
    significant_paths = significant_paths,
    total_paths = total_paths,
    significance_rate = ifelse(total_paths > 0, significant_paths / total_paths, NA)
  )
  
  # 3. Puntuación general
  all_ok <- all(
    alpha_ok, rhoc_ok, ave_ok, htmt_ok,
    significant_paths > 0
  )
  
  quality_report$overall_quality <- ifelse(all_ok, "GOOD", "NEEDS REVISION")
  
  return(quality_report)
}

# ============================================================================
# 4. FUNCIONES PARA GENERACIÓN DE REPORTES
# ============================================================================

# Función para crear reporte en formato texto
generate_text_report <- function(model_summary, boot_summary, filename = NULL) {
  #' Genera un reporte de texto del análisis PLS-SEM
  #'
  #' @param model_summary Resumen del modelo
  #' @param boot_summary Resumen del modelo bootstrapped
  #' @param filename Nombre del archivo (opcional)
  #' @return Texto del reporte
  
  report <- ""
  
  # Encabezado
  report <- paste0(report, "=" %+% rep("=", 70) %+% "=\n")
  report <- paste0(report, "PLS-SEM ANALYSIS REPORT\n")
  report <- paste0(report, "Generated: ", Sys.time(), "\n")
  report <- paste0(report, "=" %+% rep("=", 70) %+% "=\n\n")
  
  # 1. Modelo de Medición
  report <- paste0(report, "1. MEASUREMENT MODEL EVALUATION\n")
  report <- paste0(report, "-" %+% rep("-", 70) %+% "-\n\n")
  
  reliability <- model_summary$reliability
  report <- paste0(report, "Reliability and Convergent Validity:\n")
  report <- paste0(report, capture.output(print(reliability)), "\n\n")
  
  # 2. Validez Discriminante
  report <- paste0(report, "2. DISCRIMINANT VALIDITY (HTMT)\n")
  report <- paste0(report, "-" %+% rep("-", 70) %+% "-\n\n")
  
  htmt <- model_summary$validity$htmt
  report <- paste0(report, capture.output(print(htmt)), "\n\n")
  
  # 3. Modelo Estructural
  report <- paste0(report, "3. STRUCTURAL MODEL\n")
  report <- paste0(report, "-" %+% rep("-", 70) %+% "-\n\n")
  
  paths <- boot_summary$bootstrapped_paths
  report <- paste0(report, "Path Coefficients (Bootstrap Results):\n")
  report <- paste0(report, capture.output(print(paths)), "\n\n")
  
  # Guardar si se especifica filename
  if (!is.null(filename)) {
    writeLines(report, filename)
  }
  
  return(report)
}

# Función para crear tabla de resultados en formato académico
format_academic_table <- function(data, caption = "", digits = 4) {
  #' Formatea una tabla para publicación académica
  #'
  #' @param data Data frame con resultados
  #' @param caption Título de la tabla
  #' @param digits Número de decimales
  #' @return Data frame formateado
  
  # Redondear números
  numeric_cols <- sapply(data, is.numeric)
  data[numeric_cols] <- round(data[numeric_cols], digits)
  
  # Agregar atributos
  attr(data, "caption") <- caption
  
  return(data)
}

# ============================================================================
# 5. FUNCIONES PARA VALIDACIÓN AVANZADA
# ============================================================================

# Función para verificar supuestos de PLS-SEM
check_pls_assumptions <- function(data) {
  #' Verifica los supuestos de PLS-SEM
  #'
  #' @param data Data frame con variables
  #' @return Lista con resultados de verificación
  
  assumptions <- list()
  
  # 1. Tamaño de muestra
  n <- nrow(data)
  assumptions$sample_size <- list(
    n = n,
    adequate = n >= 30,
    recommendation = ifelse(n >= 100, "Large", ifelse(n >= 30, "Adequate", "Too small"))
  )
  
  # 2. Valores faltantes
  missing_pct <- sum(is.na(data)) / (nrow(data) * ncol(data)) * 100
  assumptions$missing_values <- list(
    percentage = round(missing_pct, 2),
    acceptable = missing_pct < 5
  )
  
  # 3. Outliers (usando distancia de Mahalanobis)
  if (nrow(data) > ncol(data)) {
    mahal_dist <- mahalanobis(data, colMeans(data, na.rm = TRUE), 
                              cov(data, use = "complete.obs"))
    outliers <- sum(mahal_dist > qchisq(0.95, ncol(data)))
    assumptions$outliers <- list(
      count = outliers,
      percentage = round(outliers / nrow(data) * 100, 2),
      acceptable = outliers / nrow(data) < 0.05
    )
  }
  
  return(assumptions)
}

# ============================================================================
# 6. FUNCIONES PARA ANÁLISIS DE SENSIBILIDAD
# ============================================================================

# Función para análisis de estabilidad de constructos
analyze_construct_stability <- function(boot_summary) {
  #' Analiza la estabilidad de los constructos mediante bootstrapping
  #'
  #' @param boot_summary Resumen del modelo bootstrapped
  #' @return Data frame con medidas de estabilidad
  
  stability <- data.frame()
  
  # Extraer desviaciones estándar de bootstrap
  # (Implementación específica depende de estructura de boot_summary)
  
  return(stability)
}

# ============================================================================
# EJEMPLO DE USO
# ============================================================================

# Ejemplo: Crear constructos dinámicamente
example_constructs <- list(
  list(name = "COMP", items = c("comp_1", "comp_2", "comp_3")),
  list(name = "LIKE", items = c("like_1", "like_2", "like_3")),
  list(name = "CUSA", items = "cusa"),
  list(name = "CUSL", items = c("cusl_1", "cusl_2", "cusl_3"))
)

# Ejemplo: Crear paths dinámicamente
example_paths <- list(
  list(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  list(from = "CUSA", to = "CUSL")
)

# Ejemplo: Validar constructos
# valid <- validate_construct_names(sapply(example_constructs, function(x) x$name))
# print(paste("Constructos válidos:", valid))

# Ejemplo: Validar paths
# valid_paths <- validate_paths(example_paths, sapply(example_constructs, function(x) x$name))
# print(paste("Paths válidos:", valid_paths))

cat("✓ Funciones avanzadas cargadas correctamente\n")
cat("Disponibles:\n")
cat("  - create_constructs_from_list()\n")
cat("  - create_structural_model_from_list()\n")
cat("  - validate_construct_names()\n")
cat("  - validate_paths()\n")
cat("  - detect_multicollinearity()\n")
cat("  - evaluate_model_quality()\n")
cat("  - generate_text_report()\n")
cat("  - format_academic_table()\n")
cat("  - check_pls_assumptions()\n")
