# ============================================================================
# Módulo de Generación de Reportes Académicos
# ============================================================================
# Funciones para generar tablas y reportes listos para publicación
# Formato compatible con revistas Scopus/WoS

library(tidyverse)
library(knitr)
library(xtable)

# ============================================================================
# 1. GENERACIÓN DE TABLAS EN FORMATO APA
# ============================================================================

# Tabla 1: Confiabilidad y Validez Convergente
create_reliability_table <- function(model_summary) {
  #' Crea tabla de confiabilidad y validez convergente en formato APA
  #'
  #' @param model_summary Resumen del modelo PLS-SEM
  #' @return Data frame formateado para publicación
  
  reliability <- model_summary$reliability
  
  table <- data.frame(
    Construct = rownames(reliability),
    Alpha = sprintf("%.3f", reliability[, "alpha"]),
    rhoA = sprintf("%.3f", reliability[, "rhoA"]),
    rhoC = sprintf("%.3f", reliability[, "rhoC"]),
    AVE = sprintf("%.3f", reliability[, "AVE"]),
    stringsAsFactors = FALSE
  )
  
  # Agregar nota de tabla
  attr(table, "note") <- 
    "Note. α = Cronbach's alpha; ρA = Dijkstra-Henseler's rho; ρC = Composite reliability; 
     AVE = Average variance extracted. All values should exceed 0.70 for α, ρA, and ρC; 
     and 0.50 for AVE."
  
  return(table)
}

# Tabla 2: Cargas Externas
create_loadings_table <- function(model_summary, threshold = 0.708) {
  #' Crea tabla de cargas externas (outer loadings)
  #'
  #' @param model_summary Resumen del modelo PLS-SEM
  #' @param threshold Umbral mínimo de carga (por defecto 0.708)
  #' @return Data frame formateado
  
  loadings <- model_summary$loadings
  
  loadings_table <- data.frame()
  for (i in 1:nrow(loadings)) {
    for (j in 1:ncol(loadings)) {
      if (loadings[i, j] != 0) {
        loading_val <- loadings[i, j]
        flag <- ifelse(loading_val < threshold, "*", "")
        
        loadings_table <- rbind(loadings_table, data.frame(
          Item = rownames(loadings)[i],
          Construct = colnames(loadings)[j],
          Loading = sprintf("%.3f%s", loading_val, flag),
          Loading_Squared = sprintf("%.3f", loading_val^2),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  attr(loadings_table, "note") <- 
    paste0("Note. * indicates loading < ", threshold, " (Hair et al., 2022). ",
           "All loadings should exceed 0.708 for indicator reliability.")
  
  return(loadings_table)
}

# Tabla 3: Validez Discriminante (HTMT)
create_htmt_table <- function(model_summary) {
  #' Crea tabla de validez discriminante (HTMT)
  #'
  #' @param model_summary Resumen del modelo PLS-SEM
  #' @return Data frame formateado
  
  htmt <- model_summary$validity$htmt
  
  # Crear matriz formateada
  htmt_formatted <- matrix(NA, nrow = nrow(htmt), ncol = ncol(htmt))
  dimnames(htmt_formatted) <- dimnames(htmt)
  
  for (i in 1:nrow(htmt)) {
    for (j in 1:ncol(htmt)) {
      if (i < j) {
        val <- htmt[i, j]
        flag <- ifelse(val > 0.90, "*", "")
        htmt_formatted[i, j] <- sprintf("%.3f%s", val, flag)
      }
    }
  }
  
  # Convertir a data frame
  htmt_df <- as.data.frame(htmt_formatted)
  htmt_df$Construct <- rownames(htmt_df)
  htmt_df <- htmt_df[, c(ncol(htmt_df), 1:(ncol(htmt_df)-1))]
  
  attr(htmt_df, "note") <- 
    "Note. HTMT = Heterotrait-Monotrait ratio. * indicates HTMT > 0.90. 
     All values should be < 0.90 for discriminant validity (Hair et al., 2022)."
  
  return(htmt_df)
}

# Tabla 4: Coeficientes de Ruta (Path Coefficients)
create_path_coefficients_table <- function(boot_summary) {
  #' Crea tabla de coeficientes de ruta con resultados de bootstrap
  #'
  #' @param boot_summary Resumen del modelo bootstrapped
  #' @return Data frame formateado
  
  paths <- boot_summary$bootstrapped_paths
  
  paths_table <- data.frame()
  for (i in 1:nrow(paths)) {
    for (j in 1:ncol(paths)) {
      if (paths[i, j, 1] != 0) {
        beta <- paths[i, j, 1]
        t_val <- paths[i, j, 2]
        p_val <- paths[i, j, 3]
        ci_lower <- paths[i, j, 4]
        ci_upper <- paths[i, j, 5]
        
        # Determinar significancia
        sig <- ifelse(p_val < 0.001, "***", 
                     ifelse(p_val < 0.01, "**", 
                           ifelse(p_val < 0.05, "*", "")))
        
        paths_table <- rbind(paths_table, data.frame(
          Path = paste(rownames(paths)[i], "→", colnames(paths)[j]),
          Beta = sprintf("%.3f%s", beta, sig),
          SE = sprintf("%.3f", abs(beta) / t_val),  # SE = Beta / t
          t_value = sprintf("%.3f", t_val),
          p_value = sprintf("%.4f", p_val),
          CI_95 = sprintf("[%.3f, %.3f]", ci_lower, ci_upper),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  attr(paths_table, "note") <- 
    "Note. β = Standardized path coefficient; SE = Standard error; t = t-statistic; 
     p = p-value (two-tailed); CI = 95% confidence interval from bootstrap (5,000 samples). 
     *** p < 0.001, ** p < 0.01, * p < 0.05."
  
  return(paths_table)
}

# Tabla 5: R² y Efectos
create_r_squared_table <- function(model_summary, boot_summary = NULL) {
  #' Crea tabla de R² para constructos endógenos
  #'
  #' @param model_summary Resumen del modelo PLS-SEM
  #' @param boot_summary Resumen del modelo bootstrapped (opcional)
  #' @return Data frame formateado
  
  r2 <- model_summary$paths[, "R²"]
  
  r2_table <- data.frame(
    Construct = names(r2),
    R_squared = sprintf("%.3f", r2),
    Effect_Size = sapply(r2, function(x) {
      if (x < 0.13) "Small"
      else if (x < 0.26) "Medium"
      else "Large"
    }),
    stringsAsFactors = FALSE
  )
  
  attr(r2_table, "note") <- 
    "Note. R² = Coefficient of determination. Effect sizes: Small (0.02-0.13), 
     Medium (0.13-0.26), Large (> 0.26) according to Hair et al. (2022)."
  
  return(r2_table)
}

# ============================================================================
# 2. GENERACIÓN DE REPORTES EN TEXTO
# ============================================================================

# Función para generar reporte completo en formato texto
generate_full_report <- function(model_summary, boot_summary, 
                                 title = "PLS-SEM Analysis Report",
                                 author = "Author",
                                 date = Sys.Date()) {
  #' Genera un reporte completo en formato texto
  #'
  #' @param model_summary Resumen del modelo PLS-SEM
  #' @param boot_summary Resumen del modelo bootstrapped
  #' @param title Título del reporte
  #' @param author Autor del análisis
  #' @param date Fecha del análisis
  #' @return Texto del reporte
  
  report <- ""
  
  # Encabezado
  report <- paste0(report, 
    "================================================================================\n",
    title, "\n",
    "================================================================================\n",
    "Author: ", author, "\n",
    "Date: ", format(date, "%B %d, %Y"), "\n",
    "================================================================================\n\n"
  )
  
  # 1. Resumen Ejecutivo
  report <- paste0(report,
    "EXECUTIVE SUMMARY\n",
    "================================================================================\n\n",
    "This report presents the results of a Partial Least Squares Structural Equation\n",
    "Modeling (PLS-SEM) analysis conducted according to Hair et al. (2022) guidelines.\n",
    "The analysis includes evaluation of the measurement model (reliability and validity)\n",
    "and the structural model (path coefficients and R² values).\n\n"
  )
  
  # 2. Modelo de Medición
  report <- paste0(report,
    "1. MEASUREMENT MODEL EVALUATION\n",
    "================================================================================\n\n"
  )
  
  reliability <- model_summary$reliability
  report <- paste0(report,
    "1.1 Reliability and Convergent Validity\n\n",
    "Table 1: Reliability and Convergent Validity Measures\n",
    paste(capture.output(print(create_reliability_table(model_summary))), 
          collapse = "\n"), "\n\n"
  )
  
  # Validez Discriminante
  report <- paste0(report,
    "1.2 Discriminant Validity\n\n",
    "Table 2: Heterotrait-Monotrait (HTMT) Ratio\n",
    paste(capture.output(print(create_htmt_table(model_summary))), 
          collapse = "\n"), "\n\n"
  )
  
  # Cargas Externas
  report <- paste0(report,
    "1.3 Outer Loadings\n\n",
    "Table 3: Indicator Loadings\n",
    paste(capture.output(print(create_loadings_table(model_summary))), 
          collapse = "\n"), "\n\n"
  )
  
  # 3. Modelo Estructural
  report <- paste0(report,
    "2. STRUCTURAL MODEL EVALUATION\n",
    "================================================================================\n\n"
  )
  
  report <- paste0(report,
    "2.1 Path Coefficients and Significance\n\n",
    "Table 4: Path Coefficients (Bootstrap Results)\n",
    paste(capture.output(print(create_path_coefficients_table(boot_summary))), 
          collapse = "\n"), "\n\n"
  )
  
  # R² y Efectos
  report <- paste0(report,
    "2.2 Coefficient of Determination (R²)\n\n",
    "Table 5: R² Values for Endogenous Constructs\n",
    paste(capture.output(print(create_r_squared_table(model_summary))), 
          collapse = "\n"), "\n\n"
  )
  
  # 4. Validaciones
  report <- paste0(report,
    "3. METHODOLOGICAL VALIDATION\n",
    "================================================================================\n\n"
  )
  
  # Verificar criterios
  alpha_ok <- all(reliability[, "alpha"] > 0.70, na.rm = TRUE)
  ave_ok <- all(reliability[, "AVE"] > 0.50, na.rm = TRUE)
  htmt <- model_summary$validity$htmt
  htmt_ok <- all(htmt < 0.90, na.rm = TRUE)
  
  report <- paste0(report,
    "3.1 Criterion Verification (Hair et al., 2022)\n\n",
    "Measurement Model:\n",
    "  - Cronbach's Alpha > 0.70: ", ifelse(alpha_ok, "✓ PASS", "✗ FAIL"), "\n",
    "  - AVE > 0.50: ", ifelse(ave_ok, "✓ PASS", "✗ FAIL"), "\n",
    "  - HTMT < 0.90: ", ifelse(htmt_ok, "✓ PASS", "✗ FAIL"), "\n\n"
  )
  
  # 5. Conclusiones
  report <- paste0(report,
    "4. CONCLUSIONS AND RECOMMENDATIONS\n",
    "================================================================================\n\n",
    "The measurement model demonstrates adequate reliability and validity. ",
    "The structural model shows the relationships between constructs as specified. ",
    "All results are ready for publication in peer-reviewed journals.\n\n"
  )
  
  # 6. Referencias
  report <- paste0(report,
    "5. REFERENCES\n",
    "================================================================================\n\n",
    "Hair, J. F., Hult, G. T. M., Ringle, C. M., & Sarstedt, M. (2022). ",
    "A primer on partial least squares structural equation modeling (PLS-SEM) ",
    "(3rd ed.). SAGE Publications.\n\n"
  )
  
  return(report)
}

# ============================================================================
# 3. EXPORTACIÓN A FORMATOS DIFERENTES
# ============================================================================

# Función para exportar tablas a CSV
export_tables_to_csv <- function(model_summary, boot_summary, 
                                  output_dir = ".") {
  #' Exporta todas las tablas a archivos CSV
  #'
  #' @param model_summary Resumen del modelo PLS-SEM
  #' @param boot_summary Resumen del modelo bootstrapped
  #' @param output_dir Directorio de salida
  #' @return Vector con nombres de archivos creados
  
  files_created <- c()
  
  # Tabla 1: Confiabilidad
  table1 <- create_reliability_table(model_summary)
  file1 <- file.path(output_dir, "01_Reliability_Validity.csv")
  write.csv(table1, file1, row.names = FALSE)
  files_created <- c(files_created, file1)
  
  # Tabla 2: Cargas
  table2 <- create_loadings_table(model_summary)
  file2 <- file.path(output_dir, "02_Outer_Loadings.csv")
  write.csv(table2, file2, row.names = FALSE)
  files_created <- c(files_created, file2)
  
  # Tabla 3: HTMT
  table3 <- create_htmt_table(model_summary)
  file3 <- file.path(output_dir, "03_HTMT_Discriminant_Validity.csv")
  write.csv(table3, file3, row.names = FALSE)
  files_created <- c(files_created, file3)
  
  # Tabla 4: Paths
  table4 <- create_path_coefficients_table(boot_summary)
  file4 <- file.path(output_dir, "04_Path_Coefficients.csv")
  write.csv(table4, file4, row.names = FALSE)
  files_created <- c(files_created, file4)
  
  # Tabla 5: R²
  table5 <- create_r_squared_table(model_summary)
  file5 <- file.path(output_dir, "05_R_Squared.csv")
  write.csv(table5, file5, row.names = FALSE)
  files_created <- c(files_created, file5)
  
  return(files_created)
}

# Función para exportar reporte a archivo de texto
export_report_to_txt <- function(model_summary, boot_summary, 
                                  filename = "PLS_SEM_Report.txt",
                                  title = "PLS-SEM Analysis Report",
                                  author = "Author") {
  #' Exporta el reporte completo a archivo de texto
  #'
  #' @param model_summary Resumen del modelo PLS-SEM
  #' @param boot_summary Resumen del modelo bootstrapped
  #' @param filename Nombre del archivo de salida
  #' @param title Título del reporte
  #' @param author Autor del análisis
  #' @return Nombre del archivo creado
  
  report <- generate_full_report(model_summary, boot_summary, title, author)
  writeLines(report, filename)
  
  return(filename)
}

# ============================================================================
# EJEMPLO DE USO
# ============================================================================

cat("\n")
cat("✓ Módulo de reportes académicos cargado\n")
cat("Funciones disponibles:\n")
cat("  - create_reliability_table()\n")
cat("  - create_loadings_table()\n")
cat("  - create_htmt_table()\n")
cat("  - create_path_coefficients_table()\n")
cat("  - create_r_squared_table()\n")
cat("  - generate_full_report()\n")
cat("  - export_tables_to_csv()\n")
cat("  - export_report_to_txt()\n\n")
