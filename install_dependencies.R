# ============================================================================
# Script de Instalación de Dependencias
# PLS-SEM Academic Analyzer
# ============================================================================
# Este script instala todos los paquetes necesarios para ejecutar la aplicación
# Ejecutar una sola vez antes de usar la aplicación

# Verificar versión de R
if (R.version$major < 4) {
  warning("Se recomienda R 4.0 o superior. Tu versión actual es: ", R.version$version.string)
}

# Lista de paquetes necesarios
required_packages <- c(
  "shiny",              # Framework Shiny
  "shinydashboard",     # Dashboard UI
  "shinyFiles",         # Interfaz de archivos
  "readxl",             # Lectura de Excel
  "seminr",             # Motor PLS-SEM
  "tidyverse",          # Manipulación de datos
  "DT",                 # Tablas interactivas
  "plotly",             # Gráficos interactivos
  "gridExtra",          # Composición de gráficos
  "ggplot2"             # Visualización
)

# Función para instalar paquetes faltantes
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    cat("Instalando", package, "...\n")
    install.packages(package, dependencies = TRUE)
    
    # Verificar instalación
    if (require(package, character.only = TRUE)) {
      cat("✓", package, "instalado correctamente\n")
    } else {
      cat("✗ Error al instalar", package, "\n")
      return(FALSE)
    }
  } else {
    cat("✓", package, "ya está instalado\n")
  }
  return(TRUE)
}

# Instalar todos los paquetes
cat("========================================\n")
cat("Instalando dependencias...\n")
cat("========================================\n\n")

all_installed <- TRUE
for (pkg in required_packages) {
  if (!install_if_missing(pkg)) {
    all_installed <- FALSE
  }
}

# Verificar instalación de seminr específicamente
cat("\n========================================\n")
cat("Verificando instalación de seminr...\n")
cat("========================================\n\n")

if (require(seminr, character.only = TRUE)) {
  seminr_version <- packageVersion("seminr")
  cat("✓ seminr versión", as.character(seminr_version), "instalado\n")
  
  # Cargar datos de ejemplo
  data("corp_rep_data")
  cat("✓ Datos de ejemplo disponibles\n")
  
  # Mostrar información
  cat("\nInformación de seminr:\n")
  cat("- Paquete: seminr\n")
  cat("- Versión:", as.character(seminr_version), "\n")
  cat("- Descripción: Building and Estimating Structural Equation Models\n")
  cat("- Documentación: https://sem-in-r.github.io/seminr/\n")
} else {
  cat("✗ Error: seminr no pudo ser instalado\n")
  all_installed <- FALSE
}

# Resumen final
cat("\n========================================\n")
if (all_installed) {
  cat("✓ Todas las dependencias instaladas correctamente\n")
  cat("========================================\n\n")
  cat("Puedes ejecutar la aplicación con:\n")
  cat("  shiny::runApp('app.R')\n")
} else {
  cat("✗ Hubo errores durante la instalación\n")
  cat("========================================\n\n")
  cat("Intenta instalar manualmente:\n")
  cat("  install.packages('seminr')\n")
  cat("  install.packages('shiny')\n")
}
