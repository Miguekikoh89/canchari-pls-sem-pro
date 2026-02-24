# ============================================================================
# Módulo de Visualización de Modelos Estructurales PLS-SEM
# ============================================================================
# Funciones para crear gráficos del modelo estructural exportables
# Formatos: PNG, PDF, SVG

library(seminr)
library(ggplot2)
library(igraph)
library(gridExtra)

# ============================================================================
# 1. FUNCIONES BÁSICAS DE VISUALIZACIÓN
# ============================================================================

# Función para extraer información de la red del modelo
extract_network_info <- function(model_summary, boot_summary) {
  #' Extrae información de nodos y aristas del modelo
  #'
  #' @param model_summary Resumen del modelo PLS-SEM
  #' @param boot_summary Resumen del modelo bootstrapped
  #' @return Lista con nodos y aristas
  
  # Extraer constructos (nodos)
  reliability <- model_summary$reliability
  constructs <- rownames(reliability)
  
  # Extraer R² para cada constructo
  r2 <- model_summary$paths[, "R²"]
  
  # Extraer paths (aristas)
  paths <- boot_summary$bootstrapped_paths
  
  edges <- data.frame()
  for (i in 1:nrow(paths)) {
    for (j in 1:ncol(paths)) {
      if (paths[i, j, 1] != 0) {
        edges <- rbind(edges, data.frame(
          from = rownames(paths)[i],
          to = colnames(paths)[j],
          beta = paths[i, j, 1],
          p_value = paths[i, j, 3],
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(list(
    constructs = constructs,
    r2 = r2,
    edges = edges
  ))
}

# ============================================================================
# 2. VISUALIZACIÓN CON SEMINR (MÉTODO NATIVO)
# ============================================================================

# Función para guardar gráfico de seminr en múltiples formatos
save_seminr_plot <- function(pls_model, 
                             filename_base = "pls_model",
                             formats = c("png", "pdf")) {
  #' Guarda el gráfico del modelo seminr en múltiples formatos
  #'
  #' @param pls_model Modelo PLS-SEM estimado
  #' @param filename_base Nombre base del archivo (sin extensión)
  #' @param formats Vector de formatos (png, pdf, svg)
  #' @return Vector con nombres de archivos creados
  
  files_created <- c()
  
  for (format in formats) {
    filename <- paste0(filename_base, ".", format)
    
    if (format == "png") {
      png(filename, width = 1200, height = 800, res = 300)
      plot(pls_model)
      dev.off()
    } else if (format == "pdf") {
      pdf(filename, width = 12, height = 8)
      plot(pls_model)
      dev.off()
    } else if (format == "svg") {
      svg(filename, width = 12, height = 8)
      plot(pls_model)
      dev.off()
    }
    
    files_created <- c(files_created, filename)
    cat(paste0("✓ Gráfico guardado: ", filename, "\n"))
  }
  
  return(files_created)
}

# ============================================================================
# 3. VISUALIZACIÓN PERSONALIZADA CON GGPLOT2
# ============================================================================

# Función para crear gráfico personalizado del modelo
create_custom_model_plot <- function(model_summary, boot_summary,
                                     show_r2 = TRUE,
                                     show_pvalues = TRUE,
                                     highlight_significant = TRUE) {
  #' Crea un gráfico personalizado del modelo estructural
  #'
  #' @param model_summary Resumen del modelo PLS-SEM
  #' @param boot_summary Resumen del modelo bootstrapped
  #' @param show_r2 Mostrar R² en constructos
  #' @param show_pvalues Mostrar p-values en paths
  #' @param highlight_significant Resaltar paths significativos
  #' @return Objeto ggplot
  
  # Extraer información
  network_info <- extract_network_info(model_summary, boot_summary)
  
  constructs <- network_info$constructs
  r2 <- network_info$r2
  edges <- network_info$edges
  
  # Crear gráfico base
  p <- ggplot() +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  # Agregar título
  p <- p + ggtitle("PLS-SEM Structural Model")
  
  # Agregar información de constructos
  construct_text <- paste0(
    "Constructs: ", paste(constructs, collapse = ", "), "\n",
    "Paths: ", nrow(edges), "\n",
    "Significant paths (p < 0.05): ", 
    sum(edges$p_value < 0.05, na.rm = TRUE)
  )
  
  p <- p + annotate("text", x = 0.5, y = 0.5, label = construct_text,
                    size = 4, hjust = 0.5, vjust = 0.5)
  
  # Agregar información de paths
  if (nrow(edges) > 0) {
    path_text <- paste0(
      "Path Coefficients:\n",
      paste(apply(edges, 1, function(row) {
        sig <- ifelse(as.numeric(row["p_value"]) < 0.05, "*", "")
        paste0(row["from"], " → ", row["to"], ": β = ", 
               round(as.numeric(row["beta"]), 3), sig)
      }), collapse = "\n")
    )
    
    p <- p + annotate("text", x = 0.5, y = 0.2, label = path_text,
                      size = 3, hjust = 0.5, vjust = 1, family = "monospace")
  }
  
  return(p)
}

# ============================================================================
# 4. VISUALIZACIÓN CON IGRAPH
# ============================================================================

# Función para crear gráfico con igraph (red)
create_network_plot <- function(model_summary, boot_summary,
                                layout = "circle",
                                node_size = 20,
                                edge_width_scale = 2) {
  #' Crea un gráfico de red del modelo con igraph
  #'
  #' @param model_summary Resumen del modelo PLS-SEM
  #' @param boot_summary Resumen del modelo bootstrapped
  #' @param layout Tipo de layout (circle, spring, etc.)
  #' @param node_size Tamaño de nodos
  #' @param edge_width_scale Factor de escala para ancho de aristas
  #' @return Gráfico de red
  
  # Extraer información
  network_info <- extract_network_info(model_summary, boot_summary)
  
  constructs <- network_info$constructs
  r2 <- network_info$r2
  edges <- network_info$edges
  
  # Crear grafo
  g <- graph_from_data_frame(
    d = edges[, c("from", "to")],
    directed = TRUE,
    vertices = data.frame(name = constructs)
  )
  
  # Agregar atributos de aristas
  E(g)$beta <- edges$beta
  E(g)$p_value <- edges$p_value
  E(g)$significant <- edges$p_value < 0.05
  
  # Agregar atributos de nodos
  V(g)$r2 <- r2[match(V(g)$name, names(r2))]
  
  # Seleccionar layout
  if (layout == "circle") {
    layout_matrix <- layout_in_circle(g)
  } else if (layout == "spring") {
    layout_matrix <- layout_with_fr(g)
  } else if (layout == "tree") {
    layout_matrix <- layout_as_tree(g)
  } else {
    layout_matrix <- layout_nicely(g)
  }
  
  # Crear gráfico
  plot(g,
       layout = layout_matrix,
       vertex.size = node_size,
       vertex.label = V(g)$name,
       vertex.label.cex = 0.8,
       vertex.color = "lightblue",
       vertex.frame.color = "darkblue",
       edge.width = abs(E(g)$beta) * edge_width_scale,
       edge.color = ifelse(E(g)$significant, "red", "gray"),
       edge.arrow.size = 0.5,
       edge.curved = 0.2,
       main = "PLS-SEM Structural Model Network")
  
  # Agregar leyenda
  legend("topleft",
         legend = c("Significant (p < 0.05)", "Non-significant"),
         col = c("red", "gray"),
         lty = 1,
         lwd = 2)
  
  return(invisible(g))
}

# ============================================================================
# 5. TABLA DE RESUMEN VISUAL
# ============================================================================

# Función para crear tabla visual de resultados
create_summary_table_plot <- function(model_summary, boot_summary) {
  #' Crea una tabla visual con resumen de resultados
  #'
  #' @param model_summary Resumen del modelo PLS-SEM
  #' @param boot_summary Resumen del modelo bootstrapped
  #' @return Objeto ggplot con tabla
  
  # Extraer información
  network_info <- extract_network_info(model_summary, boot_summary)
  edges <- network_info$edges
  
  # Crear tabla de paths
  paths_summary <- data.frame(
    Path = paste(edges$from, "→", edges$to),
    Beta = round(edges$beta, 3),
    P_Value = round(edges$p_value, 4),
    Significant = ifelse(edges$p_value < 0.05, "***", "")
  )
  
  # Crear gráfico de tabla
  p <- ggplot() +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  
  # Agregar tabla como anotación
  table_text <- paste(
    "Path Coefficients Summary\n",
    "========================\n",
    paste(apply(paths_summary, 1, function(row) {
      paste(row[1], "β =", row[2], "p =", row[3], row[4])
    }), collapse = "\n")
  )
  
  p <- p + annotate("text", x = 0.5, y = 0.5, label = table_text,
                    size = 3, hjust = 0.5, vjust = 0.5, family = "monospace") +
    ggtitle("PLS-SEM Results Summary")
  
  return(p)
}

# ============================================================================
# 6. EXPORTACIÓN DE GRÁFICOS
# ============================================================================

# Función para exportar gráfico personalizado
export_custom_plot <- function(plot_object, 
                               filename_base = "pls_model_custom",
                               formats = c("png", "pdf"),
                               width = 12,
                               height = 8,
                               dpi = 300) {
  #' Exporta un gráfico ggplot en múltiples formatos
  #'
  #' @param plot_object Objeto ggplot
  #' @param filename_base Nombre base del archivo
  #' @param formats Vector de formatos
  #' @param width Ancho en pulgadas
  #' @param height Alto en pulgadas
  #' @param dpi Resolución en DPI
  #' @return Vector con nombres de archivos creados
  
  files_created <- c()
  
  for (format in formats) {
    filename <- paste0(filename_base, ".", format)
    
    if (format == "png") {
      ggsave(filename, plot_object, width = width, height = height, 
             dpi = dpi, device = "png")
    } else if (format == "pdf") {
      ggsave(filename, plot_object, width = width, height = height, 
             device = "pdf")
    } else if (format == "svg") {
      ggsave(filename, plot_object, width = width, height = height, 
             device = "svg")
    }
    
    files_created <- c(files_created, filename)
    cat(paste0("✓ Gráfico guardado: ", filename, "\n"))
  }
  
  return(files_created)
}

# ============================================================================
# 7. FUNCIÓN COMPLETA DE VISUALIZACIÓN
# ============================================================================

# Función maestra para generar todos los gráficos
generate_all_visualizations <- function(pls_model, model_summary, boot_summary,
                                        output_dir = ".",
                                        include_seminr = TRUE,
                                        include_custom = TRUE,
                                        include_network = TRUE,
                                        formats = c("png", "pdf")) {
  #' Genera todos los gráficos del modelo
  #'
  #' @param pls_model Modelo PLS-SEM estimado
  #' @param model_summary Resumen del modelo
  #' @param boot_summary Resumen del modelo bootstrapped
  #' @param output_dir Directorio de salida
  #' @param include_seminr Incluir gráfico nativo de seminr
  #' @param include_custom Incluir gráfico personalizado
  #' @param include_network Incluir gráfico de red
  #' @param formats Formatos de exportación
  #' @return Vector con nombres de archivos creados
  
  files_created <- c()
  
  cat("\nGenerando visualizaciones del modelo...\n")
  cat("=" %+% rep("=", 50) %+% "=\n\n")
  
  # 1. Gráfico nativo de seminr
  if (include_seminr) {
    cat("1. Gráfico nativo de seminr...\n")
    files <- save_seminr_plot(pls_model,
                              filename_base = file.path(output_dir, "01_seminr_model"),
                              formats = formats)
    files_created <- c(files_created, files)
    cat("\n")
  }
  
  # 2. Gráfico personalizado
  if (include_custom) {
    cat("2. Gráfico personalizado...\n")
    custom_plot <- create_custom_model_plot(model_summary, boot_summary)
    files <- export_custom_plot(custom_plot,
                                filename_base = file.path(output_dir, "02_custom_model"),
                                formats = formats)
    files_created <- c(files_created, files)
    cat("\n")
  }
  
  # 3. Gráfico de red
  if (include_network) {
    cat("3. Gráfico de red (igraph)...\n")
    
    for (format in formats) {
      filename <- file.path(output_dir, paste0("03_network_model.", format))
      
      if (format == "png") {
        png(filename, width = 1200, height = 800, res = 300)
      } else if (format == "pdf") {
        pdf(filename, width = 12, height = 8)
      }
      
      create_network_plot(model_summary, boot_summary)
      dev.off()
      
      files_created <- c(files_created, filename)
      cat(paste0("✓ Gráfico guardado: ", filename, "\n"))
    }
    cat("\n")
  }
  
  cat("=" %+% rep("=", 50) %+% "=\n")
  cat("✓ Visualizaciones completadas\n")
  cat("Archivos creados:", length(files_created), "\n\n")
  
  return(files_created)
}

# ============================================================================
# EJEMPLO DE USO
# ============================================================================

cat("\n")
cat("✓ Módulo de visualización cargado\n")
cat("Funciones disponibles:\n")
cat("  - save_seminr_plot()\n")
cat("  - create_custom_model_plot()\n")
cat("  - create_network_plot()\n")
cat("  - create_summary_table_plot()\n")
cat("  - export_custom_plot()\n")
cat("  - generate_all_visualizations()\n\n")
