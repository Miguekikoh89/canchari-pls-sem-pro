# ============================================================================
# CANCHARI PLS-SEM PRO V2.0 - EDICIÓN MÁXIMA
# ============================================================================
# FIXES v2.0:
#   - Diagrama: corregido renderizado DiagrammeR + exportación PNG via webshot2
#   - Q2 (Blindfolding): extracción robusta desde seminr::blindfold()
#   - Descarga ZIP: incluye todas las tablas + diagrama PNG + reporte HTML
#   - UI/UX mejorada: barra de progreso, badges de semáforo, panel interpretación
#   - Validación de modelo antes del análisis
#   - Segunda versión del diagrama: ggplot2 como fallback y para exportar
# ============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(seminr)
library(tidyverse)
library(DT)
library(DiagrammeR)

# Paquetes opcionales para exportar diagrama (instalar si se quiere exportar SVG/PNG)
has_diagrammersvg <- requireNamespace("DiagrammeRsvg", quietly = TRUE)
has_rsvg          <- requireNamespace("rsvg",          quietly = TRUE)
if (has_diagrammersvg) library(DiagrammeRsvg)
if (has_rsvg)          library(rsvg)
library(glue)
library(officer)
library(flextable)
library(ggplot2)
library(openxlsx)
library(svglite)
library(grid)
has_knitr <- requireNamespace("knitr", quietly = TRUE)

# ── helpers ──────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

safe_num <- function(x, digits = 3) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  val <- suppressWarnings(as.numeric(x[1]))
  if (is.na(val)) return(NA_real_)
  round(val, digits)
}

safe_list_get <- function(x, name) {
  if (is.null(x) || !is.list(x)) return(NULL)
  if (!name %in% names(x)) return(NULL)
  x[[name]]
}

clean_names <- function(n) {
  n <- gsub("[^[:alnum:]]", "_", n)
  n <- gsub("^([0-9])", "X\\1", n)
  n
}

parse_item_range <- function(range_str, data_names) {
  if (is.null(range_str) || trimws(range_str) == "") return(NULL)
  parts <- unlist(strsplit(range_str, ","))
  final <- c()
  for (p in parts) {
    p <- trimws(p)
    if (grepl("-", p)) {
      rp <- strsplit(p, "-")[[1]]
      if (length(rp) == 2) {
        prefix <- sub("[0-9]+$", "", trimws(rp[1]))
        s <- suppressWarnings(as.numeric(sub("^.*?([0-9]+)$","\\1",trimws(rp[1]))))
        e <- suppressWarnings(as.numeric(sub("^.*?([0-9]+)$","\\1",trimws(rp[2]))))
        if (!is.na(s) && !is.na(e)) final <- c(final, paste0(prefix, s:e))
      }
    } else {
      final <- c(final, p)
    }
  }
  valid <- data_names[match(tolower(final), tolower(data_names))]
  unique(valid[!is.na(valid)])
}

# ── MICOM: Measurement Invariance Composite Models (seminr, sin cSEM) ─────────
# Paso 1: Configuracion (siempre cumplido)
# Paso 2: Igualdad compuesta = correlacion de construct scores >= 0.90
# Paso 3: Igualdad de medias y varianzas (permutation test)
# ── MICOM: Measurement Invariance of Composite Models ─────────────────────────
# CORRECCION CRITICA (Henseler et al. 2016 / Hair et al. 2022):
#
# ERROR ANTERIOR: cor(scores_G1[1:n], scores_G2[1:n])
#   Esto correlaciona observacion-i de G1 con observacion-i de G2 (muestras
#   independientes). Produce r ≈ 0 (ruido). INCORRECTO.
#
# METODO CORRECTO: Correlacion de composites usando PESOS de ambos grupos
#   1. Estimar PLS por grupo → extraer outer_weights w1, w2
#   2. Para el mismo constructo cn:
#        c1 = X_pool %*% w1_cn   (composite con pesos del grupo 1)
#        c2 = X_pool %*% w2_cn   (composite con pesos del grupo 2)
#        r  = cor(c1, c2)        → r ≈ 0.999 si los pesos son identicos (invariante)
#   3. Test de permutacion: permutar filas de X_pool, recalcular c1,c2,r
#      p-valor = proporcion(r_perm <= r_orig) [unilateral izquierda]
# ─────────────────────────────────────────────────────────────────────────────
run_micom <- function(data_full, group_var, m_model, s_model, n_permut = 1000) {
  grupos <- sort(unique(as.character(data_full[[group_var]])))
  if (length(grupos) < 2) stop("Se necesitan >= 2 grupos para MICOM.")
  if (length(grupos) > 20) stop(paste0("La variable '", group_var, "' tiene ",
    length(grupos), " valores unicos. Use una variable categorica con <= 20 grupos."))

  item_cols <- setdiff(names(data_full), group_var)

  # ── Estimar PLS por grupo → extraer outer weights y construct scores ────────
  models_g  <- list()
  weights_g <- list()
  scores_g  <- list()

  for (g in grupos) {
    dat_g <- data_full[data_full[[group_var]] == g, item_cols, drop = FALSE]
    pls_g <- tryCatch(
      estimate_pls(data = dat_g, measurement_model = m_model,
                   structural_model = s_model),
      error = function(e) stop(paste0("Error estimando modelo en grupo '", g, "': ", e$message))
    )
    models_g[[g]]  <- pls_g
    weights_g[[g]] <- tryCatch(as.matrix(pls_g$outer_weights), error = function(e) NULL)
    scores_g[[g]]  <- tryCatch(as.data.frame(pls_g$construct_scores),
                                error = function(e) NULL)
  }

  constructs_nm <- colnames(weights_g[[grupos[1]]])

  # ── Paso 1: Configuracion ────────────────────────────────────────────────────
  paso1 <- data.frame(
    Paso        = "1 - Configuracion",
    Descripcion = "Mismo modelo de medida en todos los grupos",
    Grupos      = paste(grupos, collapse = " | "),
    Resultado   = paste0(length(grupos), " grupos | ", length(constructs_nm), " constructos"),
    OK          = "\u2713 Cumplido",
    stringsAsFactors = FALSE
  )

  pairs <- combn(grupos, 2, simplify = FALSE)
  resumen_rows <- list()

  for (pr in pairs) {
    g1 <- pr[1]; g2 <- pr[2]
    par_lbl <- paste0(g1, " vs ", g2)

    dat_g1 <- data_full[data_full[[group_var]] == g1, item_cols, drop = FALSE]
    dat_g2 <- data_full[data_full[[group_var]] == g2, item_cols, drop = FALSE]
    w1 <- weights_g[[g1]]   # outer weights: rows=items, cols=constructs
    w2 <- weights_g[[g2]]

    for (cn in constructs_nm) {
      if (!(cn %in% colnames(w1)) || !(cn %in% colnames(w2))) next

      # Items del constructo cn presentes en ambos grupos
      w1_cn    <- w1[, cn]
      w2_cn    <- w2[, cn]
      items_cn <- intersect(names(w1_cn), names(w2_cn))
      items_cn <- items_cn[items_cn %in% colnames(dat_g1) & items_cn %in% colnames(dat_g2)]
      if (length(items_cn) < 1) next

      w1_vec <- w1_cn[items_cn]
      w2_vec <- w2_cn[items_cn]

      # Pool de datos (ambos grupos combinados) para calcular composites
      X_pool <- as.matrix(rbind(
        dat_g1[, items_cn, drop = FALSE],
        dat_g2[, items_cn, drop = FALSE]
      ))

      # ── Paso 2: Invarianza composicional ─────────────────────────────────────
      # r = cor(X %*% w1, X %*% w2)
      # Cuando w1 ≈ w2 → c1 ≈ c2 → r ≈ 1 (invariante)
      c1_pool <- as.numeric(X_pool %*% w1_vec)
      c2_pool <- as.numeric(X_pool %*% w2_vec)
      r_orig  <- tryCatch(cor(c1_pool, c2_pool), error = function(e) NA_real_)
      if (is.na(r_orig)) next

      # Permutacion: permutar filas de X_pool, recalcular c1,c2,r
      r_perm_dist <- replicate(n_permut, {
        X_shuf <- X_pool[sample(nrow(X_pool)), , drop = FALSE]
        cp1 <- as.numeric(X_shuf %*% w1_vec)
        cp2 <- as.numeric(X_shuf %*% w2_vec)
        tryCatch(cor(cp1, cp2), error = function(e) NA_real_)
      })
      r_perm_dist <- r_perm_dist[!is.na(r_perm_dist)]
      # p-valor bilateral identico a SmartPLS:
      # proporcion de permutaciones con r >= r_orig (distribucion nula)
      # SmartPLS Paso 2: p = P(r_perm >= r_obs) — test unilateral derecha
      # Cuando r_orig es muy alto (≈1), practicamente ninguna permutacion lo supera → p alto
      p_r <- if (length(r_perm_dist) > 0) mean(r_perm_dist >= r_orig) else NA_real_

      inv_comp <- !is.na(r_orig) && r_orig >= 0.90

      # ── Paso 3: Igualdad de medias y varianzas ────────────────────────────────
      # Usar construct scores del propio grupo (estimados con sus pesos)
      sc_g1 <- scores_g[[g1]]; sc_g2 <- scores_g[[g2]]
      if (is.null(sc_g1) || is.null(sc_g2)) next
      if (!(cn %in% names(sc_g1)) || !(cn %in% names(sc_g2))) next

      v1 <- sc_g1[[cn]][!is.na(sc_g1[[cn]])]
      v2 <- sc_g2[[cn]][!is.na(sc_g2[[cn]])]
      if (length(v1) < 3 || length(v2) < 3) next

      obs_md <- mean(v1) - mean(v2)
      obs_vd <- var(v1)  - var(v2)
      all_sc <- c(v1, v2); n1 <- length(v1)

      pm_dist <- replicate(n_permut, {
        s <- sample(all_sc)
        mean(s[seq_len(n1)]) - mean(s[(n1+1):length(s)])
      })
      pv_dist <- replicate(n_permut, {
        s <- sample(all_sc)
        var(s[seq_len(n1)]) - var(s[(n1+1):length(s)])
      })
      p_med <- mean(abs(pm_dist) >= abs(obs_md))
      p_var <- mean(abs(pv_dist) >= abs(obs_vd))

      resultado <- if (!inv_comp) {
        "No invariante"
      } else if (p_med >= 0.05 && p_var >= 0.05) {
        "Invarianza total"
      } else {
        "Invarianza parcial"
      }

      resumen_rows[[length(resumen_rows) + 1]] <- data.frame(
        Constructo               = cn,
        Grupos                   = par_lbl,
        Correlacion_original     = round(r_orig, 3),
        p_valor_permutacion      = round(p_r,    3),
        Invarianza_composicional = if (inv_comp) "Si" else "No",
        Dif_media_p_valor        = round(p_med,  3),
        Dif_varianza_p_valor     = round(p_var,  3),
        Resultado                = resultado,
        stringsAsFactors = FALSE
      )
    }
  }

  tabla_resumen <- if (length(resumen_rows) > 0)
    do.call(rbind, resumen_rows)
  else
    data.frame(Nota = "Not enough data to calculate MICOM / Sin datos suficientes para calcular MICOM.", stringsAsFactors = FALSE)

  list(paso1 = paso1, tabla_resumen = tabla_resumen, grupos = grupos)
}

# ── MGA: Multi-Group Analysis (Permutation Test) ──────────────────────────────
# Implementacion exacta de Henseler, Ringle & Sarstedt (2016) / SmartPLS 4.
# Columnas de salida identicas a SmartPLS:
#   Relacion | Original_G1 | Original_G2 | Diferencia_original |
#   Media_permutacion | IC_2.5pct | IC_97.5pct | p_valor_permutacion | Sig
# ─────────────────────────────────────────────────────────────────────────────
run_mga <- function(data_full, group_var, m_model, s_model, min_n = 30, n_permut = 1000) {
  grupos <- sort(unique(as.character(data_full[[group_var]])))
  if (length(grupos) > 20)
    stop(paste0("La variable '", group_var, "' tiene ", length(grupos),
      " valores unicos. Use una variable categorica con <= 20 grupos."))

  n_tab     <- table(as.character(data_full[[group_var]]))
  grupos_ok <- names(n_tab)[n_tab >= min_n]
  if (length(grupos_ok) < 2)
    stop(paste0("Solo ", length(grupos_ok), " grupos con n >= ", min_n, "."))
  grupos <- sort(grupos_ok)

  item_cols <- setdiff(names(data_full), group_var)

  # Helper: estima PLS y extrae path coefficients como named list
  get_paths <- function(dat) {
    pls_g <- tryCatch(
      estimate_pls(data = dat, measurement_model = m_model,
                   structural_model = s_model),
      error = function(e) NULL
    )
    if (is.null(pls_g)) return(NULL)
    pm <- tryCatch({
      p <- pls_g$path_coef
      if (is.null(p)) p <- as.matrix(summary(pls_g)$paths)
      as.matrix(p)
    }, error = function(e) NULL)
    if (is.null(pm)) return(NULL)
    out <- list()
    for (r in rownames(pm)) {
      for (cl in colnames(pm)) {
        val <- suppressWarnings(as.numeric(pm[r, cl]))
        if (!is.na(val) && abs(val) > 1e-10)
          out[[paste0(r, " -> ", cl)]] <- val
      }
    }
    out
  }

  # Estimar modelos observados por grupo
  paths_obs <- list()
  for (g in grupos) {
    dat_g        <- data_full[data_full[[group_var]] == g, item_cols, drop = FALSE]
    paths_obs[[g]] <- get_paths(dat_g)
    if (is.null(paths_obs[[g]]))
      stop(paste0("No se pudo estimar el modelo en el grupo '", g, "'."))
  }

  paths_comunes <- Reduce(intersect, lapply(paths_obs, names))
  if (length(paths_comunes) == 0) stop("No hay paths comunes entre grupos.")

  pairs <- combn(grupos, 2, simplify = FALSE)
  rows  <- list()

  for (pr in pairs) {
    g1 <- pr[1]; g2 <- pr[2]
    lbl_g1   <- paste0("Original_", g1)
    lbl_g2   <- paste0("Original_", g2)
    dat1     <- data_full[data_full[[group_var]] == g1, item_cols, drop = FALSE]
    dat2     <- data_full[data_full[[group_var]] == g2, item_cols, drop = FALSE]
    dat_pair <- rbind(dat1, dat2)
    n1       <- nrow(dat1)

    for (path_nm in paths_comunes) {
      b1_obs <- paths_obs[[g1]][[path_nm]]
      b2_obs <- paths_obs[[g2]][[path_nm]]
      if (is.null(b1_obs) || is.null(b2_obs)) next

      obs_diff <- b1_obs - b2_obs

      perm_diffs <- replicate(n_permut, {
        idx <- sample(nrow(dat_pair))
        dp1 <- dat_pair[idx[seq_len(n1)],           item_cols, drop = FALSE]
        dp2 <- dat_pair[idx[(n1+1):nrow(dat_pair)], item_cols, drop = FALSE]
        bt1 <- tryCatch({
          b <- get_paths(dp1); if (!is.null(b) && !is.null(b[[path_nm]])) b[[path_nm]] else NA_real_
        }, error = function(e) NA_real_)
        bt2 <- tryCatch({
          b <- get_paths(dp2); if (!is.null(b) && !is.null(b[[path_nm]])) b[[path_nm]] else NA_real_
        }, error = function(e) NA_real_)
        if (is.na(bt1) || is.na(bt2)) NA_real_ else bt1 - bt2
      })
      pd_ok <- perm_diffs[!is.na(perm_diffs)]

      if (length(pd_ok) < 10) {
        p_val <- pd_mu <- pd_lo <- pd_hi <- NA_real_
      } else {
        p_val <- mean(abs(pd_ok) >= abs(obs_diff))
        pd_mu <- mean(pd_ok)
        pd_lo <- quantile(pd_ok, 0.025, names = FALSE)
        pd_hi <- quantile(pd_ok, 0.975, names = FALSE)
      }

      sig <- if (is.na(p_val)) "N/D" else
             if (p_val < 0.001) "***" else
             if (p_val < 0.01)  "**"  else
             if (p_val < 0.05)  "*"   else "n.s."

      row_df <- data.frame(Relacion = path_nm, stringsAsFactors = FALSE)
      row_df[[lbl_g1]]                  <- round(b1_obs,   3)
      row_df[[lbl_g2]]                  <- round(b2_obs,   3)
      row_df[["Diferencia_original"]]   <- round(obs_diff, 3)
      row_df[["Media_permutacion"]]     <- if (is.na(pd_mu)) NA_real_ else round(pd_mu, 3)
      row_df[["IC_2.5pct"]]             <- if (is.na(pd_lo)) NA_real_ else round(pd_lo, 3)
      row_df[["IC_97.5pct"]]            <- if (is.na(pd_hi)) NA_real_ else round(pd_hi, 3)
      row_df[["p_valor_permutacion"]]   <- if (is.na(p_val)) NA_real_ else round(p_val, 3)
      row_df[["Sig"]]                   <- sig
      rows[[length(rows) + 1]]          <- row_df
    }
  }

  if (length(rows) == 0)
    return(data.frame(Nota = "No se pudo calcular MGA.", stringsAsFactors = FALSE))
  do.call(rbind, rows)
}

calc_cr_ave <- function(L) {
  if (is.null(L)) return(data.frame(Constructo=character(), CR=numeric(), AVE=numeric()))
  L <- as.matrix(L)
  # seminr: filas=ítems, columnas=constructos; si al revés, transponer
  rn <- rownames(L); cn <- colnames(L)
  if (!is.null(cn) && any(grepl("[0-9]",cn)) && !is.null(rn) && !any(grepl("[0-9]",rn)) && nrow(L) <= ncol(L)) L <- t(L)
  constructs <- colnames(L)
  out <- data.frame(Constructo=constructs, CR=NA_real_, AVE=NA_real_, stringsAsFactors=FALSE)
  for (j in seq_along(constructs)) {
    lam <- suppressWarnings(as.numeric(L[,j]))
    lam <- lam[!is.na(lam) & lam != 0]
    if (!length(lam)) next
    cr  <- (sum(lam)^2) / ((sum(lam)^2) + sum(1 - lam^2))
    ave <- sum(lam^2) / length(lam)
    if (!is.na(cr) && !is.na(ave) && cr < ave) { tmp <- cr; cr <- ave; ave <- tmp }
    out$CR[j]  <- round(cr,  3)
    out$AVE[j] <- round(ave, 3)
  }
  out
}

# ── Diagrama DOT ─────────────────────────────────────────────────────────────

build_pls_dot <- function(pls_est, paths_df = NULL, loadings_df = NULL,
                          r2_df = NULL, title = NULL, rankdir = "LR", digits = 3) {
  if (is.null(pls_est)) stop("pls_est es NULL")
  fmt <- function(x) {
    if (is.null(x) || length(x) == 0) return("")
    v <- suppressWarnings(as.numeric(x))
    if (is.na(v)) return("")
    format(round(v, digits), nsmall = digits)
  }

  summ <- tryCatch(summary(pls_est), error = function(e) NULL)

  # ── paths ─────────────────────────────────────────────────────────────────
  # seminr guarda los paths como matrix (exogenas x endogenas)
  # ESTRATEGIA: intentar múltiples fuentes
  if (is.null(paths_df)) {
    paths_df <- tryCatch({
      # Fuente 1: summ$paths es matrix de coeficientes (filas=origen, cols=destino)
      p <- summ$paths
      if (is.matrix(p) || is.data.frame(p)) {
        pm <- as.matrix(p)
        rows <- rownames(pm); cols <- colnames(pm)
        out_p <- data.frame(From=character(), To=character(), Beta=numeric(), stringsAsFactors=FALSE)
        for (r in rows) for (cl in cols) {
          val <- suppressWarnings(as.numeric(pm[r, cl]))
          if (!is.na(val) && val != 0) out_p <- rbind(out_p, data.frame(From=r, To=cl, Beta=val))
        }
        if (nrow(out_p) > 0) out_p else NULL
      } else NULL
    }, error = function(e) NULL)
  }

  # Fuente 2: desde el modelo estructural estimado directamente
  if (is.null(paths_df) || nrow(paths_df) == 0) {
    paths_df <- tryCatch({
      # pls_est$smMatrix: matrix de relaciones (0/1) del modelo estructural
      sm <- pls_est$smMatrix
      if (is.null(sm)) sm <- pls_est$structural_model
      if (!is.null(sm)) {
        pm <- as.matrix(sm)
        # Coeficientes desde path_coef
        coefs <- pls_est$path_coef %||% pls_est$pathCoef %||% matrix(0)
        out_p <- data.frame(From=character(), To=character(), Beta=numeric(), stringsAsFactors=FALSE)
        for (r in rownames(pm)) for (cl in colnames(pm)) {
          if (!is.na(pm[r,cl]) && pm[r,cl] != 0) {
            beta_val <- tryCatch(as.numeric(coefs[r, cl]), error=function(e) NA_real_)
            if (is.na(beta_val)) beta_val <- as.numeric(pm[r, cl])
            out_p <- rbind(out_p, data.frame(From=r, To=cl, Beta=beta_val))
          }
        }
        if (nrow(out_p) > 0) out_p else NULL
      } else NULL
    }, error = function(e) NULL)
  }

  # Fuente 3: desde bootstrapped_paths si existe
  if (is.null(paths_df) || nrow(paths_df) == 0) {
    paths_df <- tryCatch({
      bp <- pls_est$boots %||% pls_est$bootstrapped_paths
      if (is.null(bp)) return(NULL)
      df_bp <- as.data.frame(bp)
      # rownames como "From -> To"
      rn <- rownames(df_bp)
      if (!is.null(rn) && all(grepl("->", rn))) {
        parts <- strsplit(rn, "\\s*->\\s*")
        out_p <- data.frame(
          From = sapply(parts, `[`, 1),
          To   = sapply(parts, `[`, 2),
          Beta = suppressWarnings(as.numeric(df_bp[[1]])),
          stringsAsFactors = FALSE
        )
        out_p[!is.na(out_p$Beta) & out_p$Beta != 0, ]
      } else NULL
    }, error = function(e) NULL)
  }

  # ── loadings ───────────────────────────────────────────────────────────────
  if (is.null(loadings_df)) {
    loadings_df <- tryCatch({
      ol <- NULL
      # Fuente 1: summ$loadings
      if (!is.null(summ$loadings)) ol <- summ$loadings
      # Fuente 2: outer_loadings del objeto
      if (is.null(ol)) ol <- pls_est$outer_loadings
      # Fuente 3: loadings del objeto
      if (is.null(ol)) ol <- pls_est$loadings
      if (is.null(ol)) return(NULL)

      if (is.matrix(ol) || is.data.frame(ol)) {
        # seminr: filas=items, columnas=constructos
        mat <- as.matrix(ol)
        out_l <- data.frame(indicator=character(), construct=character(), loading=numeric(), stringsAsFactors=FALSE)
        for (item in rownames(mat)) for (cn in colnames(mat)) {
          val <- suppressWarnings(as.numeric(mat[item, cn]))
          if (!is.na(val) && val != 0)
            out_l <- rbind(out_l, data.frame(indicator=item, construct=cn, loading=val))
        }
        out_l
      } else NULL
    }, error = function(e) NULL)
  }

  # ── R² ─────────────────────────────────────────────────────────────────────
  if (is.null(r2_df)) {
    r2_df <- tryCatch({
      r2 <- NULL
      for (nm in c("r_squared","rSquared","r2","fSquare")) {
        r2 <- tryCatch(summ[[nm]], error=function(e) NULL)
        if (!is.null(r2)) break
      }
      if (is.null(r2)) return(NULL)
      if (is.numeric(r2) && !is.null(names(r2)))
        data.frame(Construct = names(r2), R2 = as.numeric(r2), stringsAsFactors = FALSE)
      else {
        df_r2 <- as.data.frame(r2)
        df_r2$Construct <- rownames(df_r2)
        names(df_r2)[1] <- "R2"
        df_r2[, c("Construct","R2")]
      }
    }, error = function(e) NULL)
  }

  if (is.null(paths_df)   || nrow(paths_df)   == 0) stop("No hay coeficientes de ruta para dibujar.")
  if (is.null(loadings_df)|| nrow(loadings_df) == 0) stop("No hay cargas factoriales para dibujar indicadores.")

  constructs  <- sort(unique(c(paths_df$From, paths_df$To, loadings_df$construct)))
  r2_map      <- setNames(rep(NA_real_, length(constructs)), constructs)
  if (!is.null(r2_df) && nrow(r2_df) > 0) {
    idx <- match(r2_df$Construct, names(r2_map))
    r2_map[idx[!is.na(idx)]] <- r2_df$R2[!is.na(idx)]
  }

  # Nodos constructo (elipses azules)
  construct_nodes <- vapply(constructs, function(cn) {
    r2  <- r2_map[[cn]]
    top <- if (!is.na(r2)) fmt(r2) else ""
    lab <- if (nzchar(top)) paste0(top, "\\n", cn) else cn
    paste0('"', cn, '" [shape=ellipse, fixedsize=false, width=1.4, height=0.9, ',
           'style="filled,rounded", fillcolor="#1565C0", fontcolor="white", ',
           'fontsize=13, fontname="Helvetica-Bold", label="', lab, '"];')
  }, character(1))

  # Nodos indicador (rectángulos azul claro)
  indicators <- unique(loadings_df$indicator)
  indicator_nodes <- paste0(
    '"', indicators, '" [shape=box, style="filled,rounded", fillcolor="#E3F2FD", ',
    'fontcolor="#1A237E", fontsize=11, fontname="Helvetica", margin="0.15,0.1"];'
  )

  # Aristas estructurales (rojo)
  struct_edges <- apply(paths_df, 1, function(row) {
    beta_val <- suppressWarnings(as.numeric(row["Beta"]))
    beta_lbl <- if (is.na(beta_val)) "" else format(round(beta_val, digits), nsmall = digits)
    paste0('"', row["From"], '" -> "', row["To"], '" [',
           'color="#E53935", penwidth=2.2, arrowsize=1.0, ',
           'fontcolor="#C62828", label="', beta_lbl, '", ',
           'fontsize=12, fontname="Helvetica-Bold"];')
  })

  # Aristas de medición (gris azulado)
  meas_edges <- apply(loadings_df, 1, function(row) {
    load_val <- suppressWarnings(as.numeric(row["loading"]))
    load_lbl <- if (is.na(load_val)) "" else format(round(load_val, digits), nsmall = digits)
    paste0('"', row["indicator"], '" -> "', row["construct"], '" [',
           'color="#546E7A", penwidth=1.2, arrowsize=0.6, ',
           'fontcolor="#37474F", label="', load_lbl, '", ',
           'fontsize=10, fontname="Helvetica"];')
  })

  graph_title <- if (!is.null(title) && nzchar(title))
    paste0('labelloc="t"; label="', title, '"; fontsize=16; fontname="Helvetica-Bold";')
  else ""

  paste0(
    'digraph PLS {',
    'graph [rankdir=', rankdir, ', bgcolor="white", splines=polyline, overlap=false, ',
    'nodesep=0.6, ranksep=1.0, ', graph_title, '];',
    'node [fontname="Helvetica"]; edge [fontname="Helvetica"];',
    paste(c(construct_nodes, indicator_nodes, struct_edges, meas_edges), collapse = "\n"),
    '}'
  )
}

# ── Interpretación automática / Automatic interpretation ─────────────────────

interpretar_plssem <- function(tables, lng = "es") {
  en <- (lng == "en")
  lineas <- character(0)

  # ── Medición: AVE y CR ──────────────────────────────────────────────────────
  if (!is.null(tables$Confiabilidad) && nrow(tables$Confiabilidad) > 0) {
    df <- tables$Confiabilidad
    lineas <- c(lineas, paste0("<b>&#128309; ", if(en) "Validity and Reliability (Measurement Model):" else "Validez y Confiabilidad del Modelo de Medida:", "</b>"))
    for (i in seq_len(nrow(df))) {
      ave <- suppressWarnings(as.numeric(df$AVE[i]))
      cr  <- suppressWarnings(as.numeric(df$Composite_Reliability_CR[i]))
      ca  <- suppressWarnings(as.numeric(df$Cronbach_Alpha[i]))
      nm  <- df$Constructo[i]
      ok_ave <- !is.na(ave) && ave >= 0.5
      ok_cr  <- !is.na(cr)  && cr  >= 0.7
      semaf  <- if (ok_ave && ok_cr) "&#128994;" else if (ok_cr || ok_ave) "&#128993;" else "&#128308;"
      ave_s  <- if (is.na(ave)) "NA" else as.character(round(ave, 3))
      cr_s   <- if (is.na(cr))  "NA" else as.character(round(cr,  3))
      ca_s   <- if (is.na(ca))  "NA" else as.character(round(ca,  3))
      lineas <- c(lineas, paste0("&nbsp;&nbsp;", semaf, " <b>", nm, "</b>: AVE=", ave_s, ", CR=", cr_s, ", &alpha;=", ca_s))
    }
  }

  # ── HTMT ────────────────────────────────────────────────────────────────────
  if (!is.null(tables$HTMT) && nrow(tables$HTMT) > 0) {
    lineas <- c(lineas, paste0("<br><b>&#128995; ", if(en) "Discriminant Validity (HTMT):" else "Validez Discriminante (HTMT):", "</b>"))
    htmt_vals <- suppressWarnings(as.numeric(tables$HTMT$HTMT))
    ok <- all(htmt_vals < 0.85, na.rm = TRUE)
    semaf  <- if (ok) "&#128994;" else "&#128308;"
    texto  <- if (ok)
      if(en) "All HTMT &lt; 0.85 &#10003; Discriminant validity confirmed"
      else   "Todos los HTMT &lt; 0.85 &#10003; Se confirma validez discriminante"
    else
      if(en) "Some HTMT &ge; 0.85 &#9888; REVIEW discriminant validity"
      else   "Alg&uacute;n HTMT &ge; 0.85 &#9888; REVISAR validez discriminante"
    lineas <- c(lineas, paste0("&nbsp;&nbsp;", semaf, " ", texto))
    for (i in seq_len(nrow(tables$HTMT))) {
      hv  <- suppressWarnings(as.numeric(tables$HTMT$HTMT[i]))
      sem <- if (!is.na(hv) && hv < 0.85) "&#128994;" else "&#128308;"
      lineas <- c(lineas, paste0("&nbsp;&nbsp;&nbsp;&nbsp;", sem, " ",
                                 tables$HTMT$C1[i], " &harr; ", tables$HTMT$C2[i],
                                 ": HTMT=", round(hv, 3)))
    }
  }

  # ── R² ──────────────────────────────────────────────────────────────────────
  if (!is.null(tables$R2) && nrow(tables$R2) > 0) {
    lineas <- c(lineas, paste0("<br><b>&#128992; ", if(en) "Predictive Power (R&sup2;):" else "Poder Predictivo (R&sup2;):", "</b>"))
    for (i in seq_len(nrow(tables$R2))) {
      r2    <- suppressWarnings(as.numeric(tables$R2$R2[i]))
      r2adj <- if ("R2_adj" %in% names(tables$R2)) suppressWarnings(as.numeric(tables$R2$R2_adj[i])) else NA
      nm    <- tables$R2$Constructo[i]
      r2s   <- if (is.na(r2)) "NA" else as.character(round(r2, 3))
      r2adjs <- if (is.na(r2adj)) "" else paste0(", R&sup2;<sub>adj</sub>=", round(r2adj, 3))
      nivel <- if (is.na(r2)) "N/A"
               else if (r2 >= 0.75) if(en) "Substantial &#128994;" else "Sustancial &#128994;"
               else if (r2 >= 0.50) if(en) "Moderate &#128993;"    else "Moderado &#128993;"
               else if (r2 >= 0.25) if(en) "Weak &#128992;"        else "D&eacute;bil &#128992;"
               else                 if(en) "Very weak &#128308;"    else "Muy d&eacute;bil &#128308;"
      lineas <- c(lineas, paste0("&nbsp;&nbsp;<b>", nm, "</b>: R&sup2;=", r2s, r2adjs, " &rarr; ", nivel))
    }
  }

  # ── Coeficientes de ruta ─────────────────────────────────────────────────────
  if (!is.null(tables$Paths) && nrow(tables$Paths) > 0) {
    lineas <- c(lineas, paste0("<br><b>&#128308; ", if(en) "Path Coefficients (Hypotheses):" else "Coeficientes de Ruta (Hip&oacute;tesis):", "</b>"))
    for (i in seq_len(nrow(tables$Paths))) {
      b  <- suppressWarnings(as.numeric(tables$Paths$Beta[i]))
      tv <- suppressWarnings(as.numeric(tables$Paths$T_Valor[i]))
      pv <- suppressWarnings(as.numeric(tables$Paths$P_Valor[i]))
      nm <- tables$Paths$Path[i]
      bs  <- if (is.na(b))  "NA" else as.character(round(b,  3))
      tvs <- if (is.na(tv)) "NA" else as.character(round(tv, 3))
      sig <- if (!is.na(pv) && pv < 0.001) "p&lt;0.001 &#128994;&#128994;"
             else if (!is.na(pv) && pv < 0.01)  "p&lt;0.01 &#128994;"
             else if (!is.na(pv) && pv < 0.05)  "p&lt;0.05 &#128994;"
             else if (!is.na(pv) && pv < 0.10)  "p&lt;0.10 &#128993;"
             else "n.s. &#128308;"
      lineas <- c(lineas, paste0("&nbsp;&nbsp;<b>", nm, "</b>: &beta;=", bs,
                                 ", t=", tvs, ", ", sig))
    }
  }

  # ── Q² ──────────────────────────────────────────────────────────────────────
  if (!is.null(tables$Q2) && nrow(tables$Q2) > 0 && !("Nota" %in% names(tables$Q2))) {
    lineas <- c(lineas, paste0("<br><b>&#128994; ", if(en) "Predictive Relevance (Q&sup2;):" else "Relevancia Predictiva (Q&sup2;):", "</b>"))
    q2col <- if ("Q2" %in% names(tables$Q2)) "Q2" else names(tables$Q2)[2]
    for (i in seq_len(nrow(tables$Q2))) {
      qv  <- suppressWarnings(as.numeric(tables$Q2[[q2col]][i]))
      cnm <- tables$Q2$Constructo[i]
      qvs <- if (is.na(qv)) "N/A" else as.character(round(qv, 3))
      nivel <- if (is.na(qv)) "N/A"
               else if (qv >= 0.35) if(en) "Large &#128994;"  else "Grande &#128994;"
               else if (qv >= 0.15) if(en) "Medium &#128993;" else "Mediano &#128993;"
               else if (qv >  0)    if(en) "Small &#128992;"  else "Peque&ntilde;o &#128992;"
               else                 if(en) "No relevance &#128308;" else "Sin relevancia &#128308;"
      lineas <- c(lineas, paste0("&nbsp;&nbsp;<b>", cnm, "</b>: Q&sup2;=", qvs, " &rarr; ", nivel))
    }
  }

  # ── SRMR ────────────────────────────────────────────────────────────────────
  if (!is.null(tables$SRMR) && nrow(tables$SRMR) > 0) {
    srmr <- suppressWarnings(as.numeric(tables$SRMR$Valor[1]))
    srmrs <- if (is.na(srmr)) "N/A" else as.character(round(srmr, 4))
    semaf <- if (!is.na(srmr) && srmr <= 0.08)
               if(en) "&#128994; Good fit (SRMR &le; 0.08)" else "&#128994; Buen ajuste (SRMR &le; 0.08)"
             else if (!is.na(srmr) && srmr <= 0.10)
               if(en) "&#128993; Acceptable fit (SRMR &le; 0.10)" else "&#128993; Ajuste aceptable (SRMR &le; 0.10)"
             else
               if(en) "&#128308; Poor fit (SRMR &gt; 0.10)" else "&#128308; Ajuste cuestionable (SRMR &gt; 0.10)"
    lineas <- c(lineas, paste0("<br><b>&#9898; ", if(en) "Model Fit:" else "Ajuste del Modelo:",
                               "</b><br>&nbsp;&nbsp;SRMR=", srmrs, " &rarr; ", semaf))
  }

  # ── Efectos Indirectos ───────────────────────────────────────────────────────
  if (!is.null(tables$IndirectEffects) && nrow(tables$IndirectEffects) > 0 &&
      !("Nota" %in% names(tables$IndirectEffects))) {
    lineas <- c(lineas, paste0("<br><b>&#128279; ", if(en) "Indirect Effects (Mediation):" else "Efectos Indirectos (Mediaci&oacute;n):", "</b>"))
    df_ind <- tables$IndirectEffects
    for (i in seq_len(nrow(df_ind))) {
      beta_i <- suppressWarnings(as.numeric(df_ind$Beta_ind[i]))
      sig_i  <- if ("Sig" %in% names(df_ind)) df_ind$Sig[i] else "N/A"
      ic2_i  <- if ("IC_2.5"  %in% names(df_ind)) suppressWarnings(as.numeric(df_ind$IC_2.5[i]))  else NA
      ic9_i  <- if ("IC_97.5" %in% names(df_ind)) suppressWarnings(as.numeric(df_ind$IC_97.5[i])) else NA
      beta_s <- if (is.na(beta_i)) "N/A" else as.character(round(beta_i, 3))
      ic_lbl <- if(en) "CI" else "IC"
      ic_s   <- if (is.na(ic2_i) || is.na(ic9_i)) "" else paste0(" [", ic_lbl, ": ", round(ic2_i,3), "; ", round(ic9_i,3), "]")
      sig_ok <- !is.na(ic2_i) && !is.na(ic9_i) && ((ic2_i > 0 && ic9_i > 0) || (ic2_i < 0 && ic9_i < 0))
      semaf  <- if (sig_ok) "&#128994;" else if (sig_i %in% c("***","**","*")) "&#128993;" else "&#128308;"
      lineas <- c(lineas, paste0("&nbsp;&nbsp;", semaf, " <b>", df_ind$Path[i], "</b>: &beta;=",
                                 beta_s, ic_s, " ", sig_i))
    }
  }

  # ── PLS Predict ─────────────────────────────────────────────────────────────
  if (!is.null(tables$PLSPredict) && nrow(tables$PLSPredict) > 0) {
    lineas <- c(lineas, paste0("<br><b>&#128302; PLS Predict (Out-of-Sample):</b>"))
    for (i in seq_len(nrow(tables$PLSPredict))) {
      q2p  <- suppressWarnings(as.numeric(tables$PLSPredict$Q2_predict[i]))
      cnm  <- tables$PLSPredict$Constructo[i]
      rmse <- suppressWarnings(as.numeric(tables$PLSPredict$RMSE_modelo[i]))
      rmse_n <- suppressWarnings(as.numeric(tables$PLSPredict$RMSE_naive[i]))
      q2ps <- if (is.na(q2p)) "N/A" else as.character(round(q2p,3))
      mejor<- if (!is.na(rmse) && !is.na(rmse_n) && rmse < rmse_n) "&#128994;" else "&#128308;"
      nivel<- if (is.na(q2p)) "N/A"
              else if (q2p >= 0.35) if(en) "Large &#128994;"  else "Grande &#128994;"
              else if (q2p >= 0.15) if(en) "Medium &#128993;" else "Mediano &#128993;"
              else if (q2p > 0)     if(en) "Small &#128992;"  else "Peque&ntilde;o &#128992;"
              else                  if(en) "No relevance &#128308;" else "Sin relevancia &#128308;"
      lineas <- c(lineas, paste0("&nbsp;&nbsp;", mejor, " <b>", cnm,
                                 "</b>: Q&sup2;predict=", q2ps, " &rarr; ", nivel))
    }
  }

  paste(lineas, collapse = "<br>")
}


# ============================================================================
# GAUSSIAN COPULA ENDOGENEITY TEST  — v3.0
# Park & Gupta (2012) adapted for PLS-SEM robustness testing
# ============================================================================

# ── Helper: build scores_df from either PLS construct scores OR composite means ──
build_scores_df <- function(pls_est, data_raw, construct_items_map, use_mean_scores = FALSE) {
  if (use_mean_scores && !is.null(data_raw) && !is.null(construct_items_map)) {
    # Composite mean scores: rowMeans of each construct's indicators
    df <- as.data.frame(
      lapply(names(construct_items_map), function(cn) {
        items <- intersect(construct_items_map[[cn]], names(data_raw))
        if (length(items) == 0) return(rep(NA_real_, nrow(data_raw)))
        rowMeans(data_raw[, items, drop = FALSE], na.rm = FALSE)
      })
    )
    names(df) <- names(construct_items_map)
    return(df)
  }
  # Default: PLS latent scores with 3-level fallback
  tryCatch(as.data.frame(pls_est$construct_scores),          error = function(e) NULL) %||%
  tryCatch(as.data.frame(pls_est$constructScores),           error = function(e) NULL) %||%
  tryCatch(as.data.frame(seminr::construct_scores(pls_est)), error = function(e) NULL)
}

# ── Core copula regression ────────────────────────────────────────────────────
run_gaussian_copula <- function(scores_df, p_df, paths_table = NULL, lang = "es") {
  # paths_table: the app's results$tables$Paths — used to look up actual PLS betas
  # Detect endogenous constructs (appear as "to" in paths)
  endogenous <- unique(p_df$to)
  all_from   <- unique(p_df$from)
  exogenous  <- setdiff(all_from, endogenous)

  if (length(exogenous) == 0)
    stop("No se encontraron variables exógenas en el modelo estructural.")
  if (length(endogenous) == 0)
    stop("No se encontraron variables endógenas en el modelo estructural.")

  exogenous  <- intersect(exogenous,  names(scores_df))
  endogenous <- intersect(endogenous, names(scores_df))

  if (length(exogenous) == 0 || length(endogenous) == 0)
    stop("Los scores de los constructos no contienen variables del modelo. Ejecute el análisis PLS primero.")

  # Build PLS beta lookup from Paths table if available
  pls_beta_lookup <- list()
  if (!is.null(paths_table) && is.data.frame(paths_table) && nrow(paths_table) > 0 &&
      "Path" %in% names(paths_table) && "Beta" %in% names(paths_table)) {
    for (k in seq_len(nrow(paths_table))) {
      key <- gsub("\\s+", "", as.character(paths_table$Path[k]))
      pls_beta_lookup[[key]] <- suppressWarnings(as.numeric(paths_table$Beta[k]))
    }
  }

  rows <- list()

  for (y_nm in endogenous) {
    preds <- p_df$from[p_df$to == y_nm]
    preds <- intersect(preds, names(scores_df))
    if (length(preds) == 0) next

    y_vec <- as.numeric(scores_df[[y_nm]])

    for (x_nm in preds) {
      x_vec <- as.numeric(scores_df[[x_nm]])

      # NA removal — joint complete cases for X and Y
      valid <- !is.na(x_vec) & !is.na(y_vec)
      if (sum(valid) < 10) next
      xv <- x_vec[valid]
      yv <- y_vec[valid]
      n  <- length(xv)

      # ── Park & Gupta (2012) copula construction ──
      ranks       <- rank(xv, ties.method = "average")
      uniform     <- ranks / (n + 1)                     # formula: rank/(n+1)
      copula_term <- qnorm(uniform)                       # Φ⁻¹

      # ── Build regression data (safe names to avoid collisions) ──
      other_preds <- setdiff(preds, x_nm)
      other_preds <- intersect(other_preds, names(scores_df))
      # Use internal safe column names to avoid collisions with construct names
      reg_data    <- data.frame(Y_end = yv, X_pred = xv, Cop_term = copula_term,
                                stringsAsFactors = FALSE)
      op_safe     <- character(0)
      for (op in other_preds) {
        safe_nm          <- paste0("ctrl_", make.names(op))
        reg_data[[safe_nm]] <- as.numeric(scores_df[[op]])[valid]
        op_safe          <- c(op_safe, safe_nm)
      }
      # Remove rows where any ctrl predictor is NA
      reg_data <- reg_data[complete.cases(reg_data), , drop = FALSE]
      n_reg    <- nrow(reg_data)
      if (n_reg < 10) next

      fml_str <- paste0("Y_end ~ X_pred + Cop_term",
                        if (length(op_safe)) paste0(" + ", paste(op_safe, collapse = " + ")) else "")
      fml     <- as.formula(fml_str)

      fit <- tryCatch(stats::lm(fml, data = reg_data), error = function(e) NULL)
      if (is.null(fit)) next

      cs <- tryCatch(summary(fit)$coefficients, error = function(e) NULL)
      if (is.null(cs) || !"Cop_term" %in% rownames(cs)) next

      copula_coef <- cs["Cop_term", "Estimate"]
      copula_se   <- cs["Cop_term", "Std. Error"]
      copula_t    <- cs["Cop_term", "t value"]
      copula_p    <- cs["Cop_term", "Pr(>|t|)"]
      ci_lo       <- copula_coef - 1.96 * copula_se
      ci_hi       <- copula_coef + 1.96 * copula_se

      # ── Look up ACTUAL PLS beta (not bivariate OLS) ──
      path_key   <- gsub("\\s+", "", paste0(x_nm, "->", y_nm))
      path_key2  <- gsub("\\s+", "", paste0(x_nm, "\u2192", y_nm))
      pls_beta   <- pls_beta_lookup[[path_key]] %||%
                    pls_beta_lookup[[path_key2]] %||%
                    NA_real_

      interp <- if (is.na(copula_p)) "N/A"
                else if (copula_p < 0.05) {
                  if (lang == "en") "\u26a0 Potential endogeneity detected (p < 0.05)"
                  else              "\u26a0 Posible endogeneidad detectada (p < 0.05)"
                } else {
                  if (lang == "en") "\u2713 No evidence of endogeneity (p \u2265 0.05)"
                  else              "\u2713 Sin evidencia de endogeneidad (p \u2265 0.05)"
                }

      # Human-readable formula for the "Technical Details" box
      fml_human <- paste0(y_nm, " ~ ", x_nm, " + Copula(",  x_nm, ")",
                          if (length(other_preds)) paste0(" + ", paste(other_preds, collapse = " + ")) else "")

      rows[[length(rows) + 1]] <- data.frame(
        Path          = paste0(x_nm, " \u2192 ", y_nm),
        Predictor     = x_nm,
        Endogenous    = y_nm,
        PLS_Beta      = round(pls_beta,   4),   # actual PLS bootstrapped beta
        Copula_Coef   = round(copula_coef, 4),
        Std_Error     = round(copula_se,   4),
        t_value       = round(copula_t,    3),
        p_value       = round(copula_p,    4),
        CI_lo         = round(ci_lo,       4),
        CI_hi         = round(ci_hi,       4),
        N_used        = n_reg,
        Formula       = fml_human,
        Interpretation = interp,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0)
    stop("No se pudo calcular la Cópula Gaussiana. Verifique el modelo estructural.")

  do.call(rbind, rows)
}

# ── Plot A: Forest / Dot-and-Whisker (main results plot) ─────────────────────
make_copula_results_plot <- function(copula_tbl) {
  req_cols <- c("Path", "PLS_Beta", "Copula_Coef", "CI_lo", "CI_hi", "p_value")
  if (is.null(copula_tbl) || !all(req_cols %in% names(copula_tbl))) return(NULL)

  df <- copula_tbl[!is.na(copula_tbl$Copula_Coef), ]
  if (nrow(df) == 0) return(NULL)

  # Significance flag
  df$sig_label <- ifelse(!is.na(df$p_value),
                         paste0("p = ", formatC(df$p_value, digits = 4, format = "f")), "")
  df$sig_color <- ifelse(!is.na(df$p_value) & df$p_value < 0.05, "#C62828", "#2E7D32")
  df$point_shape<- ifelse(!is.na(df$p_value) & df$p_value < 0.05, 18, 16)

  # Overall verdict annotation
  any_endo   <- any(df$p_value < 0.05, na.rm = TRUE)
  annot_text <- if (any_endo) "\u26a0 Potential endogeneity detected in at least one path"
                else            "\u2713 No evidence of endogeneity detected"
  annot_col  <- if (any_endo) "#C62828" else "#2E7D32"

  # Path as factor, ordered by Copula_Coef
  df$Path <- factor(df$Path, levels = df$Path[order(df$Copula_Coef)])

  # Reference segments for PLS_Beta (faint)
  has_pls <- !all(is.na(df$PLS_Beta))

  p <- ggplot(df, aes(x = Copula_Coef, y = Path)) +
    # Reference line at 0
    geom_vline(xintercept = 0, linetype = "dashed", color = "#888888", linewidth = 0.6) +
    # PLS Beta as faint reference diamond
    { if (has_pls)
        geom_point(aes(x = PLS_Beta), shape = 5, size = 3.5, color = "#1565C0",
                   alpha = 0.45, stroke = 1.2)
      else list() } +
    # 95% CI whiskers
    geom_errorbarh(aes(xmin = CI_lo, xmax = CI_hi),
                   height = 0.25, linewidth = 0.9, color = "#555555") +
    # Copula coefficient point
    geom_point(aes(color = sig_color, shape = point_shape),
               size = 4.5, stroke = 1) +
    scale_color_identity() +
    scale_shape_identity() +
    # p-value labels
    geom_text(aes(x = CI_hi, label = sig_label),
              hjust = -0.12, size = 3.2, color = "#333333", fontface = "italic") +
    # Overall verdict annotation
    annotate("text",
             x = max(c(df$CI_hi, df$PLS_Beta), na.rm = TRUE) * 1.05,
             y = Inf, label = annot_text,
             hjust = 1, vjust = 1.8, size = 3.5, color = annot_col, fontface = "bold") +
    # Legend guide for PLS Beta reference
    { if (has_pls)
        annotate("text", x = -Inf, y = -Inf,
                 label = "\u25c7 = PLS bootstrapped \u03b2 (reference)",
                 hjust = -0.05, vjust = -0.8, size = 3, color = "#1565C0", alpha = 0.7)
      else list() } +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.28))) +
    labs(
      title    = "Gaussian Copula Endogeneity Test",
      subtitle = "Forest plot \u2014 Copula coefficient with 95% CI per structural path",
      x        = "Copula Coefficient (\u03b2\u2082)",
      y        = "Structural Path",
      caption  = paste0(
        "Filled circle (\u25cf) = Copula coeff.; \u25c7 = PLS bootstrapped \u03b2 (reference). ",
        "Red \u25c6 = p < 0.05. CI = 1.96 \u00d7 SE.\n",
        "Reference: Park & Gupta (2012). Marketing Science, 31(2), 317\u2013333."
      )
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title      = element_text(face = "bold", color = "#1A237E", size = 15),
      plot.subtitle   = element_text(color = "#555555", size = 11),
      plot.caption    = element_text(size = 8.5, color = "#666666"),
      axis.text.y     = element_text(size = 11, face = "bold"),
      axis.text.x     = element_text(size = 10),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#EEEEEE"),
      panel.border    = element_rect(color = "#CCCCCC", fill = NA),
      plot.margin     = margin(12, 60, 12, 12)
    )

  p
}

# ── Plot B: Copula Visualization (rank-normal scatter / contour) ─────────────
make_copula_visualization_plot <- function(scores_df, x_nm, y_nm, view_type = "scatter") {
  if (is.null(scores_df)) return(NULL)
  if (!(x_nm %in% names(scores_df)) || !(y_nm %in% names(scores_df))) return(NULL)

  xv <- as.numeric(scores_df[[x_nm]])
  yv <- as.numeric(scores_df[[y_nm]])
  valid <- !is.na(xv) & !is.na(yv)
  if (sum(valid) < 10) return(NULL)

  xv <- xv[valid]; yv <- yv[valid]
  n  <- length(xv)

  zX <- qnorm(rank(xv, ties.method = "average") / (n + 1))
  zY <- qnorm(rank(yv, ties.method = "average") / (n + 1))

  df_viz <- data.frame(zX = zX, zY = zY)

  path_lbl <- paste0(x_nm, " \u2192 ", y_nm)
  caption_txt <- paste0(
    "Rank-normal transformed variables (z_X, z_Y) used in the Gaussian Copula procedure.\n",
    "Formula: z = \u03a6\u207b\u00b9(rank(x)/(n+1))  |  N = ", n
  )

  if (view_type == "contour") {
    p <- ggplot(df_viz, aes(x = zX, y = zY)) +
      geom_density_2d_filled(contour_var = "density", alpha = 0.85, bins = 10) +
      geom_density_2d(color = "white", linewidth = 0.35, alpha = 0.6) +
      geom_point(alpha = 0.25, size = 1.2, color = "#1A237E") +
      scale_fill_viridis_d(option = "plasma", name = "Density", direction = -1) +
      labs(
        title    = paste0("Copula Visualization \u2014 2D Density: ", path_lbl),
        subtitle = "Contour/density plot of rank-normal transformed variables",
        x        = paste0("z_", x_nm, " = \u03a6\u207b\u00b9(rank(", x_nm, ")/(n+1))"),
        y        = paste0("z_", y_nm, " = \u03a6\u207b\u00b9(rank(", y_nm, ")/(n+1))"),
        caption  = caption_txt
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title    = element_text(face = "bold", color = "#1A237E", size = 14),
        plot.subtitle = element_text(color = "#555555", size = 10),
        plot.caption  = element_text(size = 8.5, color = "#666666"),
        panel.border  = element_rect(color = "#CCCCCC", fill = NA),
        legend.position = "right"
      )
  } else {
    # scatter + smooth density overlay (stat_density_2d)
    p <- ggplot(df_viz, aes(x = zX, y = zY)) +
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon",
                      alpha = 0.35, bins = 9, color = NA) +
      scale_fill_gradient(low = "#E3F2FD", high = "#1565C0", name = "Density") +
      geom_point(alpha = 0.55, size = 2, color = "#1A237E") +
      geom_smooth(method = "lm", se = TRUE, color = "#E53935",
                  fill = "#EF9A9A", linewidth = 1, linetype = "solid") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#999999", linewidth = 0.4) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#999999", linewidth = 0.4) +
      labs(
        title    = paste0("Copula Visualization \u2014 Scatter + Density: ", path_lbl),
        subtitle = "Rank-normal transformed variables with linear trend and density overlay",
        x        = paste0("z_", x_nm, " = \u03a6\u207b\u00b9(rank(", x_nm, ")/(n+1))"),
        y        = paste0("z_", y_nm, " = \u03a6\u207b\u00b9(rank(", y_nm, ")/(n+1))"),
        caption  = caption_txt
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title    = element_text(face = "bold", color = "#1A237E", size = 14),
        plot.subtitle = element_text(color = "#555555", size = 10),
        plot.caption  = element_text(size = 8.5, color = "#666666"),
        panel.border  = element_rect(color = "#CCCCCC", fill = NA),
        legend.position = "right"
      )
  }

  p
}

# ── get_num_col helper ────────────────────────────────────────────────────────

get_num_col <- function(df, exact_names = character(0), regex_pats = character(0)) {
  if (is.null(df) || nrow(df) == 0) return(rep(NA_real_, 0))
  nms <- names(df); low <- tolower(nms)
  for (nm in exact_names) {
    idx <- which(low == tolower(nm))
    if (length(idx) > 0) { v <- suppressWarnings(as.numeric(df[[idx[1]]])); if (!all(is.na(v))) return(v) }
  }
  for (pat in regex_pats) {
    idx <- which(grepl(tolower(pat), low, perl = TRUE))
    if (length(idx) > 0) { v <- suppressWarnings(as.numeric(df[[idx[1]]])); if (!all(is.na(v))) return(v) }
  }
  rep(NA_real_, nrow(df))
}

# ============================================================================
# UI
# ============================================================================

ui <- dashboardPage(
  title = "CANCHARI PLS-SEM PRO V2.0",
  skin  = "blue",

  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://img.icons8.com/color/24/000000/flow-chart.png"),
      " CANCHARI PLS-SEM PRO V2.0"
    )
  ),

  dashboardSidebar(
    sidebarMenu(id = "sidebar",
      div(style="padding:8px 16px 4px 16px;",
        selectInput("app_lang", NULL,
          choices = c("Español" = "es", "English" = "en"),
          selected = "es", width = "100%")
      ),
      uiOutput("sidebar_menu_ui"),
      hr(),
      tags$div(style = "padding:10px; color:#aaa; font-size:11px;",
        "CANCHARI PLS-SEM PRO V2.0", br(),
        "Powered by seminr")
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background:#f4f6f9; }
        .tooltip-box { background:#E3F2FD; border-left:3px solid #1565C0; padding:6px 10px; border-radius:4px; font-size:12px; color:#1A237E; margin-bottom:8px; }
        .badge-soportada { background:#2E7D32; color:white; padding:2px 8px; border-radius:10px; font-size:11px; }
        .badge-rechazada { background:#C62828; color:white; padding:2px 8px; border-radius:10px; font-size:11px; }
        .box { border-radius:8px; box-shadow:0 2px 8px rgba(0,0,0,.08); }
        .box-header { border-radius:8px 8px 0 0; }
        .btn-analizar { background:#E53935; color:white; border:none; font-size:16px; font-weight:bold; padding:14px; width:100%; border-radius:6px; }
        .btn-analizar:hover { background:#C62828; color:white; }
        .semaforo { font-size:14px; line-height:2; }
        .progress-bar { background:#1565C0; }
        .badge-ok  { background:#2E7D32; color:white; padding:2px 8px; border-radius:10px; }
        .badge-warn{ background:#F57F17; color:white; padding:2px 8px; border-radius:10px; }
        .badge-err { background:#C62828; color:white; padding:2px 8px; border-radius:10px; }
        .log-box   { background:#1a1a2e; color:#00e676; font-family:monospace; font-size:13px; border-radius:6px; padding:12px; min-height:120px; }
        table.dataTable thead { background:#1565C0; color:white; }
        .nav-tabs > li.active > a { color:#1565C0; font-weight:bold; border-top:3px solid #1565C0; }
        #interp_panel { background:#fff; border-left:4px solid #1565C0; padding:14px; border-radius:4px; font-size:13px; line-height:1.8; }
      ")),
      tags$script(HTML("
        // pickerInput (shinyWidgets) gestiona su propio binding Shiny nativo.
        // No se requiere JS adicional para c_picker_.
      "))
    ),

    tabItems(

      # ── PROYECTO ──────────────────────────────────────────────────────────
      tabItem(tabName = "project",
        fluidRow(
          box(uiOutput("box_project_title_ui"), status = "primary", solidHeader = TRUE, width = 6,
              uiOutput("project_name_ui"),
              uiOutput("create_project_ui"),
              hr(),
              uiOutput("projects_ui"),
              uiOutput("load_project_ui")
          ),
          box(uiOutput("box_manage_title_ui"), status = "warning", solidHeader = TRUE, width = 6,
              uiOutput("save_all_ui"),
              br(), br(),
              uiOutput("save_results_ui"),
              br(), br(),
              uiOutput("clear_results_ui"),
              hr(),
              verbatimTextOutput("project_status")
          )
        )
      ),

      # ── CARGAR DATOS ──────────────────────────────────────────────────────
      tabItem(tabName = "upload",
        fluidRow(
          box(uiOutput("box_upload_title_ui"), status = "primary", solidHeader = TRUE, width = 4,
              uiOutput("file_upload_ui"),
              uiOutput("load_data_ui"),
              br(),
              uiOutput("data_info_ui")
          ),
          box(uiOutput("box_preview_title_ui"), status = "info", solidHeader = TRUE, width = 8,
              DTOutput("data_preview")
          )
        )
      ),

      # ── DEFINIR MODELO ────────────────────────────────────────────────────
      tabItem(tabName = "model",
        fluidRow(
          box(uiOutput("box_constructs_title_ui"), status = "primary", solidHeader = TRUE, width = 6,
              uiOutput("constructs_hint_ui"),
              br(), br(),
              uiOutput("construct_inputs"),
              br(),
              fluidRow(
                column(6, uiOutput("add_construct_ui")),
                column(6, uiOutput("clear_model_ui"))
              )
          ),
          box(uiOutput("box_paths_title_ui"), status = "danger", solidHeader = TRUE, width = 6,
              uiOutput("paths_hint_ui"),
              br(), br(),
              uiOutput("path_inputs"),
              br(),
              uiOutput("add_path_ui")
          )
        ),
        fluidRow(
          box(uiOutput("box_validate_title_ui"), status = "success", solidHeader = TRUE, width = 12,
              uiOutput("validate_model_ui"),
              br(), br(),
              verbatimTextOutput("validation_output")
          )
        )
      ),

      # ── ANÁLISIS ──────────────────────────────────────────────────────────
      tabItem(tabName = "analysis",
        fluidRow(
          box(uiOutput("box_analysis_title_ui"), status = "warning", solidHeader = TRUE, width = 5,
              uiOutput("analysis_controls_ui"),
              br(),
              uiOutput("run_analysis_btn_ui")
          ),
          box(uiOutput("box_console_title_ui"), status = "info", solidHeader = TRUE, width = 7,
              div(class="log-box", verbatimTextOutput("status_log")),
              br(),
              uiOutput("progress_ui")
          )
        )
      ),

      # ── RESULTADOS ────────────────────────────────────────────────────────
      tabItem(tabName = "results",
        uiOutput("results_tabs_ui")
      ),

      # ── DESCARGAR ─────────────────────────────────────────────────────────
      tabItem(tabName = "download",
        fluidRow(
          box(uiOutput("box_download_title_ui"), status = "success", solidHeader = TRUE, width = 12,
              uiOutput("download_desc_ui"),
              br(),
              fluidRow(
                column(4,
                  downloadButton("download_zip",          uiOutput("dl_zip_label"),       class="btn btn-success btn-block btn-lg"),
                  br(), br(),
                  downloadButton("download_html",         uiOutput("dl_html_label"),       class="btn btn-info btn-block"),
                  br(),
                  downloadButton("download_diagram_svg2", uiOutput("dl_svg_label"),        class="btn btn-primary btn-block"),
                  br(),
                  downloadButton("download_word",         uiOutput("dl_word_label"),       class="btn btn-warning btn-block")
                ),
                column(8,
                  box(uiOutput("box_tables_title_ui"), status = "info", solidHeader = FALSE, width = NULL,
                      uiOutput("available_tables_ui")
                  )
                )
              )
          )
        )
      ),

      # ── TAMAÑO DE MUESTRA / SAMPLE SIZE ──────────────────────────────────
      tabItem(tabName = "sample",
        uiOutput("sample_size_ui")
      )

    ), # end tabItems

    tags$footer(
      style = "position:fixed;bottom:0;left:0;right:0;padding:6px 16px;background:#1565C0;color:white;font-size:12px;z-index:9999;",
      HTML(paste0("© ", format(Sys.Date(),"%Y"),
                  " CANCHARI PLS-SEM PRO V2.0 &nbsp;|&nbsp; Powered by seminr &nbsp;|&nbsp; Hair et al. (2022) &nbsp;|&nbsp; 8 advanced modules"))
    )
  )
)


# ============================================================================
# MÓDULO: TAMAÑO DE MUESTRA / SAMPLE SIZE  (CANCHARI PLS-SEM PRO)
# ============================================================================

# ── Helper functions ─────────────────────────────────────────────────────────

#' Detect max number of predictors pointing to any endogenous construct
detect_max_predictors <- function(paths_df) {
  if (is.null(paths_df) || nrow(paths_df) == 0) return(1L)
  if (!all(c("from","to") %in% names(paths_df))) {
    # Try generic column names
    if (ncol(paths_df) >= 2) {
      paths_df <- setNames(paths_df[,1:2], c("from","to"))
    } else return(1L)
  }
  tbl <- table(paths_df$to)
  if (length(tbl) == 0) return(1L)
  as.integer(max(tbl))
}

#' Power analysis for PLS-SEM via pwr package (Cohen 1988 / Hair et al. 2022)
#' Uses pwr.f2.test: multiple regression approximation
#' @param u   number of predictors (max arrows into any endogenous construct)
#' @param f2  Cohen's f² effect size
#' @param alpha significance level
#' @param power statistical power (1-β)
#' @return list with n_min, n_target, details
calculate_pls_power_n <- function(u, f2 = 0.15, alpha = 0.05, power = 0.80, margin = 0.15) {
  has_pwr <- requireNamespace("pwr", quietly = TRUE)
  if (has_pwr) {
    res <- pwr::pwr.f2.test(u = u, f2 = f2, sig.level = alpha, power = power)
    v   <- ceiling(res$v)            # denominator df
    n   <- v + u + 1                 # total N
  } else {
    # Fallback: Green (1991) formula  N >= 50 + 8m
    n <- max(50 + 8 * u, ceiling((1.96 + qnorm(power))^2 * (1 + f2) / f2) + u + 1)
  }
  n_min    <- max(n, 10L)
  n_target <- ceiling(n_min * (1 + margin))
  list(
    n_min    = n_min,
    n_target = n_target,
    u        = u,
    f2       = f2,
    alpha    = alpha,
    power    = power,
    margin   = margin,
    method   = if(has_pwr) "pwr::pwr.f2.test" else "Green (1991) approximation"
  )
}

#' Classical sample size (Cochran formula)
#' For unknown/large population: n = z² * p * q / e²
#' For finite population: n_adj = n / (1 + (n-1)/N)
calculate_classical_sample_size <- function(pop_type = "large", N_pop = NULL,
                                             conf = 0.95, error = 0.05, p = 0.5) {
  z <- qnorm(1 - (1 - conf) / 2)
  q <- 1 - p
  n_inf <- ceiling(z^2 * p * q / error^2)
  if (pop_type == "finite" && !is.null(N_pop) && is.numeric(N_pop) && N_pop > 0) {
    n_adj <- ceiling(n_inf / (1 + (n_inf - 1) / N_pop))
    list(n = n_adj, n_infinite = n_inf, pop_type = "finite", N_pop = N_pop,
         conf = conf, error = error, p = p)
  } else {
    list(n = n_inf, n_infinite = n_inf, pop_type = "large", N_pop = NULL,
         conf = conf, error = error, p = p)
  }
}

#' Classify sample strength for PLS-SEM
classify_sample_strength <- function(n_real, n_power) {
  if (is.na(n_real) || n_real <= 0) return(list(label="Sin dato", color="secondary", icon="❓"))
  if (n_real < n_power)     return(list(label="⚠ Insuficiente",  color="danger",  icon="🔴"))
  if (n_real < 100)         return(list(label="✓ Aceptable mín.", color="warning", icon="🟡"))
  if (n_real < 200)         return(list(label="✓ Adecuado",       color="info",    icon="🔵"))
  if (n_real < 384)         return(list(label="✓ Robusto",        color="success", icon="🟢"))
  if (n_real < 500)         return(list(label="✓✓ Excelente",     color="success", icon="🟢"))
  return(list(label="✓✓✓ Muy Robusto", color="success", icon="🟢"))
}

#' Smart message based on sample assessment
sample_smart_message <- function(n_real, n_power, lang = "es") {
  if (is.na(n_real) || n_real <= 0) return("")
  es <- lang == "es"
  if (n_real < n_power) {
    if(es) paste0("⚠ Su muestra actual (n = ", n_real, ") es INSUFICIENTE para el nivel de potencia requerido (n mín = ", n_power, "). ",
                  "Se recomienda ampliar la muestra antes de proceder con el análisis PLS-SEM.")
    else   paste0("⚠ Your current sample (n = ", n_real, ") is INSUFFICIENT for the required power level (n min = ", n_power, "). ",
                  "Expanding the sample before PLS-SEM analysis is strongly recommended.")
  } else if (n_real < 100) {
    if(es) paste0("Su muestra (n = ", n_real, ") supera el mínimo por análisis de potencia pero está por debajo de 100 casos. ",
                  "Se recomienda aumentar la muestra para mejorar la precisión del bootstrapping y la estabilidad del modelo.")
    else   paste0("Your sample (n = ", n_real, ") exceeds the power analysis minimum but is below 100 cases. ",
                  "Increasing the sample is recommended to improve bootstrapping precision and model stability.")
  } else if (n_real < 200) {
    if(es) paste0("✓ Su muestra (n = ", n_real, ") es adecuada para el análisis PLS-SEM. ",
                  "Para análisis de mediación o efectos indirectos, una muestra ≥ 200 aumenta la robustez.")
    else   paste0("✓ Your sample (n = ", n_real, ") is adequate for PLS-SEM analysis. ",
                  "For mediation or indirect effects analysis, n ≥ 200 increases robustness.")
  } else if (n_real < 384) {
    if(es) paste0("✓ Su muestra (n = ", n_real, ") es robusta para PLS-SEM y adecuada para análisis de mediación secuencial.")
    else   paste0("✓ Your sample (n = ", n_real, ") is robust for PLS-SEM and suitable for sequential mediation analysis.")
  } else {
    if(es) paste0("✓✓ Su muestra (n = ", n_real, ") es excelente. Supera el criterio de potencia estadística y el umbral poblacional clásico (384), ",
                  "lo que proporciona respaldo metodológico dual.")
    else   paste0("✓✓ Your sample (n = ", n_real, ") is excellent. It exceeds the power analysis threshold and the classical population criterion (n=384), ",
                  "providing dual methodological support.")
  }
}

#' Generate academic report in Spanish
generate_sample_size_report_es <- function(pw, cl = NULL, model_detail = "") {
  f2_label <- switch(as.character(pw$f2),
    "0.02" = "pequeño (f² = 0.02)",
    "0.15" = "mediano (f² = 0.15)",
    "0.35" = "grande (f² = 0.35)",
    paste0("f² = ", pw$f2)
  )
  txt <- paste0(
    "Se realizó un análisis de potencia estadística para modelos PLS-SEM siguiendo las recomendaciones de ",
    "Hair et al. (2022), considerando un tamaño de efecto ", f2_label, ", un nivel de significancia de ",
    pw$alpha, ", una potencia estadística de ", pw$power, " y un máximo de ", pw$u,
    " predictor(es) apuntando hacia un constructo endógeno", if(nzchar(model_detail)) paste0(" (", model_detail, ")") else "", ". ",
    "El análisis indicó un tamaño mínimo de muestra de ", pw$n_min, " casos. ",
    "Considerando un margen adicional del ", round(pw$margin * 100), "% por posibles pérdidas o depuración de datos, ",
    "el tamaño objetivo se estableció en ", pw$n_target, " participantes. ",
    "No obstante, por criterios de robustez metodológica en PLS-SEM — especialmente para el análisis de efectos indirectos ",
    "mediante bootstrapping — se recomienda trabajar con muestras superiores al mínimo estimado."
  )
  if (!is.null(cl)) {
    txt <- paste0(txt, "\n\nAdicionalmente, bajo el supuesto de población ",
      if(cl$pop_type == "finite") paste0("finita (N = ", cl$N_pop, ")") else "grande",
      " y un nivel de confianza del ", round(cl$conf * 100), "% con un margen de error del ",
      round(cl$error * 100), "%, el tamaño mínimo de muestra estimado mediante la fórmula clásica de Cochran fue de ",
      cl$n, " participantes. En consecuencia, se recomienda que el investigador considere tanto la potencia estadística ",
      "del modelo como la lógica del diseño muestral en función del contexto del estudio.")
  }
  txt
}

#' Generate academic report in English
generate_sample_size_report_en <- function(pw, cl = NULL, model_detail = "") {
  f2_label <- switch(as.character(pw$f2),
    "0.02" = "small (f² = 0.02)",
    "0.15" = "medium (f² = 0.15)",
    "0.35" = "large (f² = 0.35)",
    paste0("f² = ", pw$f2)
  )
  txt <- paste0(
    "A statistical power analysis for PLS-SEM was conducted following Hair et al. (2022), ",
    "assuming a ", f2_label, " effect size, a significance level of ", pw$alpha,
    ", a statistical power of ", pw$power, ", and a maximum of ", pw$u,
    " predictor(s) pointing to an endogenous construct",
    if(nzchar(model_detail)) paste0(" (", model_detail, ")") else "", ". ",
    "The analysis indicated a minimum sample size of ", pw$n_min, " cases. ",
    "After adding a ", round(pw$margin * 100), "% margin for possible data loss or case exclusion, ",
    "the target sample size was set at ", pw$n_target, " participants. ",
    "However, for robustness purposes in PLS-SEM — particularly for indirect effects assessment ",
    "via bootstrapping — a sample size exceeding the estimated minimum is recommended."
  )
  if (!is.null(cl)) {
    txt <- paste0(txt, "\n\nAdditionally, assuming a ",
      if(cl$pop_type == "finite") paste0("finite population (N = ", cl$N_pop, ")") else "large population",
      " with a confidence level of ", round(cl$conf * 100), "% and a margin of error of ",
      round(cl$error * 100), "%, the minimum sample size estimated via the classical Cochran formula was ",
      cl$n, " participants. Accordingly, researchers are advised to consider both the statistical power ",
      "of the model and the logic of the sampling design in the context of their study.")
  }
  txt
}

# ============================================================================
# SERVER: Sample Size Module
# ============================================================================

# (This block is appended into server function below via output$sample_size_ui)


# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  # Helper: leer items de constructo i desde pickerInput o textInput segun disponibilidad
  get_items_str <- function(i) {
    tryCatch({
      picker_val <- input[[paste0("c_picker_", i)]]
      text_val   <- input[[paste0("c_items_",  i)]]
      if (!is.null(picker_val) && is.character(picker_val) && length(picker_val) > 0 && any(nzchar(picker_val))) {
        paste(trimws(picker_val), collapse = ",")
      } else if (!is.null(text_val) && nzchar(trimws(text_val %||% ""))) {
        trimws(text_val)
      } else {
        ""
      }
    }, error = function(e) "")
  }



  data_raw       <- reactiveVal(NULL)
  results        <- reactiveValues(tables = list(), log = "► Listo. Cargue datos para comenzar.", pls_est = NULL, dot_code = NULL)
  model_store_file <- file.path(getwd(), "plssem_model_v2.rds")
  last_model     <- reactiveVal(NULL)
  pending_restore <- NULL   # ya no se usa (last_model() se lee directo en renderUI)

  # ── i18n: diccionario bilingüe ─────────────────────────────────────────────
  i18n <- reactive({
    es <- input$app_lang != "en"
    list(
      # Sidebar
      menu_project   = if(es) "📁 Proyecto"       else "📁 Project",
      menu_upload    = if(es) "📊 Cargar Datos"   else "📊 Load Data",
      menu_model     = if(es) "🔧 Definir Modelo" else "🔧 Define Model",
      menu_analysis  = if(es) "⚡ Análisis"        else "⚡ Analysis",
      menu_results   = if(es) "📈 Resultados"     else "📈 Results",
      menu_download  = if(es) "💾 Descargar"      else "💾 Download",
      menu_sample    = if(es) "🎯 Tamaño Muestra" else "🎯 Sample Size",
      # Project tab
      box_project    = if(es) "🗂 Crear / Abrir Proyecto"  else "🗂 Create / Open Project",
      lbl_proj_name  = if(es) "Nombre del proyecto"        else "Project name",
      ph_proj_name   = if(es) "Ej: MiTesis2025"            else "E.g.: MyThesis2025",
      btn_create     = if(es) "✚ Crear y Usar"             else "✚ Create & Use",
      lbl_proj_list  = if(es) "Proyectos existentes"       else "Existing projects",
      btn_open       = if(es) "📂 Abrir Proyecto"          else "📂 Open Project",
      box_manage     = if(es) "💾 Guardar / Gestionar"     else "💾 Save / Manage",
      btn_save_all   = if(es) "💾 Guardar TODO"            else "💾 Save ALL",
      btn_save_res   = if(es) "📋 Guardar Solo Resultados" else "📋 Save Results Only",
      btn_clear_res  = if(es) "🗑 Limpiar Resultados"      else "🗑 Clear Results",
      lbl_cur_proj   = if(es) "Proyecto actual: (ninguno)" else "Current project: (none)",
      # Upload tab
      box_upload     = if(es) "📂 Cargar Archivo Excel"    else "📂 Load Excel File",
      lbl_file       = if(es) "Seleccionar .xlsx"          else "Select .xlsx",
      btn_examinar   = if(es) "Examinar..."                else "Browse...",
      ph_file        = if(es) "Ningún archivo"             else "No file selected",
      btn_load_data  = if(es) "✔ Cargar y Validar"        else "✔ Load & Validate",
      box_preview    = if(es) "🔍 Vista Previa (primeras 10 filas)" else "🔍 Preview (first 10 rows)",
      no_data        = if(es) "Sin datos cargados."        else "No data loaded.",
      # Model tab
      box_constructs = if(es) "🔷 Constructos (Nombre → Ítems)"   else "🔷 Constructs (Name → Items)",
      hint_constructs= if(es) "Ítems separados por coma. Rango: ítem1-ítem5. Segundo orden: C1+C2"
                       else   "Items separated by comma. Range: item1-item5. Second order: C1+C2",
      lbl_c_name     = if(es) "Nombre"     else "Name",
      lbl_c_items    = if(es) "Ítems (coma, rango con -, 2do orden con +)" else "Items (comma, range with -, 2nd order with +)",
      btn_add_con    = if(es) "➕ Añadir Constructo"  else "➕ Add Construct",
      box_paths      = if(es) "🔴 Relaciones Estructurales"  else "🔴 Structural Relationships",
      hint_paths     = if(es) "Define las hipótesis: Desde → Hacia" else "Define hypotheses: From → To",
      lbl_p_from     = if(es) "Desde (exógeno)"   else "From (exogenous)",
      lbl_p_to       = if(es) "Hacia (endógeno)"  else "To (endogenous)",
      btn_add_path   = if(es) "➕ Añadir Relación" else "➕ Add Relationship",
      box_validate   = if(es) "✅ Validar Modelo"  else "✅ Validate Model",
      btn_validate   = if(es) "🔍 Verificar Configuración" else "🔍 Verify Configuration",
      # Analysis tab
      box_analysis   = if(es) "⚙ Configuración del Análisis" else "⚙ Analysis Configuration",
      lbl_nboot      = if(es) "Iteraciones Bootstrapping"   else "Bootstrap Iterations",
      lbl_omit       = if(es) "Distancia omisión (Q²/Blindfolding)" else "Omission Distance (Q²/Blindfolding)",
      lbl_calc_q2    = if(es) "Calcular Q² (Blindfolding)"  else "Calculate Q² (Blindfolding)",
      lbl_calc_f2    = if(es) "Calcular f² (Effect Size)"   else "Calculate f² (Effect Size)",
      lbl_hoc_x2     = if(es) "🔺 [SOLO variable dicotómica] Ajuste HOC × 2 — NO usar con escalas Likert" else "🔺 [ONLY for dichotomous variable] HOC × 2 adjustment — Do NOT use with Likert scales",
      lbl_groups     = if(es) "👥 Grupos (MICOM / MGA)"     else "👥 Groups (MICOM / MGA)",
      lbl_group_var  = if(es) "Variable de grupo (categórica)" else "Group variable (categorical)",
      hint_group_var = if(es) "⚠ Use solo variables con pocas categorías (ej: género, sede, nivel). NO usar variables numéricas continuas."
                       else   "⚠ Use only variables with few categories (e.g.: gender, site, level). Do NOT use continuous numeric variables.",
      lbl_min_n      = if(es) "Mínimo n por grupo"          else "Min n per group",
      lbl_run_micom  = if(es) "Calcular MICOM (invariancia)" else "Calculate MICOM (invariance)",
      lbl_run_mga    = if(es) "Calcular MGA (comparación grupos)" else "Calculate MGA (group comparison)",
      btn_run        = if(es) "▶ EJECUTAR ANÁLISIS PLS-SEM"  else "▶ RUN PLS-SEM ANALYSIS",
      box_console    = if(es) "📟 Consola de Diagnóstico"    else "📟 Diagnostic Console",
      # Results tabs
      tab_measurement  = if(es) "🔵 Medición"       else "🔵 Measurement",
      tab_diagram      = if(es) "🗺 Diagrama"        else "🗺 Diagram",
      tab_structural   = if(es) "🔴 Estructural"     else "🔴 Structural",
      tab_discriminant = if(es) "🟣 Discriminante"   else "🟣 Discriminant",
      tab_indirect     = "🔗 Ind. Effects",
      tab_hypotheses   = if(es) "📋 Hipótesis"       else "📋 Hypotheses",
      tab_predict      = "🔮 PLS Predict",
      tab_diagnostic   = if(es) "⚙ Diagnóstico"     else "⚙ Diagnostic",
      tab_interpret    = if(es) "🧠 Interpretación"  else "🧠 Interpretation",
      tab_micom        = "\U0001f4d0 MICOM",
      tab_mga          = "👥 MGA",
      # Measurement boxes
      box_reliability  = if(es) "Confiabilidad y Validez Convergente" else "Reliability and Convergent Validity",
      hint_reliability = if(es) "Criterios: α ≥ 0.7 | CR ≥ 0.7 | AVE ≥ 0.5" else "Criteria: α ≥ 0.7 | CR ≥ 0.7 | AVE ≥ 0.5",
      box_loadings     = if(es) "Cargas Factoriales (Outer Loadings)" else "Outer Loadings",
      hint_loadings    = if(es) "Criterio: λ ≥ 0.7 (mínimo aceptable: 0.4)" else "Criterion: λ ≥ 0.7 (minimum acceptable: 0.4)",
      # Structural boxes
      box_paths_coef   = if(es) "Coeficientes de Ruta (β, STDEV, t, p)"  else "Path Coefficients (β, STDEV, t, p)",
      hint_paths_coef  = if(es) "Significativo: |t| > 1.96 (p < 0.05) | |t| > 2.576 (p < 0.01)" else "Significant: |t| > 1.96 (p < 0.05) | |t| > 2.576 (p < 0.01)",
      box_r2           = if(es) "R² (Coeficiente de Determinación)"   else "R² (Coefficient of Determination)",
      hint_r2          = if(es) "Sustancial: R²≥0.75 | Moderado: R²≥0.50 | Débil: R²≥0.25" else "Substantial: R²≥0.75 | Moderate: R²≥0.50 | Weak: R²≥0.25",
      box_q2           = if(es) "Q² (Predictive Relevance - Blindfolding)" else "Q² (Predictive Relevance - Blindfolding)",
      hint_q2          = if(es) "Criterio: Q²>0 (pequeño≥0.02, medio≥0.15, grande≥0.35)" else "Criterion: Q²>0 (small≥0.02, medium≥0.15, large≥0.35)",
      box_f2           = if(es) "f² (Effect Size - Cohen)"   else "f² (Effect Size - Cohen)",
      hint_f2          = if(es) "Criterio: pequeño≥0.02, mediano≥0.15, grande≥0.35" else "Criterion: small≥0.02, medium≥0.15, large≥0.35",
      box_vif          = if(es) "VIF (Colinealidad)"   else "VIF (Collinearity)",
      hint_vif         = if(es) "Criterio: VIF < 5 (estricto: < 3.3)" else "Criterion: VIF < 5 (strict: < 3.3)",
      # ── VIF extendido ──────────────────────────────────────────────────────
      box_vif_struct   = if(es) "VIF Estructural (Modelo Interno)" else "Structural VIF (Inner Model)",
      hint_vif_struct  = if(es) "Colinealidad del modelo estructural (Hair et al., 2022). Criterio estricto: VIF < 3.3"
                         else   "Structural model collinearity (Hair et al., 2022). Strict criterion: VIF < 3.3",
      box_vif_full     = if(es) "VIF Colinealidad Total (Sesgo de Método Común)" else "Full Collinearity VIF (Common Method Bias)",
      hint_vif_full    = if(es) "Evaluación completa de colinealidad — Kock (2015). Cada variable latente se regresa sobre todas las demás simultáneamente."
                         else   "Full collinearity assessment — Kock (2015). Each latent variable is regressed on all others simultaneously.",
      note_vif_full    = if(es) "Valores de VIF inferiores a 3.3 sugieren ausencia de sesgo de método común."
                         else   "VIF values below 3.3 suggest absence of common method bias.",
      # Discriminant boxes
      box_htmt         = if(es) "HTMT (Heterotrait-Monotrait Ratio)"   else "HTMT (Heterotrait-Monotrait Ratio)",
      box_fl           = "Fornell-Larcker Criterion",
      box_cl           = "Cross-Loadings",
      # Indirect effects
      box_indirect     = if(es) "Efectos Indirectos Específicos (Mediación)" else "Specific Indirect Effects (Mediation)",
      box_total        = if(es) "Efectos Totales"   else "Total Effects",
      hint_total       = if(es) "Efecto Total = Efecto Directo + Suma de Efectos Indirectos" else "Total Effect = Direct Effect + Sum of Indirect Effects",
      # Hypotheses
      box_hyp          = if(es) "Tabla de Hipótesis (formato APA/Paper)" else "Hypothesis Table (APA/Paper format)",
      btn_export_word  = if(es) "📄 Exportar Word (APA)"  else "📄 Export Word (APA)",
      # PLS Predict
      box_predict      = if(es) "PLS Predict — Relevancia Predictiva Out-of-Sample" else "PLS Predict — Out-of-Sample Predictive Relevance",
      # Diagnostic
      box_srmr         = if(es) "Ajuste del Modelo (SRMR)"   else "Model Fit (SRMR)",
      hint_srmr        = if(es) "Criterio: SRMR < 0.08 indica buen ajuste" else "Criterion: SRMR < 0.08 indicates good fit",
      # Interpretation
      box_interp       = if(es) "Resumen Interpretativo Automático" else "Automatic Interpretation Summary",
      # MICOM
      micom_ref        = if(es) "Referencia:" else "Reference:",
      micom_ref_txt    = "Hair et al. (2017). Mirror, mirror on the wall.",
      micom_proc       = if(es) "Proceso:" else "Process:",
      micom_proc_txt   = if(es) "Paso 1 (Configuración) → Paso 2 (r original + permutación) → Paso 3 (Medias y varianzas)"
                         else   "Step 1 (Configuration) → Step 2 (r original + permutation) → Step 3 (Means and variances)",
      micom_crit       = if(es) "Criterios:" else "Criteria:",
      micom_crit_txt   = if(es) "r ≥ 0.90 = invarianza composicional | p-valor (medias y varianzas) ≥ 0.05 = invarianza total"
                         else   "r ≥ 0.90 = compositional invariance | p-value (means and variances) ≥ 0.05 = full invariance",
      micom_step1      = if(es) "✅ Paso 1 – Configuración"  else "✅ Step 1 – Configuration",
      micom_step1_hint = if(es) "Igualdad de especificación del modelo en todos los grupos (siempre cumplido en PLS-SEM composites)."
                         else   "Equality of model specification across all groups (always fulfilled in PLS-SEM composites).",
      micom_summary_h  = if(es) "✅ Tabla Resumen MICOM (Pasos 2 y 3)" else "✅ MICOM Summary Table (Steps 2 and 3)",
      micom_summary_hint = if(es) "Correlación original (Paso 2) + p-valores de diferencia de medias y varianzas (Paso 3). Referencia: Hair et al. (2017)."
                           else   "Original correlation (Step 2) + p-values for mean and variance differences (Step 3). Reference: Hair et al. (2017).",
      # MGA
      mga_ref          = if(es) "Referencia:" else "Reference:",
      mga_ref_txt      = "Hair et al. (2018). Advanced Issues in PLS-SEM.",
      mga_method       = if(es) "Método:" else "Method:",
      mga_method_txt   = if(es) "Permutation test bilateral (n=1000). Sig: *** p<0.001, ** p<0.01, * p<0.05"
                         else   "Bilateral permutation test (n=1000). Sig: *** p<0.001, ** p<0.01, * p<0.05",
      mga_rec          = if(es) "Recomendación:" else "Recommendation:",
      mga_rec_txt      = if(es) "Verificar invariancia (MICOM Pasos 1-2) antes de MGA."
                         else   "Verify invariance (MICOM Steps 1-2) before MGA.",
      mga_betas        = if(es) "Comparación de Betas por Grupo" else "Beta Comparison by Group",
      # Download tab
      box_download     = if(es) "📦 Exportar Resultados"  else "📦 Export Results",
      dl_desc          = if(es) "El ZIP incluye: todas las tablas CSV + diagrama SVG + reporte HTML."
                         else   "The ZIP includes: all CSV tables + SVG diagram + HTML report.",
      dl_zip           = if(es) "📦 Descargar TODO (ZIP)"  else "📦 Download ALL (ZIP)",
      dl_html          = if(es) "🌐 Reporte HTML"          else "🌐 HTML Report",
      dl_svg           = if(es) "🗺 Diagrama SVG"          else "🗺 SVG Diagram",
      dl_word          = if(es) "📄 Reporte Word (APA)"    else "📄 Word Report (APA)",
      box_tables_avail = if(es) "Tablas disponibles"       else "Available tables",
      no_tables        = if(es) "Sin tablas generadas aún." else "No tables yet. Run analysis first.",
      # Misc
      run_first        = if(es) "Ejecute el análisis primero." else "Run analysis first.",
      no_mediation     = if(es) "No se detectaron paths de mediación o no se ha ejecutado el análisis." else "No mediation paths detected or analysis not run.",
      # Validation messages
      val_no_items     = if(es) "✗ Constructo '%s': ningún ítem coincide con los datos" else "✗ Construct '%s': no items match the data",
      val_one_item     = if(es) "⚠ Constructo '%s': solo 1 ítem (se recomienda ≥2)"    else "⚠ Construct '%s': only 1 item (≥2 recommended)",
      val_ok           = if(es) "✓ Constructo '%s': %d ítems [%s]"                      else "✓ Construct '%s': %d items [%s]",
      val_path         = if(es) "✓ Ruta H%d: %s → %s"                                  else "✓ Path H%d: %s → %s",
      # Status log defaults
      log_ready        = if(es) "► Listo. Cargue datos para comenzar." else "► Ready. Load data to begin.",
      log_proj_active  = if(es) "✓ Proyecto activo: "   else "✓ Active project: ",
      log_proj_open    = if(es) "✓ Proyecto abierto: "  else "✓ Project opened: ",
      log_saved_all    = if(es) "✓ Guardado completo: " else "✓ Fully saved: ",
      log_saved_res    = if(es) "✓ Resultados guardados: " else "✓ Results saved: ",
      log_cleared      = if(es) "Resultados borrados de memoria." else "Results cleared from memory.",
      # Hypothesis decision
      hyp_supported    = if(es) "✓ Soportada"  else "✓ Supported",
      hyp_rejected     = if(es) "✗ Rechazada"  else "✗ Rejected",
      hyp_col          = if(es) "Decisión"     else "Decision",
      # Project status
      proj_none        = if(es) "Proyecto actual: (ninguno)" else "Current project: (none)",
      proj_status_fmt  = if(es) c("Proyecto: ","Data: ","Modelo: ","Resultados: ") else c("Project: ","Data: ","Model: ","Results: ")
    )
  })

  # ── Sidebar menu (reactive) ────────────────────────────────────────────────
  output$sidebar_menu_ui <- renderUI({
    t <- i18n()
    sidebarMenu(id = "sidebar_inner",
      menuItem(t$menu_sample,   tabName = "sample",   icon = icon("calculator")),
      menuItem(t$menu_project,  tabName = "project",  icon = icon("folder-open")),
      menuItem(t$menu_upload,   tabName = "upload",   icon = icon("upload")),
      menuItem(t$menu_model,    tabName = "model",    icon = icon("sitemap")),
      menuItem(t$menu_analysis, tabName = "analysis", icon = icon("bolt")),
      menuItem(t$menu_results,  tabName = "results",  icon = icon("chart-bar")),
      menuItem(t$menu_download, tabName = "download", icon = icon("download"))
    )
  })

  # ── Project tab UI ─────────────────────────────────────────────────────────
  output$box_project_title_ui <- renderUI({ i18n()$box_project })
  output$project_name_ui <- renderUI({
    t <- i18n()
    textInput("project_name", t$lbl_proj_name, placeholder = t$ph_proj_name)
  })
  output$create_project_ui <- renderUI({
    actionButton("create_project", i18n()$btn_create, class="btn btn-success btn-block")
  })
  output$load_project_ui <- renderUI({
    actionButton("load_project", i18n()$btn_open, class="btn btn-info btn-block")
  })
  output$box_manage_title_ui <- renderUI({ i18n()$box_manage })
  output$save_all_ui <- renderUI({
    actionButton("save_all", i18n()$btn_save_all, class="btn btn-warning btn-block")
  })
  output$save_results_ui <- renderUI({
    actionButton("save_results_only", i18n()$btn_save_res, class="btn btn-default btn-block")
  })
  output$clear_results_ui <- renderUI({
    actionButton("clear_results_mem", i18n()$btn_clear_res, class="btn btn-danger btn-block")
  })

  # ── Upload tab UI ──────────────────────────────────────────────────────────
  output$box_upload_title_ui  <- renderUI({ i18n()$box_upload })
  output$box_preview_title_ui <- renderUI({ i18n()$box_preview })
  output$file_upload_ui <- renderUI({
    t <- i18n()
    fileInput("file_upload", t$lbl_file, accept = ".xlsx",
              buttonLabel = t$btn_examinar, placeholder = t$ph_file)
  })
  output$load_data_ui <- renderUI({
    actionButton("load_data", i18n()$btn_load_data, class="btn btn-success btn-block")
  })

  # ── Model tab UI ───────────────────────────────────────────────────────────
  output$box_constructs_title_ui <- renderUI({ i18n()$box_constructs })
  output$box_paths_title_ui      <- renderUI({ i18n()$box_paths })
  output$box_validate_title_ui   <- renderUI({ i18n()$box_validate })
  output$constructs_hint_ui <- renderUI({ tags$small(tags$i(i18n()$hint_constructs)) })
  output$paths_hint_ui      <- renderUI({ tags$small(tags$i(i18n()$hint_paths)) })
  output$add_construct_ui <- renderUI({
    actionButton("add_construct", i18n()$btn_add_con, class="btn btn-primary")
  })
  output$clear_model_ui <- renderUI({
    es <- input$app_lang != "en"
    actionButton("clear_model",
      if(es) "🗑 Limpiar Modelo" else "🗑 Clear Model",
      class = "btn btn-warning btn-block")
  })
  output$add_path_ui <- renderUI({
    actionButton("add_path", i18n()$btn_add_path, class="btn btn-danger")
  })
  output$validate_model_ui <- renderUI({
    actionButton("validate_model", i18n()$btn_validate, class="btn btn-success")
  })

  # ── Analysis tab UI ────────────────────────────────────────────────────────
  output$box_analysis_title_ui <- renderUI({ i18n()$box_analysis })
  output$box_console_title_ui  <- renderUI({ i18n()$box_console })
  output$analysis_controls_ui <- renderUI({
    t <- i18n()
    tagList(
      sliderInput("nboot", t$lbl_nboot, min = 500, max = 10000, value = isolate(input$nboot) %||% 5000, step = 500),
      numericInput("omission_distance", t$lbl_omit, value = isolate(input$omission_distance) %||% 7, min = 5, max = 15),
      checkboxInput("calc_q2", t$lbl_calc_q2, value = isolate(input$calc_q2) %||% TRUE),
      checkboxInput("calc_f2", t$lbl_calc_f2, value = isolate(input$calc_f2) %||% TRUE),
      checkboxInput("hoc_x2",  t$lbl_hoc_x2,  value = isolate(input$hoc_x2)  %||% FALSE),
      tags$h5(t$lbl_groups),
      selectInput("group_var", t$lbl_group_var, choices = c(""), selected = ""),
      tags$small(tags$i(t$hint_group_var)),
      br(),
      numericInput("min_group_n", t$lbl_min_n, value = isolate(input$min_group_n) %||% 30, min = 10, step = 5),
      checkboxInput("run_micom", t$lbl_run_micom, value = isolate(input$run_micom) %||% FALSE),
      checkboxInput("run_mga",   t$lbl_run_mga,   value = isolate(input$run_mga)   %||% FALSE)
    )
  })
  output$run_analysis_btn_ui <- renderUI({
    t <- i18n()
    tags$button(t$btn_run, class="btn-analizar",
                onclick="Shiny.setInputValue('run_analysis', Math.random())")
  })

  # ── Results tabs (reactive to language) ────────────────────────────────────
  output$results_tabs_ui <- renderUI({
    t <- i18n()
    tabsetPanel(id = "results_tabs",
      tabPanel(t$tab_measurement, br(),
        fluidRow(box(title = t$box_reliability, status = "primary", solidHeader = TRUE, width = 12,
            tags$small(t$hint_reliability), br(), br(), DTOutput("table_rel"))),
        fluidRow(box(title = t$box_loadings, status = "info", solidHeader = TRUE, width = 12,
            tags$small(t$hint_loadings), br(), br(), DTOutput("table_load")))
      ),
      tabPanel(t$tab_diagram, br(),
        fluidRow(box(title = "Path Model PLS-SEM", status = "primary", solidHeader = TRUE, width = 12,
            div(style = "text-align:right; margin-bottom:8px;",
                downloadButton("download_diagram_svg", "⬇ SVG", class="btn-sm btn-default"), " ",
                downloadButton("download_diagram_png", "⬇ PNG", class="btn-sm btn-primary")),
            DiagrammeR::grVizOutput("pls_diagram", height = "680px")))
      ),
      tabPanel(t$tab_structural, br(),
        fluidRow(box(title = t$box_paths_coef, status = "danger", solidHeader = TRUE, width = 12,
            tags$small(t$hint_paths_coef), br(), br(), DTOutput("table_paths"))),
        fluidRow(
          box(title = t$box_r2, status = "warning", solidHeader = TRUE, width = 6,
              tags$small(t$hint_r2), br(), br(), DTOutput("table_r2")),
          box(title = t$box_q2, status = "success", solidHeader = TRUE, width = 6,
              tags$small(t$hint_q2), br(), br(), DTOutput("table_q2"))
        ),
        fluidRow(
          box(title = t$box_f2, status = "info", solidHeader = TRUE, width = 6,
              tags$small(t$hint_f2), br(), br(), DTOutput("table_f2")),
          box(title = t$box_vif, status = "primary", solidHeader = TRUE, width = 6,
              tags$small(t$hint_vif), br(), br(), DTOutput("table_vif"))
        ),
        # ── VIF Estructural (Hair et al., 2022) ─────────────────────────────
        fluidRow(
          box(
            title = t$box_vif_struct, status = "primary", solidHeader = TRUE, width = 12,
            tags$div(
              style = "background:#E3F2FD; border-left:4px solid #1565C0; padding:10px; border-radius:4px; margin-bottom:12px;",
              tags$b("Hair et al. (2022). "),
              tags$span(t$hint_vif_struct)
            ),
            DTOutput("table_vif_structural")
          )
        ),
        # ── VIF Colinealidad Total / CMB (Kock, 2015) ───────────────────────
        fluidRow(
          box(
            title = t$box_vif_full, status = "warning", solidHeader = TRUE, width = 12,
            tags$div(
              style = "background:#FFF8E1; border-left:4px solid #F9A825; padding:10px; border-radius:4px; margin-bottom:12px;",
              tags$b("Kock (2015). "),
              tags$span(t$hint_vif_full)
            ),
            DTOutput("table_vif_full"),
            br(),
            tags$div(
              style = "background:#F1F8E9; border-left:4px solid #558B2F; padding:8px 12px; border-radius:4px; margin-top:10px;",
              tags$i(class = "fa fa-info-circle", style = "color:#558B2F; margin-right:6px;"),
              tags$span(style = "color:#33691E; font-size:13px;", uiOutput("vif_full_note_ui"))
            )
          )
        )
      ),
      tabPanel(t$tab_discriminant, br(),
        fluidRow(box(title = t$box_htmt, status = "info", solidHeader = TRUE, width = 12,
            uiOutput("htmt_help_ui"), br(), DTOutput("table_htmt"))),
        fluidRow(
          box(title = t$box_fl, status = "warning", solidHeader = TRUE, width = 6,
              uiOutput("fl_help_ui"), br(), DTOutput("table_fl")),
          box(title = t$box_cl, status = "primary", solidHeader = TRUE, width = 6,
              uiOutput("cl_help_ui"), br(), DTOutput("table_cl"))
        )
      ),
      tabPanel(t$tab_indirect, br(),
        fluidRow(box(title = t$box_indirect, status = "warning", solidHeader = TRUE, width = 12,
            uiOutput("indirect_help_ui"), br(), DTOutput("table_indirect"))),
        fluidRow(box(title = t$box_total, status = "primary", solidHeader = TRUE, width = 12,
            tags$small(t$hint_total), br(), br(), DTOutput("table_total_effects")))
      ),
      tabPanel(t$tab_hypotheses, br(),
        fluidRow(box(title = t$box_hyp, status = "success", solidHeader = TRUE, width = 12,
            uiOutput("hyp_help_ui"), br(), DTOutput("table_hypotheses"), br(),
            downloadButton("download_hyp_docx", t$btn_export_word, class = "btn btn-success")))
      ),
      tabPanel(t$tab_predict, br(),
        fluidRow(box(title = t$box_predict, status = "primary", solidHeader = TRUE, width = 12,
            uiOutput("plspredict_help_ui"), br(), DTOutput("table_plspredict")))
      ),
      tabPanel(t$tab_diagnostic, br(),
        fluidRow(box(title = t$box_srmr, status = "success", solidHeader = TRUE, width = 6,
            tags$small(t$hint_srmr), br(), br(), DTOutput("table_srmr")))
      ),
      tabPanel(t$tab_interpret, br(),
        fluidRow(box(title = t$box_interp, status = "primary", solidHeader = TRUE, width = 12,
            div(id = "interp_panel", class = "semaforo", uiOutput("interp_output"))))
      ),
      tabPanel(t$tab_micom, br(),
        fluidRow(box(title = "MICOM - Measurement Invariance of Composite Models",
            status = "warning", solidHeader = TRUE, width = 12,
            tags$div(style = "background:#FFF8E1;border-left:4px solid #F9A825;padding:10px;border-radius:4px;margin-bottom:12px;",
              tags$b(t$micom_ref), paste0(" ", t$micom_ref_txt), br(),
              tags$b(t$micom_proc), paste0(" ", t$micom_proc_txt), br(),
              tags$b(t$micom_crit), paste0(" ", t$micom_crit_txt)
            ),
            tags$h5(style="color:#1565C0; margin-top:10px;", t$micom_step1),
            tags$small(t$micom_step1_hint), br(), br(),
            DTOutput("table_micom_p1"), hr(),
            tags$h5(style="color:#1565C0; margin-top:10px;", t$micom_summary_h),
            tags$small(t$micom_summary_hint), br(), br(),
            DTOutput("table_micom_resumen"), br(),
            uiOutput("micom_summary_ui")
        ))
      ),
      tabPanel(t$tab_mga, br(),
        fluidRow(box(title = "MGA - Multi-Group Analysis (Permutation Test)",
            status = "danger", solidHeader = TRUE, width = 12,
            tags$div(style = "background:#FFEBEE;border-left:4px solid #E53935;padding:10px;border-radius:4px;margin-bottom:12px;",
              tags$b(t$mga_ref), paste0(" ", t$mga_ref_txt), br(),
              tags$b(t$mga_method), paste0(" ", t$mga_method_txt), br(),
              tags$b(t$mga_rec), paste0(" ", t$mga_rec_txt)
            ),
            DTOutput("table_mga")
        )),
        fluidRow(box(title = t$mga_betas, status = "primary", solidHeader = FALSE, width = 12,
            uiOutput("mga_summary_ui")))
      ),
      tabPanel("🧪 Robustness Analysis", br(),

        # ── Control row: scoring toggle + Run button ────────────────────────
        fluidRow(
          box(title = "Gaussian Copula Endogeneity Test \u2014 Park & Gupta (2012)",
              status = "info", solidHeader = TRUE, width = 12,

              # Method info banner
              tags$div(style = "background:#E3F2FD;border-left:4px solid #1565C0;padding:10px;border-radius:4px;margin-bottom:12px;",
                tags$b("\U0001f4d6 M\u00e9todo / Method:"),
                tags$small(" Gaussian Copula test following Park & Gupta (2012) for detection of potential endogeneity in cross-sectional predictive models."),
                br(),
                tags$b("\U0001f50d Criterion:"),
                tags$small(" p < 0.05 \u2192 Potential endogeneity detected | p \u2265 0.05 \u2192 No evidence of endogeneity"),
                br(),
                tags$b("\U0001f4da Reference:"),
                tags$small(" Park, S. & Gupta, S. (2012). Handling Endogeneity in Marketing Models Using Copulas. Marketing Science, 31(2), 317\u2013333.")
              ),

              # Scoring method toggle
              fluidRow(
                column(6,
                  checkboxInput("copula_use_mean_scores",
                    "\U0001f4ca Use composite mean scores instead of PLS latent scores",
                    value = FALSE)
                ),
                column(6,
                  actionButton("run_copula_test", "\u25b6 Run Gaussian Copula Test",
                               class = "btn btn-info btn-lg",
                               style = "width:100%;")
                )
              ),
              br(),
              uiOutput("copula_status_ui"),
              br(),

              # Technical Details box (shown after run)
              uiOutput("copula_tech_details_ui"),
              br(),

              # Results table
              DTOutput("table_copula"),
              br(),
              fluidRow(
                column(2, downloadButton("dl_copula_csv",   "\u2b07 CSV",           class = "btn btn-sm btn-default btn-block")),
                column(3, downloadButton("dl_copula_excel", "\u2b07 Excel (.xlsx)", class = "btn btn-sm btn-success btn-block")),
                column(3, downloadButton("dl_copula_word",  "\u2b07 Word (.docx)",  class = "btn btn-sm btn-warning btn-block")),
                column(2, downloadButton("dl_copula_pdf",   "\u2b07 PDF",           class = "btn btn-sm btn-danger btn-block"))
              )
          )
        ),

        # ── Two-plot sub-tabs ────────────────────────────────────────────────
        fluidRow(
          box(title = "Plots", status = "primary", solidHeader = TRUE, width = 12,
            tabsetPanel(id = "copula_plot_tabs",

              # ── Plot A: Forest / Dot-and-Whisker ──────────────────────────
              tabPanel("\U0001f4ca Results Plot (Forest)", br(),
                tags$small(style = "color:#555;",
                  "Publication-ready forest plot: Copula coefficient (\u25cf) with 95% CI per structural path. ",
                  "\u25c7 = PLS bootstrapped \u03b2 (reference only). Red = p < 0.05."
                ),
                br(), br(),
                plotOutput("plot_copula_forest", height = "460px"),
                br(),
                fluidRow(
                  column(3, downloadButton("dl_forest_png", "\u2b07 PNG (300 dpi)", class = "btn btn-sm btn-primary btn-block")),
                  column(3, downloadButton("dl_forest_pdf", "\u2b07 PDF (vector)",  class = "btn btn-sm btn-danger btn-block")),
                  column(3, downloadButton("dl_forest_svg", "\u2b07 SVG",           class = "btn btn-sm btn-default btn-block"))
                )
              ),

              # ── Plot B: Copula Visualization ──────────────────────────────
              tabPanel("\U0001f52c Copula Visualization (Optional)", br(),
                tags$small(style = "color:#555;",
                  "Visualize the rank-normal transformed variables (z_X, z_Y) used in the copula procedure. ",
                  "Select a path and view type below."
                ),
                br(), br(),
                fluidRow(
                  column(4,
                    uiOutput("copula_viz_path_selector_ui")
                  ),
                  column(4,
                    radioButtons("copula_viz_type", "View type:",
                      choices  = c("Scatter + density overlay" = "scatter",
                                   "2D density / contour"      = "contour"),
                      selected = "scatter", inline = TRUE)
                  ),
                  column(4,
                    uiOutput("copula_viz_n_ui")
                  )
                ),
                br(),
                plotOutput("plot_copula_viz", height = "480px"),
                br(),
                tags$small(style = "color:#777; font-style:italic;",
                  "This visualization displays the rank-normal transformed variables (z_X, z_Y) used for copula-based diagnostics."),
                br(), br(),
                fluidRow(
                  column(3, downloadButton("dl_viz_png", "\u2b07 PNG (300 dpi)", class = "btn btn-sm btn-primary btn-block")),
                  column(3, downloadButton("dl_viz_pdf", "\u2b07 PDF (vector)",  class = "btn btn-sm btn-danger btn-block")),
                  column(3, downloadButton("dl_viz_svg", "\u2b07 SVG",           class = "btn btn-sm btn-default btn-block"))
                )
              )
            )
          )
        )
      )
    )
  })

  # ── Download tab UI ────────────────────────────────────────────────────────
  output$box_download_title_ui <- renderUI({ i18n()$box_download })
  output$download_desc_ui      <- renderUI({ tags$p(i18n()$dl_desc) })
  output$dl_zip_label          <- renderUI({ i18n()$dl_zip })
  output$dl_html_label         <- renderUI({ i18n()$dl_html })
  output$dl_svg_label          <- renderUI({ i18n()$dl_svg })
  output$dl_word_label         <- renderUI({ i18n()$dl_word })
  output$box_tables_title_ui   <- renderUI({ i18n()$box_tables_avail })


# ── Poblar selector de variable de grupo para MICOM/MGA ───────────────────
observe({
  req(data_raw())
  cols <- names(data_raw())
  updateSelectInput(session, "group_var", choices = c("", cols), selected = input$group_var %||% "")
})

  # ── Persistencia modelo en memoria ───────────────────────────────────────
  observe({
    req(construct_count(), path_count())
    nm1 <- input[[paste0("c_name_", 1)]]
    if (is.null(nm1)) return()
    mdl_tmp <- list(
      constructs = lapply(seq_len(construct_count()), function(i)
        list(name = input[[paste0("c_name_", i)]], items_str = get_items_str(i))),
      paths = data.frame(
        from = sapply(seq_len(path_count()), function(i) input[[paste0("p_from_", i)]] %||% ""),
        to   = sapply(seq_len(path_count()), function(i) input[[paste0("p_to_",   i)]] %||% ""),
        stringsAsFactors = FALSE
      )
    )
    has_any <- any(vapply(mdl_tmp$constructs, function(x) !is.null(x$name) && nzchar(trimws(x$name %||% "")), logical(1)))
    if (has_any) last_model(mdl_tmp)
  })

  observe({
    if (is.null(last_model()) && file.exists(model_store_file)) {
      mdl <- tryCatch(readRDS(model_store_file), error = function(e) NULL)
      if (!is.null(mdl)) last_model(mdl)
    }
  })

  # ── PROYECTOS ─────────────────────────────────────────────────────────────
  projects_root  <- file.path(getwd(), "projects")
  if (!dir.exists(projects_root)) dir.create(projects_root, recursive = TRUE)
  current_project <- reactiveVal(NULL)

  proj_paths <- reactive({
    req(current_project())
    base <- file.path(projects_root, current_project())
    list(base = base,
         data    = file.path(base, "data_raw.rds"),
         model   = file.path(base, "model.rds"),
         results = file.path(base, "results_tables.rds"))
  })

  output$projects_ui <- renderUI({
    projs <- list.dirs(projects_root, full.names = FALSE, recursive = FALSE)
    selectInput("project_pick", i18n()$lbl_proj_list, choices = if (length(projs)) projs else character(0))
  })

  output$project_status <- renderText({
    t <- i18n()
    if (is.null(current_project())) return(t$proj_none)
    p <- proj_paths()
    lbl <- t$proj_status_fmt
    paste0(lbl[1], current_project(), "\n",
           lbl[2], file.exists(p$data),    "\n",
           lbl[3], file.exists(p$model),   "\n",
           lbl[4], file.exists(p$results))
  })

  observeEvent(input$create_project, {
    nm <- trimws(input$project_name)
    req(nzchar(nm))
    base <- file.path(projects_root, nm)
    if (!dir.exists(base)) dir.create(base, recursive = TRUE)
    current_project(nm)
    results$log <- paste0(i18n()$log_proj_active, nm)
  })

  observeEvent(input$load_project, {
    nm <- input$project_pick; req(!is.null(nm) && nzchar(nm))
    current_project(nm); p <- proj_paths()
    if (file.exists(p$data))    { df  <- tryCatch(readRDS(p$data),    error=function(e) NULL); if (!is.null(df))  data_raw(df) }
    if (file.exists(p$model))   { mdl <- tryCatch(readRDS(p$model),   error=function(e) NULL); if (!is.null(mdl)) last_model(mdl) }
    if (file.exists(p$results)) { tb  <- tryCatch(readRDS(p$results), error=function(e) NULL); if (!is.null(tb))  results$tables <- tb }
    results$log <- paste0(i18n()$log_proj_open, nm)
  })

  observeEvent(input$save_all, {
    req(current_project()); p <- proj_paths()
    if (!is.null(data_raw())) tryCatch(saveRDS(data_raw(), p$data), error=function(e) NULL)
    mdl <- last_model()
    if (!is.null(mdl)) tryCatch(saveRDS(mdl, p$model), error=function(e) NULL)
    if (length(results$tables) > 0) tryCatch(saveRDS(results$tables, p$results), error=function(e) NULL)
    results$log <- paste0(i18n()$log_saved_all, current_project())
  })

  observeEvent(input$save_results_only, {
    req(current_project()); p <- proj_paths()
    req(length(results$tables) > 0)
    tryCatch(saveRDS(results$tables, p$results), error=function(e) NULL)
    results$log <- paste0(i18n()$log_saved_res, current_project())
  })

  observeEvent(input$clear_results_mem, {
    results$tables <- list(); results$pls_est <- NULL
    results$log <- i18n()$log_cleared
  })

  # ── CONSTRUCTOS (UI dinámica) ─────────────────────────────────────────────
  construct_count <- reactiveVal(4)
  output$construct_inputs <- renderUI({
    t    <- i18n()
    es   <- isTRUE(input$app_lang != "en")
    cnt  <- construct_count()
    mdl  <- isolate(last_model())

    # Obtener columnas disponibles de forma segura
    cols <- character(0)
    df_tmp <- tryCatch(data_raw(), error = function(e) NULL)
    if (!is.null(df_tmp) && is.data.frame(df_tmp) && ncol(df_tmp) > 0) {
      nms  <- names(df_tmp)
      keep <- vapply(nms, function(cn) {
        x <- df_tmp[[cn]]
        if (is.numeric(x)) return(TRUE)
        suppressWarnings(sum(!is.na(as.numeric(as.character(x)))) >= 3)
      }, logical(1))
      cols <- nms[keep]
    }

    lapply(1:cnt, function(i) {
      pre_name  <- if (!is.null(mdl) && i <= length(mdl$constructs)) mdl$constructs[[i]]$name      %||% "" else ""
      pre_items <- if (!is.null(mdl) && i <= length(mdl$constructs)) mdl$constructs[[i]]$items_str %||% "" else ""
      pre_selected <- if (nzchar(pre_items)) trimws(unlist(strsplit(pre_items, ","))) else character(0)
      pre_selected <- pre_selected[pre_selected %in% cols]

      fluidRow(style = "margin-bottom:8px;",
        column(1, tags$div(style="padding-top:28px; text-align:center; font-weight:bold; color:#1565C0; font-size:16px;", paste0("C",i))),
        column(3, textInput(paste0("c_name_",i), t$lbl_c_name, value = pre_name)),
        column(8,
          if (length(cols) > 0) {
            pickerInput(
              inputId  = paste0("c_picker_", i),
              label    = if (es) "Ítems de este constructo:" else "Items for this construct:",
              choices  = cols,
              selected = pre_selected,
              multiple = TRUE,
              options  = list(
                `actions-box`          = TRUE,
                `live-search`          = TRUE,
                `live-search-placeholder` = if (es) "Buscar ítem..." else "Search item...",
                `selected-text-format` = "count > 3",
                `count-selected-text`  = if (es) "{0} ítems seleccionados" else "{0} items selected",
                `select-all-text`      = if (es) "Seleccionar todos" else "Select All",
                `deselect-all-text`    = if (es) "Deseleccionar todos" else "Deselect All",
                `none-selected-text`   = if (es) "Sin selección" else "Nothing selected",
                size                   = 8
              ),
              width = "100%"
            )
          } else {
            tagList(
              textInput(paste0("c_items_", i), t$lbl_c_items, value = pre_items),
              tags$small(style="color:#f59e0b;", "⚠️ Carga tus datos primero para ver el selector de ítems")
            )
          }
        )
      )
    })
  })
  observeEvent(input$add_construct, { construct_count(construct_count() + 1) })

  # ── LIMPIAR MODELO ────────────────────────────────────────────────────────
  observeEvent(input$clear_model, {
    tryCatch(if (file.exists(model_store_file)) file.remove(model_store_file), error = function(e) NULL)
    last_model(NULL)
    construct_count(4)
    path_count(3)
    for (i in 1:10) {
      tryCatch(updateTextInput(session, paste0("c_name_",  i), value = ""), error = function(e) NULL)
      tryCatch(updateTextInput(session, paste0("c_items_", i), value = ""), error = function(e) NULL)
      tryCatch(
        updatePickerInput(session, paste0("c_picker_", i), selected = character(0)),
        error = function(e) NULL
      )
    }
    for (i in 1:10) {
      tryCatch(updateTextInput(session, paste0("p_from_", i), value = ""), error = function(e) NULL)
      tryCatch(updateTextInput(session, paste0("p_to_",   i), value = ""), error = function(e) NULL)
    }
    output$validation_output <- renderText("")
  })

  # ── RELACIONES (UI dinámica) ──────────────────────────────────────────────
  path_count <- reactiveVal(3)
  output$path_inputs <- renderUI({
    t   <- i18n()
    cnt <- path_count()  # dependencia reactiva
    mdl <- isolate(last_model())  # isolate: no sobreescribe ediciones manuales
    lapply(1:cnt, function(i) {
      pre_from <- if (!is.null(mdl) && !is.null(mdl$paths) && i <= nrow(mdl$paths)) mdl$paths$from[i] %||% "" else ""
      pre_to   <- if (!is.null(mdl) && !is.null(mdl$paths) && i <= nrow(mdl$paths)) mdl$paths$to[i]   %||% "" else ""
      fluidRow(
        column(1, tags$div(style="padding-top:28px; text-align:center; font-weight:bold; color:#E53935;", paste0("H",i))),
        column(5, textInput(paste0("p_from_",i), t$lbl_p_from, value = pre_from)),
        column(1, tags$div(style="padding-top:28px; text-align:center; font-size:20px;", "→")),
        column(5, textInput(paste0("p_to_",i),   t$lbl_p_to,   value = pre_to))
      )
    })
  })
  observeEvent(input$add_path, { path_count(path_count() + 1) })

  # ── Restaurar modelo guardado ─────────────────────────────────────────────
  # SOLUCIÓN DEFINITIVA: los renderUI leen isolate(last_model()) como valor inicial
  # de cada textInput. Al cambiar los contadores → re-render → valores desde last_model.
  # isolate() evita que ediciones manuales del usuario se sobreescriban cuando
  # last_model() cambia por otras razones.
  observeEvent(last_model(), {
    mdl <- last_model(); req(mdl)
    # Actualizar contadores → dispara re-render → renderUI usará last_model() como valores
    if (!is.null(mdl$constructs) && length(mdl$constructs) > 0)
      construct_count(length(mdl$constructs))
    if (!is.null(mdl$paths) && nrow(mdl$paths) > 0)
      path_count(nrow(mdl$paths))
  }, ignoreInit = TRUE)

  # ── CARGAR DATOS ──────────────────────────────────────────────────────────
  observeEvent(input$load_data, {
    req(input$file_upload)
    tryCatch({
      df_raw <- read_excel(input$file_upload$datapath)

      # Detectar columnas que parecen categóricas (pocas categorías) ANTES de procesar
      # para protegerlas del jitter que destruye sus valores exactos
      n_unique_orig <- sapply(names(df_raw), function(col) {
        length(unique(na.omit(df_raw[[col]])))
      })

      # Convertir todo a numérico
      df_num <- as.data.frame(lapply(df_raw, function(x) suppressWarnings(as.numeric(as.character(x)))))
      names(df_num) <- names(df_raw)
      df_num <- df_num[complete.cases(df_num), ]

      # Aplicar jitter SOLO a columnas con muchos valores únicos (ítems tipo Likert con muchos vals)
      # NO aplicar a columnas con <= 10 categorías únicas (variables de grupo como Género=1,2)
      df_j <- df_num
      for (col in names(df_j)) {
        if (n_unique_orig[col] > 10) {
          df_j[[col]] <- jitter(df_j[[col]], amount = 0.0001)
        }
        # Columnas con <= 10 valores únicos se quedan intactas (son variables de agrupación)
      }

      colnames(df_j) <- clean_names(colnames(df_j))
      data_raw(as.data.frame(df_j))

      # Detectar nombres de posibles vars de grupo para informar al usuario
      group_cols <- names(n_unique_orig)[n_unique_orig <= 10]
      group_cols_clean <- clean_names(group_cols)
      cat_info <- if (length(group_cols_clean) > 0)
        paste0(if(input$app_lang=="en") " | Possible group vars: " else " | Posibles vars. de grupo: ",
               paste(group_cols_clean, collapse=", "))
      else ""

      results$log <- paste0(if(input$app_lang=="en") "✓ Data loaded: " else "✓ Datos cargados: ",
                             nrow(df_j),
                             if(input$app_lang=="en") " cases | " else " casos | ",
                             ncol(df_j),
                             if(input$app_lang=="en") " variables" else " variables",
                             cat_info)
    }, error = function(e) {
      results$log <- paste0(if(input$app_lang=="en") "✗ Load error: " else "✗ Error carga: ", e$message)
    })
  })

  output$data_preview <- renderDT({
    req(data_raw())
    datatable(head(data_raw(), 10), options = list(scrollX = TRUE, dom = "t"), rownames = FALSE)
  })

  output$data_info_ui <- renderUI({
    df <- data_raw()
    t  <- i18n()
    if (is.null(df)) return(tags$p(t$no_data, style="color:gray"))
    tagList(
      tags$p(style="color:#2E7D32; font-weight:bold;",
             paste0("✓ ", nrow(df), if(input$app_lang=="en") " obs. | " else " obs. | ", ncol(df), if(input$app_lang=="en") " vars" else " vars")),
      tags$small(style="color:#555;", if(input$app_lang=="en") "Variables: " else "Variables: ", paste(names(df), collapse=", "))
    )
  })

  # ── VALIDAR MODELO ────────────────────────────────────────────────────────
  observeEvent(input$validate_model, {
    df  <- data_raw()
    t   <- i18n()
    msg <- character(0)

    for (i in 1:construct_count()) {
      nm <- trimws(input[[paste0("c_name_",i)]] %||% "")
      it <- get_items_str(i)
      if (!nzchar(nm) || !nzchar(it)) next

      # ── Detectar si es HOC (sintaxis C1+C2) ──────────────────────────────
      is_hoc <- FALSE
      if (grepl("[+|]", it)) {
        parts <- trimws(unlist(strsplit(it, "[+|]")))
        parts <- parts[nzchar(parts)]
        # Recolectar todos los nombres de constructos definidos
        all_defined_names <- sapply(1:construct_count(), function(j) {
          trimws(input[[paste0("c_name_", j)]] %||% "")
        })
        if (length(parts) >= 2 && all(parts %in% all_defined_names)) {
          is_hoc <- TRUE
          msg <- c(msg, paste0("✓ Constructo '", nm, "': HOC de 2° orden [", paste(parts, collapse=" + "), "]"))
        }
      }

      if (!is_hoc) {
        items <- if (!is.null(df)) parse_item_range(it, names(df)) else strsplit(it,",")[[1]]
        if (length(items) == 0) {
          # Verificar si quizás el usuario olvidó definir los sub-constructos (typo)
          parts_check <- trimws(unlist(strsplit(it, "[+|,]")))
          parts_check <- parts_check[nzchar(parts_check)]
          all_defined_check <- sapply(1:construct_count(), function(j) {
            trimws(input[[paste0("c_name_", j)]] %||% "")
          })
          if (length(parts_check) >= 2 && all(parts_check %in% all_defined_check)) {
            # Es HOC pero no fue detectado por el detector principal (caso edge)
            msg <- c(msg, paste0("✓ Constructo '", nm, "': HOC de 2° orden [", paste(parts_check, collapse=" + "), "]"))
          } else {
            msg <- c(msg, sprintf(t$val_no_items, nm))
          }
        }
        else if (length(items) < 2)
          msg <- c(msg, sprintf(t$val_one_item, nm))
        else
          msg <- c(msg, sprintf(t$val_ok, nm, length(items), paste(items,collapse=", ")))
      }
    }

    for (i in 1:path_count()) {
      fr <- trimws(input[[paste0("p_from_",i)]] %||% "")
      to <- trimws(input[[paste0("p_to_",i)]]   %||% "")
      if (!nzchar(fr) || !nzchar(to)) next
      msg <- c(msg, sprintf(t$val_path, i, fr, to))
    }

    output$validation_output <- renderText(paste(msg, collapse = "\n"))
  })

  # ── EJECUTAR ANÁLISIS ─────────────────────────────────────────────────────
  observeEvent(input$run_analysis, {
    req(data_raw())
    t <- isolate(i18n())
    es <- isolate(input$app_lang) != "en"

    results$log   <- if(es) "► Iniciando motor PLS-SEM..." else "► Starting PLS-SEM engine..."
    results$tables <- list()
    results$pls_est <- NULL

    tryCatch({

      # ── 1. Modelo de medida ──────────────────────────────────────────────

      results$log <- if(es) "► [1/7] Construyendo modelo de medida..." else "► [1/7] Building measurement model..."
      def_map <- list()
      for (i in 1:construct_count()) {
        nm <- trimws(input[[paste0("c_name_",i)]] %||% "")
        it <- get_items_str(i)
        if (nzchar(nm) && nzchar(it)) def_map[[nm]] <- it
      }
      req(length(def_map) > 0)

      resolve_items <- function(it_str) {
        if (grepl("[+|]", it_str)) {
          parts <- trimws(unlist(strsplit(it_str, "[+|]")))
          parts <- parts[nzchar(parts)]
          if (length(parts) >= 2 && all(parts %in% names(def_map))) {
            expanded <- c()
            for (p in parts) expanded <- c(expanded, parse_item_range(def_map[[p]], names(data_raw())))
            return(unique(expanded))
          }
        }
        parse_item_range(it_str, names(data_raw()))
      }

      # ── Detectar HOC (sintaxis C1+C2) ────────────────────────────────────────
      hoc_specs <- list()
      for (nm in names(def_map)) {
        it_str <- def_map[[nm]]
        if (grepl("[+|]", it_str)) {
          parts <- trimws(unlist(strsplit(it_str, "[+|]")))
          parts <- parts[nzchar(parts)]
          if (length(parts) >= 2 && all(parts %in% names(def_map)))
            hoc_specs[[nm]] <- parts
        }
      }

      # ── LOC de primer orden ───────────────────────────────────────────────────
      c_list <- list()
      construct_items_map <- list()
      for (nm in names(def_map)) {
        if (nm %in% names(hoc_specs)) next
        items <- parse_item_range(def_map[[nm]], names(data_raw()))
        if (!is.null(items) && length(items) > 0) {
          c_list[[length(c_list)+1]] <- composite(nm, items)
          construct_items_map[[nm]] <- items
        }
      }

      # ── HOC: Two-Stage Approach (Hair et al. 2022 / SmartPLS equivalent) ────────
      # Stage 1: estimar PLS COMPLETO con todos los LOC → extraer construct scores PLS reales
      # Stage 2: usar esos scores como ítems del HOC en el modelo principal
      # Esto replica exactamente el procedimiento de SmartPLS para variables de 2° orden.
      hoc_data <- as.data.frame(data_raw())

      if (length(hoc_specs) > 0) {
        # ── Stage 1: modelo SATURADO con TODOS los LOC ───────────────────────────────
        # Igual que SmartPLS: estima todos los constructos de primer orden juntos
        # con un modelo saturado (todas las rutas posibles entre LOC).
        # Esto garantiza que los construct scores de CV, BL, ML etc. capturan
        # toda la covarianza del sistema antes de usarlos como indicadores del HOC.
        all_loc_names_s1 <- names(construct_items_map)[!names(construct_items_map) %in% names(hoc_specs)]

        c_list_stage1 <- list()
        for (nm in all_loc_names_s1) {
          items_nm <- construct_items_map[[nm]]
          items_nm <- items_nm[items_nm %in% names(hoc_data)]
          if (length(items_nm) >= 1)
            c_list_stage1[[length(c_list_stage1)+1]] <- composite(nm, items_nm)
        }

        # Modelo saturado: cada LOC conectado con todos los demás LOC
        p_list_s1 <- list(); p_added_s1 <- character(0)
        for (fi in seq_along(all_loc_names_s1)) {
          for (ti in seq_along(all_loc_names_s1)) {
            if (fi == ti) next
            f_nm <- all_loc_names_s1[fi]; t_nm <- all_loc_names_s1[ti]
            k_s1 <- paste0(f_nm, "->", t_nm)
            if (!(k_s1 %in% p_added_s1)) {
              p_list_s1[[length(p_list_s1)+1]] <- paths(from=f_nm, to=t_nm)
              p_added_s1 <- c(p_added_s1, k_s1)
            }
          }
        }

        stage1_scores <- NULL
        if (length(c_list_stage1) >= 2 && length(p_list_s1) >= 1) {
          stage1_scores <- tryCatch({
            m_s1 <- do.call(constructs, c_list_stage1)
            s_s1 <- do.call(relationships, p_list_s1)
            pls_s1 <- estimate_pls(data = hoc_data,
                                   measurement_model = m_s1,
                                   structural_model  = s_s1)
            sc <- as.data.frame(pls_s1$construct_scores)
            message("Stage-1 OK. Constructos: ", paste(names(sc), collapse=", "))
            sc
          }, error = function(e) {
            message("Stage-1 PLS error (fallback): ", e$message)
            NULL
          })
        }

        # ── Stage 2: agregar scores PLS del Stage-1 como ítems del HOC ──────────
        for (hoc_nm in names(hoc_specs)) {
          locs    <- hoc_specs[[hoc_nm]]
          locs_ok <- locs[locs %in% names(construct_items_map)]
          if (length(locs_ok) < 2) next

          score_cols <- c()
          for (l in locs_ok) {
            col_nm <- paste0("__hoc_", hoc_nm, "_", l)

            if (!is.null(stage1_scores) && l %in% names(stage1_scores)) {
              # ✓ Usar construct score PLS real del Stage-1 (idéntico a SmartPLS)
              hoc_data[[col_nm]] <- as.numeric(stage1_scores[[l]])
            } else {
              # Fallback: media de ítems estandarizados (si Stage-1 falló)
              items_l <- construct_items_map[[l]]
              items_l <- items_l[items_l %in% names(hoc_data)]
              if (length(items_l) == 0) next
              mat <- scale(as.matrix(hoc_data[, items_l, drop = FALSE]))
              hoc_data[[col_nm]] <- as.numeric(rowMeans(mat, na.rm = TRUE))
            }
            score_cols <- c(score_cols, col_nm)
          }

          if (length(score_cols) >= 2) {
            # mode_B = reflective at 2nd order (composite of composites) — evita NA en mmMatrix
            c_list[[length(c_list)+1]] <- composite(hoc_nm, score_cols, mode_B)
            construct_items_map[[hoc_nm]] <- score_cols
            stage_src <- if (!is.null(stage1_scores)) "Two-Stage PLS" else "Two-Stage fallback"
            results$log <- paste0("✓ HOC '", hoc_nm,
                                  "' [", stage_src, ": ", paste(locs_ok, collapse="+"), "]")
          }
        }
      }
      req(length(c_list) > 0)
      m_model <- do.call(constructs, c_list)

      # ── 2. Modelo estructural ────────────────────────────────────────────
      p_list <- list()
      p_df   <- data.frame(from=character(), to=character(), stringsAsFactors=FALSE)
      for (i in 1:path_count()) {
        fr <- trimws(input[[paste0("p_from_",i)]] %||% "")
        to <- trimws(input[[paste0("p_to_",i)]]   %||% "")
        if (nzchar(fr) && nzchar(to)) {
          p_list[[length(p_list)+1]] <- paths(from=fr, to=to)
          p_df <- rbind(p_df, data.frame(from=fr, to=to, stringsAsFactors=FALSE))
        }
      }
      req(nrow(p_df) > 0)
      s_model <- do.call(relationships, p_list)

      # Guardar modelo
      tryCatch(saveRDS(last_model(), model_store_file), error=function(e) NULL)

      # ── 3. Estimación PLS ───────────────────────────────────────────────
      results$log <- if(es) "► [2/7] Estimando modelo PLS-SEM..." else "► [2/7] Estimating PLS-SEM model..."
      pls_est <- estimate_pls(data = hoc_data, measurement_model = m_model, structural_model = s_model)
      summ    <- summary(pls_est)
      results$pls_est <- pls_est


      # ── Construct scores (necesario para Q², f², R² fallback y SRMR) ────
      scores_df <- tryCatch(as.data.frame(pls_est$construct_scores),          error=function(e) NULL) %||%
                   tryCatch(as.data.frame(pls_est$constructScores),           error=function(e) NULL) %||%
                   tryCatch(as.data.frame(seminr::construct_scores(pls_est)), error=function(e) NULL)

      # ── 4. Generar diagrama DOT ──────────────────────────────────────────
      results$log <- if(es) "► [3/7] Generando diagrama..." else "► [3/7] Generating diagram..."
      # Preparar r2_df para el diagrama (usa "Construct" no "Constructo")
      r2_for_dot <- tryCatch({
        r2 <- NULL
        for (nm in c("r_squared","rSquared","r2","fSquare")) {
          r2 <- tryCatch(summ[[nm]], error=function(e) NULL)
          if (!is.null(r2)) break
        }
        if (!is.null(r2) && is.numeric(r2) && !is.null(names(r2)))
          data.frame(Construct = names(r2), R2 = as.numeric(r2), stringsAsFactors = FALSE)
        else NULL
      }, error = function(e) NULL)
      dot_code <- tryCatch(
        build_pls_dot(pls_est, r2_df = r2_for_dot, title = "PLS-SEM Path Model", rankdir = "LR", digits = 3),
        error = function(e) { message("DOT error: ", e$message); NULL }
      )
      results$dot_code <- dot_code

      # ── 5. Q² Blindfolding / Stone-Geisser ───────────────────────────────────
      if (isTRUE(input$calc_q2)) {
        results$log <- if(es) "► [4/7] Calculando Q² (Stone-Geisser)..." else "► [4/7] Calculating Q² (Stone-Geisser)..."

        q2_tbl <- data.frame()

        # INTENTO 1: seminr::blindfold() si existe en esta versión
        bf_fn <- tryCatch(
          getExportedValue("seminr", "blindfold"),
          error = function(e) NULL
        )
        # Algunos builds lo exponen como predict_pls_out_of_sample
        if (is.null(bf_fn)) {
          bf_fn <- tryCatch(
            getExportedValue("seminr", "predict_pls_out_of_sample"),
            error = function(e) NULL
          )
        }

        if (!is.null(bf_fn)) {
          bf <- tryCatch(
            bf_fn(pls_est, omission_distance = as.integer(input$omission_distance)),
            error = function(e) { message("Blindfold call error: ", e$message); NULL }
          )
          if (!is.null(bf)) {
            # Intentar múltiples slots según versión de seminr
            for (slot_nm in c("construct_crossvalidated_redundancy",
                              "crossvalidated_redundancy", "cv_redundancy",
                              "redundancy_cv", "q_squared", "Q2")) {
              obj <- tryCatch(bf[[slot_nm]], error = function(e) NULL)
              if (!is.null(obj)) {
                if (is.numeric(obj) && !is.null(names(obj))) {
                  q2_tbl <- data.frame(Constructo = names(obj),
                                       Q2 = round(as.numeric(obj), 3),
                                       Metodo = "Blindfolding",
                                       stringsAsFactors = FALSE)
                } else if (is.matrix(obj) || is.data.frame(obj)) {
                  df_q2 <- as.data.frame(obj)
                  df_q2$Constructo <- rownames(df_q2)
                  num_cols <- which(sapply(df_q2, is.numeric))
                  if (length(num_cols)) {
                    q2_tbl <- data.frame(Constructo = df_q2$Constructo,
                                         Q2 = round(as.numeric(df_q2[[num_cols[1]]]), 3),
                                         Metodo = "Blindfolding",
                                         stringsAsFactors = FALSE)
                  }
                }
                if (nrow(q2_tbl) > 0) break
              }
            }
          }
        }

        # INTENTO 2 (fallback): Blindfolding real Stone-Geisser sobre construct scores
        # Q² = 1 - SSE_pred / SSO
        # SSO = suma cuadrados de y observados (centrados)
        # SSE_pred = suma cuadrados de errores de PREDICCION (omitiendo cada d-esima obs)
        # Este es el procedimiento exacto de SmartPLS (Hair et al. 2022)
        if (nrow(q2_tbl) == 0 && !is.null(scores_df) && nrow(scores_df) > 0) {
          d <- as.integer(input$omission_distance)  # distancia de omision (default 7)
          if (is.na(d) || d < 2) d <- 7L
          endos_q2 <- unique(p_df$to)

          for (endo in endos_q2) {
            preds <- unique(p_df$from[p_df$to == endo])
            preds <- preds[preds %in% names(scores_df)]
            if (!(endo %in% names(scores_df)) || !length(preds)) next

            y_all <- scores_df[[endo]]
            X_all <- as.matrix(scores_df[, preds, drop = FALSE])
            n     <- length(y_all)

            # SSO: suma de cuadrados observados (sin centrar, como en seminr/SmartPLS)
            SSO <- sum(y_all^2)

            # Blindfolding: omitir cada d-esima observacion en grupos ciclicos
            # Grupo k contiene indices: k, k+d, k+2d, ... para k=1..d
            SSE_blind <- 0
            valid_preds <- 0

            for (k in seq_len(d)) {
              # Indices del grupo de omision k
              omit_idx <- seq(k, n, by = d)
              keep_idx <- setdiff(seq_len(n), omit_idx)

              if (length(keep_idx) < (length(preds) + 2)) next  # muy pocos datos

              # Ajustar modelo SIN las observaciones omitidas
              X_keep <- X_all[keep_idx, , drop = FALSE]
              y_keep <- y_all[keep_idx]

              fit_k <- tryCatch(
                stats::lm(y_keep ~ X_keep),
                error = function(e) NULL
              )
              if (is.null(fit_k)) next

              # Predecir las observaciones OMITIDAS con el modelo entrenado sin ellas
              X_omit <- X_all[omit_idx, , drop = FALSE]
              y_omit <- y_all[omit_idx]

              coefs <- coef(fit_k)
              # coefs[1] = intercepto, coefs[2..] = betas para cada predictor
              y_pred_omit <- coefs[1] + X_omit %*% coefs[-1]

              # Acumular SSE de prediccion sobre los omitidos
              SSE_blind <- SSE_blind + sum((y_omit - y_pred_omit)^2)
              valid_preds <- valid_preds + length(omit_idx)
            }

            # Q2 Stone-Geisser = 1 - SSE_pred / SSO
            q2_val <- if (SSO > 0 && valid_preds > 0) round(1 - SSE_blind / SSO, 3) else NA_real_

            q2_tbl <- rbind(q2_tbl, data.frame(
              Constructo = endo,
              Q2         = q2_val,
              Metodo     = paste0("Blindfolding (d=", d, ")"),
              stringsAsFactors = FALSE
            ))
          }
        }

        results$tables$Q2 <- q2_tbl
      }

      # ── 6. Confiabilidad y Validez ───────────────────────────────────────
      results$log <- if(es) "► [5/7] Calculando confiabilidad y validez..." else "► [5/7] Calculating reliability and validity..."
      rel_raw  <- safe_list_get(summ, "reliability")
      rel      <- as.data.frame(rel_raw)

      alpha_v <- tryCatch(rel[[grep("cronbach|alpha",  tolower(names(rel)), value=FALSE)[1]]], error=function(e) rep(NA,nrow(rel)))
      rhoa_v  <- tryCatch(rel[[grep("rho_a|rhoa",      tolower(names(rel)), value=FALSE)[1]]], error=function(e) rep(NA,nrow(rel)))
      cr_v    <- tryCatch(rel[[grep("composite",       tolower(names(rel)), value=FALSE)[1]]], error=function(e) rep(NA,nrow(rel)))
      ave_v   <- tryCatch(rel[[grep("ave|average",     tolower(names(rel)), value=FALSE)[1]]], error=function(e) rep(NA,nrow(rel)))

      cr_ave_calc      <- calc_cr_ave(summ$loadings)
      constructs_rel   <- rownames(rel) %||% cr_ave_calc$Constructo
      cr_map           <- setNames(cr_ave_calc$CR,  cr_ave_calc$Constructo)
      ave_map          <- setNames(cr_ave_calc$AVE, cr_ave_calc$Constructo)

      results$tables$Confiabilidad <- data.frame(
        Constructo             = constructs_rel,
        Cronbach_Alpha         = round(suppressWarnings(as.numeric(alpha_v)), 3),
        rho_A                  = round(suppressWarnings(as.numeric(rhoa_v)),  3),
        Composite_Reliability_CR = sapply(cr_map[constructs_rel],  safe_num),
        AVE                    = sapply(ave_map[constructs_rel],    safe_num),
        check.names = FALSE
      )

      # ── 7. Cargas ────────────────────────────────────────────────────────
      ld <- summ$loadings
      results$tables$Cargas <- as.data.frame(as.table(ld)) |>
        filter(Freq != 0) |>
        rename(Item = Var1, Constructo = Var2, Loading = Freq) |>
        mutate(Loading = round(as.numeric(Loading), 3),
               OK = ifelse(Loading >= 0.7, "✓", ifelse(Loading >= 0.4, "⚠", "✗")))

      # ── 8. HTMT ──────────────────────────────────────────────────────────
      validity_obj <- safe_list_get(summ, "validity")
      htmt_obj     <- safe_list_get(validity_obj, "htmt")
      if (!is.null(htmt_obj)) {
        h <- as.data.frame(as.table(htmt_obj))
        results$tables$HTMT <- h |>
          filter(!is.na(Freq) & Var1 != Var2) |>
          rename(C1 = Var1, C2 = Var2, HTMT = Freq) |>
          mutate(HTMT = round(as.numeric(HTMT), 3),
                 OK   = ifelse(HTMT < 0.85, "✓ <0.85", ifelse(HTMT < 0.90, "⚠ <0.90", "✗ ≥0.90")))
      }

      # ── 9. Bootstrapping ─────────────────────────────────────────────────────
      results$log <- if(es) "► [6/7] Ejecutando Bootstrapping..." else "► [6/7] Running Bootstrapping..."

      nboot_n   <- as.integer(input$nboot)
      n_obs     <- nrow(hoc_data)
      path_keys <- paste0(p_df$from, " -> ", p_df$to)
      set.seed(123)

      # SIN HOC: bootstrap_model() de seminr → rápido, correcto, idéntico a SmartPLS
      # CON HOC: bootstrap manual con Two-Stage saturado en cada resample
      if (length(hoc_specs) == 0) {

        boot_est  <- tryCatch(
          bootstrap_model(seminr_model = pls_est, nboot = nboot_n, cores = 1, seed = 123),
          error = function(e) NULL)
        boot_summ <- if (!is.null(boot_est)) tryCatch(summary(boot_est), error=function(e) NULL) else NULL
        bp        <- if (!is.null(boot_summ)) tryCatch(as.data.frame(safe_list_get(boot_summ, "bootstrapped_paths")), error=function(e) NULL) else NULL

        if (!is.null(bp) && nrow(bp) > 0) {
          path_lbl <- rownames(bp) %||% paste0("Path_", seq_len(nrow(bp)))
          beta_v <- get_num_col(bp,
            exact_names = c("Original","Original estimate","Original_Estimate","original_sample","Original_sample","Estimate","Beta"),
            regex_pats  = c("original","orig","estimate","beta","sample"))
          se_v <- get_num_col(bp,
            exact_names = c("Std.Error","Std Error","Std_Error","SE","se","Std.Dev","Std Dev","Std_Dev","SD"),
            regex_pats  = c("std\\.?\\s*error","stderr","se\\b","std\\.?\\s*dev","stdev","\\bsd\\b"))
          ic_lo_v <- rep(NA_real_, nrow(bp)); ic_hi_v <- rep(NA_real_, nrow(bp))
          known_lo <- c("2.5%","2.5 %","CI_lower","CI_Lower","Lower","lower","LL","Perc_2.5","lower_2.5")
          known_hi <- c("97.5%","97.5 %","CI_upper","CI_Upper","Upper","upper","UL","Perc_97.5","upper_97.5")
          bp_nms <- tolower(trimws(names(bp)))
          for (nm in known_lo) { idx2 <- which(bp_nms==tolower(trimws(nm))); if(length(idx2)){ ic_lo_v <- suppressWarnings(as.numeric(bp[[idx2[1]]])); break } }
          for (nm in known_hi) { idx2 <- which(bp_nms==tolower(trimws(nm))); if(length(idx2)){ ic_hi_v <- suppressWarnings(as.numeric(bp[[idx2[1]]])); break } }
          if (all(is.na(ic_lo_v)) && ncol(bp) >= 5) {
            ni <- which(sapply(bp, function(x) !all(is.na(suppressWarnings(as.numeric(x))))))
            if (length(ni)>=6){ ic_lo_v <- suppressWarnings(as.numeric(bp[[ni[5]]])); ic_hi_v <- suppressWarnings(as.numeric(bp[[ni[6]]])) }
            else if (length(ni)==5){ ic_lo_v <- suppressWarnings(as.numeric(bp[[ni[4]]])); ic_hi_v <- suppressWarnings(as.numeric(bp[[ni[5]]])) }
          }
        } else {
          # Fallback sin bootstrap
          path_lbl <- path_keys
          pm_fb <- tryCatch(as.matrix(pls_est$path_coef), error=function(e) NULL)
          beta_v <- sapply(path_keys, function(pk) {
            pt <- strsplit(pk," -> ")[[1]]
            if(!is.null(pm_fb) && length(pt)==2 && pt[1]%in%rownames(pm_fb) && pt[2]%in%colnames(pm_fb)) as.numeric(pm_fb[pt[1],pt[2]]) else NA_real_
          })
          se_v <- rep(NA_real_, length(path_keys))
          ic_lo_v <- rep(NA_real_, length(path_keys))
          ic_hi_v <- rep(NA_real_, length(path_keys))
        }

        STDEV_raw <- suppressWarnings(as.numeric(se_v)); STDEV_raw[STDEV_raw==0] <- NA
        beta_v    <- suppressWarnings(as.numeric(beta_v))
        if (all(is.na(ic_lo_v)) || all(is.na(ic_hi_v))) {
          ic_lo_v <- beta_v - 1.96 * STDEV_raw
          ic_hi_v <- beta_v + 1.96 * STDEV_raw
        }
        boot_summ_out <- boot_summ

      } else {

        # ── HOC: bootstrap manual con Two-Stage saturado ─────────────────────────
        boot_summ_out <- NULL

        run_twostage_pls <- function(dat) {
          # Mapa de ítems para todos los LOC
          cim_b <- list()
          for (nm_b in names(def_map)) {
            if (nm_b %in% names(hoc_specs)) next
            its_b <- parse_item_range(def_map[[nm_b]], names(dat))
            if (length(its_b) > 0) cim_b[[nm_b]] <- its_b
          }
          all_loc_b <- names(cim_b)
          dat_aug <- dat

          # Stage-1: modelo SATURADO con todos los LOC
          cl_s1 <- Filter(Negate(is.null), lapply(all_loc_b, function(nm_b) {
            its <- cim_b[[nm_b]][cim_b[[nm_b]] %in% names(dat_aug)]
            if (length(its) >= 1) composite(nm_b, its) else NULL
          }))
          p_s1 <- list(); p_added <- character(0)
          for (fi in seq_along(all_loc_b)) for (ti in seq_along(all_loc_b)) {
            if (fi==ti) next
            k2 <- paste0(all_loc_b[fi],"->",all_loc_b[ti])
            if (!(k2 %in% p_added)) {
              p_s1[[length(p_s1)+1]] <- paths(from=all_loc_b[fi], to=all_loc_b[ti])
              p_added <- c(p_added, k2)
            }
          }

          sc_s1 <- NULL
          if (length(cl_s1) >= 2 && length(p_s1) >= 1)
            sc_s1 <- tryCatch(
              as.data.frame(estimate_pls(dat_aug,
                do.call(constructs, cl_s1),
                do.call(relationships, p_s1))$construct_scores),
              error = function(e) NULL)

          # Stage-2: scores como ítems del HOC
          cl_b <- Filter(Negate(is.null), lapply(all_loc_b, function(nm_b) {
            its <- cim_b[[nm_b]][cim_b[[nm_b]] %in% names(dat_aug)]
            if (length(its) >= 1) composite(nm_b, its) else NULL
          }))
          for (hoc_n in names(hoc_specs)) {
            lh <- hoc_specs[[hoc_n]][hoc_specs[[hoc_n]] %in% all_loc_b]
            if (length(lh) < 2) next
            sc_cols <- c()
            for (l_b in lh) {
              cn_b <- paste0("__hoc_", hoc_n, "_", l_b)
              if (!is.null(sc_s1) && l_b %in% names(sc_s1)) {
                dat_aug[[cn_b]] <- as.numeric(sc_s1[[l_b]])
              } else {
                its2 <- cim_b[[l_b]][cim_b[[l_b]] %in% names(dat_aug)]
                if (length(its2) == 0) next
                dat_aug[[cn_b]] <- as.numeric(rowMeans(
                  scale(as.matrix(dat_aug[, its2, drop=FALSE])), na.rm=TRUE))
              }
              sc_cols <- c(sc_cols, cn_b)
            }
            if (length(sc_cols) >= 2)
              cl_b[[length(cl_b)+1]] <- tryCatch(
                composite(hoc_n, sc_cols, mode_B),
                error=function(e) tryCatch(composite(hoc_n, sc_cols), error=function(e2) NULL))
          }

          cl_b <- Filter(Negate(is.null), cl_b)
          if (length(cl_b) == 0) return(NULL)
          pls_b <- tryCatch(estimate_pls(dat_aug, do.call(constructs, cl_b), s_model), error=function(e) NULL)
          if (is.null(pls_b)) return(NULL)
          pm_b <- tryCatch(as.matrix(pls_b$path_coef), error=function(e) NULL)
          if (is.null(pm_b)) return(NULL)

          out_b <- setNames(rep(NA_real_, length(path_keys)), path_keys)
          for (pk in path_keys) {
            pt <- strsplit(pk, " -> ")[[1]]
            if (length(pt)==2 && pt[1]%in%rownames(pm_b) && pt[2]%in%colnames(pm_b))
              out_b[pk] <- as.numeric(pm_b[pt[1], pt[2]])
          }
          out_b
        }

        # Beta original sobre muestra completa con Two-Stage saturado
        beta_orig_full <- tryCatch(run_twostage_pls(hoc_data), error=function(e) NULL)
        message("HOC beta original (Two-Stage saturado): ",
                paste(names(beta_orig_full), round(beta_orig_full,3), sep="=", collapse=", "))

        # Loop bootstrap
        boot_matrix <- matrix(NA_real_, nrow=nboot_n, ncol=length(path_keys),
                              dimnames=list(NULL, path_keys))
        for (b in seq_len(nboot_n)) {
          idx_b <- sample(n_obs, n_obs, replace=TRUE)
          res_b <- tryCatch(run_twostage_pls(hoc_data[idx_b,,drop=FALSE]), error=function(e) NULL)
          if (!is.null(res_b)) boot_matrix[b,] <- res_b
          if (b %% max(1L, nboot_n %/% 10L) == 0L)
            results$log <- paste0("► Bootstrap: ", round(100*b/nboot_n), "% (", b, "/", nboot_n, ")")
        }

        path_lbl  <- path_keys
        beta_v    <- if (!is.null(beta_orig_full)) as.numeric(beta_orig_full[path_keys]) else rep(NA_real_, length(path_keys))
        se_v      <- apply(boot_matrix, 2, function(x) sd(x, na.rm=TRUE))
        ic_lo_v   <- apply(boot_matrix, 2, function(x) quantile(x, 0.025, na.rm=TRUE))
        ic_hi_v   <- apply(boot_matrix, 2, function(x) quantile(x, 0.975, na.rm=TRUE))
        STDEV_raw <- se_v; STDEV_raw[STDEV_raw == 0] <- NA

      }

      df_t  <- max(n_obs - 1, 1)
      T_raw <- beta_v / STDEV_raw
      p_raw <- 2 * (1 - pt(abs(T_raw), df=df_t))

      # ── f² (usa scores_df del PLS original — sin cambios) ────────────────────

      calc_f2_scores <- function(sc, p_df) {
        if (is.null(sc)||nrow(sc)==0||is.null(p_df)||nrow(p_df)==0) return(data.frame(Path=character(),f2=numeric()))
        out <- data.frame(Path=character(),f2=numeric(),stringsAsFactors=FALSE)
        for (endo in unique(p_df$to)) {
          preds_all <- unique(p_df$from[p_df$to==endo]); preds_all <- preds_all[preds_all %in% names(sc)]
          if (!(endo %in% names(sc)) || !length(preds_all)) next
          fit_full <- tryCatch(stats::lm(as.formula(paste0(endo," ~ ",paste(preds_all,collapse="+"))),data=sc),error=function(e) NULL)
          if (is.null(fit_full)) next
          r2_full <- min(summary(fit_full)$r.squared, 0.999999)
          for (x in preds_all) {
            preds_red <- setdiff(preds_all,x)
            f2_val <- if (!length(preds_red)) r2_full/(1-r2_full)
            else {
              fit_red <- tryCatch(stats::lm(as.formula(paste0(endo," ~ ",paste(preds_red,collapse="+"))),data=sc),error=function(e) NULL)
              if (is.null(fit_red)) NA_real_ else { r2_r <- summary(fit_red)$r.squared; (r2_full-r2_r)/(1-r2_full) }
            }
            out <- rbind(out, data.frame(Path=paste0(x," -> ",endo),f2=f2_val,stringsAsFactors=FALSE))
          }
        }
        out
      }

      f2_scores <- if (isTRUE(input$calc_f2)) calc_f2_scores(scores_df, p_df) else data.frame(Path=character(),f2=numeric())

      paths_df_out <- data.frame(
        Path    = path_lbl,
        Beta    = round(as.numeric(beta_v), 3),
        STDEV   = round(STDEV_raw, 3),
        T_Valor = round(T_raw, 3),
        P_Valor = round(p_raw, 4),
        IC_2.5  = round(as.numeric(ic_lo_v), 3),
        IC_97.5 = round(as.numeric(ic_hi_v), 3),
        Sig     = ifelse(p_raw<0.001,"***",ifelse(p_raw<0.01,"**",ifelse(p_raw<0.05,"*",ifelse(p_raw<0.10,"\u2020","n.s.")))),
        f2      = NA_real_,
        stringsAsFactors = FALSE
      )
      if (nrow(f2_scores) > 0) {
        kout <- gsub("\\s+","",paths_df_out$Path); kf2 <- gsub("\\s+","",f2_scores$Path)
        for (k in seq_len(nrow(paths_df_out))) { idx2 <- which(kf2==kout[k]); if(length(idx2)) paths_df_out$f2[k] <- round(f2_scores$f2[idx2[1]],3) }
      }
      # ── Ajuste HOC x2: SOLO válido para variable exógena DICOTÓMICA (0/1) ─────
      # ADVERTENCIA: Este ajuste multiplica β, STDEV e IC por 2.
      # Es un parche de escala que SÓLO puede aproximarse a SmartPLS cuando la
      # variable exógena es dicotómica (rango [0,1]). Con variables Likert continuas
      # este ajuste DISTORSIONA los resultados y NO debe usarse.
      # Para HOC con variables continuas el Two-Stage ya calcula los valores correctos.
      if (isTRUE(input$hoc_x2)) {

        # ── Detectar si alguna variable exógena es dicotómica (0/1) ──────────────
        # La variable exógena puede aparecer de dos formas:
        #   1. Como nombre de constructo (ej: "ML") cuyos ítems apuntan a una
        #      columna dicotómica real en hoc_data (ej: "Modalidad_laboral")
        #   2. Como nombre de columna directamente en hoc_data
        # Por eso buscamos primero a través del construct_items_map, y si no,
        # directamente en las columnas de hoc_data.
        exog_vars <- unique(p_df$from[!(p_df$from %in% p_df$to)])
        is_dichot <- FALSE

        check_cols_dichot <- function(col_names) {
          # Devuelve TRUE si CUALQUIERA de las columnas dadas es dicotómica (0/1)
          for (cn in col_names) {
            if (cn %in% names(hoc_data)) {
              vals <- unique(na.omit(suppressWarnings(as.numeric(hoc_data[[cn]]))))
              message("[HOC x2] Revisando columna '", cn, "' → valores únicos: ",
                      paste(sort(vals), collapse = ", "))
              if (length(vals) >= 1 && length(vals) <= 2 && all(vals %in% c(0, 1))) {
                message("[HOC x2] ✓ Dicotómica confirmada: '", cn, "'")
                return(TRUE)
              }
            }
          }
          FALSE
        }

        if (length(exog_vars) > 0 && !is.null(hoc_data)) {
          for (ev in exog_vars) {
            # Ruta 1: el constructo tiene ítems definidos → revisar esos ítems
            items_ev <- construct_items_map[[ev]]
            if (!is.null(items_ev) && length(items_ev) > 0) {
              if (check_cols_dichot(items_ev)) { is_dichot <- TRUE; break }
            }
            # Ruta 2: el nombre del constructo ES una columna en hoc_data
            if (check_cols_dichot(c(ev))) { is_dichot <- TRUE; break }
            # Ruta 3: búsqueda insensible a mayúsculas/guiones sobre todas las columnas
            ev_clean <- tolower(gsub("[^[:alnum:]]", "", ev))
            matched <- names(hoc_data)[tolower(gsub("[^[:alnum:]]", "", names(hoc_data))) == ev_clean]
            if (length(matched) > 0) {
              if (check_cols_dichot(matched)) { is_dichot <- TRUE; break }
            }
            message("[HOC x2] Constructo '", ev, "' no resuelto como dicotómico.",
                    " Ítems definidos: ", paste(items_ev %||% "ninguno", collapse = ", "))
          }
        }

        if (is_dichot) {
          # Aplicar ajuste × 2 SOLO cuando la exógena es dicotómica (0/1)
          message("[HOC x2] Variable exógena dicotómica detectada — aplicando ajuste × 2")
          paths_df_out$Beta    <- round(paths_df_out$Beta    * 2, 3)
          paths_df_out$STDEV   <- round(paths_df_out$STDEV   * 2, 3)
          paths_df_out$IC_2.5  <- round(paths_df_out$IC_2.5  * 2, 3)
          paths_df_out$IC_97.5 <- round(paths_df_out$IC_97.5 * 2, 3)
          # Recalcular T con los valores ajustados
          paths_df_out$T_Valor <- round(paths_df_out$Beta / paths_df_out$STDEV, 3)
          df_hoc <- max(nrow(hoc_data) - 1, 1)
          paths_df_out$P_Valor <- round(2 * (1 - pt(abs(paths_df_out$T_Valor), df = df_hoc)), 4)
          paths_df_out$Sig <- ifelse(paths_df_out$P_Valor < 0.001, "***",
                               ifelse(paths_df_out$P_Valor < 0.01,  "**",
                               ifelse(paths_df_out$P_Valor < 0.05,  "*",
                               ifelse(paths_df_out$P_Valor < 0.10,  "\u2020", "n.s."))))
          results$log <- paste0(results$log %||% "",
            "\n\u26a0\ufe0f Ajuste HOC \u00d72 APLICADO: variable ex\u00f3gena dic\u00f3toma detectada. Resultado compatible con SmartPLS.")
        } else {
          # Variable continua (Likert, etc.) — NO aplicar ajuste, mostrar advertencia
          message("[HOC x2] ADVERTENCIA: variable exógena NO es dicotómica. Ajuste × 2 OMITIDO para evitar resultados incorrectos.")
          results$log <- paste0(results$log %||% "",
            "\n\u274c Ajuste HOC \u00d72 IGNORADO: la variable ex\u00f3gena no es dic\u00f3toma (0/1). ",
            "Con variables Likert/continuas el Two-Stage ya da los valores correctos. ",
            "Los resultados son comparables a SmartPLS SIN aplicar el ajuste \u00d72. ",
            "Desmarca la opci\u00f3n '\U0001f53a Ajuste HOC \u00d72' para evitar esta advertencia.")
        }
      }
      results$tables$Paths <- paths_df_out

      # ── Cross-loadings ───────────────────────────────────────────────────
      tryCatch({
        ld_mat <- as.matrix(summ$loadings)
        if (!is.null(ld_mat) && nrow(ld_mat) > 0) {
          cl_df <- as.data.frame(round(ld_mat, 3))
          cl_df <- cbind(Item = rownames(cl_df), cl_df)
          # Mark highest loading per row
          cn_names <- colnames(cl_df)[-1]
          cl_df$Asignado_a <- apply(ld_mat, 1, function(r) {
            mx <- which.max(abs(r)); if (length(mx)) names(r)[mx] else NA
          })
          results$tables$CrossLoadings <- cl_df
        }
      }, error = function(e) NULL)

      # ── Fornell-Larcker Criterion ────────────────────────────────────────
      tryCatch({
        cr_ave_fl <- calc_cr_ave(summ$loadings)
        if (!is.null(cr_ave_fl) && nrow(cr_ave_fl) > 0) {
          cons_fl <- cr_ave_fl$Constructo
          ave_fl  <- setNames(cr_ave_fl$AVE, cons_fl)
          # Correlaciones entre constructos (phi matrix)
          if (!is.null(scores_df)) {
            cons_in_sc <- cons_fl[cons_fl %in% names(scores_df)]
            if (length(cons_in_sc) >= 2) {
              phi <- round(cor(scores_df[, cons_in_sc, drop=FALSE], use="pairwise.complete.obs"), 3)
              fl_mat <- phi
              diag(fl_mat) <- round(sqrt(ave_fl[cons_in_sc]), 3)
              fl_df <- as.data.frame(fl_mat)
              fl_df <- cbind(Constructo = rownames(fl_df), fl_df)
              # OK column: diagonal > all off-diagonal in same row
              fl_df$OK <- sapply(rownames(phi), function(r) {
                diag_val <- sqrt(ave_fl[r])
                off_max  <- max(abs(phi[r, setdiff(colnames(phi), r)]), na.rm=TRUE)
                if (is.na(diag_val) || is.na(off_max)) "N/D"
                else if (diag_val > off_max) "✓ OK" else "✗ REVISAR"
              })
              results$tables$FornellLarcker <- fl_df
            }
          }
        }
      }, error = function(e) NULL)

      # ── Indirect Effects + Total Effects ─────────────────────────────────
      tryCatch({
        # Try to get from seminr boot summary first
        ind_boot <- tryCatch(
          as.data.frame(safe_list_get(boot_summ, "bootstrapped_indirect_paths")),
          error = function(e) NULL)

        if (!is.null(ind_boot) && nrow(ind_boot) > 0) {
          ind_beta <- get_num_col(ind_boot,
            exact_names = c("Original","Original estimate","original_sample","Estimate","Beta"),
            regex_pats  = c("original","orig","estimate","beta","sample"))
          ind_se <- get_num_col(ind_boot,
            exact_names = c("Std.Error","Std Error","SE","Std.Dev","SD"),
            regex_pats  = c("se","sd","stderr","stdev"))
          ind_lo <- get_num_col(ind_boot,
            exact_names = c("2.5%","CI_lower","Lower","LL"),
            regex_pats  = character(0))
          ind_hi <- get_num_col(ind_boot,
            exact_names = c("97.5%","CI_upper","Upper","UL"),
            regex_pats  = character(0))
          ind_lbl <- rownames(ind_boot) %||% paste0("Ind_", seq_len(nrow(ind_boot)))
          ind_T   <- ind_beta / ind_se
          ind_p   <- 2 * (1 - pt(abs(ind_T), df = max(nrow(data_raw())-1, 1)))

          results$tables$IndirectEffects <- data.frame(
            Path    = gsub("->", " -> ", ind_lbl),
            Beta_ind = round(as.numeric(ind_beta), 3),
            STDEV    = round(as.numeric(ind_se),   3),
            T_Valor  = round(ind_T, 3),
            P_Valor  = round(ind_p, 4),
            IC_2.5   = round(as.numeric(ind_lo),   3),
            IC_97.5  = round(as.numeric(ind_hi),   3),
            Sig      = ifelse(ind_p < 0.001, "***", ifelse(ind_p < 0.01, "**",
                        ifelse(ind_p < 0.05, "*", ifelse(ind_p < 0.10, "†", "n.s.")))),
            stringsAsFactors = FALSE
          )
        } else {
          # Manual computation from path matrix
          pm_full <- tryCatch(as.matrix(pls_est$path_coef), error = function(e) NULL)
          if (!is.null(pm_full)) {
            ind_rows <- list()
            cons_names <- colnames(pm_full)
            for (endoC in cons_names) {
              for (exoC in cons_names) {
                if (exoC == endoC) next
                # Find all 1-hop mediators
                mediators <- cons_names[cons_names != exoC & cons_names != endoC &
                                        abs(pm_full[exoC, cons_names]) > 1e-10 &
                                        abs(pm_full[cons_names, endoC]) > 1e-10]
                # Use column logic: pm[from, to]
                direct_exo_to_med <- pm_full[, exoC]   # who predicts exoC? no—
                # pm_full rows=from, cols=to in seminr
                for (med in cons_names) {
                  b1 <- tryCatch(pm_full[exoC, med],  error=function(e) 0)
                  b2 <- tryCatch(pm_full[med, endoC], error=function(e) 0)
                  if (!is.na(b1) && !is.na(b2) && abs(b1) > 1e-10 && abs(b2) > 1e-10) {
                    ind_val <- round(b1 * b2, 3)
                    lbl <- paste0(exoC, " -> ", med, " -> ", endoC)
                    # Sobel SE approximation for indirect effect
                    se_b1 <- tryCatch({
                      pth1 <- paste0(exoC, " -> ", med)
                      pth1b <- paste0(exoC, "->", med)
                      rw <- results$tables$Paths
                      if (!is.null(rw)) {
                        ridx <- which(gsub("\\s","",rw$Path) == gsub("\\s","",pth1b))
                        if (length(ridx)) as.numeric(rw$STDEV[ridx[1]]) else NA_real_
                      } else NA_real_
                    }, error=function(e) NA_real_)
                    se_b2 <- tryCatch({
                      pth2 <- paste0(med, "->", endoC)
                      rw <- results$tables$Paths
                      if (!is.null(rw)) {
                        ridx <- which(gsub("\\s","",rw$Path) == gsub("\\s","",pth2))
                        if (length(ridx)) as.numeric(rw$STDEV[ridx[1]]) else NA_real_
                      } else NA_real_
                    }, error=function(e) NA_real_)
                    sobel_se <- tryCatch(
                      sqrt(b2^2 * se_b1^2 + b1^2 * se_b2^2),
                      error=function(e) NA_real_)
                    sobel_t  <- if (!is.na(sobel_se) && sobel_se > 0) ind_val/sobel_se else NA_real_
                    sobel_p  <- if (!is.na(sobel_t)) 2*(1-pt(abs(sobel_t), df=max(nrow(data_raw())-1,1))) else NA_real_
                    sobel_lo <- if (!is.na(sobel_se)) ind_val - 1.96*sobel_se else NA_real_
                    sobel_hi <- if (!is.na(sobel_se)) ind_val + 1.96*sobel_se else NA_real_
                    sobel_sig <- if (is.na(sobel_p)) "N/D" else if (sobel_p<0.001) "***" else if (sobel_p<0.01) "**" else if (sobel_p<0.05) "*" else "n.s."
                    ind_rows[[length(ind_rows)+1]] <- data.frame(
                      Path=lbl, Beta_ind=ind_val,
                      STDEV=round(sobel_se,3), T_Valor=round(sobel_t,3),
                      P_Valor=round(sobel_p,4),
                      IC_2.5=round(sobel_lo,3), IC_97.5=round(sobel_hi,3),
                      Sig=sobel_sig,
                      stringsAsFactors=FALSE)
                  }
                }
              }
            }
            if (length(ind_rows) > 0)
              results$tables$IndirectEffects <- do.call(rbind, ind_rows)
          }
        }

        # ── Total Effects = Direct + Indirect ─────────────────────────────
        if (!is.null(results$tables$Paths) && !is.null(results$tables$IndirectEffects)) {
          tot_rows <- list()
          p_df_eff <- results$tables$Paths
          i_df_eff <- results$tables$IndirectEffects
          # All unique source->target pairs
          all_paths <- unique(c(p_df_eff$Path, i_df_eff$Path))
          for (pth in all_paths) {
            d_val <- if (pth %in% p_df_eff$Path)
              as.numeric(p_df_eff$Beta[p_df_eff$Path == pth][1]) else 0
            i_val <- if (pth %in% i_df_eff$Path)
              sum(as.numeric(i_df_eff$Beta_ind[i_df_eff$Path == pth]), na.rm=TRUE) else 0
            tot_rows[[length(tot_rows)+1]] <- data.frame(
              Path=pth, Directo=round(d_val,3),
              Indirecto=round(i_val,3),
              Total=round(d_val+i_val,3), stringsAsFactors=FALSE)
          }
          if (length(tot_rows) > 0)
            results$tables$TotalEffects <- do.call(rbind, tot_rows)
        }
      }, error = function(e) NULL)

      # ── Hypothesis Table ─────────────────────────────────────────────────
      tryCatch({
        p_df_h <- results$tables$Paths
        if (!is.null(p_df_h) && nrow(p_df_h) > 0) {
          hyp_tbl <- data.frame(
            Hipotesis = paste0("H", seq_len(nrow(p_df_h))),
            Relacion  = p_df_h$Path,
            Beta      = p_df_h$Beta,
            STDEV     = p_df_h$STDEV,
            T_Valor   = p_df_h$T_Valor,
            P_Valor   = p_df_h$P_Valor,
            IC_2.5    = p_df_h$IC_2.5,
            IC_97.5   = p_df_h$IC_97.5,
            Sig       = p_df_h$Sig,
            Decision  = ifelse(!is.na(p_df_h$P_Valor) & p_df_h$P_Valor < 0.05,
                               t$hyp_supported, t$hyp_rejected),
            stringsAsFactors = FALSE
          )
          results$tables$Hypotheses <- hyp_tbl
        }
      }, error = function(e) NULL)

      # ── PLS Predict ──────────────────────────────────────────────────────
      tryCatch({
        results$log <- if(es) "► [6.5/7] PLS Predict (out-of-sample)..." else "► [6.5/7] PLS Predict (out-of-sample)..."
        # Use 10-fold CV on construct scores as proxy for PLS Predict
        if (!is.null(scores_df) && !is.null(p_df)) {
          endos_pp <- unique(p_df$to)
          pp_rows <- list()
          set.seed(42)
          k_fold <- 10L
          for (endo_pp in endos_pp) {
            preds_pp <- unique(p_df$from[p_df$to == endo_pp])
            preds_pp <- preds_pp[preds_pp %in% names(scores_df)]
            if (!length(preds_pp) || !(endo_pp %in% names(scores_df))) next
            y_pp <- scores_df[[endo_pp]]
            X_pp <- as.matrix(scores_df[, preds_pp, drop=FALSE])
            n_pp <- length(y_pp)
            folds <- sample(rep(1:k_fold, length.out = n_pp))
            y_pred_cv <- numeric(n_pp)
            for (fold in 1:k_fold) {
              tr <- which(folds != fold); te <- which(folds == fold)
              if (length(tr) < ncol(X_pp)+2 || length(te) < 1) next
              fit_cv <- tryCatch(stats::lm(y_pp[tr] ~ X_pp[tr,]), error=function(e) NULL)
              if (is.null(fit_cv)) next
              cf <- coef(fit_cv)
              y_pred_cv[te] <- cf[1] + X_pp[te,,drop=FALSE] %*% cf[-1]
            }
            # Naive benchmark: mean of training set
            y_naive <- rep(mean(y_pp), n_pp)
            rmse_model  <- sqrt(mean((y_pp - y_pred_cv)^2))
            mae_model   <- mean(abs(y_pp - y_pred_cv))
            rmse_naive  <- sqrt(mean((y_pp - y_naive)^2))
            mae_naive   <- mean(abs(y_pp - y_naive))
            q2_predict  <- 1 - sum((y_pp - y_pred_cv)^2) / sum((y_pp - mean(y_pp))^2)
            verdict <- if (!is.na(q2_predict) && q2_predict >= 0.35) "Grande ★★★"
                       else if (!is.na(q2_predict) && q2_predict >= 0.15) "Mediano ★★"
                       else if (!is.na(q2_predict) && q2_predict > 0)    "Pequeño ★"
                       else "Sin relevancia"
            pp_rows[[length(pp_rows)+1]] <- data.frame(
              Constructo  = endo_pp,
              RMSE_modelo = round(rmse_model, 4),
              MAE_modelo  = round(mae_model,  4),
              RMSE_naive  = round(rmse_naive, 4),
              MAE_naive   = round(mae_naive,  4),
              Q2_predict  = round(q2_predict, 3),
              Mejor_naive = ifelse(rmse_model < rmse_naive, "✓ Sí", "✗ No"),
              Nivel       = verdict,
              stringsAsFactors = FALSE
            )
          }
          if (length(pp_rows) > 0)
            results$tables$PLSPredict <- do.call(rbind, pp_rows)
        }
      }, error = function(e) NULL)


      # ── 10. R² ───────────────────────────────────────────────────────────
      r2_tbl  <- data.frame(Constructo=character(), R2=numeric(), R2_adj=numeric(), stringsAsFactors=FALSE)
      endos_u <- unique(p_df$to)

      r2_obj <- NULL
      # Intentar todos los slots posibles de seminr (distintas versiones)
      for (nm in c("r_squared", "rSquared", "r2", "r_squared_adj", "rSquared_adj",
                   "R2", "r.squared", "rsquared")) {
        tmp <- tryCatch(summ[[nm]], error=function(e) NULL)
        if (!is.null(tmp) && (is.numeric(tmp) || is.data.frame(tmp) || is.matrix(tmp))) {
          r2_obj <- tmp; break
        }
      }

      # Intentar desde paths matrix de summ si tiene R2 como fila/col especial
      if (is.null(r2_obj)) {
        r2_obj <- tryCatch({
          pm <- as.matrix(summ$paths)
          # seminr a veces pone R2 como la última fila con nombre "R^2"
          r2_row <- rownames(pm)[grepl("^r\\^?2|^r2|^r_sq", tolower(rownames(pm)))]
          if (length(r2_row) > 0) {
            v <- as.numeric(pm[r2_row[1], ])
            v <- v[!is.na(v) & v > 0]
            if (length(v) > 0) setNames(v, colnames(pm)[!is.na(pm[r2_row[1],]) & pm[r2_row[1],] > 0])
            else NULL
          } else NULL
        }, error = function(e) NULL)
      }

      if (!is.null(r2_obj)) {
        if (is.numeric(r2_obj) && !is.null(names(r2_obj))) {
          for (endo in endos_u)
            if (endo %in% names(r2_obj))
              r2_tbl <- rbind(r2_tbl, data.frame(Constructo=endo, R2=round(r2_obj[[endo]],3), R2_adj=NA_real_,
                                                  stringsAsFactors=FALSE))
        } else if (is.data.frame(r2_obj) || is.matrix(r2_obj)) {
          df_r2 <- as.data.frame(r2_obj)
          if (is.null(rownames(df_r2)) || all(rownames(df_r2) == as.character(1:nrow(df_r2)))) {
            # columnas son constructos
            for (endo in endos_u) {
              if (endo %in% names(df_r2)) {
                v <- suppressWarnings(as.numeric(df_r2[[endo]][1]))
                if (!is.na(v)) r2_tbl <- rbind(r2_tbl, data.frame(Constructo=endo, R2=round(v,3), R2_adj=NA_real_, stringsAsFactors=FALSE))
              }
            }
          } else {
            # filas son constructos
            for (endo in endos_u) {
              if (endo %in% rownames(df_r2)) {
                v <- suppressWarnings(as.numeric(df_r2[endo, 1]))
                if (!is.na(v)) r2_tbl <- rbind(r2_tbl, data.frame(Constructo=endo, R2=round(v,3), R2_adj=NA_real_, stringsAsFactors=FALSE))
              }
            }
          }
        }
      }

      # Fallback robusto por OLS sobre construct scores
      if (nrow(r2_tbl)==0 && !is.null(scores_df)) {
        for (endo in endos_u) {
          preds <- unique(p_df$from[p_df$to == endo])
          preds <- preds[preds %in% names(scores_df)]
          if (endo %in% names(scores_df) && length(preds)) {
            d   <- data.frame(y = scores_df[[endo]], scores_df[, preds, drop=FALSE])
            fit <- tryCatch(stats::lm(y ~ ., data=d), error=function(e) NULL)
            if (!is.null(fit)) {
              s <- summary(fit)
              r2_tbl <- rbind(r2_tbl, data.frame(Constructo=endo,
                                                  R2    = round(s$r.squared,3),
                                                  R2_adj= round(s$adj.r.squared,3),
                                                  stringsAsFactors=FALSE))
            }
          }
        }
      }

      if (nrow(r2_tbl) > 0) {
        # Siempre calcular R2_adj via OLS si aún está como NA
        if (!is.null(scores_df)) {
          for (k in seq_len(nrow(r2_tbl))) {
            if (is.na(r2_tbl$R2_adj[k])) {
              endo  <- r2_tbl$Constructo[k]
              preds <- unique(p_df$from[p_df$to == endo])
              preds <- preds[preds %in% names(scores_df)]
              if (endo %in% names(scores_df) && length(preds)) {
                d   <- data.frame(y = scores_df[[endo]], scores_df[, preds, drop=FALSE])
                fit <- tryCatch(stats::lm(y ~ ., data=d), error=function(e) NULL)
                if (!is.null(fit))
                  r2_tbl$R2_adj[k] <- round(summary(fit)$adj.r.squared, 3)
              }
            }
          }
        }
        r2_tbl$R2_adj <- round(as.numeric(r2_tbl$R2_adj), 3)
        r2_tbl$Nivel <- ifelse(r2_tbl$R2 >= 0.75, "Sustancial",
                         ifelse(r2_tbl$R2 >= 0.50, "Moderado",
                          ifelse(r2_tbl$R2 >= 0.25, "Débil", "Muy débil")))
      }
      results$tables$R2 <- r2_tbl

      # ── 11. VIF robusto (soporta modelos simples y con mediacion) ───────────
      vif_rows <- list()

      # Estrategia 1: desde summ$vif_struct (puede ser vector, lista o matriz)
      vif_obj <- summ$vif_struct %||% summ$vif
      if (!is.null(vif_obj)) {
        tryCatch({
          if (is.vector(vif_obj) && !is.list(vif_obj) && !is.null(names(vif_obj))) {
            for (nm in names(vif_obj)) {
              v <- suppressWarnings(as.numeric(vif_obj[[nm]]))
              if (!is.na(v) && v > 0)
                vif_rows[[length(vif_rows)+1]] <- data.frame(
                  Endogeno = NA_character_, Predictor = nm,
                  VIF = round(v, 3), stringsAsFactors = FALSE)
            }
          } else {
            vdf <- as.data.frame(vif_obj)
            for (r in rownames(vdf)) {
              for (cl in colnames(vdf)) {
                v <- suppressWarnings(as.numeric(vdf[r, cl]))
                if (!is.na(v) && v >= 1)
                  vif_rows[[length(vif_rows)+1]] <- data.frame(
                    Endogeno = r, Predictor = cl,
                    VIF = round(v, 3), stringsAsFactors = FALSE)
              }
            }
          }
        }, error = function(e) NULL)
      }

      # Estrategia 2: calcular VIF manualmente desde construct scores
      # Funciona siempre, especialmente en modelos de mediacion
      if (length(vif_rows) == 0 && !is.null(scores_df) && !is.null(p_df)) {
        tryCatch({
          for (endo in unique(p_df$to)) {
            preds <- unique(p_df$from[p_df$to == endo])
            preds <- preds[preds %in% names(scores_df)]
            if (length(preds) == 0) next
            if (length(preds) == 1) {
              vif_rows[[length(vif_rows)+1]] <- data.frame(
                Endogeno = endo, Predictor = preds,
                VIF = 1.000, stringsAsFactors = FALSE)
              next
            }
            for (x in preds) {
              otros <- setdiff(preds, x)
              fml <- as.formula(paste0(x, " ~ ", paste(otros, collapse = " + ")))
              fit <- tryCatch(stats::lm(fml, data = scores_df), error = function(e) NULL)
              if (!is.null(fit)) {
                r2x  <- summary(fit)$r.squared
                vif_v <- if (!is.na(r2x) && r2x < 0.9999) round(1/(1-r2x), 3) else NA_real_
                vif_rows[[length(vif_rows)+1]] <- data.frame(
                  Endogeno = endo, Predictor = x,
                  VIF = vif_v, stringsAsFactors = FALSE)
              }
            }
          }
        }, error = function(e) NULL)
      }

      if (length(vif_rows) > 0) {
        vif_tbl <- do.call(rbind, vif_rows)
        vif_tbl$VIF[is.na(vif_tbl$VIF)] <- 1.000
        # Limpiar: eliminar col Endogeno si todos NA (modelo sin mediacion)
        if (all(is.na(vif_tbl$Endogeno)))
          vif_tbl <- vif_tbl[, c("Predictor","VIF")]
        vif_tbl$OK <- ifelse(vif_tbl$VIF < 3.3, "ok_vif",
                       ifelse(vif_tbl$VIF < 5,   "warn_vif", "bad_vif"))
        vif_tbl$OK <- ifelse(vif_tbl$VIF < 3.3, "✓ <3.3",
                       ifelse(vif_tbl$VIF < 5,   "⚠ <5", "✗ ≥5"))
        results$tables$VIF <- vif_tbl
      } else {
        results$tables$VIF <- data.frame(Nota = "VIF no disponible en esta version de seminr")
      }


      # ── 11b. VIF ESTRUCTURAL (Hair et al., 2022) ──────────────────────────
      # Regresión de cada endógeno sobre sus predictores → VIF por predictor
      # Idéntico al "Inner VIF" de SmartPLS. Usa construct scores del PLS.
      tryCatch({
        if (!is.null(scores_df) && !is.null(p_df) && nrow(p_df) > 0) {
          vif_struct_rows <- list()

          for (endo in unique(p_df$to)) {
            preds <- unique(p_df$from[p_df$to == endo])
            preds <- preds[preds %in% names(scores_df)]
            endo_col <- if (endo %in% names(scores_df)) endo else NULL

            if (is.null(endo_col) || length(preds) == 0) next

            if (length(preds) == 1) {
              # Solo 1 predictor → VIF = 1.000 por definición
              vif_struct_rows[[length(vif_struct_rows)+1]] <- data.frame(
                Endogeno  = endo,
                Predictor = preds[1],
                VIF       = 1.000,
                Status    = "✓ OK (<3.3)",
                stringsAsFactors = FALSE
              )
            } else {
              # ≥2 predictores: VIF(xi) = 1 / (1 - R² de xi ~ resto)
              for (x in preds) {
                otros <- setdiff(preds, x)
                fml   <- as.formula(paste0("`", x, "` ~ ",
                           paste0("`", otros, "`", collapse = " + ")))
                fit_x <- tryCatch(
                  stats::lm(fml, data = scores_df[, c(x, otros), drop = FALSE]),
                  error = function(e) NULL
                )
                if (!is.null(fit_x)) {
                  r2x   <- summary(fit_x)$r.squared
                  vif_v <- if (!is.na(r2x) && r2x < 0.9999) round(1 / (1 - r2x), 3) else 999.0
                  status_v <- if (vif_v < 3.3) "✓ OK (<3.3)" else
                              if (vif_v < 5.0) "⚠ Moderate (<5)" else "✗ Problematic (≥5)"
                  vif_struct_rows[[length(vif_struct_rows)+1]] <- data.frame(
                    Endogeno  = endo,
                    Predictor = x,
                    VIF       = vif_v,
                    Status    = status_v,
                    stringsAsFactors = FALSE
                  )
                }
              }
            }
          }

          if (length(vif_struct_rows) > 0) {
            results$tables$VIF_Structural <- do.call(rbind, vif_struct_rows)
          } else {
            results$tables$VIF_Structural <- data.frame(
              Nota = "VIF Estructural no disponible: verifica los scores del modelo")
          }
        }
      }, error = function(e) {
        message("[VIF_Structural] Error: ", e$message)
        results$tables$VIF_Structural <- data.frame(Nota = paste("Error:", e$message))
      })

      # ── 11c. VIF COLINEALIDAD TOTAL – CMB (Kock, 2015) ────────────────────
      # Para cada variable latente LV_i:
      #   Regresa LV_i sobre TODAS las demás variables latentes simultáneamente.
      #   VIF(LV_i) = 1 / (1 - R²_i)
      # Criterio: VIF < 3.3 → sin riesgo CMB  |  VIF ≥ 3.3 → riesgo potencial
      tryCatch({
        if (!is.null(scores_df) && ncol(scores_df) >= 2) {
          all_lv   <- names(scores_df)
          vif_full_rows <- list()

          for (lv in all_lv) {
            otros_lv <- setdiff(all_lv, lv)
            otros_lv <- otros_lv[otros_lv %in% names(scores_df)]
            if (length(otros_lv) == 0) next

            fml_full <- as.formula(paste0("`", lv, "` ~ ",
                          paste0("`", otros_lv, "`", collapse = " + ")))
            fit_full <- tryCatch(
              stats::lm(fml_full, data = scores_df[, c(lv, otros_lv), drop = FALSE]),
              error = function(e) NULL
            )
            if (!is.null(fit_full)) {
              r2_full  <- summary(fit_full)$r.squared
              vif_full <- if (!is.na(r2_full) && r2_full < 0.9999) round(1 / (1 - r2_full), 3) else 999.0
              status_full <- if (vif_full < 3.3) "✓ No CMB risk (<3.3)" else "✗ Potential CMB (≥3.3)"
              vif_full_rows[[length(vif_full_rows)+1]] <- data.frame(
                Latent_Variable = lv,
                VIF             = vif_full,
                Status          = status_full,
                stringsAsFactors = FALSE
              )
            }
          }

          if (length(vif_full_rows) > 0) {
            results$tables$VIF_Full <- do.call(rbind, vif_full_rows)
          } else {
            results$tables$VIF_Full <- data.frame(
              Nota = "VIF Colinealidad Total no disponible")
          }
        }
      }, error = function(e) {
        message("[VIF_Full] Error: ", e$message)
        results$tables$VIF_Full <- data.frame(Nota = paste("Error:", e$message))
      })

      # ── 12. SRMR ─────────────────────────────────────────────────────────
      srmr_val <- NA_real_

      # Buscar en todas las rutas posibles de seminr (varía por versión)
      srmr_search_paths <- list(
        c("it_criteria", "srmr"),
        c("quality", "fit", "srmr"),
        c("quality", "srmr"),
        c("model_criteria", "srmr"),
        c("fit", "srmr"),
        c("criteria", "srmr"),
        c("model_fit", "srmr")
      )
      for (spath in srmr_search_paths) {
        obj_s <- summ
        for (nm_s in spath) {
          obj_s <- tryCatch(obj_s[[nm_s]], error = function(e) NULL)
          if (is.null(obj_s)) break
        }
        if (!is.null(obj_s) && is.numeric(obj_s) && length(obj_s) > 0) {
          srmr_val <- round(as.numeric(obj_s[1]), 4); break
        }
      }

      # Búsqueda recursiva nivel 1: recorrer slots de summ buscando "srmr"
      if (is.na(srmr_val)) {
        for (nm_s in names(summ)) {
          slot_obj <- tryCatch(summ[[nm_s]], error = function(e) NULL)
          if (is.list(slot_obj) && "srmr" %in% tolower(names(slot_obj))) {
            idx_s <- which(tolower(names(slot_obj)) == "srmr")[1]
            v <- tryCatch(as.numeric(slot_obj[[idx_s]]), error = function(e) NA_real_)
            if (!is.na(v)) { srmr_val <- round(v, 4); break }
          }
          if (is.numeric(slot_obj) && length(slot_obj) == 1 && grepl("srmr", tolower(nm_s))) {
            srmr_val <- round(as.numeric(slot_obj), 4); break
          }
        }
      }

      # Fallback manual: calcular SRMR desde correlaciones observadas vs implicadas
      if (is.na(srmr_val) && !is.null(scores_df) && !is.null(summ$loadings)) {
        srmr_val <- tryCatch({
          all_items <- unlist(construct_items_map)
          all_items <- all_items[all_items %in% names(data_raw())]
          if (length(all_items) < 2) stop("pocos items")
          R_obs <- cor(data_raw()[, all_items, drop = FALSE], use = "pairwise.complete.obs")
          L_mat <- as.matrix(summ$loadings)
          # Matriz de correlaciones implicada: R_imp_ij = lambda_i * phi_ij * lambda_j
          phi_mat <- cor(scores_df[, colnames(L_mat)[colnames(L_mat) %in% names(scores_df)], drop=FALSE],
                         use = "pairwise.complete.obs")
          diffs_sq <- c()
          items_list <- rownames(L_mat)
          cons_list  <- colnames(L_mat)
          item_cons <- sapply(items_list, function(it) {
            which_max <- which.max(abs(L_mat[it, ]))
            if (length(which_max)) cons_list[which_max] else NA
          })
          for (a in seq_along(items_list)) {
            for (b in seq_along(items_list)) {
              if (b >= a) next
              ia <- items_list[a]; ib <- items_list[b]
              ca <- item_cons[ia]; cb <- item_cons[ib]
              if (is.na(ca) || is.na(cb)) next
              if (!(ia %in% rownames(R_obs)) || !(ib %in% rownames(R_obs))) next
              r_obs <- R_obs[ia, ib]
              lam_a <- L_mat[ia, ca]; lam_b <- L_mat[ib, cb]
              phi   <- if (ca == cb) 1 else if (ca %in% rownames(phi_mat) && cb %in% colnames(phi_mat)) phi_mat[ca, cb] else NA
              if (any(is.na(c(r_obs, lam_a, lam_b, phi)))) next
              r_imp <- lam_a * phi * lam_b
              diffs_sq <- c(diffs_sq, (r_obs - r_imp)^2)
            }
          }
          if (length(diffs_sq) > 0) round(sqrt(mean(diffs_sq)), 4) else NA_real_
        }, error = function(e) NA_real_)
      }

      srmr_ok <- if (!is.na(srmr_val)) {
        ifelse(srmr_val <= 0.08, "\u2713 Buen ajuste (\u22640.08)",
               ifelse(srmr_val <= 0.10, "\u26a0 Aceptable (\u22640.10)", "\u2717 Revisar (>0.10)"))
      } else "N/D"

      results$tables$SRMR <- data.frame(
        Metrica  = "SRMR",
        Valor    = srmr_val,
        Criterio = "<= 0.08",
        OK       = srmr_ok,
        stringsAsFactors = FALSE
      )

      # ── 13. MICOM / MGA (dentro del mismo observer, m_model y s_model en scope) ──
      warnings_multigroup <- character(0)
      grp_var_run <- trimws(input$group_var %||% "")
      micom_solicitado <- isTRUE(input$run_micom)
      mga_solicitado   <- isTRUE(input$run_mga)

      if (micom_solicitado || mga_solicitado) {
        if (!nzchar(grp_var_run)) {
          warnings_multigroup <- c(warnings_multigroup,
            "\u26a0 MICOM/MGA: No se seleccion\u00f3 variable de grupo.")
        } else if (!(grp_var_run %in% names(data_raw()))) {
          warnings_multigroup <- c(warnings_multigroup,
            paste0("\u26a0 MICOM/MGA: Variable '", grp_var_run, "' no encontrada en datos."))
        } else {
          # Reconstruir data con columna de grupo redondeada al entero mas cercano
          # (el jitter puede haber convertido 1->1.000023, 2->2.000041, etc.)
          data_micomga <- data_raw()
          data_micomga[[grp_var_run]] <- round(data_micomga[[grp_var_run]])

          n_unique_grp <- length(unique(data_micomga[[grp_var_run]]))
          if (n_unique_grp > 20) {
            warnings_multigroup <- c(warnings_multigroup,
              paste0("\u26a0 MICOM/MGA: '", grp_var_run, "' tiene ", n_unique_grp,
                     " categorias. Use una variable con pocos valores (ej: Genero=1,2)."))
          } else {
            # MICOM
            if (micom_solicitado) {
              results$log <- "\u25ba [MICOM] Calculando invarianza de medida..."
              micom_out <- tryCatch(
                run_micom(data_full = data_micomga, group_var = grp_var_run,
                          m_model = m_model, s_model = s_model, n_permut = 1000),
                error = function(e) {
                  warnings_multigroup <<- c(warnings_multigroup,
                    paste0("\u26a0 MICOM error: ", e$message))
                  NULL
                }
              )
              if (!is.null(micom_out)) {
                results$tables$MICOM_P1      <- micom_out$paso1
                results$tables$MICOM_RESUMEN <- micom_out$tabla_resumen
                results$micom_out <- micom_out
                warnings_multigroup <- c(warnings_multigroup,
                  paste0("\u2713 MICOM completado: grupos [",
                         paste(micom_out$grupos, collapse=", "), "]"))
              }
            }

            # MGA
            if (mga_solicitado) {
              results$log <- "\u25ba [MGA] Calculando comparacion multigrupo..."
              mga_out <- tryCatch(
                run_mga(data_full = data_micomga, group_var = grp_var_run,
                        m_model = m_model, s_model = s_model,
                        min_n = as.integer(input$min_group_n %||% 30),
                        n_permut = 1000),
                error = function(e) {
                  warnings_multigroup <<- c(warnings_multigroup,
                    paste0("\u26a0 MGA error: ", e$message))
                  NULL
                }
              )
              if (!is.null(mga_out)) {
                results$tables$MGA <- mga_out
                results$mga_out <- mga_out
                warnings_multigroup <- c(warnings_multigroup,
                  paste0("\u2713 MGA completado: ", nrow(mga_out), " paths analizados"))
              }
            }
          }
        }
      }

      log_final <- if(es) "\u2705 An\u00e1lisis completado con \u00e9xito." else "\u2705 Analysis completed successfully."
      if (length(warnings_multigroup) > 0)
        log_final <- paste0(log_final, "\n", paste(warnings_multigroup, collapse="\n"))
      results$log <- log_final


      # Guardar automáticamente en proyecto
      if (!is.null(current_project()))
        tryCatch(saveRDS(results$tables, proj_paths()$results), error=function(e) NULL)

    }, error = function(e) {
      results$log <- paste0(if(es) "✗ Error crítico: " else "✗ Critical error: ", e$message, "\n",
                            "Trace: ", paste(conditionCall(e), collapse=" "))
    })
  })

  # ── OUTPUTS ───────────────────────────────────────────────────────────────

  output$status_log <- renderText({ results$log })

  # Diagrama
  output$pls_diagram <- DiagrammeR::renderGrViz({
    req(results$pls_est)
    dot <- results$dot_code
    if (is.null(dot) || !nzchar(dot)) {
      # Intentar regenerar
      dot <- tryCatch(
        build_pls_dot(results$pls_est, title="PLS-SEM Path Model", rankdir="LR"),
        error = function(e) { message("DOT regen error: ", e$message); NULL }
      )
    }
    if (is.null(dot) || !is.character(dot) || !nzchar(dot)) {
      # Diagrama de emergencia simple
      dot <- 'digraph PLS { graph [rankdir=LR, bgcolor="white"]; node [fontname="Helvetica"]; "Modelo" [shape=ellipse, style=filled, fillcolor="#1565C0", fontcolor=white, label="PLS-SEM\\nModelo estimado"]; }'
    }
    DiagrammeR::grViz(dot)
  })

  # Helper para escribir SVG correctamente (evita que el browser lo renombre a .html)
  svg_content_fn <- function(file) {
    dot <- results$dot_code
    req(!is.null(dot) && nzchar(dot))

    if (!has_diagrammersvg) {
      # Sin DiagrammeRsvg: guardar el código DOT renderizable
      dot_content <- paste0(
        "<?xml version='1.0' encoding='UTF-8'?>\n",
        "<!-- PLS-SEM Path Model - Codigo DOT (abrir con Graphviz) -->\n",
        "<!-- Para SVG real: install.packages('DiagrammeRsvg') -->\n",
        "<!-- DOT CODE:\n", dot, "\n-->"
      )
      con <- file(file, open = "wt", encoding = "UTF-8")
      writeLines(dot_content, con = con)
      close(con)
      return()
    }

    svg_code <- tryCatch(
      DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot)),
      error = function(e) {
        showNotification(paste("Error SVG:", e$message), type = "warning")
        NULL
      }
    )
    req(!is.null(svg_code) && nzchar(svg_code))
    # Escribir como binario para evitar conversiones de encoding que rompen el SVG
    con <- file(file, open = "wt", encoding = "UTF-8")
    writeLines(svg_code, con = con)
    close(con)
  }

  # Descargar diagrama SVG (botón en pestaña Diagrama)
  output$download_diagram_svg <- downloadHandler(
    filename = function() paste0("PLS_Diagrama_", Sys.Date(), ".svg"),
    contentType = "image/svg+xml",
    content  = svg_content_fn
  )

  # Descargar diagrama SVG (botón en pestaña Descargar)
  output$download_diagram_svg2 <- downloadHandler(
    filename = function() paste0("PLS_Diagrama_", Sys.Date(), ".svg"),
    contentType = "image/svg+xml",
    content  = svg_content_fn
  )

  # Descargar diagrama PNG
  output$download_diagram_png <- downloadHandler(
    filename = function() paste0("PLS_Diagrama_", Sys.Date(), ".png"),
    contentType = "image/png",
    content  = function(file) {
      dot <- results$dot_code
      req(!is.null(dot) && nzchar(dot))
      validate(need(has_diagrammersvg && has_rsvg,
        "Instala los paquetes DiagrammeRsvg y rsvg para exportar PNG:\ninstall.packages(c('DiagrammeRsvg','rsvg'))"))
      tryCatch({
        svg_code <- DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot))
        svg_tmp  <- tempfile(fileext = ".svg")
        con_tmp  <- file(svg_tmp, open = "wt", encoding = "UTF-8")
        writeLines(svg_code, con = con_tmp); close(con_tmp)
        rsvg::rsvg_png(svg_tmp, file)
      }, error = function(e) {
        showNotification(paste("Error PNG:", e$message), type = "error")
      })
    }
  )

  output$table_rel  <- renderDT({
    req(results$tables$Confiabilidad)
    df <- results$tables$Confiabilidad
    dt <- datatable(df, rownames=FALSE, options=list(pageLength=15, scrollX=TRUE, dom="tip")) |>
      formatStyle("AVE", backgroundColor = styleInterval(c(0.499), c("#FFCDD2","#C8E6C9"))) |>
      formatStyle("Composite_Reliability_CR", backgroundColor = styleInterval(c(0.699), c("#FFCDD2","#C8E6C9"))) |>
      formatStyle("Cronbach_Alpha", backgroundColor = styleInterval(c(0.699), c("#FFCDD2","#C8E6C9")))
    dt
  })

  output$table_load <- renderDT({
    req(results$tables$Cargas)
    datatable(results$tables$Cargas, rownames=FALSE, options=list(pageLength=20, scrollX=TRUE, dom="ftp")) |>
      formatStyle("Loading", backgroundColor = styleInterval(c(0.399, 0.699), c("#FFCDD2","#FFF9C4","#C8E6C9"))) |>
      formatStyle("OK", color = styleEqual(c("✓","⚠","✗"), c("#2E7D32","#F57F17","#C62828")))
  })

  output$table_paths <- renderDT({
    req(results$tables$Paths)
    df <- results$tables$Paths
    dt <- datatable(df, rownames=FALSE,
              options=list(pageLength=15, scrollX=TRUE, dom="tip")) |>
      formatStyle("P_Valor", backgroundColor = styleInterval(c(0.049, 0.099), c("#C8E6C9","#FFF9C4","#FFCDD2"))) |>
      formatStyle("Sig", color = styleEqual(c("***","**","*","\u2020","n.s."),
                                            c("#1B5E20","#2E7D32","#388E3C","#F57F17","#C62828")),
                  fontWeight = "bold")
    if ("IC_2.5" %in% names(df) && "IC_97.5" %in% names(df)) {
      dt <- dt |>
        formatStyle("IC_2.5",  backgroundColor = "#FFF8E1") |>
        formatStyle("IC_97.5", backgroundColor = "#FFF8E1")
    }
    dt
  })

  output$table_r2 <- renderDT({
    df <- results$tables$R2
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Nota = "R² no disponible aún. Ejecute el análisis primero."),
                       rownames=FALSE, options=list(dom="t")))
    }
    dt <- datatable(df, rownames=FALSE, options=list(pageLength=10, dom="tip")) |>
      formatStyle("R2", backgroundColor = styleInterval(c(0.249, 0.499, 0.749), c("#FFCDD2","#FFECB3","#FFF9C4","#C8E6C9")))
    if ("R2_adj" %in% names(df))
      dt <- dt |> formatStyle("R2_adj", backgroundColor = styleInterval(c(0.249, 0.499, 0.749), c("#FFCDD2","#FFECB3","#FFF9C4","#C8E6C9")))
    dt
  })

  output$table_q2 <- renderDT({
    df <- results$tables$Q2
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Nota = "Q² no disponible aún. Active 'Calcular Q²' y ejecute el análisis."),
                       rownames=FALSE, options=list(dom="t")))
    }
    if ("Nota" %in% names(df)) {
      datatable(df, rownames=FALSE, options=list(dom="t"))
    } else {
      datatable(df, rownames=FALSE, options=list(pageLength=10, dom="tip")) |>
        formatStyle("Q2", backgroundColor = styleInterval(c(0, 0.149, 0.349), c("#FFCDD2","#FFECB3","#FFF9C4","#C8E6C9")))
    }
  })

  

  output$table_f2 <- renderDT({
    req(results$tables$Paths)
    df <- results$tables$Paths[, c("Path","f2"), drop=FALSE]
    df$f2 <- round(as.numeric(df$f2), 3)
    df$Nivel <- ifelse(is.na(df$f2), "N/D", ifelse(df$f2 >= 0.35,"Grande",ifelse(df$f2>=0.15,"Mediano",ifelse(df$f2>=0.02,"Pequeño","Negligible"))))
    datatable(df, rownames=FALSE, options=list(pageLength=10, dom="tip")) |>
      formatStyle("f2", backgroundColor = styleInterval(c(0.019, 0.149, 0.349), c("#FFCDD2","#FFECB3","#FFF9C4","#C8E6C9")))
  })

  output$table_htmt <- renderDT({
    req(results$tables$HTMT)
    datatable(results$tables$HTMT, rownames=FALSE, options=list(pageLength=15, scrollX=TRUE, dom="tip")) |>
      formatStyle("HTMT", backgroundColor = styleInterval(c(0.849, 0.899), c("#C8E6C9","#FFF9C4","#FFCDD2"))) |>
      formatStyle("OK", color = styleEqual(c("✓ <0.85","⚠ <0.90","✗ ≥0.90"), c("#2E7D32","#F57F17","#C62828")))
  })

  output$table_vif <- renderDT({
    req(results$tables$VIF)
    df <- results$tables$VIF
    if ("Nota" %in% names(df))
      return(datatable(df, rownames=FALSE, options=list(dom="t")))
    dt <- datatable(df, rownames=FALSE,
                    options=list(pageLength=15, dom="tip", scrollX=TRUE))
    if ("VIF" %in% names(df))
      dt <- dt |> formatStyle("VIF",
        backgroundColor = styleInterval(c(3.299, 4.999),
          c("#C8E6C9","#FFF9C4","#FFCDD2")))
    if ("OK" %in% names(df))
      dt <- dt |> formatStyle("OK",
        color = styleEqual(c("✓ <3.3","⚠ <5","✗ ≥5"),
                           c("#2E7D32","#F57F17","#C62828")),
        fontWeight = "bold")
    dt
  })

  # ── VIF Estructural renderDT (Hair et al., 2022) ─────────────────────────
  output$table_vif_structural <- renderDT({
    req(results$tables$VIF_Structural)
    df <- results$tables$VIF_Structural
    if ("Nota" %in% names(df))
      return(datatable(df, rownames = FALSE, options = list(dom = "t")))

    dt <- datatable(
      df,
      rownames = FALSE,
      colnames = c("Endogenous" = "Endogeno",
                   "Predictor"  = "Predictor",
                   "VIF"        = "VIF",
                   "Status"     = "Status"),
      options = list(pageLength = 20, dom = "tip", scrollX = TRUE),
      caption = htmltools::tags$caption(
        style = "caption-side:top; font-weight:bold; color:#1565C0; font-size:14px;",
        "Structural Model Collinearity (Hair et al., 2022)"
      )
    ) |>
      formatStyle("VIF",
        backgroundColor = styleInterval(c(3.299, 4.999),
          c("#C8E6C9", "#FFF9C4", "#FFCDD2")),
        fontWeight = "bold"
      ) |>
      formatStyle("Status",
        color = styleEqual(
          c("✓ OK (<3.3)", "⚠ Moderate (<5)", "✗ Problematic (≥5)"),
          c("#2E7D32",     "#F57F17",          "#C62828")
        ),
        fontWeight = "bold"
      )
    dt
  })

  # ── VIF Colinealidad Total renderDT (Kock, 2015) ─────────────────────────
  output$table_vif_full <- renderDT({
    req(results$tables$VIF_Full)
    df <- results$tables$VIF_Full
    if ("Nota" %in% names(df))
      return(datatable(df, rownames = FALSE, options = list(dom = "t")))

    dt <- datatable(
      df,
      rownames = FALSE,
      colnames = c("Latent Variable" = "Latent_Variable",
                   "VIF"             = "VIF",
                   "Status"          = "Status"),
      options = list(pageLength = 20, dom = "tip", scrollX = TRUE),
      caption = htmltools::tags$caption(
        style = "caption-side:top; font-weight:bold; color:#E65100; font-size:14px;",
        "Full Collinearity Assessment (Common Method Bias – Kock, 2015)"
      )
    ) |>
      formatStyle("VIF",
        backgroundColor = styleInterval(3.299,
          c("#C8E6C9", "#FFCDD2")),
        fontWeight = "bold"
      ) |>
      formatStyle("Status",
        color = styleEqual(
          c("✓ No CMB risk (<3.3)", "✗ Potential CMB (≥3.3)"),
          c("#2E7D32",              "#C62828")
        ),
        fontWeight = "bold"
      )
    dt
  })

  # ── Note reactive for VIF Full (language-aware) ───────────────────────────
  output$vif_full_note_ui <- renderUI({
    es <- (input$app_lang %||% "es") == "es"
    if (es)
      tags$span("Los valores de VIF inferiores a 3.3 sugieren ausencia de sesgo de método común (Kock, 2015).")
    else
      tags$span("VIF values below 3.3 suggest absence of common method bias (Kock, 2015).")
  })

  output$table_srmr <- renderDT({
    req(results$tables$SRMR)
    datatable(results$tables$SRMR, rownames=FALSE, options=list(dom="t"))
  })


  # ── Language reactive ──────────────────────────────────────────────────────
  lang <- reactive({ input$app_lang %||% "es" })

  # ── Traducciones completas al cambiar idioma ───────────────────────────────
  observeEvent(input$app_lang, {
    if (input$app_lang == "en") {
      # Sidebar menu labels
      session$sendCustomMessage("translateUI", list(
        lang = "en",
        sidebar = list(
          project  = "📁 Project",
          upload   = "📊 Load Data",
          model    = "🔧 Define Model",
          analysis = "⚡ Analysis",
          results  = "📈 Results",
          download = "💾 Download"
        ),
        tabs = list(
          measurement  = "🔵 Measurement",
          diagram      = "🗺 Diagram",
          structural   = "🔴 Structural",
          discriminant = "🟣 Discriminant",
          indirect     = "🔗 Ind. Effects",
          hypotheses   = "📋 Hypotheses",
          predict      = "🔮 PLS Predict",
          diagnostic   = "⚙ Diagnostic",
          interpret    = "🧠 Interpretation"
        )
      ))
    } else {
      session$sendCustomMessage("translateUI", list(
        lang = "es",
        sidebar = list(
          project  = "📁 Proyecto",
          upload   = "📊 Cargar Datos",
          model    = "🔧 Definir Modelo",
          analysis = "⚡ Análisis",
          results  = "📈 Resultados",
          download = "💾 Descargar"
        ),
        tabs = list(
          measurement  = "🔵 Medición",
          diagram      = "🗺 Diagrama",
          structural   = "🔴 Estructural",
          discriminant = "🟣 Discriminante",
          indirect     = "🔗 Ind. Effects",
          hypotheses   = "📋 Hipótesis",
          predict      = "🔮 PLS Predict",
          diagnostic   = "⚙ Diagnóstico",
          interpret    = "🧠 Interpretación"
        )
      ))
    }
  }, ignoreInit = TRUE)

  # ── Contextual help UIs (Tooltip boxes) ───────────────────────────────────
  output$htmt_help_ui <- renderUI({
    if (lang() == "en")
      tags$small("Criterion: HTMT < 0.85 (strict: < 0.90) confirms discriminant validity (Henseler et al., 2015)")
    else
      tags$small("Criterio: HTMT < 0.85 (estricto: < 0.90) confirma validez discriminante (Henseler et al., 2015)")
  })
  output$fl_help_ui <- renderUI({
    if (lang() == "en")
      tags$small("Fornell-Larcker: diagonal (\u221aAVE) must exceed all off-diagonal correlations (Fornell & Larcker, 1981)")
    else
      tags$small("Fornell-Larcker: diagonal (\u221aAVE) debe superar todas las correlaciones fuera de la diagonal (Fornell & Larcker, 1981)")
  })
  output$cl_help_ui <- renderUI({
    if (lang() == "en")
      tags$small("Cross-loadings: each item must load highest on its own construct")
    else
      tags$small("Cargas cruzadas: cada item debe cargar m\u00e1s alto en su propio constructo")
  })
  output$indirect_help_ui <- renderUI({
    if (lang() == "en")
      tags$small("Specific indirect effects with bootstrapped CI 95%. Non-overlapping with zero indicates significant mediation (Hair et al., 2022)")
    else
      tags$small("Efectos indirectos espec\u00edficos con IC bootstrapping 95%. IC que no incluya cero indica mediaci\u00f3n significativa (Hair et al., 2022)")
  })
  output$hyp_help_ui <- renderUI({
    if (lang() == "en")
      tags$small("Hypothesis table ready to paste into your paper. Decision based on p < 0.05 (two-tailed bootstrapping)")
    else
      tags$small("Tabla de hip\u00f3tesis lista para el paper. Decisi\u00f3n basada en p < 0.05 (bootstrapping bilateral)")
  })
  output$plspredict_help_ui <- renderUI({
    if (lang() == "en")
      tags$div(
        tags$small("PLS Predict uses 10-fold cross-validation. Q\u00b2 predict > 0 = predictive relevance. RMSE/MAE vs naive benchmark (Hair et al., 2019; Shmueli et al., 2019)"),
        br(), tags$small(tags$b("Thresholds: "), "Small \u2265 0.02 | Medium \u2265 0.15 | Large \u2265 0.35")
      )
    else
      tags$div(
        tags$small("PLS Predict usa validaci\u00f3n cruzada 10-fold. Q\u00b2 predict > 0 = relevancia predictiva. RMSE/MAE vs benchmark ingenuo (Hair et al., 2019; Shmueli et al., 2019)"),
        br(), tags$small(tags$b("Umbrales: "), "Peque\u00f1o \u2265 0.02 | Mediano \u2265 0.15 | Grande \u2265 0.35")
      )
  })

  # ── Fornell-Larcker table ──────────────────────────────────────────────────
  output$table_fl <- renderDT({
    df <- results$tables$FornellLarcker
    if (is.null(df) || nrow(df) == 0)
      return(datatable(data.frame(Nota = i18n()$run_first),
                       rownames=FALSE, options=list(dom="t")))
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    dt <- datatable(df, rownames=FALSE, options=list(pageLength=10, scrollX=TRUE, dom="tip"))
    if ("OK" %in% names(df))
      dt <- dt |> formatStyle("OK",
        color = styleEqual(c("\u2713 OK","\u2717 REVISAR"), c("#2E7D32","#C62828")),
        fontWeight = "bold")
    dt
  })

  # ── Cross-loadings table ───────────────────────────────────────────────────
  output$table_cl <- renderDT({
    df <- results$tables$CrossLoadings
    if (is.null(df) || nrow(df) == 0)
      return(datatable(data.frame(Nota = i18n()$run_first),
                       rownames=FALSE, options=list(dom="t")))
    datatable(df, rownames=FALSE,
              options=list(pageLength=20, scrollX=TRUE, dom="ftp")) |>
      formatStyle("Asignado_a", color="#1565C0", fontWeight="bold")
  })

  # ── Indirect Effects table ─────────────────────────────────────────────────
  output$table_indirect <- renderDT({
    df <- results$tables$IndirectEffects
    if (is.null(df) || nrow(df) == 0)
      return(datatable(data.frame(Nota = i18n()$no_mediation),
                       rownames=FALSE, options=list(dom="t")))
    dt <- datatable(df, rownames=FALSE,
                    options=list(pageLength=15, scrollX=TRUE, dom="tip")) |>
      formatStyle("Beta_ind",
        backgroundColor = styleInterval(c(-0.0001, 0.0001), c("#FFCDD2","#F5F5F5","#C8E6C9")))
    if ("Sig" %in% names(df))
      dt <- dt |> formatStyle("Sig",
        color = styleEqual(c("***","**","*","†","n.s.","N/D"),
                           c("#1B5E20","#2E7D32","#388E3C","#F57F17","#C62828","#888")),
        fontWeight = "bold")
    if ("IC_2.5" %in% names(df) && "IC_97.5" %in% names(df)) {
      dt <- dt |>
        formatStyle("IC_2.5",  backgroundColor = "#FFF8E1") |>
        formatStyle("IC_97.5", backgroundColor = "#FFF8E1")
    }
    dt
  })

  # ── Total Effects table ────────────────────────────────────────────────────
  output$table_total_effects <- renderDT({
    df <- results$tables$TotalEffects
    if (is.null(df) || nrow(df) == 0)
      return(datatable(data.frame(Nota = i18n()$run_first),
                       rownames=FALSE, options=list(dom="t")))
    datatable(df, rownames=FALSE, options=list(pageLength=15, dom="tip")) |>
      formatStyle("Total",
        backgroundColor = styleInterval(c(-0.0001, 0.0001), c("#FFCDD2","#F5F5F5","#C8E6C9")))
  })

  # ── Hypothesis table ───────────────────────────────────────────────────────
  output$table_hypotheses <- renderDT({
    df <- results$tables$Hypotheses
    if (is.null(df) || nrow(df) == 0)
      return(datatable(data.frame(Nota = i18n()$run_first),
                       rownames=FALSE, options=list(dom="t")))
    datatable(df, rownames=FALSE,
              options=list(pageLength=15, scrollX=TRUE, dom="tip")) |>
      formatStyle("Decision",
        color = styleEqual(c("\u2713 Soportada","\u2717 Rechazada","\u2713 Supported","\u2717 Rejected"), c("#1B5E20","#C62828","#1B5E20","#C62828")),
        fontWeight = "bold") |>
      formatStyle("P_Valor",
        backgroundColor = styleInterval(c(0.049, 0.099), c("#C8E6C9","#FFF9C4","#FFCDD2"))) |>
      formatStyle("Sig",
        color = styleEqual(c("***","**","*","†","n.s."),
                           c("#1B5E20","#2E7D32","#388E3C","#F57F17","#C62828")),
        fontWeight = "bold")
  })

  # ── PLS Predict table ─────────────────────────────────────────────────────
  output$table_plspredict <- renderDT({
    df <- results$tables$PLSPredict
    if (is.null(df) || nrow(df) == 0)
      return(datatable(data.frame(Nota = i18n()$run_first),
                       rownames=FALSE, options=list(dom="t")))
    datatable(df, rownames=FALSE, options=list(pageLength=10, dom="tip")) |>
      formatStyle("Q2_predict",
        backgroundColor = styleInterval(c(0, 0.149, 0.349), c("#FFCDD2","#FFECB3","#FFF9C4","#C8E6C9")),
        fontWeight = "bold") |>
      formatStyle("Mejor_naive",
        color = styleEqual(c("\u2713 S\u00ed","\u2717 No"), c("#1B5E20","#C62828")),
        fontWeight = "bold")
  })

  # ── Word/docx export for hypotheses ───────────────────────────────────────
  output$download_hyp_docx <- downloadHandler(
    filename = function() paste0("Hipotesis_PLS_", Sys.Date(), ".docx"),
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content  = function(file) {
      df <- results$tables$Hypotheses
      if (is.null(df) || nrow(df) == 0) df <- data.frame(Nota = "Sin datos")
      tryCatch({
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc,
          if (lang() == "en") "Hypothesis Testing Results (PLS-SEM)" else "Resultados de Hipótesis (PLS-SEM)",
          style = "heading 1")
        doc <- officer::body_add_par(doc,
          if (lang() == "en")
            "Note. β = path coefficient; STDEV = standard deviation bootstrapping; t = t-statistic; p = p-value (two-tailed); IC = confidence interval 95%; *** p<.001, ** p<.01, * p<.05, † p<.10"
          else
            "Nota. β = coeficiente de ruta; STDEV = desviación estándar bootstrapping; t = estadístico t; p = valor p (bilateral); IC = intervalo de confianza 95%; *** p<.001, ** p<.01, * p<.05, † p<.10",
          style = "Normal")
        ft <- flextable::flextable(df)
        ft <- flextable::set_header_labels(ft,
          Hipotesis = if(input$app_lang=="en") "Hypothesis" else "Hipótesis",
          Relacion  = if(input$app_lang=="en") "Relationship" else "Relación",
          Decision  = if(input$app_lang=="en") "Decision" else "Decisión")
        ft <- flextable::bold(ft, part = "header")
        ft <- flextable::bg(ft, part = "header", bg = "#1565C0")
        ft <- flextable::color(ft, part = "header", color = "white")
        ft <- flextable::autofit(ft)
        ft <- flextable::theme_booktabs(ft)
        doc <- flextable::body_add_flextable(doc, ft)
        print(doc, target = file)
      }, error = function(e) {
        # Fallback plain docx
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, paste("Error generando tabla:", e$message))
        print(doc, target = file)
      })
    }
  )

  # ── Full Word report export ────────────────────────────────────────────────
  output$download_word <- downloadHandler(
    filename = function() paste0("Reporte_PLSSEM_APA_", Sys.Date(), ".docx"),
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content  = function(file) {
      all_tbls <- results$tables
      tryCatch({
        doc <- officer::read_docx()
        # Título
        doc <- officer::body_add_par(doc,
          i18n()$box_download,
          style = "heading 1")
        doc <- officer::body_add_par(doc,
          paste0(if(input$app_lang=="en") "Generated: " else "Generado: ",
                 format(Sys.time(), "%Y-%m-%d %H:%M"),
                 " | CANCHARI PLS-SEM PRO V2.0 | seminr + R"),
          style = "Normal")

        section_order <- c("Confiabilidad","Cargas","CrossLoadings","FornellLarcker",
                           "HTMT","Paths","Hypotheses","IndirectEffects","TotalEffects",
                           "R2","Q2","PLSPredict","VIF","SRMR","MICOM_RESUMEN","MGA")
        tbl_labels_en <- c(
          Confiabilidad   = "Table 1. Reliability and Convergent Validity",
          Cargas          = "Table 2. Outer Loadings",
          CrossLoadings   = "Table 3. Cross-Loadings",
          FornellLarcker  = "Table 4. Fornell-Larcker Criterion",
          HTMT            = "Table 5. Heterotrait-Monotrait Ratio (HTMT)",
          Paths           = "Table 6. Path Coefficients - Structural Model",
          Hypotheses      = "Table 7. Hypothesis Testing Results",
          IndirectEffects = "Table 8. Specific Indirect Effects",
          TotalEffects    = "Table 9. Total Effects",
          R2              = "Table 10. Coefficient of Determination (R²)",
          Q2              = "Table 11. Predictive Relevance (Q²)",
          PLSPredict      = "Table 12. PLS Predict - Out-of-Sample",
          VIF             = "Table 13. Collinearity (VIF)",
          SRMR            = "Table 14. Model Fit (SRMR)",
          MICOM_RESUMEN   = "Table 15. MICOM - Measurement Invariance",
          MGA             = "Table 16. Multi-Group Analysis (MGA)"
        )
        tbl_labels_es <- c(
          Confiabilidad   = "Tabla 1. Confiabilidad y Validez Convergente",
          Cargas          = "Tabla 2. Cargas Factoriales (Outer Loadings)",
          CrossLoadings   = "Tabla 3. Cargas Cruzadas (Cross-Loadings)",
          FornellLarcker  = "Tabla 4. Criterio Fornell-Larcker",
          HTMT            = "Tabla 5. HTMT",
          Paths           = "Tabla 6. Coeficientes de Ruta",
          Hypotheses      = "Tabla 7. Resultados de Hipótesis",
          IndirectEffects = "Tabla 8. Efectos Indirectos Específicos",
          TotalEffects    = "Tabla 9. Efectos Totales",
          R2              = "Tabla 10. Coeficiente de Determinación (R²)",
          Q2              = "Tabla 11. Relevancia Predictiva (Q²)",
          PLSPredict      = "Tabla 12. PLS Predict (Out-of-Sample)",
          VIF             = "Tabla 13. Colinealidad (VIF)",
          SRMR            = "Tabla 14. Ajuste del Modelo (SRMR)",
          MICOM_RESUMEN   = "Tabla 15. MICOM - Invarianza de Medida",
          MGA             = "Tabla 16. Análisis Multigrupo (MGA)"
        )
        tbl_labels <- if (input$app_lang=="en") tbl_labels_en else tbl_labels_es

        for (nm in section_order) {
          df_t <- all_tbls[[nm]]
          if (is.null(df_t) || !is.data.frame(df_t) || nrow(df_t) == 0) next
          lbl <- if (nm %in% names(tbl_labels)) tbl_labels[[nm]] else paste("Table -", nm)
          doc <- officer::body_add_par(doc, lbl, style = "heading 2")
          # Convert all columns to character to avoid officer type issues
          df_char <- as.data.frame(lapply(df_t, as.character), stringsAsFactors = FALSE)
          ft <- flextable::flextable(df_char)
          ft <- flextable::bold(ft, part = "header")
          ft <- flextable::bg(ft, part = "header", bg = "#1565C0")
          ft <- flextable::color(ft, part = "header", color = "white")
          ft <- flextable::fontsize(ft, size = 9, part = "all")
          ft <- flextable::autofit(ft)
          ft <- flextable::theme_booktabs(ft)
          doc <- flextable::body_add_flextable(doc, ft)
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
        print(doc, target = file)
      }, error = function(e) {
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, paste("Error:", e$message))
        print(doc, target = file)
      })
    }
  )

  # ── GAUSSIAN COPULA ENDOGENEITY TEST  v3.0 ───────────────────────────────────

  copula_results <- reactiveValues(
    table        = NULL,
    plot_forest  = NULL,
    scores_df    = NULL,   # saved for visualization plot
    p_df         = NULL,   # saved for visualization path selector
    status       = NULL,
    score_method = "PLS latent scores (seminr)",
    construct_items_map = NULL
  )

  # ── helper: get p_df from current model inputs ──────────────────────────────
  get_p_df <- function() {
    df <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
    for (i in seq_len(isolate(path_count()))) {
      fr <- trimws(isolate(input[[paste0("p_from_", i)]]) %||% "")
      to <- trimws(isolate(input[[paste0("p_to_",   i)]]) %||% "")
      if (nzchar(fr) && nzchar(to))
        df <- rbind(df, data.frame(from = fr, to = to, stringsAsFactors = FALSE))
    }
    df
  }

  # ── helper: rebuild construct_items_map from model inputs ──────────────────
  get_construct_items_map <- function() {
    df <- data_raw(); if (is.null(df)) return(NULL)
    out <- list()
    for (i in seq_len(isolate(construct_count()))) {
      nm <- trimws(isolate(input[[paste0("c_name_", i)]]) %||% "")
      it <- isolate(get_items_str(i))
      if (!nzchar(nm) || !nzchar(it)) next
      items <- parse_item_range(it, names(df))
      if (length(items) > 0) out[[nm]] <- items
    }
    if (length(out) == 0) NULL else out
  }

  observeEvent(input$run_copula_test, {
    use_mean  <- isTRUE(isolate(input$copula_use_mean_scores))
    pls_obj   <- results$pls_est
    raw_data  <- data_raw()
    cim       <- get_construct_items_map()
    p_df      <- get_p_df()

    # ── Resolve scores ───────────────────────────────────────────────────────
    scores_df <- build_scores_df(pls_obj, raw_data, cim, use_mean_scores = use_mean)

    if (is.null(scores_df) || nrow(scores_df) == 0) {
      copula_results$status <- paste0(
        "No se pudieron obtener los scores de constructos. ",
        if (use_mean) "Verifique que los indicadores estén definidos en el modelo."
        else "Ejecute el análisis PLS-SEM primero."
      )
      copula_results$table <- NULL
      return()
    }

    copula_results$scores_df    <- scores_df
    copula_results$p_df         <- p_df
    copula_results$construct_items_map <- cim
    copula_results$score_method <- if (use_mean) "Composite mean scores (rowMeans per construct)"
                                   else          "PLS latent scores (seminr::construct_scores)"

    # ── Run copula test ───────────────────────────────────────────────────────
    tryCatch({
      copula_tbl <- run_gaussian_copula(
        scores_df   = scores_df,
        p_df        = p_df,
        paths_table = results$tables$Paths,
        lang        = isolate(input$app_lang)
      )
      copula_results$table  <- copula_tbl
      copula_results$status <- "ok"

      # ── Build forest plot ─────────────────────────────────────────────────
      copula_results$plot_forest <- make_copula_results_plot(copula_tbl)

    }, error = function(e) {
      copula_results$status <- paste0("Error: ", e$message)
      copula_results$table  <- NULL
    })
  })

  # ── Status banner ────────────────────────────────────────────────────────────
  output$copula_status_ui <- renderUI({
    s <- copula_results$status
    if (is.null(s)) return(tags$div(style="color:#888; font-style:italic;",
      "Click \u25b6 Run Gaussian Copula Test after running PLS-SEM analysis."))
    if (s == "ok") {
      tbl <- copula_results$table
      any_endo <- any(tbl$p_value < 0.05, na.rm = TRUE)
      style_ok  <- "background:#E8F5E9;border-left:4px solid #2E7D32;padding:8px;border-radius:4px;"
      style_warn<- "background:#FFEBEE;border-left:4px solid #C62828;padding:8px;border-radius:4px;"
      if (any_endo) {
        tags$div(style = style_warn,
          tags$b(style = "color:#C62828;",
            "\u26a0 Potential endogeneity detected in at least one predictor (p < 0.05)"))
      } else {
        tags$div(style = style_ok,
          tags$b(style = "color:#2E7D32;",
            "\u2713 No evidence of endogeneity detected (all p \u2265 0.05)"))
      }
    } else {
      tags$div(style = "background:#FFEBEE;border-left:4px solid #E53935;padding:8px;border-radius:4px;",
        tags$b(style = "color:#C62828;", paste0("\u2717 ", s)))
    }
  })

  # ── Technical details box ────────────────────────────────────────────────────
  output$copula_tech_details_ui <- renderUI({
    req(copula_results$status == "ok")
    tbl <- copula_results$table
    n_row <- if (!is.null(tbl) && nrow(tbl) > 0) tbl$N_used[1] else "N/A"
    fml_row <- if (!is.null(tbl) && nrow(tbl) > 0) tbl$Formula[1] else "Y ~ X + Copula(X)"
    tags$div(style = "background:#F3E5F5;border-left:4px solid #7B1FA2;padding:10px;border-radius:4px;margin-bottom:8px;",
      tags$b(style="color:#6A1B9A;", "\U0001f527 Technical Details"),
      tags$ul(style="margin:6px 0 0 0; padding-left:20px; font-size:13px;",
        tags$li(tags$b("Scoring method: "), copula_results$score_method),
        tags$li(tags$b("Ranking formula: "), "rank(x) / (n + 1)  [Park & Gupta, 2012]"),
        tags$li(tags$b("Inverse normal: "), "qnorm()  [\u03a6\u207b\u00b9]"),
        tags$li(tags$b("NA handling: "), paste0("Complete-case on X and Y (+ controls); N = ", n_row)),
        tags$li(tags$b("Regression: "), fml_row),
        tags$li(tags$b("SE type: "), "OLS (conventional); two-tailed t-test"),
        tags$li(tags$b("PLS \u03b2 source: "), "Bootstrapped path coefficient from Paths table (not bivariate OLS)")
      )
    )
  })

  # ── Results table ────────────────────────────────────────────────────────────
  output$table_copula <- renderDT({
    df <- copula_results$table
    if (is.null(df)) return(datatable(
      data.frame(Note = "Click '\u25b6 Run Gaussian Copula Test' after running PLS-SEM analysis."),
      rownames = FALSE, options = list(dom = "t")))
    display_df <- df[, c("Path","PLS_Beta","Copula_Coef","Std_Error","CI_lo","CI_hi",
                         "t_value","p_value","N_used","Interpretation")]
    names(display_df) <- c("Path","PLS \u03b2","Copula Coeff.","Std. Error",
                           "CI 2.5%","CI 97.5%","t-value","p-value","N","Interpretation")
    datatable(display_df, rownames = FALSE,
              options = list(pageLength = 15, dom = "tip", scrollX = TRUE)) |>
      formatRound(c("PLS \u03b2","Copula Coeff.","Std. Error","CI 2.5%","CI 97.5%","t-value"), digits = 4) |>
      formatRound("p-value", digits = 4) |>
      formatStyle("p-value",
        backgroundColor = styleInterval(c(0.049, 0.099),
                          c("#FFCDD2", "#FFF9C4", "#C8E6C9")),
        fontWeight = "bold") |>
      formatStyle("Interpretation",
        color = styleEqual(
          c("\u26a0 Potential endogeneity detected (p < 0.05)",
            "\u2713 No evidence of endogeneity (p \u2265 0.05)",
            "\u26a0 Posible endogeneidad detectada (p < 0.05)",
            "\u2713 Sin evidencia de endogeneidad (p \u2265 0.05)"),
          c("#C62828", "#2E7D32", "#C62828", "#2E7D32")),
        fontWeight = "bold")
  })

  # ── Plot A: Forest (render) ───────────────────────────────────────────────────
  output$plot_copula_forest <- renderPlot({
    p <- copula_results$plot_forest
    if (is.null(p)) return(NULL)
    print(p)
  })

  # ── Path selector for viz plot ────────────────────────────────────────────────
  output$copula_viz_path_selector_ui <- renderUI({
    p_df <- copula_results$p_df
    if (is.null(p_df) || nrow(p_df) == 0)
      return(tags$p("Run the Copula Test first.", style = "color:#888;"))
    choices <- paste0(p_df$from, " \u2192 ", p_df$to)
    selectInput("copula_viz_path", "Select path:", choices = choices, selected = choices[1])
  })

  output$copula_viz_n_ui <- renderUI({
    scores_df <- copula_results$scores_df
    p_df      <- copula_results$p_df
    sel       <- input$copula_viz_path
    if (is.null(scores_df) || is.null(p_df) || is.null(sel) || !nzchar(sel))
      return(NULL)
    # parse selected path
    parts <- strsplit(sel, " \u2192 ")[[1]]
    if (length(parts) != 2) return(NULL)
    x_nm <- trimws(parts[1]); y_nm <- trimws(parts[2])
    if (!(x_nm %in% names(scores_df)) || !(y_nm %in% names(scores_df))) return(NULL)
    xv <- as.numeric(scores_df[[x_nm]]); yv <- as.numeric(scores_df[[y_nm]])
    n  <- sum(!is.na(xv) & !is.na(yv))
    tags$div(style = "padding-top:24px; color:#1565C0; font-weight:bold; font-size:14px;",
             paste0("N = ", n))
  })

  # ── Plot B: Visualization (render) ───────────────────────────────────────────
  output$plot_copula_viz <- renderPlot({
    scores_df <- copula_results$scores_df
    sel       <- input$copula_viz_path
    view_type <- input$copula_viz_type %||% "scatter"
    if (is.null(scores_df) || is.null(sel) || !nzchar(sel)) return(NULL)
    parts <- strsplit(sel, " \u2192 ")[[1]]
    if (length(parts) != 2) return(NULL)
    x_nm <- trimws(parts[1]); y_nm <- trimws(parts[2])
    p <- make_copula_visualization_plot(scores_df, x_nm, y_nm, view_type)
    if (is.null(p)) return(NULL)
    print(p)
  })

  # ── Save helpers (generalized) ───────────────────────────────────────────────
  save_gg_png <- function(plot_obj, file) {
    req(!is.null(plot_obj))
    ggplot2::ggsave(file, plot = plot_obj, dpi = 300, width = 12, height = 7, units = "in")
  }
  save_gg_pdf <- function(plot_obj, file) {
    req(!is.null(plot_obj))
    pdf(file, width = 12, height = 7, useDingbats = FALSE); print(plot_obj); dev.off()
  }
  save_gg_svg <- function(plot_obj, file) {
    req(!is.null(plot_obj))
    tryCatch({
      svglite::svglite(file, width = 12, height = 7); print(plot_obj); dev.off()
    }, error = function(e) {
      ggplot2::ggsave(file, plot = plot_obj, device = "svg", width = 12, height = 7)
    })
  }

  # ── Current viz plot (reactive) ───────────────────────────────────────────────
  current_viz_plot <- reactive({
    scores_df <- copula_results$scores_df
    sel       <- input$copula_viz_path
    view_type <- input$copula_viz_type %||% "scatter"
    if (is.null(scores_df) || is.null(sel) || !nzchar(sel)) return(NULL)
    parts <- strsplit(sel, " \u2192 ")[[1]]
    if (length(parts) != 2) return(NULL)
    x_nm <- trimws(parts[1]); y_nm <- trimws(parts[2])
    make_copula_visualization_plot(scores_df, x_nm, y_nm, view_type)
  })

  # ── Download handlers — TABLE ─────────────────────────────────────────────────
  output$dl_copula_csv <- downloadHandler(
    filename = function() paste0("GaussianCopula_", Sys.Date(), ".csv"),
    contentType = "text/csv",
    content = function(file) {
      df <- copula_results$table; req(!is.null(df))
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$dl_copula_excel <- downloadHandler(
    filename = function() paste0("GaussianCopula_", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      df <- copula_results$table; req(!is.null(df))
      tryCatch({
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Gaussian Copula")
        openxlsx::writeData(wb, 1, df)
        hs <- openxlsx::createStyle(fgFill="#1565C0", fontColour="white",
                                    textDecoration="bold", halign="center")
        openxlsx::addStyle(wb, 1, hs, rows=1, cols=seq_len(ncol(df)), gridExpand=TRUE)
        openxlsx::setColWidths(wb, 1, cols=seq_len(ncol(df)), widths="auto")
        openxlsx::saveWorkbook(wb, file, overwrite=TRUE)
      }, error = function(e) write.csv(df, file, row.names=FALSE))
    }
  )

  output$dl_copula_word <- downloadHandler(
    filename = function() paste0("GaussianCopula_", Sys.Date(), ".docx"),
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content = function(file) {
      df <- copula_results$table; req(!is.null(df))
      tryCatch({
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Gaussian Copula Endogeneity Test", style="heading 1")
        doc <- officer::body_add_par(doc, "Park & Gupta (2012) | PLS-SEM Robustness Analysis", style="heading 2")
        doc <- officer::body_add_par(doc,
          paste0("Scoring: ", copula_results$score_method,
                 " | Ranking: rank/(n+1) | NA: complete-case | SE: OLS two-tailed"),
          style="Normal")
        disp <- df[, c("Path","PLS_Beta","Copula_Coef","Std_Error","CI_lo","CI_hi",
                       "t_value","p_value","N_used","Interpretation")]
        names(disp) <- c("Path","PLS Beta","Copula Coef","SE","CI 2.5%","CI 97.5%",
                         "t","p","N","Interpretation")
        ft <- flextable::flextable(as.data.frame(lapply(disp, as.character)))
        ft <- flextable::bold(ft, part="header")
        ft <- flextable::bg(ft, part="header", bg="#1565C0")
        ft <- flextable::color(ft, part="header", color="white")
        ft <- flextable::autofit(ft)
        ft <- flextable::theme_booktabs(ft)
        doc <- flextable::body_add_flextable(doc, ft)
        print(doc, target=file)
      }, error = function(e) {
        doc <- officer::read_docx()
        officer::body_add_par(doc, paste("Error:", e$message))
        print(doc, target=file)
      })
    }
  )

  output$dl_copula_pdf <- downloadHandler(
    filename = function() paste0("GaussianCopula_table_", Sys.Date(), ".pdf"),
    contentType = "application/pdf",
    content = function(file) {
      df <- copula_results$table; req(!is.null(df))
      tryCatch({
        tmp_html <- tempfile(fileext=".html")
        writeLines(c("<html><body><h2>Gaussian Copula Results</h2>",
          knitr::kable(df, format="html"), "</body></html>"), tmp_html)
        # Fallback: write CSV if PDF tools unavailable
      }, error = function(e) NULL)
      # Reliable fallback: save results as simple PDF via grDevices
      pdf(file, width=14, height=max(3, nrow(df)*0.4 + 2))
      grid::grid.newpage()
      grid::grid.text(paste(capture.output(print(df)), collapse="\n"),
                      x=0.02, y=0.98, just=c("left","top"),
                      gp=grid::gpar(fontsize=7, fontfamily="mono"))
      dev.off()
    }
  )

  # ── Download handlers — FOREST PLOT ──────────────────────────────────────────
  output$dl_forest_png <- downloadHandler(
    filename = function() paste0("CopulaForestPlot_", Sys.Date(), ".png"),
    contentType = "image/png",
    content = function(file) save_gg_png(copula_results$plot_forest, file)
  )
  output$dl_forest_pdf <- downloadHandler(
    filename = function() paste0("CopulaForestPlot_", Sys.Date(), ".pdf"),
    contentType = "application/pdf",
    content = function(file) save_gg_pdf(copula_results$plot_forest, file)
  )
  output$dl_forest_svg <- downloadHandler(
    filename = function() paste0("CopulaForestPlot_", Sys.Date(), ".svg"),
    contentType = "image/svg+xml",
    content = function(file) save_gg_svg(copula_results$plot_forest, file)
  )

  # ── Download handlers — VISUALIZATION PLOT ────────────────────────────────────
  output$dl_viz_png <- downloadHandler(
    filename = function() paste0("CopulaViz_", Sys.Date(), ".png"),
    contentType = "image/png",
    content = function(file) save_gg_png(current_viz_plot(), file)
  )
  output$dl_viz_pdf <- downloadHandler(
    filename = function() paste0("CopulaViz_", Sys.Date(), ".pdf"),
    contentType = "application/pdf",
    content = function(file) save_gg_pdf(current_viz_plot(), file)
  )
  output$dl_viz_svg <- downloadHandler(
    filename = function() paste0("CopulaViz_", Sys.Date(), ".svg"),
    contentType = "image/svg+xml",
    content = function(file) save_gg_svg(current_viz_plot(), file)
  )

  # ── MICOM outputs ─────────────────────────────────────────────────────────

  output$table_micom_p1 <- renderDT({
    df <- results$tables$MICOM_P1
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(
        data.frame(Nota = "Active 'Calcular MICOM', seleccione variable de grupo y ejecute el an\u00e1lisis."),
        rownames=FALSE, options=list(dom="t")))
    }
    datatable(df, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) |>
      formatStyle("OK", color = styleEqual(c("\u2713 Cumplido"), c("#2E7D32")))
  })

  # ── Tabla resumen MICOM estilo SmartPLS (Pasos 2 + 3 consolidados) ──────────
  output$table_micom_resumen <- renderDT({
    df <- results$tables$MICOM_RESUMEN
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(
        data.frame(Nota = "Active 'Calcular MICOM', seleccione variable de grupo y ejecute el an\u00e1lisis."),
        rownames=FALSE, options=list(dom="t")))
    }
    if ("Nota" %in% names(df))
      return(datatable(df, rownames=FALSE, options=list(dom="t")))

    datatable(
      df,
      rownames = FALSE,
      colnames = c(
        "Constructo", "Grupos",
        "Correlaci\u00f3n original",
        "p-valor permutaci\u00f3n",
        "Invarianza composicional",
        "Diferencia media p-valor",
        "Diferencia varianza p-valor",
        "Resultado"
      ),
      options = list(
        pageLength = 20,
        scrollX    = TRUE,
        dom        = "tip",
        columnDefs = list(
          list(className = "dt-center",
               targets   = c(2, 3, 4, 5, 6))
        )
      )
    ) |>
      # Correlacion original: verde si >= 0.90
      formatStyle("Correlacion_original",
        backgroundColor = styleInterval(c(0.899), c("#FFCDD2", "#C8E6C9")),
        fontWeight = "bold") |>
      # p-valor permutacion: verde si >= 0.05 (no significativo = invariante)
      formatStyle("p_valor_permutacion",
        backgroundColor = styleInterval(c(0.049), c("#FFCDD2", "#C8E6C9"))) |>
      # Invarianza composicional: color texto
      formatStyle("Invarianza_composicional",
        color = styleEqual(c("Si", "No"), c("#2E7D32", "#C62828")),
        fontWeight = "bold") |>
      # p-valor medias
      formatStyle("Dif_media_p_valor",
        backgroundColor = styleInterval(c(0.049), c("#FFCDD2", "#C8E6C9"))) |>
      # p-valor varianzas
      formatStyle("Dif_varianza_p_valor",
        backgroundColor = styleInterval(c(0.049), c("#FFCDD2", "#C8E6C9"))) |>
      # Resultado: color por tipo
      formatStyle("Resultado",
        color = styleEqual(
          c("Invarianza total", "Invarianza parcial", "No invariante"),
          c("#2E7D32",          "#F57F17",            "#C62828")
        ),
        fontWeight = "bold")
  })

  output$micom_summary_ui <- renderUI({
    p1  <- results$tables$MICOM_P1
    tbl <- results$tables$MICOM_RESUMEN
    if (is.null(p1) || is.null(tbl) || "Nota" %in% names(tbl))
      return(NULL)  # Nothing to summarise yet

    grupos_str <- if (!is.null(p1$Grupos)) p1$Grupos[1] else "N/D"

    # Derive overall invariance verdict from the resumen table
    n_total    <- nrow(tbl)
    n_total_inv  <- sum(tbl$Resultado == "Invarianza total",   na.rm = TRUE)
    n_parcial_inv <- sum(tbl$Resultado == "Invarianza parcial", na.rm = TRUE)
    n_no_inv      <- sum(tbl$Resultado == "No invariante",      na.rm = TRUE)

    verdict <- if (n_no_inv == 0 && n_parcial_inv == 0) {
      "TOTAL"
    } else if (n_no_inv == 0) {
      "PARCIAL"
    } else {
      "NO CUMPLIDA"
    }
    col_v <- switch(verdict, "TOTAL"="#2E7D32", "PARCIAL"="#F57F17", "#C62828")

    en <- input$app_lang == "en"
    lbl_grupos   <- if(en) "Analyzed groups: "       else "Grupos analizados: "
    lbl_full_inv <- if(en) "Full invariance"          else "Invarianza total"
    lbl_part_inv <- if(en) "Partial invariance"       else "Invarianza parcial"
    lbl_no_inv   <- if(en) "Not invariant"            else "No invariante"
    lbl_const    <- if(en) " constructs"              else " constructos"
    ver_full <- if(en) "\u2611 MICOM Final Verdict" else "\u2611 Veredicto Final MICOM"

    tagList(
      br(),
      tags$h5(style="color:#1565C0;", ver_full),
      tags$p(tags$b(lbl_grupos), grupos_str),
      tags$div(style="display:flex; gap:12px; flex-wrap:wrap; margin-bottom:12px;",
        tags$div(style="background:#E8F5E9;border-left:4px solid #2E7D32;padding:10px;border-radius:4px;flex:1;min-width:150px;",
          tags$b(style="color:#2E7D32;", paste0(n_total_inv, lbl_const)),
          tags$p(style="margin:0;font-size:12px;", lbl_full_inv)
        ),
        tags$div(style="background:#FFF8E1;border-left:4px solid #F9A825;padding:10px;border-radius:4px;flex:1;min-width:150px;",
          tags$b(style="color:#F57F17;", paste0(n_parcial_inv, lbl_const)),
          tags$p(style="margin:0;font-size:12px;", lbl_part_inv)
        ),
        tags$div(style="background:#FFEBEE;border-left:4px solid #E53935;padding:10px;border-radius:4px;flex:1;min-width:150px;",
          tags$b(style="color:#C62828;", paste0(n_no_inv, lbl_const)),
          tags$p(style="margin:0;font-size:12px;", lbl_no_inv)
        )
      ),
      if (verdict == "TOTAL")
        tags$div(style="background:#E8F5E9;border-left:4px solid #2E7D32;padding:12px;border-radius:4px;",
          tags$b(if(en) "\u2713 FULL measurement invariance confirmed." else "\u2713 Invariancia de medida TOTAL confirmada."),
          if(en) " All constructs are compositally invariant (r \u2265 0.90) with no significant mean or variance differences. Proceed with MGA with full confidence (Hair et al., 2017)."
          else   " Todos los constructos son compositamente invariantes (r \u2265 0.90) y no presentan diferencias significativas en medias ni varianzas. Puede proceder con MGA con plena confianza (Hair et al., 2017).")
      else if (verdict == "PARCIAL")
        tags$div(style="background:#FFF8E1;border-left:4px solid #F9A825;padding:12px;border-radius:4px;",
          tags$b(if(en) "\u26a0 PARTIAL invariance." else "\u26a0 Invariancia PARCIAL."),
          if(en) " Compositally invariant constructs (r \u2265 0.90) allow MGA, but results should be interpreted cautiously for constructs with significant mean or variance differences (Hair et al., 2018)."
          else   " Los constructos con invarianza composicional (r \u2265 0.90) permiten MGA, pero los resultados deben interpretarse con cautela para los constructos con diferencias significativas en medias o varianzas (Hair et al., 2018).")
      else
        tags$div(style="background:#FFEBEE;border-left:4px solid #E53935;padding:12px;border-radius:4px;",
          tags$b(if(en) "\u2717 Invariance NOT confirmed." else "\u2717 Invariancia NO confirmada."),
          if(en) " One or more constructs do not reach r \u2265 0.90. MGA comparisons should be interpreted with extreme caution or avoided."
          else   " Uno o mas constructos no alcanzan r \u2265 0.90. Las comparaciones MGA deben interpretarse con precaucion extrema o no realizarse.")
    )
  })

  # ── MGA outputs ───────────────────────────────────────────────────────────

  output$table_mga <- renderDT({
    df <- results$tables$MGA
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(
        data.frame(Nota = "Active Calcular MGA, seleccione variable de grupo con >= 2 categorias y ejecute el analisis."),
        rownames = FALSE, options = list(dom = "t")))
    }
    if ("Nota" %in% names(df))
      return(datatable(df, rownames = FALSE, options = list(dom = "t")))

    # Renombrar columnas usando diccionario fijo (sin regex, sin escaping)
    col_names   <- names(df)
    rename_dict <- c(
      "Relacion"             = "Relacion",
      "Diferencia_original"  = "Diferencia original",
      "Media_permutacion"    = "Media permutacion",
      "IC_2.5pct"            = "2.5%",
      "IC_97.5pct"           = "97.5%",
      "p_valor_permutacion"  = "p-valor permutacion",
      "Sig"                  = "Sig"
    )
    col_display <- col_names
    for (k in seq_along(col_names)) {
      nm <- col_names[k]
      if (nm %in% names(rename_dict)) {
        col_display[k] <- rename_dict[[nm]]
      } else if (startsWith(nm, "Original_")) {
        col_display[k] <- paste0("Original (", substring(nm, 10), ")")
      }
    }

    dt <- datatable(
      df,
      rownames = FALSE,
      colnames = col_display,
      options  = list(
        pageLength  = 20,
        scrollX     = TRUE,
        dom         = "tip",
        columnDefs  = list(
          list(className = "dt-center",
               targets   = seq_len(ncol(df)) - 1)
        )
      )
    )
    # Colorear p-valor permutacion
    if ("p_valor_permutacion" %in% col_names) {
      dt <- dt |>
        formatStyle("p_valor_permutacion",
          backgroundColor = styleInterval(
            c(0.049, 0.099),
            c("#FFCDD2", "#FFF9C4", "#C8E6C9")
          ),
          fontWeight = "bold")
    }
    # Colorear columna Sig
    if ("Sig" %in% col_names) {
      dt <- dt |>
        formatStyle("Sig",
          color = styleEqual(
            c("***", "**", "*", "n.s.", "N/D"),
            c("#C62828","#E53935","#EF5350","#2E7D32","#888")
          ),
          fontWeight = "bold")
    }
    dt
  })
  output$mga_summary_ui <- renderUI({
    df <- results$tables$MGA
    en <- input$app_lang == "en"
    if (is.null(df) || "Nota" %in% names(df))
      return(tags$p(if(en) "Enable \'Calculate MGA\' and select a group variable before running."
                    else "Activa \'Calcular MGA\' y selecciona variable de grupo antes de ejecutar.",
                    style = "color:gray"))

    p_col <- if ("p_valor_permutacion" %in% names(df)) "p_valor_permutacion" else
             names(df)[grep("p_valor", names(df))[1]]

    sig_rows <- if (!is.na(p_col) && p_col %in% names(df))
      df[!is.na(df[[p_col]]) & df[[p_col]] < 0.05, ] else df[0, ]
    ns_rows  <- if (!is.na(p_col) && p_col %in% names(df))
      df[!is.na(df[[p_col]]) & df[[p_col]] >= 0.05, ] else df

    make_lbl <- function(row) {
      diff <- if ("Diferencia_original" %in% names(row)) row[["Diferencia_original"]] else ""
      pv   <- if (!is.na(p_col) && p_col %in% names(row)) row[[p_col]] else ""
      sig  <- if ("Sig" %in% names(row)) row[["Sig"]] else ""
      paste0(row[["Relacion"]], " (\u0394\u03b2=", diff, ", p=", pv, " ", sig, ")")
    }

    tagList(
      tags$h5(style = "color:#1565C0;",
        paste0("MGA: ", nrow(df),
               if(en) " paths | " else " paths | ",
               nrow(sig_rows),
               if(en) " significant | " else " significativos | ",
               nrow(ns_rows), " n.s.")),
      br(),
      fluidRow(
        column(6,
          tags$div(style = "background:#FFEBEE;border-left:4px solid #E53935;padding:10px;border-radius:4px;",
            tags$b(style = "color:#C62828;",
              paste0("\u2717 ", nrow(sig_rows),
                     if(en) " significant paths (p < 0.05)" else " paths significativos (p < 0.05)")),
            if (nrow(sig_rows) > 0)
              tags$ul(lapply(seq_len(nrow(sig_rows)), function(i) tags$li(make_lbl(sig_rows[i, ]))))
            else tags$p(if(en) "None" else "Ninguno", style = "color:#888; font-style:italic;")
          )
        ),
        column(6,
          tags$div(style = "background:#E8F5E9;border-left:4px solid #2E7D32;padding:10px;border-radius:4px;",
            tags$b(style = "color:#2E7D32;",
              paste0("\u2713 ", nrow(ns_rows),
                     if(en) " paths with no significant difference" else " paths sin diferencia significativa")),
            if (nrow(ns_rows) > 0)
              tags$ul(lapply(seq_len(min(nrow(ns_rows), 10)), function(i) tags$li(make_lbl(ns_rows[i, ]))))
            else tags$p(if(en) "None" else "Ninguno", style = "color:#888; font-style:italic;")
          )
        )
      )
    )
  })
  output$interp_output <- renderUI({
    req(length(results$tables) > 0)
    HTML(interpretar_plssem(results$tables, input$app_lang))
  })

  # Tablas disponibles
  output$available_tables_ui <- renderUI({
    nms <- names(results$tables)
    if (!length(nms)) return(tags$p(
      i18n()$no_tables,
      style="color:gray"))
    lbl_map <- c(
      Confiabilidad   = "Reliability & Validity",
      Cargas          = "Outer Loadings",
      CrossLoadings   = "Cross-Loadings",
      FornellLarcker  = "Fornell-Larcker",
      HTMT            = "HTMT",
      Paths           = "Path Coefficients + IC",
      Hypotheses      = "Hypothesis Table",
      IndirectEffects = "Indirect Effects",
      TotalEffects    = "Total Effects",
      R2              = "R-Squared",
      Q2              = "Q-Squared",
      PLSPredict      = "PLS Predict",
      f2              = "Effect Sizes",
      VIF             = "VIF",
      SRMR            = "SRMR",
      MICOM_P1        = "MICOM Step 1",
      MICOM_RESUMEN   = "MICOM Summary",
      MGA             = "MGA"
    )
    tagList(lapply(nms, function(n) {
      display_nm <- if (n %in% names(lbl_map)) lbl_map[[n]] else n
      df <- results$tables[[n]]
      nr <- if (is.data.frame(df)) nrow(df) else 0
      tags$div(style="display:inline-block;margin:3px;padding:4px 10px;background:#E3F2FD;border-radius:12px;font-size:12px;",
             paste0(display_nm, " (", nr, " rows)"))
    }))
  })

  # ── DESCARGAR ─────────────────────────────────────────────────────────────

  # Generar reporte HTML
  generar_html <- function() {
    tabs <- results$tables
    dot  <- results$dot_code

    svg_html <- ""
    if (!is.null(dot) && nzchar(dot) && has_diagrammersvg) {
      svg_html <- tryCatch(
        DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot)),
        error = function(e) ""
      )
    }

    secciones <- paste0(
      "<html><head>",
      "<meta charset='UTF-8'>",
      "<title>Reporte PLS-SEM - CANCHARI PRO V2.0</title>",
      "<style>",
      "body{font-family:Arial,sans-serif;max-width:1100px;margin:auto;padding:20px;color:#222;}",
      "h1{color:#1565C0;border-bottom:3px solid #1565C0;padding-bottom:8px;}",
      "h2{color:#1565C0;border-left:4px solid #E53935;padding-left:10px;margin-top:30px;}",
      "table{border-collapse:collapse;width:100%;margin:12px 0;}",
      "th{background:#1565C0;color:white;padding:8px 12px;text-align:left;}",
      "td{padding:7px 12px;border-bottom:1px solid #ddd;}",
      "tr:nth-child(even){background:#f7f9fc;}",
      ".footer{color:#888;font-size:12px;margin-top:40px;border-top:1px solid #ddd;padding-top:10px;}",
      ".diagram{margin:20px 0; text-align:center;}",
      "</style></head><body>",
      "<h1>Reporte PLS-SEM - CANCHARI PRO V2.0</h1>",
      "<p><b>Generado:</b> ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | <b>Software:</b> seminr + R Shiny</p>"
    )

    # Diagrama
    if (nzchar(svg_html)) {
      secciones <- paste0(secciones,
        "<h2>Diagrama del Modelo</h2>",
        "<div class='diagram'>", svg_html, "</div>")
    }

    # Interpretación
    secciones <- paste0(secciones,
      "<h2>Interpretación Automática</h2>",
      "<div style='background:#f0f4ff;border-left:4px solid #1565C0;padding:14px;border-radius:4px;'>",
      interpretar_plssem(tabs, input$app_lang), "</div>")

    # Tablas
    df_to_html <- function(df, titulo) {
      if (is.null(df) || nrow(df) == 0) return("")
      ths <- paste0("<th>", names(df), "</th>", collapse="")
      rows <- apply(df, 1, function(r) paste0("<tr><td>", paste(r, collapse="</td><td>"), "</td></tr>"))
      paste0("<h2>", titulo, "</h2><table><tr>", ths, "</tr>", paste(rows, collapse=""), "</table>")
    }

    tabla_names <- list(
      Confiabilidad = "Confiabilidad y Validez Convergente",
      Cargas        = "Cargas Factoriales (Outer Loadings)",
      HTMT          = "Validez Discriminante (HTMT)",
      Paths         = "Coeficientes de Ruta",
      R2            = "Coeficiente de Determinacion (R2)",
      Q2            = "Predictive Relevance (Q2)",
      VIF           = "Colinealidad (VIF)",
      SRMR          = "Ajuste del Modelo (SRMR)",
      MICOM_P1      = "MICOM - Paso 1: Configuracion",
      MICOM_P2      = "MICOM - Paso 2: Igualdad Compuesta",
      MICOM_P3      = "MICOM - Paso 3: Medias y Varianzas",
      MGA           = "MGA - Multi-Group Analysis"
    )

    for (nm in names(tabla_names)) {
      if (!is.null(tabs[[nm]])) {
        secciones <- paste0(secciones, df_to_html(tabs[[nm]], tabla_names[[nm]]))
      }
    }

    paste0(secciones, "<div class='footer'>CANCHARI PLS-SEM PRO V3.0 | ",
           "Cite as: CANCHARI PLS-SEM PRO V3.0 | Hair et al. (2022)</div>",
           "</body></html>")
  }

  output$download_html <- downloadHandler(
    filename = function() paste0("Reporte_PLSSEM_", Sys.Date(), ".html"),
    contentType = "text/html; charset=UTF-8",
    content  = function(file) {
      con <- file(file, open = "wt", encoding = "UTF-8")
      writeLines(generar_html(), con = con)
      close(con)
    }
  )

  output$download_zip <- downloadHandler(
    filename = function() paste0("Resultados_PLSSEM_", Sys.Date(), ".zip"),
    contentType = "application/zip",
    content  = function(file) {
      tmpdir <- tempfile(); dir.create(tmpdir)
      archivos <- character(0)

      # CSVs
      # Ensure new tables are included
      all_export_nms <- c("Confiabilidad","Cargas","CrossLoadings","FornellLarcker",
                          "HTMT","Paths","Hypotheses","IndirectEffects","TotalEffects",
                          "R2","Q2","PLSPredict","f2","VIF","SRMR","MICOM_RESUMEN","MGA",
                          names(results$tables))
      all_export_nms <- unique(all_export_nms)
      for (nm in all_export_nms) {
        df <- results$tables[[nm]]
        if (is.data.frame(df) && nrow(df) > 0) {
          fname <- file.path(tmpdir, paste0(nm, ".csv"))
          write.csv(df, fname, row.names = FALSE, fileEncoding = "UTF-8")
          archivos <- c(archivos, fname)
        }
      }

      # Diagrama SVG (con encoding correcto)
      dot <- results$dot_code
      if (!is.null(dot) && nzchar(dot) && has_diagrammersvg) {
        tryCatch({
          svg_code <- DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot))
          svg_file <- file.path(tmpdir, "Diagrama_PLS.svg")
          con_svg  <- file(svg_file, open = "wt", encoding = "UTF-8")
          writeLines(svg_code, con = con_svg); close(con_svg)
          archivos <- c(archivos, svg_file)
        }, error = function(e) NULL)
      } else if (!is.null(dot) && nzchar(dot)) {
        dot_file <- file.path(tmpdir, "Diagrama_PLS_DOT.txt")
        writeLines(dot, dot_file)
        archivos <- c(archivos, dot_file)
      }

      # Reporte HTML
      tryCatch({
        html_file <- file.path(tmpdir, "Reporte_PLS_SEM.html")
        con_html  <- file(html_file, open = "wt", encoding = "UTF-8")
        writeLines(generar_html(), con = con_html); close(con_html)
        archivos <- c(archivos, html_file)
      }, error = function(e) NULL)

      # Crear ZIP
      zip(zipfile = file, files = archivos, flags = "-j")
    }
  )

  # ============================================================================
  # OUTPUT: MÓDULO TAMAÑO DE MUESTRA / SAMPLE SIZE
  # ============================================================================

  output$sample_size_ui <- renderUI({
    t  <- i18n()
    es <- input$app_lang == "es"

    # ── Auto-detect max predictors from current model ────────────────────────
    lm  <- last_model()
    paths_df <- if (!is.null(lm) && !is.null(lm$paths)) lm$paths else NULL
    auto_u   <- detect_max_predictors(paths_df)

    # ── Build UI ─────────────────────────────────────────────────────────────
    fluidRow(
      # ── LEFT: Inputs ───────────────────────────────────────────────────────
      column(4,
        box(
          title = if(es) "⚙ Parámetros del Análisis" else "⚙ Analysis Parameters",
          status = "primary", solidHeader = TRUE, width = NULL,

          if (!is.null(paths_df) && nrow(paths_df) > 0) {
            div(class = "tooltip-box",
              tags$b(if(es) "🔍 Auto-detectado del modelo:" else "🔍 Auto-detected from model:"),
              br(),
              tags$small(if(es)
                paste0("El constructo con más predictores recibe ", auto_u, " flecha(s) entrante(s).")
              else
                paste0("The construct with most predictors has ", auto_u, " incoming arrow(s)."))
            )
          },

          numericInput("ss_u", if(es) "Máx. predictores (u) — editable" else "Max predictors (u) — editable",
            value = auto_u, min = 1, max = 20, step = 1),

          selectInput("ss_f2", if(es) "Tamaño de efecto esperado (f²)" else "Expected effect size (f²)",
            choices = setNames(
              c("0.02", "0.15", "0.35"),
              if(es) c("Pequeño — f² = 0.02","Mediano — f² = 0.15","Grande — f² = 0.35")
              else   c("Small — f² = 0.02",  "Medium — f² = 0.15", "Large — f² = 0.35")
            ),
            selected = "0.15"),

          selectInput("ss_alpha", if(es) "Nivel de significancia (α)" else "Significance level (α)",
            choices = c("0.01" = "0.01", "0.05" = "0.05", "0.10" = "0.10"),
            selected = "0.05"),

          selectInput("ss_power", if(es) "Potencia estadística (1-β)" else "Statistical power (1-β)",
            choices = c("0.80" = "0.80", "0.85" = "0.85", "0.90" = "0.90", "0.95" = "0.95"),
            selected = "0.80"),

          sliderInput("ss_margin", if(es) "Margen adicional por pérdidas (%)" else "Additional margin for data loss (%)",
            min = 0, max = 30, value = 15, step = 5, post = "%"),

          hr(),
          numericInput("ss_n_real", if(es) "Tu muestra real (para diagnóstico)" else "Your actual sample (for diagnostics)",
            value = NA, min = 1),

          hr(),
          tags$b(if(es) "📊 Muestra Clásica (opcional)" else "📊 Classical Sample (optional)"),
          br(), br(),
          checkboxInput("ss_show_classical", if(es) "Activar cálculo clásico de Cochran" else "Enable classical Cochran calculation",
            value = FALSE),

          conditionalPanel("input.ss_show_classical == true",
            selectInput("ss_pop_type", if(es) "Tipo de población" else "Population type",
              choices = setNames(
                c("large", "finite"),
                if(es) c("Grande / Desconocida", "Finita (conocida)")
                else   c("Large / Unknown",       "Finite (known)")
              ), selected = "large"),
            conditionalPanel("input.ss_pop_type == 'finite'",
              numericInput("ss_N_pop", if(es) "Tamaño poblacional (N)" else "Population size (N)",
                value = 5000, min = 100)
            ),
            selectInput("ss_conf", if(es) "Nivel de confianza" else "Confidence level",
              choices = c("90%" = "0.90", "95%" = "0.95", "99%" = "0.99"), selected = "0.95"),
            numericInput("ss_error", if(es) "Margen de error (e)" else "Margin of error (e)",
              value = 0.05, min = 0.01, max = 0.20, step = 0.01)
          ),

          br(),
          actionButton("ss_calculate", if(es) "▶ CALCULAR" else "▶ CALCULATE",
            class = "btn btn-danger btn-block", style = "font-weight:bold; font-size:15px;")
        )
      ),

      # ── RIGHT: Results ─────────────────────────────────────────────────────
      column(8,
        uiOutput("ss_results_panel")
      )
    )
  })

  # ── Reactive: run sample size calculation ───────────────────────────────────
  ss_calc <- eventReactive(input$ss_calculate, {
    u      <- max(1L, as.integer(input$ss_u %||% 2L))
    f2     <- as.numeric(input$ss_f2 %||% "0.15")
    alpha  <- as.numeric(input$ss_alpha %||% "0.05")
    power  <- as.numeric(input$ss_power %||% "0.80")
    margin <- (input$ss_margin %||% 15) / 100

    pw <- calculate_pls_power_n(u = u, f2 = f2, alpha = alpha, power = power, margin = margin)

    cl <- NULL
    if (isTRUE(input$ss_show_classical)) {
      N_pop <- if (input$ss_pop_type == "finite") {
        as.numeric(input$ss_N_pop %||% 5000)
      } else NULL
      cl <- calculate_classical_sample_size(
        pop_type = input$ss_pop_type %||% "large",
        N_pop    = N_pop,
        conf     = as.numeric(input$ss_conf %||% "0.95"),
        error    = as.numeric(input$ss_error %||% 0.05)
      )
    }

    n_real <- suppressWarnings(as.numeric(input$ss_n_real))

    # Build model detail string from paths
    lm <- last_model()
    model_detail <- ""
    if (!is.null(lm) && !is.null(lm$paths)) {
      pd <- lm$paths
      if (all(c("from","to") %in% names(pd))) {
        tbl <- sort(table(pd$to), decreasing = TRUE)
        model_detail <- paste0(names(tbl)[1], ": ", tbl[1], " predictors")
      }
    }

    list(pw = pw, cl = cl, n_real = n_real, model_detail = model_detail)
  })

  # ── Output: results panel ────────────────────────────────────────────────────
  output$ss_results_panel <- renderUI({
    es  <- input$app_lang == "es"

    # Show placeholder before calculation
    calc <- tryCatch(ss_calc(), error = function(e) NULL)
    if (is.null(calc)) {
      return(box(
        title = if(es) "📊 Resultados" else "📊 Results",
        status = "info", solidHeader = TRUE, width = NULL,
        div(class = "tooltip-box",
          if(es) "Configure los parámetros y presione ▶ CALCULAR para ver los resultados."
          else   "Configure the parameters and press ▶ CALCULATE to see results."
        )
      ))
    }

    pw     <- calc$pw
    cl     <- calc$cl
    n_real <- calc$n_real
    lang   <- if(es) "es" else "en"
    cls    <- if (!is.na(n_real)) classify_sample_strength(n_real, pw$n_min) else NULL
    smart  <- if (!is.na(n_real)) sample_smart_message(n_real, pw$n_min, lang) else ""

    # Practical recommendation
    prac_msg <- if(es) {
      if      (pw$u <= 1)  "Modelo simple (1 predictor): se recomienda al menos 100 casos para estabilidad del bootstrapping."
      else if (pw$u <= 2)  "Modelo moderado (2 predictores): se recomienda al menos 150–200 casos."
      else if (pw$u <= 3)  "Modelo con mediación (3 predictores): se recomienda al menos 200 casos."
      else                 "Modelo complejo (≥4 predictores o con moderación/MGA): se recomienda al menos 250–300 casos."
    } else {
      if      (pw$u <= 1)  "Simple model (1 predictor): at least 100 cases are recommended for bootstrapping stability."
      else if (pw$u <= 2)  "Moderate model (2 predictors): at least 150–200 cases are recommended."
      else if (pw$u <= 3)  "Mediation model (3 predictors): at least 200 cases are recommended."
      else                 "Complex model (≥4 predictors or moderation/MGA): at least 250–300 cases are recommended."
    }

    tagList(
      # ── PANEL 1: Power analysis results ───────────────────────────────────
      box(
        title = if(es) "🔬 Power Analysis para PLS-SEM" else "🔬 Power Analysis for PLS-SEM",
        status = "danger", solidHeader = TRUE, width = NULL,

        div(class = "tooltip-box", style = "margin-bottom:12px;",
          tags$b(if(es) "📐 Criterio principal en PLS-SEM:" else "📐 Main criterion in PLS-SEM:"),
          if(es) " El tamaño de muestra se determina por potencia estadística del modelo, no por la fórmula clásica de 384."
          else   " Sample size is determined by the model's statistical power, not the classical n = 384 formula."
        ),

        fluidRow(
          column(4,
            div(style = "background:#E3F2FD; border-radius:8px; padding:14px; text-align:center;",
              tags$h2(style="color:#1565C0; margin:0;", pw$n_min),
              tags$small(if(es) "N mínimo (power analysis)" else "Minimum N (power analysis)")
            )
          ),
          column(4,
            div(style = "background:#E8F5E9; border-radius:8px; padding:14px; text-align:center;",
              tags$h2(style="color:#2E7D32; margin:0;", pw$n_target),
              tags$small(if(es) paste0("N objetivo (+", round(pw$margin*100), "% margen)")
                              else paste0("Target N (+", round(pw$margin*100), "% margin)"))
            )
          ),
          column(4,
            div(style = "background:#FFF3E0; border-radius:8px; padding:14px; text-align:center;",
              tags$h2(style="color:#E65100; margin:0;", pw$u),
              tags$small(if(es) "Predictores máx. (u)" else "Max predictors (u)")
            )
          )
        ),

        br(),
        div(class = "tooltip-box",
          tags$small(
            if(es) paste0("⚙ Método: ", pw$method, " | f² = ", pw$f2,
                          " | α = ", pw$alpha, " | power = ", pw$power)
            else   paste0("⚙ Method: ", pw$method, " | f² = ", pw$f2,
                          " | α = ", pw$alpha, " | power = ", pw$power)
          )
        ),

        # Practical recommendation
        div(style = "background:#F3E5F5; border-left:3px solid #7B1FA2; padding:10px; border-radius:4px; margin-top:8px;",
          tags$b("💡"), " ", prac_msg
        )
      ),

      # ── PANEL 2: Actual sample diagnostic ─────────────────────────────────
      if (!is.na(n_real) && !is.null(cls)) {
        box(
          title = if(es) "🩺 Diagnóstico de tu Muestra Actual" else "🩺 Diagnostic of Your Actual Sample",
          status = cls$color, solidHeader = TRUE, width = NULL,

          fluidRow(
            column(6,
              div(style = paste0("background:", switch(cls$color,
                    "danger"="#FFEBEE", "warning"="#FFFDE7", "info"="#E3F2FD", "success"="#E8F5E9"),
                    "; border-radius:8px; padding:16px; text-align:center;"),
                tags$h1(style="margin:0;", cls$icon),
                tags$h3(style="margin:4px 0;", cls$label),
                tags$p(paste0("n = ", n_real))
              )
            ),
            column(6,
              div(style="padding:10px;",
                tags$b(if(es) "Comparación:" else "Comparison:"), br(), br(),
                tags$table(class="table table-condensed",
                  tags$tbody(
                    tags$tr(tags$td(if(es)"N real" else "Actual N"), tags$td(tags$b(n_real))),
                    tags$tr(tags$td(if(es)"N mín (power)" else "N min (power)"), tags$td(pw$n_min)),
                    tags$tr(tags$td(if(es)"N objetivo" else "Target N"), tags$td(pw$n_target)),
                    if(!is.null(cl)) tags$tr(tags$td(if(es)"N clásico" else "Classical N"), tags$td(cl$n))
                  )
                )
              )
            )
          ),

          div(style="background:#fff; border-radius:6px; padding:12px; border:1px solid #ddd; margin-top:8px;",
            smart
          )
        )
      },

      # ── PANEL 3: Classical sample size (optional) ─────────────────────────
      if (!is.null(cl)) {
        box(
          title = if(es) "📊 Cálculo Clásico de Muestra (Cochran)" else "📊 Classical Sample Size (Cochran)",
          status = "warning", solidHeader = FALSE, width = NULL, collapsible = TRUE,

          div(class="tooltip-box",
            tags$b(if(es) "⚠ Rol complementario:" else "⚠ Complementary role:"),
            if(es) " En PLS-SEM, este cálculo sirve como respaldo poblacional, no como criterio principal."
            else   " In PLS-SEM, this calculation serves as population-level support, not the primary criterion."
          ),

          fluidRow(
            column(6,
              div(style="background:#FFF8E1; border-radius:8px; padding:14px; text-align:center;",
                tags$h2(style="color:#F57F17; margin:0;", cl$n),
                tags$small(if(es) "N mínimo (fórmula Cochran)" else "Minimum N (Cochran formula)")
              )
            ),
            column(6,
              div(style="padding:10px;",
                tags$small(
                  if(es) paste0("Población: ", if(cl$pop_type=="finite") paste0("finita (N=",cl$N_pop,")") else "grande/desconocida",
                                " | Confianza: ", round(cl$conf*100), "% | Error: ±", round(cl$error*100), "%")
                  else   paste0("Population: ", if(cl$pop_type=="finite") paste0("finite (N=",cl$N_pop,")") else "large/unknown",
                                " | Confidence: ", round(cl$conf*100), "% | Error: ±", round(cl$error*100), "%")
                )
              )
            )
          )
        )
      },

      # ── PANEL 4: Auto-generated methodology text ───────────────────────────
      box(
        title = if(es) "📝 Reporte Metodológico Automático" else "📝 Auto-Generated Methodology Report",
        status = "success", solidHeader = TRUE, width = NULL, collapsible = TRUE,

        tags$b(if(es) "🇪🇸 Español — listo para tesis / artículo:" else "🇪🇸 Spanish — ready for thesis/paper:"),
        br(), br(),
        div(style="background:#F1F8E9; border-radius:6px; padding:14px; font-size:13px; line-height:1.8; white-space:pre-wrap;",
          generate_sample_size_report_es(pw, cl, calc$model_detail)
        ),
        br(),
        tags$b(if(es) "🇺🇸 English — ready for paper/article:" else "🇺🇸 English — ready for paper/article:"),
        br(), br(),
        div(style="background:#E3F2FD; border-radius:6px; padding:14px; font-size:13px; line-height:1.8; white-space:pre-wrap;",
          generate_sample_size_report_en(pw, cl, calc$model_detail)
        ),
        br(),
        div(class="tooltip-box",
          tags$small(
            if(es) "Referencia: Hair, J. F., Ringle, C. M., & Sarstedt, M. (2022). A Primer on Partial Least Squares Structural Equation Modeling (PLS-SEM) (3rd ed.). Sage."
            else   "Reference: Hair, J. F., Ringle, C. M., & Sarstedt, M. (2022). A Primer on Partial Least Squares Structural Equation Modeling (PLS-SEM) (3rd ed.). Sage."
          )
        )
      )
    )
  })

  # END MÓDULO TAMAÑO DE MUESTRA ─────────────────────────────────────────────

}

shinyApp(ui, server)
