# CANCHARI PLS-SEM PRO v2.0

## Description

CANCHARI PLS-SEM PRO is a web-based application developed in R (Shiny) for Partial Least Squares Structural Equation Modeling (PLS-SEM). The system provides an integrated graphical interface for conducting measurement model assessment, structural model evaluation, and advanced analytical procedures without requiring programming.

This repository accompanies the SoftwareX submission:
"CANCHARI PLS-SEM PRO: A reproducible web-based application for partial least squares structural equation modeling"

---

## Key Features

- PLS-SEM estimation (reflective models, Mode A)
- Bootstrapping (bias-corrected, configurable resamples)
- Measurement model evaluation (AVE, CR, Cronbach’s alpha, HTMT, HTMT2)
- Structural model evaluation (path coefficients, t-values, p-values, R², f²)
- Predictive relevance (Q² via blindfolding, PLS Predict)
- Measurement invariance (MICOM procedure)
- Multi-group analysis (MGA permutation test)
- Endogeneity assessment (Gaussian Copula approach)
- Cluster analysis (k-means on construct scores)
- Automatic interpretation with visual indicators
- Export to Word (APA 7), Excel, HTML, and diagrams

---

## How to Run

### Option 1 — Web Application

Access the hosted version:
https://canchari.shinyapps.io/canchari-pls-sempro/

---

### Option 2 — Local Execution

```r
install.packages(c(
  "shiny","shinydashboard","seminr","officer","flextable",
  "DiagrammeR","DT","ggplot2","openxlsx","pwr","zip",
  "dplyr","tidyr","stringr","readxl"
))

shiny::runApp("app.R")
