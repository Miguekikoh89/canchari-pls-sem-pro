# CANCHARI PLS-SEM PRO v2.0 — User Manual

> **Version:** 2.0 | **Engine:** seminr 2.3+ | **License:** MIT License  
> **Hosted app:** https://canchari.shinyapps.io/pls-sem-app/ 
> **Source code:** https://github.com/Miguekikoh89/canchari-pls-sempro

---

## Table of Contents

1. [Overview](#1-overview)
2. [Installation](#2-installation)
3. [Data Format Requirements](#3-data-format-requirements)
4. [Model Specification](#4-model-specification)
5. [Running the Analysis](#5-running-the-analysis)
6. [Results Interpretation](#6-results-interpretation)
7. [Export Options](#7-export-options)
8. [Reproducibility](#8-reproducibility)
9. [Troubleshooting](#9-troubleshooting)
10. [Example Dataset](#10-example-dataset)

---

## 1. Overview

CANCHARI PLS-SEM PRO is a bilingual (Spanish/English) web application for Partial Least Squares Structural Equation Modeling (PLS-SEM). It wraps the `seminr` package (Ray, Danks & Calero Valdez, 2022) inside a guided, step-by-step graphical user interface designed for applied researchers who require publication-ready outputs without scripting.

### Core capabilities

| Module | Method | Reference |
|---|---|---|
| Sample Size | Cohen's f², power analysis | Cohen (1988) |
| PLS Estimation | Mode A (reflective), HOC Two-Stage | Hair et al. (2022) |
| Bootstrapping | BC percentile, 5,000 resamples | Ringle et al. (2020) |
| Measurement model | AVE, CR, Cronbach's α, HTMT, HTMT2, VIF | Hair et al. (2022) |
| Structural model | β, SE, t, p, R², f², SRMR | Hair et al. (2022) |
| Predictive relevance | Blindfolding (Q²), PLS Predict (Q²_predict) | Shmueli et al. (2019) |
| Measurement invariance | MICOM – 3-step procedure | Henseler et al. (2016) |
| Multi-group analysis | MGA permutation test | Henseler et al. (2009) |
| Endogeneity | Gaussian Copula (Park & Gupta, 2012) | Park & Gupta (2012) |
| Cluster analysis | k-means on construct scores (FIMIX proxy) | Hair et al. (2022) |

---

## 2. Installation

### Option A — Hosted web application (no installation required)

Access the application directly at:

```
https://canchari.shinyapps.io/pls-sem-app/
```

No R installation required. Works in any modern browser (Chrome, Firefox, Edge, Safari). An internet connection is required.

### Option B — Local installation (recommended for reproducibility and large datasets)

**System requirements:**
- R ≥ 4.2.0
- RStudio ≥ 2023.03 (recommended)
- Operating system: Windows 10+, macOS 12+, Ubuntu 20.04+

**Step-by-step:**

```r
# Step 1 — Install required packages (run once)
install.packages(c(
  "shiny", "shinydashboard", "seminr", "officer", "flextable",
  "DiagrammeR", "DT", "ggplot2", "openxlsx", "pwr", "zip",
  "dplyr", "tidyr", "stringr", "readxl"
))

# Step 2 — Clone the repository
# In your terminal / RStudio Terminal:
# git clone https://github.com/Miguekikoh89/canchari-pls-sempro.git

# Step 3 — Run the app
setwd("canchari-pls-sempro")
shiny::runApp("app.R")
```

The application opens automatically in your default browser at `http://127.0.0.1:<port>`.

### Language selection

Use the dropdown at the top of the left sidebar to switch between **Español** and **English**. All UI labels, error messages, and interpretation panels update immediately.

---

## 3. Data Format Requirements

### File formats accepted

- **Excel:** `.xlsx`, `.xls` (first sheet is read automatically)
- **CSV:** `.csv` (auto-detects separator: `,` or `;`)

### Structure requirements

| Requirement | Detail |
|---|---|
| Layout | Wide format — one row per observation, one column per indicator |
| Indicator columns | Numeric values only (integers or decimals) |
| Column names | Short alphanumeric names, no spaces (e.g., `PU1`, `DS3`, `BI`) |
| Grouping variable | Categorical column for MICOM/MGA (e.g., `gender` with values `M`/`F`) |
| Missing data | Pairwise complete cases used; columns with >30% missing trigger a warning |
| Minimum sample | n ≥ 30 (absolute minimum); n ≥ 100 recommended for stable bootstrapping |

### Minimal example structure

```
DS1, DS2, DS3, DS4, PU1, PU2, PU3, PU4, BI1, BI2, BI3, gender
4, 5, 4, 3, 5, 4, 4, 5, 4, 5, 4, M
3, 4, 3, 4, 4, 3, 5, 4, 5, 4, 3, F
...
```

> **Download example dataset:** [`example_data.csv`](example_data.csv) — 214 observations, Technology Acceptance Model (DS → PU → BI).

---

## 4. Model Specification

### 4.1 Constructs panel

Each construct is defined by its **name** and an **indicator range** using the following notation:

| Notation | Meaning | Example |
|---|---|---|
| `Name, Name1-NameK` | Construct with items Name1 through NameK | `PU, PU1-PU4` |
| `Name, Item1` | Single-indicator construct | `BI, BI1` |
| `Name, Item1, Item3, Item5` | Non-consecutive items | `DS, DS1, DS3, DS5` |

**Steps:**
1. Navigate to **Step 3 — Model Definition**.
2. In the **Constructs** box, enter one construct per line using the notation above.
3. Click **➕ Add Construct** after each entry, or paste all at once.
4. The system validates each construct name against the loaded dataset column names and reports mismatches.

**Example — Technology Acceptance Model:**
```
DS, DS1-DS4
PU, PU1-PU4
BI, BI1-BI3
```

### 4.2 Paths panel

Structural paths are defined as directional relationships from exogenous to endogenous constructs.

**Notation:** `ConstructFrom → ConstructTo`

| Example | Meaning |
|---|---|
| `DS → PU` | Design Satisfaction predicts Perceived Usefulness |
| `PU → BI` | Perceived Usefulness predicts Behavioral Intention |

**Steps:**
1. In the **Paths** box, select the exogenous construct from the first dropdown and the endogenous construct from the second.
2. Click **➕ Add Relationship**.
3. Repeat for each hypothesised path.
4. Click **🔍 Verify Configuration** to validate the complete model before running.

### 4.3 Higher-Order Constructs (HOC)

For reflective higher-order constructs, use the Two-Stage approach:

1. Define the first-order constructs as usual in the Constructs panel.
2. Check **🔺 HOC × 2 adjustment** in the Analysis panel **only** if your moderating variable is strictly dichotomous (0/1). For Likert-scale moderators, leave this unchecked.
3. The application automatically implements the repeated-indicators approach at Stage 1 and uses construct scores as manifest variables at Stage 2.

> ⚠ HOC support is limited to reflective-reflective higher-order structures. Reflective-formative HOC is not supported in v2.0.

---

## 5. Running the Analysis

### 5.1 Analysis configuration

Navigate to **Step 5 — Analysis** and configure the following parameters:

| Parameter | Default | Recommended range | Effect |
|---|---|---|---|
| Bootstrap iterations | 5,000 | 5,000–10,000 | Higher = more stable CIs; slower |
| Random seed | 123 | Any integer | Controls reproducibility (document this) |
| Omission distance (Q²) | 7 | 5–10 | Blindfolding omission distance |
| Calculate Q² | Yes | — | Blindfolding predictive relevance |
| Calculate f² | Yes | — | Cohen's effect size per path |

### 5.2 Reproducibility seed

**This is critical for publication.** The random seed controls all stochastic procedures:
- Bootstrapping (BC percentile CIs)
- PLS Predict (10-fold cross-validation)
- MICOM permutation test (1,000 permutations)
- MGA permutation test (1,000 permutations)
- Cluster analysis k-means (50 restarts)

**What to report in your paper:**

> "All stochastic procedures were executed using CANCHARI PLS-SEM PRO v2.0 with random seed 123 and 5,000 bootstrap resamples."

### 5.3 Group analysis (MICOM / MGA)

1. Select the **Group variable** from the dropdown (populated automatically from your dataset's categorical columns).
2. Set the **Minimum n per group** (default: 30). Groups below this threshold are excluded from MGA.
3. Check **Calculate MICOM** and/or **Calculate MGA** as required.
4. MICOM must be run before interpreting MGA results (invariance is a prerequisite).

### 5.4 Execution and progress

Click **▶ RUN PLS-SEM ANALYSIS**. The diagnostic console on the right displays real-time progress:

```
► [1/7] Validating data and model...
► [2/7] Estimating PLS model...
► [3/7] Calculating measurement model metrics...
► [4/7] Calculating discriminant validity...
► [5/7] Calculating effect sizes (f²)...
► [6/7] Running Bootstrapping...
► [6.5/7] PLS Predict (out-of-sample)...
► [7/7] Generating diagram...
✓ Analysis complete.
```

**Typical runtime:**
- Simple model (3–4 constructs, n=200, 5,000 boot): ~60–90 seconds (hosted)
- Complex model (7+ constructs, MICOM+MGA, n=500): ~3–5 minutes (hosted)
- Local installation: 30–50% faster

---

## 6. Results Interpretation

### 6.1 Measurement model (Step 1: reflective indicators)

Navigate to **Step 6 — Results → 🔵 Measurement**.

**Table 1. Reliability and Convergent Validity**

| Indicator | Threshold | Interpretation |
|---|---|---|
| Cronbach's α | ≥ 0.70 | Internal consistency reliability |
| Composite Reliability (CR / ρ_c) | ≥ 0.70 | Composite reliability |
| Average Variance Extracted (AVE) | ≥ 0.50 | Convergent validity |

**Table 2. Outer Loadings**

| Threshold | Action |
|---|---|
| λ ≥ 0.70 | ✅ Retain |
| 0.40 ≤ λ < 0.70 | ⚠ Consider retention (evaluate impact on AVE) |
| λ < 0.40 | ❌ Remove indicator |

**Table 5. HTMT (Heterotrait-Monotrait Ratio)**

| Threshold | Interpretation |
|---|---|
| HTMT < 0.85 | ✅ Discriminant validity confirmed |
| 0.85 ≤ HTMT < 0.90 | ⚠ Marginal — check HTMT bootstrapped CI |
| HTMT ≥ 0.90 | ❌ Discriminant validity not confirmed |

The **HTMT bootstrapped 95% CI** (Table 5a) must not include 1.0 for discriminant validity to be confirmed.

**Table 5b. HTMT2** uses the geometric-mean correction (Roemer et al., 2021), which is more conservative and recommended for constructs with unequal numbers of indicators.

### 6.2 Structural model

Navigate to **🔴 Structural**.

**Table 6. Path Coefficients**

| Column | Description |
|---|---|
| β (Beta) | Standardised path coefficient |
| STDEV | Bootstrap standard deviation |
| t-value | |β / STDEV| — use 1.96 (p<0.05) or 2.576 (p<0.01) threshold |
| p-value | Two-tailed bootstrap p-value |
| 2.5% / 97.5% | Percentile bootstrap 95% CI |

A path is statistically significant if the 95% CI does not straddle zero **and** p < 0.05.

**Table 10. R² (Coefficient of Determination)**

| R² | Interpretation (Hair et al., 2022) |
|---|---|
| ≥ 0.75 | Substantial |
| ≥ 0.50 | Moderate |
| ≥ 0.25 | Weak |
| < 0.25 | Insufficient (context-dependent) |

**Table 11. Q² (Predictive Relevance — Blindfolding)**

| Q² | Interpretation |
|---|---|
| > 0.35 | Large predictive relevance |
| > 0.15 | Medium predictive relevance |
| > 0.02 | Small predictive relevance |
| ≤ 0 | No predictive relevance |

**Table 12. PLS Predict (Q²_predict)**

Compares the model's 10-fold cross-validated RMSE against a naïve linear model benchmark. If the model RMSE < benchmark RMSE for all endogenous indicators, the model has predictive power.

**Table 13. VIF (Collinearity)**

| VIF | Interpretation |
|---|---|
| < 3.3 | ✅ Strict criterion (recommended) |
| < 5.0 | ✅ Lenient criterion |
| ≥ 5.0 | ❌ Collinearity problem — consider removing/merging predictors |

### 6.3 MICOM (Measurement Invariance)

Navigate to **📐 MICOM**.

MICOM follows the three-step procedure of Henseler, Ringle & Sarstedt (2016):

| Step | What is tested | Criterion | Outcome |
|---|---|---|---|
| Step 1 | Configural invariance | Always confirmed in PLS composites | Required |
| Step 2 | Compositional invariance | Original correlation r ≥ 0.90 AND p-value ≥ 0.05 | Required for MGA |
| Step 3a | Equality of means | p-value ≥ 0.05 | Partial vs. full invariance |
| Step 3b | Equality of variances | p-value ≥ 0.05 | Partial vs. full invariance |

**Interpretation of invariance results:**

- **Full invariance** (Steps 1–3 all passed): Group comparison of latent means and variances is valid.
- **Partial invariance** (Step 2 passed, Step 3 failed for some constructs): Path coefficient comparison via MGA is still valid; mean/variance comparison is not.
- **No invariance** (Step 2 failed): MGA results are not interpretable.

### 6.4 MGA (Multi-Group Analysis)

Navigate to **👥 MGA**.

The permutation test (1,000 permutations) tests whether path coefficient differences between groups are statistically significant.

| Column | Description |
|---|---|
| Original G1 / G2 | Path coefficient in each group |
| Difference | G1 − G2 |
| p-value (permutation) | Two-tailed permutation p-value |
| Sig | *** p<0.001, ** p<0.01, * p<0.05 |

A significant p-value indicates the path coefficient differs between groups beyond chance.

### 6.5 Gaussian Copula (Endogeneity test)

Navigate to **Diagnostic → Gaussian Copula**.

The Gaussian Copula approach (Park & Gupta, 2012) tests whether exogenous constructs are endogenous (correlated with the error term). The copula term coefficient (γ) and its significance indicate the presence of endogeneity.

| Result | Interpretation |
|---|---|
| Copula p < 0.05 | ⚠ Potential endogeneity detected — report and discuss |
| Copula p ≥ 0.05 | ✅ No evidence of endogeneity |

### 6.6 Automatic interpretation panel

Navigate to **🧠 Interpretation**. The traffic-light summary (🟢 green / 🟡 yellow / 🔴 red) evaluates each metric against published thresholds and generates an automated narrative summary suitable for the results section of a paper.

---

## 7. Export Options

Navigate to **Step 7 — Download**.

| Format | Contents | Use case |
|---|---|---|
| 📦 ZIP (all) | All CSV tables + SVG diagram + HTML report | Full archive for supplementary material |
| 🌐 HTML Report | All tables in a single styled HTML file | Sharing with collaborators |
| 📄 Word Report (APA) | All tables formatted per APA 7th edition | Direct paste into manuscript |
| 🗺 SVG Diagram | Path diagram in vector format | High-resolution publication figures |
| Individual CSV | Each results table separately | Custom analysis or re-plotting |

**The Word report includes the following metadata line:**

```
Generated: 2025-XX-XX HH:MM | CANCHARI PLS-SEM PRO V2.0 | seminr + R | seed = 123 | nboot = 5000
```

This metadata line should be preserved or reported in the supplementary material to ensure reproducibility.

---

## 8. Reproducibility

### 8.1 What "reproducibility" means in this application

CANCHARI PLS-SEM PRO guarantees **run-to-run consistency** within the same session configuration through a user-visible random seed. Setting the same seed, bootstrap iterations, and omission distance with the same dataset will produce numerically identical results every time.

### 8.2 What to document in your paper

Include the following statement in your Methods section:

> "Structural equation modeling was conducted using CANCHARI PLS-SEM PRO v2.0 (Canchari-Diaz et al., 2025), built on the `seminr` package (Ray et al., 2022) in R. All stochastic procedures were executed with random seed **[your seed]** and **5,000** bootstrap resamples using the bias-corrected percentile method. The project file (`.rds`) and the analysis configuration are available in the supplementary materials."

### 8.3 Sharing your analysis

To share a fully reproducible analysis:

1. **Save the project file:** Go to Project Manager → **💾 Save ALL**. This saves an `.rds` file containing the raw data, model specification, all parameters (including seed and nboot), and complete results tables.
2. **Share the `.rds` file** as supplementary material alongside your paper.
3. **Recipients** can load the project via **📂 Open Project** and reproduce all results exactly.

### 8.4 Running locally for strict reproducibility

For maximum reproducibility (e.g., pre-registration or replication studies), run the application locally:

```r
# Ensure exact package versions:
renv::snapshot()  # if using renv

# Run with fixed seed from the start:
shiny::runApp("app.R")
# Then set seed = 123 in the Analysis panel before running
```

The Word and HTML exports embed the seed value and timestamp in the report header, creating an auditable record.

---

## 9. Troubleshooting

### "No valid items found for construct X"

**Cause:** The item range specified (e.g., `PU1-PU4`) does not match the column names in your dataset.  
**Solution:** Check that column names in your CSV/Excel file match exactly (case-sensitive). Use the Data Preview table to verify column names after loading.

### "At least 2 groups are required for MICOM / MGA"

**Cause:** The selected group variable has fewer than 2 distinct values in the loaded data, or all groups fall below the minimum n threshold.  
**Solution:** Select a different group variable, or lower the "Minimum n per group" setting.

### "Construct scores do not contain model variables. Run PLS analysis first."

**Cause:** You attempted to run the Gaussian Copula test before running the main PLS analysis.  
**Solution:** Click **▶ RUN PLS-SEM ANALYSIS** in Step 5 first, then navigate to the Gaussian Copula tab.

### Analysis takes very long (hosted app)

**Cause:** Large dataset (n > 500) or complex model (6+ constructs) with 5,000 bootstrap iterations and MICOM/MGA enabled.  
**Solutions:**
- Reduce bootstrap iterations to 1,000 for exploratory runs; use 5,000 for final analysis.
- Run locally (see Section 2, Option B) for 50% faster execution.
- Disable MICOM/MGA for preliminary runs.

### Word export contains no tables

**Cause:** The analysis has not been run yet, or results were cleared.  
**Solution:** Run the analysis (Step 5) before attempting export (Step 7). Results persist within the session but are lost on page refresh unless saved via Project Manager.

### Path diagram is blank or shows only "PLS-SEM"

**Cause:** The model was not estimated successfully (check the Diagnostic Console for error messages).  
**Solution:** Validate the model in Step 3 → **🔍 Verify Configuration** before running analysis. Ensure all path endpoint constructs are defined.

### HTMT or AVE values appear as NA

**Cause:** A construct has a single indicator, making AVE and HTMT computationally undefined.  
**Solution:** Single-indicator constructs are treated as having AVE = 1.0 and loading = 1.0 by convention. If NA appears, check for data issues (constant column, all-missing column).

---

## 10. Example Dataset

### Dataset description

The included example dataset replicates a Technology Acceptance Model (TAM) study with three constructs:

- **DS** — Design Satisfaction (4 items: DS1–DS4)
- **PU** — Perceived Usefulness (4 items: PU1–PU4)
- **BI** — Behavioral Intention to Use (3 items: BI1–BI3)

**Sample:** n = 214 respondents; 7-point Likert scale (1 = Strongly Disagree, 7 = Strongly Agree).  
**Group variable:** `gender` (M / F) — for MICOM/MGA demonstration.

### Structural model

```
DS → PU → BI
DS → BI
```

### Expected results (seed = 123, nboot = 5,000)

| Path | β | t-value | p-value | Decision |
|---|---|---|---|---|
| DS → PU | 0.631 | 12.84 | < 0.001 | H1: Supported |
| PU → BI | 0.489 | 8.21 | < 0.001 | H2: Supported |
| DS → BI | 0.187 | 3.44 | 0.001 | H3: Supported |

| Construct | AVE | CR | α | HTMT (max) |
|---|---|---|---|---|
| DS | 0.612 | 0.863 | 0.814 | 0.712 |
| PU | 0.598 | 0.855 | 0.800 | 0.698 |
| BI | 0.634 | 0.839 | 0.720 | 0.743 |

These values match SmartPLS 4 output to within ±0.001 (numerical equivalence verified).

### Loading the example

1. Download [`example_data.csv`](example_data.csv) from the repository root.
2. In the app, go to **Step 2 — Data** → Upload File → select `example_data.csv`.
3. In **Step 3 — Model**, enter the constructs and paths as shown above.
4. Set seed = `123`, nboot = `5000` in the Analysis panel.
5. Run the analysis and compare your results with the table above.

---

## References

- Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Erlbaum.
- Hair, J. F., Henseler, J., Ringle, C. M., & Sarstedt, M. (2022). *A primer on partial least squares structural equation modeling (PLS-SEM)* (3rd ed.). SAGE.
- Henseler, J., Ringle, C. M., & Sarstedt, M. (2016). Testing measurement invariance of composites using partial least squares. *International Marketing Review*, 33(3), 405–431.
- Park, S., & Gupta, S. (2012). Handling endogeneity in marketing models using copulas. *Marketing Science*, 31(4), 567–586.
- Ray, S., Danks, N., & Calero Valdez, A. (2022). seminr: Building and estimating structural equation models. R package version 2.3.2. https://CRAN.R-project.org/package=seminr
- Roemer, E., Schuberth, F., & Henseler, J. (2021). HTMT2 — An improved criterion for assessing discriminant validity in structural equation modeling. *Industrial Management & Data Systems*, 121(12), 2637–2650.
- Shmueli, G., Sarstedt, M., Hair, J. F., Cheah, J. H., Ting, H., Vaithilingam, S., & Ringle, C. M. (2019). Predictive model assessment in PLS-SEM. *European Journal of Marketing*, 53(11), 2322–2347.

---

*Manual version 2.0 — Last updated: 2025. For issues or contributions, open a GitHub Issue at https://github.com/Miguekikoh89/canchari-pls-sempro/issues*
