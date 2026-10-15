# Data Detective Pro: Complete Project Audit & Architectural Specification

**Audit Date:** 2026-10-15  
**Application:** Data Detective — Intelligent Dataset Exploration & Quality Fixing Platform  
**Target Architecture:** R + Shiny, bslib (Bootstrap 5), Shinylive (WebAssembly / webR) Compatible  
**Repository:** [vijaymahes9080/Data-Detective-application-using-R](https://github.com/vijaymahes9080/Data-Detective-application-using-R)  
**Live Site:** [https://vijaymahes9080.github.io/Data-Detective-application-using-R/](https://vijaymahes9080.github.io/Data-Detective-application-using-R/)

---

## 1. Executive Summary & Existing Baseline

### Current Technology Stack
- **Primary Language:** R (>= 4.0.0)
- **Framework:** Shiny
- **UI Architecture:** `bslib` (Zephyr / Bootstrap 5 theme, `page_sidebar` layout)
- **Data Manipulation:** Base R (`stats`, `utils`, `tools`, `readr`)
- **Data Visualization:** `ggplot2`
- **Data Tables:** `DT` (DataTables)
- **Deployment:** Static WebAssembly export via `shinylive::export(".", "site")` hosted on GitHub Pages via `.github/workflows/deploy-shinylive.yaml`.

### Existing Working Features
1. **File Ingestion:** Automated delimiter detection (comma, semicolon, tab, pipe) and basic CSV/TSV/RDS ingestion.
2. **Automated Profiling (`inspect_dataset`):** Basic dimension calculation, memory footprint estimation, column type inference (numeric, categorical, logical, date).
3. **Data Quality Indicator (`calculate_quality_indicator`):** Rule-based composite score (0–100) with heuristic penalties for missingness, duplicates, outliers, and constant columns.
4. **Diagnostic Tabs:** Overview, Data Quality, Missing Values, Duplicates, Outliers, Distributions, Correlations, Relationships, Leakage, Bias, and Report Dossier.
5. **Sample Datasets:** Clean HR Sample, Messy Anomaly Sample, and Realistic Sales Sample.

---

## 2. Identified Gaps & Problems in Existing Baseline

### Gap 1: 100% Read-Only Diagnostics (No Data Cleaning or Fixing Capabilities)
- In the original codebase, Data Detective only diagnoses problems. It explicitly notes in `mod_duplicates.R`:
  > *"Data Detective never alters or deletes records."*
- Users have no ability to fix missing values, deduplicate records, cap extreme outliers, cast data types, standardize dirty categories, or filter invalid rows.

### Gap 2: Missing Reversible State Management (No Undo / Redo / Reset)
- The app only held a single `dataset` reactive value.
- There was no separation between an immutable raw dataset (`dataset_raw`), an active working dataset (`dataset_working`), and a transformation history stack (`dataset_history`).

### Gap 3: Missing Column Studio & Deep Univariate Drilldown
- Columns could only be explored inside global distribution and outlier tabs rather than through a dedicated, unified **Column Studio** with type-specific diagnostics, quantile tables, and plain-language summaries.

### Gap 4: UI/UX Aesthetic Limitations
- The existing styling used generic light cards without clear visual hierarchy, missing tactile micro-interactions, missing before-and-after cell diffing, and lacked a dedicated workbench layout for issue remediation.

### Gap 5: Deployment & Shinylive Compatibility Safeguards
- Because deployment is targeted at **Shinylive** (running in-browser via WebAssembly), all cleaning operations must be implemented in pure, highly-optimized Base R and `stats` without relying on compiled C/C++ packages not supported by webR.

---

## 3. Architecture of the Interactive Quality Fixing Workbench

```
                        ┌───────────────────────────────┐
                        │      RAW UPLOAD / SAMPLE      │
                        └───────────────┬───────────────┘
                                        │
                                        ▼
                        ┌───────────────────────────────┐
                        │  dataset_raw (Immutable Base) │
                        └───────────────┬───────────────┘
                                        │
                                        ▼
                        ┌───────────────────────────────┐
                        │ dataset_working (Active Data) │◄─────────────┐
                        └───────────────┬───────────────┘              │
                                        │                              │
         ┌──────────────────────────────┴──────────────────────────────┤
         ▼                                                             │
┌─────────────────────────────────┐   Apply / Undo / Reset             │
│   ANALYTICAL PROFILING ENGINE   │   (Pushes / Pops state)            │
│ Missing │ Dups │ Outliers │ Type│                                    │
└────────────────┬────────────────┘                                    │
                 ▼                                                     │
┌─────────────────────────────────┐                                    │
│   UNIFIED FINDINGS & FIX CARDS  │                                    │
│ Detect → Explain → Configure    │                                    │
└────────────────┬────────────────┘                                    │
                 ▼                                                     │
┌─────────────────────────────────┐                                    │
│  BEFORE & AFTER DIFF ENGINE     │                                    │
│ Cell-level diffs & sample delta │                                    │
└────────────────┬────────────────┘                                    │
                 ▼                                                     │
┌─────────────────────────────────┐                                    │
│   R/transformation_engine.R     │────────────────────────────────────┘
│ Impute, Dedupe, Cap, Cast, Clean│
└─────────────────────────────────┘
```

---

## 4. Prioritized Engineering Changes

| Phase | Component | Action | Description |
|---|---|---|---|
| 1 | `docs/PROJECT_AUDIT.md` | NEW | Detailed audit and architectural specification |
| 2 | `R/transformation_engine.R` | NEW | Pure-R transformation library (imputation, deduplication, capping, casting, categories, rules, diffing) |
| 3 | `R/analysis_functions.R` | ENHANCE | Add type mismatch detection, category inconsistency analysis, and actionable metadata to findings |
| 4 | `R/mod_workbench.R` | NEW | Full Interactive Data Quality Fixing Workbench module with Issue Cards, Fix Centers, Diff Modal, and History |
| 5 | `R/mod_column_studio.R` | NEW | Dedicated Column Studio module for deep univariate inspection and visualization |
| 6 | `app.R` | UPDATE | Multi-state reactive management (`dataset_raw`, `dataset_working`, `dataset_history`), tab integration, sidebar controls |
| 7 | `www/styles.css` | UPGRADE | Data-Dense Dashboard theme, diff highlight styling, micro-animations, reduced-motion accessibility |
| 8 | `tests/test_transformations.R` | NEW | Comprehensive test suite for all transformations, edge cases, immutability, and undo/reset |
| 9 | `README.md` | UPDATE | Modernized documentation reflecting Data Detective Pro with interactive cleaning capabilities |

---

## 5. Technical & Statistical Risk Controls

1. **Statistical Honesty:**
   - Outliers are treated as statistically unusual observations, not confirmed errors. Winsorizing and removal require explicit user confirmation.
   - Imputation methods (mean, median, mode, forward fill) display sample bias warnings.
2. **Data Safety:**
   - `dataset_raw` is strictly read-only and immutable.
   - Any transformation can be undone instantly using the session history stack or reverted completely to original.
3. **WebAssembly Compatibility:**
   - Strict avoidance of unsupported binaries; pure Base R routines guarantee zero-dependency execution in browser via Shinylive.
