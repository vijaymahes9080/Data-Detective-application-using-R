# DATA DETECTIVE
> **Automated Dataset Investigation Platform**  
> *"Upload. Investigate. Understand."*

![R](https://img.shields.io/badge/Language-R%20%3E%3D%204.0-blue.svg)
![Shiny](https://img.shields.io/badge/Framework-Shiny-brightgreen.svg)
![License](https://img.shields.io/badge/License-MIT-green.svg)
![DataQuality](https://img.shields.io/badge/System-Statistical%20Investigation-purple.svg)

---

## 1. Problem Statement

Data practitioners spend up to 80% of their analysis time discovering data quality anomalies, structural defects, missingness mechanisms, and statistical quirks in tabular datasets. Often, these issues remain hidden until models perform poorly or yield invalid conclusions.

**Data Detective** is a statistical data-quality and automated dataset investigation platform built with R and Shiny. Users upload any tabular dataset (CSV), and the application systematically audits its structure, statistical distributions, anomalies, relationships, and potential points of failure—transforming raw data into an actionable **Dataset Investigation Report**.

> **Important System Notice:**  
> Data Detective is a **deterministic, rule-based statistical investigation platform**. It is **NOT** an AI chatbot and does **NOT** rely on external AI or cloud APIs. Every metric, chart, and finding is computed dynamically from the active dataset.

---

## 2. Objectives

- **Automated Profiling:** Instant computation of variable types, memory footprint, completeness, and central tendencies.
- **Transparent Quality Scoring:** Rule-based composite data-quality scoring with granular point deductions.
- **Statistical Anomaly Diagnostics:** Rigorous IQR and Z-score outlier detection without labeling points as "errors".
- **Relational Integrity:** Full pairwise Pearson correlation, multicollinearity flagging ($|r| \ge 0.70$), and bivariate scatter exploration.
- **Pattern & Representation Screening:** Screening for category concentration, differential missingness across cohorts, and data leakage signals.
- **Structured Investigation Dossier:** Generating categorized findings (High, Warning, Info) with explicit evidence and recommended next steps.
- **Instant Export:** One-click download of self-contained HTML reports and audit CSVs.

---

## 3. Technology Stack

- **Primary Language:** R (>= 4.0.0)
- **Application Framework:** Shiny
- **UI Architecture:** bslib (Modern Bootstrap 5 Zephyr theme), HTML5, CSS3
- **Data Wrangling:** readr, utils, stats, tools
- **Visualization:** ggplot2
- **Interactive Tables:** DT (DataTables)
- **Reporting:** Self-contained HTML report generator & R Markdown / knitr template

---

## 4. Architecture & Project Structure

Data Detective enforces strict separation between data ingestion, analytical engines, presentation modules, and reporting:

```
DATA INPUT  ──►  DATA VALIDATION  ──►  PROFILING  ──►  QUALITY SCORING
                                                           │
                                                           ▼
REPORT GENERATION  ◄──  FINDING ENGINE  ◄──  STATISTICAL & PATTERN ENGINES
```

### Directory Layout

```
Data-Detective/
├── app.R                          # Main Shiny Application (UI & Server orchestration)
├── DESCRIPTION                    # R Package & Dependency metadata
├── README.md                      # Comprehensive project documentation
├── .gitignore                     # R and OS exclusions
│
├── R/                             # Modular Analytical Engines
│   ├── utility_functions.R        # Formatting, safe moments, and data type detection
│   ├── data_loader.R              # CSV validation, safe ingestion, column name repair
│   ├── data_profile.R             # Dataset dimensions, memory footprint, column summaries
│   ├── data_quality.R             # Transparent composite quality scoring & deductions
│   ├── missing_analysis.R         # Missingness diagnostics, severity levels & row stats
│   ├── duplicate_analysis.R       # Exact & potential duplicate record detection
│   ├── constant_analysis.R        # Constant, near-constant & low-variance column identification
│   ├── outlier_analysis.R         # IQR bounds (1.5x) and Z-score outlier detection
│   ├── distribution_analysis.R    # Summary parameters, empirical density, factual observations
│   ├── categorical_analysis.R     # Frequencies, category concentration & rare classes
│   ├── correlation_analysis.R     # Pearson correlation matrix, pairs & multicollinearity
│   ├── anomaly_analysis.R         # Target analysis, leakage cues, representation & group stats
│   ├── finding_engine.R           # Central finding rules engine & executive summary generator
│   └── report_generator.R         # Self-contained HTML report builder & CSV export helpers
│
├── modules/                       # Shiny UI/Server Modules
│   ├── mod_overview.R             # Summary cards, quality indicator badge, dataset preview
│   ├── mod_quality.R              # Missing values bar chart, duplicates viewer, constant vars
│   ├── mod_outliers.R             # Outliers summary, variable drilldown, boxplot, histogram
│   ├── mod_distributions.R        # Distribution metrics, empirical density curve, factual notes
│   ├── mod_categorical.R          # Class frequencies, category concentration banner, bar plot
│   ├── mod_correlations.R         # Correlation heatmap, bivariate scatter, multicollinearity table
│   ├── mod_patterns.R             # Target analysis, leakage indicators, representation, groups, dates
│   ├── mod_findings.R             # Central Investigation Center, severity filters, finding cards
│   └── mod_report.R               # Live report preview, HTML & CSV download handlers
│
├── www/                           # Static Assets & Styling
│   └── style.css                  # Custom styling, card borders, typography
│
├── reports/                       # Report Templates
│   └── report_template.Rmd        # R Markdown investigation report template
│
├── sample_data/                   # Curated Synthetic Test Datasets
│   ├── sample_clean.csv           # Baseline clean HR dataset (25 rows, 8 cols)
│   ├── sample_messy.csv           # Anomaly dataset (missingness, duplicates, outliers, constants)
│   └── sample_sales.csv           # Business sales dataset (Date, Product, Region, Price, Revenue)
│
└── tests/                         # Automated Unit Tests
    ├── testthat.R                 # Testthat test driver
    └── testthat/
        ├── test-profiling.R       # Dataset profiling & dimensions tests
        ├── test-quality.R         # Missingness, duplicates & constant checks tests
        ├── test-outliers.R        # IQR outlier bounds & Pearson correlation tests
        └── test-findings.R        # Finding rules engine & narrative generator tests
```

---

## 5. Installation & Setup

### Prerequisites

Install R (version 4.0 or later). Install required packages from CRAN:

```r
install.packages(c(
  "shiny",
  "bslib",
  "DT",
  "ggplot2",
  "readr",
  "testthat",
  "rmarkdown",
  "knitr"
))
```

### Running Locally or Online

Launch the application directly from the project directory:

```r
# In R or RStudio
shiny::runApp()
```

Or deploy directly to **shinyapps.io**, **Posit Cloud**, or a containerized environment (Docker / Shiny Server).

---

## 6. How to Use Data Detective

1. **Upload Dataset:** On the welcome landing screen, select your CSV dataset or click one of the pre-loaded sample datasets (*Clean HR*, *Messy Anomalies*, *Sales*).
2. **Review Dashboard:** Check high-level metric cards, the composite Data Quality Status badge, and the dynamic Executive Summary.
3. **Inspect Quality & Missingness:** Dive into column missingness distributions, examine duplicate records in the dedicated viewer, and review uninformative constants.
4. **Diagnose Outliers & Distributions:** Select numeric variables to view boxplots and histograms with 1.5×IQR boundary lines.
5. **Analyze Correlations:** Review the Pearson heatmap and check the Multicollinearity table for variables sharing $|r| \ge 0.70$.
6. **Screen Patterns & Leakage:** Designate a target column to screen for potential data leakage indicators and assess representation disparities across cohorts.
7. **Audit Findings:** Navigate to the **Investigation Center** to filter findings by severity (High, Warning, Info) and review transparent evidence and recommended next steps.
8. **Export Dossier:** Download a self-contained HTML report or raw findings in CSV format.

---

## 7. Statistical Investigation Methodology

| Investigation Domain | Detection Algorithm / Heuristic | Flagging Thresholds |
|---|---|---|
| **Missing Values** | Exact column-wise and row-wise counts | Complete (0%), Low ($\le 5\%$), Moderate ($5-20\%$), High ($20-50\%$), Critical ($> 50\%$) |
| **Duplicates** | Exact row duplication across all features | Flagged if $\ge 1$ duplicate row exists; High severity if $\ge 5\%$ of dataset |
| **Outliers** | Tukey's Interquartile Range (IQR) method | Lower Bound = $Q_1 - 1.5 \times \text{IQR}$; Upper Bound = $Q_3 + 1.5 \times \text{IQR}$ |
| **Constants / Low Variance** | Distinct value cardinality and variance | Strictly constant ($N_{\text{uniq}} \le 1$), Near-constant ($\ge 90\%$ concentration), or $\text{Var} < 10^{-9}$ |
| **Correlations** | Pairwise Pearson product-moment ($r$) | Strong ($|r| \ge 0.70$), Very Strong / Multicollinear ($|r| \ge 0.90$) |
| **Data Leakage** | Feature-target similarity & extreme correlation | Name embedding, near-identity ($\ge 99\%$ match), or $|r| \ge 0.95$ with target |
| **Representation Disparities** | Sub-cohort empirical frequencies | Category concentration ($\ge 90\%$) or differential missingness ($\ge 30\%$ gap across groups) |

### Investigation Language Principles

- **Neutrality:** Uses terms such as *"statistically unusual observation"* instead of *"wrong value"*.
- **Causality Caution:** Reassures users that *"correlation indicates statistical association, not causation"*.
- **Ethical Integrity:** Evaluates representation as descriptive statistical concentration; never asserts bias, intent, or discrimination.
- **Exploratory Guidance:** Potential leakage signals are flagged as *"exploratory signals, never proof of leakage"*.

---

## 8. Automated Testing

Run the full unit test suite using `testthat`:

```r
testthat::test_dir("tests/testthat")
```

The test suite covers:
- Empty and malformed dataset handling.
- Dataset dimensions, memory sizing, and data type detection.
- Missingness calculation and heuristic severity classification.
- Exact and potential duplicate identification.
- Constant and near-constant variable isolation.
- IQR outlier computation and interval boundary accuracy.
- Pearson correlation matrix calculation and strength thresholding.
- Rule-based finding engine generation and dynamic summary synthesis.

---

## 9. Future-Ready Architecture

The codebase is built with modular abstraction layers to facilitate future extensions:
- Additional file parsers (Excel `.xlsx`, Parquet, JSON, Arrow).
- Database drivers (PostgreSQL, DuckDB, Snowflake).
- Advanced statistical hypothesis testing (normality tests, ANOVA, chi-square).
- Schema drift and dataset version comparison.

---

## 10. License & Author

- **Author:** Vijay Mahes
- **Email:** [Vijaypradhap2004@gmail.com](mailto:Vijaypradhap2004@gmail.com)
- **License:** MIT License
