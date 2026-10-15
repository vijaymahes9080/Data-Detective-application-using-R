# LinkedIn Project Launch Post

> **Post Image Asset:** [`image.png`](image.png) (Attach this image with your LinkedIn post)

---

### Post Copy (Ready to Copy-Paste into LinkedIn)

Most data cleaning scripts start the same way: hunting down silent missing values, checking for duplicate records, and finding out which features are secretly measuring the exact same thing before model training.

To automate that exploratory pass, I built **Data Detective** — an interactive data quality and statistical profiling platform written in R and Shiny.

The key detail: it runs **100% inside your browser via Shinylive (WebAssembly)**. There is no backend server, no container setup, and your data never leaves your laptop.

Here is what it handles under the hood:

- **Instant Dataset Profiling:** Memory footprint, column types, distinct values, and central tendency moments without manual `summary()` calls.
- **Data Quality Score (0–100):** A deterministic score with clear, itemized deductions for missingness, duplicates, outliers, and constant columns.
- **Outlier Diagnostics:** Tukey's $1.5 \times \text{IQR}$ fences and Z-score metrics. It flags observations as statistically unusual rather than blindly assuming they are errors.
- **Correlation & Collinearity Matrix:** Interactive Pearson heatmap with automatic alerts when any feature pair hits $|r| \ge 0.70$.
- **Leakage & Bias Screening:** Flags post-target naming cues, target duplication, and cohort-level missingness shifts.
- **Visual Analytics with 3D Depth:** Pure-R isometric 3D projections with azimuth and elevation rotation sliders, alongside standard 2D views.
- **Reproducible Script Export:** Generates a standalone `pipeline.R` file using pure Base R math, so you can re-run the entire audit headlessly or in CI.

The project is completely free and open source:

👉 **Live In-Browser App:** https://vijaymahes9080.github.io/Data-Detective-application-using-R/  
👉 **GitHub Repository:** https://github.com/vijaymahes9080/Data-Detective-application-using-R  

If you work with tabular datasets in R, try it out with one of your messy files. Feedback and suggestions are always welcome!

#RStats #DataScience #OpenSource #DataQuality
