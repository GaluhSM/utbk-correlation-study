# UTBK-SNBT 2026 Inter-Subtest Correlation & Latent Factor Structure Replication

> Butuh ini dalam Bahasa Indonesia? Klik di [sini](./%5BID%5DREADME.md)

[![R \>= 4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://www.r-project.org/) [![Code License: MIT](https://img.shields.io/badge/Code_License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) [![Data License: CC BY 4.0](https://img.shields.io/badge/Data_License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/) [![Reproducibility: renv](https://img.shields.io/badge/reproducibility-renv-green.svg)](https://rstudio.github.io/renv/) [![R CI](https://github.com/GaluhSM/utbk-correlation-study/actions/workflows/r-ci.yaml/badge.svg)](https://github.com/GaluhSM/utbk-correlation-study/actions/workflows/r-ci.yaml)

Open-source R replication package and dataset for estimating inter-subtest correlations, latent factor structures (EFA & CFA), range restriction bias, and national percentile projections on the 2026 Indonesian University Entrance Examination (UTBK-SNBT).

## Table of Contents

- [About](#about)
- [Features](#features)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Project Structure](#project-structure)
- [Usage](#usage)
- [Web Calculator](#web-calculator)
- [Data](#data)
- [Testing](#testing)
- [Reproducibility](#reproducibility)
- [Documentation](#documentation)
- [Contributing](#contributing)
- [License](#license)
- [Citation](#citation)
- [Contact](#contact)

## About

The inter-subtest correlation structure and latent cognitive dimensionality of the 2026 UTBK-SNBT are not publicly available, and the subtest-level descriptive statistics in circulation derive from an unofficial, unverified internal table attributed to the national admissions board (SNPMB). This project provides a fully reproducible pipeline for:

1.  Estimating Pearson correlation matrices across 8 cognitive subtests ($N = 1,886$).
2.  Modeling latent cognitive dimensionality via Exploratory Factor Analysis (EFA; Horn's Parallel Analysis) and Confirmatory Factor Analysis (CFA; 1-factor $g$-model vs. 2-factor domain-specific model).
3.  Quantifying selection bias and attenuation via **Thorndike Case III** multivariate range restriction correction.
4.  Solving the population composite standard deviation ($S_Z$) via full-covariance fixed-point iteration.
5.  Modeling calibrated national percentile rankings.

## Features

- **Standardized Pipeline:** Strictly formatted adhering to tidyverse principles and Project TIER standards.
- **Factor Analytic Modeling:** Robust Maximum Likelihood (`MLM`) CFA modeling comparing general $g$ vs. domain-specific (Verbal Literacy vs. Numeracy/Logic) constructs.
- **High-Precision Solvers:** Vectorized implementation of Lawley-Aitken / Thorndike Case III iteration.
- **Automated Outputs:** Publication-ready LaTeX tables (`booktabs`), correlation heatmaps, scree plots, and exhaustive computational audit PDF reports.
- **Automated Codebook:** Generates variable dictionaries dynamically from data dictionaries.
- **Web Calculator:** A static, client-side page that turns the paper's results into a rough national rank estimate for an individual's eight subtest scores.

## Prerequisites

- **R Version:** `R >= 4.1.0`
- **RStudio** (recommended)
- **LaTeX Distribution:** TinyTeX, TeX Live, or MiKTeX (for PDF compilation)

## Installation

Clone the repository and restore the exact environment dependencies:

``` bash
git clone https://github.com/GaluhSM/utbk-correlation-study.git
cd utbk-correlation-study
```

Inside R console:

``` r
install.packages("renv")
renv::restore()
```

## Project Structure

``` text
utbk-correlation-study/
├── README.md                      # English documentation
├── [ID]README.md                  # Indonesian documentation
├── LICENSE                        # Open-access license
├── CITATION.cff                   # Citation metadata (GitHub "Cite this repository", Zenodo)
├── .github/
│   └── workflows/
│       ├── r-ci.yaml              # Lint (report only) and unit tests
│       └── pages.yml              # Deploys web/ to GitHub Pages
├── run_all.R                      # Master pipeline orchestration
├── data/
│   ├── raw/                       # Immutable raw datasets
│   │   ├── utbk_score_samples.csv
│   │   └── utbk_national_descriptive_statistics.csv
│   ├── processed/                 # Validated analysis dataset
│   │   └── utbk_clean.csv
│   └── metadata/                  # Machine-readable dictionaries
│       ├── dictionary.csv
│       └── stage_labels.csv
├── R/                             # Isolated utility modules
│   ├── utils_stats.R
│   ├── utils_tables.R
│   └── utils_figures.R
├── scripts/                       # Sequential execution scripts
│   ├── 01_import_and_validate.R
│   ├── 02_descriptive_analysis.R
│   ├── 03_correlation_models.R
│   ├── 04_factor_analysis.R
│   ├── 05_stage_saturation.R
│   ├── 06_ranking_simulation.R
│   ├── 07_generate_codebook.R
│   ├── 08_export_paper_tables.R   # Runs last; builds LaTeX tables, incl. those from 09-12
│   ├── 09_sensitivity_lbi.R       # LBI sensitivity: 3-factor EFA, CFA residual covariance, Case III with 8/7/6 subtests
│   ├── 10_bootstrap_uncertainty.R # Bootstrap CIs and sensitivity to the unofficial national SDs
│   ├── 11_nonnormality_sensitivity.R # Non-normality diagnostics; percentile sensitivity (Johnson SB + Gaussian copula)
│   ├── 12_slodr_sensitivity.R     # SLODR scenarios (level-dependent correlation); reads output of 11
│   └── 13_export_web_data.R       # Builds web/data.js from the outputs above; last step of run_all.R
├── reports/
│   └── exact_computational_report.Rmd
├── tests/
│   ├── testthat/
│   │   ├── test_codebook.R
│   │   ├── test_descriptive.R
│   │   ├── test_factor_analysis.R
│   │   ├── test_thorndike.R
│   │   ├── test_utils_figures.R
│   │   └── test_utils_tables.R
│   └── web/
│       └── verify_web.mjs         # Checks the JavaScript results against output/tables/
├── docs/
│   └── codebook.md
├── web/                           # Static web calculator (no build step)
│   ├── index.html
│   ├── style.css
│   ├── app.js
│   ├── calc.js
│   └── data.js
├── output/
│   ├── tables/
│   ├── figures/
│   └── reports/
└── paper/                         # LaTeX manuscript (merged 2026-09;
                                    # see main.tex, preamble.tex, sections/)
```

## Usage

To execute the complete analysis pipeline, run unit tests, and generate all tables/reports with a single command:

``` r
source("run_all.R")
```

> **Run time and order.** `run_all.R` executes the sensitivity scripts `09`-`12` before the unit tests and the table export (`08`). They are simulation-heavy (bootstrap, copula, nested calibration) and take several minutes in total. `12` needs the output of `11`, so keep that order if you run them individually. `run_all.R` ends with script `13`, which rewrites `web/data.js` from the outputs (see [Web Calculator](#web-calculator)).

## Web Calculator

The static page in `web/` estimates a participant's national rank from the eight subtest scores. It complements the paper and has not been validated separately. The result is a rough estimate based on unofficial national parameters; it is neither an official ranking nor a prediction of admission.

- **Live page:** <https://galuhsm.github.io/utbk-correlation-study/>
- **Privacy:** everything runs in the browser. Scores are neither sent nor stored; there are no cookies, analytics, ads, or external fonts and scripts. The page is not affiliated with SNPMB.
- **Method:** the same formula as `scripts/06_ranking_simulation.R` ($z = (x - 500) / SD$, upper normal tail probability times $N = 846,518$), shown across the correlation scenarios of the paper, with the bootstrap interval, the sensitivity to the unofficial national SDs, and indicative tail adjustments from scripts `11` and `12`.
- **Data flow:** R outputs in `output/tables/` $\rightarrow$ `scripts/13_export_web_data.R` $\rightarrow$ `web/data.js` $\rightarrow$ browser. `run_all.R` runs `13` as its last step; it can also be run alone with `source("scripts/13_export_web_data.R")`. The DOI and repository URL shown by the page are set at the top of script `13`.
- **Consistency check:** `node tests/web/verify_web.mjs` (requires Node.js) compares the JavaScript results with the R tables.
- **Local preview:** open `web/index.html` in a browser; no server is needed.
- **Deployment:** GitHub Pages through `.github/workflows/pages.yml` (Settings $\rightarrow$ Pages $\rightarrow$ Source: GitHub Actions), or Cloudflare Pages (empty build command, output directory `web`).
- **Frozen:** the numbers equal those in the paper and are not updated after release `v1.0`.

## Data

The analysis uses two primary datasets stored in `data/raw/`:

- `utbk_score_samples.csv`: Crowdsourced test scores ($N = 1,886$) collected across 19 sequential stages.
- `utbk_national_descriptive_statistics.csv`: Unofficial, unverified national descriptive parameters, taken from an internal table attributed to the national admissions board (SNPMB) that circulated on social media. Verification is only partial (triangulated on the maximum PK and PM scores); see the Method and Limitations sections of the paper. The number of attendees ($N = 846,518$, out of 871,496 registrants) is not part of this table; it comes from the official SNPMB press-conference slides (25 May 2026) and is defined in `R/constants.R`.

Variable definitions and scaling rules are documented in [`docs/codebook.md`](./docs/codebook.md).

## Testing

Unit tests are implemented using `testthat`. Execute all tests with:

``` r
testthat::test_dir("tests/testthat")
```

## Reproducibility

- Dependencies are locked via `renv.lock`.
- Numerical algorithms use explicit tolerance thresholds (`tol = 1e-15`).
- Session information is embedded directly inside rendered computational reports.

## Documentation

Full variable definitions, encoding rules, and psychometric operationalizations are detailed in [`docs/codebook.md`](./docs/codebook.md).

## Contributing

Contributions, issues, and feature requests are welcome. Please open an issue to discuss proposed modifications.

## License

This project uses a **dual license**, as detailed in [`LICENSE`](./LICENSE):

- **Code** (`R/`, `scripts/`, `tests/`, `web/`, and standalone `.R`/`.Rmd` files): [MIT License](https://opensource.org/licenses/MIT).
- **Data, documentation & manuscript** (`data/`, `docs/`, `output/`, and `.csv`/`.tex`/`.pdf` files): [Creative Commons Attribution 4.0 International License (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/).

## Citation

``` bibtex
@article{placeholder2026utbk,
  author = {Saputra, Galuh},
  title  = {How Strongly Correlated Are the Subtests in the 2026 Indonesian University Entrance Exam (UTBK-SNBT)? Crowdsourced Estimates and Implications for National Ranking},
  year   = {2026},
  url    = {[https://github.com/GaluhSM/utbk-correlation-study](https://github.com/GaluhSM/utbk-correlation-study)}
}
```

GitHub's *Cite this repository* button reads [`CITATION.cff`](./CITATION.cff).

## Contact

- **Author:** Galuh Saputra
- **Email:** `galmei.id@gmail.com`