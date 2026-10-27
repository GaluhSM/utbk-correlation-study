# =============================================================================
# Judul       : 04 Pemodelan Analisis Faktor (EFA & CFA)
# Deskripsi   : Menjalankan Horn's Parallel Analysis, EFA 2-faktor, dan
#               estimasi CFA (Model 1-Faktor g vs. Model 2-Faktor Domain)
#               untuk menguji struktur dimensionalitas laten 8 subtes.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-15
# Input       : data/processed/utbk_clean.csv
# Output      : output/tables/efa_summary.rds
#               output/tables/cfa_model_fit_comparison.csv
#               output/tables/cfa_factor_loadings.csv
#               output/tables/cfa_latent_correlation.csv
#               output/figures/scree_parallel_plot.png
# =============================================================================

# Muat Package ----------------------------------------------------------------
library(dplyr)
library(readr)
library(tibble)
library(here)
library(lavaan)
library(psych)
library(ggplot2)

# Muat Modul Utilitas ---------------------------------------------------------
source(here::here("R", "constants.R"))
source(here::here("R", "utils_stats.R"))
source(here::here("R", "utils_figures.R"))

# Konstanta -------------------------------------------------------------------
PATH_DATA_CLEAN <- here::here("data", "processed", "utbk_clean.csv")
DIR_OUT_TABLES  <- here::here("output", "tables")
DIR_OUT_FIGURES <- here::here("output", "figures")
# SEED_PARALLEL_ANALYSIS (benih RNG utk psych::fa.parallel() yg stokastik)
# dipakai dari R/constants.R — satu sumber kebenaran, lihat komentar di sana.

# 1. Impor Data Analisis ------------------------------------------------------
data_clean <- readr::read_csv(
  PATH_DATA_CLEAN,
  col_types     = readr::cols(),
  show_col_types = FALSE
)

# 2. Exploratory Factor Analysis (EFA) & Parallel Analysis --------------------
message("-> Menjalankan Horn's Parallel Analysis dan EFA...")

efa_output <- run_utbk_efa(
  df_data   = data_clean,
  subtests  = SUBTESTS,
  n_factors = 2L,
  rotate    = "oblimin",
  fm        = "minres",
  seed      = SEED_PARALLEL_ANALYSIS
)

# 3. Confirmatory Factor Analysis (CFA): 1-Faktor vs 2-Faktor -----------------
message("-> Melakukan fitting model CFA (Robust MLM Estimator)...")

cfa_models <- fit_utbk_cfa_models(
  df_data   = data_clean,
  estimator = "MLM"
)

# Ekstraksi Fit Indices dan Komparasi Chi-Square Satorra-Bentler
cfa_fit_table <- extract_cfa_fit_indices(
  fit_1f = cfa_models$fit_1f,
  fit_2f = cfa_models$fit_2f
)

# Ekstraksi Muatan Faktor Terstandarisasi Model 2-Faktor
cfa_loadings_table <- extract_cfa_loadings(cfa_models$fit_2f)

# Ekstraksi Korelasi Antarfaktor Laten (Literacy ~~ Numeracy)
latent_cov_df <- lavaan::standardizedSolution(cfa_models$fit_2f) |>
  dplyr::filter(lhs == "literacy" & rhs == "numeracy" & op == "~~")

phi_estimate <- latent_cov_df$est.std
phi_pvalue   <- latent_cov_df$pvalue

message(
  sprintf(
    "-> Korelasi antarfaktor laten (phi): %.3f (p = %.4f)",
    phi_estimate, phi_pvalue
  )
)

# 4. Simpan Artefak Komputasi & Tabel Data ------------------------------------
dir.create(DIR_OUT_TABLES, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_OUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

saveRDS(
  efa_output,
  file.path(DIR_OUT_TABLES, "efa_summary.rds")
)

readr::write_csv(
  cfa_fit_table,
  file.path(DIR_OUT_TABLES, "cfa_model_fit_comparison.csv")
)

readr::write_csv(
  cfa_loadings_table,
  file.path(DIR_OUT_TABLES, "cfa_factor_loadings.csv")
)

# Simpan korelasi antarfaktor laten (phi) agar skrip hilir (08) bisa
# memakainya kembali tanpa perlu fit ulang model CFA yang sudah dihitung
# di sini.
readr::write_csv(
  tibble::tibble(phi_estimate = phi_estimate, phi_pvalue = phi_pvalue),
  file.path(DIR_OUT_TABLES, "cfa_latent_correlation.csv")
)

# 5. Visualisasi Scree Plot Parallel Analysis ---------------------------------
p_scree <- plot_scree_parallel(
  parallel_res = efa_output$parallel_res,
  judul        = "Horn's Parallel Analysis Scree Plot (UTBK-SNBT 2026)"
)

ggplot2::ggsave(
  filename = file.path(DIR_OUT_FIGURES, "scree_parallel_plot.png"),
  plot     = p_scree,
  width    = 7,
  height   = 5,
  dpi      = 300
)

message("Sukses! Pemodelan faktor EFA/CFA selesai dieksekusi dan disimpan.")