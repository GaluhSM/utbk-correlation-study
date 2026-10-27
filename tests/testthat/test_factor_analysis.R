# =============================================================================
# Judul       : Unit Test Pemodelan Faktor (EFA & CFA)
# Deskripsi   : Menguji integritas komputasi EFA, konvergensi CFA, validitas
#               derajat kebebasan, ketiadaan Heywood cases, dan komparasi model.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-15
# =============================================================================

library(testthat)
library(readr)
library(here)
library(lavaan)
library(psych)

source(here::here("R", "constants.R"))
source(here::here("R", "utils_stats.R"))

test_that("run_utbk_efa menghasilkan eigenvalue dan muatan faktor yang valid", {
  df_clean <- readr::read_csv(
    here::here("data", "processed", "utbk_clean.csv"),
    show_col_types = FALSE
  )
  
  efa_out <- run_utbk_efa(
    df_data   = df_clean,
    subtests  = SUBTESTS,
    n_factors = 2L,
    rotate    = "oblimin"
  )
  
  # Struktur output list
  expect_type(efa_out, "list")
  expect_named(
    efa_out,
    c("parallel_res", "efa_res", "loadings_df", "eigenvalues")
  )
  
  # Dimensi muatan faktor (8 subtes x 2 faktor)
  expect_equal(nrow(efa_out$loadings_df), 8L)
  
  # Seluruh komunalitas (h2) dan uniqueness (u2) berada pada rentang [0, 1]
  expect_true(all(efa_out$loadings_df$h2 >= 0 & efa_out$loadings_df$h2 <= 1))
  expect_true(all(efa_out$loadings_df$u2 >= 0 & efa_out$loadings_df$u2 <= 1))
  
  # Eigenvalue pertama bernilai paling besar (> 1)
  expect_gt(efa_out$eigenvalues$empiris_fa[1], 1.0)
})

test_that(
  "CFA Model 1 dan Model 2 konvergen dengan derajat kebebasan yang tepat", {
    df_clean <- readr::read_csv(
      here::here("data", "processed", "utbk_clean.csv"),
      show_col_types = FALSE
    )
    
    cfa_out <- fit_utbk_cfa_models(df_clean, estimator = "MLM")
    
    # Konvergensi optimizer lavaan
    expect_true(lavaan::lavInspect(cfa_out$fit_1f, "converged"))
    expect_true(lavaan::lavInspect(cfa_out$fit_2f, "converged"))
    
    # Derajat kebebasan teoretis (ekstraksi as.integer agar lolos uji kelas tipe):
    # Matriks unik = 8 * 9 / 2 = 36
    # Model 1: 8 loadings + 8 residual variances = 16 parameter -> df = 20
    # Model 2: 5 + 3 loadings + 8 residuals + 1 cov = 17 parameter -> df = 19
    df_1f <- as.integer(unname(lavaan::fitMeasures(cfa_out$fit_1f, "df")))
    df_2f <- as.integer(unname(lavaan::fitMeasures(cfa_out$fit_2f, "df")))
    
    expect_equal(df_1f, 20L)
    expect_equal(df_2f, 19L)
  })

test_that("CFA tidak memiliki Heywood cases (varians residual positif)", {
  df_clean <- readr::read_csv(
    here::here("data", "processed", "utbk_clean.csv"),
    show_col_types = FALSE
  )
  
  cfa_out <- fit_utbk_cfa_models(df_clean, estimator = "MLM")
  
  # Ekstrak varians residual (theta)
  theta_1f <- diag(lavaan::lavInspect(cfa_out$fit_1f, "theta"))
  theta_2f <- diag(lavaan::lavInspect(cfa_out$fit_2f, "theta"))
  
  expect_true(all(theta_1f > 0))
  expect_true(all(theta_2f > 0))
  
  # Seluruh muatan terstandarisasi berada dalam rentang [0, 1]
  loadings_2f <- extract_cfa_loadings(cfa_out$fit_2f)
  expect_true(all(loadings_2f$lambda_std > 0 & loadings_2f$lambda_std < 1))
})

test_that(paste(
  "Model 2-Faktor memiliki kecocokan yang signifikan lebih baik",
  "daripada Model 1-Faktor"
), {
  df_clean <- readr::read_csv(
    here::here("data", "processed", "utbk_clean.csv"),
    show_col_types = FALSE
  )
  
  cfa_out <- fit_utbk_cfa_models(df_clean, estimator = "MLM")
  fit_comp <- extract_cfa_fit_indices(cfa_out$fit_1f, cfa_out$fit_2f)
  
  # Delta df harus tepat 1
  expect_equal(fit_comp$delta_df[2], 1L)
  
  # Penurunan chi-square harus positif dan signifikan (p < 0.001)
  expect_gt(fit_comp$delta_chisq[2], 0)
  expect_lt(fit_comp$delta_p[2], 0.001)
  
  # AIC Model 2 harus lebih rendah daripada Model 1
  expect_lt(fit_comp$aic[2], fit_comp$aic[1])
})