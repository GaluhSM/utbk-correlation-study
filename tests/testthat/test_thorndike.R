# =============================================================================
# Judul       : Unit Test Model Matematika Thorndike Case III
# Deskripsi   : Menguji properti matriks koreksi Thorndike Case III dan
#               konvergensi fixed-point iteration.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-14
# =============================================================================

library(testthat)
library(readr)
library(here)

source(here::here("R", "constants.R"))
source(here::here("R", "utils_stats.R"))

test_that(
  "thorndike_case_3 mempertahankan identitas matriks saat lambda_sq = 0", {
    mat_dummy <- matrix(c(1.0, 0.3, 0.3, 1.0), nrow = 2)
    vec_dummy <- c(0.6, 0.6)
    
    mat_result <- thorndike_case_3(mat_dummy, vec_dummy, lambda_sq = 0.0)
    expect_equal(mat_result, mat_dummy, tolerance = 1e-12)
  })

test_that("thorndike_case_3 menghasilkan matriks simetris dan diagonal 1.0", {
  df_clean <- readr::read_csv(
    here::here("data", "processed", "utbk_clean.csv"),
    show_col_types = FALSE
  )
  
  mat_raw  <- cor(as.matrix(df_clean[SUBTESTS]))
  vec_r_iz <- vapply(
    SUBTESTS, function(s) cor(df_clean[[s]], df_clean$mean_8), numeric(1)
  )
  
  mat_corr <- thorndike_case_3(mat_raw, vec_r_iz, lambda_sq = 0.581757)
  
  expect_true(isSymmetric(mat_corr, tol = 1e-12))
  expect_equal(unname(diag(mat_corr)), rep(1.0, 8))
  expect_true(all(mat_corr >= -1.0 & mat_corr <= 1.0))
})

test_that(
  "solve_population_sd mencapai konvergensi stabil pada batas presisi tinggi", {
    df_clean <- readr::read_csv(
      here::here("data", "processed", "utbk_clean.csv"),
      show_col_types = FALSE
    )
    df_nat   <- readr::read_csv(
      here::here("data", "raw", "utbk_national_descriptive_statistics.csv"),
      show_col_types = FALSE
    )
    
    mat_raw  <- cor(as.matrix(df_clean[SUBTESTS]))
    vec_r_iz <- vapply(
      SUBTESTS, function(s) cor(df_clean[[s]], df_clean$mean_8), numeric(1)
    )
    sig_nat  <- setNames(df_nat$standard_deviation, df_nat$subject_name)[SUBTESTS]
    
    res <- solve_population_sd(
      mat_r_raw     = mat_raw,
      vec_r_iz      = vec_r_iz,
      vec_sigma_nat = sig_nat,
      s_sample_z    = sd(df_clean$mean_8),
      s_init        = 0.300380571821857,
      tol           = TOL_FIXED_POINT,
      max_iter      = MAX_ITER_FIXED_POINT
    )
    
    # Prasyarat sebelum memeriksa hasil apa pun: solve_population_sd()
    # sendiri mendokumentasikan bahwa hasil non-konvergen memberi
    # warning() dan converged = FALSE — jangan laporkan hasil sbg
    # konvergen tanpa memeriksa field ini terlebih dahulu.
    expect_true(res$converged)
    expect_equal(unname(res$s_population_z), 67.7166172742717, tolerance = 1e-6)
    expect_equal(unname(res$mean_r_corrected), 0.424598730237, tolerance = 1e-6)
    expect_lte(tail(res$history$delta, 1), TOL_FIXED_POINT)
  })