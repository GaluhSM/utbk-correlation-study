# =============================================================================
# Judul       : Unit Test Statistik Deskriptif & Validitas Data
# Deskripsi   : Menguji kelengkapan data bersih, kepatuhan batas nilai nasional,
#               dan kalkulasi komposit mean_8.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-14
# =============================================================================

library(testthat)
library(readr)
library(here)
library(dplyr)

source(here::here("R", "constants.R"))

test_that("Dataset bersih memiliki dimensi dan integritas yang tepat", {
  path_clean <- here::here("data", "processed", "utbk_clean.csv")
  expect_true(file.exists(path_clean))
  
  df_clean <- readr::read_csv(path_clean, show_col_types = FALSE)
  
  # Pastikan N = N_SAMPLE_EXPECTED dan 10 kolom
  expect_equal(nrow(df_clean), N_SAMPLE_EXPECTED)
  expect_equal(ncol(df_clean), 10L)
  
  # Tidak boleh ada missing value
  expect_equal(sum(is.na(df_clean)), 0L)
})

test_that("Nilai subtes berada dalam rentang parameter tidak resmi nasional", {
  df_clean <- readr::read_csv(
    here::here("data", "processed", "utbk_clean.csv"),
    show_col_types = FALSE
  )
  df_nat   <- readr::read_csv(
    here::here("data", "raw", "utbk_national_descriptive_statistics.csv"),
    show_col_types = FALSE
  )
  
  for (subtes in SUBTESTS) {
    row_nat <- df_nat |> dplyr::filter(subject_name == subtes)
    min_val <- min(df_clean[[subtes]])
    max_val <- max(df_clean[[subtes]])
    
    expect_gte(min_val, row_nat$minimum)
    expect_lte(max_val, row_nat$maximum)
  }
})

test_that("Komposit mean_8 dihitung secara deterministik dan presisi", {
  df_clean <- readr::read_csv(
    here::here("data", "processed", "utbk_clean.csv"),
    show_col_types = FALSE
  )
  
  computed_means <- rowMeans(df_clean[SUBTESTS])
  expect_equal(df_clean$mean_8, computed_means, tolerance = 1e-12)
})