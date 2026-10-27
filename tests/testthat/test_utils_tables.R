# =============================================================================
# Judul       : Unit Test Utilitas Format Tabel Publikasi
# Deskripsi   : Menguji fungsi-fungsi di R/utils_tables.R memakai data toy
#               mandiri (tidak bergantung file data proyek yang sesungguhnya).
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-17
# =============================================================================

library(testthat)
library(here)

source(here::here("R", "utils_tables.R"))

test_that("p_to_stars menghasilkan notasi bintang sesuai ambang batas", {
  hasil <- p_to_stars(c(0.2, 0.03, 0.005, 0.0001, NA))
  expect_equal(hasil, c("", "$^{*}$", "$^{**}$", "$^{***}$", ""))
})

test_that("format_latex_correlation_table mengembalikan string LaTeX valid", {
  dn  <- list(c("A", "B"), c("A", "B"))
  mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, dimnames = dn)
  
  hasil <- format_latex_correlation_table(
    mat, "tab:contoh", "Contoh Tabel", "Catatan kaki."
  )
  
  expect_type(hasil, "character")
  expect_length(hasil, 1L)
  expect_true(grepl("\\\\begin\\{table\\}", hasil))
  expect_true(grepl("tab:contoh", hasil))
})

test_that(paste(
  "format_latex_cfa_fit_table hanya beri bintang jika p aktual",
  "signifikan"
), {
  df_fit <- tibble::tibble(
    model            = c("Model 1", "Model 2"),
    chisq_scaled     = c(120.5, 80.2),
    df_scaled        = c(20L, 19L),
    cfi_robust       = c(0.90, 0.97),
    tli_robust       = c(0.88, 0.96),
    rmsea_robust     = c(0.08, 0.04),
    rmsea_ci_lower   = c(0.07, 0.03),
    rmsea_ci_upper   = c(0.09, 0.05),
    srmr             = c(0.06, 0.03),
    aic              = c(5000, 4900),
    delta_chisq      = c(NA, 40.3),
    delta_df         = c(NA, 1L),
    delta_p          = c(NA, 0.0001)
  )
  
  hasil <- format_latex_cfa_fit_table(
    df_fit, "tab:cfa", "Contoh Fit CFA", "Catatan kaki."
  )
  
  expect_true(grepl("\\$\\^\\{\\*\\*\\*\\}\\$", hasil))
  expect_false(grepl("NA\\$\\^", hasil))
})

test_that(paste(
  "format_latex_factor_loadings_table beri bintang per-baris dari",
  "p_value"
), {
  df_loadings <- tibble::tibble(
    latent_factor = c("Literacy", "Literacy"),
    subtest       = c("pu", "ppu"),
    lambda_std    = c(0.75, 0.20),
    se            = c(0.02, 0.05),
    z_value       = c(37.5, 4.0),
    p_value       = c(0.0001, 0.049),
    r_squared     = c(0.56, 0.04)
  )
  
  hasil <- format_latex_factor_loadings_table(
    df_loadings, "tab:loadings", "Contoh Muatan Faktor", "Catatan kaki."
  )
  
  expect_true(grepl("0,750\\$\\^\\{\\*\\*\\*\\}\\$", hasil))
  expect_true(grepl("0,200\\$\\^\\{\\*\\}\\$", hasil))
})