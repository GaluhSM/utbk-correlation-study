# =============================================================================
# Judul       : Unit Test Utilitas Visualisasi
# Deskripsi   : Menguji fungsi-fungsi di R/utils_figures.R memakai data toy
#               mandiri (tidak bergantung file data proyek yang sesungguhnya).
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-17
# =============================================================================

library(testthat)
library(here)

source(here::here("R", "utils_stats.R"))
source(here::here("R", "utils_figures.R"))

test_that("plot_correlation_heatmap mengembalikan objek ggplot", {
  dn  <- list(c("A", "B", "C"), c("A", "B", "C"))
  mat <- matrix(
    c(1, 0.5, 0.2, 0.5, 1, 0.3, 0.2, 0.3, 1), nrow = 3, dimnames = dn
  )
  
  p <- plot_correlation_heatmap(mat, judul = "Contoh Heatmap")
  
  expect_s3_class(p, "ggplot")
})

test_that("plot_correlation_heatmap menolak input non-matriks persegi", {
  expect_error(plot_correlation_heatmap(matrix(1:6, nrow = 2)))
})

test_that("plot_scree_parallel mengembalikan objek ggplot", {
  # Objek toy yang meniru struktur output psych::fa.parallel() —
  # hanya field fa.values dan fa.sim yang dipakai plot_scree_parallel().
  pa_res_toy <- list(
    fa.values = c(3.2, 1.1, 0.6, 0.4),
    fa.sim    = c(1.3, 1.1, 0.9, 0.7)
  )
  
  p <- plot_scree_parallel(pa_res_toy)
  
  expect_s3_class(p, "ggplot")
})

test_that("plot_scree_parallel menolak objek tanpa fa.values", {
  expect_error(plot_scree_parallel(list(fa.sim = c(1, 2))))
})