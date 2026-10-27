# =============================================================================
# Judul       : 06 Matriks Translasi Persentil Standar (Redesign)
# Deskripsi   : Menerjemahkan titik persentil standar (M-3SD..M+3SD, aturan
#               68-95-99.7) menjadi skor mentah — per subtes (SD nasional
#               univariat, tanpa perlu skenario korelasi) dan per komposit
#               (SD teoretis komposit di bawah berbagai asumsi korelasi
#               antarsubtes, termasuk estimasi eksak Thorndike Case III).
#               Sengaja TIDAK memakai skor individu siapa pun. Properti
#               inti tabel ini: pada baris (titik benchmark) yang sama,
#               persentil SELALU identik lintas kolom (skenario/subtes) —
#               satu-satunya yang berubah adalah skor mentah yang
#               berkorespondensi dengannya. Ini menekankan bahwa
#               ketidakpastian asumsi korelasi berdampak pada "skor berapa
#               untuk persentil X", bukan pada persentil itu sendiri.
#               (Redesign — gantikan pendekatan skor individu versi
#               sebelumnya yang diambil dari baris pertama data sampel.)
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-26
# Input       : data/processed/utbk_clean.csv
#               data/raw/utbk_national_descriptive_statistics.csv
#               output/tables/iteration_history.csv
# Output      : output/tables/tabel_ranking_subtes.csv
#               output/tables/tabel_ranking_komposit.csv
# =============================================================================

# Muat Package ----------------------------------------------------------------
library(dplyr)
library(readr)
library(here)
library(tibble)
library(purrr)

# Muat Modul Utilitas ---------------------------------------------------------
source(here::here("R", "constants.R"))
source(here::here("R", "utils_stats.R"))

# 1. Impor Data ----------------------------------------------------------------
data_clean <- readr::read_csv(
  here::here("data", "processed", "utbk_clean.csv"),
  col_types      = readr::cols(),
  show_col_types = FALSE
)
data_nat <- readr::read_csv(
  here::here("data", "raw", "utbk_national_descriptive_statistics.csv"),
  col_types      = readr::cols(),
  show_col_types = FALSE
)

# 2. Titik Benchmark Standar ---------------------------------------------------
# Konvensi 68-95-99.7: tujuh titik M-3SD .. M+3SD dalam satuan SD populasi.
# top_pct di tiap titik HANYA fungsi dari z (rumus normal baku) — properti
# inilah yang membuat tabel ini benar-benar "matriks translasi persentil":
# skor mentah boleh berubah antarkolom, persentil di tiap baris tidak.
BENCHMARK_Z <- c(-3, -2, -1, 0, 1, 2, 3)
BENCHMARK_LABEL <- c(
  "M - 3 SD", "M - 2 SD", "M - 1 SD", "M",
  "M + 1 SD", "M + 2 SD", "M + 3 SD"
)

#' Hitung Persentil "Top" dari Z-Score Baku
#'
#' @param z Numeric vector, titik benchmark dalam satuan SD populasi.
#'
#' @return Numeric vector, persentase populasi yang bernilai >= titik
#'   tersebut (skala 0-100).
top_pct_from_z <- function(z) {
  (1 - pnorm(z)) * 100
}

#' Hitung Estimasi Peringkat Nasional dari Persentil "Top"
#'
#' @param top_pct Numeric vector, skala 0-100 (lihat `top_pct_from_z()`).
#'
#' @return Integer vector, estimasi peringkat nasional (dibulatkan).
rank_from_top_pct <- function(top_pct) {
  round(N_POPULATION * top_pct / 100)
}

benchmark_percentile <- tibble::tibble(
  benchmark_label = BENCHMARK_LABEL,
  z               = BENCHMARK_Z,
  top_pct         = top_pct_from_z(BENCHMARK_Z),
  rank_nasional   = rank_from_top_pct(top_pct_from_z(BENCHMARK_Z))
)

# 3. Tabel Translasi per Subtes (Univariat) ------------------------------------
# Tidak perlu skenario korelasi di sini: skor tiap subtes diamati langsung
# secara nasional, sehingga SD-nya sudah pasti diketahui (bukan hasil
# pemodelan) — beda dengan komposit di bagian 4.
sigma_nat_dict <- setNames(data_nat$standard_deviation, data_nat$subject_name)

tabel_rank_subtes <- purrr::map_dfr(SUBTESTS, function(subtes) {
  sd_nas <- sigma_nat_dict[[subtes]]
  dplyr::bind_cols(
    tibble::tibble(subtest = subtes),
    benchmark_percentile,
    tibble::tibble(score = MU_POPULATION + BENCHMARK_Z * sd_nas)
  )
})

# 4. Tabel Translasi Komposit Lintas Skenario Korelasi -------------------------
sum_sq_sig <- sum(data_nat$standard_deviation^2)
sq_sum_sig <- (sum(data_nat$standard_deviation))^2

# Muat hasil iterasi utk nilai Thorndike Case III eksak (dihitung di
# scripts/03, disimpan agar tidak perlu iterasi ulang di sini).
history_df <- readr::read_csv(
  here::here("output", "tables", "iteration_history.csv"),
  col_types      = readr::cols(),
  show_col_types = FALSE
)
sd_thorndike <- tail(history_df$sd_composite_out, 1)
r_thorndike  <- tail(history_df$mean_r_corrected, 1)
r_raw        <- compute_off_diagonal_mean(cor(as.matrix(data_clean[SUBTESTS])))

#' Simpangan Baku Komposit Teoretis di Bawah Korelasi Rata-Rata Seragam
#'
#' Formula umum utk k subtes berkorelasi rata-rata r yang sama antarpasangan
#' (asumsi penyederhanaan). Untuk versi eksak Thorndike Case III yang TIDAK
#' mengasumsikan korelasi seragam antarpasangan, lihat `solve_population_sd()`
#' di `R/utils_stats.R` — hasilnya (`sd_thorndike`) dipakai langsung sbg satu
#' skenario di bawah, bukan dihitung ulang lewat fungsi ini.
#'
#' @param r Numeric skalar, asumsi korelasi rata-rata antarsubtes.
#'
#' @return Numeric skalar, SD komposit teoretis.
sd_scenario <- function(r) {
  sqrt(((1 - r) * sum_sq_sig + r * sq_sum_sig) / (N_SUBTESTS^2))
}

# Desain simetris di sekitar estimasi Thorndike Case III: tiga skenario di
# bawahnya (Raw Sample, 0.350, 0.400) dan tiga di atasnya (0.450, 0.500,
# 0.550). Nilai-nilai r hipotetis berfungsi sbg analisis sensitivitas, bukan
# klaim rentang literatur.
scenarios <- list(
  list(label = "Raw Sample",         r = r_raw,       sd = sd_scenario(r_raw)),
  list(label = "r = 0.350",          r = 0.350,       sd = sd_scenario(0.350)),
  list(label = "r = 0.400",          r = 0.400,       sd = sd_scenario(0.400)),
  list(label = "Thorndike Case III", r = r_thorndike, sd = sd_thorndike),
  list(label = "r = 0.450",          r = 0.450,       sd = sd_scenario(0.450)),
  list(label = "r = 0.500",          r = 0.500,       sd = sd_scenario(0.500)),
  list(label = "r = 0.550",          r = 0.550,       sd = sd_scenario(0.550))
)

tabel_rank_komposit <- purrr::map_dfr(scenarios, function(scen) {
  dplyr::bind_cols(
    tibble::tibble(
      scenario  = scen$label,
      r_assumed = scen$r,
      sd_pop_z  = scen$sd
    ),
    benchmark_percentile,
    tibble::tibble(score = MU_POPULATION + BENCHMARK_Z * scen$sd)
  )
})

# Verifikasi invarian desain: persentil di titik benchmark yang sama HARUS
# identik lintas skenario (hanya skor mentah yang boleh berbeda). Kalau ini
# gagal, ada bug pada sd_scenario()/urutan BENCHMARK_Z, bukan pada data.
invarian_top_pct <- tabel_rank_komposit |>
  dplyr::group_by(benchmark_label) |>
  dplyr::summarise(rentang_top_pct = max(top_pct) - min(top_pct), .groups = "drop")

stopifnot(
  "Persentil harus identik lintas skenario pada titik benchmark yang sama" =
    all(invarian_top_pct$rentang_top_pct < 1e-9)
)

# 5. Simpan Output --------------------------------------------------------------
readr::write_csv(
  tabel_rank_subtes, here::here("output", "tables", "tabel_ranking_subtes.csv")
)
readr::write_csv(
  tabel_rank_komposit,
  here::here("output", "tables", "tabel_ranking_komposit.csv")
)

message(
  "Selesai! Matriks translasi persentil standar (subtes & komposit) ",
  "disimpan."
)