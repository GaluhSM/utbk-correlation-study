# =============================================================================
# Judul       : 03 Model Korelasi Sampel dan Thorndike Case III
# Deskripsi   : Menghitung korelasi sampel mentah, konvergensi fixed-point S_Z,
#               korelasi terkoreksi Thorndike Case III, dan diagnostik asumsi
#               seleksi (apakah SD populasi yang tersirat cocok dengan SD
#               nasional tidak resmi).
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-19
# Input       : data/processed/utbk_clean.csv
#               data/raw/utbk_national_descriptive_statistics.csv
# Output      : output/tables/matriks_korelasi_raw.rds
#               output/tables/matriks_korelasi_terkoreksi.rds
#               output/tables/hasil_thorndike.rds
#               output/tables/iteration_history.csv
#               output/tables/tabel_diagnostik_seleksi.csv
#               output/figures/heatmap_raw.png
#               output/figures/heatmap_thorndike.png
# =============================================================================

# Muat Package ----------------------------------------------------------------
library(readr)
library(here)

# Muat Modul Utilitas ---------------------------------------------------------
source(here::here("R", "constants.R"))
source(here::here("R", "utils_stats.R"))
source(here::here("R", "utils_figures.R"))

# Konstanta Path --------------------------------------------------------------
DIR_OUT_TABLES  <- here::here("output", "tables")
DIR_OUT_FIGURES <- here::here("output", "figures")

# 1. Impor Data ---------------------------------------------------------------
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

n_sample_str <- format(nrow(data_clean), big.mark = ".", decimal.mark = ",")

# 2. Korelasi Sampel Mentah ---------------------------------------------------
mat_scores <- as.matrix(data_clean[SUBTESTS])
mat_r_raw  <- cor(mat_scores)

# Korelasi Subtes ke Komposit (r_iZ)
vec_r_iz <- vapply(SUBTESTS, function(subtes) {
  cor(data_clean[[subtes]], data_clean$mean_8)
}, numeric(1))

# Parameter Simpangan Baku Nasional
sigma_nat_dict <- setNames(data_nat$standard_deviation, data_nat$subject_name)
stopifnot(
  "Seluruh subtes harus ada di data nasional" =
    all(SUBTESTS %in% names(sigma_nat_dict))
)
vec_sigma_nat <- sigma_nat_dict[SUBTESTS]

s_sample_z <- sd(data_clean$mean_8)

# 3. Fixed-Point Iteration Thorndike Case III ---------------------------------
# Tebakan awal alami: s_sample_z. Toleransi dan batas iterasi dari
# constants.R (tol 1e-15 lama berada di bawah resolusi double).
res_iter <- solve_population_sd(
  mat_r_raw     = mat_r_raw,
  vec_r_iz      = vec_r_iz,
  vec_sigma_nat = vec_sigma_nat,
  s_sample_z    = s_sample_z,
  s_init        = s_sample_z,
  tol           = TOL_FIXED_POINT,
  max_iter      = MAX_ITER_FIXED_POINT
)

# Hentikan pipeline bila tidak konvergen: angka S_Z dan korelasi terkoreksi
# tidak boleh mengalir ke tabel paper tanpa konvergensi terverifikasi.
if (!isTRUE(res_iter$converged)) {
  stop(
    "Fixed-point iteration tidak konvergen (delta terakhir = ",
    format(res_iter$final_delta, digits = 3),
    "). Periksa data/parameter sebelum melanjutkan.",
    call. = FALSE
  )
}

# 4. Diagnostik Asumsi Seleksi ------------------------------------------------
# Case III mengasumsikan seleksi hanya lewat komposit. Bila benar, SD populasi
# yang tersirat dari sampel mendekati SD nasional tidak resmi (rasio ~ 1).
vec_sd_sample <- vapply(SUBTESTS, function(subtes) {
  sd(data_clean[[subtes]])
}, numeric(1))

diag_selection <- check_selection_assumption(
  subtests      = SUBTESTS,
  sd_sample     = vec_sd_sample,
  vec_r_iz      = vec_r_iz,
  lambda_sq     = res_iter$lambda_sq,
  vec_sigma_nat = vec_sigma_nat
)

# 5. Simpan Hasil Komputasi ---------------------------------------------------
dir.create(DIR_OUT_TABLES, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_OUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

saveRDS(mat_r_raw, file.path(DIR_OUT_TABLES, "matriks_korelasi_raw.rds"))
saveRDS(
  res_iter$mat_r_corrected,
  file.path(DIR_OUT_TABLES, "matriks_korelasi_terkoreksi.rds")
)
# Objek hasil utuh, dipakai skrip hilir (06) agar tidak menebak nilai dari
# file CSV riwayat iterasi.
saveRDS(res_iter, file.path(DIR_OUT_TABLES, "hasil_thorndike.rds"))
readr::write_csv(
  res_iter$history,
  file.path(DIR_OUT_TABLES, "iteration_history.csv")
)
readr::write_csv(
  diag_selection,
  file.path(DIR_OUT_TABLES, "tabel_diagnostik_seleksi.csv")
)

# 6. Generate Heatmap Figures -------------------------------------------------
p1 <- plot_correlation_heatmap(
  mat_r_raw,
  sprintf("Matriks Korelasi Raw Sample (N = %s)", n_sample_str)
)
p2 <- plot_correlation_heatmap(
  res_iter$mat_r_corrected, "Matriks Korelasi Terkoreksi (Thorndike Case III)"
)

ggplot2::ggsave(
  file.path(DIR_OUT_FIGURES, "heatmap_raw.png"),
  p1, width = 7, height = 6, dpi = 300
)
ggplot2::ggsave(
  file.path(DIR_OUT_FIGURES, "heatmap_thorndike.png"),
  p2, width = 7, height = 6, dpi = 300
)

# 7. Ringkasan di Konsol ------------------------------------------------------
message(sprintf(
  "Iterasi konvergen pada siklus ke-%d (delta akhir = %.3e, tol = %.1e)",
  res_iter$n_iterations, res_iter$final_delta, TOL_FIXED_POINT
))
message(sprintf(
  "S_Z Populasi: %.15f | Rerata R terkoreksi: %.15f",
  res_iter$s_population_z, res_iter$mean_r_corrected
))
message(sprintf(
  "Diagnostik seleksi: rasio SD tersirat/nasional berkisar %.2f - %.2f",
  min(diag_selection$ratio), max(diag_selection$ratio)
))
if (any(abs(diag_selection$ratio - 1) > 0.15)) {
  message(
    "PERINGATAN: sebagian subtes menyimpang >15% dari SD nasional. Asumsi ",
    "seleksi-hanya-lewat-komposit (Case III) tidak sepenuhnya terpenuhi; ",
    "laporkan sebagai limitasi (lihat tabel_diagnostik_seleksi.csv)."
  )
}