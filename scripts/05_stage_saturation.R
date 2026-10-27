# =============================================================================
# Judul       : 05 Analisis Progresi dan Saturasi Korelasi per Tahap
# Deskripsi   : Menghitung korelasi off-diagonal kumulatif seiring bertambahnya
#               volume sampel dan klaster tahapan pengumpulan data.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-14
# Input       : data/processed/utbk_clean.csv
#               data/metadata/stage_labels.csv
# Output      : output/tables/tabel_saturasi_tahap.csv
# =============================================================================

library(dplyr)
library(readr)
library(here)
library(purrr)

source(here::here("R", "constants.R"))
source(here::here("R", "utils_stats.R"))

data_clean   <- readr::read_csv(
  here::here("data", "processed", "utbk_clean.csv"),
  col_types      = readr::cols(),
  show_col_types = FALSE
)
stage_labels <- readr::read_csv(
  here::here("data", "metadata", "stage_labels.csv"),
  col_types      = readr::cols(),
  show_col_types = FALSE
)


# Hitung kumulatif per stage
unique_stages <- sort(unique(data_clean$stage))

tabel_tahap <- purrr::map_dfr(unique_stages, function(stg) {
  df_sub <- data_clean |> dplyr::filter(stage <= stg)
  mat_sub <- cor(as.matrix(df_sub[SUBTESTS]))
  
  stg_info <- stage_labels |> dplyr::filter(stage == stg)
  
  tibble::tibble(
    stage       = stg,
    stage_code  = ifelse(
      nrow(stg_info) > 0, stg_info$stage_code, paste0("stage_", stg)
    ),
    stage_name  = ifelse(nrow(stg_info) > 0, stg_info$stage_name, "-"),
    category    = ifelse(nrow(stg_info) > 0, stg_info$category, "-"),
    n_cumulative = nrow(df_sub),
    r_off_diagonal = compute_off_diagonal_mean(mat_sub)
  )
})

readr::write_csv(
  tabel_tahap, here::here("output", "tables", "tabel_saturasi_tahap.csv")
)
message("Selesai! Tabel saturasi tahapan disimpan.")