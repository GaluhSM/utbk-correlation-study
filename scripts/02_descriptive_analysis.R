# =============================================================================
# Judul       : 02 Analisis Statistik Deskriptif Univariat
# Deskripsi   : Menghitung statistik sampel vs populasi nasional dan selisihnya.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-14
# Input       : data/processed/utbk_clean.csv
#               data/raw/utbk_national_descriptive_statistics.csv
# Output      : output/tables/tabel_deskriptif_komparasi.csv
# =============================================================================

library(dplyr)
library(readr)
library(here)
library(tibble)

source(here::here("R", "constants.R"))

# 1. Muat Data ----------------------------------------------------------------
data_clean <- readr::read_csv(here::here("data", "processed", "utbk_clean.csv"), show_col_types = FALSE)
data_nat   <- readr::read_csv(here::here("data", "raw", "utbk_national_descriptive_statistics.csv"), show_col_types = FALSE)

# 2. Komputasi Statistik Sampel -----------------------------------------------
summary_sample <- tibble::tibble(
  subject_name = SUBTESTS,
  mean_sample  = vapply(data_clean[SUBTESTS], mean, numeric(1)),
  sd_sample    = vapply(data_clean[SUBTESTS], sd, numeric(1)),
  median_sample = vapply(data_clean[SUBTESTS], median, numeric(1)),
  min_sample   = vapply(data_clean[SUBTESTS], min, numeric(1)),
  max_sample   = vapply(data_clean[SUBTESTS], max, numeric(1))
)

# 3. Gabungkan dengan Parameter Nasional ---------------------------------------
comparison_table <- summary_sample |>
  dplyr::left_join(
    data_nat |>
      dplyr::select(
        subject_name,
        mean_nat = mean,
        sd_nat   = standard_deviation,
        min_nat  = minimum,
        max_nat  = maximum
      ),
    by = "subject_name"
  ) |>
  dplyr::mutate(
    mean_diff = mean_sample - mean_nat,
    sd_diff   = sd_sample - sd_nat
  )

# 4. Simpan Output ------------------------------------------------------------
dir.create(here::here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(comparison_table, here::here("output", "tables", "tabel_deskriptif_komparasi.csv"))
message("Selesai! Tabel deskriptif disimpan.")