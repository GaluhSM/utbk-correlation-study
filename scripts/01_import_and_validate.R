# =============================================================================
# Judul       : 01 Import dan Validasi Integritas Data
# Deskripsi   : Membaca dataset mentah skor UTBK-SNBT 2026 dan parameter
#               deskriptif nasional, memvalidasi aturan batas, menghitung
#               variabel komposit mean_8, dan mengekspor data bersih.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-14
# Input       : data/raw/utbk_score_samples.csv
#               data/raw/utbk_national_descriptive_statistics.csv
# Output      : data/processed/utbk_clean.csv
# =============================================================================

# Muat Package ----------------------------------------------------------------
library(dplyr)
library(readr)
library(here)
library(tibble)

source(here::here("R", "constants.R"))

# Konstanta Path --------------------------------------------------------------
PATH_RAW_SAMPLES  <- here::here("data", "raw", "utbk_score_samples.csv")
PATH_RAW_NATIONAL <- here::here(
  "data", "raw",
  "utbk_national_descriptive_statistics.csv"
)
PATH_OUT_PROCESSED <- here::here("data", "processed", "utbk_clean.csv")

# 1. Impor Data Mentah --------------------------------------------------------
data_samples_raw <- readr::read_csv(
  PATH_RAW_SAMPLES,
  show_col_types = FALSE
)

data_nat_raw <- readr::read_csv(
  PATH_RAW_NATIONAL,
  show_col_types = FALSE
)

# 2. Pemeriksaan Struktur Dasar -----------------------------------------------
stopifnot(
  "Jumlah baris sampel harus sesuai N_SAMPLE_EXPECTED" =
    nrow(data_samples_raw) == N_SAMPLE_EXPECTED,
  "Seluruh subtes harus ada di kolom" = all(SUBTESTS %in% names(data_samples_raw)),
  "Kolom stage harus ada" = "stage" %in% names(data_samples_raw),
  "Tidak boleh ada nilai NA pada sampel" = sum(is.na(data_samples_raw)) == 0
)

# 3. Validasi Batas Nilai terhadap Parameter Nasional -------------------------
for (subtes in SUBTESTS) {
  row_nat <- data_nat_raw |>
    dplyr::filter(subject_name == subtes)
  
  stopifnot(nrow(row_nat) == 1)
  
  min_nas <- row_nat$minimum
  max_nas <- row_nat$maximum
  
  nilai_min_sampel <- min(data_samples_raw[[subtes]])
  nilai_max_sampel <- max(data_samples_raw[[subtes]])
  
  if (nilai_min_sampel < min_nas || nilai_max_sampel > max_nas) {
    stop(
      sprintf(
        "Subtes %s memiliki nilai di luar batas tidak resmi [%.2f, %.2f]. Ditemukan: [%.2f, %.2f]",
        subtes, min_nas, max_nas, nilai_min_sampel, nilai_max_sampel
      )
    )
  }
}

# 4. Transformasi Data: Hitung Komposit dan Standarisasi -----------------------
data_clean <- data_samples_raw |>
  dplyr::mutate(
    mean_8 = rowMeans(dplyr::across(dplyr::all_of(SUBTESTS)))
  ) |>
  dplyr::relocate(dplyr::all_of(SUBTESTS), mean_8, stage)

# Verifikasi komposit
stopifnot(
  "mean_8 harus numerik" = is.numeric(data_clean$mean_8),
  "mean_8 tidak boleh ada NA" = sum(is.na(data_clean$mean_8)) == 0
)

# 5. Simpan Dataset Bersih (Analysis-Ready) ------------------------------------
if (!dir.exists(dirname(PATH_OUT_PROCESSED))) {
  dir.create(dirname(PATH_OUT_PROCESSED), recursive = TRUE)
}

readr::write_csv(data_clean, PATH_OUT_PROCESSED)

message("Sukses! Data tervalidasi dan disimpan di: ", PATH_OUT_PROCESSED)
message(sprintf("Dimensi data bersih: %d baris x %d kolom", nrow(data_clean), ncol(data_clean)))