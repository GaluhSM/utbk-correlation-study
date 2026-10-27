# =============================================================================
# Judul       : 13 Ekspor Data untuk Web Kalkulator Peringkat
# Deskripsi   : Menyusun web/data.js dari keluaran R yang sama dengan paper,
#               sehingga angka di web identik dengan angka di paper. Web
#               hanya mengonsumsi berkas ini (tanpa menjalankan R) dan
#               menghitung peringkat di peramban lewat rumus yang sama dengan
#               scripts/06_ranking_simulation.R. Skrip ini hanya merangkai
#               (tanpa menghitung ulang) parameter, skenario, bootstrap,
#               sensitivitas SD nasional, dan rasio ekor distribusi/SLODR.
#               Dipanggil sebagai tahap terakhir run_all.R; boleh juga
#               dijalankan sendiri setelah keluaran R tersedia.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-10-26
# Input       : data/raw/utbk_national_descriptive_statistics.csv
#               data/metadata/dictionary.csv
#               output/tables/tabel_ranking_komposit.csv
#               output/tables/boot_ringkasan.csv
#               output/tables/sens_sd_nasional.csv
#               output/tables/slodr_ekor.csv
# Output      : web/data.js
# =============================================================================

# Muat Package ----------------------------------------------------------------
library(dplyr)
library(readr)
library(here)
library(jsonlite)

# Muat Modul Utilitas ---------------------------------------------------------
source(here::here("R", "constants.R"))

# Pengaturan Metadata Web -----------------------------------------------------
# Isi DOI dan URL repositori setelah paper terbit; web menampilkan tautannya
# hanya bila tidak kosong. Setelah diubah, jalankan ulang skrip ini.
WEB_VERSI <- "1.0"
WEB_DOI   <- ""
WEB_REPO  <- ""
WEB_JUDUL_PAPER <- paste0(
  "Seberapa Kuat Subtes UTBK-SNBT 2026 Saling Berkorelasi? ",
  "Estimasi dari Data Crowdsourced dan Implikasinya bagi ",
  "Estimasi Peringkat Nasional"
)
WEB_SUMBER_N <- paste0(
  "Paparan konferensi pers hasil SNBT 2026, Panitia SNPMB, 25 Mei 2026"
)

# Singkatan tampilan subtes (urutan input di formulir mengikuti berkas CSV).
SINGKATAN_SUBTES <- c(
  pu = "PU", ppu = "PPU", pbm = "PBM", pk = "PK",
  lbi_saintek = "LBI Saintek", lbi_soshum = "LBI Soshum",
  lbe = "LBE", pm = "PM"
)

# 1. Impor Keluaran ------------------------------------------------------------
baca <- function(...) {
  readr::read_csv(here::here(...), col_types = readr::cols(),
                  show_col_types = FALSE)
}
data_nat    <- baca("data", "raw", "utbk_national_descriptive_statistics.csv")
kamus       <- baca("data", "metadata", "dictionary.csv")
komposit    <- baca("output", "tables", "tabel_ranking_komposit.csv")
boot        <- baca("output", "tables", "boot_ringkasan.csv")
sens_sd     <- baca("output", "tables", "sens_sd_nasional.csv")
slodr_ekor  <- baca("output", "tables", "slodr_ekor.csv")

stopifnot(
  "Subtes di CSV nasional harus sama dengan SUBTESTS" =
    setequal(data_nat$subject_name, SUBTESTS)
)

# 2. Subtes -------------------------------------------------------------------
subtes <- lapply(seq_len(nrow(data_nat)), function(i) {
  kode <- data_nat$subject_name[i]
  list(
    kode    = kode,
    singkat = unname(SINGKATAN_SUBTES[kode]),
    label   = kamus$var_label[match(kode, kamus$var_name)],
    sd      = data_nat$standard_deviation[i],
    min     = data_nat$minimum[i],
    q1      = data_nat$quartile_one[i],
    median  = data_nat$median[i],
    q3      = data_nat$quartile_three[i],
    maks    = data_nat$maximum[i]
  )
})

# 3. Skenario Komposit (urutan sama dengan Tabel 9 paper) -------------------
skenario_unik <- komposit |>
  dplyr::distinct(scenario, r_assumed, sd_pop_z)

skenario <- lapply(seq_len(nrow(skenario_unik)), function(i) {
  asal <- skenario_unik$scenario[i]
  info <- switch(
    asal,
    "Raw Sample" = list("mentah", "Korelasi sampel mentah", "data"),
    "Thorndike Case III" = list(
      "thorndike", "Estimasi Thorndike Case III", "data"
    ),
    list("hip", "Hipotetis (sensitivitas)", "hipotetis")
  )
  list(
    kode  = info[[1]], nama = info[[2]], jenis = info[[3]],
    r     = skenario_unik$r_assumed[i], sd = skenario_unik$sd_pop_z[i]
  )
})

# 4. Bootstrap dan Sensitivitas SD Nasional -----------------------------------
ambil_boot <- function(nama) {
  baris <- boot[boot$statistik == nama, ]
  list(est = baris$estimasi, lo = baris$ci_bawah, hi = baris$ci_atas)
}
bootstrap <- list(
  s_z = ambil_boot("S_Z populasi"),
  r   = ambil_boot("R-bar terkoreksi")
)

sens <- lapply(seq_len(nrow(sens_sd)), function(i) {
  list(
    label = sens_sd$skenario[i],
    s_z   = sens_sd$s_population_z[i],
    r     = sens_sd$r_terkoreksi_rerata[i]
  )
})

# 5. Rasio Ekor Atas (distribusi miring; SLODR b = 0,02 dan 0,04) -----------
kemiringan_dipakai <- c(0, 0.02, 0.04)
ekor_df <- slodr_ekor |>
  dplyr::filter(
    vapply(kemiringan, function(x) {
      any(abs(x - kemiringan_dipakai) < 1e-9)
    }, logical(1)),
    k_sd > 0
  ) |>
  dplyr::mutate(rasio = pct_ekor_sim / pct_ekor_normal)

ekor <- lapply(seq_len(nrow(ekor_df)), function(i) {
  list(
    b     = ekor_df$kemiringan[i],
    k     = as.integer(ekor_df$k_sd[i]),
    rasio = ekor_df$rasio[i]
  )
})

# 6. Rakit dan Simpan ---------------------------------------------------------
web_data <- list(
  meta = list(
    versi       = WEB_VERSI,
    n_populasi  = N_POPULATION,
    mu          = MU_POPULATION,
    sumber_n    = WEB_SUMBER_N,
    judul_paper = WEB_JUDUL_PAPER,
    doi         = WEB_DOI,
    repo        = WEB_REPO
  ),
  subtes    = subtes,
  skenario  = skenario,
  bootstrap = bootstrap,
  sens_sd   = sens,
  ekor      = ekor
)

json_teks <- jsonlite::toJSON(
  web_data, auto_unbox = TRUE, digits = NA, pretty = TRUE
)

dir.create(here::here("web"), showWarnings = FALSE)
readr::write_lines(
  paste0("window.UTBK_DATA = ", json_teks, ";"),
  here::here("web", "data.js")
)

message(
  "Selesai! web/data.js disimpan. Verifikasi dengan: ",
  "node tests/web/verify_web.mjs"
)