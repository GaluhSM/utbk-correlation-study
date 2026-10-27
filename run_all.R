# =============================================================================
# Judul       : Master Script Orkestrasi Replikasi
# Deskripsi   : Menjalankan seluruh pipeline analisis dari impor, pemodelan,
#               analisis sensitivitas (09--12), testing, ekspor tabel,
#               kompilasi laporan PDF, hingga ekspor data web (13).
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-15
# =============================================================================

message("=== [1/14] Menjalankan Impor & Validasi Data ===")
source(here::here("scripts", "01_import_and_validate.R"))

message("=== [2/14] Menjalankan Analisis Deskriptif ===")
source(here::here("scripts", "02_descriptive_analysis.R"))

message("=== [3/14] Menjalankan Model Korelasi & Thorndike ===")
source(here::here("scripts", "03_correlation_models.R"))

message("=== [4/14] Menjalankan Pemodelan Faktor (EFA & CFA) ===")
source(here::here("scripts", "04_factor_analysis.R"))

message("=== [5/14] Menjalankan Analisis Saturasi Tahapan ===")
source(here::here("scripts", "05_stage_saturation.R"))

message("=== [6/14] Menjalankan Simulasi Ranking ===")
source(here::here("scripts", "06_ranking_simulation.R"))

message("=== [7/14] Menghasilkan Codebook Otomatis ===")
source(here::here("scripts", "07_generate_codebook.R"))

# Analisis sensitivitas (09--12) menambah beban simulasi (bootstrap, copula,
# kalibrasi bersarang) sehingga menjalankannya memakan waktu beberapa menit.
# Urutannya tidak boleh diubah: 12 membaca keluaran 11 (nonnorm_fit_marginal.csv),
# dan keempatnya harus selesai sebelum ekspor tabel LaTeX (08) agar tabel
# sensitivitas ikut dibuat. Skrip-skrip ini mengubah options(width) untuk
# mencetak ringkasan konsol, jadi pengaturan sesi disimpan lalu dipulihkan.
opsi_sebelum_sensitivitas <- options()

message("=== [8/14] Menjalankan Sensitivitas Perlakuan LBI (EFA 3-faktor, CFA, Case III) ===")
source(here::here("scripts", "09_sensitivity_lbi.R"))

message("=== [9/14] Menjalankan Bootstrap dan Sensitivitas SD Nasional ===")
source(here::here("scripts", "10_bootstrap_uncertainty.R"))

message("=== [10/14] Menjalankan Diagnostik Non-Normalitas dan Sensitivitas Persentil ===")
source(here::here("scripts", "11_nonnormality_sensitivity.R"))

message("=== [11/14] Menjalankan Skenario SLODR ===")
source(here::here("scripts", "12_slodr_sensitivity.R"))

options(opsi_sebelum_sensitivitas)

# Suite pengujian WAJIB dijalankan di sini — sebelum ekspor tabel LaTeX,
# bukan sesudahnya. Kalau ada test yang gagal, pipeline berhenti (testthat
# melempar error) sebelum tabel yang berpotensi salah sempat ditulis ke
# output/tables/*.tex dan ter-input ke naskah paper.
message("=== [12/14] Menjalankan Suite Pengujian (testthat) ===")
testthat::test_dir(here::here("tests", "testthat"))

message("=== [13/14] Menghasilkan Tabel Publikasi LaTeX & Laporan PDF ===")
source(here::here("scripts", "08_export_paper_tables.R"))

# -----------------------------------------------------------------------------
# Audit trail: simpan sessionInfo() ke file log sebelum render() dipanggil.
# Jika rmarkdown::render() gagal, jejak environment run tetap tersimpan
# dan bisa diperiksa tanpa membuka PDF.
# -----------------------------------------------------------------------------
log_path <- here::here(
  "output", "reports", "session_info_audit.txt"
)
dir.create(here::here("output", "reports"), showWarnings = FALSE)
writeLines(
  c(
    paste0("Run timestamp : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    capture.output(sessionInfo())
  ),
  con = log_path
)
message("Session info disimpan ke: ", log_path)

rmarkdown::render(
  input       = here::here("reports", "exact_computational_report.Rmd"),
  output_dir  = here::here("output", "reports"),
  output_file = "exact_computational_report.pdf"
)

# Ekspor data web dijalankan paling akhir dan di lingkungan terpisah
# (new.env) agar variabel perantaranya tidak mencemari sesi global. Skrip ini
# menulis ulang web/data.js; DOI dan URL repositori yang tampil di web diatur
# lewat WEB_DOI dan WEB_REPO di bagian atas skrip 13.
message("=== [14/14] Mengekspor Data Web Kalkulator (web/data.js) ===")
source(
  here::here("scripts", "13_export_web_data.R"),
  local = new.env()
)

message(
  "\n Seluruh pipeline replikasi (14 tahap) selesai dieksekusi dengan sukses!"
)