# =============================================================================
# Judul       : 07 Generator Codebook Otomatis
# Deskripsi   : Membuat file docs/codebook.md terstandarisasi dari dictionary
#               dan data bersih aktual. Menyertakan bagian "Riwayat Versi"
#               yang diperbarui secara kumulatif setiap pipeline dijalankan.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-14
# =============================================================================

library(dplyr)
library(readr)
library(purrr)
library(glue)
library(here)

df   <- readr::read_csv(
  here::here("data", "processed", "utbk_clean.csv"),
  col_types      = readr::cols(),
  show_col_types = FALSE
)
dict <- readr::read_csv(
  here::here("data", "metadata", "dictionary.csv"),
  col_types      = readr::cols(.default = "c"),
  show_col_types = FALSE
)

get_stat_line <- function(x) {
  if (is.numeric(x)) {
    glue::glue(
      "Mean = {sprintf('%.3f', mean(x, na.rm = TRUE))}; ",
      "Median = {sprintf('%.3f', median(x, na.rm = TRUE))}; ",
      "SD = {sprintf('%.3f', sd(x, na.rm = TRUE))}; ",
      "Min = {sprintf('%.3f', min(x, na.rm = TRUE))}; ",
      "Max = {sprintf('%.3f', max(x, na.rm = TRUE))}"
    )
  } else {
    "Non-numeric variable"
  }
}

# Menghitung baris "Missing" dari data sesungguhnya (n dan %), bukan nilai
# tetap — persentase harus ikut berubah begitu data baru memuat missing value.
missing_line <- function(x) {
  n_na <- sum(is.na(x))
  pct  <- round(mean(is.na(x)) * 100, 2)
  glue::glue("`NA` (n = {n_na}, {pct}%)")
}

# Tampilkan '-' untuk nilai NA pada field dictionary opsional (unit, sumber,
# catatan) — dipakai berulang di render_block() agar tidak duplikasi ifelse().
na_dash <- function(x) {
  ifelse(is.na(x), "-", x)
}

render_block <- function(v, d_row, data) {
  x <- data[[v]]
  glue::glue(
    "### `{v}`\n\n",
    "| Atribut | Nilai |\n|---|---|\n",
    "| **Label** | {d_row$var_label} |\n",
    "| **Tipe R** | `{class(x)[1]}` |\n",
    "| **Skala ukur** | {d_row$measurement_scale} |\n",
    "| **Satuan** | {na_dash(d_row$unit)} |\n",
    "| **Missing** | {missing_line(x)} |\n",
    "| **Sumber** | {na_dash(d_row$source_question)} |\n",
    "| **Statistik Sampel** | {get_stat_line(x)} |\n",
    "| **Catatan** | {na_dash(d_row$notes)} |\n\n"
  )
}

header <- glue::glue(
  "# Codebook: Dataset Replikasi UTBK-SNBT 2026\n\n",
  "| Field | Isi |\n|---|---|\n",
  "| **Nama dataset** | utbk_clean.csv |\n",
  "| **Jumlah observasi** | {nrow(df)} |\n",
  "| **Jumlah variabel** | {ncol(df)} |\n",
  "| **Tanggal dihasilkan** | {Sys.Date()} |\n",
  "| **Versi R** | {R.version.string} |\n\n",
  "---\n\n## Kamus Variabel\n\n"
)

body <- purrr::map_chr(names(df), function(v) {
  d_row <- dict |> dplyr::filter(var_name == v)
  if (nrow(d_row) == 0) {
    d_row <- tibble::tibble(
      var_label = "-", measurement_scale = "-", unit = "-",
      source_question = "-", notes = "-"
    )
  }
  render_block(v, d_row, df)
})

# =============================================================================
# Bagian 15: Riwayat Versi (Standar Codebook §15)
# Setiap run pipeline menambahkan satu baris entri ke file CSV history
# (data/metadata/codebook_version_history.csv) dan menyertakan seluruh
# tabel riwayat itu ke codebook.md — sehingga audit trail terjaga kumulatif
# tanpa menimpa catatan run sebelumnya.
# =============================================================================

history_path <- here::here(
  "data", "metadata", "codebook_version_history.csv"
)

# Baca history yang sudah ada, atau buat kerangka kosong jika belum ada.
if (file.exists(history_path)) {
  version_history <- readr::read_csv(
    history_path,
    col_types      = readr::cols(.default = "c"),
    show_col_types = FALSE
  )
} else {
  version_history <- tibble::tibble(
    versi           = character(),
    tanggal         = character(),
    r_version       = character(),
    n_observasi     = character(),
    n_variabel      = character(),
    keterangan      = character()
  )
}

# Tentukan nomor versi berikutnya (1.0, 1.1, 1.2, dst.)
next_minor <- nrow(version_history)
new_version <- glue::glue("1.{next_minor}")

new_entry <- tibble::tibble(
  versi       = as.character(new_version),
  tanggal     = as.character(Sys.Date()),
  r_version   = R.version.string,
  n_observasi = as.character(nrow(df)),
  n_variabel  = as.character(ncol(df)),
  keterangan  = "Pipeline otomatis (run_all.R)"
)

version_history <- dplyr::bind_rows(version_history, new_entry)

readr::write_csv(version_history, history_path)

# Render tabel riwayat sebagai Markdown untuk disertakan di codebook.
version_rows <- purrr::pmap_chr(
  version_history,
  function(versi, tanggal, r_version, n_observasi, n_variabel,
           keterangan) {
    glue::glue(
      "| {versi} | {tanggal} | {r_version} ",
      "| {n_observasi} | {n_variabel} | {keterangan} |"
    )
  }
)

version_section <- paste0(
  "\n\n---\n\n## Riwayat Versi\n\n",
  "| Versi | Tanggal | Versi R | N Observasi | N Variabel ",
  "| Keterangan |\n",
  "|---|---|---|---|---|---|\n",
  paste(version_rows, collapse = "\n"),
  "\n"
)

dir.create(here::here("docs"), showWarnings = FALSE)
writeLines(
  c(header, body, version_section),
  here::here("docs", "codebook.md")
)
message("Codebook berhasil diperbarui di docs/codebook.md")