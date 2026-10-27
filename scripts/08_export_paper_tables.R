# =============================================================================
# Judul       : 08 Ekspor Tabel Naskah Paper LaTeX
# Deskripsi   : Membuat seluruh berkas .tex tabel publikasi siap \input ke
#               naskah paper: matriks korelasi (mentah & terkoreksi),
#               komparasi fit indeks CFA, muatan faktor, statistik
#               deskriptif komparasi sampel-vs-nasional, saturasi korelasi
#               per tahap pengumpulan data, diagnostik pelanggaran asumsi
#               korelasi seragam (Case III), dan matriks translasi
#               persentil standar (per subtes & komposit lintas skenario
#               korelasi, hasil redesign scripts/06 — lihat header skrip
#               tsb). Teks yang berasal dari data (label subtes/faktor/
#               skenario) di-escape otomatis oleh utils_tables.R; N dibaca
#               dari data, bukan ditulis manual.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-26
# Input       : data/processed/utbk_clean.csv
#               output/tables/matriks_korelasi_raw.rds
#               output/tables/matriks_korelasi_terkoreksi.rds
#               output/tables/cfa_model_fit_comparison.csv
#               output/tables/cfa_factor_loadings.csv
#               output/tables/cfa_latent_correlation.csv
#               output/tables/tabel_deskriptif_komparasi.csv
#               output/tables/tabel_saturasi_tahap.csv
#               output/tables/tabel_diagnostik_seleksi.csv
#               output/tables/tabel_ranking_subtes.csv
#               output/tables/tabel_ranking_komposit.csv
# Output      : output/tables/tab_korelasi.tex
#               output/tables/tab_korelasi_terkoreksi.tex
#               output/tables/tab_cfa_fit.tex
#               output/tables/tab_factor_loadings.tex
#               output/tables/tab_deskriptif.tex
#               output/tables/tab_saturasi_tahap.tex
#               output/tables/tab_diagnostik.tex
#               output/tables/tab_ranking_subtes.tex
#               output/tables/tab_ranking_komposit.tex
#               output/tables/tab_nonnormal_diag.tex (dari skrip 11)
#               output/tables/tab_nonnormal_sim.tex (dari skrip 11)
#               output/tables/tab_slodr_ekor.tex (dari skrip 12)
#               output/tables/tab_slodr_case3.tex (dari skrip 12)
# =============================================================================

# Muat Package ----------------------------------------------------------------
library(readr)
library(here)
library(dplyr)
library(tidyr)

# Muat Modul Utilitas ---------------------------------------------------------
source(here::here("R", "constants.R"))
source(here::here("R", "utils_stats.R"))
source(here::here("R", "utils_tables.R"))

# Konstanta Path --------------------------------------------------------------
PATH_DATA_CLEAN   <- here::here("data", "processed", "utbk_clean.csv")
PATH_RAW_CORR     <- here::here("output", "tables", "matriks_korelasi_raw.rds")
PATH_CORR_CORR    <- here::here(
  "output", "tables", "matriks_korelasi_terkoreksi.rds"
)
PATH_CFA_FIT      <- here::here(
  "output", "tables", "cfa_model_fit_comparison.csv"
)
PATH_CFA_LOADINGS <- here::here(
  "output", "tables", "cfa_factor_loadings.csv"
)
PATH_CFA_PHI      <- here::here(
  "output", "tables", "cfa_latent_correlation.csv"
)
PATH_DESKRIPTIF   <- here::here(
  "output", "tables", "tabel_deskriptif_komparasi.csv"
)
PATH_SATURASI     <- here::here(
  "output", "tables", "tabel_saturasi_tahap.csv"
)
PATH_DIAGNOSTIK   <- here::here(
  "output", "tables", "tabel_diagnostik_seleksi.csv"
)
PATH_RANK_SUBTES  <- here::here(
  "output", "tables", "tabel_ranking_subtes.csv"
)
PATH_RANK_KOMPOSIT <- here::here(
  "output", "tables", "tabel_ranking_komposit.csv"
)
DIR_OUT_TABLES    <- here::here("output", "tables")

# Kamus Label Subtes dan Faktor Laten -----------------------------------------
LABEL_MAP_SUBTEST <- c(
  "pu"          = "PU",
  "ppu"         = "PPU",
  "pbm"         = "PBM",
  "pk"          = "PK",
  "lbi_saintek" = "LBI Saintek",
  "lbi_soshum"  = "LBI Soshum",
  "lbe"         = "LBE",
  "pm"          = "PM"
)

# Karakter "&" pada label ini aman: escape_latex() mengubahnya menjadi "\&"
# saat tabel dibentuk, sehingga tidak menambah kolom tabular.
LABEL_MAP_FACTOR <- c(
  "literacy" = "Literasi",
  "numeracy" = "Numerasi"
)

# Fungsi Bantu ----------------------------------------------------------------

#' Petakan Kode ke Label Tampilan
#'
#' Berbeda dengan dplyr::recode(), kode yang tidak ada di kamus dianggap
#' kesalahan (bukan dibiarkan diam-diam) agar label tabel paper tidak salah.
#'
#' @param x Character vector kode (mis. nama kolom atau subtes).
#' @param label_map Named character vector: nama = kode, nilai = label.
#'
#' @return Character vector label tanpa nama.
#' @examples
#' map_labels(c("pu", "pm"), LABEL_MAP_SUBTEST)
map_labels <- function(x, label_map) {
  hasil <- unname(label_map[as.character(x)])
  if (anyNA(hasil)) {
    stop(
      "Kode tanpa label di kamus: ",
      paste(unique(as.character(x)[is.na(hasil)]), collapse = ", "),
      ". Tambahkan ke LABEL_MAP_* di skrip ini.",
      call. = FALSE
    )
  }
  hasil
}

# format_int_id() dipakai dari R/utils_tables.R (satu sumber kebenaran utk
# seluruh formatter tabel LaTeX), tidak lagi didefinisikan lokal di sini.

# Ukuran Sampel (dibaca dari data agar caption tidak basi) ---------------------
data_clean <- readr::read_csv(
  PATH_DATA_CLEAN,
  col_types      = readr::cols(),
  show_col_types = FALSE
)
n_sample_str <- format_int_id(nrow(data_clean))

# 1. Ekspor Tabel Matriks Korelasi (Mentah & Terkoreksi) -----------------------
mat_raw  <- readRDS(PATH_RAW_CORR)
mat_corr <- readRDS(PATH_CORR_CORR)

colnames(mat_raw)  <- map_labels(colnames(mat_raw), LABEL_MAP_SUBTEST)
rownames(mat_raw)  <- map_labels(rownames(mat_raw), LABEL_MAP_SUBTEST)
colnames(mat_corr) <- map_labels(colnames(mat_corr), LABEL_MAP_SUBTEST)
rownames(mat_corr) <- map_labels(rownames(mat_corr), LABEL_MAP_SUBTEST)

r_raw_str <- format(
  round(compute_off_diagonal_mean(mat_raw), 3),
  decimal.mark = ",",
  nsmall       = 3
)

r_corr_str <- format(
  round(compute_off_diagonal_mean(mat_corr), 3),
  decimal.mark = ",",
  nsmall       = 3
)

tex_raw <- format_latex_correlation_table(
  mat         = mat_raw,
  label_tabel = "tab:korelasi",
  caption     = sprintf(
    "Matriks Korelasi Pearson Antarsubtes (Data Sampel, $N=%s$)",
    n_sample_str
  ),
  note        = sprintf(
    "Rerata korelasi \\textit{off-diagonal} (%d pasangan unik) $\\approx %s$.",
    N_UNIQUE_PAIRS, r_raw_str
  )
)

tex_corr <- format_latex_correlation_table(
  mat         = mat_corr,
  label_tabel = "tab:korelasi_terkoreksi",
  caption     = paste0(
    "Matriks Korelasi Antarsubtes Terkoreksi \\textit{Restriction of Range} ",
    "(Thorndike Case III)"
  ),
  note        = sprintf(
    "Rerata korelasi \\textit{off-diagonal} terkoreksi $\\approx %s$.",
    r_corr_str
  )
)

dir.create(DIR_OUT_TABLES, recursive = TRUE, showWarnings = FALSE)
writeLines(tex_raw, file.path(DIR_OUT_TABLES, "tab_korelasi.tex"))
writeLines(tex_corr, file.path(DIR_OUT_TABLES, "tab_korelasi_terkoreksi.tex"))

# 2. Ekspor Tabel Komparasi Fit Indeks CFA ------------------------------------
cfa_fit_df <- readr::read_csv(
  PATH_CFA_FIT,
  col_types      = readr::cols(),
  show_col_types = FALSE
)

# Nama berkas dibungkus \texttt{} dengan underscore ter-escape; underscore
# mentah di mode teks LaTeX menyebabkan error "Missing $ inserted".
note_cfa_fit <- paste0(
  "$N = ", n_sample_str, "$. ",
  "Model 1 memuat \\textit{g-factor} tunggal, sedangkan Model 2 memuat dua faktor berkorelasi (Literasi dan Numerasi). ",
  "Estimator yang dipakai adalah \\textit{robust maximum likelihood} (MLM). ",
  "$\\chi^2_{\\text{SB}}$ adalah \\textit{scaled chi-square} Satorra--Bentler; ",
  "CFI = \\textit{Comparative Fit Index}; TLI = \\textit{Tucker--Lewis Index}; ",
  "RMSEA = \\textit{Root Mean Square Error of Approximation}; ",
  "SRMR = \\textit{Standardized Root Mean Square Residual}; ",
  "AIC = \\textit{Akaike Information Criterion}. ",
  "Signifikansi $\\Delta\\chi^2$: $^{*}p < 0,05$; $^{**}p < 0,01$; ",
  "$^{***}p < 0,001$ (dihitung dari nilai $p$ eksak",
  ")."
)

tex_cfa_fit <- format_latex_cfa_fit_table(
  df_fit      = cfa_fit_df,
  label_tabel = "tab:cfa_fit",
  caption     = paste0(
    "Perbandingan Indeks Kecocokan Model 1 (\\textit{g-factor} Tunggal) dan ",
    "Model 2 (Dua Faktor Berkorelasi) pada CFA"
  ),
  note        = note_cfa_fit
)

writeLines(tex_cfa_fit, file.path(DIR_OUT_TABLES, "tab_cfa_fit.tex"))

# 3. Ekspor Tabel Muatan Faktor Terstandarisasi -------------------------------
# A. Ambil tabel muatan faktor untuk isi tabel
cfa_loadings_df <- readr::read_csv(
  PATH_CFA_LOADINGS,
  col_types      = readr::cols(),
  show_col_types = FALSE
)
cfa_loadings_df$latent_factor <- map_labels(
  cfa_loadings_df$latent_factor, LABEL_MAP_FACTOR
)
cfa_loadings_df$subtest <- map_labels(
  cfa_loadings_df$subtest, LABEL_MAP_SUBTEST
)

# B. Ambil nilai phi yang sudah dihitung & disimpan di scripts/04 — hindari
#    fit ulang model CFA (mahal secara komputasi & duplikasi logika)
phi_table <- readr::read_csv(
  PATH_CFA_PHI,
  col_types      = readr::cols(),
  show_col_types = FALSE
)
phi_val    <- phi_table$phi_estimate
phi_pvalue <- phi_table$phi_pvalue

phi_str <- format(round(phi_val, 3), decimal.mark = ",", nsmall = 3)

note_loadings <- sprintf(
  paste0(
    "Bintang pada $\\lambda$ menandakan signifikansi tiap muatan: ",
    "$^{*}p < 0,05$; $^{**}p < 0,01$; $^{***}p < 0,001$. ",
    "$SE$ = kesalahan standar; $z$ = nilai uji rasio kritis; ",
    "$R^2$ ($h^2$) = proporsi varians yang dijelaskan oleh faktor laten ",
    "terkait. ",
    "Korelasi antarfaktor laten adalah $\\phi = %s$%s."
  ),
  phi_str,
  p_to_stars(phi_pvalue)
)

# C. Bentuk dan simpan tabel LaTeX
tex_loadings <- format_latex_factor_loadings_table(
  df_loadings = cfa_loadings_df,
  label_tabel = "tab:factor_loadings",
  caption     = paste0(
    "Muatan Faktor Terstandarisasi dan Komunalitas Subtes pada Model ",
    "2 (CFA)"
  ),
  note        = note_loadings
)

writeLines(tex_loadings, file.path(DIR_OUT_TABLES, "tab_factor_loadings.tex"))

# 4. Ekspor Tabel Deskriptif Komparasi (Sampel vs Nasional) -------------------
deskriptif_df <- readr::read_csv(
  PATH_DESKRIPTIF,
  col_types      = readr::cols(),
  show_col_types = FALSE
)
deskriptif_df$subject_name <- map_labels(
  deskriptif_df$subject_name, LABEL_MAP_SUBTEST
)

tex_deskriptif <- format_latex_descriptive_table(
  df_desc     = deskriptif_df,
  label_tabel = "tab:deskriptif",
  caption     = paste0(
    "Perbandingan Statistik Deskriptif Sampel ($N=", n_sample_str,
    "$) dan Parameter Nasional Tidak Resmi per Subtes"
  ),
  note        = paste0(
    "$\\Delta M = M_{\\text{sampel}} - M_{\\text{nasional}}$; ",
    "$\\Delta SD = SD_{\\text{sampel}} - SD_{\\text{nasional}}$. ",
    "Parameter nasional tidak resmi bersumber dari tabel internal Panitia Seleksi Nasional Penerimaan Mahasiswa Baru (SNPMB) yang beredar di media sosial dan ",
    "belum terverifikasi secara resmi (triangulasi parsial pada nilai maksimum PK dan PM; lihat Bagian Metode)."
  )
)

writeLines(tex_deskriptif, file.path(DIR_OUT_TABLES, "tab_deskriptif.tex"))

# 5. Ekspor Tabel Saturasi Korelasi per Tahap ---------------------------------
saturasi_df <- readr::read_csv(
  PATH_SATURASI,
  col_types      = readr::cols(),
  show_col_types = FALSE
)

tex_saturasi <- format_latex_stage_saturation_table(
  df_stage    = saturasi_df,
  label_tabel = "tab:saturasi_tahap",
  caption     = paste0(
    "Rerata Korelasi \\textit{Off-Diagonal} Antarsubtes secara Kumulatif ",
    "Seiring Bertambahnya Tahap Pengumpulan Data"
  ),
  note        = paste0(
    "$N$ kumulatif adalah jumlah entri dari Tahap 1 sampai tahap terkait ",
    "(tahap disusun berurutan sesuai kronologi pengumpulan). ",
    "Nilai $\\bar{r}$ \\textit{off-diagonal}, yaitu rerata korelasi di luar diagonal matriks korelasi \\textit{raw sample}, dihitung ulang dari entri ",
    "kumulatif itu."
  )
)

writeLines(tex_saturasi, file.path(DIR_OUT_TABLES, "tab_saturasi_tahap.tex"))

# 6. Ekspor Tabel Diagnostik Pelanggaran Asumsi Case III ----------------------
diagnostik_df <- readr::read_csv(
  PATH_DIAGNOSTIK,
  col_types      = readr::cols(),
  show_col_types = FALSE
)
diagnostik_df$subtest <- map_labels(diagnostik_df$subtest, LABEL_MAP_SUBTEST)

tex_diagnostik <- format_latex_diagnostic_table(
  df_diag     = diagnostik_df,
  label_tabel = "tab:diagnostik",
  caption     = paste0(
    "Diagnostik Deviasi $SD$ Tersirat dari Korelasi Item-Total terhadap ",
    "$SD$ Nasional per Subtes"
  ),
  note        = paste0(
    "Rasio = $SD$ tersirat populasi $\\div$ $SD$ nasional. Rasio yang ",
    "menyimpang jauh dari 1 mengindikasikan pelanggaran asumsi korelasi ",
    "seragam yang disyaratkan koreksi Case III untuk subtes itu."
  )
)

writeLines(tex_diagnostik, file.path(DIR_OUT_TABLES, "tab_diagnostik.tex"))

# 7. Ekspor Tabel Matriks Translasi Persentil Standar (Redesign K4) -----------
# Kedua tabel di bagian ini berasal dari scripts/06_ranking_simulation.R
# (format long, satu baris per titik-benchmark x kolom-nilai) dan perlu
# dipivot ke format lebar (satu baris per titik-benchmark) sebelum dikirim
# ke format_latex_benchmark_table() — lihat dokumentasi fungsi tsb di
# R/utils_tables.R.

# 7a. Per Subtes (SD Nasional Univariat, Tanpa Skenario Korelasi)
rank_subtes_df <- readr::read_csv(
  PATH_RANK_SUBTES,
  col_types      = readr::cols(),
  show_col_types = FALSE
)
rank_subtes_df$subtest <- map_labels(rank_subtes_df$subtest, LABEL_MAP_SUBTEST)

rank_subtes_wide <- rank_subtes_df |>
  dplyr::select(
    benchmark_label, top_pct, rank_nasional, subtest, score
  ) |>
  tidyr::pivot_wider(names_from = subtest, values_from = score) |>
  dplyr::arrange(match(
    benchmark_label,
    c(
      "M - 3 SD", "M - 2 SD", "M - 1 SD", "M",
      "M + 1 SD", "M + 2 SD", "M + 3 SD"
    )
  ))

tex_rank_subtes <- format_latex_benchmark_table(
  df_wide     = rank_subtes_wide,
  label_tabel = "tab:ranking_subtes",
  caption     = paste0(
    "Matriks Translasi Persentil Standar ke Skor Mentah per Subtes ",
    "($M = 500$, $SD$ Nasional Tidak Resmi per Subtes)"
  ),
  note        = paste0(
    "Persentil (\\textit{top \\%}) identik lintas subtes pada baris yang ",
    "sama secara desain. Hanya skor mentah yang berbeda antarsubtes karena ",
    "perbedaan $SD$ nasional. Peringkat nasional dihitung dari jumlah ",
    "peserta hadir, $N_{\\text{nasional}} = ", format_int_id(N_POPULATION), "$."
  )
)

writeLines(
  tex_rank_subtes, file.path(DIR_OUT_TABLES, "tab_ranking_subtes.tex")
)

# 7b. Komposit Lintas Skenario Korelasi
rank_komposit_df <- readr::read_csv(
  PATH_RANK_KOMPOSIT,
  col_types      = readr::cols(),
  show_col_types = FALSE
)

rank_komposit_wide <- rank_komposit_df |>
  dplyr::select(
    benchmark_label, top_pct, rank_nasional, scenario, score
  ) |>
  tidyr::pivot_wider(names_from = scenario, values_from = score) |>
  dplyr::arrange(match(
    benchmark_label,
    c(
      "M - 3 SD", "M - 2 SD", "M - 1 SD", "M",
      "M + 1 SD", "M + 2 SD", "M + 3 SD"
    )
  ))

tex_rank_komposit <- format_latex_benchmark_table(
  df_wide     = rank_komposit_wide,
  label_tabel = "tab:ranking_komposit",
  caption     = paste0(
    "Matriks Translasi Persentil Standar ke Skor Komposit Lintas ",
    "Skenario Korelasi Antarsubtes ($M = 500$)"
  ),
  note        = paste0(
    "Persentil (\\textit{top \\%}) identik lintas skenario pada baris ",
    "yang sama secara desain. Hanya skor komposit yang berbeda antarskenario ",
    "karena perbedaan $SD$ skor komposit teoretis di bawah tiap asumsi ",
    "korelasi. Thorndike Case III adalah estimasi ",
    "terkoreksi \\textit{restriction of range} (lihat Metode), sedangkan skenario $r$ lain ",
    "adalah titik pembanding hipotetis dengan asumsi korelasi rerata ",
    "seragam antarpasangan subtes. Peringkat nasional dihitung dari ",
    "jumlah peserta hadir, $N_{\\text{nasional}} = ", format_int_id(N_POPULATION), "$."
  )
)

writeLines(
  tex_rank_komposit, file.path(DIR_OUT_TABLES, "tab_ranking_komposit.tex")
)

# 8. Tabel Sensitivitas LBI (hasil scripts/09_sensitivity_lbi.R) --------------
# Dilewati dengan pesan bila skrip 09 belum pernah dijalankan (mis. 08 dijalankan
# sendiri di luar run_all.R), supaya tidak berhenti karena file yang belum ada.
PATH_SENS_CFA   <- file.path(DIR_OUT_TABLES, "sens_cfa_fit.csv")
PATH_SENS_RING  <- file.path(DIR_OUT_TABLES, "sens_cfa_ringkasan.csv")
PATH_SENS_THORN <- file.path(DIR_OUT_TABLES, "sens_thorndike_lbi.csv")

if (all(file.exists(PATH_SENS_CFA, PATH_SENS_RING, PATH_SENS_THORN))) {
  sens_cfa <- readr::read_csv(
    PATH_SENS_CFA, col_types = readr::cols(), show_col_types = FALSE
  )
  sens_ring <- readr::read_csv(
    PATH_SENS_RING, col_types = readr::cols(), show_col_types = FALSE
  )
  sens_thorn <- readr::read_csv(
    PATH_SENS_THORN, col_types = readr::cols(), show_col_types = FALSE
  )
  stopifnot(
    "sens_cfa harus 2 baris (model utama, model + kovarians residual)" =
      nrow(sens_cfa) == 2L && nrow(sens_ring) == 2L
  )
  
  # 8a. Tabel CFA: model utama vs + kovarians residual antar-LBI
  nama_model_sens <- c("Utama: 2-faktor", "2-faktor + kovarians residual LBI")
  
  baris_sens_cfa <- vapply(
    seq_len(nrow(sens_cfa)),
    function(i) {
      teks_delta <- if (is.na(sens_cfa$delta_chisq[i])) {
        "-"
      } else {
        paste0(
          format_number_id(sens_cfa$delta_chisq[i], 1L),
          " (", as.integer(sens_cfa$delta_df[i]), ")",
          p_to_stars(sens_cfa$delta_p[i])
        )
      }
      sprintf(
        "        %s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
        nama_model_sens[i],
        format_number_id(sens_cfa$chisq_scaled[i], 1L),
        as.character(as.integer(sens_cfa$df[i])),
        format_number_id(sens_cfa$cfi_robust[i], 3L),
        format_number_id(sens_cfa$rmsea_robust[i], 3L),
        format_number_id(sens_cfa$srmr[i], 3L),
        format_number_id(sens_cfa$aic[i], 0L),
        format_number_id(sens_cfa$bic[i], 0L),
        format_number_id(sens_ring$phi_literasi_numerasi[i], 3L),
        format_number_id(sens_ring$korelasi_residual_lbi[i], 3L),
        teks_delta
      )
    },
    character(1)
  )
  
  tex_sens_cfa <- paste(
    c(
      build_latex_table_header(
        "tab:sens_cfa",
        paste0(
          "Analisis Sensitivitas CFA Model Dua Faktor dengan dan tanpa ",
          "Kovarians Residual antara LBI Saintek dan LBI Soshum"
        )
      ),
      "    \\resizebox{\\textwidth}{!}{%",
      "    \\begin{tabular}{lcccccccccc}",
      "        \\toprule",
      paste0(
        "        Model & $\\chi^2_{\\text{SB}}$ & $df$ & CFI & RMSEA & SRMR & ",
        "AIC & BIC & $\\phi$ & $r_{\\text{resid}}$ & ",
        "$\\Delta\\chi^2$ ($df$) \\\\"
      ),
      "        \\midrule",
      baris_sens_cfa,
      build_latex_table_footer(
        note = paste0(
          "$N = ", n_sample_str, "$. Estimator yang dipakai adalah \\textit{robust maximum ",
          "likelihood} (MLM). Model utama identik dengan Model 2 pada ",
          "Tabel~\\ref{tab:cfa_fit}. $\\phi$ = korelasi antarfaktor laten ",
          "terstandar; $r_{\\text{resid}}$ = korelasi residual terstandar ",
          "antara LBI Saintek dan LBI Soshum. CFI = \\textit{Comparative Fit Index}; RMSEA = \\textit{Root Mean Square Error of Approximation}; SRMR = \\textit{Standardized Root Mean Square Residual}; AIC = \\textit{Akaike Information Criterion}; BIC = \\textit{Bayesian Information Criterion}. Signifikansi $\\Delta\\chi^2$: ",
          "$^{*}p < 0,05$; $^{**}p < 0,01$; $^{***}p < 0,001$ (dihitung dari nilai $p$ eksak",
          ")."
        ),
        resized = TRUE
      )
    ),
    collapse = "\n"
  )
  writeLines(tex_sens_cfa, file.path(DIR_OUT_TABLES, "tab_sensitivitas_cfa.tex"))
  
  # 8b. Tabel Thorndike Case III: 8 vs 7 vs 6 subtes
  baris_sens_thorn <- vapply(
    seq_len(nrow(sens_thorn)),
    function(i) {
      sprintf(
        "        %s & %s & %s & %s & %s & %s & %s & %s \\\\",
        escape_latex(sens_thorn$skenario[i]),
        as.character(as.integer(sens_thorn$k[i])),
        format_number_id(sens_thorn$r_raw_rerata[i], 3L),
        format_number_id(sens_thorn$s_sample_z[i], 2L),
        format_number_id(sens_thorn$s_population_z[i], 2L),
        format_number_id(sens_thorn$lambda_sq[i], 3L),
        format_number_id(sens_thorn$r_terkoreksi_rerata[i], 3L),
        paste0(
          format_number_id(sens_thorn$rasio_sd_min[i], 3L), "--",
          format_number_id(sens_thorn$rasio_sd_maks[i], 3L)
        )
      )
    },
    character(1)
  )
  
  tex_sens_thorn <- paste(
    c(
      build_latex_table_header(
        "tab:sens_thorndike",
        paste0(
          "Analisis Sensitivitas Thorndike Case III terhadap Perlakuan LBI ",
          "pada Skor Komposit"
        )
      ),
      "    \\resizebox{\\textwidth}{!}{%",
      "    \\begin{tabular}{lccccccc}",
      "        \\toprule",
      paste0(
        "        Skenario & $k$ & $\\bar{r}$ \\textit{Raw Sample} & $s_Z$ Sampel & ",
        "$S_Z$ Populasi & $\\lambda^2$ & $\\bar{R}$ Terkoreksi & ",
        "Rasio $SD$ (min--maks) \\\\"
      ),
      "        \\midrule",
      baris_sens_thorn,
      build_latex_table_footer(
        note = paste0(
          "Skor komposit adalah rerata $k$ subtes yang dipakai, dan tiap subtes memakai ",
          "$SD$ nasional tidak resminya sendiri. $\\bar{r}$ adalah rerata korelasi ",
          "\\textit{raw sample}, dan $\\bar{R}$ adalah rerata korelasi terkoreksi ",
          "\\textit{restriction of range}. $\\lambda^2$ adalah multiplikator seleksi ($\\frac{S_Z^2}{s_Z^2} - 1$). Rasio adalah $SD$ tersirat populasi terhadap $SD$ ",
          "nasional tidak resmi (lihat Tabel~\\ref{tab:diagnostik}). Rerata ",
          "kedua skor LBI tidak dipakai karena $SD$ nasionalnya tidak ",
          "tersedia."
        ),
        resized = TRUE
      )
    ),
    collapse = "\n"
  )
  writeLines(
    tex_sens_thorn, file.path(DIR_OUT_TABLES, "tab_sensitivitas_thorndike.tex")
  )
} else {
  message(
    "-> Tabel sensitivitas LBI dilewati: jalankan scripts/09_sensitivity_lbi.R ",
    "terlebih dahulu."
  )
}

# 9. Tabel Bootstrap dan Sensitivitas SD Nasional (hasil scripts/10_...) ------
# Dilewati dengan pesan bila skrip 10 belum pernah dijalankan (mis. 08 dijalankan
# sendiri di luar run_all.R).
PATH_BOOT_RING <- file.path(DIR_OUT_TABLES, "boot_ringkasan.csv")
PATH_SENS_SD   <- file.path(DIR_OUT_TABLES, "sens_sd_nasional.csv")

if (all(file.exists(PATH_BOOT_RING, PATH_SENS_SD))) {
  boot_ring <- readr::read_csv(
    PATH_BOOT_RING, col_types = readr::cols(), show_col_types = FALSE
  )
  sens_sd <- readr::read_csv(
    PATH_SENS_SD, col_types = readr::cols(), show_col_types = FALSE
  )
  stopifnot(
    "boot_ringkasan harus 3 baris (S_Z, lambda^2, R-bar)" =
      nrow(boot_ring) == 3L,
    "sens_sd_nasional harus 6 skenario" = nrow(sens_sd) == 6L
  )
  
  # 9a. Tabel interval bootstrap
  label_boot  <- c("$S_Z$ populasi", "$\\lambda^2$", "$\\bar{R}$ terkoreksi")
  digits_boot <- c(2L, 3L, 3L)
  
  baris_boot <- vapply(
    seq_len(3L),
    function(i) {
      d <- digits_boot[i]
      sprintf(
        "        %s & %s & %s & %s & %s \\\\",
        label_boot[i],
        format_number_id(boot_ring$estimasi[i], d),
        format_number_id(boot_ring$boot_rerata[i], d),
        format_number_id(boot_ring$boot_se[i], d),
        paste0(
          "[", format_number_id(boot_ring$ci_bawah[i], d), "; ",
          format_number_id(boot_ring$ci_atas[i], d), "]"
        )
      )
    },
    character(1)
  )
  
  tex_boot <- paste(
    c(
      build_latex_table_header(
        "tab:bootstrap",
        paste0(
          "Interval \\textit{Bootstrap} 95\\% untuk Estimasi Utama Thorndike Case III ",
          "(Delapan Subtes)"
        )
      ),
      "    \\begin{tabular}{lcccc}",
      "        \\toprule",
      paste0(
        "        Statistik & Estimasi & Rerata \\textit{Bootstrap} & $SE$ \\textit{Bootstrap} & ",
        "Interval Persentil 95\\% \\\\"
      ),
      "        \\midrule",
      baris_boot,
      build_latex_table_footer(
        note = paste0(
          "Interval dihitung dengan \\textit{bootstrap} nonparametrik (penarikan ulang entri sampel teramati) sebanyak ",
          format_int_id(as.integer(boot_ring$n_boot[1])), " replikasi, ",
          format_int_id(as.integer(boot_ring$n_konvergen[1])),
          " di antaranya konvergen. Interval hanya menangkap ketidakpastian ",
          "pengambilan sampel dan mengandaikan $SD$ nasional serta spesifikasi Case III ",
          "benar (lihat Tabel~\\ref{tab:sens_sd})."
        ),
        resized = FALSE
      )
    ),
    collapse = "\n"
  )
  writeLines(tex_boot, file.path(DIR_OUT_TABLES, "tab_bootstrap.tex"))
  
  # 9b. Tabel sensitivitas terhadap SD nasional
  label_sd <- c(
    "$SD$ nasional tanpa penyesuaian (utama)",
    "Semua $SD$ $\\times$ 0,90",
    "Semua $SD$ $\\times$ 0,95",
    "Semua $SD$ $\\times$ 1,05",
    "Semua $SD$ $\\times$ 1,10",
    "$SD_{\\text{PU}} = 100$ ($SD$ lain tetap)"
  )
  
  baris_sd <- vapply(
    seq_len(nrow(sens_sd)),
    function(i) {
      sprintf(
        "        %s & %s & %s & %s & %s \\\\",
        label_sd[i],
        format_number_id(sens_sd$s_population_z[i], 2L),
        format_number_id(sens_sd$lambda_sq[i], 3L),
        format_number_id(sens_sd$r_terkoreksi_rerata[i], 3L),
        paste0(
          format_number_id(sens_sd$rasio_sd_min[i], 3L), "--",
          format_number_id(sens_sd$rasio_sd_maks[i], 3L)
        )
      )
    },
    character(1)
  )
  
  tex_sens_sd <- paste(
    c(
      build_latex_table_header(
        "tab:sens_sd",
        paste0(
          "Sensitivitas Estimasi Thorndike Case III terhadap Simpangan Baku ",
          "Nasional Tidak Resmi"
        )
      ),
      "    \\resizebox{\\textwidth}{!}{%",
      "    \\begin{tabular}{lcccc}",
      "        \\toprule",
      paste0(
        "        Skenario & $S_Z$ Populasi & $\\lambda^2$ & ",
        "$\\bar{R}$ Terkoreksi & Rasio $SD$ (min--maks) \\\\"
      ),
      "        \\midrule",
      baris_sd,
      build_latex_table_footer(
        note = paste0(
          "Sampel dan korelasi tidak diubah; hanya $SD$ nasional tidak resmi ",
          "yang dikalikan faktor tertentu atau (pada baris terakhir) ",
          "diganti. ",
          "Rasio = $SD$ ",
          "tersirat populasi terhadap $SD$ nasional pada skenario yang sama."
        ),
        resized = TRUE
      )
    ),
    collapse = "\n"
  )
  writeLines(tex_sens_sd, file.path(DIR_OUT_TABLES, "tab_sensitivitas_sd.tex"))
} else {
  message(
    "-> Tabel bootstrap dan sensitivitas SD dilewati: jalankan ",
    "scripts/10_bootstrap_uncertainty.R terlebih dahulu."
  )
}

# 10. Tabel Non-Normalitas (hasil scripts/11_nonnormality_sensitivity.R) ------
# Dilewati dengan pesan bila skrip 11 belum pernah dijalankan (mis. 08 dijalankan
# sendiri di luar run_all.R).
PATH_NN_DIAG  <- file.path(DIR_OUT_TABLES, "nonnorm_diagnostik.csv")
PATH_NN_EKOR  <- file.path(DIR_OUT_TABLES, "nonnorm_sim_ekor.csv")
PATH_NN_SKOR  <- file.path(DIR_OUT_TABLES, "nonnorm_sim_skor.csv")
PATH_NN_RING  <- file.path(DIR_OUT_TABLES, "nonnorm_sim_ringkasan.csv")

if (all(file.exists(PATH_NN_DIAG, PATH_NN_EKOR, PATH_NN_SKOR, PATH_NN_RING))) {
  nn_diag <- readr::read_csv(PATH_NN_DIAG, col_types = readr::cols(),
                             show_col_types = FALSE)
  nn_ekor <- readr::read_csv(PATH_NN_EKOR, col_types = readr::cols(),
                             show_col_types = FALSE)
  nn_skor <- readr::read_csv(PATH_NN_SKOR, col_types = readr::cols(),
                             show_col_types = FALSE)
  nn_ring <- readr::read_csv(PATH_NN_RING, col_types = readr::cols(),
                             show_col_types = FALSE)
  stopifnot(
    "diagnostik harus 8 subtes" = nrow(nn_diag) == N_SUBTESTS,
    "sim_ekor harus 12 baris (2 varian x 6 titik)" = nrow(nn_ekor) == 12L,
    "sim_skor harus 12 baris (2 varian x 6 titik)" = nrow(nn_skor) == 12L,
    "sim_ringkasan harus 2 varian" = nrow(nn_ring) == 2L
  )
  
  # Persentase kecil: hindari tampilan "0,000" untuk nilai yang bukan nol.
  format_pct_kecil <- function(x) {
    vapply(
      x,
      function(v) {
        if (v < 0.0005) "$<0{,}001$" else format_number_id(v, 3L)
      },
      character(1)
    )
  }
  
  # 10a. Tabel diagnostik non-normalitas per subtes
  nn_diag$subtes <- map_labels(nn_diag$subtes, LABEL_MAP_SUBTEST)
  
  baris_nn_diag <- vapply(
    seq_len(nrow(nn_diag)),
    function(i) {
      sprintf(
        "        %s & %s & %s & %s & %s & %s & %s \\\\",
        escape_latex(nn_diag$subtes[i]),
        format_number_id(nn_diag$z_min[i], 2L),
        format_number_id(nn_diag$z_maks[i], 2L),
        format_int_id(as.integer(round(nn_diag$n_diharapkan_di_bawah_min[i]))),
        format_number_id(nn_diag$skew_bowley[i], 2L),
        format_number_id(nn_diag$rerata_minus_median[i], 1L),
        format_number_id(nn_diag$iqr_per_sd[i], 2L)
      )
    },
    character(1)
  )
  
  tex_nn_diag <- paste(
    c(
      build_latex_table_header(
        "tab:nonnormal_diag",
        paste0(
          "Diagnostik Nonnormalitas Parameter Nasional Tidak Resmi per ",
          "Subtes"
        )
      ),
      "    \\resizebox{\\textwidth}{!}{%",
      "    \\begin{tabular}{lcccccc}",
      "        \\toprule",
      paste0(
        "        Subtes & $z$ Minimum & $z$ Maksimum & ",
        "$N$ Diharapkan $<$ Minimum & Bowley & Rerata$-$Median & ",
        "$\\frac{IQR}{SD}$ \\\\"
      ),
      "        \\midrule",
      baris_nn_diag,
      build_latex_table_footer(
        note = paste0(
          "$z = \\frac{\\text{skor} - 500}{SD}$ nasional. Pada distribusi normal ",
          "dengan $N_{\\text{nasional}} = ", format_int_id(N_POPULATION),
          "$, minimum dan maksimum diharapkan sekitar $\\mp ",
          format_number_id(nn_diag$z_ekstrem_normal[1], 2L),
          "$ $SD$ dari rerata; kemiringan Bowley $= 0$; dan $\\frac{IQR}{SD} = 1{,}349$, dengan $IQR$ sebagai rentang antarkuartil. ",
          "``$N$ Diharapkan $<$ Minimum'' = $N_{\\text{nasional}} \\cdot ",
          "\\Phi(z_{\\min})$, yaitu jumlah peserta yang seharusnya berada di ",
          "bawah minimum nasional bila distribusi normal (nilai teramati ",
          "sekitar satu). Skor $M - 3\\,SD$ berada di bawah minimum nasional ",
          "pada seluruh subtes."
        ),
        resized = TRUE
      )
    ),
    collapse = "\n"
  )
  writeLines(tex_nn_diag, file.path(DIR_OUT_TABLES, "tab_nonnormal_diag.tex"))
  
  # 10b. Tabel sensitivitas persentil komposit (normal vs simulasi)
  ekor_a <- nn_ekor[startsWith(nn_ekor$varian, "A."), ]
  ekor_b <- nn_ekor[startsWith(nn_ekor$varian, "B."), ]
  skor_a <- nn_skor[startsWith(nn_skor$varian, "A."), ]
  skor_b <- nn_skor[startsWith(nn_skor$varian, "B."), ]
  stopifnot(
    "tiap varian harus 6 titik" =
      all(c(nrow(ekor_a), nrow(ekor_b), nrow(skor_a), nrow(skor_b)) == 6L),
    "urutan titik harus sama antarvarian" =
      all(ekor_a$k_sd == ekor_b$k_sd, ekor_a$k_sd == skor_a$k_sd,
          ekor_a$k_sd == skor_b$k_sd)
  )
  
  # Peluang ekor: sisi bawah untuk k < 0, sisi atas untuk k > 0.
  ambil_ekor <- function(df, normal) {
    kol_sim    <- ifelse(df$k_sd < 0, df$pct_di_bawah_sim, df$pct_di_atas_sim)
    kol_normal <- ifelse(df$k_sd < 0, df$pct_di_bawah_normal,
                         df$pct_di_atas_normal)
    if (normal) kol_normal else kol_sim
  }
  label_titik <- ifelse(
    ekor_a$k_sd < 0,
    sprintf("$M - %d\\,S_Z$", as.integer(abs(ekor_a$k_sd))),
    sprintf("$M + %d\\,S_Z$", as.integer(ekor_a$k_sd))
  )
  
  baris_nn_sim <- vapply(
    seq_len(nrow(ekor_a)),
    function(i) {
      sprintf(
        "        %s & %s & %s & %s & %s & %s & %s & %s \\\\",
        label_titik[i],
        format_pct_kecil(ambil_ekor(ekor_a, TRUE)[i]),
        format_pct_kecil(ambil_ekor(ekor_a, FALSE)[i]),
        format_pct_kecil(ambil_ekor(ekor_b, FALSE)[i]),
        format_number_id(skor_a$skor_normal[i], 1L),
        format_number_id(skor_a$skor_sim[i], 1L),
        format_number_id(skor_b$skor_sim[i], 1L),
        format_number_id(skor_b$selisih[i], 1L)
      )
    },
    character(1)
  )
  
  tex_nn_sim <- paste(
    c(
      build_latex_table_header(
        "tab:nonnormal_sim",
        paste0(
          "Sensitivitas Persentil dan Skor Komposit terhadap Asumsi Normal dengan ",
          "Marginal Johnson SB dan \\textit{Copula} Gaussian"
        )
      ),
      "    \\resizebox{\\textwidth}{!}{%",
      "    \\begin{tabular}{lccccccc}",
      "        \\toprule",
      paste0(
        "        & \\multicolumn{3}{c}{Peluang Ekor (\\%)} & ",
        "\\multicolumn{4}{c}{Skor Komposit pada Persentil Normal} \\\\"
      ),
      "        \\cmidrule(lr){2-4} \\cmidrule(lr){5-8}",
      paste0(
        "        Titik & Normal & Simulasi A & Simulasi B & Normal & ",
        "Simulasi A & Simulasi B & Selisih B \\\\"
      ),
      "        \\midrule",
      baris_nn_sim,
      build_latex_table_footer(
        note = paste0(
          "Skor komposit adalah rerata delapan subtes, dengan ",
          format_int_id(as.integer(nn_ring$n_sim[1])),
          " peserta virtual. Marginal tiap subtes dihampiri distribusi ",
          "Johnson SB yang dicocokkan ke kuantil, rerata, dan $SD$ nasional ",
          "tidak resmi, dan dependensi dimodelkan dengan \\textit{copula} Gaussian ",
          "berkorelasi seragam. Simulasi A memakai korelasi laten $= ",
          format_number_id(nn_ring$rho_laten[1], 3L),
          "$ ($\\bar{R}$ Thorndike) dan menghasilkan $SD$ skor komposit $= ",
          format_number_id(nn_ring$sd_komposit[1], 1L),
          "$. Simulasi B memakai korelasi laten $= ",
          format_number_id(nn_ring$rho_laten[2], 3L),
          "$, dikalibrasi agar $SD$ skor komposit $= S_Z = ",
          format_number_id(nn_ring$s_z_thorndike[2], 2L),
          "$. Skor komposit simulasi dipusatkan ke rerata $500$ (pergeseran ",
          format_number_id(nn_ring$geser_pemusatan[1], 2L),
          " poin). Titik $M \\pm k\\,S_Z$ dengan $M = 500$. Peluang ekor ",
          "mengacu pada sisi bawah untuk $k<0$ dan sisi atas untuk $k>0$. ",
          "Selisih B = skor simulasi B $-$ skor normal."
        ),
        resized = TRUE
      )
    ),
    collapse = "\n"
  )
  writeLines(tex_nn_sim, file.path(DIR_OUT_TABLES, "tab_nonnormal_sim.tex"))
} else {
  message(
    "-> Tabel non-normalitas dilewati: jalankan ",
    "scripts/11_nonnormality_sensitivity.R terlebih dahulu."
  )
}

# 11. Tabel Skenario SLODR (hasil scripts/12_slodr_sensitivity.R) --------------
# Dilewati dengan pesan bila skrip 12 belum pernah dijalankan (mis. 08 dijalankan
# sendiri di luar run_all.R).
PATH_SL_RING <- file.path(DIR_OUT_TABLES, "slodr_ringkasan_ekor.csv")
PATH_SL_EKOR <- file.path(DIR_OUT_TABLES, "slodr_ekor.csv")
PATH_SL_C3   <- file.path(DIR_OUT_TABLES, "slodr_case3.csv")

if (all(file.exists(PATH_SL_RING, PATH_SL_EKOR, PATH_SL_C3))) {
  sl_ring <- readr::read_csv(PATH_SL_RING, col_types = readr::cols(),
                             show_col_types = FALSE)
  sl_ekor <- readr::read_csv(PATH_SL_EKOR, col_types = readr::cols(),
                             show_col_types = FALSE)
  sl_c3   <- readr::read_csv(PATH_SL_C3, col_types = readr::cols(),
                             show_col_types = FALSE)
  stopifnot(
    "slodr_ringkasan_ekor harus 5 kemiringan" = nrow(sl_ring) == 5L,
    "slodr_ekor harus 30 baris (5 kemiringan x 6 titik)" = nrow(sl_ekor) == 30L,
    "slodr_case3 harus 3 kemiringan" = nrow(sl_c3) == 3L,
    "seluruh solver Case III harus konvergen" = all(sl_c3$konvergen)
  )
  
  fmt_pct_sl <- function(x) {
    vapply(
      x,
      function(v) {
        if (v < 0.0005) "$<0{,}001$" else format_number_id(v, 3L)
      },
      character(1)
    )
  }
  
  # 11a. Dampak SLODR pada ekor komposit (marginal Johnson SB)
  kem <- sl_ring$kemiringan
  ambil <- function(k, b, kolom) {
    sl_ekor[[kolom]][sl_ekor$k_sd == k & sl_ekor$kemiringan == b]
  }
  titik_ekor <- c(-3, -2, 2, 3)
  label_k <- function(k) {
    ifelse(k < 0, sprintf("$M - %d\\,S_Z$", as.integer(abs(k))),
           sprintf("$M + %d\\,S_Z$", as.integer(k)))
  }
  
  baris_skew <- paste0(
    "        Skewness komposit & $0{,}000$ & ",
    paste(format_number_id(sl_ring$skew, 3L), collapse = " & "), " \\\\"
  )
  baris_ekor <- vapply(titik_ekor, function(k) {
    paste0(
      "        ", label_k(k), " & ",
      fmt_pct_sl(sl_ekor$pct_ekor_normal[sl_ekor$k_sd == k][1]), " & ",
      paste(fmt_pct_sl(vapply(kem, function(b) ambil(k, b, "pct_ekor_sim"),
                              numeric(1))), collapse = " & "),
      " \\\\"
    )
  }, character(1))
  baris_skor <- vapply(titik_ekor, function(k) {
    paste0(
      "        ", label_k(k), " & ",
      format_number_id(ambil(k, kem[1], "skor_normal"), 1L), " & ",
      paste(format_number_id(
        vapply(kem, function(b) ambil(k, b, "skor_sim"), numeric(1)), 1L
      ), collapse = " & "),
      " \\\\"
    )
  }, character(1))
  
  tex_sl_ekor <- paste(
    c(
      build_latex_table_header(
        "tab:slodr_ekor",
        paste0(
          "Dampak Skenario SLODR terhadap Ekor Distribusi Skor Komposit ",
          "(Marginal Johnson SB, \\textit{Copula} Gaussian)"
        )
      ),
      "    \\resizebox{\\textwidth}{!}{%",
      "    \\begin{tabular}{lcccccc}",
      "        \\toprule",
      paste0(
        "        & Normal & \\multicolumn{5}{c}{Kemiringan SLODR $b$} \\\\"
      ),
      "        \\cmidrule(lr){3-7}",
      paste0(
        "        & & ",
        paste(format_number_id(kem, 2L), collapse = " & "), " \\\\"
      ),
      "        \\midrule",
      baris_skew,
      "        \\midrule",
      paste0("        \\multicolumn{7}{l}{\\textit{Peluang ekor (\\%)}} \\\\"),
      baris_ekor,
      "        \\midrule",
      paste0("        \\multicolumn{7}{l}{\\textit{Skor komposit pada persentil normal}} \\\\"),
      baris_skor,
      build_latex_table_footer(
        note = paste0(
          "SLODR adalah \\textit{Spearman's Law of Diminishing Returns}, yaitu gagasan bahwa korelasi antartes menurun pada kemampuan tinggi. Muatan \\textit{g-factor} ditetapkan sebagai $m(g) = m_0 - b\\,g$ dengan $g \\sim N(0,1)$; ",
          "$b = 0$ berarti korelasi konstan. $m_0$ dikalibrasi pada tiap $b$ ",
          "sehingga $SD$ komposit $= S_Z = ",
          format_number_id(sl_ring$sd_komposit[1], 2L),
          "$; skor komposit dipusatkan ke $500$. Peluang ekor mengacu pada sisi ",
          "bawah untuk $k<0$ dan sisi atas untuk $k>0$. Korelasi bersyarat ",
          "pada $g=-1$, $+1$, dan $+2$ masing-masing $",
          format_number_id(sl_ring$r_kond_g_min1[3], 3L), "$, $",
          format_number_id(sl_ring$r_kond_g_plus1[3], 3L), "$, dan $",
          format_number_id(sl_ring$r_kond_g_plus2[3], 3L),
          "$ pada $b = ", format_number_id(sl_ring$kemiringan[3], 2L),
          "$."
        ),
        resized = TRUE
      )
    ),
    collapse = "\n"
  )
  writeLines(tex_sl_ekor, file.path(DIR_OUT_TABLES, "tab_slodr_ekor.tex"))
  
  # 11b. Dampak SLODR pada koreksi Case III (populasi Gaussian)
  baris_c3 <- vapply(
    seq_len(nrow(sl_c3)),
    function(i) {
      sprintf(
        "        %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
        format_number_id(sl_c3$kemiringan[i], 2L),
        format_number_id(sl_c3$r_kond_g_min1[i], 3L),
        format_number_id(sl_c3$r_kond_g_plus1[i], 3L),
        format_number_id(sl_c3$r_populasi_benar[i], 3L),
        format_number_id(sl_c3$r_case3_taksiran[i], 3L),
        format_number_id(sl_c3$bias_r[i], 3L),
        format_number_id(sl_c3$s_z_benar[i], 1L),
        format_number_id(sl_c3$s_z_case3_taksiran[i], 1L),
        format_number_id(sl_c3$bias_s_z[i], 1L)
      )
    },
    character(1)
  )
  
  tex_sl_c3 <- paste(
    c(
      build_latex_table_header(
        "tab:slodr_case3",
        paste0(
          "Kinerja Koreksi Thorndike Case III pada Populasi Simulasi dengan ",
          "Korelasi yang Menurun pada Skor Tinggi (SLODR)"
        )
      ),
      "    \\resizebox{\\textwidth}{!}{%",
      "    \\begin{tabular}{ccccccccc}",
      "        \\toprule",
      paste0(
        "        & \\multicolumn{2}{c}{Korelasi Bersyarat} & ",
        "\\multicolumn{3}{c}{$\\bar{R}$} & \\multicolumn{3}{c}{$S_Z$} \\\\"
      ),
      "        \\cmidrule(lr){2-3} \\cmidrule(lr){4-6} \\cmidrule(lr){7-9}",
      paste0(
        "        $b$ & $g=-1$ & $g=+1$ & Benar & Case III & Bias & ",
        "Benar & Case III & Bias \\\\"
      ),
      "        \\midrule",
      baris_c3,
      build_latex_table_footer(
        note = paste0(
          "SLODR adalah \\textit{Spearman's Law of Diminishing Returns}. Populasi Gaussian dengan $SD$ nasional tidak resmi dan muatan ",
          "$m(g) = m_0 - b\\,g$; sampel terseleksi lewat skor komposit ",
          "(probabilitas seleksi logistik) dikalibrasi agar sama dengan ",
          "sampel teramati (rerata skor komposit ",
          format_number_id(sl_c3$sampel_sim_rerata[1], 1L), ", $SD$ ",
          format_number_id(sl_c3$sampel_sim_sd[1], 1L),
          ", rerata korelasi \\textit{raw sample} ",
          format_number_id(sl_c3$r_mentah_sim[1], 3L),
          "). Karena sampel sama pada semua $b$, taksiran Case III identik; ",
          "kolom Benar adalah nilai pada populasi simulasi. Bias = Case III ",
          "$-$ Benar."
        ),
        resized = TRUE
      )
    ),
    collapse = "\n"
  )
  writeLines(tex_sl_c3, file.path(DIR_OUT_TABLES, "tab_slodr_case3.tex"))
} else {
  message(
    "-> Tabel SLODR dilewati: jalankan ",
    "scripts/12_slodr_sensitivity.R terlebih dahulu."
  )
}

message(
  "Sukses! Seluruh tabel LaTeX publikasi berhasil diekspor ke output/tables/"
)