# =============================================================================
# Judul       : 09 Analisis Sensitivitas: Struktur Faktor dan Peran LBI
# Deskripsi   : Menguji seberapa bergantung hasil utama pada perlakuan dua skor
#               LBI (LBI Saintek dan LBI Soshum berasal dari satu tes yang
#               sama). Tiga bagian: (A) EFA 3-faktor untuk menafsirkan faktor
#               ketiga yang disarankan Parallel Analysis; (B) CFA 2-faktor
#               dengan kovarians residual antar-LBI; (C) koreksi Thorndike
#               Case III dengan salah satu atau kedua LBI dikeluarkan.
#               Spesifikasi utama (8 subtes) TIDAK diubah oleh skrip ini.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-10-06
# Input       : data/processed/utbk_clean.csv
#               data/raw/utbk_national_descriptive_statistics.csv
# Output      : output/tables/sens_efa3_loadings.csv
#               output/tables/sens_efa3_phi.csv
#               output/tables/sens_efa3_fit.csv
#               output/tables/sens_cfa_fit.csv
#               output/tables/sens_cfa_ringkasan.csv
#               output/tables/sens_cfa_loadings_resid.csv
#               output/tables/sens_thorndike_lbi.csv
# Catatan     : Setiap CSV hanya ditulis ke disk; ringkasannya juga dicetak
#               di konsol pada bagian akhir.
# =============================================================================

# Muat Package ----------------------------------------------------------------
library(dplyr)
library(readr)
library(tibble)
library(here)
library(lavaan)
library(psych)

# Muat Modul Utilitas ---------------------------------------------------------
source(here::here("R", "constants.R"))
source(here::here("R", "utils_stats.R"))

# Konstanta Path --------------------------------------------------------------
DIR_OUT_TABLES <- here::here("output", "tables")
dir.create(DIR_OUT_TABLES, recursive = TRUE, showWarnings = FALSE)

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

sigma_nat_dict <- setNames(data_nat$standard_deviation, data_nat$subject_name)
stopifnot(
  "Seluruh subtes harus ada di data nasional" =
    all(SUBTESTS %in% names(sigma_nat_dict))
)

# 2. Bagian A: EFA 3-Faktor ---------------------------------------------------
# Parallel Analysis menyarankan 3 faktor. EFA 3-faktor dijalankan hanya untuk
# MENAFSIRKAN faktor ketiga (siapa yang membebaninya), bukan untuk mengganti
# struktur dua-faktor a priori. Pengaturan sama dengan run_utbk_efa().
message("-> [A] EFA 3-faktor (minres, oblimin)...")

efa3 <- psych::fa(
  as.matrix(data_clean[, SUBTESTS]),
  nfactors = 3L,
  rotate   = "oblimin",
  fm       = "minres"
)

mat_load3 <- unclass(efa3$loadings)
colnames(mat_load3) <- tolower(colnames(mat_load3))
efa3_loadings <- dplyr::bind_cols(
  tibble::tibble(subtest = rownames(mat_load3)),
  tibble::as_tibble(mat_load3),
  tibble::tibble(
    h2  = unname(efa3$communality),
    com = unname(efa3$complexity)
  )
)

efa3_phi <- tibble::as_tibble(efa3$Phi, .name_repair = "minimal")
colnames(efa3_phi) <- colnames(efa3$Phi)
efa3_phi <- dplyr::bind_cols(
  tibble::tibble(faktor = rownames(efa3$Phi)),
  efa3_phi
)

efa3_fit <- tibble::tibble(
  n_factors = 3L,
  df        = unname(efa3$dof),
  chisq     = unname(efa3$STATISTIC),
  p_value   = unname(efa3$PVAL),
  tli       = unname(efa3$TLI),
  rmsea     = unname(efa3$RMSEA["RMSEA"]),
  rmsea_lo  = unname(efa3$RMSEA["lower"]),
  rmsea_hi  = unname(efa3$RMSEA["upper"]),
  rms_resid = unname(efa3$rms)
)

# 3. Bagian B: CFA dengan Kovarians Residual Antar-LBI ------------------------
# Model utama (dua-faktor) vs model yang sama + kovarians residual
# lbi_saintek ~~ lbi_soshum. Model utama dipanggil ulang dari
# fit_utbk_cfa_models() agar spesifikasinya identik dengan Tabel CFA di naskah.
message("-> [B] CFA 2-faktor dengan kovarians residual antar-LBI (MLM)...")

cfa_utama <- fit_utbk_cfa_models(data_clean, estimator = "MLM")
fit_utama <- cfa_utama$fit_2f

model_2f_resid <- "
  literacy =~ ppu + pbm + lbi_saintek + lbi_soshum + lbe
  numeracy =~ pu + pk + pm

  literacy ~~ numeracy

  # Kedua skor LBI berasal dari satu tes: residualnya boleh berkovarians
  lbi_saintek ~~ lbi_soshum
"
fit_resid <- lavaan::cfa(
  model          = model_2f_resid,
  data           = data_clean,
  estimator      = "MLM",
  std.lv         = TRUE,
  auto.fix.first = FALSE
)

ringkas_fit_cfa <- function(fit, nama_model) {
  fm <- lavaan::fitMeasures(fit)
  tibble::tibble(
    model        = nama_model,
    chisq_scaled = pick_fit_measure(fm, "chisq.scaled", "chisq"),
    df           = as.integer(pick_fit_measure(fm, "df.scaled", "df")),
    cfi_robust   = pick_fit_measure(fm, "cfi.robust", "cfi"),
    tli_robust   = pick_fit_measure(fm, "tli.robust", "tli"),
    rmsea_robust = pick_fit_measure(fm, "rmsea.robust", "rmsea"),
    srmr         = unname(fm["srmr"]),
    aic          = unname(fm["aic"]),
    bic          = unname(fm["bic"])
  )
}

cfa_fit_sens <- dplyr::bind_rows(
  ringkas_fit_cfa(fit_utama, "2-faktor utama (tanpa kovarians residual)"),
  ringkas_fit_cfa(fit_resid, "2-faktor + kovarians residual antar-LBI")
)

# Uji beda chi-square (Satorra-Bentler, lavaan menyesuaikan otomatis untuk MLM)
lrt_resid <- lavaan::lavTestLRT(fit_utama, fit_resid)
cfa_fit_sens <- cfa_fit_sens |>
  dplyr::mutate(
    delta_chisq = c(NA_real_, unname(lrt_resid$`Chisq diff`[2])),
    delta_df    = c(NA_integer_, as.integer(unname(lrt_resid$`Df diff`[2]))),
    delta_p     = c(NA_real_, unname(lrt_resid$`Pr(>Chisq)`[2]))
  )

ambil_std <- function(fit, lhs, rhs) {
  sol <- lavaan::standardizedSolution(fit)
  baris <- sol[sol$op == "~~" & sol$lhs == lhs & sol$rhs == rhs, , drop = FALSE]
  stopifnot("Parameter tidak ditemukan" = nrow(baris) == 1L)
  baris$est.std
}

cfa_ringkasan <- tibble::tibble(
  model            = cfa_fit_sens$model,
  phi_literasi_numerasi = c(
    ambil_std(fit_utama, "literacy", "numeracy"),
    ambil_std(fit_resid, "literacy", "numeracy")
  ),
  korelasi_residual_lbi = c(
    NA_real_,
    ambil_std(fit_resid, "lbi_saintek", "lbi_soshum")
  )
)

cfa_loadings_resid <- extract_cfa_loadings(fit_resid)

# 4. Bagian C: Thorndike Case III dengan LBI Dikeluarkan ----------------------
# Komposit = rerata subtes yang dipakai (sama dengan definisi utama, tetapi
# k berbeda). Rata-rata dua LBI TIDAK dipakai karena SD nasional rata-rata itu
# tidak tersedia; sebagai gantinya salah satu atau kedua LBI dikeluarkan,
# sehingga tiap subtes yang tersisa memakai SD nasionalnya sendiri.
message("-> [C] Thorndike Case III dengan LBI dikeluarkan...")

jalankan_thorndike_subset <- function(df_data, subtests, nama_skenario) {
  mat_x      <- as.matrix(df_data[, subtests])
  k          <- length(subtests)
  vec_z      <- rowMeans(mat_x)
  mat_r_raw  <- cor(mat_x)
  vec_r_iz   <- vapply(
    seq_len(k), function(i) cor(mat_x[, i], vec_z), numeric(1)
  )
  s_sample_z    <- sd(vec_z)
  vec_sigma_nat <- unname(sigma_nat_dict[subtests])
  
  hasil <- solve_population_sd(
    mat_r_raw     = mat_r_raw,
    vec_r_iz      = vec_r_iz,
    vec_sigma_nat = vec_sigma_nat,
    s_sample_z    = s_sample_z,
    s_init        = s_sample_z,
    tol           = TOL_FIXED_POINT,
    max_iter      = MAX_ITER_FIXED_POINT
  )
  diag_sel <- check_selection_assumption(
    subtests      = subtests,
    sd_sample     = apply(mat_x, 2, sd),
    vec_r_iz      = vec_r_iz,
    lambda_sq     = hasil$lambda_sq,
    vec_sigma_nat = vec_sigma_nat
  )
  
  tibble::tibble(
    skenario         = nama_skenario,
    k                = k,
    r_raw_rerata     = compute_off_diagonal_mean(mat_r_raw),
    s_sample_z       = s_sample_z,
    s_population_z   = hasil$s_population_z,
    lambda_sq        = hasil$lambda_sq,
    r_terkoreksi_rerata = hasil$mean_r_corrected,
    n_iterasi        = hasil$n_iterations,
    konvergen        = hasil$converged,
    rasio_sd_min     = min(diag_sel$ratio),
    rasio_sd_maks    = max(diag_sel$ratio)
  )
}

thorndike_lbi <- dplyr::bind_rows(
  jalankan_thorndike_subset(
    data_clean, SUBTESTS, "8 subtes (spesifikasi utama)"
  ),
  jalankan_thorndike_subset(
    data_clean, setdiff(SUBTESTS, "lbi_soshum"), "7 subtes tanpa LBI Soshum"
  ),
  jalankan_thorndike_subset(
    data_clean, setdiff(SUBTESTS, "lbi_saintek"), "7 subtes tanpa LBI Saintek"
  ),
  jalankan_thorndike_subset(
    data_clean,
    setdiff(SUBTESTS, c("lbi_saintek", "lbi_soshum")),
    "6 subtes tanpa kedua LBI"
  )
)

if (!all(thorndike_lbi$konvergen)) {
  warning("Ada skenario Thorndike yang tidak konvergen; jangan dilaporkan.",
          call. = FALSE)
}

# 5. Simpan Hasil -------------------------------------------------------------
readr::write_csv(efa3_loadings, file.path(DIR_OUT_TABLES, "sens_efa3_loadings.csv"))
readr::write_csv(efa3_phi,      file.path(DIR_OUT_TABLES, "sens_efa3_phi.csv"))
readr::write_csv(efa3_fit,      file.path(DIR_OUT_TABLES, "sens_efa3_fit.csv"))
readr::write_csv(cfa_fit_sens,  file.path(DIR_OUT_TABLES, "sens_cfa_fit.csv"))
readr::write_csv(cfa_ringkasan, file.path(DIR_OUT_TABLES, "sens_cfa_ringkasan.csv"))
readr::write_csv(
  cfa_loadings_resid, file.path(DIR_OUT_TABLES, "sens_cfa_loadings_resid.csv")
)
readr::write_csv(thorndike_lbi, file.path(DIR_OUT_TABLES, "sens_thorndike_lbi.csv"))