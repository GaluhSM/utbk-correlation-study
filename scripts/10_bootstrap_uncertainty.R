# =============================================================================
# Judul       : 10 Ketidakpastian Estimasi: Bootstrap dan Sensitivitas SD Nasional
# Deskripsi   : Dua analisis ketidakpastian untuk estimasi utama Thorndike
#               Case III (8 subtes). (A) Bootstrap nonparametrik (resampling
#               baris responden) untuk interval kepercayaan S_Z, lambda^2, dan
#               rerata korelasi terkoreksi (R-bar), beserta skor komposit pada
#               M +/- k SD. (B) Sensitivitas terhadap SD nasional yang tidak
#               resmi: seluruh SD dikalikan faktor tertentu, dan PU diganti 100.
#               Bootstrap HANYA menangkap ketidakpastian sampling, bukan
#               kesalahan spesifikasi model maupun ketidakakuratan SD nasional;
#               ketidakakuratan terakhir itulah yang diuji pada bagian B.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-10-06
# Input       : data/processed/utbk_clean.csv
#               data/raw/utbk_national_descriptive_statistics.csv
# Output      : output/tables/boot_draws.csv
#               output/tables/boot_ringkasan.csv
#               output/tables/boot_skor_komposit.csv
#               output/tables/sens_sd_nasional.csv
# =============================================================================

# Muat Package ----------------------------------------------------------------
library(dplyr)
library(readr)
library(tibble)
library(here)

# Muat Modul Utilitas ---------------------------------------------------------
source(here::here("R", "constants.R"))
source(here::here("R", "utils_stats.R"))

# Konstanta Skrip -------------------------------------------------------------
# Dua konstanta ini sengaja lokal di skrip ini (bukan di constants.R) agar
# berkas lain tidak perlu diubah; pindahkan ke constants.R bila dipakai ulang.
N_BOOTSTRAP    <- 2000L
SEED_BOOTSTRAP <- 2026L
LEVEL_CI       <- 0.95
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
vec_sigma_nat <- unname(sigma_nat_dict[SUBTESTS])

mat_x <- as.matrix(data_clean[, SUBTESTS])
n_obs <- nrow(mat_x)

# 2. Solver Ringan ------------------------------------------------------------
# Versi ringkas dari solve_population_sd() (rumus dan toleransi identik) tanpa
# pencatatan riwayat per iterasi, agar 2.000 replikasi bootstrap cepat. Hasilnya
# divalidasi terhadap solve_population_sd() pada sampel penuh di bawah.
selesaikan_ringan <- function(mat_r_raw,
                              vec_r_iz,
                              vec_sigma,
                              s_sample_z,
                              tol      = TOL_FIXED_POINT,
                              max_iter = MAX_ITER_FIXED_POINT) {
  k         <- length(vec_sigma)
  outer_sig <- outer(vec_sigma, vec_sigma)
  outer_riz <- outer(vec_r_iz, vec_r_iz)
  
  koreksi <- function(lambda_sq) {
    faktor <- sqrt(1 + lambda_sq * vec_r_iz^2)
    mat    <- (mat_r_raw + lambda_sq * outer_riz) / outer(faktor, faktor)
    diag(mat) <- 1
    mat
  }
  
  s_curr    <- s_sample_z
  konvergen <- FALSE
  n_iter    <- 0L
  for (iter in seq_len(max_iter)) {
    lambda_sq <- (s_curr / s_sample_z)^2 - 1
    s_next    <- sqrt(sum(outer_sig * koreksi(lambda_sq)) / k^2)
    delta     <- abs(s_next - s_curr)
    s_curr    <- s_next
    n_iter    <- iter
    if (delta <= tol) {
      konvergen <- TRUE
      break
    }
  }
  
  lambda_final <- (s_curr / s_sample_z)^2 - 1
  mat_final    <- koreksi(lambda_final)
  
  c(
    s_population_z      = s_curr,
    lambda_sq           = lambda_final,
    r_terkoreksi_rerata = compute_off_diagonal_mean(mat_final),
    konvergen           = as.numeric(konvergen),
    n_iterasi           = n_iter
  )
}

estimasi_dari_data <- function(mat_data, vec_sigma) {
  vec_z <- rowMeans(mat_data)
  selesaikan_ringan(
    mat_r_raw  = cor(mat_data),
    vec_r_iz   = as.numeric(cor(mat_data, vec_z)),
    vec_sigma  = vec_sigma,
    s_sample_z = sd(vec_z)
  )
}

# 3. Validasi Solver Ringan ---------------------------------------------------
message("-> Validasi solver ringan terhadap solve_population_sd()...")
vec_z_full <- rowMeans(mat_x)
ref <- solve_population_sd(
  mat_r_raw     = cor(mat_x),
  vec_r_iz      = as.numeric(cor(mat_x, vec_z_full)),
  vec_sigma_nat = vec_sigma_nat,
  s_sample_z    = sd(vec_z_full),
  s_init        = sd(vec_z_full),
  tol           = TOL_FIXED_POINT,
  max_iter      = MAX_ITER_FIXED_POINT
)
est_utama <- estimasi_dari_data(mat_x, vec_sigma_nat)

stopifnot(
  "S_Z solver ringan harus sama dengan solve_population_sd()" =
    abs(est_utama["s_population_z"] - ref$s_population_z) < 1e-9,
  "R-bar solver ringan harus sama dengan solve_population_sd()" =
    abs(est_utama["r_terkoreksi_rerata"] - ref$mean_r_corrected) < 1e-9
)

# 4. Bagian A: Bootstrap Nonparametrik ----------------------------------------
message(sprintf("-> [A] Bootstrap %d replikasi (seed %d)...",
                N_BOOTSTRAP, SEED_BOOTSTRAP))
set.seed(SEED_BOOTSTRAP)
waktu_mulai <- Sys.time()
mat_boot <- t(vapply(
  seq_len(N_BOOTSTRAP),
  function(b) {
    idx <- sample.int(n_obs, n_obs, replace = TRUE)
    estimasi_dari_data(mat_x[idx, , drop = FALSE], vec_sigma_nat)
  },
  numeric(5)
))
waktu_selesai <- Sys.time()

boot_draws <- tibble::as_tibble(mat_boot) |>
  dplyr::mutate(replikasi = dplyr::row_number(), .before = 1L)

boot_ok   <- boot_draws[boot_draws$konvergen == 1, ]
n_konv    <- nrow(boot_ok)
prob_ci   <- c((1 - LEVEL_CI) / 2, 1 - (1 - LEVEL_CI) / 2)

ringkas_boot <- function(nama, estimasi, vec_boot) {
  batas <- unname(stats::quantile(vec_boot, probs = prob_ci))
  tibble::tibble(
    statistik   = nama,
    estimasi    = unname(estimasi),
    boot_rerata = mean(vec_boot),
    boot_se     = sd(vec_boot),
    ci_bawah    = batas[1],
    ci_atas     = batas[2],
    n_boot      = N_BOOTSTRAP,
    n_konvergen = n_konv
  )
}

boot_ringkasan <- dplyr::bind_rows(
  ringkas_boot("S_Z populasi", est_utama["s_population_z"],
               boot_ok$s_population_z),
  ringkas_boot("lambda^2", est_utama["lambda_sq"], boot_ok$lambda_sq),
  ringkas_boot("R-bar terkoreksi", est_utama["r_terkoreksi_rerata"],
               boot_ok$r_terkoreksi_rerata)
)

# Skor komposit pada M + k SD: skala skor ikut bergeser bila S_Z berbeda.
k_sd <- c(-3, -2, -1, 1, 2, 3)
boot_skor <- tibble::tibble(
  k_sd       = k_sd,
  skor_titik = MU_POPULATION + k_sd * unname(est_utama["s_population_z"]),
  ci_bawah   = MU_POPULATION + k_sd * ifelse(
    k_sd > 0,
    unname(stats::quantile(boot_ok$s_population_z, prob_ci[1])),
    unname(stats::quantile(boot_ok$s_population_z, prob_ci[2]))
  ),
  ci_atas    = MU_POPULATION + k_sd * ifelse(
    k_sd > 0,
    unname(stats::quantile(boot_ok$s_population_z, prob_ci[2])),
    unname(stats::quantile(boot_ok$s_population_z, prob_ci[1]))
  )
)

# 5. Bagian B: Sensitivitas SD Nasional ---------------------------------------
message("-> [B] Sensitivitas terhadap SD nasional...")
sd_sampel_subtes <- apply(mat_x, 2, sd)

skenario_sd <- list(
  "SD nasional apa adanya (utama)" = vec_sigma_nat,
  "Semua SD x 0,90"                = vec_sigma_nat * 0.90,
  "Semua SD x 0,95"                = vec_sigma_nat * 0.95,
  "Semua SD x 1,05"                = vec_sigma_nat * 1.05,
  "Semua SD x 1,10"                = vec_sigma_nat * 1.10,
  "SD PU = 100 (SD lain tetap)"    = replace(vec_sigma_nat, SUBTESTS == "pu", 100)
)

sens_sd <- dplyr::bind_rows(lapply(names(skenario_sd), function(nm) {
  sigma_skenario <- skenario_sd[[nm]]
  est   <- estimasi_dari_data(mat_x, sigma_skenario)
  r_iz  <- as.numeric(cor(mat_x, vec_z_full))
  rasio <- sd_sampel_subtes * sqrt(1 + r_iz^2 * est["lambda_sq"]) /
    sigma_skenario
  tibble::tibble(
    skenario            = nm,
    s_population_z      = unname(est["s_population_z"]),
    lambda_sq           = unname(est["lambda_sq"]),
    r_terkoreksi_rerata = unname(est["r_terkoreksi_rerata"]),
    rasio_sd_min        = min(rasio),
    rasio_sd_maks       = max(rasio),
    konvergen           = unname(est["konvergen"]) == 1
  )
}))

# 6. Simpan Hasil -------------------------------------------------------------
readr::write_csv(boot_draws,    file.path(DIR_OUT_TABLES, "boot_draws.csv"))
readr::write_csv(boot_ringkasan, file.path(DIR_OUT_TABLES, "boot_ringkasan.csv"))
readr::write_csv(boot_skor,     file.path(DIR_OUT_TABLES, "boot_skor_komposit.csv"))
readr::write_csv(sens_sd,       file.path(DIR_OUT_TABLES, "sens_sd_nasional.csv"))