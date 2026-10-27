# =============================================================================
# Judul       : 11 Diagnostik Non-Normalitas dan Sensitivitas Persentil
# Deskripsi   : Menguji seberapa jauh asumsi normal pada matriks translasi
#               persentil (skrip 06) tahan terhadap distribusi nasional yang
#               nyata-nyata miring. Tiga bagian. (A) Diagnostik dari tujuh
#               statistik nasional per subtes (min, Q1, median, rerata, SD,
#               Q3, maks): z minimum/maksimum, jumlah peserta yang
#               "diharapkan" di luar rentang bila normal, kemiringan Bowley,
#               selisih rerata-median, IQR/SD. (B) Marginal tiap subtes
#               dihampiri dengan distribusi Johnson SB (terbatas, empat
#               parameter) yang dicocokkan ke kuantil, rerata, dan SD
#               nasional. (C) Simulasi Gaussian copula berkorelasi seragam
#               (korelasi laten = R-bar Thorndike; varian kedua dikalibrasi
#               agar SD komposit = S_Z Thorndike) untuk membandingkan
#               persentil dan skor komposit terhadap versi normal.
#               CATATAN KETERBATASAN: tujuh statistik per subtes tidak
#               mengidentifikasi bentuk distribusi secara unik, dan data
#               mikro nasional tidak tersedia (Box-Cox tidak dapat
#               diterapkan). Hasil bagian B-C bersifat INDIKATIF, bukan
#               pengganti Tabel 8/9 yang tetap memakai asumsi normal.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-10-06
# Input       : data/raw/utbk_national_descriptive_statistics.csv
#               output/tables/iteration_history.csv
# Output      : output/tables/nonnorm_diagnostik.csv
#               output/tables/nonnorm_fit_marginal.csv
#               output/tables/nonnorm_sim_ringkasan.csv
#               output/tables/nonnorm_sim_ekor.csv
#               output/tables/nonnorm_sim_skor.csv
# =============================================================================

# Muat Package ----------------------------------------------------------------
library(dplyr)
library(readr)
library(tibble)
library(here)

# Muat Modul Utilitas ---------------------------------------------------------
source(here::here("R", "constants.R"))

# Konstanta Skrip -------------------------------------------------------------
# Sengaja lokal di skrip ini (bukan di constants.R); pindahkan bila dipakai
# ulang oleh skrip lain.
N_SIM          <- 1000000L
SEED_SIM       <- 2026L
N_RESTART_FIT  <- 40L
K_SD           <- c(-3, -2, -1, 1, 2, 3)
DIR_OUT_TABLES <- here::here("output", "tables")
dir.create(DIR_OUT_TABLES, recursive = TRUE, showWarnings = FALSE)

# 1. Impor Data ---------------------------------------------------------------
data_nat <- readr::read_csv(
  here::here("data", "raw", "utbk_national_descriptive_statistics.csv"),
  col_types      = readr::cols(),
  show_col_types = FALSE
)
data_nat <- data_nat[match(SUBTESTS, data_nat$subject_name), ]
stopifnot(
  "Seluruh subtes harus ada di data nasional" = !anyNA(data_nat$subject_name)
)

history_df <- readr::read_csv(
  here::here("output", "tables", "iteration_history.csv"),
  col_types      = readr::cols(),
  show_col_types = FALSE
)
S_Z_THORNDIKE <- tail(history_df$sd_composite_out, 1)
R_THORNDIKE   <- tail(history_df$mean_r_corrected, 1)

# 2. Bagian A: Diagnostik Non-Normalitas --------------------------------------
# Untuk N = 871.496 peserta, minimum/maksimum sebuah distribusi normal
# diharapkan berada sekitar +/-4,7 SD dari rerata (qnorm pada peluang 1/N).
z_ekstrem_normal <- stats::qnorm(1 / N_POPULATION, lower.tail = FALSE)

diagnostik <- data_nat |>
  dplyr::transmute(
    subtes                    = subject_name,
    z_min                     = (minimum - MU_POPULATION) / standard_deviation,
    z_maks                    = (maximum - MU_POPULATION) / standard_deviation,
    z_ekstrem_normal          = z_ekstrem_normal,
    n_diharapkan_di_bawah_min = N_POPULATION * stats::pnorm(z_min),
    n_diharapkan_di_atas_maks = N_POPULATION *
      stats::pnorm(z_maks, lower.tail = FALSE),
    skew_bowley               = (quartile_three + quartile_one - 2 * median) /
      (quartile_three - quartile_one),
    rerata_minus_median       = mean - median,
    iqr_per_sd                = (quartile_three - quartile_one) /
      standard_deviation,
    skor_m3sd                 = MU_POPULATION - 3 * standard_deviation,
    m3sd_di_bawah_min         = skor_m3sd < minimum,
    skor_p3sd                 = MU_POPULATION + 3 * standard_deviation,
    p3sd_di_atas_maks         = skor_p3sd > maximum
  )

# 3. Bagian B: Marginal Johnson SB per Subtes ---------------------------------
# Kuantil Johnson SB: x(z) = xi + lambda * plogis((z - gamma) / delta), dengan
# z kuantil normal baku. Batas bawah xi dipaksa di bawah minimum nasional dan
# batas atas (xi + lambda) otomatis di atas maksimum. Parameter dicocokkan
# (kuadrat terkecil berbobot, galat dibagi SD nasional) ke lima kuantil
# (minimum dan maksimum diletakkan pada peluang 0,5/N dan 1 - 0,5/N, yaitu
# hampiran nilai ekstrem sampel besar), rerata, dan SD nasional.
# SB dipilih karena mampu meniru ekor kiri yang terpotong sekaligus ekor
# kanan yang panjang; keluarga Johnson SU tidak cocok pada subtes ini.
P_KUANTIL <- c(0.5 / N_POPULATION, 0.25, 0.50, 0.75, 1 - 0.5 / N_POPULATION)
Z_KUANTIL <- stats::qnorm(P_KUANTIL)
BOBOT_FIT <- c(0.5, 1, 1, 1, 0.5, 1, 1)   # 5 kuantil, rerata, SD

# Kuadratur untuk rerata dan SD distribusi hasil hampiran.
Z_GRID <- seq(-8, 8, length.out = 801)
W_GRID <- stats::dnorm(Z_GRID)
W_GRID <- W_GRID / sum(W_GRID)

kuantil_sb <- function(prm, z) {
  prm[["xi"]] + prm[["lambda"]] *
    stats::plogis((z - prm[["gamma"]]) / prm[["delta"]])
}

#' Cocokkan Johnson SB ke Kuantil, Rerata, dan SD Nasional
#'
#' @param target_q Numeric(5), skor pada P_KUANTIL (min, Q1, median, Q3, maks).
#' @param target_mean,target_sd Numeric skalar, rerata dan SD nasional.
#' @param n_restart Integer, jumlah titik awal acak Nelder-Mead.
#'
#' @return List: prm (xi, lambda, gamma, delta), loss, dan hasil hampiran.
cocokkan_johnson_sb <- function(target_q, target_mean, target_sd, n_restart) {
  x_min <- target_q[1]
  x_maks <- target_q[length(target_q)]
  
  tulis_param <- function(par) {
    c(xi = x_min - exp(par[1]), lambda = exp(par[2]),
      gamma = par[3], delta = exp(par[4]))
  }
  
  fungsi_loss <- function(par) {
    prm <- tulis_param(par)
    if (prm[["delta"]] < 0.05 || prm[["lambda"]] <= 0) return(1e9)
    q_fit  <- kuantil_sb(prm, Z_KUANTIL)
    x_grid <- kuantil_sb(prm, Z_GRID)
    m_fit  <- sum(W_GRID * x_grid)
    s_fit  <- sqrt(sum(W_GRID * (x_grid - m_fit)^2))
    galat  <- c((q_fit - target_q) / target_sd,
                (m_fit - target_mean) / target_sd,
                (s_fit - target_sd) / target_sd)
    if (!all(is.finite(galat))) return(1e9)
    sum(BOBOT_FIT * galat^2)
  }
  
  terbaik <- NULL
  for (i in seq_len(n_restart)) {
    par0 <- c(
      log(stats::runif(1, 1, 60)),
      log((x_maks - x_min) * stats::runif(1, 1, 3)),
      stats::rnorm(1, 0, 1.5),
      log(stats::runif(1, 0.5, 3))
    )
    o <- stats::optim(par0, fungsi_loss, method = "Nelder-Mead",
                      control = list(maxit = 3000, reltol = 1e-12))
    if (is.null(terbaik) || o$value < terbaik$value) terbaik <- o
  }
  
  prm    <- tulis_param(terbaik$par)
  x_grid <- kuantil_sb(prm, Z_GRID)
  m_fit  <- sum(W_GRID * x_grid)
  list(
    prm    = prm,
    loss   = terbaik$value,
    q_fit  = kuantil_sb(prm, Z_KUANTIL),
    m_fit  = m_fit,
    s_fit  = sqrt(sum(W_GRID * (x_grid - m_fit)^2))
  )
}

message("-> [B] Mencocokkan Johnson SB per subtes...")
set.seed(SEED_SIM)
fit_marginal <- dplyr::bind_rows(lapply(seq_len(nrow(data_nat)), function(i) {
  baris <- data_nat[i, ]
  target_q <- c(baris$minimum, baris$quartile_one, baris$median,
                baris$quartile_three, baris$maximum)
  hasil <- cocokkan_johnson_sb(
    target_q    = target_q,
    target_mean = baris$mean,
    target_sd   = baris$standard_deviation,
    n_restart   = N_RESTART_FIT
  )
  tibble::tibble(
    subtes      = baris$subject_name,
    xi          = hasil$prm[["xi"]],
    lambda      = hasil$prm[["lambda"]],
    gamma       = hasil$prm[["gamma"]],
    delta       = hasil$prm[["delta"]],
    loss        = hasil$loss,
    galat_min   = hasil$q_fit[1] - target_q[1],
    galat_q1    = hasil$q_fit[2] - target_q[2],
    galat_med   = hasil$q_fit[3] - target_q[3],
    galat_q3    = hasil$q_fit[4] - target_q[4],
    galat_maks  = hasil$q_fit[5] - target_q[5],
    galat_rerata = hasil$m_fit - baris$mean,
    galat_sd    = hasil$s_fit - baris$standard_deviation
  )
}))

if (any(fit_marginal$loss > 0.05)) {
  warning("Ada subtes dengan loss pencocokan > 0,05; cek tabel fit marginal.")
}

prm_mat <- as.matrix(fit_marginal[, c("xi", "lambda", "gamma", "delta")])
rownames(prm_mat) <- fit_marginal$subtes

# 4. Bagian C: Simulasi Gaussian Copula ---------------------------------------
#' Simulasikan Skor Komposit (Rerata 8 Subtes) dari Copula Gaussian
#'
#' Korelasi laten seragam `rho` (struktur satu faktor umum). Benih diset
#' di dalam fungsi sehingga pemanggilan berulang memakai bilangan acak yang
#' sama (penting untuk kalibrasi rho lewat uniroot).
simulasikan_komposit <- function(rho, prm_mat, n_sim, seed) {
  set.seed(seed)
  umum  <- stats::rnorm(n_sim)
  total <- numeric(n_sim)
  for (j in seq_len(nrow(prm_mat))) {
    z <- sqrt(rho) * umum + sqrt(1 - rho) * stats::rnorm(n_sim)
    total <- total + prm_mat[j, "xi"] + prm_mat[j, "lambda"] *
      stats::plogis((z - prm_mat[j, "gamma"]) / prm_mat[j, "delta"])
  }
  total / nrow(prm_mat)
}

hitung_momen <- function(x) {
  m <- mean(x)
  s <- sqrt(mean((x - m)^2))
  c(skew = mean((x - m)^3) / s^3, ekskurtosis = mean((x - m)^4) / s^4 - 3)
}

message("-> [C] Kalibrasi korelasi laten (SD komposit = S_Z Thorndike)...")
f_kalibrasi <- function(rho) {
  stats::sd(simulasikan_komposit(rho, prm_mat, N_SIM, SEED_SIM)) -
    S_Z_THORNDIKE
}
rho_kalibrasi <- stats::uniroot(f_kalibrasi, interval = c(0.05, 0.90),
                                tol = 1e-4)$root

message("-> [C] Simulasi komposit (", N_SIM, " peserta virtual)...")
varian <- list(
  "A. rho laten = R-bar Thorndike" = R_THORNDIKE,
  "B. rho laten dikalibrasi (SD = S_Z)" = rho_kalibrasi
)

hasil_varian <- lapply(names(varian), function(nm) {
  komposit <- simulasikan_komposit(varian[[nm]], prm_mat, N_SIM, SEED_SIM)
  geser    <- mean(komposit) - MU_POPULATION
  # Pemusatan: hampiran marginal menyisakan galat rerata beberapa poin; agar
  # perbandingan murni soal BENTUK, komposit digeser sehingga rerata = 500.
  list(nama = nm, rho = varian[[nm]], geser = geser,
       komposit = komposit - geser)
})

sim_ringkasan <- dplyr::bind_rows(lapply(hasil_varian, function(h) {
  mom <- hitung_momen(h$komposit)
  tibble::tibble(
    varian         = h$nama,
    rho_laten      = h$rho,
    sd_komposit    = stats::sd(h$komposit),
    s_z_thorndike  = S_Z_THORNDIKE,
    skew           = unname(mom["skew"]),
    ekskurtosis    = unname(mom["ekskurtosis"]),
    geser_pemusatan = h$geser,
    n_sim          = N_SIM
  )
}))

sim_ekor <- dplyr::bind_rows(lapply(hasil_varian, function(h) {
  dplyr::bind_rows(lapply(K_SD, function(k) {
    skor <- MU_POPULATION + k * S_Z_THORNDIKE
    tibble::tibble(
      varian              = h$nama,
      k_sd                = k,
      skor                = skor,
      pct_di_bawah_sim    = mean(h$komposit < skor) * 100,
      pct_di_bawah_normal = stats::pnorm(k) * 100,
      pct_di_atas_sim     = mean(h$komposit >= skor) * 100,
      pct_di_atas_normal  = stats::pnorm(k, lower.tail = FALSE) * 100
    )
  }))
})) |>
  dplyr::mutate(
    rank_atas_sim    = round(N_POPULATION * pct_di_atas_sim / 100),
    rank_atas_normal = round(N_POPULATION * pct_di_atas_normal / 100)
  )

sim_skor <- dplyr::bind_rows(lapply(hasil_varian, function(h) {
  dplyr::bind_rows(lapply(K_SD, function(k) {
    skor_normal <- MU_POPULATION + k * S_Z_THORNDIKE
    skor_sim    <- unname(stats::quantile(h$komposit, probs = stats::pnorm(k)))
    tibble::tibble(
      varian           = h$nama,
      k_sd             = k,
      persentil_normal = stats::pnorm(k) * 100,
      skor_normal      = skor_normal,
      skor_sim         = skor_sim,
      selisih          = skor_sim - skor_normal
    )
  }))
}))

# 5. Simpan Hasil -------------------------------------------------------------
readr::write_csv(diagnostik,    file.path(DIR_OUT_TABLES, "nonnorm_diagnostik.csv"))
readr::write_csv(fit_marginal,  file.path(DIR_OUT_TABLES, "nonnorm_fit_marginal.csv"))
readr::write_csv(sim_ringkasan, file.path(DIR_OUT_TABLES, "nonnorm_sim_ringkasan.csv"))
readr::write_csv(sim_ekor,      file.path(DIR_OUT_TABLES, "nonnorm_sim_ekor.csv"))
readr::write_csv(sim_skor,      file.path(DIR_OUT_TABLES, "nonnorm_sim_skor.csv"))