# =============================================================================
# Judul       : 12 Skenario SLODR: Korelasi yang Bergantung pada Tingkat Skor
# Deskripsi   : Menguji dua implikasi yang dinyatakan pada Subbagian
#               "Diferensiasi Kemampuan pada Skor Tinggi" di Pembahasan,
#               yaitu bila korelasi antarsubtes menurun pada skor tinggi
#               (Spearman's Law of Diminishing Returns, SLODR).
#               Model: faktor umum g ~ N(0,1); skor laten subtes
#               Z_j = m(g) * g + sqrt(1 - m(g)^2) * e_j dengan muatan
#               m(g) = m0 - b * g (b = "kemiringan" SLODR; b = 0 berarti
#               korelasi konstan). Setiap Z_j kemudian dijadikan berdistribusi
#               normal baku lewat peringkat agar marginal tidak berubah.
#               (A) Implikasi ke persentil: marginal Johnson SB hasil skrip 11,
#               m0 dikalibrasi agar SD komposit = S_Z Thorndike pada tiap b,
#               lalu peluang ekor dan skor komposit dibandingkan dengan normal
#               dan dengan korelasi konstan (b = 0).
#               (B) Implikasi ke koreksi Case III: populasi Gaussian dengan SD
#               nasional tidak resmi; sampel terseleksi lewat komposit
#               (probabilitas seleksi logistik) dikalibrasi agar sama dengan
#               sampel nyata (rerata komposit, SD komposit, rerata korelasi
#               mentah); koreksi Case III dijalankan pada sampel itu dan
#               dibandingkan dengan korelasi populasi sebenarnya pada simulasi.
#               Karena sampel nyata sama untuk semua b, taksiran Case III
#               praktis sama; yang berubah adalah nilai sebenarnya.
#               CATATAN KETERBATASAN: model ini satu faktor dan sengaja
#               sederhana; bentuk SLODR (linear pada g) dan besaran b
#               bersifat skenario ilustratif, bukan estimasi dari data;
#               bagian B memakai marginal normal agar terpisah dari efek
#               marginal miring (bagian A).
# Penulis     : Galuh Saputra
# Tanggal     : 2026-10-07
# Input       : data/processed/utbk_clean.csv
#               data/raw/utbk_national_descriptive_statistics.csv
#               output/tables/iteration_history.csv
#               output/tables/nonnorm_fit_marginal.csv (hasil skrip 11)
# Output      : output/tables/slodr_ringkasan_ekor.csv
#               output/tables/slodr_ekor.csv
#               output/tables/slodr_case3.csv
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
# Sengaja lokal di skrip ini (bukan di constants.R); pindahkan bila dipakai
# ulang oleh skrip lain.
N_SIM_SLODR    <- 300000L
SEED_SLODR     <- 2026L
KEMIRINGAN_A   <- c(0, 0.01, 0.02, 0.03, 0.04)
KEMIRINGAN_B   <- c(0, 0.02, 0.04)
K_SD           <- c(-3, -2, -1, 1, 2, 3)
DIR_OUT_TABLES <- here::here("output", "tables")
dir.create(DIR_OUT_TABLES, recursive = TRUE, showWarnings = FALSE)

# 1. Impor Data ---------------------------------------------------------------
data_clean <- readr::read_csv(
  here::here("data", "processed", "utbk_clean.csv"),
  col_types = readr::cols(), show_col_types = FALSE
)
data_nat <- readr::read_csv(
  here::here("data", "raw", "utbk_national_descriptive_statistics.csv"),
  col_types = readr::cols(), show_col_types = FALSE
)
history_df <- readr::read_csv(
  file.path(DIR_OUT_TABLES, "iteration_history.csv"),
  col_types = readr::cols(), show_col_types = FALSE
)
path_fit <- file.path(DIR_OUT_TABLES, "nonnorm_fit_marginal.csv")
if (!file.exists(path_fit)) {
  stop("nonnorm_fit_marginal.csv belum ada: jalankan skrip 11 terlebih dahulu.")
}
fit_marginal <- readr::read_csv(
  path_fit, col_types = readr::cols(), show_col_types = FALSE
)
fit_marginal <- fit_marginal[match(SUBTESTS, fit_marginal$subtes), ]
prm_mat <- as.matrix(fit_marginal[, c("xi", "lambda", "gamma", "delta")])
rownames(prm_mat) <- fit_marginal$subtes

S_Z_THORNDIKE <- tail(history_df$sd_composite_out, 1)
R_THORNDIKE   <- tail(history_df$mean_r_corrected, 1)

data_nat      <- data_nat[match(SUBTESTS, data_nat$subject_name), ]
vec_sigma_nat <- data_nat$standard_deviation

mat_x       <- as.matrix(data_clean[, SUBTESTS])
komp_sampel <- rowMeans(mat_x)
M_SAMPEL    <- mean(komp_sampel)
S_SAMPEL    <- stats::sd(komp_sampel)
R_MENTAH    <- compute_off_diagonal_mean(stats::cor(mat_x))

# 2. Pembangkit Skor Laten SLODR ----------------------------------------------
# Bilangan acak dibangkitkan sekali dan dipakai ulang di semua skenario
# (common random numbers), sehingga perbedaan antarskenario berasal dari b,
# bukan dari derau simulasi.
set.seed(SEED_SLODR)
g_umum  <- stats::rnorm(N_SIM_SLODR)
eps_mat <- matrix(stats::rnorm(N_SIM_SLODR * N_SUBTESTS), nrow = N_SIM_SLODR)

#' Bangun Skor Laten Normal Baku dengan Muatan Bergantung g
#'
#' @param muatan0 Numeric skalar, muatan pada g = 0.
#' @param kemiringan Numeric skalar b; muatan(g) = muatan0 - b * g.
#'
#' @return Matrix N_SIM_SLODR x N_SUBTESTS, marginal normal baku (via peringkat).
bangun_laten <- function(muatan0, kemiringan) {
  lam <- pmin(pmax(muatan0 - kemiringan * g_umum, 0.05), 0.95)
  z   <- matrix(0, nrow = N_SIM_SLODR, ncol = N_SUBTESTS)
  for (j in seq_len(N_SUBTESTS)) {
    mentah <- lam * g_umum + sqrt(1 - lam^2) * eps_mat[, j]
    z[, j] <- stats::qnorm(
      rank(mentah, ties.method = "average") / (N_SIM_SLODR + 1)
    )
  }
  z
}

hitung_momen <- function(x) {
  m <- mean(x)
  s <- sqrt(mean((x - m)^2))
  c(skew = mean((x - m)^3) / s^3, ekskurtosis = mean((x - m)^4) / s^4 - 3)
}

# 3. Bagian A: Dampak SLODR pada Ekor Persentil Komposit ----------------------
komposit_sb <- function(muatan0, kemiringan) {
  z     <- bangun_laten(muatan0, kemiringan)
  total <- numeric(N_SIM_SLODR)
  for (j in seq_len(N_SUBTESTS)) {
    total <- total + prm_mat[j, "xi"] + prm_mat[j, "lambda"] *
      stats::plogis((z[, j] - prm_mat[j, "gamma"]) / prm_mat[j, "delta"])
  }
  list(komposit = total / N_SUBTESTS, z = z)
}

message("-> [A] Kalibrasi muatan m0 per kemiringan (SD komposit = S_Z)...")
muatan0_a <- vapply(KEMIRINGAN_A, function(b) {
  stats::uniroot(
    function(m0) stats::sd(komposit_sb(m0, b)$komposit) - S_Z_THORNDIKE,
    interval = c(0.30, 0.85), tol = 1e-3
  )$root
}, numeric(1))

message("-> [A] Simulasi komposit per kemiringan...")
hasil_a <- lapply(seq_along(KEMIRINGAN_A), function(i) {
  b   <- KEMIRINGAN_A[i]
  m0  <- muatan0_a[i]
  sim <- komposit_sb(m0, b)
  geser <- mean(sim$komposit) - MU_POPULATION
  idx_off <- which(!diag(N_SUBTESTS), arr.ind = TRUE)
  list(
    b = b, m0 = m0, geser = geser,
    komposit = sim$komposit - geser,
    r_laten  = mean(stats::cor(sim$z)[idx_off])
  )
})

ringkasan_a <- dplyr::bind_rows(lapply(hasil_a, function(h) {
  mom <- hitung_momen(h$komposit)
  tibble::tibble(
    kemiringan      = h$b,
    muatan0         = h$m0,
    r_kond_g_min1   = pmin(pmax(h$m0 + h$b, 0.05), 0.95)^2,
    r_kond_g_plus1  = pmin(pmax(h$m0 - h$b, 0.05), 0.95)^2,
    r_kond_g_plus2  = pmin(pmax(h$m0 - 2 * h$b, 0.05), 0.95)^2,
    r_laten_rerata  = h$r_laten,
    sd_komposit     = stats::sd(h$komposit),
    skew            = unname(mom["skew"]),
    ekskurtosis     = unname(mom["ekskurtosis"]),
    geser_pemusatan = h$geser
  )
}))

ekor_a <- dplyr::bind_rows(lapply(hasil_a, function(h) {
  dplyr::bind_rows(lapply(K_SD, function(k) {
    skor <- MU_POPULATION + k * S_Z_THORNDIKE
    pct_sim <- if (k < 0) mean(h$komposit < skor) else mean(h$komposit >= skor)
    pct_nrm <- if (k < 0) stats::pnorm(k) else stats::pnorm(k, lower.tail = FALSE)
    skor_sim <- unname(stats::quantile(h$komposit, probs = stats::pnorm(k)))
    tibble::tibble(
      kemiringan       = h$b,
      k_sd             = k,
      skor_normal      = skor,
      pct_ekor_sim     = pct_sim * 100,
      pct_ekor_normal  = pct_nrm * 100,
      skor_sim         = skor_sim,
      selisih_skor     = skor_sim - skor
    )
  }))
}))

# 4. Bagian B: Dampak SLODR pada Koreksi Case III -----------------------------
#' Cocokkan Seleksi Logistik pada Komposit ke Rerata dan SD Sampel Nyata
cocokkan_seleksi <- function(komp, target_m, target_s) {
  fungsi <- function(p) {
    w <- stats::plogis((komp - p[1]) / exp(p[2]))
    w <- w / sum(w)
    m <- sum(w * komp)
    s <- sqrt(sum(w * (komp - m)^2))
    ((m - target_m) / 10)^2 + ((s - target_s) / 5)^2
  }
  terbaik <- NULL
  for (c0 in c(560, 620)) {
    o <- stats::optim(c(c0, log(30)), fungsi, method = "Nelder-Mead",
                      control = list(maxit = 300, reltol = 1e-12))
    if (is.null(terbaik) || o$value < terbaik$value) terbaik <- o
  }
  w <- stats::plogis((komp - terbaik$par[1]) / exp(terbaik$par[2]))
  list(w = w / sum(w), loss = terbaik$value)
}

#' Statistik Berbobot Sampel Terseleksi (Rerata, SD, Korelasi, r_iZ)
statistik_terseleksi <- function(x, w) {
  m    <- as.vector(crossprod(w, x))
  xc   <- sweep(x, 2, m)
  cmat <- crossprod(xc * w, xc)
  sdv  <- sqrt(diag(cmat))
  r    <- cmat / outer(sdv, sdv)
  komp <- rowMeans(x)
  mc   <- sum(w * komp)
  cc   <- komp - mc
  sc   <- sqrt(sum(w * cc^2))
  riz  <- as.vector(crossprod(xc * w, cc)) / (sdv * sc)
  list(mat_r = r, r_bar = compute_off_diagonal_mean(r), riz = riz,
       m_komp = mc, s_komp = sc)
}

populasi_gauss <- function(muatan0, kemiringan) {
  z <- bangun_laten(muatan0, kemiringan)
  sweep(z, 2, vec_sigma_nat, "*") + MU_POPULATION
}

r_mentah_terseleksi <- function(muatan0, kemiringan) {
  x  <- populasi_gauss(muatan0, kemiringan)
  se <- cocokkan_seleksi(rowMeans(x), M_SAMPEL, S_SAMPEL)
  statistik_terseleksi(x, se$w)$r_bar - R_MENTAH
}

message("-> [B] Kalibrasi populasi Gaussian agar sampel terpilih = sampel asli...")
hasil_b <- dplyr::bind_rows(lapply(KEMIRINGAN_B, function(b) {
  message("   kemiringan b = ", b)
  m0 <- stats::uniroot(
    function(m) r_mentah_terseleksi(m, b),
    interval = c(0.30, 0.90), tol = 1e-3
  )$root
  x    <- populasi_gauss(m0, b)
  komp <- rowMeans(x)
  se   <- cocokkan_seleksi(komp, M_SAMPEL, S_SAMPEL)
  st   <- statistik_terseleksi(x, se$w)
  ref  <- solve_population_sd(
    mat_r_raw     = st$mat_r,
    vec_r_iz      = st$riz,
    vec_sigma_nat = vec_sigma_nat,
    s_sample_z    = st$s_komp,
    tol           = TOL_FIXED_POINT,
    max_iter      = MAX_ITER_FIXED_POINT
  )
  if (se$loss > 0.01) {
    warning("Seleksi tidak cocok penuh pada b = ", b, " (loss ", se$loss, ").")
  }
  tibble::tibble(
    kemiringan         = b,
    muatan0            = m0,
    r_kond_g_min1      = pmin(pmax(m0 + b, 0.05), 0.95)^2,
    r_kond_g_plus1     = pmin(pmax(m0 - b, 0.05), 0.95)^2,
    loss_seleksi       = se$loss,
    sampel_sim_rerata  = st$m_komp,
    sampel_sim_sd      = st$s_komp,
    r_mentah_sim       = st$r_bar,
    r_populasi_benar   = compute_off_diagonal_mean(stats::cor(x)),
    r_case3_taksiran   = ref$mean_r_corrected,
    bias_r             = ref$mean_r_corrected -
      compute_off_diagonal_mean(stats::cor(x)),
    s_z_benar          = stats::sd(komp),
    s_z_case3_taksiran = ref$s_population_z,
    bias_s_z           = ref$s_population_z - stats::sd(komp),
    konvergen          = ref$converged
  )
}))

# 5. Simpan Hasil -------------------------------------------------------------
readr::write_csv(ringkasan_a, file.path(DIR_OUT_TABLES, "slodr_ringkasan_ekor.csv"))
readr::write_csv(ekor_a,      file.path(DIR_OUT_TABLES, "slodr_ekor.csv"))
readr::write_csv(hasil_b,     file.path(DIR_OUT_TABLES, "slodr_case3.csv"))