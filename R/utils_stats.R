# =============================================================================
# Judul       : Utilitas Komputasi Psikometri dan Statistik
# Deskripsi   : Fungsi komputasi korelasi off-diagonal, penyesuaian Thorndike
#               Case III, fixed-point iteration, diagnostik asumsi seleksi,
#               EFA, dan CFA struktural.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-19
# =============================================================================

#' Hitung Rerata Korelasi Off-Diagonal
#'
#' Mengekstraksi seluruh elemen non-diagonal dari matriks korelasi persegi
#' dan menghitung nilai rata-rata aritmetisnya secara presisi.
#'
#' @param mat_korelasi Matrix numerik persegi (k x k).
#'
#' @return Numeric. Rata-rata elemen off-diagonal.
#' @examples
#' mat <- matrix(c(1, 0.5, 0.3, 0.5, 1, 0.4, 0.3, 0.4, 1), nrow = 3)
#' compute_off_diagonal_mean(mat)
#' @export
compute_off_diagonal_mean <- function(mat_korelasi) {
  stopifnot(
    "Input harus berupa matriks" = is.matrix(mat_korelasi),
    "Matriks harus persegi"     = nrow(mat_korelasi) == ncol(mat_korelasi)
  )
  
  n <- nrow(mat_korelasi)
  if (n <= 1) {
    return(NA_real_)
  }
  
  idx_off_diagonal <- which(!diag(n), arr.ind = TRUE)
  unname(mean(mat_korelasi[idx_off_diagonal]))
}

#' Koreksi Pembatasan Rentang Thorndike Case III
#'
#' Menerapkan formula koreksi psikometri Thorndike Case III untuk mengoreksi
#' atenuasi matriks korelasi akibat seleksi tidak langsung pada variabel
#' komposit. Dengan `L = lambda_sq`, untuk pasangan (i, j):
#' `r_ij* = (r_ij + r_iZ * r_jZ * L) /
#'   sqrt((1 + r_iZ^2 * L) * (1 + r_jZ^2 * L))`.
#'
#' @param mat_r Matrix numerik (k x k). Matriks korelasi sampel mentah.
#' @param vec_r_iz Numeric vector panjang k. Korelasi tiap subtes ke komposit Z.
#' @param lambda_sq Numeric. Multiplikator seleksi: (S_Z / s_Z)^2 - 1.
#'   Harus lebih besar dari -1 (karena S_Z > 0).
#'
#' @return Matrix numerik (k x k). Matriks korelasi terkalibrasi/terkoreksi
#'   dengan diagonal tepat 1 dan nama baris/kolom dari `mat_r`.
#' @examples
#' mat_r <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
#' thorndike_case_3(mat_r, vec_r_iz = c(0.6, 0.7), lambda_sq = 0.2)
#' @export
thorndike_case_3 <- function(mat_r, vec_r_iz, lambda_sq) {
  stopifnot(
    "mat_r harus matriks persegi"         = is.matrix(mat_r) &&
      nrow(mat_r) == ncol(mat_r),
    "vec_r_iz harus numerik"              = is.numeric(vec_r_iz),
    "dimensi mat_r dan vec_r_iz sesuai"   = nrow(mat_r) == length(vec_r_iz),
    "lambda_sq harus berupa skalar angka" = is.numeric(lambda_sq) &&
      length(lambda_sq) == 1L,
    "lambda_sq harus lebih besar dari -1" = lambda_sq > -1
  )
  
  r_iz <- unname(vec_r_iz)
  
  # Versi vektorisasi dari formula per-pasangan: pembilang dan penyebut
  # dibentuk sebagai matriks (outer product) lalu dibagi elemen demi elemen.
  pembilang <- mat_r + lambda_sq * outer(r_iz, r_iz)
  faktor    <- sqrt(1 + lambda_sq * r_iz^2)
  mat_r_corr <- pembilang / outer(faktor, faktor)
  
  diag(mat_r_corr) <- 1
  dimnames(mat_r_corr) <- dimnames(mat_r)
  
  mat_r_corr
}

#' Selesaikan Simpangan Baku Populasi Komposit via Fixed-Point Iteration
#'
#' Menjalankan iterasi titik tetap swakonsisten untuk menemukan S_Z populasi
#' dan matriks korelasi terkoreksi hingga mencapai batas toleransi numerik.
#' Komposit didefinisikan sebagai rerata k subtes, sehingga
#' `S_Z^2 = sum_ij(sigma_i * sigma_j * R_ij) / k^2`.
#'
#' @param mat_r_raw Matrix numerik (k x k). Korelasi sampel mentah.
#' @param vec_r_iz Numeric vector panjang k. Korelasi subtes terhadap komposit.
#' @param vec_sigma_nat Numeric vector panjang k. Simpangan baku nasional tidak 
#'   resmi (belum terverifikasi).
#' @param s_sample_z Numeric. Simpangan baku komposit sampel.
#' @param s_init Numeric skalar. Tebakan awal S_Z (default: s_sample_z).
#' @param tol Numeric. Batas toleransi perubahan delta (default: 1e-12).
#'   Jangan diset mendekati epsilon mesin (~1e-14 untuk S_Z ~ 68): delta
#'   bisa berosilasi 1 ulp sehingga tidak pernah dinyatakan konvergen.
#' @param max_iter Integer. Batas siklus maksimum (default: 100).
#'
#' @return List berisi:
#'   \item{s_population_z}{Numeric, estimasi konvergen S_Z}
#'   \item{lambda_sq}{Numeric, nilai lambda^2 akhir}
#'   \item{mat_r_corrected}{Matrix, matriks korelasi populasi terkoreksi}
#'   \item{mean_r_corrected}{Numeric, rerata off-diagonal terkoreksi}
#'   \item{n_iterations}{Integer, jumlah siklus yang dijalankan}
#'   \item{converged}{Logical, TRUE bila delta <= tol tercapai}
#'   \item{final_delta}{Numeric, delta pada iterasi terakhir}
#'   \item{history}{Data frame, log jejak komputasi per iterasi}
#' @details Bila `max_iter` tercapai sebelum konvergen, fungsi memberi
#'   `warning()` dan `converged = FALSE`; hasil jangan dilaporkan sebagai
#'   konvergen tanpa memeriksa `converged`.
#' @examples
#' mat_r_raw <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
#' hasil <- solve_population_sd(
#'   mat_r_raw     = mat_r_raw,
#'   vec_r_iz      = c(0.6, 0.7),
#'   vec_sigma_nat = c(100, 100),
#'   s_sample_z    = 70
#' )
#' hasil$s_population_z
#' @export
solve_population_sd <- function(mat_r_raw,
                                vec_r_iz,
                                vec_sigma_nat,
                                s_sample_z,
                                s_init = s_sample_z,
                                tol = 1e-12,
                                max_iter = 100L) {
  k <- length(vec_sigma_nat)
  stopifnot(
    "mat_r_raw harus matriks persegi k x k" = is.matrix(mat_r_raw) &&
      nrow(mat_r_raw) == k && ncol(mat_r_raw) == k,
    "vec_r_iz harus numerik berpanjang k" = is.numeric(vec_r_iz) &&
      length(vec_r_iz) == k,
    "vec_sigma_nat harus numerik positif" = is.numeric(vec_sigma_nat) &&
      all(vec_sigma_nat > 0),
    "s_sample_z harus skalar positif" = is.numeric(s_sample_z) &&
      length(s_sample_z) == 1L && s_sample_z > 0,
    "s_init harus skalar positif" = is.numeric(s_init) &&
      length(s_init) == 1L && s_init > 0,
    "tol harus skalar positif" = is.numeric(tol) && length(tol) == 1L &&
      tol > 0,
    "max_iter harus skalar >= 1" = is.numeric(max_iter) &&
      length(max_iter) == 1L && max_iter >= 1
  )
  
  vec_sigma_nat <- unname(vec_sigma_nat)
  vec_r_iz      <- unname(vec_r_iz)
  s_sample_z    <- unname(s_sample_z)
  s_init        <- unname(s_init)
  
  k_sq      <- k^2
  outer_sig <- outer(vec_sigma_nat, vec_sigma_nat)
  
  s_curr      <- s_init
  log_history <- vector("list", max_iter)
  converged   <- FALSE
  final_iter  <- 0L
  delta       <- NA_real_
  
  for (iter in seq_len(max_iter)) {
    lambda_sq   <- (s_curr / s_sample_z)^2 - 1
    mat_r_corr  <- thorndike_case_3(mat_r_raw, vec_r_iz, lambda_sq)
    mean_r_corr <- compute_off_diagonal_mean(mat_r_corr)
    
    # Diagonal mat_r_corr tepat 1, sehingga jumlah seluruh sigma_i * sigma_j *
    # R_ij sudah mencakup suku varians sum(sigma_i^2).
    s_next <- sqrt(sum(outer_sig * mat_r_corr) / k_sq)
    delta  <- abs(s_next - s_curr)
    
    log_history[[iter]] <- tibble::tibble(
      iteration        = iter,
      sd_composite_in  = s_curr,
      lambda_sq        = lambda_sq,
      mean_r_corrected = mean_r_corr,
      sd_composite_out = s_next,
      delta            = delta
    )
    
    s_curr     <- s_next
    final_iter <- iter
    
    if (delta <= tol) {
      converged <- TRUE
      break
    }
  }
  
  if (!converged) {
    warning(
      sprintf(
        paste0(
          "solve_population_sd() tidak konvergen dalam %d iterasi ",
          "(delta terakhir = %.3e, tol = %.3e). ",
          "Hasil BELUM boleh dilaporkan sebagai konvergen."
        ),
        as.integer(max_iter), delta, tol
      ),
      call. = FALSE
    )
  }
  
  history_df <- dplyr::bind_rows(log_history[seq_len(final_iter)])
  lambda_sq_final <- (s_curr / s_sample_z)^2 - 1
  mat_r_final <- thorndike_case_3(mat_r_raw, vec_r_iz, lambda_sq_final)
  
  list(
    s_population_z   = unname(s_curr),
    lambda_sq        = unname(lambda_sq_final),
    mat_r_corrected  = mat_r_final,
    mean_r_corrected = unname(compute_off_diagonal_mean(mat_r_final)),
    n_iterations     = final_iter,
    converged        = converged,
    final_delta      = delta,
    history          = history_df
  )
}

#' Diagnostik Asumsi Seleksi Thorndike Case III
#'
#' Case III mengasumsikan sampel terseleksi hanya lewat komposit Z. Dalam
#' kondisi itu slope regresi X_i pada Z tidak berubah oleh seleksi, sehingga
#' SD populasi yang tersirat dari sampel adalah
#' `sd_implied = s_i * sqrt(1 + r_iZ^2 * lambda_sq)`.
#' Bila asumsi terpenuhi, `sd_implied` mendekati SD nasional tidak resmi. Rasio 
#' yang jauh dari 1 menandakan seleksi tidak (hanya) lewat komposit sehingga
#' koreksi Case III perlu dilaporkan sebagai aproksimasi/limitasi.
#'
#' @param subtests Character vector. Nama subtes (panjang k).
#' @param sd_sample Numeric vector panjang k. SD sampel tiap subtes.
#' @param vec_r_iz Numeric vector panjang k. Korelasi sampel subtes ke Z.
#' @param lambda_sq Numeric skalar. Multiplikator seleksi (lihat
#'   `solve_population_sd()`).
#' @param vec_sigma_nat Numeric vector panjang k. SD nasional tidak resmi (belum
#'   terverifikasi).
#'
#' @return Tibble berisi kolom `subtest`, `sd_sample`, `r_iz`,
#'   `sd_implied_population`, `sd_national`, `ratio` (implied / national), dan
#'   `diff` (implied - national).
#' @examples
#' check_selection_assumption(
#'   subtests      = c("a", "b"),
#'   sd_sample     = c(80, 90),
#'   vec_r_iz      = c(0.6, 0.7),
#'   lambda_sq     = 0.5,
#'   vec_sigma_nat = c(100, 100)
#' )
#' @export
check_selection_assumption <- function(subtests,
                                       sd_sample,
                                       vec_r_iz,
                                       lambda_sq,
                                       vec_sigma_nat) {
  k <- length(subtests)
  stopifnot(
    "subtests harus character" = is.character(subtests),
    "sd_sample harus numerik berpanjang k" = is.numeric(sd_sample) &&
      length(sd_sample) == k,
    "vec_r_iz harus numerik berpanjang k" = is.numeric(vec_r_iz) &&
      length(vec_r_iz) == k,
    "vec_sigma_nat harus numerik berpanjang k" = is.numeric(vec_sigma_nat) &&
      length(vec_sigma_nat) == k,
    "lambda_sq harus skalar > -1" = is.numeric(lambda_sq) &&
      length(lambda_sq) == 1L && lambda_sq > -1
  )
  
  sd_sample     <- unname(sd_sample)
  vec_r_iz      <- unname(vec_r_iz)
  vec_sigma_nat <- unname(vec_sigma_nat)
  sd_implied    <- sd_sample * sqrt(1 + vec_r_iz^2 * lambda_sq)
  
  tibble::tibble(
    subtest               = subtests,
    sd_sample             = sd_sample,
    r_iz                  = vec_r_iz,
    sd_implied_population = sd_implied,
    sd_national           = vec_sigma_nat,
    ratio                 = sd_implied / vec_sigma_nat,
    diff                  = sd_implied - vec_sigma_nat
  )
}

#' Jalankan Exploratory Factor Analysis (EFA) & Parallel Analysis
#'
#' Melakukan estimasi analisis faktor eksploratori dengan ekstraksi faktor
#' empiris dan pengujian batas retensi berbasis Horn's Parallel Analysis.
#'
#' @param df_data Data frame atau matrix skor subtes.
#' @param subtests Character vector. Daftar nama variabel subtes.
#' @param n_factors Integer. Jumlah faktor yang diekstraksi (default: 2L).
#'   Ditetapkan a priori oleh pemanggil; hasil Parallel Analysis
#'   (`parallel_res$nfact`) tidak dipakai otomatis untuk menentukannya.
#' @param rotate Character. Metode rotasi ("oblimin", "promax", "varimax").
#' @param fm Character. Metode ekstraksi faktor ("minres", "ml", "pa").
#' @param seed Integer skalar. Benih RNG untuk resampling Parallel Analysis
#'   (default: 2026L). Wajib eksplisit karena `psych::fa.parallel()`
#'   melakukan simulasi acak (`n.iter`); tanpa benih tetap, eigenvalue
#'   simulasi dan keputusan retensi faktor tidak akan identik antar-run.
#' @param n_iter_parallel Integer. Jumlah iterasi simulasi Parallel Analysis
#'   (default: 100L).
#'
#' @return List berisi:
#'   \item{parallel_res}{Objek hasil Horn's Parallel Analysis}
#'   \item{efa_res}{Objek psych::fa()}
#'   \item{loadings_df}{Tibble muatan faktor (satu kolom per faktor, bernama
#'     sesuai output psych dalam huruf kecil, mis. `mr1`, `mr2`) beserta
#'     komunalitas `h2`, uniqueness `u2`, dan kompleksitas `com`}
#'   \item{eigenvalues}{Tibble perbandingan eigenvalue empiris vs simulasi}
#' @examples
#' \dontrun{
#' data_clean <- readr::read_csv("data/processed/utbk_clean.csv")
#' hasil <- run_utbk_efa(data_clean, subtests = SUBTESTS, n_factors = 2L)
#' hasil$loadings_df
#' }
#' @export
run_utbk_efa <- function(df_data,
                         subtests,
                         n_factors = 2L,
                         rotate = "oblimin",
                         fm = "minres",
                         seed = 2026L,
                         n_iter_parallel = 100L) {
  stopifnot(
    "df_data harus data.frame atau matrix" = is.data.frame(df_data) ||
      is.matrix(df_data),
    "Seluruh subtests harus ada di data"  =
      all(subtests %in% colnames(df_data)),
    "n_factors harus integer >= 1"        = is.numeric(n_factors) &&
      length(n_factors) == 1L && n_factors >= 1 &&
      n_factors == round(n_factors),
    "seed harus skalar numerik"           = is.numeric(seed) &&
      length(seed) == 1L,
    "n_iter_parallel harus skalar >= 1"   = is.numeric(n_iter_parallel) &&
      length(n_iter_parallel) == 1L && n_iter_parallel >= 1
  )
  
  mat_data <- as.matrix(df_data[, subtests])
  
  # 1. Parallel Analysis
  # set.seed() wajib di sini: fa.parallel() melakukan resampling stokastik
  # (n.iter simulasi acak) sehingga tanpa benih tetap, hasil scree plot dan
  # keputusan retensi faktor tidak reproducible antar-run.
  set.seed(seed)
  pa_res <- psych::fa.parallel(
    mat_data,
    fm      = fm,
    fa      = "fa",
    n.iter  = as.integer(n_iter_parallel),
    plot    = FALSE,
    correct = 0.5
  )
  
  # 2. Fit EFA
  efa_fit <- psych::fa(
    mat_data,
    nfactors = n_factors,
    rotate   = rotate,
    fm       = fm
  )
  
  # 3. Format Muatan Faktor (semua faktor, nama kolom mengikuti output psych)
  loadings_mat <- unclass(efa_fit$loadings)
  colnames(loadings_mat) <- tolower(colnames(loadings_mat))
  loadings_df <- dplyr::bind_cols(
    tibble::tibble(subtest = rownames(loadings_mat)),
    tibble::as_tibble(loadings_mat),
    tibble::tibble(
      h2  = unname(efa_fit$communality),
      u2  = unname(efa_fit$uniquenesses),
      com = unname(efa_fit$complexity)
    )
  )
  
  # 4. Ringkasan Eigenvalues
  eigen_df <- tibble::tibble(
    factor_num  = seq_along(pa_res$fa.values),
    empiris_fa  = pa_res$fa.values,
    simulasi_fa = pa_res$fa.sim,
    selisih     = pa_res$fa.values - pa_res$fa.sim
  )
  
  list(
    parallel_res = pa_res,
    efa_res      = efa_fit,
    loadings_df  = loadings_df,
    eigenvalues  = eigen_df
  )
}

#' Estimasi Model CFA 1-Faktor dan 2-Faktor (Lavaan)
#'
#' Memodelkan struktur faktor konfirmatori untuk menguji apakah 8 subtes
#' merefleksikan model general (1-faktor g) atau terbagi ke dalam domain
#' spesifik Literasi vs. Numerasi/Logika (2-faktor oblique).
#'
#' @param df_data Data frame berisi skor subtes (kolom pu, ppu, pbm, pk,
#'   lbi_saintek, lbi_soshum, lbe, pm).
#' @param estimator Character. Metode estimasi (default: "MLM" robust).
#'
#' @return List berisi dua objek lavaan:
#'   \item{fit_1f}{Model 1-Faktor General (g)}
#'   \item{fit_2f}{Model 2-Faktor Domain (Literacy vs Numeracy/Logic)}
#' @details Struktur 2-faktor ditetapkan a priori. Bila struktur itu
#'   dipengaruhi hasil EFA pada sampel yang sama, indeks fit CFA bersifat
#'   optimistis dan sebaiknya dilaporkan demikian (atau gunakan split-sample).
#' @examples
#' \dontrun{
#' data_clean <- readr::read_csv("data/processed/utbk_clean.csv")
#' cfa_models <- fit_utbk_cfa_models(data_clean)
#' summary(cfa_models$fit_2f)
#' }
#' @export
fit_utbk_cfa_models <- function(df_data, estimator = "MLM") {
  kolom_wajib <- c(
    "pu", "ppu", "pbm", "pk", "lbi_saintek", "lbi_soshum", "lbe", "pm"
  )
  stopifnot(
    "df_data harus data.frame" = is.data.frame(df_data),
    "df_data kekurangan kolom subtes" = all(kolom_wajib %in% names(df_data)),
    "estimator harus character skalar" = is.character(estimator) &&
      length(estimator) == 1L
  )
  
  model_1f_syntax <- "
    # Model 1: General Intelligence Factor (g)
    g =~ pu + ppu + pbm + pk + lbi_saintek + lbi_soshum + lbe + pm
  "
  
  model_2f_syntax <- "
    # Model 2: Two-Factor Correlated Domain
    literacy =~ ppu + pbm + lbi_saintek + lbi_soshum + lbe
    numeracy =~ pu + pk + pm

    # Korelasi antarfaktor laten diestimasi bebas
    literacy ~~ numeracy
  "
  
  fit_1f <- lavaan::cfa(
    model          = model_1f_syntax,
    data           = df_data,
    estimator      = estimator,
    std.lv         = TRUE,
    auto.fix.first = FALSE
  )
  
  fit_2f <- lavaan::cfa(
    model          = model_2f_syntax,
    data           = df_data,
    estimator      = estimator,
    std.lv         = TRUE,
    auto.fix.first = FALSE
  )
  
  list(
    fit_1f = fit_1f,
    fit_2f = fit_2f
  )
}

# Ambil satu ukuran fit dari vektor fitMeasures(): pakai versi `primary`
# (mis. "chisq.scaled"), dan jatuh ke `fallback` (mis. "chisq") bila
# `primary` tidak ada. Catatan: indeks nama yang tidak ada pada vektor
# bernama mengembalikan NA (bukan NULL), sehingga operator %||% tidak bisa
# dipakai untuk fallback ini.
pick_fit_measure <- function(fm, primary, fallback) {
  nilai <- unname(fm[primary])
  if (length(nilai) == 0L || is.na(nilai)) {
    nilai <- unname(fm[fallback])
  }
  nilai
}

#' Ekstrak Indeks Goodness-of-Fit dan Uji Beda Model CFA
#'
#' Mengekstraksi metrik kecocokan model standar dan robust (CFI, TLI, RMSEA,
#' SRMR, AIC, BIC) serta menghitung selisih chi-square Satorra-Bentler.
#' Bila ukuran robust/scaled tidak tersedia (mis. estimator "ML"), fungsi
#' memakai versi standarnya.
#'
#' @param fit_1f Objek lavaan Model 1-Faktor.
#' @param fit_2f Objek lavaan Model 2-Faktor.
#'
#' @return Tibble komparasi fit model.
#' @examples
#' \dontrun{
#' cfa_models <- fit_utbk_cfa_models(data_clean)
#' extract_cfa_fit_indices(cfa_models$fit_1f, cfa_models$fit_2f)
#' }
#' @export
extract_cfa_fit_indices <- function(fit_1f, fit_2f) {
  stopifnot(
    "fit_1f harus objek lavaan" = inherits(fit_1f, "lavaan"),
    "fit_2f harus objek lavaan" = inherits(fit_2f, "lavaan")
  )
  
  get_metrics <- function(fit, model_name) {
    fm <- lavaan::fitMeasures(fit)
    tibble::tibble(
      model          = model_name,
      chisq_scaled   = pick_fit_measure(fm, "chisq.scaled", "chisq"),
      df_scaled      = as.integer(pick_fit_measure(fm, "df.scaled", "df")),
      pvalue_scaled  = pick_fit_measure(fm, "pvalue.scaled", "pvalue"),
      cfi_robust     = pick_fit_measure(fm, "cfi.robust", "cfi"),
      tli_robust     = pick_fit_measure(fm, "tli.robust", "tli"),
      rmsea_robust   = pick_fit_measure(fm, "rmsea.robust", "rmsea"),
      rmsea_ci_lower = pick_fit_measure(
        fm, "rmsea.ci.lower.robust", "rmsea.ci.lower"
      ),
      rmsea_ci_upper = pick_fit_measure(
        fm, "rmsea.ci.upper.robust", "rmsea.ci.upper"
      ),
      srmr           = unname(fm["srmr"]),
      aic            = unname(fm["aic"]),
      bic            = unname(fm["bic"])
    )
  }
  
  df_1f <- get_metrics(fit_1f, "Model 1: 1-Faktor (g-General)")
  df_2f <- get_metrics(fit_2f, "Model 2: 2-Faktor (Literasi vs Numerasi)")
  
  # Uji Beda Chi-Square Bersarang (LRT)
  lrt <- lavaan::lavTestLRT(fit_1f, fit_2f)
  
  delta_chisq <- unname(lrt$`Chisq diff`[2])
  delta_df    <- as.integer(unname(lrt$`Df diff`[2]))
  delta_p     <- unname(lrt$`Pr(>Chisq)`[2])
  
  df_comp <- dplyr::bind_rows(df_1f, df_2f) |>
    dplyr::mutate(
      delta_chisq = c(NA_real_, delta_chisq),
      delta_df    = c(NA_integer_, delta_df),
      delta_p     = c(NA_real_, delta_p),
      delta_cfi   = c(NA_real_, df_2f$cfi_robust - df_1f$cfi_robust),
      delta_aic   = c(NA_real_, df_2f$aic - df_1f$aic)
    )
  
  df_comp
}

#' Ekstrak Parameter Muatan Faktor Terstandarisasi CFA
#'
#' @param fit_model Objek lavaan hasil fitting CFA.
#'
#' @return Tibble berisi muatan faktor, SE, z, p-value, R2, dan uniqueness.
#' @examples
#' \dontrun{
#' cfa_models <- fit_utbk_cfa_models(data_clean)
#' extract_cfa_loadings(cfa_models$fit_2f)
#' }
#' @export
extract_cfa_loadings <- function(fit_model) {
  stopifnot(
    "fit_model harus objek lavaan" = inherits(fit_model, "lavaan")
  )
  
  std_sol <- lavaan::standardizedSolution(fit_model)
  r_sq    <- lavaan::lavInspect(fit_model, "r2")
  
  # Subsetting base R (bukan dplyr::filter) agar tidak memakai nama kolom
  # sebagai variabel global tak-terdefinisi (NSE) yang dikeluhkan lintr.
  std_load <- std_sol[std_sol$op == "=~", , drop = FALSE]
  r_sq_load <- unname(r_sq[std_load$rhs])
  
  tibble::tibble(
    latent_factor = std_load$lhs,
    subtest       = std_load$rhs,
    lambda_std    = std_load$est.std,
    se            = std_load$se,
    z_value       = std_load$z,
    p_value       = std_load$pvalue,
    r_squared     = r_sq_load,
    uniqueness    = 1 - r_sq_load
  )
}