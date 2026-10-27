# =============================================================================
# Judul       : Utilitas Format Tabel Publikasi
# Deskripsi   : Format tabel LaTeX booktabs/APA dan konversi data frame.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-19
# =============================================================================

#' Konversi P-Value ke Notasi Bintang Signifikansi
#'
#' Menerjemahkan nilai p menjadi notasi bintang APA standar berdasarkan
#' ambang batas konvensional. Dipakai agar tabel LaTeX publikasi selalu
#' menampilkan signifikansi yang diverifikasi dari data, bukan notasi tetap
#' yang diasumsikan berlaku untuk semua kasus.
#'
#' @param p Numeric vector. Nilai p (boleh mengandung `NA`).
#'
#' @return Character vector kode LaTeX bintang ("", "$^{*}$", "$^{**}$",
#'   atau "$^{***}$").
#' @examples
#' p_to_stars(c(0.2, 0.03, 0.005, 0.0001, NA))
#' @export
p_to_stars <- function(p) {
  dplyr::case_when(
    is.na(p)  ~ "",
    p < 0.001 ~ "$^{***}$",
    p < 0.01  ~ "$^{**}$",
    p < 0.05  ~ "$^{*}$",
    TRUE      ~ ""
  )
}

#' Escape Karakter Khusus LaTeX pada Teks Biasa
#'
#' Mengubah karakter yang bermakna khusus di LaTeX (ampersand, persen, dolar,
#' pagar, garis bawah, kurung kurawal, tilde, caret, dan backslash) menjadi
#' bentuk aman agar teks yang berasal dari data (nama subtes, label faktor,
#' nama model) tidak merusak kompilasi tabel. Contoh kasus nyata: label
#' "Numerasi & Logika" tanpa escape akan menambah satu kolom tabular.
#'
#' @param x Character vector (atau factor) yang akan di-escape.
#'
#' @return Character vector dengan panjang sama.
#' @details Hanya untuk teks *polos*. Jangan dipakai pada string yang
#'   memang sengaja berisi perintah LaTeX (mis. `caption` atau `note` yang
#'   memuat rumus dalam tanda dolar atau perintah textit).
#' @examples
#' escape_latex(c("Numerasi & Logika", "cfa_model_fit.csv", "50%"))
#' @export
escape_latex <- function(x) {
  stopifnot(
    "x harus character atau factor" = is.character(x) || is.factor(x)
  )
  x <- as.character(x)
  
  # Backslash diganti penanda sementara dahulu agar backslash hasil escape
  # di langkah berikutnya tidak ikut ter-escape ulang.
  x <- gsub("\\\\", "\001", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  gsub("\001", "\\\\textbackslash{}", x)
}

#' Format Angka ke Notasi Desimal Indonesia
#'
#' Membulatkan lalu menampilkan angka dengan koma desimal dan jumlah digit
#' tetap. Nilai `NA` ditampilkan sebagai tanda hubung ("-").
#'
#' @param x Numeric vector.
#' @param digits Integer. Jumlah digit desimal (default: 3L).
#'
#' @return Character vector dengan panjang sama seperti `x` (tanpa padding
#'   lebar bersama antar-elemen).
#' @examples
#' format_number_id(c(0.5, NA, 1234.56), digits = 2L)
#' @export
format_number_id <- function(x, digits = 3L) {
  stopifnot(
    "x harus numerik" = is.numeric(x) ||
      (is.logical(x) && all(is.na(x))),
    "digits harus skalar >= 0" = is.numeric(digits) && length(digits) == 1L &&
      digits >= 0
  )
  
  vapply(
    x,
    function(nilai) {
      if (is.na(nilai)) {
        "-"
      } else {
        format(round(nilai, digits), decimal.mark = ",", nsmall = digits)
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# Pemeriksaan argumen teks skalar (dipakai internal oleh fungsi format_*).
is_scalar_text <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

# Bagian pembuka tabel LaTeX yang sama untuk seluruh tabel publikasi.
build_latex_table_header <- function(label_tabel, caption) {
  c(
    "\\begin{table}[H]",
    "    \\centering",
    sprintf("    \\caption{%s}", caption),
    sprintf("    \\label{%s}", label_tabel),
    "    \\begin{spacing}{1.0}"
  )
}

# Bagian penutup tabel LaTeX (catatan kaki). `resized` = TRUE bila tabular
# dibungkus \resizebox.
build_latex_table_footer <- function(note, resized) {
  c(
    "        \\bottomrule",
    if (resized) "    \\end{tabular}%" else "    \\end{tabular}",
    if (resized) "    }",
    "    \\end{spacing}",
    "    \\vspace{2mm}",
    "    \\begin{flushleft}",
    sprintf("    \\footnotesize\\textit{Catatan.} %s", note),
    "    \\end{flushleft}",
    "\\end{table}"
  )
}

#' Format Matriks Korelasi ke Tabel LaTeX APA
#'
#' @param mat Matrix korelasi persegi dengan nama kolom. Nama kolom di-escape
#'   otomatis via `escape_latex()`.
#' @param label_tabel Character. Label LaTeX.
#' @param caption Character. Judul caption tabel (LaTeX mentah, tidak
#'   di-escape).
#' @param note Character. Catatan kaki tabel (LaTeX mentah, tidak di-escape).
#'
#' @return Character string kode LaTeX.
#' @examples
#' dn  <- list(c("A", "B"), c("A", "B"))
#' mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, dimnames = dn)
#' cat(format_latex_correlation_table(
#'   mat, "tab:contoh", "Contoh Tabel Korelasi", "N = 100."
#' ))
#' @export
format_latex_correlation_table <- function(mat, label_tabel, caption, note) {
  stopifnot(
    "mat harus matriks numerik persegi" = is.matrix(mat) &&
      is.numeric(mat) && nrow(mat) == ncol(mat),
    "mat harus memiliki nama kolom" = !is.null(colnames(mat)),
    "label_tabel harus character skalar" = is_scalar_text(label_tabel),
    "caption harus character skalar" = is_scalar_text(caption),
    "note harus character skalar" = is_scalar_text(note)
  )
  
  k <- nrow(mat)
  col_names <- escape_latex(colnames(mat))
  
  lines_header <- c(
    build_latex_table_header(label_tabel, caption),
    "    \\resizebox{\\textwidth}{!}{%",
    sprintf("    \\begin{tabular}{l%s}", paste(rep("c", k), collapse = "")),
    "        \\toprule",
    sprintf("        & %s \\\\", paste(col_names, collapse = " & ")),
    "        \\midrule"
  )
  
  lines_body <- vapply(
    seq_len(k),
    function(i) {
      row_vals    <- format_number_id(mat[i, ], digits = 3L)
      row_vals[i] <- "1,000"
      sprintf(
        "        %-12s & %s \\\\",
        col_names[i],
        paste(row_vals, collapse = " & ")
      )
    },
    character(1)
  )
  
  paste(
    c(lines_header, lines_body, build_latex_table_footer(note, TRUE)),
    collapse = "\n"
  )
}

#' Format Tabel Komparasi Fit Indeks CFA ke LaTeX APA
#'
#' @param df_fit Data frame hasil extract_cfa_fit_indices().
#' @param label_tabel Character. Label LaTeX.
#' @param caption Character. Judul caption tabel (LaTeX mentah).
#' @param note Character. Catatan kaki tabel (LaTeX mentah).
#'
#' @return Character string kode LaTeX.
#' @examples
#' \dontrun{
#' df_fit <- extract_cfa_fit_indices(cfa_models$fit_1f, cfa_models$fit_2f)
#' cat(format_latex_cfa_fit_table(
#'   df_fit, "tab:cfa_fit", "Perbandingan Fit Model CFA", "N = 1.886."
#' ))
#' }
#' @export
format_latex_cfa_fit_table <- function(df_fit, label_tabel, caption, note) {
  kolom_wajib <- c(
    "model", "chisq_scaled", "df_scaled", "cfi_robust", "tli_robust",
    "rmsea_robust", "rmsea_ci_lower", "rmsea_ci_upper", "srmr", "aic",
    "delta_chisq", "delta_df", "delta_p"
  )
  stopifnot(
    "df_fit harus data.frame" = is.data.frame(df_fit),
    "df_fit kekurangan kolom wajib" = all(kolom_wajib %in% names(df_fit)),
    "label_tabel harus character skalar" = is_scalar_text(label_tabel),
    "caption harus character skalar" = is_scalar_text(caption),
    "note harus character skalar" = is_scalar_text(note)
  )
  
  lines_header <- c(
    build_latex_table_header(label_tabel, caption),
    "    \\resizebox{\\textwidth}{!}{%",
    "    \\begin{tabular}{lcccccccc}",
    "        \\toprule",
    paste0(
      "        Model & $\\chi^2_{\\text{SB}}$ & $df$ & CFI & TLI & RMSEA ",
      "[90\\% CI] & SRMR & AIC & $\\Delta\\chi^2$ ($df$) \\\\"
    ),
    "        \\midrule"
  )
  
  lines_body <- vapply(
    seq_len(nrow(df_fit)),
    function(i) {
      row <- df_fit[i, ]
      rmsea_str <- sprintf(
        "%s [%s; %s]",
        format_number_id(row$rmsea_robust, 3L),
        format_number_id(row$rmsea_ci_lower, 3L),
        format_number_id(row$rmsea_ci_upper, 3L)
      )
      delta_str <- if (is.na(row$delta_chisq)) {
        "-"
      } else {
        sprintf(
          "%s (%d)%s",
          format_number_id(row$delta_chisq, 2L),
          as.integer(row$delta_df),
          p_to_stars(row$delta_p)
        )
      }
      
      sprintf(
        "        %-32s & %s & %d & %s & %s & %s & %s & %s & %s \\\\",
        escape_latex(row$model),
        format_number_id(row$chisq_scaled, 2L),
        as.integer(row$df_scaled),
        format_number_id(row$cfi_robust, 3L),
        format_number_id(row$tli_robust, 3L),
        rmsea_str,
        format_number_id(row$srmr, 3L),
        format_number_id(row$aic, 1L),
        delta_str
      )
    },
    character(1)
  )
  
  paste(
    c(lines_header, lines_body, build_latex_table_footer(note, TRUE)),
    collapse = "\n"
  )
}

#' Format Tabel Muatan Faktor Terstandarisasi CFA ke LaTeX APA
#'
#' @param df_loadings Data frame muatan faktor CFA. Wajib memiliki kolom
#'   `p_value` (lihat `extract_cfa_loadings()`) — dipakai untuk menghitung
#'   bintang signifikansi per-baris via `p_to_stars()`, bukan diasumsikan.
#'   Kolom `latent_factor` dan `subtest` di-escape otomatis via
#'   `escape_latex()`.
#' @param label_tabel Character. Label LaTeX.
#' @param caption Character. Judul caption tabel (LaTeX mentah).
#' @param note Character. Catatan kaki tabel (LaTeX mentah).
#'
#' @return Character string kode LaTeX.
#' @examples
#' \dontrun{
#' df_loadings <- extract_cfa_loadings(cfa_models$fit_2f)
#' cat(format_latex_factor_loadings_table(
#'   df_loadings, "tab:loadings", "Muatan Faktor CFA", "N = 1.886."
#' ))
#' }
#' @export
format_latex_factor_loadings_table <- function(df_loadings,
                                               label_tabel,
                                               caption,
                                               note) {
  kolom_wajib <- c(
    "latent_factor", "subtest", "lambda_std", "se", "z_value", "p_value",
    "r_squared"
  )
  stopifnot(
    "df_loadings harus data.frame" = is.data.frame(df_loadings),
    "df_loadings kekurangan kolom wajib" =
      all(kolom_wajib %in% names(df_loadings)),
    "label_tabel harus character skalar" = is_scalar_text(label_tabel),
    "caption harus character skalar" = is_scalar_text(caption),
    "note harus character skalar" = is_scalar_text(note)
  )
  
  lines_header <- c(
    build_latex_table_header(label_tabel, caption),
    "    \\begin{tabular}{llcccc}",
    "        \\toprule",
    paste0(
      "        Dimensi Laten & Subtes & $\\lambda$ (Std.) & $SE$ & $z$ & ",
      "$R^2$ ($h^2$) \\\\"
    ),
    "        \\midrule"
  )
  
  lines_body <- vapply(
    seq_len(nrow(df_loadings)),
    function(i) {
      row <- df_loadings[i, ]
      sprintf(
        "        %-15s & %-12s & %s%s & %s & %s & %s \\\\",
        escape_latex(row$latent_factor),
        escape_latex(row$subtest),
        format_number_id(row$lambda_std, 3L),
        p_to_stars(row$p_value),
        format_number_id(row$se, 3L),
        format_number_id(row$z_value, 2L),
        format_number_id(row$r_squared, 3L)
      )
    },
    character(1)
  )
  
  paste(
    c(lines_header, lines_body, build_latex_table_footer(note, FALSE)),
    collapse = "\n"
  )
}

#' Format Angka Bulat dengan Pemisah Ribuan Indonesia
#'
#' Dipindah ke sini (dari scripts/08_export_paper_tables.R) supaya bisa
#' dipakai bersama oleh seluruh formatter tabel LaTeX di file ini, bukan
#' didefinisikan ulang secara lokal.
#'
#' @param n Integer skalar atau vector.
#'
#' @return Character vector, mis. "1.886".
#' @examples
#' format_int_id(1886L)
#' @export
format_int_id <- function(n) {
  format(n, big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

#' Format Tabel Deskriptif Komparasi (Sampel vs Nasional) ke LaTeX APA
#'
#' @param df_desc Data frame hasil scripts/02_descriptive_analysis.R
#'   (`tabel_deskriptif_komparasi.csv`). Kolom `subject_name` diasumsikan
#'   SUDAH dipetakan ke label tampilan (lihat `map_labels()` di skrip
#'   pemanggil) sebelum dipanggil di sini.
#' @param label_tabel Character. Label LaTeX.
#' @param caption Character. Judul caption tabel (LaTeX mentah).
#' @param note Character. Catatan kaki tabel (LaTeX mentah).
#'
#' @return Character string kode LaTeX.
#' @examples
#' \dontrun{
#' cat(format_latex_descriptive_table(
#'   desc_df, "tab:deskriptif", "Statistik Deskriptif", "N = 1.886."
#' ))
#' }
#' @export
format_latex_descriptive_table <- function(df_desc, label_tabel, caption,
                                           note) {
  kolom_wajib <- c(
    "subject_name", "mean_sample", "sd_sample", "mean_nat", "sd_nat",
    "mean_diff", "sd_diff"
  )
  stopifnot(
    "df_desc harus data.frame" = is.data.frame(df_desc),
    "df_desc kekurangan kolom wajib" = all(kolom_wajib %in% names(df_desc)),
    "label_tabel harus character skalar" = is_scalar_text(label_tabel),
    "caption harus character skalar" = is_scalar_text(caption),
    "note harus character skalar" = is_scalar_text(note)
  )
  
  lines_header <- c(
    build_latex_table_header(label_tabel, caption),
    "    \\begin{tabular}{lcccccc}",
    "        \\toprule",
    paste0(
      "        Subtes & $M$ (Sampel) & $SD$ (Sampel) & $M$ (Nasional) & ",
      "$SD$ (Nasional) & $\\Delta M$ & $\\Delta SD$ \\\\"
    ),
    "        \\midrule"
  )
  
  lines_body <- vapply(
    seq_len(nrow(df_desc)),
    function(i) {
      row <- df_desc[i, ]
      sprintf(
        "        %-15s & %s & %s & %s & %s & %s & %s \\\\",
        escape_latex(row$subject_name),
        format_number_id(row$mean_sample, 2L),
        format_number_id(row$sd_sample, 2L),
        format_number_id(row$mean_nat, 2L),
        format_number_id(row$sd_nat, 2L),
        format_number_id(row$mean_diff, 2L),
        format_number_id(row$sd_diff, 2L)
      )
    },
    character(1)
  )
  
  paste(
    c(lines_header, lines_body, build_latex_table_footer(note, FALSE)),
    collapse = "\n"
  )
}

#' Format Tabel Saturasi Korelasi per Tahap Pengumpulan Data ke LaTeX APA
#'
#' @param df_stage Data frame hasil scripts/05_stage_saturation.R
#'   (`tabel_saturasi_tahap.csv`). Kolom `stage_name`/`category` diasumsikan
#'   sudah dalam bentuk teks siap tampil (bukan kode internal).
#' @param label_tabel Character. Label LaTeX.
#' @param caption Character. Judul caption tabel (LaTeX mentah).
#' @param note Character. Catatan kaki tabel (LaTeX mentah).
#'
#' @return Character string kode LaTeX.
#' @examples
#' \dontrun{
#' cat(format_latex_stage_saturation_table(
#'   stage_df, "tab:saturasi", "Saturasi Korelasi per Tahap",
#'   "N kumulatif meningkat monoton."
#' ))
#' }
#' @export
format_latex_stage_saturation_table <- function(df_stage, label_tabel,
                                                caption, note) {
  kolom_wajib <- c(
    "stage", "stage_name", "category", "n_cumulative", "r_off_diagonal"
  )
  stopifnot(
    "df_stage harus data.frame" = is.data.frame(df_stage),
    "df_stage kekurangan kolom wajib" = all(kolom_wajib %in% names(df_stage)),
    "label_tabel harus character skalar" = is_scalar_text(label_tabel),
    "caption harus character skalar" = is_scalar_text(caption),
    "note harus character skalar" = is_scalar_text(note)
  )
  
  lines_header <- c(
    build_latex_table_header(label_tabel, caption),
    "    \\resizebox{\\textwidth}{!}{%",
    "    \\begin{tabular}{cllcc}",
    "        \\toprule",
    paste0(
      "        Tahap & Nama Tahap & Kategori & $N$ Kumulatif & ",
      "$\\bar{r}$ \\textit{off-diagonal} \\\\"
    ),
    "        \\midrule"
  )
  
  lines_body <- vapply(
    seq_len(nrow(df_stage)),
    function(i) {
      row <- df_stage[i, ]
      sprintf(
        "        %d & %-30s & %-15s & %s & %s \\\\",
        as.integer(row$stage),
        escape_latex(row$stage_name),
        escape_latex(row$category),
        format_int_id(as.integer(row$n_cumulative)),
        format_number_id(row$r_off_diagonal, 3L)
      )
    },
    character(1)
  )
  
  paste(
    c(lines_header, lines_body, build_latex_table_footer(note, TRUE)),
    collapse = "\n"
  )
}

#' Format Tabel Diagnostik Pelanggaran Asumsi Case III ke LaTeX APA
#'
#' @param df_diag Data frame diagnostik seleksi (`tabel_diagnostik_seleksi.csv`).
#'   Kolom `subtest` diasumsikan SUDAH dipetakan ke label tampilan sebelum
#'   dipanggil di sini.
#' @param label_tabel Character. Label LaTeX.
#' @param caption Character. Judul caption tabel (LaTeX mentah).
#' @param note Character. Catatan kaki tabel (LaTeX mentah).
#'
#' @return Character string kode LaTeX.
#' @examples
#' \dontrun{
#' cat(format_latex_diagnostic_table(
#'   diag_df, "tab:diagnostik", "Diagnostik Asumsi Case III",
#'   "Ambang deviasi 15%."
#' ))
#' }
#' @export
format_latex_diagnostic_table <- function(df_diag, label_tabel, caption,
                                          note) {
  kolom_wajib <- c(
    "subtest", "sd_sample", "r_iz", "sd_implied_population", "sd_national",
    "ratio"
  )
  stopifnot(
    "df_diag harus data.frame" = is.data.frame(df_diag),
    "df_diag kekurangan kolom wajib" = all(kolom_wajib %in% names(df_diag)),
    "label_tabel harus character skalar" = is_scalar_text(label_tabel),
    "caption harus character skalar" = is_scalar_text(caption),
    "note harus character skalar" = is_scalar_text(note)
  )
  
  lines_header <- c(
    build_latex_table_header(label_tabel, caption),
    "    \\begin{tabular}{lccccc}",
    "        \\toprule",
    paste0(
      "        Subtes & $SD$ Sampel & $r_{iz}$ & $SD$ Tersirat Populasi & ",
      "$SD$ Nasional & Rasio \\\\"
    ),
    "        \\midrule"
  )
  
  lines_body <- vapply(
    seq_len(nrow(df_diag)),
    function(i) {
      row <- df_diag[i, ]
      sprintf(
        "        %-15s & %s & %s & %s & %s & %s \\\\",
        escape_latex(row$subtest),
        format_number_id(row$sd_sample, 2L),
        format_number_id(row$r_iz, 3L),
        format_number_id(row$sd_implied_population, 2L),
        format_number_id(row$sd_national, 2L),
        format_number_id(row$ratio, 3L)
      )
    },
    character(1)
  )
  
  paste(
    c(lines_header, lines_body, build_latex_table_footer(note, FALSE)),
    collapse = "\n"
  )
}

#' Format Tabel Matriks Translasi Persentil Standar ke LaTeX APA
#'
#' Formatter generik dipakai bersama oleh tabel benchmark per subtes DAN
#' per skenario korelasi komposit (lihat scripts/06_ranking_simulation.R):
#' baris = titik benchmark standar (M-3SD..M+3SD), kolom nilai = skor
#' mentah per subtes ATAU per skenario tergantung pemanggil. Persentil
#' (`top_pct`) by design SELALU identik lintas kolom nilai pada baris yang
#' sama — lihat pemeriksaan invarian di scripts/06.
#'
#' @param df_wide Data frame lebar (hasil `tidyr::pivot_wider()` dari CSV
#'   long). Wajib memiliki kolom `benchmark_label`, `top_pct`,
#'   `rank_nasional`, ditambah >= 1 kolom nilai numerik yang namanya sudah
#'   berupa label tampilan (nama subtes atau nama skenario) — nama kolom
#'   inilah yang jadi header tabel, di-escape otomatis via `escape_latex()`.
#' @param label_tabel Character. Label LaTeX.
#' @param caption Character. Judul caption tabel (LaTeX mentah).
#' @param note Character. Catatan kaki tabel (LaTeX mentah).
#'
#' @return Character string kode LaTeX.
#' @examples
#' \dontrun{
#' cat(format_latex_benchmark_table(
#'   df_wide_subtes, "tab:benchmark_subtes",
#'   "Translasi Persentil Standar per Subtes", "M = 500."
#' ))
#' }
#' @export
format_latex_benchmark_table <- function(df_wide, label_tabel, caption,
                                         note) {
  kolom_wajib <- c("benchmark_label", "top_pct", "rank_nasional")
  stopifnot(
    "df_wide harus data.frame" = is.data.frame(df_wide),
    "df_wide kekurangan kolom wajib" = all(kolom_wajib %in% names(df_wide)),
    "df_wide harus punya >= 1 kolom nilai" =
      ncol(df_wide) > length(kolom_wajib),
    "label_tabel harus character skalar" = is_scalar_text(label_tabel),
    "caption harus character skalar" = is_scalar_text(caption),
    "note harus character skalar" = is_scalar_text(note)
  )
  
  kolom_nilai   <- setdiff(names(df_wide), kolom_wajib)
  header_nilai  <- escape_latex(kolom_nilai)
  k             <- length(kolom_nilai)
  
  lines_header <- c(
    build_latex_table_header(label_tabel, caption),
    "    \\resizebox{\\textwidth}{!}{%",
    sprintf(
      "    \\begin{tabular}{lcc%s}",
      paste(rep("c", k), collapse = "")
    ),
    "        \\toprule",
    sprintf(
      "        Titik \\textit{Benchmark} & Persentil (\\%%) & Peringkat Nasional & %s \\\\",
      paste(header_nilai, collapse = " & ")
    ),
    "        \\midrule"
  )
  
  lines_body <- vapply(
    seq_len(nrow(df_wide)),
    function(i) {
      row <- df_wide[i, ]
      nilai_str <- vapply(
        kolom_nilai,
        function(nm) format_number_id(row[[nm]], 2L),
        character(1)
      )
      sprintf(
        "        %-10s & %s & %s & %s \\\\",
        escape_latex(row$benchmark_label),
        format_number_id(row$top_pct, 2L),
        format_int_id(as.integer(row$rank_nasional)),
        paste(nilai_str, collapse = " & ")
      )
    },
    character(1)
  )
  
  paste(
    c(lines_header, lines_body, build_latex_table_footer(note, TRUE)),
    collapse = "\n"
  )
}