# =============================================================================
# Judul       : Utilitas Visualisasi Matriks Korelasi & Analisis Faktor
# Deskripsi   : Fungsi plotting heatmap korelasi dan scree plot parallel analysis.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-15
# =============================================================================

#' Plot Heatmap Matriks Korelasi
#'
#' @param mat_korelasi Matrix korelasi numerik persegi berlabel baris dan kolom.
#' @param judul Character. Judul plot.
#' @param format_desimal Integer. Jumlah digit desimal teks anotasi (default: 3).
#'
#' @return Objek ggplot2.
#' @export
plot_correlation_heatmap <- function(mat_korelasi,
                                     judul = "Matriks Korelasi",
                                     format_desimal = 3L) {
  stopifnot(
    "Input harus matriks persegi" = is.matrix(mat_korelasi) &&
      nrow(mat_korelasi) == ncol(mat_korelasi)
  )
  
  lab_subtes <- c(
    pu = "PU", ppu = "PPU", pbm = "PBM", pk = "PK",
    lbi_saintek = "LBI Saintek", lbi_soshum = "LBI Soshum",
    lbe = "LBE", pm = "PM"
  )
  ganti_label <- function(x) ifelse(x %in% names(lab_subtes), unname(lab_subtes[x]), x)
  rata_off_diag <- compute_off_diagonal_mean(mat_korelasi)
  rownames(mat_korelasi) <- ganti_label(rownames(mat_korelasi))
  colnames(mat_korelasi) <- ganti_label(colnames(mat_korelasi))
  
  df_long <- as.data.frame(mat_korelasi) |>
    tibble::rownames_to_column(var = "var_y") |>
    tidyr::pivot_longer(
      cols      = -"var_y",
      names_to  = "var_x",
      values_to = "korelasi"
    ) |>
    dplyr::mutate(
      var_x = factor(var_x, levels = colnames(mat_korelasi)),
      var_y = factor(var_y, levels = rev(rownames(mat_korelasi))),
      label_teks = formatC(
        korelasi, format = "f", digits = format_desimal, decimal.mark = ","
      )
    )
  
  ggplot2::ggplot(
    data = df_long,
    mapping = ggplot2::aes(x = var_x, y = var_y, fill = korelasi)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = label_teks,
        color = abs(korelasi) > 0.65
      ),
      size = 3.5
    ) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "white", "FALSE" = "black"),
      guide  = "none"
    ) +
    ggplot2::scale_fill_gradient2(
      low      = "#2166AC",
      mid      = "#F7F7F7",
      high     = "#B2182B",
      midpoint = 0,
      limits   = c(-1, 1),
      name     = "Korelasi (r)",
      labels   = function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")
    ) +
    ggplot2::labs(
      title    = judul,
      subtitle = paste0(
        "Rerata off-diagonal = ",
        formatC(rata_off_diag, format = "f", digits = 4, decimal.mark = ",")
      ),
      x        = NULL,
      y        = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(
        angle = 45, hjust = 1, vjust = 1, face = "bold"
      ),
      axis.text.y      = ggplot2::element_text(face = "bold"),
      panel.grid       = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face = "bold", size = 13),
      legend.position  = "right"
    )
}

#' Plot Horn's Parallel Analysis & Scree Plot
#'
#' @param parallel_res Objek output dari psych::fa.parallel().
#' @param judul Character. Judul plot.
#'
#' @return Objek ggplot2.
#' @export
plot_scree_parallel <- function(parallel_res,
                                judul = "Horn's Parallel Analysis Scree Plot") {
  stopifnot(
    "parallel_res harus berisi fa.values" = !is.null(parallel_res$fa.values)
  )
  
  k <- length(parallel_res$fa.values)
  df_scree <- tibble::tibble(
    factor_num = rep(seq_len(k), 2),
    eigenvalue = c(parallel_res$fa.values, parallel_res$fa.sim),
    tipe       = rep(c("Data Empiris", "Simulasi Data Acak"), each = k)
  )
  
  ggplot2::ggplot(
    data    = df_scree,
    mapping = ggplot2::aes(
      x        = factor_num,
      y        = eigenvalue,
      color    = tipe,
      linetype = tipe,
      shape    = tipe
    )
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted", color = "grey50") +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_x_continuous(breaks = seq_len(k)) +
    ggplot2::scale_y_continuous(
      labels = function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")
    ) +
    ggplot2::scale_color_manual(
      values = c("Data Empiris" = "#B2182B", "Simulasi Data Acak" = "#2166AC")
    ) +
    ggplot2::labs(
      title    = judul,
      subtitle = "Faktor dipertahankan bila nilai eigen data empiris melebihi simulasi data acak",
      x        = "Nomor Faktor",
      y        = "Nilai Eigen",
      color    = "Kategori",
      linetype = "Kategori",
      shape    = "Kategori"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 13),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}