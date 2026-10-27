# =============================================================================
# Judul       : Unit Test Kontrak Dictionary vs Data Aktual
# Deskripsi   : Memvalidasi data/metadata/dictionary.csv terhadap
#               data/processed/utbk_clean.csv sesuai checklist QA (Bagian 13
#               standar codebook): variabel tak terdokumentasi, entri
#               dictionary yatim, duplikat nama, konvensi snake_case, tipe R
#               aktual, dan rentang nilai data.
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-17
# =============================================================================

library(testthat)
library(readr)
library(here)

df <- readr::read_csv(
  here::here("data", "processed", "utbk_clean.csv"),
  show_col_types = FALSE
)
dict <- readr::read_csv(
  here::here("data", "metadata", "dictionary.csv"),
  col_types      = readr::cols(.default = "c"),
  show_col_types = FALSE
)

test_that("Semua kolom data terdokumentasi di dictionary.csv", {
  variabel_tak_terdokumentasi <- setdiff(names(df), dict$var_name)
  expect_equal(
    variabel_tak_terdokumentasi, character(0),
    info = "Ada kolom di data yang tidak punya entri di dictionary.csv"
  )
})

test_that("Tidak ada entri dictionary.csv yang yatim (tanpa kolom data)", {
  entri_yatim <- setdiff(dict$var_name, names(df))
  expect_equal(
    entri_yatim, character(0),
    info = "Ada var_name di dictionary.csv yang tidak ada di data aktual"
  )
})

test_that("Tidak ada nama variabel duplikat di dictionary.csv", {
  expect_equal(anyDuplicated(dict$var_name), 0L)
})

test_that("Semua var_name mengikuti konvensi snake_case", {
  pola_snake_case <- "^[a-z][a-z0-9_]*$"
  expect_true(
    all(grepl(pola_snake_case, dict$var_name)),
    info = paste(
      "Variabel yang melanggar snake_case:",
      paste(dict$var_name[!grepl(pola_snake_case, dict$var_name)],
            collapse = ", ")
    )
  )
})

test_that("var_type_r di dictionary cocok dengan tipe R aktual data", {
  tipe_aktual <- vapply(df, function(x) class(x)[1], character(1))
  
  for (v in dict$var_name) {
    expect_equal(
      unname(tipe_aktual[[v]]),
      dict$var_type_r[dict$var_name == v],
      info = paste("Variabel:", v)
    )
  }
})

test_that("Nilai data berada dalam rentang min_value/max_value dictionary", {
  for (i in seq_len(nrow(dict))) {
    row <- dict[i, ]
    v   <- row$var_name
    
    if (!is.na(row$min_value) && !is.na(row$max_value) && is.numeric(df[[v]])) {
      min_dict <- as.numeric(row$min_value)
      max_dict <- as.numeric(row$max_value)
      
      expect_gte(
        min(df[[v]], na.rm = TRUE), min_dict,
        label = paste("min(", v, ")")
      )
      expect_lte(
        max(df[[v]], na.rm = TRUE), max_dict,
        label = paste("max(", v, ")")
      )
    }
  }
})