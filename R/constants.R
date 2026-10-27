# =============================================================================
# Judul       : Konstanta Bersama Proyek
# Deskripsi   : Satu sumber kebenaran untuk nilai yang dipakai berulang di
#               seluruh skrip, pengujian, dan laporan — mencegah drift ketika
#               ada penambahan/pergantian subtes di masa depan (sebelumnya
#               daftar ini di-hardcode ulang identik di >10 lokasi terpisah).
# Penulis     : Galuh Saputra
# Tanggal     : 2026-09-19
# =============================================================================

# Daftar Nama Kolom 8 Subtes UTBK-SNBT 2026 -----------------------------------
# Konstanta ini tidak pernah berubah selama satu eksekusi pipeline, karenanya
# ditulis UPPER_SNAKE_CASE (Rule 2.3 Standarisasi_Kode_R.md). Semua
# skrip, pengujian, dan laporan yang memerlukan daftar subtes wajib
# source() file ini, bukan mendefinisikan ulang literalnya sendiri.
SUBTESTS <- c(
  "pu", "ppu", "pbm", "pk",
  "lbi_saintek", "lbi_soshum", "lbe", "pm"
)

# Turunan dari SUBTESTS --------------------------------------------------------
# Jumlah subtes (k) dan jumlah pasangan unik korelasi off-diagonal (k(k-1)/2).
# Dipakai sebagai pengganti angka ajaib 8, 28, dan 64 (= k^2).
N_SUBTESTS     <- length(SUBTESTS)
N_UNIQUE_PAIRS <- as.integer(choose(N_SUBTESTS, 2L))

# Parameter Data --------------------------------------------------------------
# Ukuran sampel yang diharapkan: dipakai sebagai pemeriksaan integritas
# di 01_import_and_validate.R dan test, bukan untuk caption (caption
# membaca N langsung dari data agar tidak basi).
N_SAMPLE_EXPECTED <- 1886L

# Jumlah peserta UTBK-SNBT 2026 yang HADIR (846.518), dari 871.496 pendaftar.
# Sumber: paparan konferensi pers hasil SNBT 2026, Panitia SNPMB, 25 Mei 2026
# (dokumen resmi; berbeda dari statistik deskriptif per subtes yang tidak
# resmi). Tidak tersedia di utbk_national_descriptive_statistics.csv, sehingga
# dicatat di sini sebagai satu-satunya lokasi definisi.
N_POPULATION <- 846518L

# Rerata skor terskala nasional tiap subtes (skala SNPMB, identik utk kedelapan
# subtes: hanya SD yang berbeda antarsubtes, lihat
# utbk_national_descriptive_statistics.csv).
MU_POPULATION <- 500.0

# Parameter Solver Fixed-Point Thorndike Case III -----------------------------
# tol = 1e-12 (satuan skor) sengaja berada ~2 orde di atas resolusi double
# untuk S_Z ~ 68 (ulp ~ 1.4e-14). Toleransi 1e-15 di bawah resolusi tersebut
# hanya "berhasil" bila delta kebetulan tepat 0, dan bisa berosilasi 1 ulp.
TOL_FIXED_POINT      <- 1e-12
MAX_ITER_FIXED_POINT <- 100L

# Benih RNG -------------------------------------------------------------------
# Dipakai untuk Horn's Parallel Analysis (psych::fa.parallel() stokastik).
SEED_PARALLEL_ANALYSIS <- 2026L