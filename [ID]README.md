# Replikasi Korelasi Antarsubtes, Struktur Faktor & Bias Seleksi UTBK-SNBT 2026

> Need this in English? Click [here](./README.md)

[![R \>= 4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://www.r-project.org/) [![Code License: MIT](https://img.shields.io/badge/Code_License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) [![Data License: CC BY 4.0](https://img.shields.io/badge/Data_License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/) [![Reproducibility: renv](https://img.shields.io/badge/reproducibility-renv-green.svg)](https://rstudio.github.io/renv/) [![R CI](https://github.com/placeholder/utbk-correlation-study/actions/workflows/r-ci.yaml/badge.svg)](https://github.com/placeholder/utbk-correlation-study/actions/workflows/r-ci.yaml)

Paket replikasi dan dataset terbuka berbasis R untuk estimasi korelasi antarsubtes, pemodelan struktur faktor laten (EFA & CFA), koreksi bias restriksi rentang (*range restriction*), dan pemodelan persentil nasional pada UTBK-SNBT 2026.

## Daftar Isi

- [Latar Belakang](#latar-belakang)
- [Fitur Utama](#fitur-utama)
- [Prasyarat Sistem](#prasyarat-sistem)
- [Instalasi](#instalasi)
- [Struktur Proyek](#struktur-proyek)
- [Penggunaan](#penggunaan)
- [Data](#data)
- [Pengujian (Testing)](#pengujian-testing)
- [Reproduksibilitas](#reproduksibilitas)
- [Dokumentasi](#dokumentasi)
- [Kontribusi](#kontribusi)
- [Lisensi](#lisensi)
- [Sitasi](#sitasi)
- [Kontak](#kontak)

## Latar Belakang

Struktur korelasi antarsubtes dan dimensionalitas faktor laten kognitif UTBK-SNBT 2026 tidak tersedia bagi publik, dan statistik deskriptif per subtes yang beredar bersumber dari tabel internal yang disebut berasal dari Panitia SNPMB, tidak resmi, dan belum terverifikasi. Repositori ini menyediakan analisis yang dapat direproduksi penuh untuk:

1.  Mengestimasi matriks korelasi Pearson antardelapan subtes kognitif ($N = 1.886$).
2.  Memodelkan dimensionalitas laten melalui *Exploratory Factor Analysis* (EFA; *Parallel Analysis*) dan *Confirmatory Factor Analysis* (CFA; model 1-faktor $g$ vs. model 2-faktor domain spesifik).
3.  Mengkuantifikasi bias seleksi melalui penyesuaian **Thorndike Case III** berbasis pembatasan rentang multivariat.
4.  Menyelesaikan estimasi simpangan baku komposit populasi ($S_Z$) via *fixed-point iteration* kovarians heterogen penuh.
5.  Memodelkan peringkat dan persentil komposit nasional terkalibrasi.

## Fitur Utama

- **Pipeline Terstandarisasi:** Mengikuti konvensi Tidyverse dan standar Project TIER untuk keterbukaan sains.
- **Pemodelan Analisis Faktor:** Estimasi CFA berbasis *Robust Maximum Likelihood* (`MLM`) untuk menguji konstruk general $g$ vs. domain spesifik (Literasi Verbal vs. Numerasi/Logika).
- **Komputasi Presisi Tinggi:** Implementasi algoritma tervektorisasi untuk formula Lawley-Aitken / Thorndike Case III.
- **Keluaran Otomatis:** Tabel publikasi LaTeX (`booktabs`), visualisasi heatmap, *scree plot*, dan laporan audit komputasi PDF presisi penuh.
- **Codebook Otomatis:** Generator kamus variabel yang selalu sinkron dengan data aktual.

## Prasyarat Sistem

- **Versi R:** `R >= 4.1.0`
- **RStudio** (sangat direkomendasikan)
- **Distribusi LaTeX:** TinyTeX, TeX Live, atau MiKTeX (untuk kompilasi PDF)

## Instalasi

Klon repositori ini dan pulihkan lingkungan dependensi paket:

``` bash
git clone https://github.com/placeholder/utbk-correlation-study.git
cd utbk-correlation-study
```

Di konsol R:

``` r
install.packages("renv")
renv::restore()
```

## Struktur Proyek

```text
utbk-correlation-study/
├── README.md                      # Dokumentasi Bahasa Inggris
├── [ID]README.md                  # Dokumentasi Bahasa Indonesia
├── LICENSE                        # Lisensi data & kode terbuka
├── run_all.R                      # Skrip master orkestrasi
├── data/
│   ├── raw/                       # Data mentah (read-only)
│   │   ├── utbk_score_samples.csv
│   │   └── utbk_national_descriptive_statistics.csv
│   ├── processed/                 # Data bersih siap analisis
│   │   └── utbk_clean.csv
│   └── metadata/                  # Kamus data & label
│       ├── dictionary.csv
│       └── stage_labels.csv
├── R/                             # Modul fungsi terisolasi
│   ├── utils_stats.R
│   ├── utils_tables.R
│   └── utils_figures.R
├── scripts/                       # Skrip eksekusi sekuensial
│   ├── 01_import_and_validate.R
│   ├── 02_descriptive_analysis.R
│   ├── 03_correlation_models.R
│   ├── 04_factor_analysis.R
│   ├── 05_stage_saturation.R
│   ├── 06_ranking_simulation.R
│   ├── 07_generate_codebook.R
│   ├── 08_export_paper_tables.R   # Dijalankan terakhir; membuat tabel LaTeX, termasuk dari 09-12
│   ├── 09_sensitivity_lbi.R       # Sensitivitas LBI: EFA 3-faktor, kovarians residual CFA, Case III dengan 8/7/6 subtes
│   ├── 10_bootstrap_uncertainty.R # Interval bootstrap dan sensitivitas terhadap SD nasional tidak resmi
│   ├── 11_nonnormality_sensitivity.R # Diagnostik non-normalitas; sensitivitas persentil (Johnson SB + copula Gaussian)
│   └── 12_slodr_sensitivity.R     # Skenario SLODR (korelasi bergantung tingkat skor); membaca keluaran 11
├── reports/
│   └── exact_computational_report.Rmd
├── tests/
│   └── testthat/
│       ├── test_descriptive.R
│       ├── test_factor_analysis.R
│       └── test_thorndike.R
├── docs/
│   └── codebook.md
├── output/
│   ├── tables/
│   ├── figures/
│   └── reports/
└── paper/                         # Naskah LaTeX (digabung 2026-09;
                                    # lihat main.tex, preamble.tex, sections/)
```

## Penggunaan

Untuk menjalankan seluruh alur analisis, pengujian unit, serta mengekspor seluruh tabel dan laporan PDF dalam satu perintah:

``` r
source("run_all.R")
```

> **Waktu eksekusi dan urutan.** `run_all.R` menjalankan skrip sensitivitas `09`-`12` sebelum pengujian dan ekspor tabel (`08`). Skrip-skrip ini berat secara simulasi (bootstrap, copula, kalibrasi bersarang) dan memakan waktu beberapa menit secara keseluruhan. `12` membutuhkan keluaran `11`, jadi pertahankan urutan itu bila dijalankan satu per satu.

## Data

Analisis memanfaatkan dua dataset utama pada folder `data/raw/`:

- `utbk_score_samples.csv`: Data skor sampel sukarela ($N = 1.886$) dari 19 tahapan utas.
- `utbk_national_descriptive_statistics.csv`: Parameter deskriptif nasional tidak resmi dan belum terverifikasi, diambil dari tabel internal yang disebut berasal dari Panitia SNPMB dan beredar di media sosial. Verifikasinya baru parsial (triangulasi pada skor maksimum PK dan PM); lihat bagian Metode dan Keterbatasan pada naskah. Jumlah peserta yang hadir ($N = 846.518$, dari 871.496 pendaftar) bukan bagian tabel ini; angka itu berasal dari paparan resmi konferensi pers SNPMB (25 Mei 2026) dan didefinisikan di `R/constants.R`.

Definisi variabel dan aturan penskalaan terdokumentasi lengkap pada [`docs/codebook.md`](./docs/codebook.md).

## Pengujian (Testing)

Pengujian unit dibangun menggunakan kerangka kerja `testthat`. Jalankan seluruh pengujian dengan:

``` r
testthat::test_dir("tests/testthat")
```

## Reproduksibilitas

- Seluruh versi paket dikunci melalui `renv.lock`.
- Algoritma numerik menggunakan toleransi presisi tinggi (`tol = 1e-15`).
- Rincian `sessionInfo()` dicetak otomatis pada laporan komputasi.

## Dokumentasi

Dokumentasi variabel, skala ukur, dan operasionalisasi psikometri tersedia pada [`docs/codebook.md`](./docs/codebook.md).

## Kontribusi

Kontribusi, perbaikan *bug*, atau saran pengembangan sangat terbuka. Silakan buat *issue* atau *pull request*.

## Lisensi

Proyek ini memakai **dual license**, sebagaimana dirinci pada [`LICENSE`](./LICENSE):

- **Kode** (`R/`, `scripts/`, `tests/`, dan berkas mandiri `.R`/`.Rmd`): [Lisensi MIT](https://opensource.org/licenses/MIT).
- **Data, dokumentasi & naskah** (`data/`, `docs/`, `output/`, dan berkas `.csv`/`.tex`/`.pdf`): [Creative Commons Attribution 4.0 International License (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/).

## Sitasi

``` bibtex
@article{placeholder2026utbk,
  author = {Saputra, Galuh},
  title  = {How Strongly Correlated Are the Subtests in the 2026 Indonesian University Entrance Exam (UTBK-SNBT)? Crowdsourced Estimates and Implications for National Ranking},
  year   = {2026},
  url    = {[https://github.com/placeholder/utbk-correlation-study](https://github.com/placeholder/utbk-correlation-study)}
}
```

## Kontak

- **Penulis:** Galuh Saputra
- **Email:** `galmei.id@gmail.com`