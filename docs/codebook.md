# Codebook: Dataset Replikasi UTBK-SNBT 2026

| Field | Isi |
|---|---|
| **Nama dataset** | utbk_clean.csv |
| **Jumlah observasi** | 1886 |
| **Jumlah variabel** | 10 |
| **Tanggal dihasilkan** | 2026-10-28 |
| **Versi R** | R version 4.6.1 (2026-06-24 ucrt) |

---

## Kamus Variabel

### `pu`

| Atribut | Nilai |
|---|---|
| **Label** | Penalaran Umum |
| **Tipe R** | `numeric` |
| **Skala ukur** | interval |
| **Satuan** | skor |
| **Missing** | `NA` (n = 0, 0%) |
| **Sumber** | Subtes 1 |
| **Statistik Sampel** | Mean = 633.258; Median = 628.620; SD = 76.845; Min = 406.100; Max = 839.420 |
| **Catatan** | Parameter nasional: Mean=500, SD=71.52 |

### `ppu`

| Atribut | Nilai |
|---|---|
| **Label** | Pengetahuan dan Pemahaman Umum |
| **Tipe R** | `numeric` |
| **Skala ukur** | interval |
| **Satuan** | skor |
| **Missing** | `NA` (n = 0, 0%) |
| **Sumber** | Subtes 2 |
| **Statistik Sampel** | Mean = 669.955; Median = 672.520; SD = 61.500; Min = 413.970; Max = 852.510 |
| **Catatan** | Parameter nasional: Mean=500, SD=100.00 |

### `pbm`

| Atribut | Nilai |
|---|---|
| **Label** | Pemahaman Bacaan dan Menulis |
| **Tipe R** | `numeric` |
| **Skala ukur** | interval |
| **Satuan** | skor |
| **Missing** | `NA` (n = 0, 0%) |
| **Sumber** | Subtes 3 |
| **Statistik Sampel** | Mean = 680.953; Median = 687.190; SD = 76.617; Min = 410.510; Max = 873.440 |
| **Catatan** | Parameter nasional: Mean=500, SD=100.00 |

### `pk`

| Atribut | Nilai |
|---|---|
| **Label** | Pengetahuan Kuantitatif |
| **Tipe R** | `numeric` |
| **Skala ukur** | interval |
| **Satuan** | skor |
| **Missing** | `NA` (n = 0, 0%) |
| **Sumber** | Subtes 4 |
| **Statistik Sampel** | Mean = 678.527; Median = 676.370; SD = 131.895; Min = 358.920; Max = 1173.550 |
| **Catatan** | Parameter nasional: Mean=500, SD=100.00 |

### `lbi_saintek`

| Atribut | Nilai |
|---|---|
| **Label** | Literasi Bahasa Indonesia Saintek |
| **Tipe R** | `numeric` |
| **Skala ukur** | interval |
| **Satuan** | skor |
| **Missing** | `NA` (n = 0, 0%) |
| **Sumber** | Subtes 5 |
| **Statistik Sampel** | Mean = 631.629; Median = 630.830; SD = 82.309; Min = 331.350; Max = 908.460 |
| **Catatan** | Parameter nasional: Mean=500, SD=100.00 |

### `lbi_soshum`

| Atribut | Nilai |
|---|---|
| **Label** | Literasi Bahasa Indonesia Soshum |
| **Tipe R** | `numeric` |
| **Skala ukur** | interval |
| **Satuan** | skor |
| **Missing** | `NA` (n = 0, 0%) |
| **Sumber** | Subtes 6 |
| **Statistik Sampel** | Mean = 634.693; Median = 634.855; SD = 70.108; Min = 356.650; Max = 862.570 |
| **Catatan** | Parameter nasional: Mean=500, SD=100.00 |

### `lbe`

| Atribut | Nilai |
|---|---|
| **Label** | Literasi Bahasa Inggris |
| **Tipe R** | `numeric` |
| **Skala ukur** | interval |
| **Satuan** | skor |
| **Missing** | `NA` (n = 0, 0%) |
| **Sumber** | Subtes 7 |
| **Statistik Sampel** | Mean = 680.435; Median = 684.845; SD = 86.502; Min = 331.820; Max = 882.080 |
| **Catatan** | Parameter nasional: Mean=500, SD=100.00 |

### `pm`

| Atribut | Nilai |
|---|---|
| **Label** | Penalaran Matematika |
| **Tipe R** | `numeric` |
| **Skala ukur** | interval |
| **Satuan** | skor |
| **Missing** | `NA` (n = 0, 0%) |
| **Sumber** | Subtes 8 |
| **Statistik Sampel** | Mean = 653.229; Median = 658.630; SD = 100.013; Min = 340.330; Max = 966.180 |
| **Catatan** | Parameter nasional: Mean=500, SD=100.00 |

### `mean_8`

| Atribut | Nilai |
|---|---|
| **Label** | Komposit Rerata 8 Subtes |
| **Tipe R** | `numeric` |
| **Skala ukur** | interval |
| **Satuan** | skor |
| **Missing** | `NA` (n = 0, 0%) |
| **Sumber** | Kalkulasi komposit |
| **Statistik Sampel** | Mean = 657.835; Median = 659.684; SD = 53.843; Min = 451.817; Max = 799.885 |
| **Catatan** | Skor komposit berbobot setara |

### `stage`

| Atribut | Nilai |
|---|---|
| **Label** | Tahap Penjaringan Data |
| **Tipe R** | `numeric` |
| **Skala ukur** | ordinal |
| **Satuan** | tahap |
| **Missing** | `NA` (n = 0, 0%) |
| **Sumber** | Metadata pengumpulan |
| **Statistik Sampel** | Mean = 11.978; Median = 13.000; SD = 5.836; Min = 1.000; Max = 19.000 |
| **Catatan** | Mengelompokkan asal utas media sosial |



---

## Riwayat Versi

| Versi | Tanggal | Versi R | N Observasi | N Variabel | Keterangan |
|---|---|---|---|---|---|
| 1.0 | 2026-09-18 | R version 4.6.1 (2026-06-24 ucrt) | 1886 | 10 | Pipeline otomatis (run_all.R) |
| 1.1 | 2026-09-19 | R version 4.6.1 (2026-06-24 ucrt) | 1886 | 10 | Pipeline otomatis (run_all.R) |
| 1.2 | 2026-09-26 | R version 4.6.1 (2026-06-24 ucrt) | 1886 | 10 | Pipeline otomatis (run_all.R) |
| 1.3 | 2026-10-06 | R version 4.6.1 (2026-06-24 ucrt) | 1886 | 10 | Pipeline otomatis (run_all.R) |
| 1.4 | 2026-10-06 | R version 4.6.1 (2026-06-24 ucrt) | 1886 | 10 | Pipeline otomatis (run_all.R) |
| 1.5 | 2026-10-09 | R version 4.6.1 (2026-06-24 ucrt) | 1886 | 10 | Pipeline otomatis (run_all.R) |
| 1.6 | 2026-10-22 | R version 4.6.1 (2026-06-24 ucrt) | 1886 | 10 | Pipeline otomatis (run_all.R) |
| 1.7 | 2026-10-23 | R version 4.6.1 (2026-06-24 ucrt) | 1886 | 10 | Pipeline otomatis (run_all.R) |
| 1.8 | 2026-10-23 | R version 4.6.1 (2026-06-24 ucrt) | 1886 | 10 | Pipeline otomatis (run_all.R) |
| 1.9 | 2026-10-24 | R version 4.6.1 (2026-06-24 ucrt) | 1886 | 10 | Pipeline otomatis (run_all.R) |
| 1.10 | 2026-10-26 | R version 4.6.1 (2026-06-24 ucrt) | 1886 | 10 | Pipeline otomatis (run_all.R) |
| 1.11 | 2026-10-27 | R version 4.6.1 (2026-06-24 ucrt) | 1886 | 10 | Pipeline otomatis (run_all.R) |
| 1.12 | 2026-10-28 | R version 4.6.1 (2026-06-24 ucrt) | 1886 | 10 | Pipeline otomatis (run_all.R) |
| 1.13 | 2026-10-28 | R version 4.6.1 (2026-06-24 ucrt) | 1886 | 10 | Pipeline otomatis (run_all.R) |
| 1.14 | 2026-10-28 | R version 4.6.1 (2026-06-24 ucrt) | 1886 | 10 | Pipeline otomatis (run_all.R) |

