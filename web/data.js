window.UTBK_DATA = {
  "meta": {
    "versi": "1.0",
    "n_populasi": 846518,
    "mu": 500,
    "sumber_n": "Paparan konferensi pers hasil SNBT 2026, Panitia SNPMB, 25 Mei 2026",
    "judul_paper": "Seberapa Kuat Subtes UTBK-SNBT 2026 Saling Berkorelasi? Estimasi dari Data Crowdsourced dan Implikasinya bagi Estimasi Peringkat Nasional",
    "doi": "",
    "repo": ""
  },
  "subtes": [
    {
      "kode": "pu",
      "singkat": "PU",
      "label": "Penalaran Umum",
      "sd": 71.52,
      "min": 306,
      "q1": 448.92,
      "median": 494.6,
      "q3": 543.77,
      "maks": 894.43
    },
    {
      "kode": "ppu",
      "singkat": "PPU",
      "label": "Pengetahuan dan Pemahaman Umum",
      "sd": 100,
      "min": 231.92,
      "q1": 427.36,
      "median": 495.09,
      "q3": 571.98,
      "maks": 935.37
    },
    {
      "kode": "pbm",
      "singkat": "PBM",
      "label": "Pemahaman Bacaan dan Menulis",
      "sd": 100,
      "min": 229.8,
      "q1": 427.8,
      "median": 495.11,
      "q3": 566.78,
      "maks": 940.77
    },
    {
      "kode": "pk",
      "singkat": "PK",
      "label": "Pengetahuan Kuantitatif",
      "sd": 100,
      "min": 357.1,
      "q1": 424.46,
      "median": 474.94,
      "q3": 560.9,
      "maks": 1173.55
    },
    {
      "kode": "lbi_saintek",
      "singkat": "LBI Saintek",
      "label": "Literasi Bahasa Indonesia Saintek",
      "sd": 100,
      "min": 246.09,
      "q1": 426.91,
      "median": 503.76,
      "q3": 571.6,
      "maks": 953.68
    },
    {
      "kode": "lbi_soshum",
      "singkat": "LBI Soshum",
      "label": "Literasi Bahasa Indonesia Soshum",
      "sd": 100,
      "min": 211.44,
      "q1": 429.08,
      "median": 503.65,
      "q3": 572.52,
      "maks": 916.66
    },
    {
      "kode": "lbe",
      "singkat": "LBE",
      "label": "Literasi Bahasa Inggris",
      "sd": 100,
      "min": 317.17,
      "q1": 423.3,
      "median": 483.71,
      "q3": 560.42,
      "maks": 890.73
    },
    {
      "kode": "pm",
      "singkat": "PM",
      "label": "Penalaran Matematika",
      "sd": 100,
      "min": 266.9,
      "q1": 421.62,
      "median": 489.1,
      "q3": 575.46,
      "maks": 997.7
    }
  ],
  "skenario": [
    {
      "kode": "mentah",
      "nama": "Korelasi sampel mentah",
      "jenis": "data",
      "r": 0.300380571821856,
      "sd": 60.1237599223154
    },
    {
      "kode": "hip",
      "nama": "Hipotetis (sensitivitas)",
      "jenis": "hipotetis",
      "r": 0.35,
      "sd": 63.3886709120802
    },
    {
      "kode": "hip",
      "nama": "Hipotetis (sensitivitas)",
      "jenis": "hipotetis",
      "r": 0.4,
      "sd": 66.5167166958803
    },
    {
      "kode": "thorndike",
      "nama": "Estimasi Thorndike Case III",
      "jenis": "data",
      "r": 0.424598730237187,
      "sd": 67.7166172742716
    },
    {
      "kode": "hip",
      "nama": "Hipotetis (sensitivitas)",
      "jenis": "hipotetis",
      "r": 0.45,
      "sd": 69.5041264962016
    },
    {
      "kode": "hip",
      "nama": "Hipotetis (sensitivitas)",
      "jenis": "hipotetis",
      "r": 0.5,
      "sd": 72.3683190353348
    },
    {
      "kode": "hip",
      "nama": "Hipotetis (sensitivitas)",
      "jenis": "hipotetis",
      "r": 0.55,
      "sd": 75.1233891674224
    }
  ],
  "bootstrap": {
    "s_z": {
      "est": 67.7166172742716,
      "lo": 66.6289919237471,
      "hi": 68.7930655516537
    },
    "r": {
      "est": 0.424598730237195,
      "lo": 0.406825136281314,
      "hi": 0.44237581653419
    }
  },
  "sens_sd": [
    {
      "label": "SD nasional apa adanya (utama)",
      "s_z": 67.7166172742716,
      "r": 0.424598730237195
    },
    {
      "label": "Semua SD x 0,90",
      "s_z": 53.7864835577232,
      "r": 0.29983542772386
    },
    {
      "label": "Semua SD x 0,95",
      "s_z": 60.9336848879189,
      "r": 0.366629595418246
    },
    {
      "label": "Semua SD x 1,05",
      "s_z": 74.2242463333857,
      "r": 0.475123183089544
    },
    {
      "label": "Semua SD x 1,10",
      "s_z": 80.5172266336718,
      "r": 0.519375287391301
    },
    {
      "label": "SD PU = 100 (SD lain tetap)",
      "s_z": 72.9807131824969,
      "r": 0.465849656757246
    }
  ],
  "ekor": [
    {
      "b": 0,
      "k": 1,
      "rasio": 1.01284596224438
    },
    {
      "b": 0,
      "k": 2,
      "rasio": 1.46226258126512
    },
    {
      "b": 0,
      "k": 3,
      "rasio": 2.81255811750605
    },
    {
      "b": 0.02,
      "k": 1,
      "rasio": 1.02354000876742
    },
    {
      "b": 0.02,
      "k": 2,
      "rasio": 1.32995565632701
    },
    {
      "b": 0.02,
      "k": 3,
      "rasio": 1.90631682766872
    },
    {
      "b": 0.04,
      "k": 1,
      "rasio": 1.03238518280709
    },
    {
      "b": 0.04,
      "k": 2,
      "rasio": 1.19383922967417
    },
    {
      "b": 0.04,
      "k": 3,
      "rasio": 1.19268267845077
    }
  ]
};
