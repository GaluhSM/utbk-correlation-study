# Folder `images/`

Letakkan seluruh berkas gambar/figur (PNG, JPG, atau PDF) untuk naskah
di folder ini, misalnya:

- `kerangka-berpikir.png` — diagram kerangka berpikir (dirujuk di
  `sections/03-tinjauan-pustaka.tex`)
- `hasil-analisis.png` — visualisasi hasil analisis (dirujuk di
  `sections/05-hasil.tex`)

Rujuk gambar di naskah menggunakan:

```latex
\begin{figure}[htbp]
    \centering
    \includegraphics[width=0.7\textwidth]{images/nama-berkas.png}
    \caption{Deskripsi singkat gambar}
    \label{fig:label-unik}
\end{figure}
```

Berkas ini hanya penanda folder dan dapat dihapus setelah Anda
menambahkan gambar sungguhan.
