# Dashboard Eksplorasi Nilai Akhir Siswa SMA (Student Performance)

Proyek UTS mata kuliah **Eksplorasi dan Visualisasi Data Multivariat**
(Program Studi Statistika, Universitas AKPRIND Indonesia, T.A. 2026/2027).

Aplikasi interaktif dibuat dengan **R Shiny** untuk mengeksplorasi faktor-faktor yang berkaitan
dengan nilai akhir siswa SMA (G3), mencakup analisis univariat, bivariat, multivariat, dan regresi linear berganda.

## Latar belakang dan tujuan

Prestasi akademik dipengaruhi banyak faktor, seperti kebiasaan belajar, riwayat kegagalan, latar belakang
keluarga, dan motivasi melanjutkan pendidikan. Tujuan analisis:

1. Mendeskripsikan karakteristik tiap variabel.
2. Mengeksplorasi hubungan nilai akhir dengan variabel lain.
3. Membangun model regresi linear berganda untuk menjelaskan nilai akhir (G3).

## Sumber data

Cortez, P. (2008). *Student Performance* [Dataset]. UCI Machine Learning Repository.
https://doi.org/10.24432/C5TG7T (lisensi CC BY 4.0).

- File yang dipakai: `student-por` (mata pelajaran Bahasa Portugis)
- Jumlah observasi: 649
- Jumlah variabel: 33 (variabel numerik, ordinal, dan kategorik)
- Data hilang: tidak ada

Definisi lengkap setiap variabel tersedia di tab **Ringkasan Data** pada aplikasi.

## Model regresi

- Respons (Y): `G3`
- Prediktor numerik: `studytime`, `failures`, `absences`, `Medu`
- Prediktor kategorik: `school`, `sex`, `higher`
- `G1` dan `G2` tidak dimasukkan karena sangat berkorelasi dengan `G3`

## Struktur repository

```
.
├── app.R               # aplikasi R Shiny
├── student-por.xlsx    # data
└── README.md
```

## Cara menjalankan

1. Pasang R dan RStudio.
2. Pasang paket yang dibutuhkan:

```r
install.packages(c("shiny", "bslib", "ggplot2", "readxl", "DT"))
```

3. Taruh `app.R` dan `student-por.xlsx` di folder yang sama.
4. Buka `app.R` di RStudio lalu klik **Run App**, atau jalankan:

```r
shiny::runApp()
```

## Isi dashboard

| Tab | Isi |
|---|---|
| Ringkasan | Latar belakang, sumber, pengolahan data, definisi variabel, tabel data |
| Univariat | Histogram, bar chart, statistik deskriptif |
| Bivariat | Boxplot dan bar chart proporsi |
| Multivariat | Scatter berwarna per kelompok, heatmap korelasi |
| Regresi | Alasan pemilihan variabel, persamaan, uji, koefisien, uji asumsi klasik (normalitas, multikolinearitas, homoskedastisitas, autokorelasi), visualisasi, interpretasi, keterbatasan |
| Kesimpulan | Kesimpulan utama dan rekomendasi |

Filter di sidebar (sekolah, jenis kelamin, buang G3 = 0) berlaku di semua tab, termasuk model regresi.

## Catatan pengolahan data

- Tidak ada data hilang.
- Terdapat 15 siswa dengan G3 = 0 (kemungkinan tidak mengikuti ujian). Data dipertahankan pada model utama,
  dan tersedia opsi filter untuk membandingkan hasil tanpa nilai tersebut.
- Variabel ordinal seperti `studytime` dan `Medu` diperlakukan sebagai numerik pada regresi.
