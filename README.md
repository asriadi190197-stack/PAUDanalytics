# PAUDanalytics v0.2.0

R package + Shiny dashboard untuk analisis longitudinal hasil observasi PAUD.

## Fitur utama
- Observasi Awal, Tengah, dan Akhir Semester.
- Statistik longitudinal untuk enam aspek perkembangan.
- Profil perkembangan individual setiap anak.
- Deteksi aspek dan indikator meningkat, stagnan, atau menurun.
- Narasi perkembangan otomatis berbasis aturan.
- Rekomendasi tindak lanjut per aspek.
- Ekspor analisis kelas ke Excel.
- Ekspor laporan individual ke Word.

## Format data
Satu baris adalah satu anak pada satu periode observasi. Kolom `periode` menggunakan:
- Awal Semester
- Tengah Semester
- Akhir Semester

## Instalasi dari file ZIP source package
```r
install.packages("remotes")
remotes::install_local("PAUDanalytics_0.2.0.zip", dependencies = TRUE, force = TRUE)
library(PAUDanalytics)
paudshiny()
```

Setelah terinstal, penggunaan berikutnya cukup:
```r
library(PAUDanalytics)
paudshiny()
```
atau satu baris:
```r
PAUDanalytics::paudshiny()
```

## Catatan metodologis
Indikator pada aplikasi adalah contoh indikator operasional. Sesuaikan dengan TP/ATP, instrumen satuan pendidikan, dan pedoman asesmen yang digunakan. Status stagnan/menurun adalah sinyal analitik untuk tindak lanjut observasi, bukan diagnosis perkembangan.
