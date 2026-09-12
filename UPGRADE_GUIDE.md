# Upgrade ke PAUDanalytics 0.4.1

1. Salin seluruh isi ZIP pembaruan ke root repository PAUDanalytics lokal Anda dan pilih Replace/Merge.
2. Di GitHub Desktop, isi Summary: `Improve visuals and report card v0.4.1`.
3. Klik Commit to main, lalu Push origin.
4. Di RStudio jalankan:

```r
remotes::install_github("asriadi190197-stack/PAUDanalytics", force = TRUE, upgrade = "never")
PAUDanalytics::paudshiny()
```

Uji terutama menu Profil Anak, CP, Perubahan Indikator, dan Ekspor Laporan.
