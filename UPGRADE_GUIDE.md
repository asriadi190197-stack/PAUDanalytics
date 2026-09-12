# Upgrade PAUDanalytics ke v0.4.2

Versi 0.4.2 memperbarui Beranda:

- grafik garis dihapus dari Beranda;
- ditambahkan donut chart status perkembangan (Meningkat/Stagnan/Menurun);
- ditambahkan horizontal bar chart capaian akhir enam aspek;
- grafik garis longitudinal tetap tersedia di menu **Perkembangan Kelas**.

## Cara update

1. Salin seluruh isi ZIP update ke root repository `PAUDanalytics` dan pilih Replace/Merge.
2. Di GitHub Desktop: commit dengan pesan `Redesign home dashboard v0.4.2` lalu Push origin.
3. Di RStudio jalankan:

```r
remotes::install_github("asriadi190197-stack/PAUDanalytics", force = TRUE, upgrade = "never")
PAUDanalytics::paudshiny()
```
