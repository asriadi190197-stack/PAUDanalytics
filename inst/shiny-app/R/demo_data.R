make_demo_data <- function() {
  set.seed(20260912)
  children <- data.frame(
    nama_sekolah = rep("PAUD Contoh Harapan Bangsa", 8),
    npsn = rep("12345678", 8),
    tahun_ajaran = rep("2026/2027", 8),
    semester = rep("Ganjil", 8),
    id_anak = sprintf("A%03d", 1:8),
    nama_anak = c("Aisyah", "Bima", "Citra", "Damar", "Elina", "Faris", "Gita", "Hana"),
    kelompok = rep("B", 8),
    usia_bulan = c(66, 64, 68, 63, 65, 67, 62, 69),
    stringsAsFactors = FALSE
  )

  out <- tidyr::crossing(
    children,
    periode = factor(period_order, levels = period_order)
  ) |>
    dplyr::arrange(id_anak, periode)

  for (k in indicator_meta$kode) {
    base <- sample(1:3, nrow(children), replace = TRUE)
    values <- unlist(lapply(base, function(b) {
      middle <- b + sample(c(0, 0, 1), 1)
      ending <- b + sample(c(0, 1, 1), 1)
      pmin(4, pmax(1, c(b, middle, ending)))
    }))
    out[[k]] <- values
  }

  out$periode <- as.character(out$periode)
  out
}
