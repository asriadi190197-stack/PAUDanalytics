category_code <- function(x, bb = 1.49, mb = 2.49, bsh = 3.49) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x <= bb ~ "BB",
    x <= mb ~ "MB",
    x <= bsh ~ "BSH",
    TRUE ~ "BSB"
  )
}

category_long <- function(x) {
  dplyr::recode(
    x,
    "BB" = "Belum Berkembang",
    "MB" = "Mulai Berkembang",
    "BSH" = "Berkembang Sesuai Harapan",
    "BSB" = "Berkembang Sangat Baik",
    .default = x
  )
}

focus_text <- function(aspect) {
  switch(
    aspect,
    "Nilai Agama dan Moral" = "teladan, rutinitas bermakna, cerita reflektif, kepedulian terhadap sesama dan lingkungan",
    "Nilai Pancasila" = "berbagi peran, aturan kelas, gotong royong, identitas diri, kepedulian, dan penghargaan terhadap keberagaman",
    "Fisik Motorik" = "permainan gerak kasar, motorik halus, kemandirian merawat diri, kebiasaan sehat, dan keselamatan",
    "Kognitif" = "pemecahan masalah, pola, bilangan, klasifikasi, eksperimen sederhana, dan eksplorasi sebab-akibat",
    "Bahasa" = "percakapan dua arah, membaca nyaring, bercerita, bermain peran, menyimak, dan kegiatan pramembaca",
    "Sosial-Emosional" = "pengenalan emosi, strategi regulasi diri, kerja sama, berbagi, negosiasi, dan penyelesaian konflik sederhana",
    "aktivitas bermain yang sesuai tahap perkembangan"
  )
}

recommendation_text <- function(aspect, status, final_cat) {
  focus <- focus_text(aspect)
  if (is.na(status)) {
    return("Observasi belum cukup untuk menghasilkan rekomendasi longitudinal.")
  }
  if (status == "Menurun") {
    return(paste0(
      "Terlihat penurunan pada aspek ini. Lakukan observasi ulang pada beberapa konteks sebelum menyimpulkan perubahan perkembangan. Prioritaskan stimulasi melalui ",
      focus,
      ", dengan dukungan individual dan dokumentasi anekdot."
    ))
  }
  if (status == "Stagnan") {
    return(paste0(
      "Perkembangan relatif stagnan. Variasikan konteks, tingkat bantuan, dan tantangan melalui ",
      focus,
      ". Amati apakah kemampuan muncul secara konsisten tanpa bantuan."
    ))
  }
  if (status == "Meningkat") {
    return(paste0(
      "Terjadi kemajuan. Pertahankan strategi yang efektif melalui ",
      focus,
      ", lalu tingkatkan tantangan secara bertahap agar kemampuan semakin mandiri dan konsisten."
    ))
  }
  if (!is.na(final_cat) && final_cat == "BSB") {
    return(paste0(
      "Capaian sudah sangat baik. Berikan pengayaan dan pilihan kegiatan yang lebih kompleks melalui ",
      focus,
      "."
    ))
  }
  paste0("Pertahankan stimulasi perkembangan melalui ", focus, ".")
}

make_narrative <- function(child, aspect_change) {
  if (nrow(aspect_change) == 0) {
    return("Data observasi longitudinal belum memadai untuk membuat narasi.")
  }

  child_name <- unique(child$nama_anak)[1]
  final_mean <- child |>
    dplyr::filter(periode == "Akhir Semester") |>
    dplyr::summarise(m = mean(skor_aspek, na.rm = TRUE)) |>
    dplyr::pull(m)

  final_cat <- category_code(final_mean)
  increased <- aspect_change |>
    dplyr::filter(status == "Meningkat") |>
    dplyr::arrange(dplyr::desc(delta)) |>
    dplyr::pull(aspek)
  stagnant <- aspect_change |>
    dplyr::filter(status == "Stagnan") |>
    dplyr::pull(aspek)
  declined <- aspect_change |>
    dplyr::filter(status == "Menurun") |>
    dplyr::pull(aspek)

  strengths <- if (length(increased) > 0) {
    paste(head(increased, 2), collapse = " dan ")
  } else {
    "beberapa aspek perkembangan"
  }

  sentence1 <- paste0(
    child_name,
    " menunjukkan perkembangan yang secara umum berada pada kategori ",
    category_long(final_cat),
    " pada akhir semester."
  )
  sentence2 <- if (length(increased) > 0) {
    paste0(" Kemajuan paling terlihat pada ", strengths, ".")
  } else {
    " Pola perkembangan cenderung stabil sepanjang periode observasi."
  }
  sentence3 <- if (length(stagnant) > 0) {
    paste0(
      " Aspek yang masih relatif stagnan adalah ",
      paste(stagnant, collapse = ", "),
      "; guru disarankan memberikan variasi pengalaman bermain, bantuan bertahap, dan observasi ulang dalam konteks yang berbeda."
    )
  } else {
    " Tidak terdapat aspek yang terdeteksi stagnan berdasarkan batas perubahan yang digunakan."
  }
  sentence4 <- if (length(declined) > 0) {
    paste0(
      " Terdapat penurunan pada ",
      paste(declined, collapse = ", "),
      ". Temuan ini perlu diverifikasi melalui observasi berulang dan tidak sebaiknya ditafsirkan sebagai diagnosis atau kemunduran permanen."
    )
  } else {
    " Tidak terdapat aspek yang terdeteksi menurun."
  }

  paste0(sentence1, sentence2, sentence3, sentence4)
}

format_age <- function(months) {
  if (length(months) == 0 || is.na(months[1])) return("-")
  m <- as.integer(months[1])
  paste0(m %/% 12, " tahun ", m %% 12, " bulan")
}

cp_narrative <- function(cp_name, final_score, final_cat) {
  if (is.na(final_score) || is.na(final_cat)) return("Data belum memadai untuk menyusun narasi CP.")
  level <- category_long(final_cat)
  paste0(
    "Pada elemen CP ", cp_name, ", capaian akhir berada pada kategori ", level,
    " dengan skor rata-rata ", sprintf("%.2f", final_score),
    ". Hasil ini perlu dibaca bersama bukti autentik hasil observasi, catatan anekdot, hasil karya, dan konteks keseharian anak."
  )
}
