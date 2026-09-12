indicator_meta <- tibble::tribble(
  ~kode, ~aspek, ~indikator, ~cp_elemen,
  "NAM1", "Nilai Agama dan Moral", "Mengenal keyakinan/ajaran agama yang dianut", "Nilai Agama dan Budi Pekerti",
  "NAM2", "Nilai Agama dan Moral", "Mempraktikkan kebiasaan ibadah sesuai konteks dan bimbingan", "Nilai Agama dan Budi Pekerti",
  "NAM3", "Nilai Agama dan Moral", "Menunjukkan kasih sayang, kejujuran, tanggung jawab, dan perilaku baik", "Nilai Agama dan Budi Pekerti",
  "NAM4", "Nilai Agama dan Moral", "Menghargai sesama dan lingkungan", "Nilai Agama dan Budi Pekerti",
  "PAN1", "Nilai Pancasila", "Mengenali identitas diri, keluarga, satuan pendidikan, dan Indonesia", "Jati Diri",
  "PAN2", "Nilai Pancasila", "Mengikuti aturan sederhana dalam kehidupan sehari-hari", "Jati Diri",
  "PAN3", "Nilai Pancasila", "Menghargai perbedaan, berbagi, dan bekerja sama", "Jati Diri",
  "PAN4", "Nilai Pancasila", "Menunjukkan kepedulian, kemandirian, dan tanggung jawab", "Jati Diri",
  "FM1", "Fisik Motorik", "Menggunakan gerak motorik kasar secara terkoordinasi", "Jati Diri",
  "FM2", "Fisik Motorik", "Menggunakan gerak motorik halus untuk memanipulasi benda dan alat", "Jati Diri",
  "FM3", "Fisik Motorik", "Menunjukkan kemandirian dalam merawat diri", "Jati Diri",
  "FM4", "Fisik Motorik", "Menerapkan kebiasaan sehat, aman, dan menjaga keselamatan diri", "Jati Diri",
  "KOG1", "Kognitif", "Memecahkan masalah sederhana melalui eksplorasi", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "KOG2", "Kognitif", "Mengenali pola, hubungan, sebab-akibat, persamaan, dan perbedaan", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "KOG3", "Kognitif", "Menggunakan konsep bilangan, bentuk, ukuran, ruang, atau simbol secara bermakna", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "KOG4", "Kognitif", "Mengamati, membandingkan, mengelompokkan, dan membuat simpulan sederhana", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "BHS1", "Bahasa", "Menyimak dan memahami pesan, cerita, atau instruksi sederhana", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "BHS2", "Bahasa", "Menggunakan kosakata dan bahasa lisan untuk berkomunikasi", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "BHS3", "Bahasa", "Mengekspresikan gagasan, pengalaman, kebutuhan, atau pertanyaan", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "BHS4", "Bahasa", "Menunjukkan kesadaran awal terhadap bunyi bahasa, teks, simbol, dan pramembaca", "Dasar-dasar Literasi, Matematika, Sains, Teknologi, Rekayasa, dan Seni",
  "SE1", "Sosial-Emosional", "Mengenali diri, kebutuhan, minat, dan perasaan diri", "Jati Diri",
  "SE2", "Sosial-Emosional", "Mengelola emosi dan perilaku dengan dukungan yang sesuai", "Jati Diri",
  "SE3", "Sosial-Emosional", "Berinteraksi, berbagi, dan bekerja sama dengan teman maupun orang dewasa", "Jati Diri",
  "SE4", "Sosial-Emosional", "Menunjukkan empati dan menyelesaikan konflik sederhana dengan dukungan", "Jati Diri"
)

aspect_order <- c(
  "Nilai Agama dan Moral", "Nilai Pancasila", "Fisik Motorik",
  "Kognitif", "Bahasa", "Sosial-Emosional"
)

period_order <- c("Awal Semester", "Tengah Semester", "Akhir Semester")
