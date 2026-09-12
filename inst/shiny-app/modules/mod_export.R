mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shinydashboard::box(
      width = 5, title = "Laporan Kelas", status = "primary", solidHeader = TRUE,
      shiny::p("Excel berisi data observasi, skor aspek, CP, perubahan aspek, dan perubahan indikator."),
      shiny::downloadButton(ns("download_excel"), "Unduh Excel Analisis")
    ),
    shinydashboard::box(
      width = 7, title = "Rapor Perkembangan Anak", status = "info", solidHeader = TRUE,
      shiny::selectInput(ns("child"), "Pilih Anak", choices = NULL),
      shiny::p("Dokumen Word (.docx) berisi identitas, ringkasan CP Fase Fondasi, enam aspek STPPA, narasi perkembangan, rekomendasi, dan area tanda tangan."),
      shiny::downloadButton(ns("download_docx"), "Unduh Rapor Word (.docx)")
    )
  )
}

mod_export_server <- function(id, validated, aspect_scores, aspect_change, indicator_change, cp_scores) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      choices_df <- validated() |>
        dplyr::distinct(id_anak, nama_anak) |>
        dplyr::arrange(nama_anak)
      shiny::updateSelectInput(session, "child", choices = stats::setNames(choices_df$id_anak, choices_df$nama_anak))
    })

    output$download_excel <- shiny::downloadHandler(
      filename = function() paste0("Analisis_PAUD_", Sys.Date(), ".xlsx"),
      content = function(file) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Data")
        openxlsx::writeData(wb, "Data", validated())
        openxlsx::addWorksheet(wb, "Skor Aspek")
        openxlsx::writeData(wb, "Skor Aspek", aspect_scores())
        openxlsx::addWorksheet(wb, "Skor CP")
        openxlsx::writeData(wb, "Skor CP", cp_scores())
        openxlsx::addWorksheet(wb, "Perubahan Aspek")
        openxlsx::writeData(wb, "Perubahan Aspek", aspect_change())
        openxlsx::addWorksheet(wb, "Perubahan Indikator")
        openxlsx::writeData(wb, "Perubahan Indikator", indicator_change())
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    output$download_docx <- shiny::downloadHandler(
      filename = function() {
        shiny::req(input$child)
        nm <- validated() |>
          dplyr::filter(id_anak == input$child) |>
          dplyr::slice(1) |>
          dplyr::pull(nama_anak)
        paste0("Rapor_PAUD_", gsub("[^A-Za-z0-9]+", "_", nm), ".docx")
      },
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
      content = function(file) {
        shiny::req(input$child)
        child_row <- validated() |>
          dplyr::filter(id_anak == input$child) |>
          dplyr::slice(1)
        child_as <- aspect_scores() |> dplyr::filter(id_anak == input$child)
        change <- aspect_change() |> dplyr::filter(id_anak == input$child)
        child_cp <- cp_scores() |> dplyr::filter(id_anak == input$child)

        getv <- function(nm, default = "-") {
          if (nm %in% names(child_row) && length(child_row[[nm]]) > 0 && !is.na(child_row[[nm]][1]) && nzchar(as.character(child_row[[nm]][1]))) {
            as.character(child_row[[nm]][1])
          } else default
        }

        identity <- data.frame(
          `Identitas` = c("Satuan Pendidikan", "NPSN", "Nama Anak", "ID Anak", "Usia", "Kelompok/Kelas", "Semester", "Tahun Ajaran"),
          `Keterangan` = c(
            getv("nama_sekolah"), getv("npsn"), getv("nama_anak"), getv("id_anak"),
            format_age(suppressWarnings(as.numeric(getv("usia_bulan", NA_character_)))), getv("kelompok"),
            getv("semester"), getv("tahun_ajaran")
          ),
          check.names = FALSE
        )

        cp_wide <- child_cp |>
          dplyr::select(cp_elemen, periode, skor_cp, kategori) |>
          tidyr::pivot_wider(names_from = periode, values_from = c(skor_cp, kategori), names_sep = "__")
        final_score_col <- "skor_cp__Akhir Semester"
        final_cat_col <- "kategori__Akhir Semester"
        final_scores <- if (final_score_col %in% names(cp_wide)) cp_wide[[final_score_col]] else rep(NA_real_, nrow(cp_wide))
        final_cats <- if (final_cat_col %in% names(cp_wide)) cp_wide[[final_cat_col]] else rep(NA_character_, nrow(cp_wide))
        cp_report <- cp_wide
        cp_report[["Narasi CP"]] <- mapply(
          cp_narrative,
          cp_report$cp_elemen,
          final_scores,
          final_cats,
          SIMPLIFY = TRUE
        )

        aspect_report <- change |>
          dplyr::transmute(
            `Aspek STPPA` = as.character(aspek),
            `Awal` = round(.data[["skor_aspek__Awal Semester"]], 2),
            `Akhir` = round(.data[["skor_aspek__Akhir Semester"]], 2),
            `Kategori Akhir` = .data[["kategori__Akhir Semester"]],
            `Perubahan` = round(delta, 2),
            `Status` = status
          )

        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "RAPOR PERKEMBANGAN ANAK PAUD", style = "heading 1")
        doc <- officer::body_add_par(doc, "Asesmen Longitudinal Berbasis STPPA dan Capaian Pembelajaran Fase Fondasi")
        doc <- officer::body_add_par(doc, "IDENTITAS", style = "heading 2")

        ft_id <- flextable::flextable(identity) |>
          flextable::theme_box() |>
          flextable::bold(j = 1) |>
          flextable::autofit()
        doc <- flextable::body_add_flextable(doc, ft_id)

        doc <- officer::body_add_par(doc, "CAPAIAN PEMBELAJARAN (CP) FASE FONDASI", style = "heading 2")
        doc <- officer::body_add_par(doc, "Enam aspek STPPA dipetakan secara terintegrasi ke tiga elemen CP Fase Fondasi. Skor membantu membaca pola perkembangan dan tetap perlu dilengkapi bukti autentik.")

        cp_display <- cp_report
        names(cp_display) <- gsub("skor_cp__", "Skor ", names(cp_display), fixed = TRUE)
        names(cp_display) <- gsub("kategori__", "Kategori ", names(cp_display), fixed = TRUE)
        names(cp_display)[names(cp_display) == "cp_elemen"] <- "Elemen CP"
        ft_cp <- flextable::flextable(cp_display) |>
          flextable::theme_box() |>
          flextable::autofit() |>
          flextable::fontsize(size = 8, part = "all")
        doc <- flextable::body_add_flextable(doc, ft_cp)

        doc <- officer::body_add_par(doc, "RINGKASAN ENAM ASPEK STPPA", style = "heading 2")
        ft_as <- flextable::flextable(aspect_report) |>
          flextable::theme_box() |>
          flextable::autofit() |>
          flextable::fontsize(size = 9, part = "all")
        doc <- flextable::body_add_flextable(doc, ft_as)

        doc <- officer::body_add_par(doc, "NARASI PERKEMBANGAN", style = "heading 2")
        doc <- officer::body_add_par(doc, make_narrative(child_as, change))

        doc <- officer::body_add_par(doc, "REKOMENDASI TINDAK LANJ", style = "heading 2")
        for (i in seq_len(nrow(change))) {
          row <- change[i, ]
          final_cat <- if ("kategori__Akhir Semester" %in% names(row)) row[["kategori__Akhir Semester"]] else NA_character_
          doc <- officer::body_add_par(doc, paste0(as.character(row$aspek), ": ", recommendation_text(as.character(row$aspek), row$status, final_cat)))
        }

        doc <- officer::body_add_par(doc, "CATATAN GURU", style = "heading 2")
        doc <- officer::body_add_par(doc, "........................................................................................................................")
        doc <- officer::body_add_par(doc, "........................................................................................................................")

        doc <- officer::body_add_par(doc, "PENGESAHAN", style = "heading 2")
        sign_tbl <- data.frame(
          `Orang Tua/Wali` = "\n\n\n(____________________)",
          `Guru Kelas` = "\n\n\n(____________________)",
          `Kepala Satuan PAUD` = "\n\n\n(____________________)",
          check.names = FALSE
        )
        ft_sign <- flextable::flextable(sign_tbl) |>
          flextable::theme_box() |>
          flextable::align(align = "center", part = "all") |>
          flextable::autofit()
        doc <- flextable::body_add_flextable(doc, ft_sign)

        doc <- officer::body_add_par(doc, "Catatan: Hasil aplikasi merupakan dukungan asesmen pendidikan dan bukan diagnosis perkembangan anak.")
        print(doc, target = file)
      }
    )
  })
}
