mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shinydashboard::box(
      width = 4, title = "Laporan Kelas", status = "primary", solidHeader = TRUE,
      shiny::p("Excel berisi data, skor aspek, CP, perubahan aspek, dan perubahan indikator."),
      shiny::downloadButton(ns("download_excel"), "Unduh Excel Analisis")
    ),
    shinydashboard::box(
      width = 8, title = "Rapor Perkembangan Anak", status = "info", solidHeader = TRUE,
      shiny::selectInput(ns("child"), "Pilih Anak", choices = NULL),
      shiny::p("Rapor Word dibuat portrait, ringkas, dan mudah dicetak. Detail indikator ditempatkan sebagai lampiran agar tabel utama tidak melewati margin."),
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
        child_as <- aspect_scores() |>
          dplyr::filter(id_anak == input$child)
        change <- aspect_change() |>
          dplyr::filter(id_anak == input$child)
        child_cp <- cp_scores() |>
          dplyr::filter(id_anak == input$child)
        child_ind <- indicator_change() |>
          dplyr::filter(id_anak == input$child)

        getv <- function(nm, default = "-") {
          if (nm %in% names(child_row) && length(child_row[[nm]]) > 0 && !is.na(child_row[[nm]][1]) && nzchar(as.character(child_row[[nm]][1]))) {
            as.character(child_row[[nm]][1])
          } else {
            default
          }
        }

        style_ft <- function(ft, font_size = 9) {
          ft |>
            flextable::theme_vanilla() |>
            flextable::bg(bg = "#17324D", part = "header") |>
            flextable::color(color = "#FFFFFF", part = "header") |>
            flextable::bold(part = "header") |>
            flextable::fontsize(size = font_size, part = "all") |>
            flextable::font(fontname = "Aptos", part = "all") |>
            flextable::valign(valign = "center", part = "all") |>
            flextable::padding(padding.top = 4, padding.bottom = 4, padding.left = 4, padding.right = 4, part = "all") |>
            flextable::set_table_properties(layout = "fixed", width = 1)
        }

        identity <- data.frame(
          Identitas = c("Satuan Pendidikan", "NPSN", "Nama Anak", "ID Anak", "Usia", "Kelompok/Kelas", "Semester", "Tahun Ajaran"),
          Keterangan = c(
            getv("nama_sekolah"), getv("npsn"), getv("nama_anak"), getv("id_anak"),
            format_age(suppressWarnings(as.numeric(getv("usia_bulan", NA_character_)))),
            getv("kelompok"), getv("semester"), getv("tahun_ajaran")
          ),
          check.names = FALSE
        )

        cp_final <- child_cp |>
          dplyr::filter(periode == "Akhir Semester") |>
          dplyr::transmute(
            `Elemen CP` = cp_elemen,
            `Skor Akhir` = round(skor_cp, 2),
            `Kategori` = kategori,
            `Deskripsi Perkembangan` = mapply(cp_narrative, cp_elemen, skor_cp, kategori, SIMPLIFY = TRUE)
          )

        aspect_report <- change |>
          dplyr::transmute(
            `Aspek STPPA` = as.character(aspek),
            `Awal` = round(.data[["skor_aspek__Awal Semester"]], 2),
            `Akhir` = round(.data[["skor_aspek__Akhir Semester"]], 2),
            `Kategori Akhir` = .data[["kategori__Akhir Semester"]],
            `Status` = status
          )

        plot_file <- tempfile(fileext = ".png")
        p <- child_as |>
          dplyr::mutate(aspek = factor(as.character(aspek), levels = aspect_order)) |>
          ggplot2::ggplot(ggplot2::aes(periode, skor_aspek, group = 1)) +
          ggplot2::geom_line(linewidth = 1.2, colour = "#2B7A78") +
          ggplot2::geom_point(size = 4.2, shape = 21, stroke = 1.2, fill = "white", colour = "#2B7A78") +
          ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", skor_aspek)), vjust = -1.05, size = 3.2, fontface = "bold", colour = "#17324D") +
          ggplot2::facet_wrap(~aspek, ncol = 2) +
          ggplot2::scale_y_continuous(limits = c(0.85, 4.25), breaks = 1:4, labels = c("1 BB", "2 MB", "3 BSH", "4 BSB")) +
          ggplot2::labs(x = NULL, y = "Skor perkembangan") +
          ggplot2::theme_minimal(base_size = 10) +
          ggplot2::theme(
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(colour = "#E8EDF2"),
            strip.background = ggplot2::element_rect(fill = "#EEF4F6", colour = NA),
            strip.text = ggplot2::element_text(face = "bold", colour = "#17324D"),
            axis.text.x = ggplot2::element_text(face = "bold"),
            plot.margin = ggplot2::margin(8, 8, 8, 8)
          )
        ggplot2::ggsave(plot_file, p, width = 6.2, height = 7.0, units = "in", dpi = 220, bg = "white")

        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "RAPOR PERKEMBANGAN ANAK", style = "heading 1")
        doc <- officer::body_add_par(doc, "PAUD · Asesmen Longitudinal STPPA & CP Fase Fondasi")
        doc <- officer::body_add_par(doc, paste0("Semester ", getv("semester"), " · Tahun Ajaran ", getv("tahun_ajaran")))

        doc <- officer::body_add_par(doc, "IDENTITAS ANAK DAN SATUAN PENDIDIKAN", style = "heading 2")
        ft_id <- flextable::flextable(identity) |>
          style_ft(font_size = 9.5) |>
          flextable::bold(j = 1) |>
          flextable::width(j = 1, width = 2.0) |>
          flextable::width(j = 2, width = 4.1)
        doc <- flextable::body_add_flextable(doc, ft_id)

        doc <- officer::body_add_par(doc, "RINGKASAN PERKEMBANGAN", style = "heading 2")
        doc <- officer::body_add_par(doc, make_narrative(child_as, change))

        doc <- officer::body_add_par(doc, "CAPAIAN PEMBELAJARAN (CP) FASE FONDASI", style = "heading 2")
        doc <- officer::body_add_par(doc, "CP disajikan dalam tiga elemen terintegrasi. Interpretasi perlu dibaca bersama bukti autentik dan catatan observasi guru.")
        ft_cp <- flextable::flextable(cp_final) |>
          style_ft(font_size = 8.3) |>
          flextable::width(j = 1, width = 1.55) |>
          flextable::width(j = 2, width = 0.75) |>
          flextable::width(j = 3, width = 0.85) |>
          flextable::width(j = 4, width = 3.0) |>
          flextable::align(j = c(2, 3), align = "center", part = "all")
        doc <- flextable::body_add_flextable(doc, ft_cp)

        doc <- officer::body_add_break(doc)
        doc <- officer::body_add_par(doc, "PROFIL ENAM ASPEK STPPA", style = "heading 2")
        ft_as <- flextable::flextable(aspect_report) |>
          style_ft(font_size = 8.8) |>
          flextable::width(j = 1, width = 2.45) |>
          flextable::width(j = c(2, 3), width = 0.65) |>
          flextable::width(j = 4, width = 1.15) |>
          flextable::width(j = 5, width = 1.2) |>
          flextable::align(j = 2:5, align = "center", part = "all")
        doc <- flextable::body_add_flextable(doc, ft_as)

        doc <- officer::body_add_par(doc, "GRAFIK PERKEMBANGAN LONGITUDINAL", style = "heading 2")
        doc <- officer::body_add_img(doc, src = plot_file, width = 6.15, height = 6.95)

        doc <- officer::body_add_break(doc)
        doc <- officer::body_add_par(doc, "REKOMENDASI TINDAK LANJ", style = "heading 2")
        for (i in seq_len(nrow(change))) {
          row <- change[i, ]
          final_cat <- if ("kategori__Akhir Semester" %in% names(row)) row[["kategori__Akhir Semester"]] else NA_character_
          doc <- officer::body_add_par(doc, as.character(row$aspek), style = "heading 3")
          doc <- officer::body_add_par(doc, recommendation_text(as.character(row$aspek), row$status, final_cat))
        }

        doc <- officer::body_add_par(doc, "CATATAN GURU", style = "heading 2")
        doc <- officer::body_add_par(doc, "........................................................................................................................")
        doc <- officer::body_add_par(doc, "........................................................................................................................")
        doc <- officer::body_add_par(doc, "........................................................................................................................")

        doc <- officer::body_add_par(doc, "PENGESAHAN", style = "heading 2")
        sign_tbl <- data.frame(
          `Orang Tua/Wali` = "\n\n(____________________)",
          `Guru Kelas` = "\n\n(____________________)",
          `Kepala Satuan PAUD` = "\n\n(____________________)",
          check.names = FALSE
        )
        ft_sign <- flextable::flextable(sign_tbl) |>
          style_ft(font_size = 9) |>
          flextable::align(align = "center", part = "all") |>
          flextable::width(j = 1:3, width = 2.05)
        doc <- flextable::body_add_flextable(doc, ft_sign)
        doc <- officer::body_add_par(doc, "Catatan: Hasil aplikasi merupakan dukungan asesmen pendidikan dan bukan diagnosis perkembangan anak.")

        doc <- officer::body_add_break(doc)
        doc <- officer::body_add_par(doc, "LAMPIRAN · DETAIL INDIKATOR", style = "heading 1")
        doc <- officer::body_add_par(doc, "Lampiran berikut memuat detail indikator. Tabel dipecah per aspek agar tetap nyaman dibaca pada kertas portrait.")

        for (asp in aspect_order) {
          tab <- child_ind |>
            dplyr::filter(aspek == asp) |>
            dplyr::transmute(
              Kode = kode,
              Indikator = indikator,
              Awal = round(.data[["Awal Semester"]], 1),
              Tengah = round(.data[["Tengah Semester"]], 1),
              Akhir = round(.data[["Akhir Semester"]], 1),
              Status = status
            )
          if (nrow(tab) > 0) {
            doc <- officer::body_add_par(doc, asp, style = "heading 2")
            ft_ind <- flextable::flextable(tab) |>
              style_ft(font_size = 8.2) |>
              flextable::width(j = 1, width = 0.55) |>
              flextable::width(j = 2, width = 3.0) |>
              flextable::width(j = 3:5, width = 0.55) |>
              flextable::width(j = 6, width = 0.9) |>
              flextable::align(j = c(1, 3, 4, 5, 6), align = "center", part = "all")
            doc <- flextable::body_add_flextable(doc, ft_ind)
          }
        }

        print(doc, target = file)
        unlink(plot_file)
      }
    )
  })
}
