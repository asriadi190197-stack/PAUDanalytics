mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shinydashboard::box(
      width = 6, title = "Laporan Kelas", status = "primary", solidHeader = TRUE,
      shiny::p("Excel berisi data, skor aspek, perubahan, dan indikator yang perlu perhatian."),
      shiny::downloadButton(ns("download_excel"), "Unduh Excel Analisis")
    ),
    shinydashboard::box(
      width = 6, title = "Laporan Individual", status = "info", solidHeader = TRUE,
      shiny::selectInput(ns("child"), "Pilih Anak", choices = NULL),
      shiny::downloadButton(ns("download_docx"), "Unduh Laporan Word")
    )
  )
}

mod_export_server <- function(id, validated, aspect_scores, aspect_change, indicator_change) {
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
        paste0("Laporan_", gsub(" ", "_", nm), ".docx")
      },
      content = function(file) {
        shiny::req(input$child)
        child_as <- aspect_scores() |> dplyr::filter(id_anak == input$child)
        change <- aspect_change() |> dplyr::filter(id_anak == input$child)
        nm <- unique(child_as$nama_anak)[1]

        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Laporan Perkembangan Anak", style = "heading 1")
        doc <- officer::body_add_par(doc, paste("Nama:", nm))
        doc <- officer::body_add_par(doc, make_narrative(child_as, change))
        doc <- officer::body_add_par(doc, "Ringkasan Per Aspek", style = "heading 2")
        doc <- officer::body_add_table(doc, change |> dplyr::select(aspek, delta, status), style = "Table Grid")
        doc <- officer::body_add_par(doc, "Rekomendasi", style = "heading 2")

        for (i in seq_len(nrow(change))) {
          row <- change[i, ]
          final_cat <- if ("kategori__Akhir Semester" %in% names(row)) {
            row[["kategori__Akhir Semester"]]
          } else {
            NA_character_
          }
          doc <- officer::body_add_par(doc, as.character(row$aspek), style = "heading 3")
          doc <- officer::body_add_par(doc, recommendation_text(as.character(row$aspek), row$status, final_cat))
        }
        print(doc, target = file)
      }
    )
  })
}
