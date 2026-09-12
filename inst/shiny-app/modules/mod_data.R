mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 4, title = "Input", status = "primary", solidHeader = TRUE,
        shiny::fileInput(ns("file"), "Upload Excel / CSV", accept = c(".xlsx", ".xls", ".csv")),
        shiny::downloadButton(ns("download_template"), "Unduh Template Longitudinal"),
        shiny::checkboxInput(ns("use_demo"), "Gunakan data contoh", TRUE),
        shiny::helpText("Kolom wajib: id_anak, nama_anak, kelompok, usia_bulan, periode, dan indikator.")
      ),
      shinydashboard::box(
        width = 8, title = "Preview Data", status = "primary", solidHeader = TRUE,
        DT::DTOutput(ns("data_table"))
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12, title = "Kamus Indikator Operasional", status = "info", solidHeader = TRUE,
        DT::DTOutput(ns("indicator_meta"))
      )
    )
  )
}

mod_data_server <- function(id, template_df) {
  shiny::moduleServer(id, function(input, output, session) {
    raw_data <- shiny::reactive({
      if (isTRUE(input$use_demo) || is.null(input$file)) {
        return(template_df)
      }
      ext <- tolower(tools::file_ext(input$file$name))
      if (ext %in% c("xlsx", "xls")) {
        readxl::read_excel(input$file$datapath)
      } else {
        utils::read.csv(input$file$datapath, stringsAsFactors = FALSE)
      }
    })

    output$data_table <- DT::renderDT({
      DT::datatable(raw_data(), options = list(scrollX = TRUE, pageLength = 10))
    })
    output$indicator_meta <- DT::renderDT({
      DT::datatable(indicator_meta, options = list(pageLength = 12, scrollX = TRUE))
    })

    output$download_template <- shiny::downloadHandler(
      filename = function() "template_observasi_longitudinal_PAUD.xlsx",
      content = function(file) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Data Observasi")
        openxlsx::writeData(wb, "Data Observasi", template_df)
        openxlsx::addWorksheet(wb, "Kamus Indikator")
        openxlsx::writeData(wb, "Kamus Indikator", indicator_meta)
        openxlsx::addWorksheet(wb, "Petunjuk")
        petunjuk <- data.frame(
          Petunjuk = c(
            "Satu baris = satu anak pada satu periode.",
            "Gunakan periode persis: Awal Semester, Tengah Semester, Akhir Semester.",
            "Skor default indikator 1-4.",
            "Jangan mengubah kode indikator tanpa menyesuaikan aplikasi."
          )
        )
        openxlsx::writeData(wb, "Petunjuk", petunjuk)
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    raw_data
  })
}
