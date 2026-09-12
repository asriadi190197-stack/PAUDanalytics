mod_indicator_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 4, title = "Pilih Anak & Aspek", status = "primary", solidHeader = TRUE,
        shiny::selectInput(ns("child"), "Anak", choices = NULL),
        shiny::selectInput(ns("aspect"), "Aspek", choices = aspect_order, selected = "Bahasa")
      ),
      shinydashboard::box(
        width = 8, title = "Tren per Indikator", status = "primary", solidHeader = TRUE,
        shiny::plotOutput(ns("trend"), height = 380)
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12, title = "Deteksi Indikator Stagnan / Menurun", status = "warning", solidHeader = TRUE,
        DT::DTOutput(ns("status_table"))
      )
    )
  )
}

mod_indicator_server <- function(id, validated, long_indicator, indicator_change) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      choices_df <- validated() |>
        dplyr::distinct(id_anak, nama_anak) |>
        dplyr::arrange(nama_anak)
      shiny::updateSelectInput(session, "child", choices = stats::setNames(choices_df$id_anak, choices_df$nama_anak))
    })

    output$trend <- shiny::renderPlot({
      shiny::req(input$child, input$aspect)
      long_indicator() |>
        dplyr::filter(id_anak == input$child, aspek == input$aspect) |>
        ggplot2::ggplot(ggplot2::aes(periode, skor, group = kode, linetype = kode)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 2.3) +
        ggplot2::scale_y_continuous(limits = c(1, 4), breaks = 1:4) +
        ggplot2::labs(x = NULL, y = "Skor indikator", linetype = "Indikator") +
        ggplot2::theme_minimal(base_size = 12)
    })

    output$status_table <- DT::renderDT({
      shiny::req(input$child, input$aspect)
      tab <- indicator_change() |>
        dplyr::filter(id_anak == input$child, aspek == input$aspect) |>
        dplyr::arrange(factor(status, levels = c("Menurun", "Stagnan", "Meningkat"))) |>
        dplyr::select(kode, indikator, `Awal Semester`, `Tengah Semester`, `Akhir Semester`, delta, status)
      DT::datatable(tab, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })
  })
}
