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
        shiny::plotOutput(ns("trend"), height = 520)
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
      d <- long_indicator() |>
        dplyr::filter(id_anak == input$child, aspek == input$aspect)

      ggplot2::ggplot(d, ggplot2::aes(periode, skor, group = 1)) +
        ggplot2::geom_line(linewidth = 1.15, colour = paud_palette$purple) +
        ggplot2::geom_point(size = 4.2, shape = 21, stroke = 1.2, fill = "white", colour = paud_palette$purple) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", skor)), vjust = -1.1, size = 3.3, fontface = "bold", colour = paud_palette$navy) +
        ggplot2::facet_wrap(~paste0(kode, " · ", indikator), ncol = 2) +
        score_scale() +
        ggplot2::labs(x = NULL, y = "Skor indikator", subtitle = "Satu panel untuk satu indikator agar tidak terjadi tumpang tindih garis maupun label.") +
        paud_theme(base_size = 11) +
        ggplot2::theme(legend.position = "none", strip.text = ggplot2::element_text(size = 9.5, face = "bold"))
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
