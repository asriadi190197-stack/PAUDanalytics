mod_child_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 3, title = "Pilih Anak", status = "primary", solidHeader = TRUE,
        shiny::selectInput(ns("child"), "Anak", choices = NULL),
        shiny::uiOutput(ns("info"))
      ),
      shinydashboard::box(
        width = 9, title = "Grafik Longitudinal 6 Aspek", status = "primary", solidHeader = TRUE,
        shiny::plotOutput(ns("trend"), height = 400)
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12, title = "Perubahan Awal → Akhir", status = "info", solidHeader = TRUE,
        DT::DTOutput(ns("change_table"))
      )
    )
  )
}

mod_child_server <- function(id, validated, aspect_scores, aspect_change) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      choices_df <- validated() |>
        dplyr::distinct(id_anak, nama_anak) |>
        dplyr::arrange(nama_anak)
      shiny::updateSelectInput(session, "child", choices = stats::setNames(choices_df$id_anak, choices_df$nama_anak))
    })

    output$info <- shiny::renderUI({
      shiny::req(input$child)
      d <- validated() |>
        dplyr::filter(id_anak == input$child) |>
        dplyr::slice(1)
      shiny::tagList(
        shiny::h4(d$nama_anak),
        shiny::p(paste("Kelompok:", d$kelompok)),
        shiny::p(paste("Usia:", d$usia_bulan, "bulan"))
      )
    })

    output$trend <- shiny::renderPlot({
      shiny::req(input$child)
      aspect_scores() |>
        dplyr::filter(id_anak == input$child) |>
        ggplot2::ggplot(ggplot2::aes(periode, skor_aspek, group = aspek, linetype = aspek)) +
        ggplot2::geom_line(linewidth = 1.1) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::scale_y_continuous(limits = c(1, 4), breaks = 1:4) +
        ggplot2::labs(x = NULL, y = "Skor aspek", linetype = "Aspek") +
        ggplot2::theme_minimal(base_size = 12)
    })

    output$change_table <- DT::renderDT({
      shiny::req(input$child)
      tab <- aspect_change() |>
        dplyr::filter(id_anak == input$child) |>
        dplyr::select(aspek, dplyr::contains("Awal Semester"), dplyr::contains("Akhir Semester"), delta, status)
      DT::datatable(tab, options = list(scrollX = TRUE), rownames = FALSE)
    })
  })
}
