mod_cp_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 4, title = "Pilih Anak", status = "primary", solidHeader = TRUE,
        shiny::selectInput(ns("child"), "Anak", choices = NULL),
        shiny::p("Hasil enam aspek STPPA dipetakan ke tiga elemen CP Fase Fondasi.")
      ),
      shinydashboard::box(
        width = 8, title = "Perkembangan Berdasarkan CP", status = "info", solidHeader = TRUE,
        shiny::plotOutput(ns("cp_trend"), height = 360)
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12, title = "Ringkasan CP Fase Fondasi", status = "primary", solidHeader = TRUE,
        DT::DTOutput(ns("cp_table"))
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12, title = "Pemetaan Indikator ke CP", status = "warning", solidHeader = TRUE,
        DT::DTOutput(ns("mapping"))
      )
    )
  )
}

mod_cp_server <- function(id, validated, cp_scores) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      choices_df <- validated() |>
        dplyr::distinct(id_anak, nama_anak) |>
        dplyr::arrange(nama_anak)
      shiny::updateSelectInput(session, "child", choices = stats::setNames(choices_df$id_anak, choices_df$nama_anak))
    })

    output$cp_trend <- shiny::renderPlot({
      shiny::req(input$child)
      cp_scores() |>
        dplyr::filter(id_anak == input$child) |>
        ggplot2::ggplot(ggplot2::aes(periode, skor_cp, group = cp_elemen, linetype = cp_elemen)) +
        ggplot2::geom_line(linewidth = 1.1) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::scale_y_continuous(limits = c(1, 4), breaks = 1:4) +
        ggplot2::labs(x = NULL, y = "Skor CP", linetype = "Elemen CP") +
        ggplot2::theme_minimal(base_size = 12)
    })

    output$cp_table <- DT::renderDT({
      shiny::req(input$child)
      tab <- cp_scores() |>
        dplyr::filter(id_anak == input$child) |>
        dplyr::select(cp_elemen, periode, skor_cp, kategori) |>
        tidyr::pivot_wider(names_from = periode, values_from = c(skor_cp, kategori), names_sep = "__")
      DT::datatable(tab, options = list(scrollX = TRUE), rownames = FALSE)
    })

    output$mapping <- DT::renderDT({
      DT::datatable(
        indicator_meta |>
          dplyr::select(cp_elemen, aspek, kode, indikator) |>
          dplyr::arrange(cp_elemen, aspek, kode),
        options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE
      )
    })
  })
}
