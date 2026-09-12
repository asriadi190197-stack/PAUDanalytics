mod_cp_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 4, title = "Capaian Pembelajaran", status = "primary", solidHeader = TRUE,
        shiny::selectInput(ns("child"), "Pilih anak", choices = NULL),
        shiny::p("Enam aspek STPPA dipetakan secara terintegrasi ke tiga elemen CP Fase Fondasi.")
      ),
      shinydashboard::box(
        width = 8, title = "Perjalanan CP Fase Fondasi", status = "info", solidHeader = TRUE,
        shiny::plotOutput(ns("cp_trend"), height = 500)
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(width = 12, title = "Ringkasan CP", status = "primary", solidHeader = TRUE, DT::DTOutput(ns("cp_table")))
    ),
    shiny::fluidRow(
      shinydashboard::box(width = 12, title = "Pemetaan Indikator ke CP", status = "warning", solidHeader = TRUE, DT::DTOutput(ns("mapping")))
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
      d <- cp_scores() |> dplyr::filter(id_anak == input$child)
      ggplot2::ggplot(d, ggplot2::aes(periode, skor_cp, group = 1)) +
        ggplot2::geom_line(linewidth = 1.25, colour = paud_palette$blue) +
        ggplot2::geom_point(size = 4.5, shape = 21, stroke = 1.2, fill = "white", colour = paud_palette$blue) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", skor_cp)), vjust = -1.15, fontface = "bold", colour = paud_palette$navy, size = 3.5) +
        ggplot2::facet_wrap(~cp_elemen, ncol = 1) +
        score_scale() +
        ggplot2::labs(x = NULL, y = "Skor CP", subtitle = "Setiap elemen ditampilkan pada panel terpisah agar garis dan label tidak tumpang tindih.") +
        paud_theme(base_size = 12) +
        ggplot2::theme(legend.position = "none")
    })

    output$cp_table <- DT::renderDT({
      shiny::req(input$child)
      tab <- cp_scores() |>
        dplyr::filter(id_anak == input$child) |>
        dplyr::select(cp_elemen, periode, skor_cp, kategori) |>
        tidyr::pivot_wider(names_from = periode, values_from = c(skor_cp, kategori), names_sep = "__")
      DT::datatable(tab, options = list(scrollX = TRUE, pageLength = 3), rownames = FALSE)
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
