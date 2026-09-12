mod_class_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 5, title = "Filter", status = "primary", solidHeader = TRUE,
        shiny::selectInput(ns("aspect"), "Aspek", choices = aspect_order, selected = "Bahasa")
      ),
      shinydashboard::box(
        width = 7, title = "Perubahan Rata-rata Kelas", status = "primary", solidHeader = TRUE,
        shiny::plotOutput(ns("trend"), height = 320)
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 7, title = "Distribusi Kategori Antarperiode", status = "info", solidHeader = TRUE,
        shiny::plotOutput(ns("categories"), height = 390)
      ),
      shinydashboard::box(
        width = 5, title = "Ringkasan Perubahan", status = "warning", solidHeader = TRUE,
        DT::DTOutput(ns("change_table"))
      )
    )
  )
}

mod_class_server <- function(id, aspect_scores, aspect_change) {
  shiny::moduleServer(id, function(input, output, session) {
    output$trend <- shiny::renderPlot({
      aspect_scores() |>
        dplyr::filter(as.character(aspek) == input$aspect) |>
        dplyr::group_by(periode) |>
        dplyr::summarise(
          mean = mean(skor_aspek, na.rm = TRUE),
          se = stats::sd(skor_aspek, na.rm = TRUE) / sqrt(dplyr::n()),
          .groups = "drop"
        ) |>
        ggplot2::ggplot(ggplot2::aes(periode, mean, group = 1)) +
        ggplot2::geom_line(linewidth = 1.2) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = pmax(1, mean - se), ymax = pmin(4, mean + se)), width = 0.12
        ) +
        ggplot2::scale_y_continuous(limits = c(1, 4), breaks = 1:4) +
        ggplot2::labs(x = NULL, y = "Rata-rata aspek", title = input$aspect) +
        ggplot2::theme_minimal(base_size = 13)
    })

    output$categories <- shiny::renderPlot({
      aspect_scores() |>
        dplyr::filter(as.character(aspek) == input$aspect) |>
        dplyr::count(periode, kategori) |>
        dplyr::group_by(periode) |>
        dplyr::mutate(p = n / sum(n)) |>
        dplyr::ungroup() |>
        ggplot2::ggplot(ggplot2::aes(periode, p, fill = kategori)) +
        ggplot2::geom_col() +
        ggplot2::scale_y_continuous(labels = scales::percent_format()) +
        ggplot2::labs(x = NULL, y = "Proporsi anak", fill = "Kategori") +
        ggplot2::theme_minimal(base_size = 12)
    })

    output$change_table <- DT::renderDT({
      tab <- aspect_change() |>
        dplyr::filter(as.character(aspek) == input$aspect) |>
        dplyr::count(status, name = "Jumlah")
      if (nrow(tab) > 0 && sum(tab$Jumlah) > 0) {
        tab <- tab |>
          dplyr::mutate(Persen = scales::percent(Jumlah / sum(Jumlah), accuracy = 0.1))
      }
      DT::datatable(tab, options = list(dom = "t"), rownames = FALSE)
    })
  })
}
