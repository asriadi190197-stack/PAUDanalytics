mod_class_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 4, title = "Filter Analisis", status = "primary", solidHeader = TRUE,
        shiny::selectInput(ns("aspect"), "Aspek perkembangan", choices = aspect_order, selected = "Bahasa"),
        shiny::p("Grafik menampilkan rata-rata kelas dan distribusi kategori pada tiga waktu observasi.")
      ),
      shinydashboard::box(
        width = 8, title = "Perubahan Rata-rata Kelas", status = "primary", solidHeader = TRUE,
        shiny::plotOutput(ns("trend"), height = 340)
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 8, title = "Distribusi Kategori Antarperiode", status = "info", solidHeader = TRUE,
        shiny::plotOutput(ns("categories"), height = 390)
      ),
      shinydashboard::box(
        width = 4, title = "Ringkasan Perubahan", status = "warning", solidHeader = TRUE,
        DT::DTOutput(ns("change_table"))
      )
    )
  )
}

mod_class_server <- function(id, aspect_scores, aspect_change) {
  shiny::moduleServer(id, function(input, output, session) {
    output$trend <- shiny::renderPlot({
      d <- aspect_scores() |>
        dplyr::filter(as.character(aspek) == input$aspect) |>
        dplyr::group_by(periode) |>
        dplyr::summarise(
          mean = mean(skor_aspek, na.rm = TRUE),
          se = stats::sd(skor_aspek, na.rm = TRUE) / sqrt(dplyr::n()),
          .groups = "drop"
        )

      ggplot2::ggplot(d, ggplot2::aes(periode, mean, group = 1)) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = pmax(1, mean - se), ymax = pmin(4, mean + se)),
          width = 0.10, linewidth = 0.8, colour = paud_palette$aqua
        ) +
        ggplot2::geom_line(linewidth = 1.45, colour = paud_palette$teal) +
        ggplot2::geom_point(size = 5, shape = 21, stroke = 1.4, fill = "white", colour = paud_palette$teal) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", mean)), vjust = -1.25, fontface = "bold", colour = paud_palette$navy, size = 4) +
        score_scale() +
        ggplot2::labs(x = NULL, y = "Rata-rata kelas", title = input$aspect, subtitle = "Pita tipis menunjukkan ±1 standard error.") +
        paud_theme(base_size = 13) +
        ggplot2::theme(legend.position = "none")
    })

    output$categories <- shiny::renderPlot({
      d <- aspect_scores() |>
        dplyr::filter(as.character(aspek) == input$aspect) |>
        dplyr::count(periode, kategori) |>
        dplyr::group_by(periode) |>
        dplyr::mutate(p = n / sum(n)) |>
        dplyr::ungroup()

      ggplot2::ggplot(d, ggplot2::aes(periode, p, fill = kategori)) +
        ggplot2::geom_col(width = 0.62) +
        ggplot2::geom_text(
          ggplot2::aes(label = ifelse(p >= .08, scales::percent(p, accuracy = 1), "")),
          position = ggplot2::position_stack(vjust = .5), colour = "white", fontface = "bold", size = 3.6
        ) +
        ggplot2::scale_fill_manual(values = category_colors, drop = FALSE) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
        ggplot2::labs(x = NULL, y = "Proporsi anak", fill = "Kategori") +
        paud_theme(base_size = 12)
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
