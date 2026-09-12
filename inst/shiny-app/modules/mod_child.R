mod_child_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 3, title = "Profil Anak", status = "primary", solidHeader = TRUE,
        shiny::selectInput(ns("child"), "Pilih anak", choices = NULL),
        shiny::uiOutput(ns("info"))
      ),
      shinydashboard::box(
        width = 9, title = "Perjalanan Perkembangan 6 Aspek", status = "primary", solidHeader = TRUE,
        shiny::plotOutput(ns("trend"), height = 590)
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12, title = "Ringkasan Perubahan Awal → Akhir", status = "info", solidHeader = TRUE,
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
        shiny::h4(style = "font-weight:700;color:#17324D", d$nama_anak),
        shiny::p(shiny::tags$b("Kelompok: "), d$kelompok),
        shiny::p(shiny::tags$b("Usia: "), format_age(suppressWarnings(as.numeric(d$usia_bulan))))
      )
    })

    output$trend <- shiny::renderPlot({
      shiny::req(input$child)
      d <- aspect_scores() |>
        dplyr::filter(id_anak == input$child) |>
        dplyr::mutate(aspek = factor(as.character(aspek), levels = aspect_order))

      ggplot2::ggplot(d, ggplot2::aes(x = periode, y = skor_aspek, group = 1)) +
        ggplot2::geom_line(linewidth = 1.25, colour = paud_palette$teal) +
        ggplot2::geom_point(size = 4.4, shape = 21, stroke = 1.2, fill = "white", colour = paud_palette$teal) +
        ggplot2::geom_text(
          ggplot2::aes(label = sprintf("%.2f", skor_aspek)),
          vjust = -1.15, size = 3.5, fontface = "bold", colour = paud_palette$navy
        ) +
        ggplot2::facet_wrap(~aspek, ncol = 2) +
        score_scale() +
        ggplot2::labs(
          x = NULL, y = "Skor perkembangan",
          subtitle = "Awal, tengah, dan akhir semester ditampilkan terpisah agar pola setiap aspek mudah dibaca."
        ) +
        paud_theme(base_size = 12) +
        ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(face = "bold"))
    })

    output$change_table <- DT::renderDT({
      shiny::req(input$child)
      tab <- aspect_change() |>
        dplyr::filter(id_anak == input$child) |>
        dplyr::transmute(
          Aspek = as.character(aspek),
          Awal = round(.data[["skor_aspek__Awal Semester"]], 2),
          Akhir = round(.data[["skor_aspek__Akhir Semester"]], 2),
          Perubahan = round(delta, 2),
          Status = status
        )
      DT::datatable(tab, options = list(dom = "t", pageLength = 6), rownames = FALSE)
    })
  })
}
