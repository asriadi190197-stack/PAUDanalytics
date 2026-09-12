mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::valueBoxOutput(ns("vb_child")),
      shinydashboard::valueBoxOutput(ns("vb_obs")),
      shinydashboard::valueBoxOutput(ns("vb_progress")),
      shinydashboard::valueBoxOutput(ns("vb_stagnant"))
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 8, title = "Tren Rata-rata Kelas", status = "primary", solidHeader = TRUE,
        shiny::plotOutput(ns("trend"), height = 360)
      ),
      shinydashboard::box(
        width = 4, title = "Cara Membaca", status = "info", solidHeader = TRUE,
        shiny::tags$p("Satu anak idealnya memiliki tiga observasi: Awal, Tengah, dan Akhir Semester."),
        shiny::tags$p("Aplikasi membaca perubahan skor per aspek dan per indikator."),
        shiny::tags$div(
          class = "note",
          shiny::tags$strong("Penting: "),
          "Status stagnan/menurun adalah sinyal analitik untuk tindak lanjut observasi, bukan diagnosis perkembangan."
        )
      )
    )
  )
}

mod_home_server <- function(id, validated, aspect_scores, aspect_change) {
  shiny::moduleServer(id, function(input, output, session) {
    output$vb_child <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        dplyr::n_distinct(validated()$id_anak), "Anak",
        icon = shiny::icon("children"), color = "aqua"
      )
    })
    output$vb_obs <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        nrow(validated()), "Observasi",
        icon = shiny::icon("clipboard-check"), color = "blue"
      )
    })
    output$vb_progress <- shinydashboard::renderValueBox({
      p <- mean(aspect_change()$status == "Meningkat", na.rm = TRUE)
      shinydashboard::valueBox(
        scales::percent(p, accuracy = 1), "Aspek meningkat",
        icon = shiny::icon("arrow-trend-up"), color = "green"
      )
    })
    output$vb_stagnant <- shinydashboard::renderValueBox({
      p <- mean(aspect_change()$status == "Stagnan", na.rm = TRUE)
      shinydashboard::valueBox(
        scales::percent(p, accuracy = 1), "Aspek stagnan",
        icon = shiny::icon("pause"), color = "yellow"
      )
    })
    output$trend <- shiny::renderPlot({
      aspect_scores() |>
        dplyr::group_by(periode, aspek) |>
        dplyr::summarise(mean = mean(skor_aspek, na.rm = TRUE), .groups = "drop") |>
        ggplot2::ggplot(ggplot2::aes(periode, mean, group = aspek, linetype = aspek)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::scale_y_continuous(limits = c(1, 4), breaks = 1:4) +
        ggplot2::labs(x = NULL, y = "Rata-rata skor", linetype = "Aspek") +
        ggplot2::theme_minimal(base_size = 12)
    })
  })
}
