mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shinydashboard::box(
      width = 6, title = "Batas Kategori", status = "primary", solidHeader = TRUE,
      shiny::numericInput(ns("cut_bb"), "Batas atas BB", 1.49, min = 1, max = 4, step = 0.01),
      shiny::numericInput(ns("cut_mb"), "Batas atas MB", 2.49, min = 1, max = 4, step = 0.01),
      shiny::numericInput(ns("cut_bsh"), "Batas atas BSH", 3.49, min = 1, max = 4, step = 0.01)
    ),
    shinydashboard::box(
      width = 6, title = "Deteksi Perubahan", status = "warning", solidHeader = TRUE,
      shiny::numericInput(ns("stagnation_delta"), "Batas stagnan (|Δ| ≤)", 0.15, min = 0, max = 1, step = 0.05),
      shiny::numericInput(ns("decline_delta"), "Batas penurunan (Δ <)", -0.15, min = -2, max = 0, step = 0.05),
      shiny::tags$div(
        class = "note",
        "Nilai ini dapat disesuaikan dengan karakteristik instrumen. Untuk skor ordinal 1–4, interpretasikan perubahan secara hati-hati."
      )
    )
  )
}

mod_settings_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive(list(
      cut_bb = input$cut_bb,
      cut_mb = input$cut_mb,
      cut_bsh = input$cut_bsh,
      stagnation_delta = input$stagnation_delta,
      decline_delta = input$decline_delta
    ))
  })
}
