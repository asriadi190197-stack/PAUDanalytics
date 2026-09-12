mod_narrative_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 4, title = "Pilih Anak", status = "primary", solidHeader = TRUE,
        shiny::selectInput(ns("child"), "Anak", choices = NULL)
      ),
      shinydashboard::box(
        width = 8, title = "Narasi Perkembangan Otomatis", status = "info", solidHeader = TRUE,
        shiny::uiOutput(ns("narrative"))
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12, title = "Rekomendasi per Aspek", status = "warning", solidHeader = TRUE,
        shiny::uiOutput(ns("recommendations"))
      )
    )
  )
}

mod_narrative_server <- function(id, validated, aspect_scores, aspect_change) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      choices_df <- validated() |>
        dplyr::distinct(id_anak, nama_anak) |>
        dplyr::arrange(nama_anak)
      shiny::updateSelectInput(session, "child", choices = stats::setNames(choices_df$id_anak, choices_df$nama_anak))
    })

    output$narrative <- shiny::renderUI({
      shiny::req(input$child)
      child_as <- aspect_scores() |> dplyr::filter(id_anak == input$child)
      change <- aspect_change() |> dplyr::filter(id_anak == input$child)
      shiny::tags$div(
        class = "rec",
        shiny::tags$p(style = "font-size:16px;line-height:1.7;", make_narrative(child_as, change)),
        shiny::tags$p(
          class = "text-muted",
          "Narasi dihasilkan otomatis dari pola skor dan aturan analitik. Guru tetap perlu menambahkan bukti autentik seperti catatan anekdot, hasil karya, foto kegiatan, atau konteks perilaku anak."
        )
      )
    })

    output$recommendations <- shiny::renderUI({
      shiny::req(input$child)
      change <- aspect_change() |> dplyr::filter(id_anak == input$child)
      shiny::tagList(lapply(seq_len(nrow(change)), function(i) {
        row <- change[i, ]
        final_cat <- if ("kategori__Akhir Semester" %in% names(row)) {
          row[["kategori__Akhir Semester"]]
        } else {
          NA_character_
        }
        shiny::tags$div(
          class = "rec",
          shiny::tags$h4(as.character(row$aspek)),
          shiny::tags$strong(paste("Status:", row$status, "| Δ =", round(row$delta, 2))),
          shiny::tags$p(recommendation_text(as.character(row$aspek), row$status, final_cat))
        )
      }))
    })
  })
}
