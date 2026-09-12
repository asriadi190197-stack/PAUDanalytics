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
        width = 4,
        title = "Status Perkembangan",
        status = "primary",
        solidHeader = TRUE,
        shiny::plotOutput(ns("status_donut"), height = 330)
      ),
      shinydashboard::box(
        width = 8,
        title = "Capaian Akhir per Aspek",
        status = "primary",
        solidHeader = TRUE,
        shiny::plotOutput(ns("aspect_bar"), height = 330)
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        title = "Ringkasan Dashboard",
        status = "info",
        solidHeader = TRUE,
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::tags$div(
              class = "rec",
              shiny::tags$strong("Donut status"),
              shiny::tags$p("Menunjukkan proporsi aspek yang meningkat, stagnan, atau menurun dari awal ke akhir semester.")
            )
          ),
          shiny::column(
            width = 4,
            shiny::tags$div(
              class = "rec",
              shiny::tags$strong("Capaian akhir"),
              shiny::tags$p("Membandingkan rata-rata skor enam aspek perkembangan pada observasi akhir semester.")
            )
          ),
          shiny::column(
            width = 4,
            shiny::tags$div(
              class = "note",
              shiny::tags$strong("Catatan: "),
              "Status stagnan/menurun merupakan sinyal analitik untuk observasi lanjutan, bukan diagnosis perkembangan."
            )
          )
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
      if (!is.finite(p)) p <- 0
      shinydashboard::valueBox(
        scales::percent(p, accuracy = 1), "Aspek meningkat",
        icon = shiny::icon("arrow-trend-up"), color = "green"
      )
    })

    output$vb_stagnant <- shinydashboard::renderValueBox({
      p <- mean(aspect_change()$status == "Stagnan", na.rm = TRUE)
      if (!is.finite(p)) p <- 0
      shinydashboard::valueBox(
        scales::percent(p, accuracy = 1), "Aspek stagnan",
        icon = shiny::icon("pause"), color = "yellow"
      )
    })

    output$status_donut <- shiny::renderPlot({
      d <- aspect_change() |>
        dplyr::filter(!is.na(status)) |>
        dplyr::count(status, name = "n") |>
        dplyr::mutate(
          status = factor(status, levels = c("Meningkat", "Stagnan", "Menurun")),
          p = n / sum(n),
          label = scales::percent(p, accuracy = 1)
        ) |>
        dplyr::arrange(status)

      shiny::validate(shiny::need(nrow(d) > 0, "Belum ada perubahan Awal–Akhir yang dapat diringkas."))

      ggplot2::ggplot(d, ggplot2::aes(x = 2, y = n, fill = status)) +
        ggplot2::geom_col(width = 0.78, colour = "white", linewidth = 1.2) +
        ggplot2::coord_polar(theta = "y") +
        ggplot2::xlim(0.55, 2.5) +
        ggplot2::geom_text(
          ggplot2::aes(label = ifelse(p >= 0.06, label, "")),
          position = ggplot2::position_stack(vjust = 0.5),
          colour = "white", fontface = "bold", size = 4.6
        ) +
        ggplot2::annotate(
          "text", x = 0.55, y = 0,
          label = paste0(sum(d$n), "\nprofil aspek"),
          colour = paud_palette$navy, fontface = "bold", size = 4.7, lineheight = 0.95
        ) +
        ggplot2::scale_fill_manual(values = status_colors, drop = FALSE) +
        ggplot2::labs(fill = NULL, subtitle = "Perubahan skor awal ke akhir semester") +
        ggplot2::theme_void(base_size = 12) +
        ggplot2::theme(
          plot.subtitle = ggplot2::element_text(
            hjust = 0.5, colour = paud_palette$muted,
            margin = ggplot2::margin(b = 8)
          ),
          legend.position = "bottom",
          legend.text = ggplot2::element_text(colour = paud_palette$ink),
          plot.margin = ggplot2::margin(5, 8, 5, 8)
        )
    })

    output$aspect_bar <- shiny::renderPlot({
      scores <- aspect_scores()
      available_periods <- unique(as.character(scores$periode[!is.na(scores$periode)]))
      period_use <- if ("Akhir Semester" %in% available_periods) {
        "Akhir Semester"
      } else {
        tail(period_order[period_order %in% available_periods], 1)
      }

      shiny::validate(shiny::need(length(period_use) == 1 && !is.na(period_use), "Belum ada data periode yang dapat ditampilkan."))

      d <- scores |>
        dplyr::filter(as.character(periode) == period_use) |>
        dplyr::group_by(aspek) |>
        dplyr::summarise(mean = mean(skor_aspek, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(
          aspek = factor(as.character(aspek), levels = rev(aspect_order)),
          label = sprintf("%.2f", mean)
        )

      ggplot2::ggplot(d, ggplot2::aes(x = mean, y = aspek, fill = aspek)) +
        ggplot2::geom_col(width = 0.62, show.legend = FALSE) +
        ggplot2::geom_text(
          ggplot2::aes(label = label),
          hjust = -0.18, fontface = "bold", colour = paud_palette$navy, size = 4.1
        ) +
        ggplot2::scale_fill_manual(values = aspect_colors, drop = FALSE) +
        ggplot2::scale_x_continuous(
          limits = c(0, 4.35), breaks = 1:4,
          labels = c("1\nBB", "2\nMB", "3\nBSH", "4\nBSB"),
          expand = ggplot2::expansion(mult = c(0, 0))
        ) +
        ggplot2::labs(
          x = "Rata-rata skor",
          y = NULL,
          subtitle = paste("Observasi", period_use)
        ) +
        paud_theme(base_size = 12) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_line(colour = paud_palette$grid, linewidth = 0.45),
          axis.text.y = ggplot2::element_text(face = "bold", size = 10.5),
          plot.subtitle = ggplot2::element_text(colour = paud_palette$muted, margin = ggplot2::margin(b = 8)),
          plot.margin = ggplot2::margin(8, 30, 8, 8)
        )
    })
  })
}
