library(shiny)
library(shinydashboard)

# PAUDanalytics v0.4.3
# Modular application architecture.

source(file.path("R", "config.R"), local = TRUE)
source(file.path("R", "helpers.R"), local = TRUE)
source(file.path("R", "demo_data.R"), local = TRUE)
source(file.path("R", "theme.R"), local = TRUE)

module_files <- list.files("modules", pattern = "\\.R$", full.names = TRUE)
for (module_file in module_files) {
  source(module_file, local = TRUE)
}

template_df <- make_demo_data()

ui <- shinydashboard::dashboardPage(
  skin = "blue",
  shinydashboard::dashboardHeader(title = "PAUDanalytics v0.4.3"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Beranda", tabName = "home", icon = shiny::icon("home")),
      shinydashboard::menuItem("Data Longitudinal", tabName = "data", icon = shiny::icon("file-excel")),
      shinydashboard::menuItem("Perkembangan Kelas", tabName = "class", icon = shiny::icon("chart-line")),
      shinydashboard::menuItem("Profil Anak", tabName = "child", icon = shiny::icon("child")),
      shinydashboard::menuItem("Capaian Pembelajaran (CP)", tabName = "cp", icon = shiny::icon("book-open")),
      shinydashboard::menuItem("Perubahan Indikator", tabName = "indicator", icon = shiny::icon("magnifying-glass-chart")),
      shinydashboard::menuItem("Narasi & Rekomendasi", tabName = "narrative", icon = shiny::icon("file-lines")),
      shinydashboard::menuItem("Ekspor Laporan", tabName = "export", icon = shiny::icon("download")),
      shinydashboard::menuItem("Pengaturan", tabName = "settings", icon = shiny::icon("gear")),
      shinydashboard::menuItem("Tentang", tabName = "about", icon = shiny::icon("circle-info"))
    )
  ),
  shinydashboard::dashboardBody(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        ".content-wrapper{background:#F4F7FA}.main-header .logo{background:#17324D!important;font-weight:700}.main-header .navbar{background:#17324D!important}.main-sidebar{background:#142536!important}.sidebar-menu>li.active>a{border-left-color:#58B4AE!important;background:#1D364D!important}.box{border-radius:14px;border-top:0!important;box-shadow:0 8px 24px rgba(23,50,77,.08);overflow:hidden}.box-header{padding:14px 16px}.box-title{font-weight:700;color:#17324D}.small-box{border-radius:14px;box-shadow:0 8px 24px rgba(23,50,77,.08)}.small-box h3{font-size:27px}.content-header h1{color:#17324D;font-weight:700}.btn-primary,.btn-info{border:0;border-radius:8px}.note{padding:14px;background:#FFF9EE;border-left:4px solid #D7A84B;border-radius:8px}.rec{padding:14px;margin-bottom:10px;background:white;border:1px solid #E5EAF0;border-radius:10px}.dataTables_wrapper{font-size:13px}.form-control{border-radius:8px}.nav-tabs-custom,.well{border-radius:12px}"
      ))
    ),
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "home", mod_home_ui("home")),
      shinydashboard::tabItem(tabName = "data", mod_data_ui("data")),
      shinydashboard::tabItem(tabName = "class", mod_class_ui("class")),
      shinydashboard::tabItem(tabName = "child", mod_child_ui("child")),
      shinydashboard::tabItem(tabName = "cp", mod_cp_ui("cp")),
      shinydashboard::tabItem(tabName = "indicator", mod_indicator_ui("indicator")),
      shinydashboard::tabItem(tabName = "narrative", mod_narrative_ui("narrative")),
      shinydashboard::tabItem(tabName = "export", mod_export_ui("export")),
      shinydashboard::tabItem(tabName = "settings", mod_settings_ui("settings")),
      shinydashboard::tabItem(
        tabName = "about",
        shiny::fluidRow(
          shinydashboard::box(
            width = 12, title = "Tentang PAUDanalytics", status = "primary", solidHeader = TRUE,
            shiny::h3("PAUDanalytics 0.4.3"),
            shiny::p("Aplikasi analisis observasi longitudinal PAUD untuk mendukung pemantauan perkembangan anak, pemetaan STPPA, Capaian Pembelajaran Fase Fondasi, dan penyusunan rapor perkembangan."),
            shiny::tags$hr(),
            shiny::h4("Tentang Pengembang Aplikasi"),
            shiny::tags$div(
              style = "padding:18px;background:#F7FAFC;border:1px solid #E5EAF0;border-radius:12px;margin-bottom:16px;",
              shiny::h4("DR. Muh. Asriadi AM, M.Pd.", style = "margin-top:0;color:#17324D;font-weight:700;"),
              shiny::p(shiny::tags$strong("Afiliasi: "), "Dosen UPI Kampus Cibiru"),
              shiny::p(shiny::tags$strong("Email: "), shiny::tags$a(href = "mailto:muhasriadi@upi.edu", "muhasriadi@upi.edu")),
              shiny::p("Pengembang PAUDanalytics.", style = "margin-bottom:0;color:#5D6B78;")
            ),
            shiny::tags$div(
              class = "note",
              shiny::tags$strong("Catatan metodologis: "),
              "Indikator operasional di aplikasi dapat disesuaikan dengan instrumen lembaga. Hasil analitik merupakan dukungan asesmen dan bukan diagnosis perkembangan."
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  settings <- mod_settings_server("settings")
  raw_data <- mod_data_server("data", template_df = template_df)

  validated <- shiny::reactive({
    d <- as.data.frame(raw_data())
    required <- c("id_anak", "nama_anak", "kelompok", "usia_bulan", "periode")
    missing_cols <- setdiff(required, names(d))
    if (length(missing_cols) > 0) {
      stop(paste("Kolom wajib belum ada:", paste(missing_cols, collapse = ", ")), call. = FALSE)
    }

    valid_indicators <- intersect(indicator_meta$kode, names(d))
    if (length(valid_indicators) == 0) {
      stop("Tidak ada kolom indikator yang dikenali.", call. = FALSE)
    }

    d$periode <- factor(as.character(d$periode), levels = period_order, ordered = TRUE)
    for (k in valid_indicators) {
      d[[k]] <- suppressWarnings(as.numeric(d[[k]]))
    }
    d
  })

  long_indicator <- shiny::reactive({
    d <- validated()
    indicators <- intersect(indicator_meta$kode, names(d))
    d |>
      tidyr::pivot_longer(dplyr::all_of(indicators), names_to = "kode", values_to = "skor") |>
      dplyr::left_join(indicator_meta, by = "kode")
  })

  aspect_scores <- shiny::reactive({
    cfg <- settings()
    long_indicator() |>
      dplyr::group_by(id_anak, nama_anak, kelompok, usia_bulan, periode, aspek) |>
      dplyr::summarise(skor_aspek = mean(skor, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(
        kategori = category_code(skor_aspek, cfg$cut_bb, cfg$cut_mb, cfg$cut_bsh),
        aspek = factor(aspek, levels = aspect_order)
      )
  })


  cp_scores <- shiny::reactive({
    cfg <- settings()
    long_indicator() |>
      dplyr::group_by(id_anak, nama_anak, kelompok, usia_bulan, periode, cp_elemen) |>
      dplyr::summarise(skor_cp = mean(skor, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(kategori = category_code(skor_cp, cfg$cut_bb, cfg$cut_mb, cfg$cut_bsh))
  })

  status_from_delta <- function(delta) {
    cfg <- settings()
    dplyr::case_when(
      is.na(delta) ~ NA_character_,
      delta < cfg$decline_delta ~ "Menurun",
      abs(delta) <= cfg$stagnation_delta ~ "Stagnan",
      TRUE ~ "Meningkat"
    )
  }

  aspect_change <- shiny::reactive({
    a <- aspect_scores() |>
      dplyr::select(id_anak, nama_anak, aspek, periode, skor_aspek, kategori) |>
      tidyr::pivot_wider(names_from = periode, values_from = c(skor_aspek, kategori), names_sep = "__")

    initial_col <- "skor_aspek__Awal Semester"
    final_col <- "skor_aspek__Akhir Semester"
    if (!(initial_col %in% names(a)) || !(final_col %in% names(a))) {
      return(a |> dplyr::mutate(delta = NA_real_, status = NA_character_))
    }

    a |>
      dplyr::mutate(
        delta = .data[[final_col]] - .data[[initial_col]],
        status = status_from_delta(delta)
      )
  })

  indicator_change <- shiny::reactive({
    x <- long_indicator() |>
      dplyr::group_by(id_anak, nama_anak, aspek, kode, indikator, periode) |>
      dplyr::summarise(skor = mean(skor, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = periode, values_from = skor)

    if (!all(c("Awal Semester", "Akhir Semester") %in% names(x))) {
      return(x |> dplyr::mutate(delta = NA_real_, status = NA_character_))
    }

    x |>
      dplyr::mutate(
        delta = `Akhir Semester` - `Awal Semester`,
        status = status_from_delta(delta)
      )
  })

  mod_home_server("home", validated, aspect_scores, aspect_change)
  mod_class_server("class", aspect_scores, aspect_change)
  mod_child_server("child", validated, aspect_scores, aspect_change)
  mod_cp_server("cp", validated, cp_scores)
  mod_indicator_server("indicator", validated, long_indicator, indicator_change)
  mod_narrative_server("narrative", validated, aspect_scores, aspect_change)
  mod_export_server("export", validated, aspect_scores, aspect_change, indicator_change, cp_scores)
}

shiny::shinyApp(ui, server)
