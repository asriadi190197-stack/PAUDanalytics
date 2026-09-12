# Visual system for PAUDanalytics v0.4.1
paud_palette <- list(
  navy = "#17324D",
  teal = "#2B7A78",
  aqua = "#58B4AE",
  gold = "#D7A84B",
  rose = "#C96B78",
  blue = "#4D7EA8",
  green = "#5B9A78",
  purple = "#7B6FA6",
  ink = "#25313C",
  muted = "#667788",
  grid = "#E8EDF2",
  paper = "#FFFFFF",
  background = "#F4F7FA"
)

aspect_colors <- c(
  "Nilai Agama dan Moral" = "#2B7A78",
  "Nilai Pancasila" = "#D7A84B",
  "Fisik Motorik" = "#4D7EA8",
  "Kognitif" = "#7B6FA6",
  "Bahasa" = "#C96B78",
  "Sosial-Emosional" = "#5B9A78"
)

category_colors <- c(
  "BB" = "#D9DEE5",
  "MB" = "#E7B963",
  "BSH" = "#67A6A0",
  "BSB" = "#2B7A78"
)

status_colors <- c(
  "Menurun" = "#C96B78",
  "Stagnan" = "#D7A84B",
  "Meningkat" = "#2B7A78"
)

paud_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 3, colour = paud_palette$navy),
      plot.subtitle = ggplot2::element_text(size = base_size, colour = paud_palette$muted, margin = ggplot2::margin(b = 10)),
      axis.title = ggplot2::element_text(face = "bold", colour = paud_palette$ink),
      axis.text = ggplot2::element_text(colour = paud_palette$ink),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = paud_palette$grid, linewidth = 0.45),
      strip.background = ggplot2::element_rect(fill = "#EEF4F6", colour = NA),
      strip.text = ggplot2::element_text(face = "bold", colour = paud_palette$navy),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(14, 18, 12, 12)
    )
}

score_scale <- function() {
  ggplot2::scale_y_continuous(
    limits = c(0.85, 4.25), breaks = 1:4,
    labels = c("1\nBB", "2\nMB", "3\nBSH", "4\nBSB"),
    expand = ggplot2::expansion(mult = c(0.04, 0.08))
  )
}
