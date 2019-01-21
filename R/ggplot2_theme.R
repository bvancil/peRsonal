#' Title
#'
#' @param base_size base font size in points (11)
#' @param base_family base font family ('Lucida Sans Unicode')
#' @param base_line_size base line size in units of 22pt (\code{base_size/22})
#' @param base_rect_size base rectangle size in units of 22pt (\code{base_size/22})
#'
#' @return a ggtheme()-like object
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(extrafont)
#'   windowsFonts()
#'   extrafont::font_import()
#'   extrafont::loadfonts(device="win")
#'   windowsFonts()
#'   ggplot2::ggplot(data = iris, ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#'     ggplot2::geom_point() +
#'     ggplot2::labs(title = 'Iris setosa is separable, but versicolor and verginica are not.') +
#'     theme_modern()
#' }
theme_modern <- function(
  base_size = 11, base_family = "Lucida Sans Unicode", base_line_size = base_size/22,
  base_rect_size = base_size/22
) {
  grid_color <- "grey92"
  tick_color <- "grey80"
  axis_label_color <- "grey40"

  half_line <- base_size / 2
  quarter_line <- half_line / 2

  ggplot2::theme_grey(
    base_size = base_size, base_family = base_family,
    base_line_size = base_line_size, base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = "white", colour = NA
      ),
      panel.border = ggplot2::element_rect(
        fill = NA, colour = tick_color
      ),
      panel.grid = ggplot2::element_line(
          colour = grid_color
      ),
      panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(0.5)),
      strip.background = ggplot2::element_rect(
        fill = "grey85", colour = tick_color
      ),
      legend.key = ggplot2::element_rect(
        fill = "white", colour = NA
      ),
      # Tick labels
      axis.text = ggplot2::element_text(size = ggplot2::rel(0.8), colour = tick_color),
      axis.ticks = ggplot2::element_line(colour = tick_color),
      # Axis labels
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = quarter_line), vjust = 1,
        colour = axis_label_color
      ),
      axis.title.x.top = ggplot2::element_text(
        margin = ggplot2::margin(b = quarter_line), vjust = 0,
        colour = axis_label_color
      ),
      axis.title.y = ggplot2::element_text(
        angle = 90, margin = ggplot2::margin(r = quarter_line), vjust = 1,
        colour = axis_label_color
      ),
      axis.title.y.right = ggplot2::element_text(
        angle = -90, margin = ggplot2::margin(l = quarter_line), vjust = 0,
        colour = axis_label_color
      ),
      complete = TRUE
    )
}

