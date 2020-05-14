#' Title
#'
#' @param palette
#' @param direction
#'
#' @return
#' @export
#'
#' @examples
#' names(pal_list)
#' scales::show_col(color_pal("Reds")(5))
color_pal <- function(palette, direction = 1) {
  function(n) {
    pal <- grDevices::colorRampPalette(pal_list[[palette]]$hex, space = "rgb")(n)
    if (direction == -1) {
      pal <- rev(pal)
    }
    pal
  }
}
#' Title
#'
#' @param palette
#' @param direction
#' @param aesthetics
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' (d <- ggplot(dsamp, aes(carat, price)) +
#'     geom_point(aes(colour = clarity)))
#' d + scale_colour_brewer()
#' d + scale_colors("Viridis")
#' d + scale_colors("Viridis", aesthetics = "fill")
#' d + scale_colors("Reds")
#' d + scale_colors("Earth")
#' d + scale_colors("Portland")
scale_colors <- function(palette, direction = 1, aesthetics = "colour", ...) {
  ggplot2::discrete_scale(aesthetics, "colorPal", color_pal(palette, direction), ...)
}
