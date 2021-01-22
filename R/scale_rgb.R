#' Red green blue colour scale
#'
#' Maps the values of several variables to coordinates in the RGB colour space.
#'
#' @inheritDotParams chromatic_scale
#' @inheritParams chromatic_scale
#'
#' @return A `ScaleChromatic` ggproto object that can be added to a plot.
#' @export
#' @name scale_rgb
#' @family colour space scales
#'
#' @examples
#' NULL
scale_colour_rgb <- function(..., na.value = "grey50",
                             aesthetics = c("colour", "red", "green", "blue")) {
  scale <- chromatic_scale(
    aesthetics, "colour_rgb",
    rgb_palette, na.value = na.value, ...,
    prototype = rgb_spec
  )
  scale
}

#' @rdname scale_rgb
#' @export
scale_fill_rgb <- function(..., na.value = "grey50",
                           aesthetics = c("fill", "red", "green", "blue")) {
  scale <- chromatic_scale(
    aesthetics, "fill_rgb",
    rgb_palette, na.value = na.value, ...,
    prototype = rgb_spec
  )
  scale
}

# Palette -----------------------------------------------------------------

#' RGB palette
#'
#' This RGB palette converts a `rgb_spec` vector to colours.
#'
#' @param x A `rgb_spec` vector
#' @param min,max A `numeric(1)` specifying the upper and lower limits.
#'
#' @return A `character` vector of the same length as `x` with hexadecimal
#'   colour notation.
#' @export
#'
#' @examples
#' # By default, the limits are c(0, 1)
#' rgb_palette(rgb_spec(c(0, 0.5, 1), c(0.5, 0, 1), c(1, 0.5, 0)))
rgb_palette <- function(x, min = 0, max = 1) {
  if (!inherits(x, "rgb_spec")) {
    rlang::abort(glue::glue(
      "The `rgb_palette` only applies to `rgb_spec` vectors, not `{class(x)[[1]]}`"
    ))
  }
  x <- as.matrix(x)
  x <- pmax(pmin(x, max), min)
  scale <- 255 / (max - min)
  farver::encode_colour(as.matrix(x) * scale, from = "rgb")
}
