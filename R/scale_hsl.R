
#' Hue saturation lightness colour scale
#'
#' Maps the values of several variables to coordinates in the HSL colour space.
#'
#' @inheritDotParams chromatic_scale
#' @inheritParams chromatic_scale
#'
#' @return A `ScaleChromatic` ggproto object that can be added to a plot.
#' @export
#' @name scale_hsl
#' @family colour space scales
#'
#' @examples
#' NULL
scale_colour_hsl <- function(
  ..., na.value = 0, guide = "none",
  aesthetics = c("colour", "hue", "saturation", "lightness")
) {
  scale <- chromatic_scale(
    aesthetics, "colour_hsl",
    hsl_palette, na.value = na.value, guide = guide, ...,
    prototype = hsl_spec,
    super = ScaleChromatic
  )
  scale
}

#' @rdname scale_hsl
#' @export
scale_fill_hsl <- function(
  ..., na.value = 0, guide = "none",
  aesthetics = c("fill", "hue", "saturation", "lightness")
) {
  scale <- chromatic_scale(
    aesthetics, "fill_hsl",
    hsl_palette, na.value = na.value, guide = guide, ...,
    prototype = hsl_spec,
    super = ScaleChromatic
  )
  scale
}


# Palette -----------------------------------------------------------------

#' HSL palette
#'
#' This HSL palette converts a `hsl_spec` vector to colours.
#'
#' @param x A `hsl_spec` vector
#' @param min,max A `numeric(1)` specifying the upper and lower limits
#'
#' @return A `character` vector of the same length as `x` with hexadecimal
#'   colour notation.
#' @export
#'
#' @examples
#' # By default, the limits are c(0, 1)
#' hsl_palette(hsl_spec(c(0, 0.5, 1), c(0.5, 0, 1), c(1, 0.5, 0)))
hsl_palette <- function(x, min = 0, max = 1) {
  if (!inherits(x, "hsl_spec")) {
    rlang::abort(glue::glue(
      "The `hsl_palette` only applies to `hsl_spec` vectors, not `{class(x)[[1]]}`"
    ))
  }
  x <- as.matrix(x)
  x <- pmax(pmin(x, max), min)
  r <- max - min
  x[, 1] <- x[, 1] * 360 / r
  x[, 2:3] <- x[, 2:3] * 100 / r
  farver::encode_colour(x, from = "hsl")
}
