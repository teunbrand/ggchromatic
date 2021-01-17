#' Hue saturation value scale
#'
#' Maps the values of several variables to coordinates in the HSV colour space.
#'
#' @inheritDotParams chromatic_scale
#' @inheritParams chromatic_scale
#'
#' @return A `ScaleChromatic` ggproto object that can be added to a plot.
#' @export
#' @name scale_hsv
#' @family colour space scales
#'
#' @examples
#' NULL
scale_colour_hsv <- function(..., na.value = 0, guide = "none",
                             limits = NULL,
                             aesthetics = c("colour", "hue", "saturation", "value")) {
  scale <- chromatic_scale(
    aesthetics, "colour_hsv", limits = limits,
    hsv_palette, na.value = na.value, guide = guide, ...,
    prototype = hsv_spec,
    super = ScaleChromatic
  )
  scale
}

#' @export
#' @rdname scale_hsv
scale_fill_hsv <- function(..., na.value = 0, guide = "none",
                             limits = NULL,
                             aesthetics = c("fill", "hue", "saturation", "value")) {
  scale <- chromatic_scale(
    aesthetics, "colour_hsv", limits = limits,
    hsv_palette, na.value = na.value, guide = guide, ...,
    prototype = hsv_spec,
    super = ScaleChromatic
  )
  scale
}

# Palette -----------------------------------------------------------------

#' HSV palette
#'
#' This HSV palete converts a `hsv_spec` vector to colours.
#'
#' @param x A `hsv_spec` vector
#' @param min,max A `numeric(1)` specifying the upper and lower limits.
#'
#' @return A `character` vector of the same length as `x` with hexadecimal
#'   colour notation.
#' @export
#'
#' @examples
#' # By default, the limits are c(0, 1)
#' hsv_palette(hsv_spec(c(0, 0.5, 1), c(0.5, 0, 1), c(1, 0.5, 0)))
hsv_palette <- function(x, min = 0, max = 1) {
  if (!inherits(x, "hsv_spec")) {
    rlang::abort(glue::glue(
      "The `hsv_palette` only applies to `hsv_spec` vectors, not `{class(x)[[1]]}`"
    ))
  }
  x <- as.matrix(x)
  x <- pmax(pmin(x, max), min)
  r <- max - min
  x[, 1] <- x[, 1] * 360 / r
  x[, 2:3] <- x[, 2:3] * 1 / r
  farver::encode_colour(x, from = "hsv")
}
