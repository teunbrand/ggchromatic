#' Cyan, magenta and yellow colour scale
#'
#' Maps the values of several variables to coordinates in the CMY colour space.
#'
#' @inheritDotParams chromatic_scale
#' @inheritParams chromatic_scale
#'
#' @return A `ScaleChromatic` ggproto object that can be added to a plot.
#' @export
#' @name scale_cmy
#' @family colour space scales
#'
#' @examples
#' NULL
scale_colour_cmy <- function(
  ..., na.value = "grey50",
  aesthetics = c("colour", "cyan", "magenta", "yellow"))
{
  scale <- chromatic_scale(
    aesthetics, "colour_cmy",
    cmy_palette, na.value = na.value, ...,
    prototype = cmy_spec
  )
  scale
}

#' @export
#' @rdname scale_cmyk
scale_fill_cmy <- function(
  ..., na.value = "grey50",
  aesthetics = c("fill", "cyan", "magenta", "yellow")
) {
  scale <- chromatic_scale(
    aesthetics, "fill_cmy",
    cmy_palette, na.value = na.value, ...,
    prototype = cmy_spec
  )
  scale
}

#' CMY palette
#'
#' This CMY palette converts a `cmy_spec` vector to colours.
#'
#' @param x A `cmy_spec` vector
#' @param min,max A `numeric(1)` specifying the lower and upper limits.
#'
#' @return A `character` vector of the same length as `x` with hexadecimal
#'   colour notation.
#' @export
#'
#'
#' @examples
#' # By default, the limits are c(0, 1)
#' cmy_palette(cmy_spec(c(0, 0.5, 1), c(0.5, 0, 1), c(1, 0.5, 0)))
cmy_palette <- function(x, min = 0, max = 1) {
  if (!inherits(x, "cmy_spec")) {
    rlang::abort(glue::glue(
      "The `cmy_palette` only applies to `cmy_spec` vectors, not `{class(x)[[1]]}`"
    ))
  }

  x <- as.matrix(x)
  x <- pmax(pmin(x, max), min)
  scale <- 1 / (max - min)
  farver::encode_colour(as.matrix(x) * scale, from = "cmy")
}
