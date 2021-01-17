#' Cyan, magenta, yellow and black colour scale
#'
#' Maps the values of several variables to coordinates in the CMYK colour space.
#'
#' @inheritDotParams chromatic_scale
#' @inheritParams chromatic_scale
#'
#' @return A `ScaleChromatic` ggproto object that can be added to a plot.
#' @export
#' @name scale_cmyk
#'
#' @examples
#' NULL
scale_colour_cmyk <- function(
  ..., na.value = 0, guide = "none",
  limits = NULL,
  aesthetics = c("colour", "cyan", "magenta", "yellow", "key"))
{
  scale <- chromatic_scale(
    aesthetics, "colour_cmyk", limits = limits,
    cmyk_palette, na.value = na.value, guide = guide, ...,
    prototype = cmyk_spec,
    super = ScaleChromatic
  )
  scale
}

#' @export
#' @rdname scale_cmyk
scale_fill_cmyk <- function(
  ..., na.value = 0, guide = "none",
  limits = NULL,
  aesthetics = c("fill", "cyan", "magenta", "yellow", "key")
) {
  scale <- chromatic_scale(
    aesthetics, "fill_cmyk", limits = limits,
    cmyk_palette, na.value = na.value, guide = guide, ...,
    prototype = cmyk_spec,
    super = ScaleChromatic
  )
  scale
}

# Palette -----------------------------------------------------------------

#' CMYK palette
#'
#' This CMYK palette converts a `cmyk_spec` vector to colours.
#'
#' @param x A `cmyk_spec` vector
#' @param min,max A `numeric(1)` specifying the lower and upper limits.
#'
#' @return A `character` vector of the same length as `x` with hexadecimal
#'   colour notation.
#' @export
#'
#' @examples
#' # By default, the limits are c(0, 1)
#' cmyk_palette(cmyk_spec(c(0, 0.5, 1), c(0.5, 0, 1),
#'                        c(1, 0.5, 0), c(0.2, 0.4, 0.6)))
cmyk_palette <- function(x, min = 0, max = 1) {
  if (!inherits(x, "cmyk_spec")) {
    rlang::abort(glue::glue(
      "The `cmyk_palette` only applies to `cmyk_spec` vectors, not `{class(x)[[1]]}`"
    ))
  }

  x <- as.matrix(x)
  x <- pmax(pmin(x, max), min)
  scale <- 1 / (max - min)
  farver::encode_colour(as.matrix(x) * scale, from = "cmyk")
}
