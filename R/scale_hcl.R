#' Hue chroma luminance colour scale
#'
#' Maps the values of several variables to coordinates in HCL colour space.
#'
#' @inheritDotParams chromatic_scale
#' @inheritParams chromatic_scale
#'
#' @return A `ScaleChromatic` ggproto object that can be added to a plot.
#' @export
#' @name scale_hcl
#' @family colour space scales
#'
#' @examples
#' NULL
scale_colour_hcl <- function(..., na.value = "grey50",
                             aesthetics = c("colour", "hue", "chroma", "luminance")) {
  scale <- chromatic_scale(
    aesthetics, "colour_hcl",
    hcl_palette, na.value = na.value, ...,
    prototype = hcl_spec
  )
  scale
}

#' @rdname scale_hcl
#' @export
scale_fill_hcl <- function(..., na.value = "grey50",
                           aesthetics = c("fill", "hue", "chroma", "luminance")) {
  scale <- chromatic_scale(
    aesthetics, "fill_hcl",
    hcl_palette, na.value = na.value, ...,
    prototype = hcl_spec
  )
  scale
}

# Palette -----------------------------------------------------------------

#' HCL palette
#'
#' This HCL palette converts a `hcl_spec` vector to colours.
#'
#' @param x A `hcl_spec` vector
#' @param min,max A `numeric(1)` specifying the upper and lower limits.
#'
#' @return A `character` vector of the same length as `x` with hexadecimal
#'   colour notation.
#' @export
#'
#' @examples
#' # By default, the limits are c(0, 1)
#' hcl_palette(hcl_spec(c(0, 0.5, 1), c(0.5, 0, 1), c(1, 0.5, 0)))
hcl_palette <- function(x, min = 0, max = 1) {
  if (!inherits(x, "hcl_spec")) {
    rlang::abort(glue::glue(
      "The `hcl_palette` only applies to `hcl_spec` vectors, not `{class(x)[[1]]}`"
    ))
  }
  x <- as.matrix(x)
  x <- pmax(pmin(x, max), min)
  r <- max - min
  x[, 1] <- x[, 1] * 360 / r
  x[, 2] <- x[, 2] * 180 / r
  x[, 3] <- x[, 3] * 100 / r
  farver::encode_colour(x, from = "hcl")
}
