#' Colour space palettes
#'
#' These palette functions transform all-numeric `colour_spec` vectors into
#' colours.
#'
#' @param x An `colour_spec` vector with a `numeric` vector in every channel.
#' @param min,max A `numeric(1)` specifying the lower and upper limits
#'   respectively.
#'
#' @return A `character` vector of the same length as the `x` argument with
#'   hexadecimal colour notation.
#' @name colourspace_palettes
#'
#' @details The conversion from `colour_spec` vectors to colour is powered by
#'   the [farver::encode_colour()] function.
#'
#' @seealso The [colour_spec] page for building colour space vectors. The
#'   [scale_chromatic] page for the scales wherein these palettes are used.
#'
#' @examples
#' # Getting the colour "red" in different colour spaces
#' rgb_palette(rgb_spec(1, 0, 0))
#' hsv_palette(hsv_spec(0, 1, 1))
#' hsl_palette(hsl_spec(0, 1, 0.5))
#' hcl_palette(hcl_spec(0.034, 0.995, 0.532))
#' cmyk_palette(cmyk_spec(0, 1, 1, 0))
#' cmy_palette(cmy_spec(0, 1, 1))
#' lab_palette(lab_spec(0.5, 1, 1))
NULL

# RGB ---------------------------------------------------------------------

#' @export
#' @describeIn colourspace_palettes Red, Green and Blue colour space.
rgb_palette <- function(x, min = 0, max = 1) {
  check_palette(x, "rgb")
  x <- pal_transform(x, min, max, 255, 255, 255)
  encode_colour(x, from = "rgb")
}

# HSV ---------------------------------------------------------------------

#' @export
#' @describeIn colourspace_palettes Hue, Saturation and Value colour space.
hsv_palette <- function(x, min = 0, max = 1) {
  check_palette(x, "hsv")
  x <- pal_transform(x, min, max, 360, 1, 1)
  encode_colour(x, from = "hsv")
}

# HSL ---------------------------------------------------------------------

#' @export
#' @describeIn colourspace_palettes Hue, Saturation and Lightness colour space.
hsl_palette <- function(x, min = 0, max = 1) {
  check_palette(x, "hsl")
  x <- pal_transform(x, min, max, 360, 100, 100)
  encode_colour(x, from = "hsl")
}

# HCL ---------------------------------------------------------------------

#' @export
#' @describeIn colourspace_palettes Hue, Chroma and Luminance colour space.
hcl_palette <- function(x, min = 0, max = 1) {
  check_palette(x, "hcl")
  x <- pal_transform(x, min, max, 360, 180, 100)
  encode_colour(x, from = "hcl")
}

# CMYK --------------------------------------------------------------------

#' @export
#' @describeIn colourspace_palettes Cyan, Magenta, Yellow and Key (black) colour
#'  space.
cmyk_palette <- function(x, min = 0, max = 1) {
  check_palette(x, "cmyk")
  x <- pal_transform(x, min, max, 1, 1, 1, 1)
  encode_colour(x, from = "cmyk")
}

# CMY ---------------------------------------------------------------------

#' @export
#' @describeIn colourspace_palettes Cyan, Magenta and Yellow colour space.
cmy_palette <- function(x, min = 0, max = 1) {
  check_palette(x, "cmy")
  x <- pal_transform(x, min, max, 1, 1, 1)
  encode_colour(x, from = "cmy")
}


# L*a*b* ------------------------------------------------------------------

#' @export
#' @describeIn colourspace_palettes Lightness*, a* (green-red axis), b*
#'   (blue-yellow axis) colour space. Also known as CIE L*ab.
lab_palette <- function(x, min = 0, max = 1) {
  check_palette(x, "lab")
  x <- pal_transform(x, min, max, 100, 184.4, 202.3)
  x[, 2] <- x[, 2] - 86.2
  x[, 3] <- x[, 3] - 107.8
  encode_colour(x, from = "lab")
}

# OK Lab ------------------------------------------------------------------

#' @export
#' @describeIn colourspace_palettes An OK Lightness, a (green-red axis), b
#'   (blue-yellow axis) colour space. Also known as OKLab. Needs farver package
#'   version \>2.0.3.
oklab_palette <- function(x, min = 0, max = 1) {
  abort_farver("2.0.3", "(dev)")
  check_palette(x, "oklab")
  x <- pal_transform(x, min, max, 1, 0.51, 0.51)
  x[, 2] <- x[, 2] - 0.23
  x[, 3] <- x[, 3] - 0.31
  encode_colour(x, from = "oklab")
}

# Helpers -----------------------------------------------------------------

# The dot argument should contain scaling factors for the range that the
# channel accepts in `farver::encode_colour`.
pal_transform <- function(x, min, max, ...) {
  dots <- list2(...)
  x <- as.matrix(x)
  x <- pmax(pmin(x, max), min)
  range <- abs(max - min)
  for (i in seq_along(dots)) {
    x[, i] <- x[, i] * dots[[i]] / range
  }
  return(x)
}

check_palette <- function(x, type) {
  if (!inherits(x, paste0(type, "_spec"))) {
    vec <- paste0(type, "_spec")
    pal <- paste0(type, "_palette")
    input <- vec_ptype_full(x)
    rlang::abort(glue::glue(
      "The `{pal}` only applies to `{vec}` vectors, not `{input}` objects."
    ))
  }
}

# This is the function used to determine how [0-1] input should be transformed
# to get to the range the palette handles.
estimate_palette <- function(space) {
  colours <- c(rainbow(1000), colors())
  space <- farver::decode_colour(colours, to = space)
  space <- apply(space, 2, function(x) {
    c(min = min(x), max = max(x), diff = diff(range(x)))
  })
  return(space)
}
