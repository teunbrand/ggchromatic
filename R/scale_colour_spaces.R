#' Colour space scales
#'
#' These scales map the values of `colour_spec` vectors to coordinates in a
#' colour space.
#'
#' @inheritDotParams chromatic_scale
#' @inheritParams chromatic_scale
#'
#' @return A `ScaleChromatic` ggproto object that can be added to a plot.
#'
#' @name scale_chromatic
#'
#' @section Functions:
#'  * `scale_*_rgb()`: Red, Green and Blue colour space.
#'  * `scale_*_hsv()`: Hue, Saturation and Value colour space.
#'  * `scale_*_hsl()`: Hue, Saturation and Lightness colour space.
#'  * `scale_*_hcl()`: Hue, Chroma and Luminance colour space.
#'  * `scale_*_cmyk()`: Cyan, Magenta, Yellow and Key (black) colour space.
#'  * `scale_*_cmy()`: Cyan, Magenta and Yellow colour space.
#'
#' @examples
#' NULL
NULL

# RGB ---------------------------------------------------------------------

#' @export
#' @rdname scale_chromatic
scale_colour_rgb <- function(..., aesthetics = "colour") {
  chromatic_scale(aesthetics, "colour_rgb", rgb_palette, prototype = rgb_spec)
}

#' @export
#' @rdname scale_chromatic
scale_fill_rgb <- function(..., aesthetics = "fill") {
  chromatic_scale(aesthetics, "fill_rgb", rgb_palette, prototype = rgb_spec)
}

# HSV ---------------------------------------------------------------------

#' @export
#' @rdname scale_chromatic
scale_colour_hsv <- function(..., aesthetics = "colour") {
  chromatic_scale(aesthetics, "colour_hsv", hsv_palette, prototype = hsv_spec)
}

#' @export
#' @rdname scale_chromatic
scale_fill_hsv <- function(..., aesthetics = "fill") {
  chromatic_scale(aesthetics, "fill_hsv", hsv_palette, prototype = hsv_spec)
}

# HSL ---------------------------------------------------------------------

#' @export
#' @rdname scale_chromatic
scale_colour_hsl <- function(..., aesthetics = "colour") {
  chromatic_scale(aesthetics, "colour_hsl", hsl_palette, prototype = hsl_spec)
}

#' @export
#' @rdname scale_chromatic
scale_fill_hsl <- function(..., aesthetics = "fill") {
  chromatic_scale(aesthetics, "fill_hsl", hsl_palette, prototype = hsl_spec)
}

# HCL ---------------------------------------------------------------------

#' @export
#' @rdname scale_chromatic
scale_colour_hcl <- function(..., aesthetics = "colour") {
  chromatic_scale(aesthetics, "colour_hcl", hcl_palette, prototype = hcl_spec)
}

#' @export
#' @rdname scale_chromatic
scale_fill_hcl <- function(..., aesthetics = "fill") {
  chromatic_scale(aesthetics, "fill_hcl", hcl_palette, prototype = hcl_spec)
}

# CMYK --------------------------------------------------------------------

#' @export
#' @rdname scale_chromatic
scale_colour_cmyk <- function(..., aesthetics = "colour") {
  chromatic_scale(aesthetics, "colour_cmyk",
                  cmyk_palette, prototype = cmyk_spec)
}

#' @export
#' @rdname scale_chromatic
scale_fill_cmyk <- function(..., aesthetics = "fill") {
  chromatic_scale(aesthetics, "fill_cmyk", cmyk_palette, prototype = cmyk_spec)
}

# CMY ---------------------------------------------------------------------

#' @export
#' @rdname scale_chromatic
scale_colour_cmy <- function(..., aesthetics = "colour") {
  chromatic_scale(aesthetics, "colour_cmy", cmy_palette, prototype = cmy_spec)
}

#' @export
#' @rdname scale_chromatic
scale_fill_cmy <- function(..., aesthetics = "fill") {
  chromatic_scale(aesthetics, "fill_cmy", cmy_palette, prototype = cmy_spec)
}


