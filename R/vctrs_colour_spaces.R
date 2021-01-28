#' Colour space vectors
#'
#' These functions create a vectors that store values specifying different
#' channels in a colour space.
#'
#' @param h,c,l,r,g,b,m,y,k Colour channels. Abbreviations are colour space
#'   specific and written in full in the **Functions** section. Can be a mix of
#'   `logical`, `integer`, `double`, `character` or `factor` vectors of equal
#'   length or length 1.
#'
#' @details These constructors are convenient for creating vectors that can be
#'   supplied to the `aes()` function in ggplot2. During plot building, a
#'   chromatic colour or fill scale will be assigned to coordinate the training
#'   and mapping of colour space vectors. Missing channels will get placeholder
#'   values that will not affect scale training.
#'
#' @name colour_spec
#' @return An `colour_spec`, `vctrs_rcrd` S3 vector.
#'
#' @seealso [rgb()][grDevices::rgb], [hsv()][grDevices::hsv],
#'   [hcl()][grDevices::hcl] and [farver::encode_colour()].
#'
#' @examples
#' NULL
NULL

#' @export
#' @describeIn colour_spec Hue, Chroma and Luminance colour space.
hcl_spec <- function(h = double(),
                     c = double(),
                     l = double()) {
  new_colour_spec(h = h, c = c, l = l, class = "hcl_spec")
}

#' @export
#' @describeIn colour_spec Hue, Saturation and Value colour space.
hsv_spec <- function(h = double(),
                     s = double(),
                     v = double()) {
  new_colour_spec(h = h, s = s, v = v, class = "hsv_spec")
}

#' @export
#' @describeIn colour_spec Hue, Saturation and Lightness colour space.
hsl_spec <- function(h = double(),
                     s = double(),
                     l = double()) {
  new_colour_spec(h = h, s = s, l = l, class = "hsl_spec")
}

#' @export
#' @describeIn colour_spec Red, Green and Blue colour space.
rgb_spec <- function(r = double(),
                     g = double(),
                     b = double()) {
  new_colour_spec(r = r, g = g, b = b, class = "rgb_spec")
}

#' @export
#' @describeIn colour_spec Cyan, Magenta, Yellow and Key (black) colour space.
cmyk_spec <- function(c = double(),
                      m = double(),
                      y = double(),
                      k = double()) {
  new_colour_spec(c = c, m = m, y = y, k = k, class = "cmyk_spec")
}

#' @export
#' @describeIn colour_spec Cyan, Magenta and Yellow colour space.
cmy_spec <- function(c = double(),
                     m = double(),
                     y = double()) {
  new_colour_spec(c = c, m = m, y = y, class = "cmy_spec")
}
