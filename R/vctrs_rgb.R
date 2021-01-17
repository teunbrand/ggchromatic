# Constructors ------------------------------------------------------------

#' Red-Green-Blue specification
#'
#' Creates a vector that stores values for red, green and blue channels in the
#' RGB colour space.
#'
#' @param r,g,b Vectors with values for the red, green and blue channels
#'   respectively.
#'
#' @return A `rgb_spec`, `vctrs_rcrd` S3 class object.
#' @export
#'
#' @examples
#' rgb_spec(c("A", "B", "C"), c(1, 2, 3), c(10, 9, 8))
rgb_spec <- function(r = double(),
                     g = double(),
                     b = double()) {
  rgb <- list(r = r, g = g, b = b)
  lens <- lengths(rgb)
  rgb[lens == 0] <- list(new_void_channel(max(lens)))
  rgb <- vec_recycle_common(r = rgb$r, g = rgb$g, b = rgb$b)
  new_rgb_spec(r = rgb$r, g = rgb$g, b = rgb$b)
}

# Internal constructor
new_rgb_spec <- function(r = double(),
                         g = double(),
                         b = double()) {
  size <- length(r)
  r <- vec_assert(r, size = size, arg = "r")
  g <- vec_assert(g, size = size, arg = "g")
  b <- vec_assert(b, size = size, arg = "b")
  new_rcrd(list(r = r, g = g, b = b),
           class = c("rgb_spec", "colour_spec"))
}

# Boilerplate -------------------------------------------------------------

# We need to be able to cast `double` to `rgb_spec` due to the subassignment of
# `NA_real_`s by `scales::oob_censor()`.

#' @export
vec_ptype2.rgb_spec.rgb_spec <- function(x, y, ...) {
  x <- vec_data(x)
  y <- vec_data(y)
  rgb_spec(
    r = vec_ptype2(x$r, y$r),
    g = vec_ptype2(x$g, y$g),
    b = vec_ptype2(x$b, y$b)
  )
}

#' @export
vec_ptype2.rgb_spec.double <- function(x, y, ...) rgb_spec()

#' @export
vec_cast.rgb_spec.rgb_spec <- function(x, to, ...) x

#' @export
vec_cast.rgb_spec.double <- function(x, to, ...) rgb_spec(x, x, x)

#' @export
#' @noRd
#' @keywords internal
scale_type.rgb_spec <- function(x) "rgb"


