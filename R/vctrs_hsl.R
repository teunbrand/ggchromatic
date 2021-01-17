# Constructors ------------------------------------------------------------

#' Hue-Saturation-Lightness specification
#'
#' Creates a vector that stores values for hue, saturation and lightness
#' channels in the HSL colour space.
#'
#' @param h,s,l Vectors with values for hue, saturation and lightness channels
#'   respectively
#'
#' @return A `hsl_spec`, `vctrs_rcrd` S3 class object.
#' @export
#' @family colour space vectors
#'
#' @examples
#' hsv_spec(c("A", "B", "C"), c(1, 2, 3), c(10, 9, 8))
hsl_spec <- function(h = double(),
                     s = double(),
                     l = double()) {
  hsl <- list(h = h, s = s, l = l)
  lens <- lengths(hsl)
  hsl[lens == 0] <- list(new_void_channel(max(lens)))
  hsl <- vec_recycle_common(h = hsl$h, s = hsl$s, l = hsl$l)
  new_hsl_spec(h = hsl$h, s = hsl$s, l = hsl$l)
}

new_hsl_spec <- function(h = double(),
                         s = double(),
                         l = double()) {
  size <- length(h)
  h <- vec_assert(h, size = size, arg = "h")
  s <- vec_assert(s, size = size, arg = "s")
  l <- vec_assert(l, size = size, arg = "l")
  new_rcrd(list(h = h, s = s, l = l),
           class = c("hsl_spec", "colour_spec"))
}

# Boiler plate ------------------------------------------------------------

#' @export
vec_ptype2.hsl_spec.hsl_spec <- function(x, y, ...) {
  x <- vec_data(x)
  y <- vec_data(y)
  hsl_spec(
    h = vec_ptype2(x$h, y$h),
    s = vec_ptype2(x$s, y$s),
    l = vec_ptype2(x$l, y$l)
  )
}

#' @export
vec_ptype2.hsl_spec.double <- function(x, y, ...) hsl_spec()

#' @export
vec_cast.hsl_spec.hsl_spec <- function(x, to, ...) x

#' @export
vec_cast.hsl_spec.double <- function(x, to, ...) hsl_spec(x, x, x)

#' @export
#' @noRd
#' @keywords internal
scale_type.hsl_spec <- function(x) "hsl"

