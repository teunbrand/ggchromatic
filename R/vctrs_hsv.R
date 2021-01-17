# Constructors ------------------------------------------------------------

#' Hue-Saturation-Value specification
#'
#' Creates a vector that stores values for hue, saturation and value channels in
#' the HSV colour space.
#'
#' @param h,s,v Vectors with values for the hue, saturation and value channels
#'   respectively.
#'
#' @return A `hsv_spec`, `vctrs_rcrd` S3 class object.
#' @export
#' @family colour space vectors
#'
#' @examples
#' hsv_spec(c("A", "B", "C"), c(1, 2, 3), c(10, 9, 8))
hsv_spec <- function(h = double(),
                     s = double(),
                     v = double()) {
  hsv <- list(h = h, s = s, v = v)
  lens <- lengths(hsv)
  hsv[lens == 0] <- list(new_void_channel(max(lens)))
  hsv <- vec_recycle_common(h = hsv$h, s = hsv$s, v = hsv$v)
  new_hsv_spec(h = hsv$h, s = hsv$s, v = hsv$v)
}

# Internal constructor
new_hsv_spec <- function(h = double(),
                         s = double(),
                         v = double()) {
  size <- length(h)
  h <- vec_assert(h, size = size, arg = "h")
  s <- vec_assert(s, size = size, arg = "s")
  v <- vec_assert(v, size = size, arg = "v")
  new_rcrd(list(h = h, s = s, v = v),
           class = c("hsv_spec", "colour_spec"))
}

# Boilerplate -------------------------------------------------------------

#' @export
vec_ptype2.hsv_spec.hsv_spec <- function(x, y, ...) {
  x <- vec_data(x)
  y <- vec_data(y)
  hsv_spec(
    h = vec_ptype2(x$h, y$h),
    s = vec_ptype2(x$s, y$s),
    v = vec_ptype2(x$v, y$v)
  )
}

#' @export
vec_ptype2.hsv_spec.double <- function(x, y, ...) hsv_spec()

#' @export
vec_cast.hsv_spec.hsv_spec <- function(x, to, ...) x

#' @export
vec_cast.hsv_spec.double <- function(x, to, ...) hsv_spec(x, x, x)

#' @export
#' @noRd
#' @keywords internal
scale_type.hsv_spec <- function(x) "hsv"

