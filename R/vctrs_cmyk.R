# Constructors ------------------------------------------------------------

#' Cyan-Magenta-Yellow-Black specification
#'
#' Creates a vector that stores values for cyan, magenta, yellow and black
#' channels in the CMYK colour space.
#'
#' @param c,m,y,k Vectors with values for the cyan, magenta, yellow and key
#'   (black) channels respectively.
#'
#' @return A `cmyk_spec`, `vctrs_rcrd` S3 class object.
#' @export
#' @family colour space vectors
#'
#' @examples
#' cmyk_spec(c("A", "B", "C"), 1:3, 10:8, c("X","Y","Z"))
cmyk_spec <- function(c = double(),
                      m = double(),
                      y = double(),
                      k = double()) {
  cmyk <- list(c = c, m = m, y = y, k = k)
  lens <- lengths(cmyk)
  cmyk[lens == 0] <- list(new_void_channel(max(lens)))
  cmyk <- vec_recycle_common(c = cmyk$c, m = cmyk$m, y = cmyk$y, k = cmyk$k)
  new_cmyk_spec(c = cmyk$c, m = cmyk$m, y = cmyk$y, k = cmyk$k)
}

# Internal constructor
new_cmyk_spec <- function(c = double(),
                          m = double(),
                          y = double(),
                          k = double()) {
  size <- length(m)
  c <- vec_assert(c, size = size, arg = "c")
  m <- vec_assert(m, size = size, arg = "m")
  y <- vec_assert(y, size = size, arg = "y")
  k <- vec_assert(k, size = size, arg = "k")
  new_rcrd(list(c = c, m = m, y = y, k = k),
           class = c("cmyk_spec", "colour_spec"))
}

# Boilerplate -------------------------------------------------------------

# We need to be able to cast `double` to `cmyk_spec` due to the subassignment of
# `NA_real_`s by `scales::oob_censor()`.

#' @export
vec_ptype2.cmyk_spec.cmyk_spec <- function(x, y, ...) {
  x <- vec_data(x)
  y <- vec_data(y)
  cmyk_spec(
    c = vec_ptype2(x$c, y$c),
    m = vec_ptype2(x$m, y$m),
    y = vec_ptype2(x$y, y$y),
    k = vec_ptype2(x$k, y$k)
  )
}

#' @export
vec_ptype2.cmyk_spec.double <- function(x, y, ...) cmyk_spec()

#' @export
vec_cast.cmyk_spec.cmyk_spec <- function(x, to, ...) x

#' @export
vec_cast.cmyk_spec.double <- function(x, to, ...) cmyk_spec(x, x, x, x)

#' @export
#' @noRd
#' @keywords internal
scale_type.cmyk_spec <- function(x) "cmyk"
