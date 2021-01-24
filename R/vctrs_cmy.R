# Constructors ------------------------------------------------------------

#' Cyan-Magenta-Yellow specification
#'
#' Creates a vector that stores values for cyan, magenta and yellow channels in
#' the CMY colour space.
#'
#' @param c,m,y Vectors with values for the cyan, magenta and yellow channels
#'   respectively.
#'
#' @return A `cmy_spec`, `vctrs_rcrd` S3 class object.
#' @export
#' @family colour space vectors
#'
#' @examples
#' cmy_spec(c("A", "B", "C"), 1:3, 10:8)
cmy_spec <- function(c = double(),
                     m = double(),
                     y = double()) {
  cmy <- list(c = c, m = m, y = y)
  lens <- lengths(cmy)
  cmy[lens == 0] <- list(new_void_channel(max(lens)))
  cmy <- vec_recycle_common(c = cmy$c, m = cmy$m, y = cmy$y)
  new_cmy_spec(c = cmy$c, m = cmy$m, y = cmy$y)
}

new_cmy_spec <- function(c = double(),
                         m = double(),
                         y = double()) {
  size <- length(m)
  c <- vec_assert(c, size = size, arg = "c")
  m <- vec_assert(m, size = size, arg = "m")
  y <- vec_assert(y, size = size, arg = "y")
  new_rcrd(list(c = c, m = m, y = y),
           class = c("cmy_spec", "colour_spec"))
}

# Boilerplate -------------------------------------------------------------

# We need to be able to cast `double` to `cmy_spec` due to the subassignment of
# `NA_real_`s by `scales::oob_censor()`.

#' @export
vec_ptype2.cmy_spec.cmy_spec <- function(x, y, ...) {
  x <- vec_data(x)
  y <- vec_data(y)
  cmy_spec(
    c = vec_ptype2(x$c, y$c),
    m = vec_ptype2(x$m, y$m),
    y = vec_ptype2(x$y, y$y)
  )
}

#' @export
vec_ptype2.cmy_spec.double <- function(x, y, ...) cmy_spec()

#' @export
vec_cast.cmy_spec.cmy_spec <- function(x, to, ...) x

#' @export
vec_cast.cmy_spec.double <- function(x, to, ...) cmy_spec(x, x, x)

#' @export
#' @noRd
#' @keywords internal
scale_type.cmy_spec <- function(x) "cmy"
