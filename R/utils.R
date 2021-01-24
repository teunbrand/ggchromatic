# Pad vector with NAs until size

#' Pad with NAs
#'
#' This is a simple helper for some of the internals.
#'
#' @param x The object to pad.
#' @param size To what length to pad.
#'
#' @return A vector of length `size` or list wherein elements are of length
#'   `size`.
#' @export
#'
#' @examples
#' pad_nas(1, 2)
pad_nas <- function(x, size) {
  UseMethod("pad_nas", x)
}

#' @method pad_nas default
#' @export
pad_nas.default <- function(x, size = length(x)) {
  y <- vec_init(x, n = size)
  y[seq_along(x)] <- x
  return(y)
}

#' @method pad_nas list
#' @export
pad_nas.list <- function(x, size = max(lengths(x))) {
  force(size)
  lens <- lengths(x)
  x[lens == 0] <- list(NA)
  lapply(x, pad_nas, size = size)
}

#' @method pad_nas vexpression
#' @export
pad_nas.vexpression <- function(x, size = max(length(x))) {
  vec_c(x, new_vexpression(rep(NA, size - length(x))))
}

#' Remove the NAs
#'
#' This is a simple helper for some of the internals.
#'
#' @param x A vector or `list` to remove the `NA`s from.
#'
#' @return A vector or `list` without `NA`s.
#' @export
#'
#' @examples
#' without_nas(c(NA, 2))
without_nas <- function(x) {
  UseMethod("without_nas", x)
}

#' @method without_nas default
#' @export
without_nas.default <- function(x) {
  x[!is.na(x)]
}

#' @method without_nas list
#' @export
without_nas.list <- function(x) {
  lapply(x, without_nas)
}

#' @export
#' @method without_nas vexpression
without_nas.vexpression <- function(x) {
  x[!is.na(as.list(vec_data(x)))]
}

# Lapply over nonzero length elements
lapply_nz <- function(X, FUN, ...) {
  X[lengths(X) > 0] <- lapply(X[lengths(X) > 0], FUN, ...)
  return(X)
}

# Conditional lapply
clapply <- function(X, test, FUN, ...) {
  X[test] <- lapply(X[test], FUN, ...)
  return(X)
}

# Test whether variable is discrete. Void channels are considered discrete for
# convenience, since the use case is often to apply a transformation to
# continuous colour channels, but not discrete or void ones.
is_discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x) || is_void_channel(x)
}

# Like expand.grid(), but faster
xpand <- function(a, b) {
  new_data_frame(list(
    a = rep.int(a, length(b)),
    b = rep.int(b, rep.int(length(a), length(b)))
  ))
}
