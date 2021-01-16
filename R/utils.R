
# Pad vector with NAs until size
#' @export
#' @noRd
#' @keywords internal
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

# Return vector without NAs
#' @export
#' @noRd
#' @keywords internal
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

# Lapply over nonzero length elements
lapply_nz <- function(X, FUN, ...) {
  X[lengths(X) > 0] <- lapply(X[lengths(X) > 0], FUN, ...)
  return(X)
}

is_discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x) || is_void_channel(x)
}

