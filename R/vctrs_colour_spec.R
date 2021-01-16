# Methods -----------------------------------------------------------------

#' @export
#' @method format colour_spec
format.colour_spec <- function(x, ...) {
  out <- lapply(vec_data(x), function(x) {
    if (is.numeric(x)) {
      return(signif(x, 3))
    } else {
      x
    }
  })
  out <- lapply(out, format)
  out <- paste0("[", do.call(paste, c(out, sep = ",")), "]")
  out[is.na(x)] <- NA
  out
}

#' @export
#' @method as.matrix colour_spec
as.matrix.colour_spec <- function(x, ...) {
  x <- vec_data(x)
  x[] <- lapply(x, function(y) {
    if (is_discrete(y)) {
      lim <- unique(y)
      y <- rescale(match(y, lim), from = c(1, length(lim)))
    }
    return(y)
  })
  as.matrix(x, ...)
}

is_colour_spec <- function(x) {
  inherits(x, "colour_spec")
}

# Utilities ---------------------------------------------------------------

channels_discrete <- function(colour_spec, parallel = FALSE) {
  discrete <- vapply(vec_data(colour_spec), is_discrete, logical(1))
  void <- vapply(vec_data(colour_spec), is_void_channel, logical(1))
  if (parallel) {
    x <- as.list(vec_data(colour_spec))
    x[!discrete | void] <- list(NULL)
    return(x)
  } else {
    as.list(vec_data(colour_spec)[discrete & !void])
  }
}

channels_continuous <- function(colour_spec, parellel = FALSE) {
  continuous <- !vapply(vec_data(colour_spec), is_discrete, logical(1))
  void <- vapply(vec_data(colour_spec), is_void_channel, logical(1))
  if (parellel) {
    x <- as.list(vec_data(colour_spec))
    x[!continuous | void] <- list(NULL)
    return(x)
  } else {
    as.list(vec_data(colour_spec)[continuous & !void])
  }
}

channels_apply <- function(X, FUN, ...) {
  channels <- fields(X)
  for (channel in channels) {
    field(X, channel) <- FUN(field(X, channel), ...)
  }
  X
}

channels_apply_c <- function(X, FUN, ...) {
  continuous <- !vapply(vec_data(X), is_discrete, logical(1))
  channels <- fields(X)[continuous]
  for (channel in channels) {
    field(X, channel) <- FUN(field(X, channel), ...)
  }
  X
}

channels_lapply <- function(X, FUN, ...) {
  Y <- lapply(vec_data(X), FUN, ...)
  lens <- lengths(y)
  out <- vec_init(X, max(lens))
  for (fname in names(Y)) {
    field(out, fname) <- c(Y[[fname]], rep(NA, max(lens) - lens[[fname]]))
  }
  return(out)
}

merge_hybrid_fields <- function(discrete, continuous) {
  discrete   <- vec_data(discrete)
  continuous <- vec_data(continuous)
  empty_d <- vapply(discrete, is.null, logical(1))
  empty_c <- vapply(continuous, is.null, logical(1))
  double_empty <- empty_d + empty_c
  if (any(double_empty == 0)) {
    rlang::abort("Failed to mix continuous and discrete fields.")
  }
  continuous[empty_c] <- discrete[empty_c]
  continuous[double_empty == 2] <- NA
  return(continuous)
}




