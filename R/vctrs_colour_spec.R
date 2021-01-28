
# Constructor -------------------------------------------------------------

new_colour_spec <- function(..., class) {
  vals <- rlang::list2(...)
  missing <- vapply(vals, identical, logical(1), quote(expr = ))
  vals[missing] <- list(double())

  n <- lengths(vals)
  vals[n == 0] <- list(new_void_channel(max(n)))
  vals <- vec_recycle_common(!!!vals)
  new_rcrd(vals, class = c("colour_spec", class))
}


# Boilerplate -------------------------------------------------------------

#' @export
vec_ptype2.colour_spec.colour_spec <- function(x, y, ...) {
  if (!all(class(x)[1:3] == class(y)[1:3])) {
    stop_incompatible_type(x, y, ...)
  }
  z <- mapply(vec_ptype2, x = vec_data(x), y = vec_data(y),
              x_arg = fields(x),
              SIMPLIFY = FALSE)
  vec_restore(z, x)
}

#' @export
vec_ptype2.colour_spec.double <- function(x, y, ...) {
  spectrum_constructor(x)()
}

#' @export
vec_cast.colour_spec.colour_spec <- function(x, to, ...) x

#' @export
vec_cast.colour_spec.double <- function(x, to, ...) {
  fun <- spectrum_constructor(to)
  x <- vec_set_names(rep(list(x), n_fields(fun())), fields(fun()))
  do.call(fun, x)
}

# Methods -----------------------------------------------------------------

#' @export
#' @method vec_ptype_abbr colour_spec
vec_ptype_abbr.colour_spec <- function(x, ...) {
  gsub("_spec$", "", class(x)[[2]])
}

#' @export
#' @method vec_ptype_full colour_spec
vec_ptype_full.colour_spec <- function(x, ...) {
  class(x)[[2]]
}

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

spectrum_name <- function(x) {
  if (is_colour_spec(x)) {
    # Spectrum name is in 1 class after `colour_spec`
    i <- match("colour_spec", class(x))
    name <- class(x)[i + 1]
    gsub("_spec$", "", name)
  } else {
    return(NULL)
  }
}

spectrum_constructor <- function(x) {
  if (is_colour_spec(x)) {
    x <- spectrum_name(x)
  }
  x <- match.arg(x, c("rgb", "cmyk", "hsl", "hsv", "hcl", "cmy"))
  switch(
    x,
    "rgb" = rgb_spec,
    "hsv" = hsv_spec,
    "hsl" = hsl_spec,
    "cmyk" = cmyk_spec,
    "cmy" = cmy_spec,
    "hcl" = hcl_spec,
    rlang::abort("Cannot find constructor for `", typof(x), "`")
  )
}

#' @export
#' @method scale_type colour_spec
scale_type.colour_spec <- function(x) {
  spectrum_name(x)
}


# Utilities ---------------------------------------------------------------

channel_is_discrete <- function(colour_spec) {
  vapply(vec_data(colour_spec), is_discrete, logical(1))
}

channel_is_void <- function(colour_spec) {
  vapply(vec_data(colour_spec), is_void_channel, logical(1))
}

channels_discrete <- function(colour_spec, parallel = FALSE) {
  discrete <- channel_is_discrete(colour_spec)
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
  continuous <- !channel_is_discrete(colour_spec)
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
  Y <- vec_data(X)
  Y[] <- lapply(Y, FUN, ...)
  vec_restore(Y, X)
}

melt_channels <- function(x, channels = fields(x)) {
  vec_c(!!!unname(vec_data(x)[channels]))
}

set_channel_default <- function(x, channels) {
  UseMethod("set_channel_default")
}

set_channel_default.default <- function(x, channels) {
  rlang::abort(paste0("Don't know how to set default channels for `",
                      typof(x), "` object."))
}

set_channel_default.colour_spec <- function(x, channels) {
  channels <- intersect(channels, fields(x))
  for (f in channels) {
    field(x, f) <- rep_len(mean(field(x, f)), length.out = length(x))
  }
  x
}

set_channel_default.hsv_spec <- function(x, channels) {
  funs <- list(h = min, s = max, v = max)
  channels <- intersect(channels, fields(x))
  for (f in channels) {
    field(x, f) <- rep_len(funs[[f]](field(x, f)), length.out = length(x))
  }
  x
}

set_channel_default.hsl_spec <- function(x, channels) {
  funs <- list(h = min, s = max, l = mean)
  channels <- intersect(channels, fields(x))
  for (f in channels) {
    field(x, f) <- rep_len(funs[[f]](field(x, f)), length.out = length(x))
  }
  x
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




