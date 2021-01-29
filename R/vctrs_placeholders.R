# Void channel ------------------------------------------------------------

#' Void channel
#'
#' A `void_channel` vector is a placeholder vector takes the place of normal
#' values in a `colour_spec` vector. This occurs automatically when a
#' `colour_spec` is constructed with missing channels.
#'
#' @param x A vector with the appropriate size for the void channel.
#'
#' @return A `void_channel` vector
#'
#' @details Void channels do not participate in scale training, preserving
#'   existing scale limits. While mapping a `colour_spec` vector to a vector of
#'   hexadecimal colours, void channels take on the midpoint of the channel
#'   limits.
#'
#' @noRd
#' @keywords internal
#' @examples
#' void_channel(1)
void_channel <- function(x = logical()) {
  new_void_channel(size = vec_size(x))
}

new_void_channel <- function(size = 0) {
  new_vctr(rep(NA, size), class = "void_channel")
}

is_void_channel <- function(x) {
  inherits(x, "void_channel")
}

format.void_channel <- function(x, ...) {
  return("")
}

# Void channel boilerplate ------------------------------------------------

#' @export
vec_ptype2.void_channel.void_channel <- function(x, y, ...) new_void_channel()

#' @export
vec_ptype2.void_channel.default <- function(x, y, ...) y

#' @export
vec_ptype2.void_channel.character <- function(x, y, ...) character()

#' @export
vec_ptype2.void_channel.logical <- function(x, y, ...) logical()

#' @export
#' @method vec_ptype2.character void_channel
vec_ptype2.character.void_channel <- function(x, y, ...) character()

#' @export
#' @method vec_ptype2.logical void_channel
vec_ptype2.logical.void_channel <- function(x, y, ...) logical()

#' @export
vec_cast.void_channel.void_channel <- function(x, to, ...) x

#' @export
vec_cast.void_channel.character <- function(x, to, ...) to

#' @export
#' @method vec_cast.character void_channel
vec_cast.character.void_channel <- function(x, to, ...) as.character(vec_data(x))

#' @export
#' @method rescale void_channel
rescale.void_channel <- function(x, to, from, ...) {
  rep(mean(to), length(x))
}

# Void functions ----------------------------------------------------------

fill_void <- function(x, fill = 0) {
  UseMethod("fill_void")
}

fill_void.default <- function(x, fill = 0) {
  rlang::abort(paste0("Cannot fill void of `", typeof(x), "` object."))
}

fill_void.colour_spec <- function(x, fill = 0) {
  void <- channel_is_void(x)
  data <- vec_data(x)
  data <- clapply(data, void, function(i){rep_len(fill, length(i))})
  vec_restore(data, x)
}

# Expressions -------------------------------------------------------------

new_vexpression <- function(x = expression()) {
  if (!is.expression(x)) {
    x <- as.expression(x)
    if (!is.expression(x)) {
      stop("Error in expression packaging.")
    }
  }
  new_vctr(as.list(x), class = "vexpression")
}


# Expression boilerplate --------------------------------------------------

#' @export
vec_ptype2.vexpression.vexpression <- function(x, y, ...) new_vexpression()

#' @export
vec_ptype2.vexpression.character <- function(x, y, ...) new_vexpression()

#' @export
vec_ptype2.vexpression.void_channel <- function(x, y, ...) new_vexpression()

#' @export
#' @method vec_ptype2.character vexpression
vec_ptype2.character.vexpression <- function(x, y, ...) new_vexpression()

#' @export
vec_cast.vexpression.vexpression <- function(x, to, ...) x

#' @export
#' @method vec_cast.character vexpression
vec_cast.character.vexpression <- function(x, to, ...) new_vexpression(x)

#' @export
vec_cast.vexpression.character <- function(x, to, ...) new_vexpression(x)

#' @export
vec_cast.vexpression.void_channel <- function(x, to, ...) new_vexpression(vec_data(x))

# Expression functions ----------------------------------------------------

as.expression.vexpression <- function(x) {
  do.call(expression, vec_data(x))
}

is_alt_language <- function(x) {
  is.language(x) || inherits(x, "vexpression")
}

unwrap_vexpr <- function(x) {
  if (inherits(x, "vexpression")) {
    as.expression(x)
  } else {
   x
  }
}

unwrap_language <- function(x) {
  if (inherits(x, "vexpression")) {
    as.expression(x)
  } else {
    do.call(expression, x)
  }
}
