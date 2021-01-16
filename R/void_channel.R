# Class to hold an unspecified channel
new_void_channel <- function(size = 0) {
  new_vctr(rep(NA, size), class = "void_channel")
}

is_void_channel <- function(x) {
  inherits(x, "void_channel")
}

format.void_channel <- function(x, ...) {
  return("")
}

vec_ptype2.void_channel.void_channel <- function(x, y, ...) new_void_channel()

vec_cast.void_channel.void_channel <- function(x, to, ...) x

#' @export
#' @method rescale void_channel
rescale.void_channel <- function(x, to, from, ...) {
  rep(mean(to), length(x))
}
