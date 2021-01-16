# Hybrid discrete / continuous scale
RangeChromatic <- ggproto(
  "RangeChromatic", NULL,
  range_d = NULL,
  range_c = NULL,
  reset = function(self) {
    self$range_d <- NULL
    self$range_c <- NULL
  },
  train = function(self, x, drop = FALSE, na.rm = FALSE) {
    range_c <- self$range_c
    if (is.null(range_c)) {
      range_c <- list(NULL)
    }
    self$range_c <- mapply(function(x, y) {
      train_continuous(x, existing = y)
    }, x = channels_continuous(x, TRUE), y = range_c, SIMPLIFY = FALSE)

    range_d <- self$range_d
    if (is.null(range_d)) {
      range_d <- list(NULL)
    }
    self$range_d <- mapply(function(x, y) {
      train_discrete(x, existing = y, drop = drop, na.rm = na.rm)
    }, x = channels_discrete(x, TRUE), y = range_d, SIMPLIFY = FALSE)
  },
  get_range = function(self) {
    if (is.null(self$range_d) * is.null(self$range_c)) {
      return(NULL)
    }
    merge_hybrid_fields(self$range_d, self$range_c)
  }
)

chromatic_range <- function() {
  ggproto(NULL, RangeChromatic)
}
