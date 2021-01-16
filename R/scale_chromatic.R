# Constructor -------------------------------------------------------------

#' Chromatic scale constructor
#'
#' Constructs a chromatic scale. A chromatic scale can take several values for
#' the same observations and places these as coordinates in a colour space.
#'
#' @inheritParams ggplot2::continuous_scale
#' @param palette A palette function that when called with a `colour_spec`
#'   vector should return a vector of colours.
#' @param breaks NOT IMPLEMENTED YET
#' @param labels NOT IMPLEMENTED YET
#' @param limits One of \itemize{
#'  \item `NULL` to use the default scale range
#'  \item A `colour_spec` vector. For continuous channels, must be a length 2
#'  vector giving the minimum and maximum. For discrete channels, the relevant
#'  channel should define possible values. For mixed usage, the continuous
#'  limits can be padded with `NA`s.
#'  \item A `function` that accepts the existing (automatic) limits and returns
#'  new limits.
#'  \item A named `list` with names of the channels with one of the above per
#'  channel.
#' }
#' @param prototype A `function` that serves as constructor for the specific
#'   `colour_spec` class.
#' @param channel_limits A `colour_spec` vector of length 2 indicating the
#'   limits for each channel.
#'
#' @return A `ScaleChromatic` ggproto object.
#' @export
#'
#' @examples
#' NULL
chromatic_scale <- function(
  aesthetics,
  scale_name,
  palette,
  name = waiver(),
  breaks = waiver(),
  n.breaks = NULL,
  labels = waiver(),
  limits = NULL,
  rescaler = rescale,
  oob = oob_censor,
  expand = waiver(),
  na.value = NA,
  trans = "identity",
  guide = "legend",
  prototype = NULL,
  channel_limits = NULL,
  super = ScaleChromatic
) {
  aesthetics <- standardise_aes_names(aesthetics)

  check_breaks_labels(breaks, labels)

  if (is.null(breaks)) {
    guide <- "none"
  }

  trans <- as.trans(trans)
  if (!is.null(limits) && !is.function(limits)) {
    limits <- trans$transform(limits)
  }

  channel_limits <- check_channel_limits(channel_limits, prototype)

  ggproto(
    NULL, super,
    call = match.call(),

    aesthetics = aesthetics,
    scale_name = scale_name,
    palette = palette,

    range = chromatic_range(),
    limits = limits,
    trans = trans,
    na.value = na.value,
    expand = expand,
    rescaler = rescaler,
    oob = oob,

    name = name,
    breaks = breaks,
    n.breaks = n.breaks,

    labels = labels,
    guide = guide,
    ptype = prototype,
    channel_limits = channel_limits
  )
}

# ggproto -----------------------------------------------------------------

#' @importFrom scales oob_censor
ScaleChromatic <- ggproto(
  "ScaleChromatic", Scale,

  # General components

  na.value = NA,

  # Continuous components

  range = chromatic_range(),
  oob = oob_censor,
  rescaler = rescale,
  minor_breaks = waiver(),
  n.breaks = NULL,
  trans = identity_trans(),
  is_discrete = function() FALSE,

  # Discrete components

  drop = TRUE,

  # Methods

  clone = function(self) {
    new <- ggproto(NULL, self)
    new$range <- chromatic_range()
    new
  },

  is_empty = function(self) {
    has_data <- !is.null(self$range$get_range())
    # Omitted is finite check in `has_limits`
    has_limits <- is.function(self$limits) || (!is.null(self$limits))
    !has_data && !has_limits
  },

  train = function(self, x) {
    if (length(x) == 0) {
      return()
    }
    self$range$train(x, drop = self$drop, na.rm = TRUE)
  },

  rescale = function(self, x, limits = self$get_limits(),
                     channel_limits = self$channel_limits) {

    # Loop through fields, apply `self$rescaler()` if continuous
    fields <- fields(limits)
    for (f in fields(limits)) {
      lim <- without_nas(field(limits, f))
      if (is_discrete(field(x, f)) && !is_void_channel(field(x, f))) {
        y <- self$rescaler(match(field(x, f), lim),
                      to = field(channel_limits, f),
                      from = c(1, length(field(limits, f))))
      } else {
        y <- self$rescaler(field(x, f), to = field(channel_limits, f),
                           from = lim)
      }
      field(x, f) <- y
    }
    x
  },

  apply_oob = function(self, x, limits = self$get_limits(), oob = self$oob) {
    # Wrapper for `self$oob()`, apply to continuous fields only
    channels <- fields(limits)
    for (chan in channels) {
      y <- field(x, chan)
      if (!is_discrete(y)) {
        field(x, chan) <- oob(y, range = without_nas(field(limits, chan)))
      }
    }
    return(x)
  },

  map = function(self, x, limits = self$get_limits()) {

    x <- self$rescale(self$apply_oob(x, limits = limits), limits = limits)

    uniq <- unique(x)
    pal <- self$palette(uniq)
    scaled <- pal[vec_match(x, uniq)]
    scaled[is.na(scaled)] <- self$na.value
    scaled
  },

  transform = function(self, x) {
    if (is_colour_spec(x)) {
      discrete <- vapply(vec_data(x), is_discrete, logical(1))
      for (channel in fields(x)[!discrete]) {
        field(x, channel) <- self$trans$transform(field(x, channel))
      }
    } else if (!is_discrete(x)) {
      x <- self$trans$transform(x)
    }
    x
  },

  transform_df = function(self, df) {
    if (ggplot2:::empty(df)) {
      return()
    }
    aesthetics <- intersect(self$aesthetics, names(df))
    if (length(aesthetics) == 0) {
      return()
    }

    channel_aes <- setdiff(aesthetics, c("colour", "fill"))
    not_channel <- setdiff(aesthetics, channel_aes)
    if (length(channel_aes) > 0 && length(not_channel) == 0) {
      # Reconstruct colour spec from channel aesthetics
      new <- df[channel_aes]
      colnames(new) <- substr(colnames(new), 1, 1)
      new <- do.call(self$ptype, as.list(new))
      df[[self$aesthetics[[1]]]] <- new
      df[channel_aes] <- NULL
      aesthetics <- intersect(self$aesthetics, names(df))
    }

    lapply(df[aesthetics], self$transform)
  },

  get_limits = function(self) {

    if (self$is_empty()) {
      return(vec_cast(c(0, 1), self$ptype()))
    }
    lim <- self$limits
    range <- self$range$get_range()
    if (is.null(lim)) {
      lim <- range
    } else if (is.function(lim)) {
      lim <- lapply(range, function(x) {
        in_transform_space(x, self$trans, lim)
      })
    } else if (is.list(lim)) {
      fields <- fields(self$ptype())
      lim <- lapply(setNames(fields, fields), function(f) {
        l <- lim[[f]]
        range <- self$range$range_c[[f]] %||% self$range$range_d[[f]]
        if (is.null(l))
          return(range)
        if (is.function(l)) {
          in_transform_space(range, self$trans, l)
        } else {
          l
        }
      })
    } else {
      lim <- self$limits
    }
    if (!vec_is(lim, self$ptype())) {
      lim <- do.call(self$ptype, pad_nas(lim))
    }
    continuous <- fields(lim)[!vapply(lim, is_discrete, logical(1))]
    for (f in continuous) {
      x <- field(lim, f)[1:2]
      x <- ifelse(is.na(x), field(range, f)[1:2], x)
      field(lim, f)[1:2] <- x
    }

    lim
  },

  print = function(self, ...) {

    show_range <- function(x) {
      if (is_discrete(x)) {
        do.call(paste, as.list(x))
      } else {
        paste0(formatC(without_nas(x), digits = 3), collapse = " -- ")
      }
    }

    cat("<", class(self)[[1]], ">\n", sep = "")

    lim <- vec_data(self$get_limits())
    for (i in names(lim)) {
      cat(" Range ", toupper(i), ":  ", show_range(lim[[i]]), "\n", sep = "")
    }
  }
)

# Helpers -----------------------------------------------------------------

in_transform_space <- function(x, trans, FUN, ...) {
  if (is_discrete(x)) {
    FUN(x, ...)
  } else {
    trans$transform(FUN(trans$inverse(x), ...))
  }
}

check_breaks_labels <- function(breaks, labels) {
  if (is.null(breaks)) {
    return(TRUE)
  }
  if (is.null(labels)) {
    return(TRUE)
  }
  bad_labels <- is.atomic(breaks) && is.atomic(labels) &&
    length(breaks) != length(labels)
  if (bad_labels) {
    rlang::abort("`breaks` and `labels` must have the same length.")
  }
  TRUE
}

check_channel_limits <- function(x, ptype) {
  fields <- fields(ptype())
  if (!is_colour_spec(x)) {
    defaults <- setNames(rep(list(c(0, 1)), length(fields)), fields)
    if (!is.null(x)) {
      common_fields <- intersect(fields, fields(x))
      for (f in common_fields) {
        vec_assert(x[[f]], size = 2, arg = "channel_limits")
      }
      defaults[common_fields] <- x[common_fields]
    }
    x <- defaults
  } else {
    vec_assert(x, size = 2)
    if (!inherits(x, class(ptype()))) {
      rlang::abort(glue::glue(
        "Channel limits are class {class(x)[[1]]} but should of class {class(ptype())[[1]]}"
      ))
    }
    x <- vec_data(x)
  }
  x <- lapply(x, oob_squish)
  vec_restore(x, ptype())
}
