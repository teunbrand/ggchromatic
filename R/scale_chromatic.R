# Constructor -------------------------------------------------------------

#' Chromatic scale constructor
#'
#' Constructs a chromatic scale. A chromatic scale can take several values for
#' the same observations and places these as coordinates in a colour space.
#'
#' @inheritParams ggplot2::continuous_scale
#' @param palette A palette function that when called with a `colour_spec`
#'   vector should return a vector of colours.
#' @param breaks One of \itemize{
#'   \item `NULL` for no breaks.
#'   \item `waiver()` for the default breaks computed by the
#'   [transformation object](scales::trans_new()).
#'   \item A `colour_spec` vector. For continuous channels, must be a numeric
#'   channel. For discrete channels, a character channel. Channels can be padded
#'   with `NA`s if the desired breaks are of unequal length.
#'   \item A `function` that uses the limits as input and returns breaks. Note
#'   that this is used for both continuous and discrete channels.
#'   \item A named `list` with the names of channels with (1) a `character`
#'   or `numeric` vector giving the breaks for that channel or (2) a function to
#'   be applied to the limits of that channel or (3) `NULL` for no breaks in
#'   that channel. Channels whose names are absent in the `list`'s names are
#'   treated with the `waiver()` option above.
#' }
#' @param labels One of \itemize{
#'  \item `NULL` for no labels.
#'  \item `waiver()` for the default labels. In case of continuous channels,
#'  these are passed through the format function of the
#'  [transformation object](scales::trans_new()).
#'  \item A `colour_spec` vector with character vectors in the channels. The
#'  channels can be padded with `NA`s to match the length of channels with the
#'  most breaks.
#'  \item A `function` that uses the breaks as input and returns labels. Note
#'  that this is used for both continuous and discrete channels.
#'  \item A named `list` with the names of channels with (1) a `character`
#'  vector giving the labels for that channel or (2) a function to be applied to
#'  the breaks of that channel or (3) `NULL` for no labels in that channel.
#'  Channels whose names are absent in the `list`'s names are treated with the
#'  `waiver()` option above.
#' }
#' @param limits One of \itemize{
#'  \item `NULL` to use the default scale range.
#'  \item A `colour_spec` vector. For continuous channels, must be a length 2
#'  vector giving the minimum and maximum. For discrete channels, the relevant
#'  channel should define possible values. For mixed usage, the continuous
#'  limits can be padded with `NA`s.
#'  \item A `function` that accepts the existing (automatic) limits and returns
#'  new limits. Note that this is used for both continuous and discrete
#'  channels.
#'  \item A named `list` with names of channels with (1) a vector defining
#'  the limits or (2) a function to be applied to the natural limits. Channels
#'  whose names are absent in the `list`'s names are treated with the `NULL`
#'  option above.
#' }
#' @param prototype A `function` that serves as constructor for the specific
#'   `colour_spec` class.
#' @param channel_limits One of: \itemize{
#'  \item A `colour_spec` vector of length 2 containing `numeric` channels that
#'  indicating the limits for each channel between 0-1.
#'  \item A named `list` with channel names and length 1 or 2 `numeric` vectors
#'  that indicate the limits for that channel between 0-1.
#'  }
#' @return A `ScaleChromatic` ggproto object.
#' @export
#'
#' @examples
#' # Empty channels will take the midpoint of the channel limits.
#' # Note that the 'luminance' channel is missing below.
#' p <- ggplot(economics, aes(date, unemploy)) +
#'   geom_point(aes(colour = hcl_spec(pop, psavert)))
#' p
#'
#' # You can set the output of missing channels through the channel limits.
#' # Setting 1 value fixes the output for that channel.
#' p + scale_colour_hcl(channel_limits = list(l = 0.8))
#'
#' # Alternatively, you can constrain the output of particular channels.
#' # Setting 2 values between 0-1 restricts the channel output range.
#' p <- ggplot(economics, aes(date, unemploy)) +
#'   geom_point(aes(colour = hcl_spec(pop, psavert, pce)))
#' p + scale_colour_hcl(channel_limits = list(l = c(0.5, 1), c = c(0.2, 0.8)))
#'
#' # Setting breaks, labels and limits through named lists
#' p + scale_colour_hcl(
#'   breaks = list(c = c(5, 10, 15)),
#'   labels = list(h = scales::number_format(scale = 1e-3, suffix = "K")),
#'   limits = list(h = c(200e3, 300e3), c = c(5, 15), l = c(3000, 9000)),
#'   oob = scales::oob_squish
#' )
#'
#' # Scale can have names for every channel to be displayed in the guide
#' p + scale_colour_hcl(name = c("Hue", "Chroma", "Luminance"))
#'
#' # Scale can handle all-discrete variables
#' p <- ggplot(iris, aes(Sepal.Width, Sepal.Length))
#' p + geom_point(aes(colour = rgb_spec(Species, sample(Species), sample(Species))))
#'
#' # Or can handle a mix between discrete and continuous
#' p + geom_point(aes(colour = cmy_spec(Petal.Length, Species, Petal.Width)))
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
  na.value = "grey50",
  trans = "identity",
  guide = "chromatic",
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
    discrete <- channel_is_discrete(x)
    void <- channel_is_void(x)
    fields <- fields(limits)
    limits <- without_nas(as.list(vec_data(limits)))
    # Loop through discrete fields, match to limits
    for (f in fields[discrete & !void]) {
      field(x, f) <- self$rescaler(match(field(x, f), limits[[f]]),
                                   to = field(channel_limits, f),
                                   from = c(1, length(limits[[f]])))
    }
    # Loop through fields, apply `self$rescaler()` if continuous or void
    for (f in fields[!(discrete & !void)]) {
      field(x, f) <- self$rescaler(field(x, f), to = field(channel_limits, f),
                                   from = limits[[f]])
    }
    x
  },

  apply_oob = function(self, x, limits = self$get_limits(),
                       oob = self$oob) {
    # Wrapper for `self$oob()`, apply to continuous fields only
    discrete <- channel_is_discrete(limits) | channel_is_discrete(x)
    for (f in fields(limits)[!discrete]) {
      field(x, f) <- oob(field(x, f), range = without_nas(field(limits, f)))
    }
    return(x)
  },

  map = function(self, x, limits = self$get_limits(),
                 channel_limits = self$channel_limits) {

    x <- self$rescale(self$apply_oob(x, limits = limits),
                      limits = limits, channel_limits = channel_limits)

    uniq <- unique(x)
    pal <- self$palette(uniq)
    scaled <- pal[vec_match(x, uniq)]
    scaled[is.na(scaled)] <- self$na.value
    scaled
  },

  transform = function(self, x) {
    if (is_colour_spec(x)) {
      discrete <- channel_is_discrete(x)
      for (channel in fields(x)[!discrete]) {
        field(x, channel) <- self$trans$transform(field(x, channel))
      }
    } else if (!is_discrete(x)) {
      x <- self$trans$transform(x)
    }
    x
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
    continuous <- fields(lim)[!channel_is_discrete(lim)]
    for (f in continuous) {
      x <- field(lim, f)[1:2]
      x <- ifelse(is.na(x), field(range, f)[1:2], x)
      field(lim, f)[1:2] <- x
    }

    lim
  },

  get_breaks = function(self, limits = self$get_limits()) {
    if (self$is_empty()) {
      return(NULL)
    }
    if (is.null(self$breaks)) {
      return(NULL)
    }
    if (!is.list(self$breaks) && !is.function(self$breaks) &&
        is.na(self$breaks)) {
      rlang::abort("Invalid breaks specification. Use `NULL` not `NA`.")
    }

    breaks <- channels_apply_c(limits, self$trans$inverse)
    breaks <- without_nas(as.list(vec_data(breaks)))
    breaks <- breaks[lengths(breaks) > 0] # Remove void channels (all NAs)

    disc <- vapply(breaks, is_discrete, logical(1))
    calc <- rep(TRUE, length(breaks))

    # Apply zero range ~ lower limit
    zerorange <- setNames(rep(FALSE, length(breaks)), names(breaks))
    zerorange[!disc & calc] <- vapply(breaks[!disc & calc],
                                      zero_range, logical(1))
    breaks <- clapply(breaks, zerorange, `[[`, 1)
    calc[zerorange | disc] <- FALSE

    if (inherits(self$breaks, "waiver")) {
      # Use trans to calculate breaks for continuous variables
      breaks[calc] <- breaks_from_trans(breaks[calc], self$trans,
                                        self$n.breaks)
    } else if (is.function(self$breaks)) {
      breaks <- clapply(breaks, !zerorange, self$breaks)
    } else if (is.list(self$breaks)) {
      # rlang::abort("List breaks not implemented yet")
      fields <- names(breaks)
      is_defined <- fields %in% names(self$breaks)
      # Undefined breaks: use trans breaks
      breaks[calc & !is_defined] <- breaks_from_trans(
        breaks[calc & !is_defined], self$trans, self$n.breaks
      )
      defined <- self$breaks[names(self$breaks) %in% fields[is_defined]]
      breaks[is_defined] <- lapply(
        setNames(nm = fields[is_defined]), function(f) {
          brk <- defined[[f]]
          if (is.null(brk)) {
            return(new_void_channel(1))
          } else if (is.function(brk)) {
            if (!zerorange[[f]]) {
              brk(breaks[[f]])
            } else {
              breaks[[f]]
            }
          } else {
            brk
          }
        })
    } else {
      breaks <- without_nas(as.list(vec_data(self$breaks)))
    }

    breaks <- clapply(breaks, !disc, self$trans$transform)

    breaks <- do.call(self$ptype, pad_nas(breaks))

    breaks <- self$apply_oob(breaks, limits, oob = oob_censor)

    breaks <- as.list(vec_data(breaks))
    breaks <- without_nas(breaks)
    breaks <- do.call(self$ptype, pad_nas(breaks))

    breaks

  },

  get_labels = function(self, breaks = self$get_breaks()) {

    if (is.null(breaks)) {
      return(NULL)
    }
    if (is.null(self$labels)) {
      return(NULL)
    }
    if (identical(self$labels, NA)) {
      rlang::abort("Invalid labels specification. Use NULL not NA.")
    }

    labels <- channels_apply_c(breaks, self$trans$inverse)
    labels <- without_nas(as.list(vec_data(labels)))
    labels <- labels[lengths(labels) > 0] # Remove void channels (all NAs)
    disc <- vapply(labels, is_discrete, logical(1))

    if (inherits(self$labels, "waiver")) {
      labels <- clapply(labels,  disc, as.character)
      labels <- clapply(labels, !disc, self$trans$format)
    } else if (is.function(self$labels)) {
      labels <- lapply(labels, self$labels)
    } else if (is.list(self$labels) && is_named(self$labels)) {
      fields <- names(labels)
      is_defined <- fields %in% names(self$labels)
      labels <- clapply(labels,  disc & !is_defined, as.character)
      labels <- clapply(labels, !disc & !is_defined, self$trans$format)
      defined <- self$labels[names(self$labels) %in% fields[is_defined]]
      labels[is_defined] <- lapply(
        setNames(nm = fields[is_defined]), function(f) {
          lbl <- defined[[f]]
          if (is.null(lbl)) {
            return(new_void_channel(1))
          } else if (is.function(lbl)) {
            lbl(labels[[f]])
          } else {
            lbl
          }
        })
    } else {
      labels <- without_nas(as.list(vec_data(self$labels)))
    }

    # Flatten list labels
    list_lab <- vapply(labels, is.list, logical(1))
    labels <- clapply(labels, list_lab, function(lab) {
      lab[vapply(lab, length, integer(1)) == 0] <- ""
      lab <- lapply(lab, `[`, 1)
      unlist(lab)
    })
    lang_lab <- vapply(labels, is.language, logical(1))
    labels <- clapply(labels, lang_lab, function(lab) {
      new_vexpression(lab)
    })

    labels <- do.call(self$ptype, pad_nas(labels))

    if (length(labels) != length(breaks)) {
      rlang::abort("Breaks and labels are different lengths.")
    }

    labels

  },

  make_title = function(title, sub = substitute(title)) {
    sub <- call_args(sub)
    global <- eval(sub[[2]], envir = parent.frame(2))
    sub <- call_args(sub[[1]])
    scale <- eval(sub[[2]], envir = parent.frame(2))
    guide <- eval(sub[[1]], envir = parent.frame(2))
    if (!inherits(guide, "waiver")) {
      return(guide)
    }
    if (!inherits(scale, "waiver")) {
      return(scale)
    }
    title <- decompose_title(global)
    return(title)
  },

  print = function(self, ...) {

    show_range <- function(x) {
      if (is_discrete(x)) {
        if (is_void_channel(x)) {
          "Absent"
        } else {
          do.call(paste, as.list(x))
        }
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

breaks_from_trans <- function(limits, trans, n.breaks = NULL) {
  if (!is.null(n.breaks) && "n" %in% names(formals(trans$breaks))) {
    lapply(limits, trans$breaks, n = n.breaks)
  } else {
    lapply(limits, trans$breaks)
  }
}

check_breaks_labels <- function(breaks, labels) {
  if (is.null(breaks)) {
    return(TRUE)
  }
  if (is.null(labels)) {
    return(TRUE)
  }
  if (inherits(breaks, "waiver")) {
    return(TRUE)
  }
  if (inherits(labels, "waiver")) {
    return(TRUE)
  }

  if (is.list(breaks) && is.list(labels)) {
    bad_labels <- mapply(function(x, y) {
      is.atomic(x) && is.atomic(y) && length(x) != length(y)
    }, x = breaks, y = labels, SIMPLIFY = FALSE)
    bad_labels <- any(do.call(c, bad_labels))
  } else {
    bad_labels <- is.atomic(breaks) && is.atomic(labels) &&
      length(breaks) != length(labels)
  }

  if (bad_labels) {
    rlang::abort("`breaks` and `labels` must have the same length.")
  }
  TRUE
}

check_channel_limits <- function(x, ptype) {
  fields <- fields(ptype())
  if (!is_colour_spec(x)) {
    defaults <- vec_set_names(rep(list(c(0, 1)), length(fields)), fields)
    if (!is.null(x)) {
      common_fields <- intersect(fields, fields(x))
      defaults[common_fields] <- vec_recycle_common(!!!x[common_fields],
                                                    .size = 2)
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

decompose_title <- function(title, sep = NULL) {
  if (is.null(title)) {
    return(NULL)
  }
  lang <- str2lang(as_string(title))
  if (is_call(lang)) {
    lang <- call_standardise(lang)
    args <- call_args(lang)
    fun  <- fn_fmls_names(call_fn(lang))
    init <- rep(list(expr()), length(fun))
    i <- match(names(args), fun)
    j <- match(fun, names(args))
    init[!is.na(j)] <- unname(args)[!is.na(i)]
    args <- vapply(init, as_label, character(1))
    args[args == "<empty>"] <- ""
    return(args)
  }
  if (!is.null(sep)) {
    title <- strsplit(title, sep)[[1]]
    return(title)
  }
  title
}
