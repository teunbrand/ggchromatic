#' Continuous colour bar guide 2
#'
#' This is just like the regular `guide_colourbar`, but with a specialised
#' training method that decodes `colour_spec` vectors from chromatic scales.
#'
#' @inheritParams ggplot2::guide_colorbar
#' @param channel A `character(1)` with the channel name to display. The
#'   default, `"auto"`, removes empty channels and subsequently takes the first.
#'
#' @return A `guide`, `colourbar2` S3 list object.
#' @export
#'
#' @examples
#' NULL
guide_colourbar2 <- function(
  # Title
  title = waiver(),
  title.position = NULL,
  title.theme = NULL,
  title.hjust = NULL,
  title.vjust = NULL,

  # Label
  label = TRUE,
  label.position = NULL,
  label.theme = NULL,
  label.hjust = NULL,
  label.vjust = NULL,

  # Bar
  barwidth = NULL,
  barheight = NULL,
  nbin = 300,
  raster = TRUE,
  channel = "auto",

  # Frame
  frame.colour = NULL,
  frame.linewidth = 0.5,
  frame.linetype = 1,

  # Ticks
  ticks = TRUE,
  ticks.colour = "white",
  ticks.linewidth = 0.5,
  draw.ulim = TRUE,
  draw.llim = TRUE,

  # General
  direction = NULL,
  default.unit = "line",
  reverse = FALSE,
  order = 0,
  available_aes = c("colour", "color", "fill"),
  ...
) {
  if (!is.null(barwidth) && !is.unit(barwidth)) {
    barwidth <- unit(barwidth, default.unit)
  }
  if (!is.null(barheight) && !is.unit(barheight)) {
    barheight <- unit(barheight, default.unit)
  }
  structure(list(
    # title
    title = title,
    title.position = title.position,
    title.theme = title.theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,

    # label
    label = label,
    label.position = label.position,
    label.theme = label.theme,
    label.hjust = label.hjust,
    label.vjust = label.vjust,

    # bar
    barwidth = barwidth,
    barheight = barheight,
    nbin = nbin,
    raster = raster,
    channel = channel,

    # frame
    frame.colour = frame.colour,
    frame.linewidth = frame.linewidth,
    frame.linetype = frame.linetype,

    # ticks
    ticks = ticks,
    ticks.colour = ticks.colour,
    ticks.linewidth = ticks.linewidth,
    draw.ulim = draw.ulim,
    draw.llim = draw.llim,

    # general
    direction = direction,
    default.unit = default.unit,
    reverse = reverse,
    order = order,

    # parameter
    available_aes = available_aes,
    ...,
    name = "colourbar2"),
    class = c("guide", "colourbar2")
  )
}

#' @export
#' @method guide_train colourbar2
guide_train.colourbar2 <- function(guide, scale, aesthetic = NULL) {
  if (!inherits(scale, "ScaleChromatic")) {
    rlang::warn("The colourbar2 guide needs chromatic scales.")
    return(NULL)
  }
  if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
    rlang::warn("colourbar2 needs appropriate scales.")
    return(NULL)
  }

  aes <- aesthetic %||% scale$aesthetics[[1]]
  spec <- fields(scale$ptype())

  limits <- scale$get_limits()
  void <- vapply(vec_data(limits), function(x) {all(is.na(x))}, logical(1))
  disc <- channel_is_discrete(limits) & !void

  if (length(guide$channel == 1) && guide$channel == "auto") {
    channel <- spec[!void]
  } else {
    channel <- guide$channel
  }
  channel <- match(channel, spec)[1]
  if (is.na(channel)) {
    rlang::abort("Invalid channel specification in colourbar2 guide.")
  }
  if (void[channel] || disc[channel]) {
    rlang::abort("Cannot set colourbar2 guide for empty or discrete channel.")
  }
  if (length(guide$title) > 1) {
    guide$title <- guide$title[channel]
  }

  breaks <- as.list(vec_data(scale$get_breaks(limits = limits))[channel])
  breaks <- do.call(scale$ptype, without_nas(breaks))
  labels <- scale$get_labels()

  ticks <- new_data_frame(setNames(
    list(scale$map(breaks)), aes
  ))
  ticks$.value <- vec_data(breaks)[[channel]]
  ticks$.label <- without_nas(vec_data(labels)[[channel]])
  guide$key <- ticks

  .limits <- without_nas(as.list(vec_data(limits))[channel])
  .bar <- lapply(.limits, function(x) {
    out <- seq(x[1], x[2], length.out = guide$nbin)
    if (length(out) == 0) {
      out <- unique(x)
    }
    return(out)
  })
  .bar <- do.call(scale$ptype, .bar)
  guide$bar <- new_data_frame(list(
    colour = scale$map(.bar),
    value = vec_data(.bar)[[channel]]
  ))
  if (guide$reverse) {
    guide$key <- guide$key[nrow(guide$key):1, ]
    guide$bar <- guide$bar[nrow(guide$bar):1, ]
  }
  guide$hash <- with(guide, digest::digest(list(title, key$.label, bar, name)))
  class(guide) <- c("guide", "colorbar")
  guide
}
