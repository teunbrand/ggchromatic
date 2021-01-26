# Guide constructor -------------------------------------------------------

#' Chromatic colour rectangle guide
#'
#' The colour rectangle guide is a specialised guide for chromatic scales. It
#' maps two channels of a chromatic scales along the x and y axes and renders a
#' rectangle raster displaying the colours.
#'
#' @inheritParams ggplot2::guide_colorbar
#' @param title A character string or expression indicating the title of guide.
#'   If `NULL`, the title is not shown. By default (`waiver()`), the name of the
#'   scale object or the name specified in `labs()` is used for the title. Note
#'   that the colour rectangle guide can take 2 titles: one for each axis.
#' @param check.overlap If `TRUE`, overlapping labels are silently removed. If
#'   `FALSE`, labels are displayed regardless of whether they overlap.
#' @param rectwidth,rectheight A `numeric(1)` or `grid::unit()` object
#'   specifying the width/height of the colour rectangle. Default value is the
#'   `legend.key.width/height` or `legend.key.size` in the theme, times 4.
#' @param channels A `character()` with the channel names to display. The
#'   default, `"auto"`, removes empty channels and subsequently takes the first
#'   two.
#'
#' @return A `guide_colourrect` S3 object.
#' @export
#'
#' @examples
#' NULL
guide_colourrect <- function(
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
  check.overlap = TRUE,

  # Rectangle
  rectwidth = NULL,
  rectheight = NULL,
  nbin = 50,
  raster = TRUE,

  # Frame
  frame.colour = "black",
  frame.linewidth = 0.5,
  frame.linetype = 1,

  # Ticks
  ticks = TRUE,
  ticks.colour = "black",
  ticks.linewidth = 0.5,

  # General
  default.unit = "line",
  channels = "auto",
  order = 0,
  available_aes = c("colour", "color", "fill"),
  ...
) {
  if (!is.null(rectwidth) && !is.unit(rectwidth)) {
    rectwidth <- unit(rectwidth , default.unit)
  }
  if (!is.null(rectwidth) && !is.unit(rectheight)) {
    rectheight <- unit(rectheight, default.unit)
  }

  structure(list(
    # Title
    title = title,
    title.position = title.position,
    title.theme = title.theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,

    # Label
    label = label,
    label.position = label.position,
    label.theme = label.theme,
    label.hjust = label.hjust,
    label.vjust = label.vjust,
    check.overlap = TRUE,

    # Rectangle
    rectwidth = rectwidth,
    rectheight = rectwidth,
    nbin = nbin,
    raster = raster,

    # Frame
    frame.colour = frame.colour,
    frame.linewidth = frame.linewidth,
    frame.linetype = frame.linetype,

    # Ticks
    ticks = TRUE,
    ticks.colour = "black",
    ticks.linewidth = 0.5,

    # General
    default.unit = default.unit,
    channels = channels,
    order = order,

    # Parameters
    available_aes = available_aes,
    ...,
    name = "colourrect"),
    class = c("guide", "colourrect", "colorbar")
  )
}

# Guide methods -----------------------------------------------------------

#' @export
#' @method guide_train colourrect
guide_train.colourrect <- function(guide, scale, aesthetic = NULL) {
  if (!inherits(scale, "ScaleChromatic")) {
    rlang::warn("The colourrect guide needs chromatic scales.")
    return(NULL)
  }
  if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
    rlang::warn("Colourrect guide needs appropriate scales.")
  }

  aes <- aesthetic %||% scale$aesthetic[[1]]

  guide$key <- guide_key_from_chromatic(scale, aes)

  limits <- vec_data(scale$get_limits())
  void <- vapply(limits, function(x) all(is.na(x)), logical(1))

  # Set proper channels
  if (length(guide$channels) == 1 && guide$channels == "auto") {
    channels <- names(limits)[!void]
  } else {
    channels <- guide$channels
  }
  channels <- match(channels, names(limits))[1:2]
  if (anyNA(channels)) {
    rlang::abort("Invalid channel specification in colourrect guide.")
  }
  if (length(guide$title) == 3) {
    guide$title <- guide$title[channels]
  }
  guide$key$.channel <- match(guide$key$.channel, channels)
  guide$key <- guide$key[!is.na(guide$key$.channel), ]
  limits <- limits[, channels]



  disc <- vapply(limits, is_discrete, logical(1))
  limits <- without_nas(lapply(limits, unique))

  # Sequence between continuous limits
  cols <- clapply(limits, !disc, function(x) {
    seq(x[1], x[2], length.out = guide$nbin)
  })
  bins <- lengths(cols)

  # Make colours
  cols <- setNames(xpand(cols[[1]], rev(cols[[2]])), names(bins))
  cols <- do.call(scale$ptype, cols)
  ch_lim <- set_channel_default(scale$channel_limits,
                                setdiff(names(void), names(bins)))
  cols <- scale$map(cols, channel_limits = ch_lim)
  dim(cols) <- unname(bins)

  guide$rstr <- t(cols)
  guide
}

#' @export
#' @method guide_gengrob colourrect
guide_gengrob.colourrect <- function(guide, theme) {

  cols <- build_rect_grob(guide, theme)
  col_params <- cols$params
  cols <- cols$grob

  frame <- build_rect_frame(guide, col_params)

  axes <- build_rect_axes(guide, theme, col_params)

  titles <- build_rect_titles(guide, theme, col_params)

  hgap <- ggplot2:::width_cm(theme$legend.spacing.x %||%
                               0.5 * unit(titles$fontsize, "pt"))
  vgap <- ggplot2:::height_cm(theme$legend.spacing.y %||%
                                0.5 * unit(titles$fontsize, "pt"))


  widths <- c(titles$width, hgap, axes$label.width,
              axes$ticklength, col_params$size$width)
  heights <- c(col_params$size$height, axes$ticklength, axes$label.height,
               vgap, titles$height)

  padding <- convertUnit(theme$legend.margin %||% margin(), "cm",
                         valueOnly = TRUE)

  widths <- c(padding[4], widths,  padding[2])
  heights <- c(padding[1], heights, padding[3])

  xpos <- 6
  ypos <- 2

  gt <- gtable(widths = unit(widths, "cm"),
               heights = unit(heights, "cm"))
  gt <- gtable_add_grob(
    gt, element_render(theme, "legend.background"), clip = "off",
    t = 1, r = -1, b = -1, l = 1, name = "background"
  )
  gt <- gtable_add_grob(
    gt, cols, clip = 'off',
    t = ypos, l = xpos, r = xpos, b = ypos, name = "colours"
  )
  gt <- gtable_add_grob(
    gt, frame, clip = "off",
    t = ypos, l = xpos, r = xpos, b = ypos, name = "frame"
  )
  gt <- gtable_add_grob(
    gt, axes$xticks, clip = "off",
    t = ypos + 1, l = xpos, r = xpos, b = ypos + 1, name = "x_ticks"
  )
  gt <- gtable_add_grob(
    gt, axes$yticks, clip = "off",
    t = ypos, r = xpos - 1, l = xpos - 1, b = ypos, name = "y_ticks"
  )
  gt <- gtable_add_grob(
    gt, axes$xlabs, clip = "off",
    t = ypos + 2, l = xpos, r = xpos, b = ypos + 2, name = "x_labels"
  )
  gt <- gtable_add_grob(
    gt, axes$ylabs, clip = "off",
    t = ypos, r = xpos - 2, l = xpos - 2, b = ypos, name = "y_labels"
  )
  gt <- gtable_add_grob(
    gt, titles$xtitle, clip = "off",
    t = ypos + 4, r = xpos, l = xpos, b = ypos + 4, name = "y_labels"
  )
  gt <- gtable_add_grob(
    gt, titles$ytitle, clip = "off",
    t = ypos, r = xpos - 4, l = xpos - 4, b = ypos, name = "y_labels"
  )
  gt

}


# Grob constructors -------------------------------------------------------

build_rect_grob <- function(guide, theme) {

  # What does theme think?
  width  <- theme$legend.key.width  %||% theme$legend.key.size
  height <- theme$legend.key.height %||% theme$legend.key.size

  # What does guide think?
  width  <- ggplot2:::width_cm(guide$rectwidth   %||% width * 4)
  height <- ggplot2:::height_cm(guide$rectheight %||% height * 4)

  rectgrob <- rasterGrob(guide$rstr,
                         width = width,
                         height = height,
                         default.units = "cm")

  params <- list(
    size = list(width = width, height = height)
  )

  return(list(grob = rectgrob, params = params))
}

build_rect_frame <- function(guide, params) {

  width <- params$size$width
  height <- params$size$height

  x = c(0, 0, 1)
  y = c(1, 0, 0)

  grob <- linesGrob(x = x, y = y,
                    gp = gpar(
                      col = guide$frame.colour,
                      lty = guide$frame.linetype,
                      lwd = guide$frame.linewidth * .pt
                    ))
  return(grob)
}

build_rect_axes <- function(guide, theme, params) {
  key <- guide$key
  values <- split(key$.value, key$.channel)
  .labels <- split(key$.label, key$.channel)

  ticklength <- 0.05

  if (guide$ticks) {
    xticks <- polylineGrob(
      x = unit(rep(values[[1]], 2), "npc"),
      y = unit(rep(c(0, ticklength), each = length(values[[1]])), "cm"),
      id = rep(seq_along(values[[1]]), 2),
      gp = gpar(
        col = guide$ticks.colour,
        lwd = guide$ticks.linewidth * .pt
      )
    )
    yticks <- polylineGrob(
      x = unit(rep(c(0, ticklength), each = length(values[[2]])), "cm"),
      y = unit(rep(values[[2]], 2), "npc"),
      id = rep(seq_along(values[[2]]), 2),
      gp = gpar(
        col = guide$ticks.colour,
        lwd = guide$ticks.linewidth * .pt
      )
    )
  } else {
    xticks <- zeroGrob()
    yticks <- zeroGrob()
    ticklength <- 0
  }

  if (guide$label) {
    label.theme <- guide$label.theme %||% calc_element("legend.text", theme)
    height <- convertUnit(stringHeight(.labels[[1]]), "cm", valueOnly = TRUE)
    height <- max(height)
    xlabs <- element_grob(
      label.theme,
      label = .labels[[1]],
      x = unit(values[[1]], "npc"),
      check.overlap = guide$check.overlap
    )

    width <- convertUnit(stringWidth(.labels[[2]]), "cm", valueOnly = TRUE)
    width <- max(width)
    ylabs <- element_grob(
      label.theme,
      label = .labels[[2]],
      y = unit(values[[2]], "npc"),
      check.overlap = guide$check.overlap
    )
  } else {
    height <- 0
    width <- 0
    xlabs <- zeroGrob()
    ylabs <- zeroGrob()
  }

  out <- list(
    xticks = xticks,
    yticks = yticks,
    xlabs = xlabs,
    ylabs = ylabs,
    ticklength = ticklength,
    label.width = width,
    label.height = height
  )
}

build_rect_titles <- function(guide, theme, params) {

  title.theme <- guide$title.theme %||% calc_element("legend.title", theme)
  title.hjust <- guide$title.hjust %||% theme$legend.title.align %||%
    title.theme$hjust %||% 0.5
  title.vjust <- guide$title.vjust %||% title.theme$vjust %||% 0.5

  if (length(guide$title) > 1) {
    label <- c(guide$title, rep("", 2 - length(guide$title)))

    xtitle <- element_grob(
      title.theme,
      label = label[1],
      hjust = title.hjust,
      margin_x = TRUE,
      margin_y = TRUE
    )
    ytitle <- element_grob(
      title.theme,
      label = label[2],
      vjust = title.vjust,
      margin_x = TRUE,
      margin_y = TRUE
    )

    height = convertUnit(grobHeight(xtitle), "cm", valueOnly = TRUE)
    width  = convertUnit(grobWidth(ytitle),  "cm", valueOnly = TRUE)
    fontsize <- title.theme$size %||%
      calc_element("legend.title", theme)$size %||%
      calc_element("text", theme)$size %||% 11

    return(list(
      xtitle = xtitle,
      ytitle = ytitle,
      height = height,
      width  = width,
      fontsize = fontsize
    ))
  }
}
