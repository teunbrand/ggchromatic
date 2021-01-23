# Guide constructor -------------------------------------------------------

#' Chromatic colour cube guide
#'
#' The colour cube guide is a specialised guide for chromatic scales. It maps
#' three channels of a chromatic scale along three dimensions and renders an
#' isometric cube displaying the colours.
#'
#' @inheritParams ggplot2::guide_colorbar
#' @param title A character string or expression indicating the title of guide. If
#'   `NULL`, the title is not shown. By default (`waiver()`), the name of the
#'   scale object or the name specified in `labs()` is used for the title. Note
#'   that the colour cube guide can take 3 titles: one for each axis.
#' @param title.offset A `numeric(1)` in centimeters determining how far away
#'   the axis titles should be drawn relative to the rightmost labels.
#' @param cubewidth,cubeheight A `numeric(1)` or `grid::unit()` object
#'   specifying the width/height of the colourcube. Default value is the
#'   `legend.key.width/height` or `legend.key.size` in the theme. The colour
#'   cube guide takes the lesser of the width/height for the size.
#' @param rotate NOT IMPLEMENTED YET.
#'
#' @return A `guide_colourcube` S3 object.
#' @export
#'
#' @examples
#' NULL
guide_colourcube <- function(
  # Title
  title = waiver(),
  title.position = NULL,
  title.theme = NULL,
  title.hjust = NULL,
  title.vjust = NULL,
  title.offset = 0.2,

  # Label
  label = TRUE,
  label.theme = NULL,
  label.hjust = NULL,
  label.vjust = NULL,

  # Cube
  cubewidth = NULL,
  cubeheight = NULL,
  nbin = 20,
  rotate = c(1, 2, 3, 4),

  # Frame
  frame.colour = "black",
  frame.linewidth = 0.5,
  frame.linetype = 1,

  # Ticks
  ticks = TRUE,
  ticks.colour = "black",
  ticks.linewidth = 0.5,

  default.unit = "line",
  order = 0,
  available_aes = c("colour", "color", "fill"),
  ...
) {
  if (!is.null(cubewidth) && !is.unit(cubewidth)) {
    cubewidth <- unit(cubewidth, default.unit)
  }
  if (!is.null(cubeheight) && !is.unit(cubeheight)) {
    cubeheight <- unit(cubeheight, default.unit)
  }
  if (!is.null(title.offset) && is.unit(title.offset)) {
    title.offset <- convertUnit(title.offset, "cm", valueOnly = TRUE)
  }

  structure(list(
    # Title
    title = title,
    title.position = title.position,
    title.theme = title.theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,
    title.offset = title.offset,

    # Label
    label = label,
    label.theme = label.theme,
    label.hjust = label.hjust,
    label.vjust = label.vjust,
    check.overlap  = TRUE,

    # Cube
    cubewidth = cubewidth,
    cubeheight = cubeheight,
    nbin = nbin,

    # Frame
    frame.colour = frame.colour,
    frame.linewidth = frame.linewidth,
    frame.linetype = frame.linetype,

    # Ticks
    ticks = ticks,
    ticks.colour = ticks.colour,
    ticks.linewidth = ticks.linewidth,

    # General
    default.unit = default.unit,
    order = order,

    # Parameter
    available_aes = available_aes,
    ...,
    name = "colourcube"
  ), class = c("guide", "colourcube", "colorbar"))
}

# Guide methods -----------------------------------------------------------

#' @export
#' @method guide_train colourcube
guide_train.colourcube <- function(guide, scale, aesthetic = NULL) {
  if (!inherits(scale, "ScaleChromatic")) {
    rlang::warn("The colourcube guide needs chromatic scales.")
    return(NULL)
  }
  if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
    rlang::warn("colourcube guide needs appropriate scales")
    return(NULL)
  }

  aes <- aesthetic %||% scale$aesthetic[[1]]

  guide$key <- guide_key_from_chromatic(scale, aes)

  # Drop any channels beyond 3. I cannot make 4D hypercubes.
  limits <- vec_data(scale$get_limits())[1:3]
  disc   <- vapply(limits, is_discrete, logical(1))
  void   <- vapply(limits, function(x) all(is.na(x)), logical(1))
  limits <- without_nas(lapply(limits, unique))
  limits[void] <- list(NA)

  tails  <- lapply(limits, tail, 1)

  # Sequence between continuous limits
  cols <- clapply(limits, !disc, function(x) {
    seq(x[1], x[2], length.out = guide$nbin)
  })
  bins <- lengths(cols)

  # Create grid at every frontal face of the cube
  cols <- lapply(list(c(1, 2), c(2, 3), c(3, 1)), function(i) {
    out <- setNames(xpand(cols[[i[[1]]]], cols[[i[[2]]]]), names(bins)[i])
    miss <- setdiff(c(1, 2, 3), i)
    out[[names(bins)[[miss]]]] <- tails[[miss]]
    out[names(bins)]
  })
  cols <- setNames(vec_rbind(!!!cols), names(limits))

  # Translate grids to colours
  cols <- do.call(scale$ptype, cols[!void])
  cols <- scale$map(cols)

  # Build cube and attach colours
  cube <- init_cube_faces(bins[1], bins[2], bins[3])
  cube$colours <- cols[cube$id]

  guide$cube <- cube
  guide
}

#' @export
#' @method guide_gengrob colourcube
guide_gengrob.colourcube <- function(guide, theme) {

  # Generate all grobs
  cube <- build_cube_grob(guide, theme)
  params <- cube$params
  cube <- cube$grob

  frame <- build_cube_frame(
    guide$key, theme, params = params,
    colour = guide$frame.colour,
    linetype = guide$frame.linetype,
    linewidth = guide$frame.linewidth
  )

  axes <- build_cube_axes(guide, theme, params)
  title <- build_cube_titles(guide, theme, axes$params)
  axes <- axes$grob
  tpar  <- title$params
  title <- title$grob

  # Set out parameters
  max_x <- tpar$max_x
  max_y <- tpar$max_y
  min_y <- tpar$min_y
  min_x <- tpar$min_x

  widths <- max_x
  heights <- c(max_y, abs(min_y))

  pos_x <- 2
  pos_y <- 2

  if (length(guide$title) == 1) {
    hgap <- ggplot2:::width_cm(theme$legend.spacing.x %||%
                                 (0.5 * unit(tpar$title_fontsize, "pt")))
    vgap <- ggplot2:::height_cm(theme$legend.spacing.y %||%
                                  (0.5 * unit(tpar$title_fontsize, "pt")))
    switch(
      guide$title.position,
      "top" = {
        pos_y <- 4
        widths  <- c(widths, max(0, tpar$title_width - sum(widths)))
        heights <- c(tpar$title_height, vgap, heights)
        tpos <- list(t = 2, l = 2, r = 3, b = 2)
      },
      "bottom" = {
        widths <- c(widths, max(0, tpar$title_width - sum(widths)))
        heights <- c(heights, vgap, tpar$title_height)
        tpos <- list(t = 5, l = 2, r = 3, b = 5)
      },
      "left" = {
        pos_x <- 4
        widths  <- c(tpar$title_width, hgap, widths)
        heights <- c(heights, max(0, tpar$title_height - sum(heights)))
        tpos <- list(t = 2, l = 2, r = 2, b = 3)
      },
      "right" = {
        widths  <- c(widths, hgap, tpar$title_width)
        heights <- c(heights, max(0, tpar$title_height - sum(heights)))
        tpos <- list(t = 2, l =  4, r = 4, b = 3)
      }
    )
  } else {
    tpos <- list(t = 2, l = 2, r = 2, b = 2)
  }

  padding <- convertUnit(theme$legend.margin %||% margin(),
                         "cm", valueOnly = TRUE)

  widths  <- c(padding[4], widths,  padding[2])
  heights <- c(padding[1], heights, padding[3])


  gt <- gtable(widths = unit(widths, "cm"), heights = unit(heights, "cm"))
  gt <- gtable_add_grob(
    gt, element_render(theme, "legend.background"), clip = "off",
    t = 1, r = -1, b = -1, l = 1, name = "background"
  )
  gt <- gtable_add_grob(
    gt, cube, clip = "off",
    t = pos_y, l = pos_x, b = pos_y, r = pos_x, name = "cube"
  )
  gt <- gtable_add_grob(
    gt, frame, clip = "off",
    t = pos_y, l = pos_x, b = pos_y, r = pos_x, name = "frame"
  )
  gt <- gtable_add_grob(
    gt, axes, clip = "off",
    t = pos_y, l = pos_x, b = pos_y, r = pos_x, name = "axes"
  )
  gt <- gtable_add_grob(
    gt, title, clip = "off",
    t = tpos$t, l = tpos$l, b = tpos$b, r = tpos$r, name = "title"
  )
  gt
}

# Grob constructors -------------------------------------------------------

build_cube_grob <- function(guide, theme) {
  # What does the theme think about size?
  width  <- theme$legend.key.width  %||% theme$legend.key.size
  height <- theme$legend.key.height %||% theme$legend.key.size

  # What does guide think about size?
  width  <- ggplot2:::width_cm(guide$cubewidth   %||% width * 5)
  height <- ggplot2:::height_cm(guide$cubeheight %||% height * 5)

  # Take minimum of width/height
  size <- min(width, height)

  # Calculate dimensions
  cube <- guide$cube
  x_range <- range(cube$x)
  y_range <- range(cube$y)
  asp <- diff(x_range) / diff(y_range)
  width <- size * asp
  height <- size

  # Rescale cube to fit desired size
  cube <- transform(
    cube,
    x = rescale(x, from = x_range, to = c(0, width)),
    y = rescale(y, from = y_range, to = c(0, height))
  )

  # Make cube grob
  firsts <- !duplicated(cube$id)
  grob <- with(cube, polygonGrob(
    x = x, y = y, id = id,
    gp = gpar(fill = colours[firsts], col = NA),
    default.units = "cm"
  ))

  params <- list(
    range = list(x = x_range, y = y_range),
    size  = list(width = width, height = height)
  )
  params <- measure_polygongrob(grob, params)

  return(list(grob = grob, params = params))
}

build_cube_frame <- function(key, theme, params, colour, linetype, linewidth) {
  # The edges of the cube
  frame <- new_data_frame(list(
    x = c(0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
    y = c(0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1),
    z = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0)
  ))

  # Build extra beams for discrete variables
  if (any(key$.discrete)) {
    key <- key[key$.discrete, ]
    beams <- new_data_frame(list(x = numeric(), y = numeric(), z = numeric()))
    if (any(key$.channel == 1)) {
      beam <- key[key$.channel == 1, ]
      value <- diff(beam$.value) / 2 + head(beam$.value, -1)
      init <- list(
        rep(c(0, 1, 1, 1), length(value)),
        rep(c(0, 0, 0, 1), length(value))
      )
      beam <- vec_set_names(
        append(init, list(rep(value, each = 4)), beam$.channel[1] - 1L),
        c("x", "y", "z")
      )
      beams <- vec_rbind(beams, new_data_frame(beam))
    }
    if (any(key$.channel == 2)) {
      beam <- key[key$.channel == 2, ]
      value <- diff(beam$.value) / 2 + head(beam$.value, -1)
      init <- list(
        rep(c(1, 0, 0, 1), length(value)),
        rep(c(0, 0, 0, 1), length(value))
      )
      beam <- vec_set_names(
        append(init, list(rep(value, each = 4)), beam$.channel[1] - 1L),
        c("x", "y", "z")
      )
      beams <- vec_rbind(beams, new_data_frame(beam))
    }
    if (any(key$.channel == 3)) {
      beam <- key[key$.channel == 3, ]
      value <- diff(beam$.value) / 2 + head(beam$.value, -1)
      init <- list(
        rep(c(0, 0, 0, 1), length(value)),
        rep(c(0, 1, 1, 1), length(value))
      )
      beam <- vec_set_names(
        append(init, list(rep(value, each = 4)), beam$.channel[1] - 1L),
        c("x", "y", "z")
      )
      beams <- vec_rbind(beams, new_data_frame(beam))
    }
    frame <- vec_rbind(frame, beams)
  }

  # Project and rescale
  frame <- do.call(project_isometric, frame)
  frame <- transform(
    frame,
    x = rescale(x, from = params$range$x, to = c(0, params$size$width)),
    y = rescale(y, from = params$range$y, to = c(0, params$size$height)),
    id = rep(seq_len(nrow(frame) / 2), each = 2)
  )

  grob <- with(frame, polylineGrob(
    x = x, y = y, id = id,
    gp = gpar(col = colour,
              lty = linetype,
              lwd = linewidth * .pt),
    default.units = "cm"
  ))
  return(grob)
}

build_cube_axes <- function(guide, theme, params) {
  key <- guide$key
  nr <- nrow(key)
  values <- split(key$.value, key$.channel)
  labels <- split(key$.label, key$.channel)

  ticklength <- 0.05

  lens <- lengths(values)

  base_position <- new_data_frame(list(
    x = c(values[[1]], rep(1, lens[2] + lens[3])),
    y = c(rep(0, lens[1]), values[[2]], rep(1, lens[[3]])),
    z = c(rep(0, lens[1] + lens[2]), values[[3]]),
    id = seq_len(sum(lens))
  ))

  offset <- new_data_frame(list(
    x = rep(c(0,  1, 0), lens),
    y = rep(c(0,  0, 1), lens),
    z = rep(c(-1, 0, 0), lens)
  ))

  if (guide$label) {
    labels <- project_isometric(
      x = c(base_position$x + offset$x * ticklength * 2),
      y = c(base_position$y + offset$y * ticklength * 2),
      z = c(base_position$z + offset$z * ticklength * 2)
    )
    labels <- transform(
      labels,
      x = rescale(x, from = params$range$x, to = c(0, params$size$width)),
      y = rescale(y, from = params$range$y, to = c(0, params$size$height)),
      hjust = rep(c(0, 0, 0.5), lens),
      vjust = rep(c(0.5, 0.5, 0), lens),
      lab = key$.label
    )
    label.theme <- guide$label.theme %||% calc_element("legend.text", theme)
    label_grob <- with(labels, element_grob(
      label.theme,
      label = lab, x = unit(x, "cm"), y = unit(y, "cm"),
      hjust = hjust, vjust = vjust,
      check.overlap = guide$check.overlap,
    ))
  } else {
    label_grob <- NULL
  }

  if (!is.null(guide$ticks.colour) && !is.na(guide$ticks.colour)) {
    ticks <- project_isometric(
      x = c(base_position$x, base_position$x + offset$x * ticklength),
      y = c(base_position$y, base_position$y + offset$y * ticklength),
      z = c(base_position$z, base_position$z + offset$z * ticklength)
    )
    ticks <- transform(
      ticks,
      x = rescale(x, from = params$range$x, to = c(0, params$size$width)),
      y = rescale(y, from = params$range$y, to = c(0, params$size$height)),
      id = rep(base_position$id, 2)
    )
    ticks_grob <- with(ticks, polylineGrob(
      x = x, y = y, id = id,
      gp = gpar(col = guide$ticks.colour,
                lwd = guide$ticks.linewidth * .pt),
      default.units = "cm"
    ))
  } else {
    ticks_grob <- NULL
  }

  # Measure grob widths
  params <- measure_titlegrob(label_grob, params)
  params <- measure_polygongrob(ticks_grob, params)

  grobs <- list(ticks = ticks_grob, label = label_grob)
  grobs <- grobs[!vapply(grobs, is.null, logical(1))]
  if (length(grobs) == 0) {
    return(list(grob = zeroGrob(), params = params))
  } else {
    grobs <- do.call(grobTree, grobs)
    return(list(grob = grobs, params = params))
  }
}

build_cube_titles <- function(guide, theme, params) {

  title.theme <- guide$title.theme %||% calc_element("legend.title", theme)

  if (length(guide$title) > 1) {
    rel <- (params$max_x + guide$title.offset) / params$size$width

    pos <- project_isometric(
      x = ifelse(c(0, 1, 1, 1, 1, 1) == 0, 1 - rel, rel),
      y = ifelse(c(0, 0, 0, 1, 1, 1) == 0, 1 - rel, rel),
      z = ifelse(c(0, 0, 0, 0, 0, 1) == 0, 1 - rel, rel)
    )
    pos <- as.matrix(transform(
      pos,
      x = rescale(x, from = params$range$x, to = c(0, params$size$width)),
      y = rescale(y, from = params$range$y, to = c(0, params$size$height))
    ))
    ang <- pos[c(2,4,6),] - pos[c(1,3,5),]
    ang <- atan2(ang[, 2], ang[, 1])
    ang <- ang * (180 / pi) %% 360
    upsidedown <- ang >= 90 & ang < 270
    ang <- ifelse(upsidedown, ang + 180, ang) %% 360
    pos <- (pos[c(2,4,6),] + pos[c(1,3,5),]) / 2

    # Need to suppress warnings here because ggplot warns about mixed angle
    # usage
    grob <- suppressWarnings(element_grob(
      title.theme,
      label = guide$title,
      angle = ang,
      x = unit(pos[, 1], "cm"), y = unit(pos[, 2], "cm"),
      hjust = 0.5, vjust = 0,
    ))

    params <- measure_titlegrob(grob, params)

  } else if (!is.null(guide$title)){
    title.hjust <- guide$title.hjust %||% theme$legend.title.align %||%
      title.theme$hjust %||% 0
    title.vjust <- guide$title.vjust %||% title.theme$vjust %||% 0.5

    grob <- element_grob(
      title.theme,
      label = guide$title[[1]],
      hjust = title.hjust,
      vjust = title.vjust,
      margin_x = TRUE,
      margin_y = TRUE
    )

    params$title_width  <- ggplot2:::width_cm(grob)
    params$title_height <- ggplot2:::height_cm(grob)
    params$title_fontsize <- title.theme$size %||%
      calc_element("legend.title", theme)$size %||%
      calc_element("text", theme)$size %||% 11
  } else {
    grob <- zeroGrob()
  }

  return(list(grob = grob,
              params = params))
}

# Helpers -----------------------------------------------------------------

guide_key_from_chromatic <- function(scale, aes) {
  limits <- scale$get_limits()
  breaks <- scale$get_breaks(limits = limits)
  labels <- scale$get_labels(breaks)

  disc <- channel_is_discrete(breaks) & !channel_is_void(breaks)

  scaled_breaks <- scale$rescale(breaks, limits = limits)
  scaled_breaks <- vec_data(scaled_breaks)

  # Manually rescale discrete breaks because of tick mark placement
  for (f in fields(breaks)[disc]) {
    brk <- field(breaks, f)
    lim <- without_nas(field(limits, f))
    new_brk <- rep(NA, length(brk))
    new_brk[!is.na(brk)] <- rescale(
      match(brk[!is.na(brk)], lim),
      from = c(0.5, length(lim) + 0.5)
    )
    scaled_breaks[[f]] <- new_brk
  }

  ticks <- new_data_frame(list(
    colour = rep(scale$map(breaks), n_fields(breaks)),
    .value = melt_channels(scaled_breaks),
    .label = melt_channels(labels),
    .channel = rep(seq_len(n_fields(breaks)), each = length(breaks))
  ))
  colnames(ticks)[[1]] <- aes
  ticks$.discrete <- disc[ticks$.channel]

  valid_breaks <- !vec_c(!!!lapply(unname(vec_data(breaks)), is.na))

  ticks <- ticks[valid_breaks, ]
}

project_isometric <- function(x, y, z, angle1 = 30, angle2 = 45) {
  # Degrees to radians
  angle1 <- asin(tan(angle1 * (pi / 180)))
  angle2 <- angle2 * (pi / 180)

  angle1 <- c(1, 0, 0,
              0, cos(angle1), -sin(angle1),
              0, sin(angle1), cos(angle1))
  dim(angle1) <- c(3, 3)

  angle2 <- c(cos(angle2), 0, sin(angle2),
              0, 1, 0,
              -sin(angle2), 0, cos(angle2))
  dim(angle2) <- c(3, 3)

  # Rotation matrix
  rot <- angle1 %*% angle2

  # Coordinates as matrix
  coords <- matrix(c(x, y, z), ncol = 3)

  # Rotate coordinates
  coords <- tcrossprod(rot, coords)

  # Project to xy-plane
  coords <- diag(c(1, 1, 0)) %*% coords
  new_data_frame(list(x = coords[1, ], y = coords[2, ]))
}

init_cube_faces <- function(xbins, ybins = xbins, zbins = xbins) {
  # Generate sequences
  x <- rev(seq_len(xbins) - 1) / xbins
  y <- (seq_len(ybins) - 1) / ybins
  z <- rev(seq_len(zbins) - 1) / zbins
  nr <- c(prod(xbins, ybins), prod(ybins, zbins), prod(zbins, xbins))

  # Generate offsets
  xo <- 1 / xbins
  yo <- 1 / ybins
  zo <- 1 / zbins

  # Setup planes
  xy <- xpand(x, y)
  yz <- xpand(y, z)
  zx <- xpand(z, x)

  id <- rep(seq_len(nr[1]), 4)
  id <- c(id, rep(seq_len(nr[2]), 4) + nr[1])
  id <- c(id, rep(seq_len(nr[3]), 4) + sum(nr[1:2]))

  coords <- project_isometric(
    x = c(rep(xy$a, 2), rep(xy$a + xo, 2), #xy, yz, zx
          rep(0, 4 * nr[2]),
          zx$b, rep(zx$b + xo, 2), zx$b),
    y = c(xy$b, rep(xy$b + yo, 2), xy$b,
          rep(yz$a, 2), rep(yz$a + yo, 2),
          rep(1, 4 * nr[3])),
    z = c(rep(0, 4 * nr[1]),
          yz$b, rep(yz$b + zo, 2), yz$b,
          rep(zx$a, 2), rep(zx$a + zo, 2))
  )

  coords$id <- id
  coords
}


xpand <- function(a, b) {
  new_data_frame(list(
    a = rep.int(a, length(b)),
    b = rep.int(b, rep.int(length(a), length(b)))
  ))
}

measure_titlegrob <- function(grob, params, unit = "cm") {
  if (is.null(grob) || inherits(grob, "zeroGrob")) {
    return(params)
  }
  measure <- measure_labels(grob)

  x <- mean(range(measure$x, na.rm = TRUE))
  y <- mean(range(measure$y, na.rm = TRUE))

  w <- convertWidth(grobWidth(grob), "cm", valueOnly = TRUE)
  h <- convertHeight(grobHeight(grob), "cm", valueOnly = TRUE)

  params$min_x <- min(x - 0.5 * w, params$min_x %||% 0)
  params$max_x <- max(x + 0.5 * w, params$max_x %||% 0)
  params$min_y <- min(y - 0.5 * h, params$min_y %||% 0)
  params$max_y <- max(y + 0.5 * h, params$max_y %||% 0)

  return(params)
}

measure_polygongrob <- function(grob, params, unit = "cm") {
  if (is.null(grob) || inherits(grob, "zeroGrob")) {
    return(params)
  }
  x <- range(convertUnit(grob$x, unit, valueOnly = TRUE))
  y <- range(convertUnit(grob$y, unit, valueOnly = TRUE))

  params$min_x <- min(x, params$min_x %||% 0)
  params$max_x <- max(x, params$max_x %||% 0)
  params$min_y <- min(y, params$min_y %||% 0)
  params$max_y <- max(y, params$max_y %||% 0)

  return(params)
}

measure_labels <- function(grob, unit = "cm") {
  if (is.null(grob) || inherits(grob, "zeroGrob") || !is.grob(grob)) {
    return(list(x = NA, y = NA))
  }
  if ("children" %in% names(grob)) {
    if (all(c("xext", "yext") %in% names(grob))) {

      x <- convertUnit(grob$x + unit(range(grob$xext), "pt"),
                       "cm", valueOnly = TRUE)
      y <- convertUnit(grob$y + unit(range(grob$yext), "pt"),
                       "cm", valueOnly = TRUE)
    } else {
      # Recursion through the children
      children <- lapply(unname(grob$children), measure_labels)
      x <- unlist(lapply(children, `[[`, "x"), recursive = TRUE)
      y <- unlist(lapply(children, `[[`, "y"), recursive = TRUE)
    }
    x <- range(x, na.rm = TRUE)
    y <- range(y, na.rm = TRUE)
    return(list(x = x, y = y))
  }
  if (inherits(grob, "text")) {
    x <- convertX(grob$x, "cm", valueOnly = TRUE)
    y <- convertY(grob$y, "cm", valueOnly = TRUE)
    w <- convertWidth(stringWidth(grob$label), "cm", valueOnly = TRUE)
    h <- convertHeight(stringWidth(grob$label), "cm", valueOnly = TRUE)
    x <- range(c(x + 0.5 * w, x - 0.5 * w), na.rm = TRUE)
    y <- range(c(y + 0.5 * h, y - 0.5 * h), na.rm = TRUE)
    return(list(x = x, y = y))
  } else {
    return(list(x = NA, y = NA))
  }
}
