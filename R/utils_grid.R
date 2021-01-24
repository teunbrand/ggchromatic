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
