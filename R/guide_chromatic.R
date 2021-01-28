#' Chromatic guide
#'
#' This 'guide' is not really a guide, but a decision function for chromatic
#' scales. It chooses the guide based on the number of channels a chromatic
#' scale has seen data from.
#'
#' @inheritParams ggplot2::guide_colorbar
#' @param ... Captures arguments to pass down to the constructor of the chosen
#'   guide.
#'
#' @details This 'guide' chooses in the following ways based on the number of
#' channels for which data is available.
#' \describe{
#'  \item{0 channels}{chooses `guide_none()`}
#'  \item{1 channel}{chooses `guide_colourbar2()`}
#'  \item{2 channels}{chooses `guide_colourrect()`}
#'  \item{3 channels}{chooses `guide_colourcube()`}
#'  \item{More channels}{chooses `guide_colourcube()` with 3 channels}
#' }
#'
#' @return A `chromatic_guide` S3 object.
#' @export
#' @family guides for chromatic scales
#'
#' @examples
#' NULL
guide_chromatic <- function(
  title = waiver(),
  ...,
  available_aes = c("colour", "color", "fill")
) {
  args <- c(list(title = title),
            list(...),
            list(available_aes = available_aes, name = "chromatic"))
  structure(args, class = c("guide", "chromatic_guide"))
}

#' @method guide_train chromatic_guide
#' @export
guide_train.chromatic_guide <- function(guide, scale, aesthetic = NULL) {
  lim <- vec_data(scale$get_limits())
  void <- vapply(lim, function(x){all(is.na(x))}, logical(1))

  fun <- switch(
    as.character(sum(!void)),
    "0" = guide_none,
    "1" = guide_colourbar2,
    "2" = guide_colourrect,
    "3" = guide_colourcube,
    guide_colourcube
  )

  args <- guide
  arg_names <- names(formals(fun))
  arg_names <- arg_names[!arg_names == "..."]
  args <- args[names(args) %in% arg_names]

  new_guide <- do.call(fun, args)
  new_guide$direction <- guide$direction
  guide_train(new_guide, scale, aesthetic)
}
