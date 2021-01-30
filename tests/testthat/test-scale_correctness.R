scales <- list(
  scale_colour_rgb(),
  scale_fill_rgb(),
  scale_colour_hsv(),
  scale_fill_hsv(),
  scale_colour_hsl(),
  scale_fill_hsl(),
  scale_colour_hcl(),
  scale_fill_hcl(),
  scale_colour_cmyk(),
  scale_fill_cmyk(),
  scale_colour_cmy(),
  scale_fill_cmy()
)

test_that("scale_* functions give correct prototypes", {
  ptypes_text <- paste0(rep(c("rgb", "hsv", "hsl", "hcl", "cmyk", "cmy"),
                            each = 2),
                        "_spec")
  ptypes <- lapply(scales, function(x){x$ptype()})
  mapply(function(p, pt) {
    expect_s3_class(p, pt)
  }, p = ptypes, pt = ptypes_text)
})

test_that("scale_* can map their own channel limits", {
  map <- vapply(scales, function(x) {
    x$train(x$channel_limits)
    x$map(x$channel_limits)
  }, character(2))

  exp <- matrix(c(
    "#000000", "#FFFFFF",
    "#000000", "#FFFFFF",
    "#000000", "#FF0000",
    "#000000", "#FF0000",
    "#000000", "#FFFFFF",
    "#000000", "#FFFFFF",
    "#000000", "#FF9FE7",
    "#000000", "#FF9FE7",
    "#FFFFFF", "#000000",
    "#FFFFFF", "#000000",
    "#FFFFFF", "#000000",
    "#FFFFFF", "#000000"
  ), nrow = 2)

  expect_identical(map, exp)
})
