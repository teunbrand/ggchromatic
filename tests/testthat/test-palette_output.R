test_that("palettes can produce red", {
  x <- c(rgb_palette(rgb_spec(1, 0, 0)),
         hsv_palette(hsv_spec(0, 1, 1)),
         hsl_palette(hsl_spec(0, 1, 0.5)),
         hcl_palette(hcl_spec(0.034, 0.995, 0.532)),
         cmyk_palette(cmyk_spec(0, 1, 1, 0)),
         cmy_palette(cmy_spec(0, 1, 1)),
         lab_palette(lab_spec(0.5, 1, 1)),
         lch_palette(lch_spec(0.53, 0.79, 0.11)))
  x <- unique(x)
  expect_identical(x, "#FF0000")
})

test_that("palettes stop when incorrect colour_spec is supplied", {
  x <- substitute(rgb_palette(hsv_spec(0, 1, 1)))
  expect_error(
    eval(x),
    "only applies to"
  )
})
