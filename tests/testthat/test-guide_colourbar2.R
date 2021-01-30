test_that("guide_colourbar2 can produce a gtable", {

  scale <- scale_colour_rgb()
  scale$train(scale$channel_limits)

  g <- guide_colourbar2(barwidth = 1, barheight = 1, title.position = "top",
                        reverse = TRUE)
  g <- guide_train(g, scale, "colour")
  expect_s3_class(g, "colorbar") # Note: NOT colourbar2 anymore

  g$direction <- "vertical"
  g <- guide_gengrob(g, theme_get())
  expect_s3_class(g, "gtable")
  expect_identical(g$layout$name, c("background", "bar", "label",
                                    "title", "ticks"))
})

test_that("guide_colourbar2 protests against incorrect scales and parameters", {
  scale <- scale_colour_brewer()

  g <- guide_colourbar2()

  expr <- substitute(guide_train(g, scale, "colour"))

  expect_warning(eval(expr), "needs chromatic scales")

  scale <- scale_colour_rgb()
  scale$train(scale$channel_limits)

  g$available_aes <- "nonsense"
  expr <- substitute(guide_train(g, scale, "colour"))
  expect_warning(eval(expr), "needs appropriate scales")

  g$available_aes <- "colour"
  g$channel <- "h"
  expr <- substitute(guide_train(g, scale, "colour"))
  expect_error(eval(expr), "Invalid channel")

  # Test discrete
  scale <- scale_colour_rgb()
  scale$train(rgb_spec(c("A", "B")))
  g$channel <- "r"

  expr <- substitute(guide_train(g, scale, "colour"))
  expect_error(eval(expr), "empty or discrete channel")

})
