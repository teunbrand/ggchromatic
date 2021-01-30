test_that("guide_colourrect can produce a gtable", {
  # Continuous scales
  scale <- scale_colour_rgb()
  x <- vec_cast(c(0, 1), scale$ptype())
  scale$train(x)

  g <- guide_colourrect(c("title1", "title2"), rectwidth = 5, rectheight = 5)
  g <- guide_train(g, scale, "colour")
  g <- guide_gengrob(g, theme_get())

  expect_s3_class(g, "gtable")
  expect_identical(g$layout$name, c("background", "colours", "frame",
                                    "x_ticks", "y_ticks", "x_labels",
                                    "y_labels", "x_title", "y_title"))

  # Discrete scales
  scale <- scale_colour_rgb()
  x <- rgb_spec(c("A", "B"), c("C", "D"), c("F", "G"))
  scale$train(x)

  g <- guide_colourrect(c("title1", 'title2'))
  g <- guide_train(g, scale, "colour")
  g <- guide_gengrob(g, theme_get())
  expect_s3_class(g, "gtable")
  expect_identical(g$layout$name, c("background", "colours", "frame",
                                    "x_ticks", "y_ticks", "x_labels",
                                    "y_labels", "x_title", "y_title"))
})

test_that("guide_colourrect protests against incorrect scales and parameters", {

  scale <- scale_colour_brewer()

  g <- guide_colourrect()

  expr <- substitute(guide_train(g, scale, "colour"))

  expect_warning(eval(expr), "needs chromatic scales")

  scale <- scale_colour_rgb()
  scale$train(scale$channel_limits)

  g$available_aes <- "nonsense"
  expr <- substitute(guide_train(g, scale, "colour"))
  expect_warning(eval(expr), "needs appropriate scales")

  g$available_aes <- "colour"
  g$channels <- "h"

  expr <- substitute(guide_train(g, scale, "colour"))
  expect_error(eval(expr), "Invalid channel")
})
