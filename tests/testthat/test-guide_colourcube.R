test_that("guide_colourcube can produce a gtable", {
  # Continuous scales
  scale <- scale_colour_rgb()
  x <- vec_cast(c(0, 1), scale$ptype())
  scale$train(x)

  g <- guide_colourcube("title", title.position = "top",
                        cubewidth = 5, cubeheight =5, title.offset = 1)
  g <- guide_train(g, scale, "colour")
  g <- guide_gengrob(g, theme_get())

  expect_s3_class(g, "gtable")
  expect_identical(g$layout$name, c("background", "cube", "frame",
                                    "axes", "title"))

  # Discrete scales
  scale <- scale_colour_rgb()
  x <- rgb_spec(c("A", "B"), c("C", "D"), c("F", "G"))
  scale$train(x)

  g <- guide_colourcube("title", title.position = "left")
  g <- guide_train(g, scale, "colour")
  g <- guide_gengrob(g, theme_get())

  expect_s3_class(g, "gtable")
  expect_identical(g$layout$name, c("background", "cube", "frame",
                                    "axes", "title"))
})

test_that("guide_colourcube protests against incorrect scales and parameters", {
  scale <- scale_colour_brewer()

  g <- guide_colourcube()

  expr <- substitute(guide_train(g, scale, "colour"))

  expect_warning(eval(expr), "needs chromatic scales")

  scale <- scale_colour_rgb()
  scale$train(scale$channel_limits)

  g$available_aes <- "nonsense"
  expr <- substitute(guide_train(g, scale, "colour"))
  expect_warning(eval(expr), "needs appropriate scales")


})
