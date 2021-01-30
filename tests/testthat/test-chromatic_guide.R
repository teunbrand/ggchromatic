test_that("chromatic guide finds the correct guides", {

  basis <- ggplot(mtcars, aes(wt, mpg))

  get_guide <- function(g) {
    g <- ggplotGrob(g)
    g <- g$grobs[g$layout$name == "guide-box"][[1]]
    g$grobs[g$layout$name == "guides"][[1]]
  }

  # Test cube choice
  g <- basis + geom_point(aes(colour = rgb_spec(wt, mpg, drat)))
  g <- get_guide(g)
  expect_identical(g$layout$name, c("background", "cube", "frame",
                                    "axes", "title"))

  # Test rect choice
  g <- basis + geom_point(aes(colour = rgb_spec(wt, mpg)))
  g <- get_guide(g)
  expect_identical(g$layout$name, c("background", "colours", "frame",
                                    "x_ticks", "y_ticks", "x_labels",
                                    "y_labels", "x_title", "y_title"))

  # Test bar choice
  g <- basis + geom_point(aes(colour = rgb_spec(wt)))
  g <- get_guide(g)
  expect_identical(g$layout$name, c("background", "bar", "label", "title", "ticks"))
})
