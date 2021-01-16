test_that("chromatic scale can be trained", {
  sc <- scale_colour_rgb()
  df <- new_data_frame(list(colour = rgb_spec(c(0, 1), c(-1, 2), c(-2, 3))))
  sc$train_df(df)
  expect_equal(sc$get_limits(), df$colour)

  df <- new_data_frame(list(colour = rgb_spec(c(-1, 0), c(-2, 1), c(-3, 2))))
  sc$train_df(df)
  expect_equal(sc$get_limits(),
               rgb_spec(c(-1, 1), c(-2, 2), c(-3, 3)))

  fun <- function(x) x + 1
  sc <- scale_colour_rgb(limits = fun)
  sc$train_df(df)
  expect_equal(sc$get_limits(),
               rgb_spec(c(0, 1), c(-1, 2), c(-2, 3)))
})

test_that("chromatic scale can train hybrid colour specs", {
  sc <- scale_colour_rgb()
  col <- rgb_spec(r = c("A", "B", "C"),
                  g = c(1:3),
                  b = c(5:7))
  sc$train(col)
  lim <- sc$get_limits()
  expect_equal(sc$get_limits(),
               rgb_spec(c("A", "B", "C"),
                        c(1L, 3L, NA),
                        c(5L, 7L, NA)))
})
