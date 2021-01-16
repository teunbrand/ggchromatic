test_that("get_limits can deduce limits from range", {
  sc <- scale_colour_rgb()

  # Untrained
  expect_equal(
    sc$get_limits(),
    rgb_spec(c(0, 1), c(0, 1), c(0, 1))
  )

  # Deduced from range
  x <- rgb_spec(c(0, 0.5, 1), c("A", "A", "B"), c(2, 3, 4))
  sc$train(x)
  expect_equal(
    sc$get_limits(),
    x[c(1, 3)]
  )
})

test_that("get_limits can handle functions", {
  sc <- scale_colour_rgb(limits = rev)

  x <- rgb_spec(c(0, 0.5, 1), c("A", "A", "B"), c(2, 3, 4))
  sc$train(x)
  expect_equal(
    sc$get_limits(),
    x[c(3, 1)]
  )
})

test_that("get_limits can handle lists", {
  sc <- scale_colour_rgb(
    limits = list(r = function(x) x * 4, g = tolower, b = NULL)
  )

  x <- rgb_spec(c(0, 1), g = c("X", "Y"), b = c("A", "B"))
  sc$train(x)
  expect_equal(
    sc$get_limits(),
    rgb_spec(c(0, 4), c("x", "y"), c("A", "B"))
  )
})


