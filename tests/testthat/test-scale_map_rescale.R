test_that("rescale works on hybrid data", {
  sc <- scale_colour_rgb()

  x <- rgb_spec(c(1:3), LETTERS[1:3], b = 3:1)
  sc$train(x)

  expect_equal(
    sc$rescale(x),
    rgb_spec(c(0, 0.5, 1), c(0, 0.5, 1), c(1, 0.5, 0))
  )
})

test_that("map works on hybrid data", {
  sc <- scale_colour_rgb()

  x <- rgb_spec(c(1:3), LETTERS[1:3], b = 3:1)
  sc$train(x)

  expect_equal(
    sc$map(x),
    c("#0000FF", "#808080", "#FFFF00")
  )
})

