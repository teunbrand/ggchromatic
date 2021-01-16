test_that("scale_transforms work on hybrid scales", {
  sc <- scale_colour_rgb(trans = log10_trans())

  # Test continuous colour_spec
  df <- new_data_frame(list(colour = rgb_spec(c(1, 10), c(1, 1000), c(0.1, 1))))
  test <- sc$transform_df(df)
  expect_equal(test$colour, rgb_spec(c(0, 1), c(0, 3), c(-1, 0)))

  # Test hybrid colour spec
  df <- new_data_frame(list(colour = rgb_spec(c("A", "B"), c(1, 1000),
                                              c(1, 100))))
  test <- sc$transform_df(df)
  expect_equal(test$colour, rgb_spec(c("A", "B"), c(0, 3), c(0, 2)))

  # Test plain continuous
  df <- new_data_frame(list(colour = c(0.1, 1, 10)))
  test <- sc$transform_df(df)
  expect_equal(test$colour, c(-1, 0, 1))

  # Test plain discrete
  df <- new_data_frame(list(colour = c("A", "B", "C")))
  test <- sc$transform_df(df)
  expect_equal(test$colour, c("A", "B", "C"))
})

test_that("scale_transforms merge channel aesthetics", {
  sc <- scale_colour_rgb()

  df <- new_data_frame(list(
    red = c(0, 1), green = c("A", "B"), blue = c(0, 1)
  ))
  test <- sc$transform_df(df)
  expect_equal(test$colour, rgb_spec(c(0, 1), c("A", "B"), c(0, 1)))
})
