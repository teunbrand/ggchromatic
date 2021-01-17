test_that("scale breaks can be calculated", {
  # Last is void channel
  x <- rgb_spec(c(0, 1, 10), c("A", "B", "C"))

  sc <- scale_colour_rgb()
  sc$train(x)

  expect_equal(sc$get_breaks(),
               rgb_spec(seq(0, 10, by = 2.5),
                        c("A", "B", "C", NA, NA),
                        new_void_channel(5)))
})

test_that("scale_breaks can be a function", {

  x <- rgb_spec(c(0, 1, 10), c("A", "B", "C"))

  sc <- scale_colour_rgb(
    breaks = function(x) {
      if (is_discrete(x)) rev(x) else seq(x[1], x[2], length.out = 3)
    }
  )

  sc$train(x)

  expect_equal(sc$get_breaks(),
               rgb_spec(c(0, 5, 10),
                        c("C", "B", "A"),
                        new_void_channel(3)))
})

test_that("scale_breaks can be NULL", {

  x <- rgb_spec(c(0, 1, 10), c("A", "B", "C"))

  sc <- scale_colour_rgb(breaks = NULL)
  sc$train(x)

  expect_null(sc$get_breaks())
})

test_that("scale_breaks can be a list", {

  x <- rgb_spec(c(0, 1, 10), c("A", "B", "C"))

  sc <- scale_colour_rgb(breaks = list(
    r = function(x) seq(x[1], x[2], length.out = 3),
    g = c("B", "C", "A"),
    b = "I should not matter"
  ))

  sc$train(x)
  sc$get_breaks()

  expect_equal(
    sc$get_breaks(),
    rgb_spec(c(0, 5, 10), c("B", "C", "A"))
  )

})
