# Choose this as data because it has continuous, discrete and absent channels
dat <- rgb_spec(c(0, 1, 10), c("A", "B", "C"))

# Breaks ------------------------------------------------------------------

test_that("scale breaks can be calculated", {

  sc <- scale_colour_rgb()
  sc$train(dat)

  expect_equal(sc$get_breaks(),
               rgb_spec(seq(0, 10, by = 2.5),
                        c("A", "B", "C", NA, NA),
                        new_void_channel(5)))
})

test_that("scale_breaks can be a function", {

  sc <- scale_colour_rgb(
    breaks = function(x) {
      if (is_discrete(x)) rev(x) else seq(x[1], x[2], length.out = 3)
    }
  )
  sc$train(dat)

  expect_equal(sc$get_breaks(),
               rgb_spec(c(0, 5, 10),
                        c("C", "B", "A"),
                        new_void_channel(3)))
})

test_that("scale_breaks can be NULL", {

  sc <- scale_colour_rgb(breaks = NULL)
  sc$train(dat)

  expect_null(sc$get_breaks())
})

test_that("scale_breaks can be a list", {

  sc <- scale_colour_rgb(breaks = list(
    r = function(x) seq(x[1], x[2], length.out = 3),
    g = c("B", "C", "A"),
    b = "I should not matter"
  ))

  sc$train(dat)

  expect_equal(
    sc$get_breaks(),
    rgb_spec(c(0, 5, 10), c("B", "C", "A"))
  )

})


# Labels ------------------------------------------------------------------

test_that("scale labels can be calculated", {

  sc <- scale_colour_rgb()
  sc$train(dat)

  expect_equal(sc$get_labels(),
               rgb_spec(c("0.0", "2.5", "5.0", "7.5", "10.0"),
                        c("A", "B", "C", NA, NA),
                        new_void_channel(5)))

})

test_that("scale labels can be a function", {

  sc <- scale_colour_rgb(
    labels = function(x) {paste0(x, "&", x)}
  )
  sc$train(dat)

  expect_equal(sc$get_labels(),
               rgb_spec(c("0&0", "2.5&2.5", "5&5", "7.5&7.5", "10&10"),
                        c("A&A", "B&B", "C&C", NA, NA),
                        new_void_channel(5)))

})

test_that("scale labels can be NULL", {

  sc <- scale_colour_rgb(labels = NULL)
  sc$train(dat)

  expect_null(sc$get_labels())
})

test_that("scale labels can be a list", {

  sc <- scale_colour_rgb(labels = list(
    r = sqrt,
    g = c("AA", "BB", "CC"),
    b = "I should not matter"
  ))

  sc$train(dat)

  expect_equal(
    sc$get_labels(),
    rgb_spec(sqrt(c(0, 2.5, 5, 7.5, 10)),
             c("AA", "BB", "CC", NA, NA),
             new_void_channel(5))
  )

})

test_that("scale labels can handle expressions", {

  sc <- scale_colour_rgb(labels = list(r = math_format()))
  sc$train(dat)

  expect_equal(
    sc$get_labels(),
    rgb_spec(new_vexpression(expression(10^0, 10^2.5, 10^5, 10^7.5, 10^10)),
             c("A", "B", "C", NA, NA),
             new_void_channel(5))
  )
})
