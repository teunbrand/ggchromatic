test_that("vctrs colourspec behave as expected", {
  x <- cmy_spec(c("A", "B"), c(0, 1), c(0, 1))
  y <- hcl_spec(c(0, 1), c(0, 1), c(0, 1))

  expr <- substitute(vec_ptype2(x, y))
  expect_error(eval(expr), "Can't combine")

  expect_s3_class(vec_ptype2(x, 2), "cmy_spec")
  expect_identical(vec_ptype_abbr(x), "cmy")
  expect_identical(
    format(x),
    c("[A,0,0]", "[B,1,1]")
  )

  expect_equal(
    as.matrix(x),
    matrix(c(0,1,0,1,0,1), 2, dimnames = list(NULL, c("c", "m", "y")))
  )

  test <- c("rgb", "hsv", "hsl", "cmyk", "cmy", "hcl")
  cnstr <- lapply(test, function(x){spectrum_constructor(x)()})
  v <- mapply(function(t, cntr) {
    expect_s3_class(cntr, paste0(t, "_spec"))
  }, t = test, cntr = cnstr)

  expect_null(spectrum_name(1))
})

test_that("vctrs placeholders behave as they should", {
  x <- void_channel(1:10)
  expect_length(x, 10)
  expect_s3_class(x, "void_channel")
  expect_equal(format(x), "")

  expect_length(c(x, x), 20)
  expect_type(c(x, "A"), type = "character")
  expect_type(c("A", x), type = "character")
  expect_type(c(TRUE, x), type = "logical")

  x <- new_vexpression(1:5)
  expect_length(x, 5)
  expect_s3_class(x, "vexpression")

  expect_length(c(x, x), 10)
  expect_s3_class(c(x, "A"), "vexpression")
  expect_s3_class(vec_c("A", x), "vexpression")

  expect_type(as.expression(x), "expression")
})
