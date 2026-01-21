test_that("INTERNAL function exists and is a function", {
  expect_true(exists("INTERNAL"))
  expect_type(INTERNAL, "closure")
})
