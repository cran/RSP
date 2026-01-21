test_that("IRT function exists and is a function", {
  expect_true(exists("IRT"))
  expect_type(IRT, "closure")
})
