test_that("CFA function exists and is a function", {
  expect_true(exists("CFA"))
  expect_type(CFA, "closure")
})
