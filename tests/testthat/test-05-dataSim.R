test_that("SIMDATA function exists and is a function", {
  expect_true(exists("SIMDATA"))
  expect_type(SIMDATA, "closure")
})
