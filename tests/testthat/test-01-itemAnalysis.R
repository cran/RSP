test_that("ITEMAN function exists and is a function", {
  expect_true(exists("ITEMAN"))
  expect_type(ITEMAN, "closure")
})
