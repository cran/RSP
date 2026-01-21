test_that("FA function exists and is a function", {
  expect_true(exists("FA"))
  expect_type(FA, "closure")
})
