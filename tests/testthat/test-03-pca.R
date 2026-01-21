test_that("PCA function exists and is a function", {
  expect_true(exists("PCA"))
  expect_type(PCA, "closure")
})
