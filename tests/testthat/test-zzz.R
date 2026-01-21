test_that("RSP package loads correctly", {
  expect_true("RSP" %in% loadedNamespaces())
})

test_that("All exported functions are available", {
  exported_fns <- c("CFA", "FA", "INTERNAL", "IRT", "ITEMAN", "PCA", "SIMDATA")
  for (fn in exported_fns) {
    expect_true(exists(fn), info = paste(fn, "should exist"))
  }
})
