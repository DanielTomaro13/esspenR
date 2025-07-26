test_that("fetch_soccer_transfers works as expected", {
  skip_on_cran()
  result <- suppressWarnings(fetch_soccer_transfers())
  expect_true(is.data.frame(result) || is.list(result))
  expect_false(is.null(result))
})
