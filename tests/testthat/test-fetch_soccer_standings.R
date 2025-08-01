test_that("fetch_soccer_standings works as expected", {
  skip_on_cran()
  result <- suppressWarnings(fetch_soccer_standings())
  expect_true(is.data.frame(result) || is.list(result))
  expect_false(is.null(result))
})
