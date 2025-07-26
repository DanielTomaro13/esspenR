test_that("fetch_soccer_teams works as expected", {
  skip_on_cran()
  result <- suppressWarnings(fetch_soccer_teams())
  expect_true(is.data.frame(result) || is.list(result))
  expect_false(is.null(result))
})
