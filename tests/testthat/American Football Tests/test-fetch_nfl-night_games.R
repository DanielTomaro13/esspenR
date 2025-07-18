test_that("fetch_nfl-night_games works as expected", {
  skip_on_cran()
  result <- suppressWarnings(fetch_nfl-night_games())
  expect_true(is.data.frame(result) || is.list(result))
  expect_false(is.null(result))
})
