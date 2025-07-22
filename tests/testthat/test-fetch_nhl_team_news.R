test_that("fetch_nhl_team_news works as expected", {
  skip_on_cran()
  result <- suppressWarnings(fetch_nhl_team_news())
  expect_true(is.data.frame(result) || is.list(result))
  expect_false(is.null(result))
})
