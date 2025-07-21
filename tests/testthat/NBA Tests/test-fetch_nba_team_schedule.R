test_that("fetch_nba_team_schedule works as expected", {
  skip_on_cran()
  result <- suppressWarnings(fetch_nba_team_schedule())
  expect_true(is.data.frame(result) || is.list(result))
  expect_false(is.null(result))
})
