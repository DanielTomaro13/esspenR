test_that("fetch_mlb_team_detailed works as expected", {
  skip_on_cran()
  result <- suppressWarnings(fetch_mlb_team_detailed())
  expect_true(is.data.frame(result) || is.list(result))
  expect_false(is.null(result))
})
