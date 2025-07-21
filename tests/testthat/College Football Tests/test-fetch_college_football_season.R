test_that("fetch_college_football_season works as expected", {
  skip_on_cran()
  result <- suppressWarnings(fetch_college_football_season())
  expect_true(is.data.frame(result) || is.list(result))
  expect_false(is.null(result))
})
