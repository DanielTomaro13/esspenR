test_that("fetch_nfl_specific_team works as expected", {
  skip_on_cran()
  result <- suppressWarnings(fetch_nfl_specific_team())
  expect_true(is.data.frame(result) || is.list(result))
  expect_false(is.null(result))
})
