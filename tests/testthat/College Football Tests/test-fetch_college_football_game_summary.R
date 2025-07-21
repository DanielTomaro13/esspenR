test_that("fetch_college_football_game_summary works as expected", {
  skip_on_cran()
  result <- suppressWarnings(fetch_college_football_game_summary())
  expect_true(is.data.frame(result) || is.list(result))
  expect_false(is.null(result))
})
