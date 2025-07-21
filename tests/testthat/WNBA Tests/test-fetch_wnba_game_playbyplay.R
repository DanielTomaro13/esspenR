test_that("fetch_wnba_game_playbyplay works as expected", {
  skip_on_cran()
  result <- suppressWarnings(fetch_wnba_game_playbyplay())
  expect_true(is.data.frame(result) || is.list(result))
  expect_false(is.null(result))
})
