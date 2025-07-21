test_that("fetch_college_baseball_scoreboard works as expected", {
  skip_on_cran()
  result <- suppressWarnings(fetch_college_baseball_scoreboard())
  expect_true(is.data.frame(result) || is.list(result))
  expect_false(is.null(result))
})
