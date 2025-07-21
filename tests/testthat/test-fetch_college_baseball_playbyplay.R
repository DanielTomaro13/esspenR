test_that("fetch_college_baseball_playbyplay works as expected", {
  skip_on_cran()
  result <- suppressWarnings(fetch_college_baseball_playbyplay())
  expect_true(is.data.frame(result) || is.list(result))
  expect_false(is.null(result))
})
