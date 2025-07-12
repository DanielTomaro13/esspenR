test_that("fetch_all_sports_leagues returns a data frame", {
  res <- fetch_all_sports_leagues()
  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) > 0)
})
