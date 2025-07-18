test_that("fetch_seasons returns seasons list", {
  res <- fetch_seasons("football", "nfl")
  expect_s3_class(res, "data.frame")
  expect_true(any(grepl("20", paste(res, collapse=" ")), na.rm=TRUE)) # crude check for years
})
