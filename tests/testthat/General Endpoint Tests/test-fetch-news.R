test_that("fetch_news returns articles", {
  res <- fetch_news("football", "nfl")
  expect_s3_class(res, "data.frame")
  expect_true(any(grepl("http", paste(res, collapse=" ")), na.rm=TRUE))
})
