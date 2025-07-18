test_that("fetch_venues returns venues data", {
  res <- fetch_venues("football", "nfl")
  expect_s3_class(res, "data.frame")
})
