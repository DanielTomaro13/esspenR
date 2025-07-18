test_that("fetch_franchises returns franchise list", {
  res <- fetch_franchises("football", "nfl")
  expect_s3_class(res, "data.frame")
  expect_true("id" %in% names(res) || "name" %in% names(res))
})
