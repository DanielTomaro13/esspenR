test_that("fetch_calender returns expected structure", {
  res <- fetch_calender("football", "nfl")
  expect_s3_class(res, "data.frame")
  expect_true("date" %in% names(res) || "startDate" %in% names(res))
})
