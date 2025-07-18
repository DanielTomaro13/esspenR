test_that("fetch_general_search returns search results", {
  res <- fetch_general_search(query = "warriors")
  expect_s3_class(res, "data.frame")
})
