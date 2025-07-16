test_that("fetch_detailed_search returns search results", {
  res <- fetch_detailed_search(query = "lebron")
  expect_s3_class(res, "data.frame")
  expect_true(any(grepl("lebron", tolower(paste(res, collapse=" "))), na.rm=TRUE))
})
