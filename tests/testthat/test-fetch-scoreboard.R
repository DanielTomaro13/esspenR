test_that("fetch_scoreboard returns games", {
  res <- fetch_scoreboard("football", "nfl")
  expect_s3_class(res, "data.frame")
})
