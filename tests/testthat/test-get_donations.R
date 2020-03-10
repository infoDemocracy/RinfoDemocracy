context("get_donations")

test_that("get_donations works correctly", {
  ec <- get_donations()
  
  expect_true(is.data.frame(ec))
  expect_true(nrow(ec) > 0)
  
})
