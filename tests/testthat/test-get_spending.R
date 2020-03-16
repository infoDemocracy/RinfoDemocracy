context("get_spending")

test_that("get_spending works correctly", {
  ec <- get_spending()
  
  expect_true(is.data.frame(ec))
  expect_true(nrow(ec) > 0)
  
})
