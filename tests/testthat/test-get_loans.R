context("get_loans")

test_that("get_loans works correctly", {
  ec <- get_loans()
  
  expect_true(is.data.frame(ec))
  expect_true(nrow(ec) > 0)
  
})
