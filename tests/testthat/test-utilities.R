
test_that("ns works", {
  expect_equal(ns(list(orange = 3, apple = 1, banana = 2)), c("apple", "banana", "orange"))
})
