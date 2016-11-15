context("Count unique values")

test_that("numberOfUniqueValues", {
  v_s_let <- c("a","a","b","c","c","c")

  expect_equal( numUniqueValues(1:10), 10L)

  expect_equal( numUniqueValues(v_s_let), 3L)

  expect_equal( numUniqueValues(factor(v_s_let)), 3L)
  expect_equal( numUniqueValues(numeric()), 0L)

})
