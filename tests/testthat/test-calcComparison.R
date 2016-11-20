context("Comparison of vectors")

test_that("Correct comparison", {
  v_n_1 <- rep(0,3)
  v_n_2 <- c(0, -1.1, 1.1)
  expect_equal(calcComparison( v_n_1, v_n_2, "=="), v_n_1 == v_n_2)
  expect_equal(calcComparison( v_n_1, v_n_2, ">="), v_n_1 >= v_n_2)
  expect_equal(calcComparison( v_n_1, v_n_2, "<="), v_n_1 <= v_n_2)
  expect_equal(calcComparison( v_n_1, v_n_2, "!="), v_n_1 != v_n_2)
  expect_equal(calcComparison( v_n_1, v_n_2, ">"), v_n_1 > v_n_2)
  expect_equal(calcComparison( v_n_1, v_n_2, "<"), v_n_1 < v_n_2)
  expect_error(calcComparison( v_n_1, v_n_2, "other"))

})
