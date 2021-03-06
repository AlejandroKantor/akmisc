context("Categorization")

test_that("differentTypesOfValues", {
  ci_intervals <- CategorizationIntervals(value = c(1,2,3),
                                          min = c(-Inf, 0, 0),
                                          max = c(0 , 0, Inf),
                                          close_left = c(0,1,0),
                                          close_right = c(0,1,0) )
  v_n_values <- -3:3
  v_expected <- c(1,1,1,2,3,3,3)
  expect_equal(categorizeByIntervals(v_n_values, ci_intervals),v_expected)

  ci_intervals[, value := c("a","b","c")]
  v_expected <- c("a","a","a","b","c","c","c")
  expect_equal(categorizeByIntervals(v_n_values, ci_intervals),v_expected)

  ci_intervals[, value := factor(value)]
  v_expected <- factor(c("a","a","a","b","c","c","c"))
  expect_equal(categorizeByIntervals(v_n_values, ci_intervals),v_expected)

})



test_that("differentTypesOfValuesLabels", {
  ci_intervals <- CategorizationIntervals(value = c(1,2,3),
                                          v_s_intervals = c("(-Inf,0)","[0,0]","(0,Inf)"))
  v_n_values <- -3:3
  v_expected <- c(1,1,1,2,3,3,3)
  expect_equal(categorizeByIntervals(v_n_values, ci_intervals),v_expected)

  ci_intervals[, value := c("a","b","c")]
  v_expected <- c("a","a","a","b","c","c","c")
  expect_equal(categorizeByIntervals(v_n_values, ci_intervals),v_expected)

  ci_intervals[, value := factor(value)]
  v_expected <- factor(c("a","a","a","b","c","c","c"))
  expect_equal(categorizeByIntervals(v_n_values, ci_intervals),v_expected)

})

test_that("differentTypesOfValuesLabelsDifferentSpaces", {
  ci_intervals <- CategorizationIntervals(value = c(1,2,3),
                                          v_s_intervals = c("( -Inf, 0)","[0,0.1111)","[  0.1111, Inf)"))
  v_n_values <- c(-2,0,0, 0.1, 0.112,100)
  v_expected <- c(1 ,2 ,2 , 2, 3,3)
  expect_equal(categorizeByIntervals(v_n_values, ci_intervals),v_expected)
})

test_that("wierdIntervals", {
  ci_intervals <- CategorizationIntervals(value = c(-99,1,2,-99),
                                          min = c(-Inf, 0, 0, 10/9),
                                          max = c(0,    0, 10/9, Inf),
                                          close_left = c(0, 1 , 0, 0),
                                          close_right = c(0,1,1, 0) )
  v_n_values <- -3:3
  v_expected <- c(-99,-99,-99,1,2,-99,-99)
  expect_equal(ci_intervals[value==2 , label] , "(0, 1.111]")
  expect_equal(categorizeByIntervals(v_n_values, ci_intervals),v_expected)


})
