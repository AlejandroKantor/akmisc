context("Is monotonic")

test_that("Correct calculation", {
  expect_true(isMonotonic(c(1,1,2,3)))
  expect_true( is.na(isMonotonic(c(1,1,NA, 2,3))))
  expect_true(isMonotonic(c(0.2,1,1:5), s_direction = "increasing"))
  expect_false(isMonotonic(1:5, s_direction = "decreasing"))
  expect_true(isMonotonic(5:1, s_direction = "decreasing"))
  expect_error(isMonotonic(5:1, s_direction = "other"))

})
