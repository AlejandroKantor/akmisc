context("Character Occurences")

test_that("Correct count", {
  expect_equal(countCharOccurrences("a", c("a other a", "zero")), c(2L, 0L))
  expect_equal(countCharOccurrences("a", c("A sentence.")),0)
  expect_equal(countCharOccurrences("a", c("A sentence."), b_ignore_case = T),1)
  expect_equal(countCharOccurrences("a", c("")),0)


})
