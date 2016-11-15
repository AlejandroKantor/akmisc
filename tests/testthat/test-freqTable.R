context("Frequency table")

test_that("frequencyTable", {
  dt_data <- data.table(var1 = c(1,1,2,2,3,3,3,3),
                        var2 = c(6,6,6,7,7,7,7,7))


  dt_freq1 <- freqTable(dt_data, "var1", b_total_row = T)
  dt_freq2 <- freqTable(dt_data, c("var1","var2"), b_include_perc = T)

  dt_expected1 <- data.table(var1 = c("1", "2", "3", "Total"),
                             freq = c(2L,2L,4L,8L))
  expect_identical(dt_freq1 , dt_expected1)
  expect_equal(ncol(dt_freq2), 4)
  expect_equal(dt_freq2[ var1 ==3 & var2 == 7 , perc], 50.0)


})
