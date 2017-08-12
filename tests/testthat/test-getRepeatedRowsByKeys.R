context("RepeatedRows")

test_that("getRepeatedRowsByKeys", {

  dt_data <- data.table(var1 = c(1,1,2,2,3) ,
                        var2 = c("a", "b" , "a","b","c"),
                        var3 = 1:5,
                        rep_count_ = 1)
  dt_data[ , var4:=factor(var2) ]

  expect_warning(getRepeatedRowsByKeys(dt_data, "var3"))
  expect_null(suppressWarnings(getRepeatedRowsByKeys(dt_data, "var3")))
  expect_null(suppressWarnings(getRepeatedRowsByKeys(dt_data, c("var1","var2"))))

  dt_restult <- data.table( var1 = c(1,1,2,2),
                            var2 = c("a", "b" , "a","b"),
                            var3 = 1:4,
                            rep_count_ = 2L)
  expect_equal(getRepeatedRowsByKeys(dt_data[ , .(var1, var2,var3)], "var1"),dt_restult)


})
