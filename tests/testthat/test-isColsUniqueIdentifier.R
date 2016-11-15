context("Unique Identifier")

test_that("identifiesUniqueIdentifiers", {

  dt_data <- data.table(var1 = c(1,1,2,2) ,
                        var2 = c("a", "b" , "a","b"),
                        var3 = 1:4 )
  dt_data[ , var4:=factor(var2) ]

  expect_false( isColsUniqueIdentifier(dt_data, v_s_cols = "var1"))

  expect_true(isColsUniqueIdentifier(dt_data, v_s_cols = c("var1", "var2")))

  expect_true(isColsUniqueIdentifier(dt_data, v_s_cols = "var3"))

  expect_true(isColsUniqueIdentifier(dt_data, v_s_cols = c("var1", "var4")))

  expect_error( isColsUniqueIdentifier(dt_data, v_s_cols = "var5"))

  v_s_cols = "var1"
  expect_error( isColsUniqueIdentifier(dt_data))
})
