context("Merge with col prioritization")

test_that( "dt_prior has more unique individuals" , {
  i_num <- 3
  dt_prior <- data.table( id = 1:i_num,
                          var1 = letters[1:i_num],
                          var2 = 111)
  dt_other <- data.table( id = 1:(i_num+2),
                          var1 = paste0(letters[10 + 1:(i_num+2)],letters[10 + 1:(i_num+2)]) ,
                          var3 = -999)
  v_s_keys <- "id"
  dt_expected <- data.table( id = 1:(i_num+2),
                             var1 =  c("a", "b","c", "nn","oo"),
                             var3 = -999,
                             var2 = c(111L,111L,111L, NA_integer_, NA_integer_))
  dt_merged <- mergeWithColPrioritization(dt_prior, dt_other,v_s_keys )
  expect_equal(ncol(dt_merged) , 4)
  expect_equal(nrow(dt_merged), 5)
  expect_equal(dt_merged, dt_expected)

})


test_that( "dt_prior has less unique individuals" , {
  i_num <- 3
  dt_prior <- data.table( id = 1:i_num,
                          id2 = 2,
                          var1 = letters[1:i_num],
                          var2 = 111)
  dt_other <- data.table( id = 1:(i_num-2),
                          id2= 2,
                          var1 = paste0(letters[10 + 1:(i_num-2)],letters[10 + 1:(i_num-2)]) ,
                          var3 = -999)
  v_s_keys <- c("id", "id2")
  dt_expected <- data.table( id = 1:(i_num),
                             id2 = 2,
                             var1 =  c("a", "b","c"),
                             var3 =c( -999L, NA_integer_, NA_integer_),
                             var2 = c(111L,111L,111L))
  dt_merged <- mergeWithColPrioritization(dt_prior, dt_other,v_s_keys )
  expect_equal(ncol(dt_merged) , 5)
  expect_equal(nrow(dt_merged), 3)
  expect_equal(dt_merged, dt_expected)

})


test_that( "dt_prior has different Ids" , {
  i_num <- 3
  dt_prior <- data.table( id = 1:i_num,
                          id2 = 2,
                          var1 = letters[1:i_num],
                          var2 = 111)
  dt_other <- data.table( id = 99,
                          id2= 2,
                          var1 ="kk" ,
                          var3 = -999)
  v_s_keys <- c("id", "id2")
  dt_expected <- data.table( id = c(1:(i_num),99),
                             id2 = 2,
                             var1 =  c("a", "b","c","kk"),
                             var3 =c(  NA_integer_, NA_integer_, NA_integer_ ,-999L),
                             var2 = c(111L,111L,111L, NA_integer_))
  dt_merged <- mergeWithColPrioritization(dt_prior, dt_other,v_s_keys )
  expect_equal(ncol(dt_merged) , 5)
  expect_equal(nrow(dt_merged), 4)
  expect_equal(dt_merged, dt_expected)

})
