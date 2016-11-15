context("Merging data.table list")

test_that("correctMerger", {
  dt_1 <- data.table(let = letters[1:3], num = 1:3)
  dt_2 <- data.table(let= letters[2:4], num = 1:3)
  dt_3 <- data.table(let = "b")
  dt_4 <- data.table(let = "b", other_col = 3.2)
  l_len_1 <- list(dt_1)
  l_len_2 <- list(dt_1, dt_2)
  l_len_3 <- list(dt_1, dt_2, dt_3)
  l_len_3_alt <- list(dt_1, dt_2, dt_4)

  dt_expect <- data.table(let = c("a","b","c","d"),
                          num.x = c(1L,2L,3L,NA_integer_),
                          num.y=c(NA_integer_,1L,2L,3L))

  dt_expect_alt <- data.table(let = c("b","c"), num.x = c(2,3), num.y=c(1,2))
  dt_expect_3 <- data.table(let = "b", num.x = c(2), num.y=c(1),other_col = 3.2 )
  setkey(dt_expect, "let")
  setkey(dt_expect_alt, "let")
  setkey(dt_expect_3, "let")

  expect_identical(mergeList(l_len_1, v_s_keys= "let"), dt_1)

  expect_equal(mergeList(l_len_2, v_s_keys= "let"), dt_expect)
  expect_equal(mergeList(l_len_2, v_s_keys= "let", all=F), dt_expect_alt)

  expect_equal(mergeList(l_len_3, v_s_keys= "let"), dt_expect)
  expect_equal(mergeList(l_len_3_alt, v_s_keys= "let", all=F), dt_expect_3)

})
