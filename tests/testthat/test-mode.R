context("Mode")

test_that("propCasesAreMode", {

  v_s_letters <- c("a","a","b","c","c","c")
  expect_equal(propCasesAreMode(1:10), 1/10)
  expect_equal(propCasesAreMode(v_s_letters), 0.5)
  expect_equal(propCasesAreMode(factor(v_s_letters)), 0.5)
  expect_equal(propCasesAreMode(numeric()), 0 )
  expect_equal(propCasesAreMode(1), 1 )


})

test_that("modeOfVector", {

  v_s_letters <- c("a","a","b","c","c","c")
  expect_equal(length(modeOfVector(1:10,b_warning_if_several_modes = F) ) , 1)
  expect_true(modeOfVector(1:10,b_warning_if_several_modes = F) %in% 1:10)
  expect_equal(modeOfVector(v_s_letters), "c")
  expect_true(modeOfVector(factor(v_s_letters))== "c")
  expect_error(modeOfVector(numeric()) )
  expect_warning(modeOfVector(1:10) )
  expect_error(modeOfVector(1:10,b_allow_several_modes = F) )


})
