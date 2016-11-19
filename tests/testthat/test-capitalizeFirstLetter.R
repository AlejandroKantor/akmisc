context("Capitalize first letter")

test_that("Correct capitalization", {
 expect_equal(capitalizeFirstLetter("this sentence should have the first letter capitalized."),
              "This sentence should have the first letter capitalized.")
  expect_equal(capitalizeFirstLetter(c("sentence one.", "sentence two!")),
                        c("Sentence one.", "Sentence two!"))
  expect_equal(capitalizeFirstLetter("NO EFFECT"), "NO EFFECT")
  expect_equal(capitalizeFirstLetter(c("a", "ab")), c("A","Ab"))


})

test_that("punctuation", {
  expect_equal(capitalizeFirstLetter("!a/%$"),
               "!a/%$")
  expect_equal(capitalizeFirstLetter(c("sentence !$3@#$ one.", "sentence two!")),
               c("Sentence !$3@#$ one.", "Sentence two!"))

  expect_equal(capitalizeFirstLetter(c(" a", "  ab")), c(" a","  ab"))


})
