#' Counts the number of times a single characte occurs
#'
#' @description Counts the number of times a single characte occurs in each string of a character vector.
#' @param char a single character cannot be a regular expression.
#' @param v_s_string a character vector.
#' @param b_ignore_case a boolean default FALSE. If TRUE case is ignored.
#' @return a integer vector of the number of times the character occurs in each string in v_s_string.
#'
#' @examples
#' countCharOccurrences("a", c("a other a", "zero"))
#' countCharOccurrences("a", c("A sentence."))
#' countCharOccurrences("a", c("A sentence."), b_ignore_case = TRUE)
#'
countCharOccurrences <- function(char, v_s_string, b_ignore_case = FALSE) {
  v_s_new_string <- gsub(char,"",v_s_string,ignore.case = b_ignore_case)
  return (nchar(v_s_string) - nchar(v_s_new_string))
}



