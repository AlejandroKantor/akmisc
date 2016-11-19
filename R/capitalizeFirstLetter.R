#' Capitalizes the first letter of each string in a character vector
#'
#' @param v_s_string a character vector
#' @return v_s_string with first letter capitalized of each string
#' @examples
#' capitalizeFirstLetter("this sentence should have the first letter capitalized.")
#' capitalizeFirstLetter(c("sentence one.", "sentence two"))
#' capitalizeFirstLetter("NO EFFECT")
#' capitalizeFirstLetter("a")
capitalizeFirstLetter <- function (v_s_string) {
  v_s_string <- paste0(toupper(substring(v_s_string, 1,1)),
                       substring(v_s_string, 2))
  return(v_s_string)
}
capitalizeFirstLetter("a")
