#' Gives the number of unique values of a vector
#'
#' @param v_values a vector.
#' @return An integer of the number of unique que values in v_values. If v_value has length zero returns 0.
#' @examples
#' numUniqueValues(1:10)
#' numUniqueValues(c("a","a","b","c","c","c"))
numUniqueValues <- function(v_values) {
  if(length( v_values ) == 0){
    i_unique <- 0L
  } else {
    dt_mode <- data.table(value= v_values)
    i_unique <- dt_mode[ , length(unique(value))]
  }
  return(i_unique)
}
