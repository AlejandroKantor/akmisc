
#' Gives the proportion of cases which are the mode
#'
#' @param v_values a vector.
#' @return A numeric of the proportion of cases which are the mode. If length(v_values) == 0 returns 0.
#' @examples
#' propCasesAreMode(1:10)
#' propCasesAreMode(c("a","a","b","c","c","c"))
#' propCasesAreMode(numeric())

propCasesAreMode <- function(v_values) {
  if(length(v_values) == 0) {
    n_max <- 0.0
  } else {
    i_len <- length(v_values)
    dt_mode <- data.table(value= v_values)
    dt_mode <- dt_mode[ ,.(prop=.N/i_len), value]
    n_max <- dt_mode[ , max(prop)]
  }
  return(n_max)
}





#' Gives the mode of a vector
#'
#' @param v_values a vector.
#' @param b_allow_several_modes logical default TRUE. If we want to allow more than one mode.
#' @param b_warning_if_several_modes logical default TRUE. If we want a warning when there is more than one mode.
#' @return A vector of length 1 with the mode of v_values. If there are more than one mode only one of the modes will be returned.
#' @examples
#' modeOfVector(1:10)
#' modeOfVector(c("a","a","b","c","c","c"))
modeOfVector <- function(v_values,
                    b_allow_several_modes = TRUE,
                    b_warning_if_several_modes = TRUE) {
  if(length(v_values ) == 0){
    stop("v_values must have at least one value")
  }

  dt_mode <- data.table(value= v_values)
  dt_mode <- dt_mode[ ,.(freq=.N), value]
  #order frequency of each value in decending order
  setorder(dt_mode, -freq)
  k_mode <- dt_mode[ 1 , value]

  if( b_allow_several_modes == FALSE  | b_warning_if_several_modes ==TRUE ){
    # see if there are ties
    i_max_freq <- dt_mode[ 1 , freq]
    i_num_with_max_freq <- dt_mode[ freq == i_max_freq, length(value) ]
    if(i_num_with_max_freq > 1){
      if(b_allow_several_modes == FALSE){
        stop("More than one mode")
      } else {
        warning("More than one mode")
      }

    }
  }

  return(k_mode)
}
