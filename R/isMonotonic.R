#' Indicates if a sequence of values are monotonic
#'
#' @description Indicates if a sequence of values are (non strictly) montonic increasing, decreasing or either.
#' @param v_n_values numeric vector. Length of \code{v_n_values} must be at least 1.
#' @param s_direction a string of the monotonic direction we are interested. Must be 'decreasing' 'increasing' or 'either'.
#' @return a boolean (logic vector length one) indicating if the sequence of values is monotonic. If v_n_values has a \code{NA} value the result is \code{NA}.
#' @examples
#' isMonotonic(c(1,1,2,3))
#' isMonotonic(c(1,1,NA, 2,3))
#' isMonotonic(1:5, s_direction = "increasing")
#' isMonotonic(1:5, s_direction = "decreasing")
#' isMonotonic(5:1, s_direction = "decreasing")
#'


isMonotonic <- function(v_n_values, s_direction = "either" ){
  v_s_direction_options <- c("decreasing" ,"increasing" , "either")
  if( !s_direction %in%  v_s_direction_options){
    stop( "s_direction must be 'decreasing' 'increasing' or 'either'")

  } else if( any(is.na(v_n_values))){
    b_condition <- NA

  } else if( tolower(s_direction)=="increasing" ){
    b_condition <- all(v_n_values == cummax(v_n_values))

  } else if( tolower(s_direction)=="decreasing" ){
    b_condition <- all(v_n_values == cummin(v_n_values))

  } else {
    b_condition <- all(v_n_values == cummax(v_n_values)) | all(v_n_values == cummin(v_n_values))

  }

  return( b_condition)
}

