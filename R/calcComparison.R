#' Compares two numeric vectors by passing comparison in a string
#'
#' @param v_n_1 a numeric vector. Length must be the same as the length of \code{v_n_2} or length 1.
#' @param v_n_2 a numeric vector. Length must be the same as the length of \code{v_n_1} or length 1.
#' @param s_comp type of comparison must be one of the following \code{"==","!=", "<=", ">=", "<" ,">"}.
#' @return a logical vector of the comparison of v_n_1 and v_n_2.
#'
#' @examples
#' v_n_1 <- rep(0,3)
#' v_n_2 <- c(0, -1.1, 1.1)
#' calcComparison( v_n_1, v_n_2, "==")
#' calcComparison( v_n_1, v_n_2, ">")
#' calcComparison( v_n_1, v_n_2, ">=")
#'
#'
calcComparison <- function( v_n_1, v_n_2, s_comp ){
  v_s_options <- c("==","!=", "<=", ">=", "<", ">")

  if( !s_comp %in% v_s_options){
    stop( paste0( "s_comp must be in ", paste0(v_s_options, collapse = ", ")))
  }

  if( s_comp =="=="){
    return( v_n_1 == v_n_2 )
  }

  if( s_comp ==">="){
    return( v_n_1 >= v_n_2 )
  }

  if( s_comp =="<="){
    return( v_n_1 <= v_n_2 )
  }

  if( s_comp =="<"){
    return( v_n_1 < v_n_2 )
  }


  if( s_comp ==">"){
    return( v_n_1 > v_n_2 )
  }


  if( s_comp =="!="){
    return( v_n_1 != v_n_2 )
  }

}

