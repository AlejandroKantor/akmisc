#' Categorize by intervals
#' @description Categorizes numeric values allowing for different types of intervals.
#' @param v_n_values a numeric vector. Must have length of at least 1.
#' @param ci_intervals a CategorizationIntervals object. See \code{\ink{CategorizationIntervals}}.
#' @details Flexibles intervals are allowed in ci_intervals for example '(-Inf,0), [0,0], (0,Inf)' is valid.
#' @return a vector with the same length as v_n_values with the same type as ci_intervals[['value']].
#' @seealso \code{\ink{CategorizationIntervals}}
#' @examples
#' ci_intervals <- CategorizationIntervals(value = c(-1,0,1),
#'                                         min = c(-Inf, 0, 0),
#'                                         max = c(0 , 0, Inf),
#'                                         close_left = c(0,1,0),
#'                                         close_right = c(0,1,0) )
#' v_n_values <- -3:3
#' categorizeByIntervals(v_n_values)
#'
#' ci_intervals <- CategorizationIntervals(value = c("Neg","Zero","Pos"),
#'                                         min = c(-Inf, 0, 0),
#'                                         max = c(0 , 0, Inf),
#'                                         close_left = c(0,1,0),
#'                                         close_right = c(0,1,0) )
#' v_n_values <- -3:3
#' categorizeByIntervals(v_n_values)
#'
categorizeByIntervals <- function( v_n_values, ci_intervals){

  i_n <- length(v_n_values)
  s_class_category <- class(ci_intervals[, value])
  dt_values <- data.table( id= 1:i_n, values=v_n_values)
  if( s_class_category == "factor"){
    dt_values[ , category := factor(NA)]
  } else {
    dt_values[ , category := as(object = NA,
                               Class = s_class_category) ]
  }
  i_num_interval_cases <- nrow(ci_intervals)
  for( i in 1:i_num_interval_cases){

    k_cat_name <- ci_intervals[i, value]
    s_comp_lower <- ifelse( ci_intervals[i, close_left]==0, "<", "<=")
    s_comp_upper <- ifelse( ci_intervals[i, close_right]==0, ">", ">=")
    n_lower <- ci_intervals[i, min]
    n_upper <-  ci_intervals[i, max]

    dt_values[  calcComparison(n_lower, values, s_comp_lower ) &
                  calcComparison(n_upper, values, s_comp_upper )  , category:=k_cat_name ]

  }
  setorder(dt_values, id)
  return(dt_values[ , category])
}
