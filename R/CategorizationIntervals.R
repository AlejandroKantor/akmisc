#' Construction function for CategorizationIntervals class
#' @description Construction function for CategorizationIntervals class used in \code{\link{categorizeByIntervals}}.
#' @param value a vector.
#' @param s_intervals a character vector. Each element must start with "[" or "(" and end with "]" or ")".
#' @param min a numeric vector.
#' @param max a numeric vector.
#' @param close_left a integer vector with only values 0 or 1 or a logical vector.
#' @param close_right a integer vector with only values 0 or 1 or a logical vector.
#' @param i_digits_label number of digits in label created as part of the object. Default is 3.
#' @details Intervals are not allowed to overlap. CategorizationIntervals class inherits from data.table. This object type is used in \code{\link{categorizeByIntervals}}.
#' @return a CategorizationIntervals object.
#' @seealso \code{\link{categorizeByIntervals}}
#' @examples
#' library(data.table)
#' ci_intervals <- CategorizationIntervals(value = c(1,2,3),
#'                                         min = c(-Inf, 0, 0),
#'                                         max = c(0 , 0, Inf),
#'                                         close_left = c(0,1,0),
#'                                         close_right = c(0,1,0) )
#' print(ci_intervals)
#' ci_intervals <- CategorizationIntervals(value = c(1,2,3),
#'                                         s_intervals = c("(-Inf,0)","[0,0]","(0,Inf)"))
#' print(ci_intervals)
CategorizationIntervals <- function(value,
                                    s_intervals = NULL,
                                    min = NULL,
                                    max = NULL,
                                    close_left = NULL,
                                    close_right = NULL,
                                    i_digits_label = 3){

  if( !is.null(s_intervals)){
    ci_intervals <- extractCategoriesFromIntervals(value, s_intervals)
  } else {
    # making sure each variable has the correct data type
    checkRepCategorizationIntervals( min, max, close_left, close_right)
    ci_intervals <- data.table(value= value, min=min, max= max,
                               close_left= close_left,close_right= close_right )
  }
  structure(ci_intervals, class = "CategorizationIntervals")
  ci_intervals <- addLabelCategorizationIntervals(ci_intervals,i_digits = i_digits_label)
  return(ci_intervals)
}

#' Extract Categories from Interals
#' @description Parses a character vector of intervals to a data.table with numeric values
#' @param value a vector.
#' @param s_intervals a character vector. Each element must start with "[" or "(" and end with "]" or ")".
#' @return data.table with columns value, min, max, close_left and close_right.
#' @seealso \code{\link{categorizeByIntervals}}, \code{\link{CategorizationIntervals}}
#' @examples
#' dt_intervals <- extractCategoriesFromIntervals(value = c(1,2,3),
#'                                         s_intervals = c("(-Inf,0)","[0,0]","(0,Inf)"))
#' print(dt_intervals)
extractCategoriesFromIntervals <- function(value, s_intervals){
  dt_intervals <- data.table(value = value, intervals = s_intervals)
  dt_intervals[  , close_left := ifelse(grepl("^\\[" ,intervals), 1,
                                        ifelse(grepl("^\\(" ,intervals),0, NA_integer_)) ]
  dt_intervals[  , close_right := ifelse(grepl("\\]$" ,intervals), 1,
                                        ifelse(grepl("\\)$" ,intervals),0, NA_integer_)) ]
  dt_intervals[ , intervals := gsub( "\\(|\\[|\\]|\\)", "", intervals)]
  dt_intervals[ , min := as.numeric(gsub( ",.*" , "", intervals))]
  dt_intervals[ , max := as.numeric(gsub( ".*," , "", intervals))]
  dt_intervals[, intervals:= NULL]

  setcolorder(dt_intervals, c("value", "min", "max", "close_left" , "close_right"))
  return(dt_intervals)
}


#' Check rep of CategorizationIntervals
#'
#' @param min a numeric vector.
#' @param max a numeric vector.
#' @param close_left a integer vector with only values 0 or 1 or a logical vector.
#' @param close_right a integer vector with only values 0 or 1 or a logical vector.
#' @return NULL
#' @keywords internal
#'
checkRepCategorizationIntervals <- function( min = NULL,
                                             max = NULL,
                                             close_left = NULL,
                                             close_right = NULL){
  if( ! (is.numeric(max) & is.numeric(min) )){
    stop("max and min must be numeric")
  }
  if( any(is.na(c(max,min)) )){
    stop("max or min has a NA value")
  }

  if( is.logical(close_left) ){
    close_left <- as.numeric(close_left)
  }

  if( is.logical(close_left) ){
    close_left <- as.numeric(close_left)
  }

  if(!( all(close_left %in% c(0,1)) & all(close_right %in% c(0,1)))){
    stop("close_left and close_right can only be TRUE FALSE 0 or 1")
  }

}

#' Adds Label to a CategorizationIntervals object in memory.
#' @param ci_intervals a CategorizationIntervals object.
#' @param i_digits number of digits in label. Default is 3.
#' @return NULL
#' @details Modifies ci_intervals object in memory also returns object. Is used as part of construction function \code{\link{CategorizationIntervals}}.

addLabelCategorizationIntervals <- function(ci_intervals,i_digits = 3){
  ci_intervals[ , label_start:=ifelse( close_left==0, "(", "[")]
  ci_intervals[ , label_end:=ifelse( close_right==0, ")", "]")]

  ci_intervals[ , label:= paste0(label_start, round(min,i_digits),", ", round(max, i_digits), label_end)]

  ci_intervals[, label_start:=NULL]
  ci_intervals[, label_end:=NULL]
  return(ci_intervals)

}

#' Converts a data.table to a as.CategorizationIntervals object
#' @param dt_intervals data.table must have columns value, min, max, close_left, close_right as defined in \code{\link{CategorizationIntervals}}.
#' @return a CategorizationIntervals object.
#' @seealso \code{\link{CategorizationIntervals}}
#' @examples
#' library(data.table)
#' dt_intervals <- data.table(value = c(1,2,3),
#'                            min = c(-Inf, 0, 0),
#'                            max = c(0 , 0, Inf),
#'                            close_left = c(0,1,0),
#'                            close_right = c(0,1,0) )
#' ci_intervals <- as.CategorizationIntervals(dt_intervals)
#' print(ci_intervals)

as.CategorizationIntervals <- function(dt_intervals){
  ci_intervals <- CategorizationIntervals(value = dt_intervals[ , value],
                                          min = dt_intervals[ ,  min],
                                          max = dt_intervals[ , max],
                                          close_left = dt_intervals[ , close_left],
                                          close_right = dt_intervals[ , close_right])
  return(ci_intervals)
}




