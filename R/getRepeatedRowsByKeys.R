#' Get repeated rows by key
#'
#' @description Returns a data.table of the repeated rows for a given key.
#' @param dt_data a data.table.
#' @param v_s_keys a character vector of column names in \code{dt_data}.
#' @return a data.table of all the rows which the combination of the values of \code{v_s_keys} are repeated. See \code{details}.
#' @details all rows of \code{dt_data} for which the combination of values of \code{v_s_keys} are repeated is returned. Additionally
#' a new column \code{rep_count_} is added  which indicates the number of times the key combination is repeated in the data set.
#' If there is no repeated cases function returns \code{NULL}.
#' @examples
#' library(data.table)
#' dt_data <- data.table( unique_identifier = c(1,2,3,3,3,4,4,5), value = rnorm(8))
#'
#' getRepeatedRowsByKeys(dt_data, "unique_identifier")
#'
getRepeatedRowsByKeys <- function(dt_data, v_s_keys = NULL) {
  dt_rep <- copy(dt_data)

  if(!all(v_s_keys %in% names(dt_data))){
    stop("v_s_cols must be in names(dt_data)")
  }

  s_new_col <- "rep_count_"
  if( s_new_col %in% v_s_keys){
    stop(paste0(s_new_col, " is not a valid name for v_s_keys"))
  }

  if( s_new_col %in% names(dt_data)){
    warning(paste0("Data already has a ", s_new_col ," column; it has been replaced"))
    dt_rep[ , rep_count_ := NULL]
  }

  dt_rep[ , rep_count_ := .N, by = v_s_keys ]

  dt_rep <- dt_rep[ rep_count_ > 1 ]
  if(nrow(dt_rep) == 0 ){
    warning("No repeated cases")
    dt_rep <- NULL
  }
  return(dt_rep)

}
