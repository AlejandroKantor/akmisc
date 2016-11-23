#' Indicates if given fields are unique identifiers of a data.table
#'
#' @description Indicates if given fields allow for unique identification of rows i.e. if they can be teated as keys of the data.table.
#' @param dt_data a data.table.
#' @param v_s_cols a character vector of column names in dt_data which we expect allow for unique idenfication.
#' @return A boolean of whether values in v_s_keys allow for unique idenfication of rows in dt_data.
#' @examples
#' library(data.table)
#' dt_data <- data.table(var1 = c(1,1,2,2) , var2 = c("a", "b" , "a","b") )
#' isColsUniqueIdentifier(dt_data, v_s_cols = "var1")
#' isColsUniqueIdentifier(dt_data, v_s_cols = c("var1", "var2"))

isColsUniqueIdentifier <- function(dt_data, v_s_cols=NULL ){

  if(is.null(v_s_cols)){
    stop("must provide v_s_cols")
  }
  if(!all(v_s_cols %in% names(dt_data))){
    stop("v_s_cols must be in names(dt_data)")
  }

  i_len_initial <- nrow(dt_data)
  dt_data_keys <- dt_data[ , v_s_cols, with =F ]
  dt_data_keys <- unique(dt_data_keys)
  i_len_final <- nrow(dt_data_keys)
  b_result <- i_len_initial==i_len_final
  return(b_result)
}
