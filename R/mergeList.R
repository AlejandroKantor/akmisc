#' Merge a list of data.table
#'
#' @param l_dt_data a list of data.tables which we want to merge
#' @param v_s_keys character vector of columns used for merging
#' @param all logical default \code{TRUE}. If \code{TRUE} keep all unique \code{v_s_keys} cases. If \code{FALSE} we only want to keep cases on all the data.tables.
#' @return a data.table which is the merger of \code{l_dt_data}. \code{v_s_keys} are set as keys of returned object.
#' @seealso \code{\link{merge.data.table}}
#' @examples
#' dt_1 <- data.table(let = letters[1:3], num = 1:3)
#' dt_2 <- data.table(let= letters[2:4], num = 1:3)
#' l_len_2 <- list(dt_1, dt_2)
#' mergeList(l_len_2, v_s_keys= "let")
#'
#' mergeList(l_len_2, v_s_keys= "let", all=F )
mergeList <- function(l_dt_data, v_s_keys, all=TRUE){
  dt_merged <- Reduce(function(dt_left, dt_right) merge(dt_left,
                                           dt_right,
                                           by=v_s_keys,
                                           all = all),
         l_dt_data, accumulate=F)
  return(dt_merged)
}
