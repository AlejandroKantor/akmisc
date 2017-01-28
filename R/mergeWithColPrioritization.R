# Used to avoid incorrect notes of "no visible binding"
utils::globalVariables(c("present_in_prior"))

#' Merger of two data.tables with repeated columns with prioritization
#' @description Merger (outer join) of two data.tables with repeated columns where we want to prioritize the values of one data.table.
#' @param dt_prior data.table for which has prioritized columns.\code{v_s_keys} must be columns in \code{dt_prior}.
#' @param dt_other data.table wich we want to merge. \code{v_s_keys} must be columns in \code{dt_other}.
#' @param v_s_keys character vector of column names to merge by.
#' @return data.table of prioritized merger of dt_prior and dt_other.
#'
#' @examples
#' library(data.table)
#' i_num <- 3
#' dt_prior <- data.table( id = 1:i_num,
#'                         var1 = letters[1:i_num],
#'                         var2 = 111)
#' dt_other <- data.table( id = 1:(i_num+2),
#'                         var1 = paste0(letters[10 + 1:(i_num+2)],letters[10 + 1:(i_num+2)]) ,
#'                         var3 = -999)
#' v_s_keys <- "id"
#'
#' dt_prior
#' dt_other
#' mergeWithColPrioritization(dt_prior, dt_other,v_s_keys )
#'


mergeWithColPrioritization <- function( dt_prior, dt_other, v_s_keys){

  #identify key values in dt_prior
  dt_prior_keys <- dt_prior[ , v_s_keys, with= F]
  dt_prior_keys[ , present_in_prior:=1]

  setkeyv(dt_prior_keys, v_s_keys)
  setkeyv(dt_other, v_s_keys)

  # add 'present_in_prior' columns to dt_other.
  dt_other <- dt_prior_keys[dt_other]
  # generate data.table with values of dt_other for key values not present in dt_prior
  dt_outer <- dt_other[ is.na(present_in_prior)]
  dt_outer[,present_in_prior:=NULL ]
  # generate data.table with values of dt_other for key values present in dt_prior
  dt_inner <- dt_other[ present_in_prior==1]
  dt_inner[,present_in_prior:=NULL ]

  # remove columns from dt_prior in dt_inner because we will use values fom dt_prior
  v_col_prior <- setdiff(names(dt_prior), v_s_keys)
  dt_inner <- dt_inner[ , setdiff(names(dt_inner),v_col_prior),with=F  ]

  setkeyv(dt_inner, v_s_keys)
  setkeyv(dt_prior, v_s_keys)

  # add to dt_prior values of non prioritized columns from dt_other
  dt_prior <- merge(dt_inner, dt_prior,  by= v_s_keys,all=T )

  # include in data.table values from dt_outher
  dt_out <- rbindlist(l= list( dt_outer, dt_prior), use.names = T, fill=T)
  setorderv(dt_out, v_s_keys)
  return(dt_out)
}


