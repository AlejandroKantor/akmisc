# Used to avoid incorrect notes of "no visible binding"
utils::globalVariables(c(".", "freq","perc"))

#' Frequency table of the number of times of individual value occurs
#'
#' @param dt_data a data.table.
#' @param v_s_vars a character vector of column names in dt_data.
#' @param b_total_row default FALSE, whether we include a row with the total amount of values.
#' @param b_include_perc default FALSE, whether we include a column with percentages.
#' @param s_order_by default "v_s_vars". How the output data.table should be ordered. If "v_s_vars", ordered by values of parameter. If "ascending", ordered by ascending frequency. If "descending", ordered by "descending" frequency.
#' @return A data.table with the number of times an individual value occurs
#' @examples
#' library(data.table)
#' dt_data <- data.table( var1 = c(1,1,2,2,3,3,3,3),
#'                        var2 = c(6,6,6,7,7,7,7,7))
#' freqTable(dt_data, "var1", b_total_row = TRUE)
#' freqTable(dt_data, c("var1","var2"), b_include_perc = TRUE)
#' freqTable(dt_data, c("var1","var2"), b_include_perc = TRUE, s_order_by = "descending" )
#' freqTable(dt_data, c("var1","var2"), b_total_row = TRUE, b_include_perc = TRUE, s_order_by = "descending")


freqTable <- function(dt_data,
                      v_s_vars,
                      b_total_row = FALSE ,
                      b_include_perc = FALSE,
                      s_order_by = "v_s_vars"){

  if(!all(v_s_vars %in% names(dt_data))){
    stop("v_s_vars must be in names(dt_data)")
  }

  v_s_order_options <- c("v_s_vars","ascending","descending")
  if(! s_order_by %in% v_s_order_options ){
    stop(paste0("s_order_by" ," must be one of the following ", paste0(v_s_order_options, collapse = ', ')))
  }


  i_nrow <- nrow(dt_data)
  dt_agg <- dt_data[ , .(freq = .N), keyby= v_s_vars ]

  if(s_order_by == "ascending"){
    setorder(dt_agg, freq)
  } else if( s_order_by == "descending" ){
    setorder(dt_agg, -freq)
  }

  if(b_total_row ==TRUE){
    dt_total <- data.table(freq = i_nrow)
    for(s_var in v_s_vars){
      dt_total[[s_var ]] = "Total"
    }
    dt_agg <- rbindlist(l= list(dt_agg, dt_total),use.names = T)
  }

  if( b_include_perc == TRUE){
    dt_agg[ , perc := freq/i_nrow * 100][]
  }
  return(dt_agg)
}


