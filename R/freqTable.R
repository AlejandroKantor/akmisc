#' Frequency table of the number of times of individual value occurs
#'
#' @param dt_data a data.table.
#' @param v_s_vars a character vector of column names in dt_data.
#' @param b_total_row default FALSE, whether we include a row with the total amount of values.
#' @param b_include_perc default FALSE, whether we include a column with percentages.
#' @return A data.table with the number of times an individual value occurs
#' @examples
#' dt_data <- data.table( var1 = c(1,1,2,2,3,3,3,3),
#'                        var2 = c(6,6,6,7,7,7,7,7))
#' freqTable(dt_data, "var1", b_total_row = T)
#' freqTable(dt_data, c("var1","var2"), b_include_perc = T)

freqTable <- function(dt_data,
                      v_s_vars,
                      b_total_row = FALSE ,
                      b_include_perc = FALSE){

  if(!all(v_s_vars %in% names(dt_data))){
    stop("v_s_vars must be in names(dt_data)")
  }

  i_nrow <- nrow(dt_data)
  dt_agg <- dt_data[ , .(freq = .N), keyby= v_s_vars ]

  if(b_total_row ==TRUE){
    dt_total <- data.table(freq = i_nrow)
    for(s_var in v_s_vars){
      dt_total[[s_var ]] = "Total"
    }
    dt_agg <- rbindlist(l= list(dt_agg, dt_total),use.names = T)
  }

  if( b_include_perc == TRUE){
    dt_agg[ , perc := freq/i_nrow * 100]
  }
  return(dt_agg)
}


