#' Read object from file or make object
#' @param s_file string of file where object is retreaved or saved see details. Must end in .rds.
#' @param makeObj function.
#' @param ... additional parameters to be passed to \code{makeObj}.
#' @param b_always_make_obj boolean if true \code{makeObj} is called regardless of if \code{s_file} exists.
#' @param b_message_time boolean, should time ellapsed by messaged.
#' @return either the object extracted from \code{s_file} or created by \code{makeObj}. See details.
#' @details It is common to produce large objects which take a long time to execute as part of a script.
#' In some cases we would like to retreave these objects but in other cases they need to be constructed again.
#' This function helps with that processes. If s_file exist and \code{b_always_make_obj} is false object will be extracted.
#' If s_file does not exist or \code{b_always_make_obj} object will be created and saved to \code{s_file}.
#' @examples
#' # create a random vector, assign it to v_n_random and save it to file.rds
#' v_n_random <- readOrMakeObj("file.rds", rnorm, n = 1000000,
#'                              b_always_make_obj = TRUE, b_message_time = TRUE )
#'
#' # retreave vector changing a parameter
#' v_n_random_2 <- readOrMakeObj("file.rds", rnorm, n = 100,
#'                                b_always_make_obj = FALSE, b_message_time = TRUE )
#' length(v_n_random_2)
#' # 1000000 not 100

readOrMakeObj <- function( s_file, makeObj = NULL, ..., b_always_make_obj = FALSE, b_message_time = FALSE ){
  pos_start_time <- Sys.time()
  if(file.exists(s_file) & (!b_always_make_obj)){
    obj_out <- readRDS(s_file)
    message("Object read from file")
  } else {
    if(is.null(makeObj)){
      stop("if s_file doesnt exist and b_always_make_obj is false must provide makeObj")
    }
    obj_out <- makeObj(...)
    message("Object made with makeObj")
    saveRDS(obj_out, s_file)
  }
  pos_end_time <- Sys.time()

  if( b_message_time == TRUE){
    s_time <- capture.output(print( pos_end_time-pos_start_time))
    message(s_time)
  }

  return(obj_out)
}
