#' Performs garbadge collections non verbosely
#'
#' @return taken from  \code{\link{gc}}: gc returns a matrix with rows "Ncells"
#' @seealso \code{\link{gc}}
#' @examples
#' collectGarbage()
collectGarbage <- function(){
  invisible({
    gc()
    gc()
  })
}


