#' Last element of a vector
#'
##' @importFrom utils tail
#'
#' @description Returns the last element of a vector or the n-th element counting from the end of a vector.
#'
#' @param x vector
#' @param n optional integer specifying the n-th element from the end to be returned
#'
#' @return scalar of the same type as \code{x}
#'
#' @author Danail Obreschkow
#'
#' @export

last = function(x, n=1) {
  return(utils::tail(x,n)[1])
}
