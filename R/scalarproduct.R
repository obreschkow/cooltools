#' Scalar product
#'
#' @description Compute scalar product of two vectors
#'
#' @param x,y d-element vectors or n-by-d matrices representing n d-element vectors
#'
#' @return Returns a scalar or a n-element vector with the scalar products
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{vectorproduct}}
#'
#' @export

scalarproduct = function(x,y) {

  x = as.matrix(x)
  y = as.matrix(y)

  if (length(x)!=length(y)) stop('x and y must have the same length')

  if (dim(x)[2]==1) {
    x = t(x)
    y = t(y)
  }

  out = rowSums(x*y)

  return(out)

}
