#' Vector product
#'
#' @description Compute cross product of two 3-element vectors
#'
#' @param x,y 3-element vectors or n-by-3 matrices representing n 3-element vectors
#' @param normalize logical flag; if set to TRUE the cross-product(s) is/are automatically normalized
#'
#' @return Returns a 3-element vector or a n-by-3 element matrix with the cross products
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{vectornorm}} and \code{\link{unitvector}}
#'
#' @export

vectorproduct = function(x,y,normalize=FALSE) {

  if (length(x)!=length(y)) stop('x and y must have the same length')

  if (length(x)==3) {
    x = array(x,c(1,3))
    y = array(y,c(1,3))
  }

  n = dim(x)[1]
  z = array(NA,c(n,3))

  for (i in seq(n)) {
    z[i,] = c(x[i,2]*y[i,3]-x[i,3]*y[i,2], x[i,3]*y[i,1]-x[i,1]*y[i,3], x[i,1]*y[i,2]-x[i,2]*y[i,1])
  }

  if (normalize) z = unitvector(z)

  return(z)

}
