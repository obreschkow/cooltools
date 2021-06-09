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
#' @seealso \code{\link{scalarproduct}}, \code{\link{vectornorm}} and \code{\link{unitvector}}
#'
#' @export

vectorproduct = function(x,y,normalize=FALSE) {

  x = as.matrix(x)
  y = as.matrix(y)

  if (length(x)!=length(y)) stop('x and y must have the same length')

  if (length(x)==3) {
    x = matrix(x,1,3)
    y = matrix(y,1,3)
  }

  out = cbind(x=x[,2]*y[,3]-x[,3]*y[,2],
              y=x[,3]*y[,1]-x[,1]*y[,3],
              z=x[,1]*y[,2]-x[,2]*y[,1])

  if (normalize) out = unitvector(out)

  return(out)

}
