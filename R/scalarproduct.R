#' Scalar product
#'
#' @description
#' Computes the scalar (dot) product of vectors. The inputs may be individual
#' vectors or matrices whose rows represent vectors.
#'
#' @param x,y Numeric vectors or matrices. If a matrix is supplied, each row is
#'   interpreted as a vector.
#'
#' @return
#' Returns either a scalar (vector-vector case) or a vector containing the
#' row-wise scalar products.
#'
#' @seealso \code{\link{scalarproduct}}, \code{\link{vectornorm}},
#'   \code{\link{unitvector}}
#'
#' @export

scalarproduct = function(x, y) {

  x = as.matrix(x)
  y = as.matrix(y)

  # convert column vectors to row vectors
  if (ncol(x) == 1) x = t(x)
  if (ncol(y) == 1) y = t(y)

  # matrix-vector
  if (nrow(x) > 1 && nrow(y) == 1) {
    if (ncol(x) != ncol(y))
      stop("x and y have incompatible dimensions")
    return(drop(x %*% c(y)))
  }

  # vector-matrix
  if (nrow(x) == 1 && nrow(y) > 1) {
    if (ncol(x) != ncol(y))
      stop("x and y have incompatible dimensions")
    return(drop(y %*% c(x)))
  }

  # vector-vector or matrix-matrix
  if (!all(dim(x) == dim(y)))
    stop("x and y have incompatible dimensions")

  rowSums(x * y)

}
