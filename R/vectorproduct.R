#' Vector product
#'
#' @description
#' Computes the vector (cross) product of two three-dimensional vectors. The
#' inputs may be individual vectors or matrices whose rows represent vectors.
#'
#' @param x,y Numeric vectors of length three or matrices with three columns.
#'   If a matrix is supplied, each row is interpreted as a vector.
#' @param normalize Logical flag indicating whether the resulting vector(s)
#'   should be normalized to unit length.
#'
#' @return
#' Returns either a 3-element vector or an \eqn{n\times3} matrix containing
#' the row-wise vector products.
#'
#' @seealso \code{\link{scalarproduct}}, \code{\link{vectornorm}},
#'   \code{\link{unitvector}}
#'
#' @export

vectorproduct = function(x, y, normalize = FALSE) {

  x = as.matrix(x)
  y = as.matrix(y)

  # convert column vectors to row vectors
  if (ncol(x) == 1) x = t(x)
  if (ncol(y) == 1) y = t(y)

  if (ncol(x) != 3 || ncol(y) != 3)
    stop("x and y must have three columns")

  # matrix-vector
  if (nrow(x) > 1 && nrow(y) == 1)
    y = y[rep(1, nrow(x)), , drop = FALSE]

  # vector-matrix
  if (nrow(x) == 1 && nrow(y) > 1)
    x = x[rep(1, nrow(y)), , drop = FALSE]

  if (nrow(x) != nrow(y))
    stop("x and y have incompatible dimensions")

  out = cbind(
    x = x[,2] * y[,3] - x[,3] * y[,2],
    y = x[,3] * y[,1] - x[,1] * y[,3],
    z = x[,1] * y[,2] - x[,2] * y[,1]
  )

  if (normalize)
    out = unitvector(out)

  if (nrow(out) == 1)
    return(drop(out))

  out

}
