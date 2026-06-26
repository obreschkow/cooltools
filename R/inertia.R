#' Moment of inertia tensor
#'
#' @description
#' Computes the moment of inertia tensor of a set of point masses in arbitrary
#' dimensions,
#' \deqn{
#' \mathbf{I}
#' =
#' \sum_i m_i
#' \left(
#' |\mathbf{x}_i|^2\mathbf{1}
#' -
#' \mathbf{x}_i\mathbf{x}_i^\mathrm{T}
#' \right),
#' }
#' where \eqn{\mathbf{1}} is the identity matrix.
#'
#' @param x Numeric vector or matrix. If a matrix is supplied, each row is
#'   interpreted as a point.
#' @param m Numeric scalar or vector of point masses. If a scalar is supplied,
#'   the same mass is assigned to every point.
#'
#' @return
#' A symmetric \eqn{d\times d} matrix containing the moment of inertia tensor,
#' where \eqn{d} is the dimension of the points.
#'
#' @seealso
#' \code{\link{quadrupole}}, \code{\link{moments}}
#'
#' @export

inertia = function(x, m = 1) {

  x = as.matrix(x)
  if (ncol(x) == 1) x = t(x)

  n = nrow(x)
  d = ncol(x)

  if (length(m) == 1)
    m = rep(m, n)

  if (length(m) != n)
    stop("m must have length one or the number of rows of x")

  M = crossprod(x, x * m)
  trM = sum(m * rowSums(x^2))

  trM * diag(d) - M
}

# PREVIOUS SIMPLE 3D-ONLY VERSION, TESTED TO GIVE IDENTICAL RESULTS IN 3D
# inertia = function(x,m=1) {
#
#   if (length(x)==3) x = array(x,c(1,3)) # to make sure that x is an array
#
#   I = array(NA,c(3,3))
#   I[1,1] = sum(m*(x[,2]^2+x[,3]^2))
#   I[2,2] = sum(m*(x[,3]^2+x[,1]^2))
#   I[3,3] = sum(m*(x[,1]^2+x[,2]^2))
#   I[1,2] = I[2,1] = -sum(m*x[,1]*x[,2])
#   I[2,3] = I[3,2] = -sum(m*x[,2]*x[,3])
#   I[3,1] = I[1,3] = -sum(m*x[,3]*x[,1])
#
#   return(I)
#
# }
