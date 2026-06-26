#' Second moment tensor
#'
#' @description
#' Computes the second moment tensor of a set of point masses in arbitrary
#' dimensions,
#' \deqn{
#' \mathbf{M}
#' =
#' \sum_i m_i\,\mathbf{x}_i\mathbf{x}_i^\mathrm{T}.
#' }
#'
#' @param x Numeric vector or matrix. If a matrix is supplied, each row is
#'   interpreted as a point.
#' @param m Numeric scalar or vector of point masses. If a scalar is supplied,
#'   the same mass is assigned to every point.
#'
#' @return
#' A symmetric \eqn{d\times d} matrix containing the second moment tensor,
#' where \eqn{d} is the dimension of the points.
#'
#' @seealso
#' \code{\link{quadrupole}}, \code{\link{inertia}}
#'
#' @export

moments = function(x, m = 1) {

  x = as.matrix(x)
  if (ncol(x) == 1) x = t(x)

  n = nrow(x)

  if (length(m) == 1)
    m = rep(m, n)

  if (length(m) != n)
    stop("m must have length one or the number of rows of x")

  crossprod(x, x * m)
}

# PREVIOUS SIMPLE 3D-ONLY VERSION, TESTED TO GIVE IDENTICAL RESULTS IN 3D
# moments = function(x,m=1) {
#
#   if (length(x)==3) x = array(x,c(1,3)) # to make sure that x is an array
#
#   M = array(NA,c(3,3))
#   M[1,1] = sum(m*x[,1]^2)
#   M[2,2] = sum(m*x[,2]^2)
#   M[3,3] = sum(m*x[,3]^2)
#   M[1,2] = M[2,1] = sum(m*x[,1]*x[,2])
#   M[2,3] = M[3,2] = sum(m*x[,2]*x[,3])
#   M[3,1] = M[1,3] = sum(m*x[,3]*x[,1])
#
#   return(M)
#
# }
