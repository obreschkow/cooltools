#' Quadrupole tensor
#'
#' @description
#' Computes the trace-free mass quadrupole tensor of a set of point masses in
#' arbitrary dimensions,
#' \deqn{
#' \mathbf{Q}
#' =
#' \sum_i m_i
#' \left(
#' d\,\mathbf{x}_i\mathbf{x}_i^\mathrm{T}
#' -
#' |\mathbf{x}_i|^2\mathbf{I}
#' \right),
#' }
#' where \eqn{d} is the dimensionality of the points and \eqn{\mathbf{I}} is
#' the \eqn{d\times d} identity matrix.
#'
#' @param x Numeric vector or matrix. If a matrix is supplied, each row is
#'   interpreted as a point.
#' @param m Numeric scalar or vector of point masses. If a scalar is supplied,
#'   the same mass is assigned to every point.
#'
#' @return
#' A symmetric \eqn{d\times d} matrix containing the trace-free quadrupole
#' tensor.
#'
#' @export
#'
quadrupole = function(x, m = 1) {

  x = as.matrix(x)
  if (ncol(x) == 1) x = t(x)

  n = nrow(x)
  d = ncol(x)

  if (length(m) == 1)
    m = rep(m, n)

  if (length(m) != n)
    stop("m must have length one or the number of rows of x")

  # sum_i m_i x_i x_i^T
  M = crossprod(x, x * m)

  # sum_i m_i |x_i|^2
  r2 = rowSums(x^2)
  tr = sum(m * r2)

  # trace-free quadrupole tensor
  d * M - tr * diag(d)
}

# PREVIOUS SIMPLE 3D-ONLY VERSION, TESTED TO GIVE IDENTICAL RESULTS IN 3D
# quadrupole2 = function(x,m=1) {
#
#   if (length(x)==3) x = array(x,c(1,3)) # to make sure that x is an array
#
#   Q = array(NA,c(3,3))
#   Q[1,1] = sum(m*(2*x[,1]^2-x[,2]^2-x[,3]^2))
#   Q[2,2] = sum(m*(2*x[,2]^2-x[,3]^2-x[,1]^2))
#   Q[3,3] = sum(m*(2*x[,3]^2-x[,1]^2-x[,2]^2))
#   Q[1,2] = Q[2,1] = sum(m*(3*x[,1]*x[,2]))
#   Q[2,3] = Q[3,2] = sum(m*(3*x[,2]*x[,3]))
#   Q[3,1] = Q[1,3] = sum(m*(3*x[,3]*x[,1]))
#
#   return(Q)
#
# }
