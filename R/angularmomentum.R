#' Compute the total angular momentum of a particle distribution
#'
#' @description
#' Computes the total angular momentum of a set of particles about their
#' centre of mass (or about a user-specified origin).
#'
#' @param m n-vector or scalar of particle masses. For specific angular momentum computation, set \code{m=1}.
#' @param x \eqn{n\times3} matrix of particle positions.
#' @param v \eqn{n\times3} matrix of particle velocities.
#' @param x0 Optional 3-vector specifying the origin about which the
#'   angular momentum is computed. If \code{NULL} (default), the centre of
#'   mass is used.
#' @param v0 Optional 3-vector specifying the reference velocity. If
#'   \code{NULL} (default), the centre-of-mass velocity is used.
#'
#' @return
#' A numeric vector of length three containing the Cartesian components of the
#' total angular momentum,
#' \deqn{\mathbf{J}=\sum_i m_i\,(\mathbf{x}_i-\mathbf{x}_0)\times
#' (\mathbf{v}_i-\mathbf{v}_0).}
#'
#' If \code{m} is \code{NULL}, a vector of zeros is returned.
#'
#' @examples
#' m = c(1, 2)
#' x = rbind(c(1, 0, 0), c(0, 1, 0))
#' v = rbind(c(0, 1, 0), c(-1, 0, 0))
#' angularmomentum(m, x, v)
#'
#' @export
#'
angularmomentum = function(m, x, v, x0 = NULL, v0 = NULL) {

  if (is.null(m) || length(m)==0 || sum(m)==0) return(c(0,0,0))

  # handle single-particle input
  if (length(x)==3) x = matrix(x, nrow = 1)
  if (length(v)==3) v = matrix(v, nrow = 1)

  # checks
  if (any(dim(x)!=dim(v))) stop('x and v must have the same size')
  if (nrow(x)!=length(m) & length(m)!=1) stop('m and x have inconsistent size')

  # automatically determine origin if not provided
  if (is.null(x0)) x0 = colSums(x*m) / sum(m)
  if (is.null(v0)) v0 = colSums(v*m) / sum(m)

  # checks
  if (length(x0)!=3) stop('x0 must be a 3-vector')
  if (length(v0)!=3) stop('v0 must be a 3-vector')

  # centre coordinates
  x = t(t(x) - as.vector(x0))
  v = t(t(v) - as.vector(v0))

  # total angular momentum
  if (nrow(x)==1) {
    m * cooltools::vectorproduct(x, v)
  } else {
    colSums(m * cooltools::vectorproduct(x, v))
  }

}
