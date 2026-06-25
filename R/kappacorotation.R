#' Compute the co-rotational kinetic energy fraction
#'
#' @description
#' Computes the co-rotational kinetic energy fraction,
#' \eqn{\kappa_\mathrm{co}}, of a particle distribution. This quantity is
#' defined as the fraction of the total kinetic energy invested in ordered
#' co-rotation about the total angular momentum axis. It is commonly used as a
#' measure of the rotational support of galaxies in particle-based simulations.
#'
#' @param m n-vector of particle masses.
#' @param x \eqn{n\times3} matrix of particle positions.
#' @param v \eqn{n\times3} matrix of particle velocities.
#' @param x0 Optional length-3 vector specifying the origin. If \code{NULL}
#'   (default), the centre of mass is used.
#' @param v0 Optional length-3 vector specifying the reference velocity. If
#'   \code{NULL} (default), the centre-of-mass velocity is used.
#'
#' @return
#' A scalar in the range \eqn{[0,1]} giving the fraction of the total kinetic
#' energy contained in co-rotating azimuthal motion,
#' \deqn{
#' \kappa_\mathrm{co}
#' =
#' \frac{\sum_{j_z>0} m\,v_\phi^2}
#'      {\sum m\,|\mathbf{v}|^2},
#' }
#' where \eqn{j_z} is the specific angular momentum about the total angular
#' momentum axis and \eqn{v_\phi=j_z/R} is the azimuthal velocity. The common
#' factor of \eqn{1/2} in the kinetic energies cancels.
#'
#' If \code{m} is \code{NULL}, \code{NA} is returned.
#'
#' @details
#' The rotation axis is taken to be the direction of the total angular
#' momentum of the particle distribution after subtracting the centre-of-mass
#' position and velocity. Only particles with positive angular momentum about
#' this axis contribute to the co-rotational kinetic energy.
#'
#' @seealso
#' \code{\link{angularmomentum}}
#'#'
#' @examples
#' m <- rep(1, 100)
#' x <- matrix(rnorm(300), ncol = 3)
#' v <- matrix(rnorm(300), ncol = 3)
#' kappacorotation(m, x, v)
#'
#' @export
#'
kappacorotation = function(m, x, v, x0 = NULL, v0 = NULL) {

  if (is.null(m) || length(m)==0 || sum(m)==0) return(NA)

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

  # specific angular momentum
  j = cooltools::vectorproduct(x, v)

  # axis of total angular momentum
  if (nrow(x)==1) {
    ez = cooltools::unitvector(j)
  } else {
    ez = cooltools::unitvector(colSums(m * j))
  }

  if (any(!is.finite(ez))) return(NA)

  # specific AM component along rotation axis
  jz = cooltools::scalarproduct(j, ez)

  # cylindrical radius
  R2 = rowSums(x^2) - cooltools::scalarproduct(x, ez)^2
  R = sqrt(pmax(R2, 0))

  # azimuthal velocity
  vphi = rep(0, length(m))
  sel = R > 0
  vphi[sel] = jz[sel] / R[sel]

  # kinetic energies, both without factor 1/2
  K_corot = sum(m * pmax(vphi, 0)^2)
  K_total = sum(m * rowSums(v^2))

  kappaco = K_corot/K_total

  return(kappaco)

}
