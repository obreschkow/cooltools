#' Evenly distributed n points on a sphere
#'
#' @description Distributes n points on a sphere in a relatively even fashion following the generalised Fibonacci algorithm, described at http://extremelearning.com.au/evenly-distributing-points-on-a-sphere/
#'
#' @param n number of points to be placed on the sphere
#' @param r radius
#' @param out.xyz logical flag specifying whether to return Cartesian coordinates (default is TRUE)
#' @param out.sph logical flag specifying whether to return spherical coordinates (default is FALSE); \code{theta}=colatitute (=polar angle=angle from z-axis), \code{phi}=longitude (=azimuth angle),
#'
#' @return matrix of n points (in n rows), in Cartesian and/or spherical coordinates.
#'
#' @examples
#' ## plot standard projections of a 1000-point Fibonacci sphere
#' xyz = fibonaccisphere()
#' plot(xyz, asp=1, pch=16, cex=0.5)
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{runif3}}
#'
#' @export

fibonaccisphere = function(n=1000, r=1, out.xyz=TRUE, out.sph=FALSE) {

  if (n<1 | round(n)!=n) stop('n must be a positive integer')
  if (!out.xyz & !out.sph) stop('either out.xyz and/or out.sph must be TRUE')

  goldenratio = (1+sqrt(5))/2
  i = seq(n)-0.5
  z = 1-2*i/n # z-coordinate for unit sphere
  theta = acos(pmax(-1,pmin(1,z))) # polar angle
  phi = (2*pi*i/goldenratio)%%(2*pi)

  if (out.xyz) {
    x = r*sin(theta)*cos(phi)
    y = r*sin(theta)*sin(phi)
    z = r*z
    if (out.sph) {
      out = cbind(x=x,y=y,z=z,theta=theta,phi=phi)
    } else {
      out = cbind(x=x,y=y,z=z)
    }
  } else {
    out = cbind(theta=theta,phi=phi)
  }

  return(out)

}
