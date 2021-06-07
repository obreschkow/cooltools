#' Generate randomly oriented vectors in 3D
#'
#' @description Generate randomly oriented vectors in 3D, following an isotropic distribution (optionally truncated to a region).
#'
#' @param n number of random vectors to be generated
#' @param r 2-vector specifying the range of radii
#' @param azimuth 2-vector specifying the range of azimuth angles (maximum range 0..2*pi)
#' @param polarangle 2-vector specifying the range of polar angles (maximum range 0..pi)
#'
#' @return Returns an n-by-3 array of n vectors.
#'
#' @author Danail Obreschkow
#'
#' @examples
#' ## draw 20 unit vectors on a sphere
#' x = runif3(20,r=c(1,1))
#' print(rowSums(x^2))
#'
#' @seealso \code{\link{runif2}}
#'
#' @export

runif3 = function(n=1,r=c(0,1), azimuth=c(0,2*pi), polarangle=c(0,pi)) {

  # handle input
  if (length(r)==1) r=rep(r,2)
  if (length(azimuth)==1) azimuth=rep(azimuth,2)
  if (length(polarangle)==1) polarangle=rep(polarangle,2)

  # make random vectors
  r = runif(n,r[1]^3,r[2]^3)^(1/3)
  phi = runif(n,azimuth[1],azimuth[2])
  z = runif(n,cos(polarangle[2]),cos(polarangle[1]))
  xy = sqrt(1-z^2)*r
  vec = cbind(x=xy*cos(phi),y=xy*sin(phi),z=z*r)

  # return values
  return(vec)

}
