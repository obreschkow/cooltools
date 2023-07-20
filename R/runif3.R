#' Generate randomly oriented vectors in 3D
#'
#' @importFrom randtoolbox halton
#'
#' @description Generate randomly oriented vectors in 3D, following an isotropic distribution (optionally truncated to a region).
#'
#' @param n number of random vectors to be generated
#' @param r 2-vector specifying the range of radii
#' @param azimuth 2-vector specifying the range of azimuth angles (maximum range 0..2*pi)
#' @param polarangle 2-vector specifying the range of polar angles (maximum range 0..pi)
#' @param quasi logical flag. If true, quasi-random numbers with low-discrepancy are drawn, based on a Halton sequence. Otherwise, the standard internal pseudo-random generator of \code{runif()} is used.
#' @param start starting index of Halton sequence. Only used if \code{quasi=TRUE}.
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

runif3 = function(n=1,r=c(0,1), azimuth=c(0,2*pi), polarangle=c(0,pi), quasi=FALSE, start=1) {

  # handle input
  if (length(r)==1) r=rep(r,2)
  if (length(azimuth)==1) azimuth=rep(azimuth,2)
  if (length(polarangle)==1) polarangle=rep(polarangle,2)

  # make random vectors
  if (quasi) {
    rnd = randtoolbox::halton(n,3,start=start)
    r = (rnd[,1]*(r[2]^3-r[1]^3)+r[1]^3)^(1/3)
    phi = rnd[,2]*(azimuth[2]-azimuth[1])+azimuth[1]
    z = rnd[,3]*(cos(polarangle[2])-cos(polarangle[1]))+cos(polarangle[1])
  } else {
    r = runif(n,r[1]^3,r[2]^3)^(1/3)
    phi = runif(n,azimuth[1],azimuth[2])
    z = runif(n,cos(polarangle[2]),cos(polarangle[1]))
  }
  xy = sqrt(1-z^2)*r
  vec = cbind(x=xy*cos(phi),y=xy*sin(phi),z=z*r)

  # return values
  return(vec)

}
