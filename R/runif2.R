#' Generate randomly oriented vectors in 2D
#'
#' @description Generate randomly oriented vectors in 2D, following an isotropic distribution (optionally truncated to a region).
#'
#' @param n number of random vectors to be generated
#' @param r 2-vector specifying the range of radii
#' @param azimuth 2-vector specifying the range of azimuth angles
#'
#' @return Returns an n-by-2 array of n vectors.
#'
#' @author Danail Obreschkow
#'
#' @examples
#' ## generate 500 unit vectors with radii between 0.5 and 1
#' x = runif2(500,r=c(0.5,1))
#' oldpar = par(pty='s')
#' plot(x,pch=20)
#' par(oldpar)
#'
#' @seealso \code{\link{runif3}}
#'
#' @export

runif2 = function(n=1, r=c(0,1), azimuth=c(0,2*pi)) {

  # handle input
  if (length(r)==1) r=rep(r,2)
  if (length(azimuth)==1) azimuth=rep(azimuth,2)

  # make random vectors
  r = runif(n,r[1]^2,r[2]^2)^(1/2)
  phi = runif(n,azimuth[1],azimuth[2])
  vec = cbind(x=r*cos(phi),y=r*sin(phi))

  # return values
  return(vec)

}
