#' Generate random unit vectors in 3D
#'
#' @description Generate random unit vectors in 3D, following an isotropic distribution (optionally truncated to a region).
#'
#' @param n number of random vectors to be generated
#' @param azimuth 2-vector specifying the range of azimuth angles
#' @param polarangle 2-vector specifying the range of polar angles
#'
#' @return Returns an n-by-3 array of n vectors.
#'
#' @author Danail Obreschkow
#'
#' @export

rsphere = function(n=1,azimuth=c(0,2*pi),polarangle=c(0,pi)) {

  phi = runif(n,azimuth[1],azimuth[2])
  z = runif(n,cos(polarangle[2]),cos(polarangle[1]))
  xy = sqrt(1-z^2)
  vec = cbind(xy*cos(phi),xy*sin(phi),z)
  return(vec)

}
