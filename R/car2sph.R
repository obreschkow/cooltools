#' Cartesian to spherical coordinate conversion
#'
#' @description Convert 3D Cartesian to spherical coordinates
#'
#' @param x 3-element or n-by-3 matrix representing the Cartesian components (x,y,z)  of n three-dimensional vectors
#'
#' @return Returns a 3-element vector or a n-by-3 element matrix representing the spherical coordinates (r,theta,phi), where theta=0...pi is the polar angle measured from the north pole and phi=0...2*pi is the azimuth measured positively from the x-axis (ISO 80000-2:2019 physics convention).
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{sph2car}}
#'
#' @export

car2sph = function(x) {

  if (length(unlist(x,use.names = FALSE))==3) x = matrix(x,1,3)

  r = sqrt(rowSums(x^2))
  theta = acos(x[,3]/r)
  theta[is.nan(theta)] = 0 # to set theta=0 if r=0
  phi = (atan2(x[,2],x[,1])+2*pi)%%(2*pi)

  return(cbind(r,theta,phi))

}
