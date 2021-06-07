#' Spherical to cartesian coordinate conversion
#'
#' @description Convert 3D spherical to Cartesian coordinates
#'
#' @param x 3-element or n-by-3 matrix representing the spherical components (r,theta,phi) of n three-dimensional vectors. Here, theta=0...pi is the polar angle measured from the north pole and phi=0...2*pi is the azimuth measured positively from the x-axis (ISO 80000-2:2019 physics convention).
#'
#' @return Returns a 3-element vector or a n-by-3 element matrix representing the Cartesian coordinates (x,y,z)
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{car2sph}}
#'
#' @export

sph2car = function(x) {

  if (length(unlist(x,use.names = FALSE))==3) x = matrix(x,1,3)

  return(data.frame(x=x[,1]*sin(x[,2])*cos(x[,3]),
                    y=x[,1]*sin(x[,2])*sin(x[,3]),
                    z=x[,1]*cos(x[,2])))

}
