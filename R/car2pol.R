#' Cartesian to polar/cylindrical coordinate conversion
#'
#' @description Convert 2D/3D Cartesian to polar/cylindrical coordinates
#'
#' @param x 2/3-element or n-by-2/3 matrix representing the Cartesian components (x,y)/(x,y,z)  of n two/three-dimensional vectors
#'
#' @return Returns a 2/3-element vector or a n-by-2/3 element matrix representing the polar/cylindrical coordinates (r,phi)/(r,phi,z), where phi=0...2*pi is the azimuth measured positively from the x-axis.
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{pol2car}}
#'
#' @export

car2pol = function(x) {

  if (length(unlist(x,use.names = FALSE))==2) x = matrix(x,1,2)
  if (length(unlist(x,use.names = FALSE))==3) x = matrix(x,1,3)

  d = dim(x)[2]

  if (!d%in%c(2,3)) stop('x must have 2 or 3 elements or columns')

  if (d==2) {
    r = sqrt(x[,1]^2+x[,2]^2)
  } else {
    r = sqrt(x[,1]^2+x[,2]^2+x[,3]^2)
  }
  phi = (atan2(x[,2],x[,1])+2*pi)%%(2*pi)

  if (d==2) {
    return(cbind(r,phi))
  } else {
    return(cbind(r,phi,z=x[,3]))
  }

}
