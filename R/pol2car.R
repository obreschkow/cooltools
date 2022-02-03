#' Polar/cylindrical to Cartesian coordinate conversion
#'
#' @description Convert polar/cylindrical coordinates to 2D/3D Cartesian coordinates
#'
#' @param x 2/3-element or n-by-2/3 matrix representing the polar/cylindrical coordinates (r,phi)/(r,phi,z), where phi=0...2*pi is the azimuth measured positively from the x-axis.
#'
#' @return Returns a 2/3-element vector or a n-by-2/3 element matrix representing the Cartesian coordinates (x,y)/(x,y,z).
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{car2pol}}
#'
#' @export

pol2car = function(x) {

  if (length(unlist(x,use.names = FALSE))==2) x = matrix(x,1,2)
  if (length(unlist(x,use.names = FALSE))==3) x = matrix(x,1,3)

  d = dim(x)[2]

  if (!d%in%c(2,3)) stop('x must have 2 or 3 elements or columns')

  if (d==2) {
    return(cbind(x=x[,1]*cos(x[,2]),y=x[,1]*sin(x[,2])))
  } else {
    return(cbind(x=x[,1]*cos(x[,2]),y=x[,1]*sin(x[,2]),z=x[,3]))
  }

}
