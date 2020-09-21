#' Mollweide projection
#'
#' @importFrom plotrix draw.ellipse
#' @importFrom stats spline
#'
#' @description Performs a Mollweide projection (also known as Babinet projection, homalographic projection, homolographic projection, and elliptical projection) of longitude and latitude coordinates. The most important feature of the Mollweide projection is that it preserves surface areas, which makes it a commonly used projection in geography, astronomy and cosmology. The total surface area of the standard projection is equal to the surface area of the unit sphere (4pi); and the shape of the fully projected sphere is an ellipse (with axes lengths 2*sqrt(2) and sqrt(2)).
#'
#' @param lon longitude or vector of longitudes in radians (unless deg=TRUE)
#' @param lat latitude or vector of latitudes in radians (unless deg=TRUE), must lie between -pi/2 and +pi/2
#' @param lon0 latitude of null meridian, which will be projected on to x=0
#' @param radius radius of spherical projection, such that the surface area of the projection equals 4piR^2
#' @param deg logical flag; if set to TRUE, the input arguments \code{lon}, \code{lat}, \code{lon0} are assumed to be in degrees (otherwise in radians)
#'
#' @return Returns a data frame of 2D Cartesian coordinates \code{x} and \code{y}.
#'
#' @author Danail Obreschkow
#'
#' @examples
#' lon = runif(1e4,0,2*pi)
#' lat = asin(runif(1e4,-1,1)) # = uniform sampling of the sphere
#' plot(mollweide(lon,lat),xlim=c(-3,3),ylim=c(-1.5,1.5),pch=16,cex=0.5)
#' plotrix::draw.ellipse(0,0,2*sqrt(2),sqrt(2),border='orange',lwd=2)
#'
#' @export

mollweide = function (lon, lat, lon0=0, radius=1, deg=FALSE) {

  # convert input to vectors (just to be sure)
  lon = as.vector(lon)
  lat = as.vector(lat)

  # convert degrees to radians
  if (deg) {
    lon = lon/180*pi
    lat = lat/180*pi
    lon0 = lon0/180*pi
  }

  # check input arguments
  if (min(lat)<(-pi/2)) stop('lat cannot be smaller than -pi/2')
  if (max(lat)>pi/2) stop('lat cannot be larger than pi/2')
  if (length(lon)>1 & length(lat)==1) {
    lat = rep(lat,length(lon))
  } else if (length(lat)>1 & length(lon)==1) {
    lon = rep(lon,length(lat))
  } else if (length(lat)!=length(lon)) {
    stop('lon and lat must be of equal length')
  }

  # recast longitude onto interval [-pi,pi]
  lon = (lon-lon0+pi)%%(2*pi)-pi

  # iteratively evaluate alpha
  ncrit = 200
  if (length(lon)<=ncrit) {
    alpha = lat
  } else {
    lat.grid = seq(0,pi/2,length=ncrit)
    alpha = lat.grid
  }

  s = seq_along(alpha)
  f = pi*sin(alpha)
  for (i in seq(1e4)) {
    t = alpha[s]
    dalpha = -(2*t+sin(2*t)-f[s])/4/(2+2*cos(t))
    alpha[s] = t+dalpha
    s = s[abs(dalpha)>1e-6]
  }

  if (length(lon)>ncrit) {
    alpha = spline(lat.grid,alpha,xout=abs(lat))$y*sign(lat)
  }

  # compute projection
  x = radius*2*sqrt(2)/pi*lon*cos(alpha)
  y = radius*sqrt(2)*sin(alpha)

  # return Cartesian coordinates
  return(data.frame(x=x,y=y))

}
