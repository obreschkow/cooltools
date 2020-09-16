#' Plot a spherical function
#'
#' @importFrom plotrix draw.circle
#' @importFrom graphics lines polygon text
#' @importFrom grDevices gray.colors
#'
#' @description Plots a spherical function in a 2D projection using only standard R graphics. This avoids compatibility issues of rgl, e.g. knitting markdown documents.
#'
#' @param f vectorized real function f(theta,phi) of the polar angle theta [0,pi] and azimuth angle [0,2pi]
#' @param n number of grid cells in each dimension used in the plot
#' @param theta0 polar angle in radians at the center of the projection
#' @param phi0 azimuth angle in radians at the center of the projection
#' @param angle angle in radians between vertical axis and central longitudinal great circle
#' @param col color map
#' @param clim 2-element vector specifying the values of f corresponding to the first and last color in col
#' @param add logical flag specifying whether the sphere is to be added to an existing plot
#' @param center center of the sphere on the plot
#' @param radius radius of the sphere on the plot
#' @param nv number or vertices used for grid lines and border
#' @param show.border logical flag specifying whether to show a circular border around the sphere
#' @param show.grid logical flag specifying whether to show grid lines
#' @param grid.phi vector of phi-values of the longitudinal grid lines
#' @param grid.theta vector of theta-values of the latitudinal grid lines
#' @param lwd line width of grid lines and border
#' @param lty line type of grid lines and border
#' @param line.col color of grid lines and border
#' @param background background color
#' @param ... additional arguments to be passed to the function f
#'
#' @author Danail Obreschkow
#'
#' @seealso \code{\link{sphericalharmonics}}, \code{\link{fibonaccisphere}}.
#'
#' @examples
#' ## Plot real spherical harmonics up to third degree
#'
#' nplot(xlim=c(-4,3.5),ylim=c(0,4),asp=1,mar=c(0,0,0,0))
#'
#' for (l in seq(0,3)) { # degree of spherical harmonic
#'   for (m in seq(-l,l)) { # order of spherical harmonic
#'
#'     # make spherical harmonic function in real-valued convention
#'     f = function(theta,phi) sphericalharmonics(l,m,cbind(theta,phi),basis='real')
#'
#'     # plot spherical harmonic
#'     sphereplot(f, 50, col=planckcolors(100), phi0=0.1, theta0=pi/3, add=TRUE, clim=c(-0.7,0.7),
#'                center=c(m,3.5-l), radius=0.47, show.grid=TRUE)
#'
#'     if (l==3) text(m,-0.15,sprintf('m=%+d',m))
#'   }
#'
#'   text(-l-0.55,3.5-l,sprintf('l=%d',l),pos=2)
#' }
#'
#' @export

sphereplot = function(f, n = 100, theta0 = pi/2, phi0 = 0, angle = 0,
                      col = gray.colors(256,0,1), clim=NULL,
                      add = FALSE, center = c(0,0), radius = 1, nv = 500,
                      show.border = FALSE,
                      show.grid = FALSE, grid.phi = seq(0,330,30)/180*pi, grid.theta = seq(30,150,30)/180*pi,
                      lwd = 0.5, lty = 1, line.col = 'black', background = 'white', ...) {

  # make rotation matrix
  R = rotation3(c(0,0,1),angle)%*%rotation3(c(-1,0,0),theta0-pi/2)%*%rotation3(c(0,-1,0),phi0)

  # make plot coordinates
  x = (seq(n)-0.5)/n*2-1
  p = expand.grid(x=x, y=x)
  xy = p$x^2+p$y^2

  # spherical coordinates of back
  p$z = -sqrt(1-pmin(1,xy))
  rot = as.matrix(p[,1:3])%*%R
  p$theta1 = acos(pmin(1,pmax(-1,rot[,2])))
  p$phi1 = atan2(rot[,1],rot[,3])%%(2*pi)

  # spherical coordinates of front
  p$z = +sqrt(1-pmin(1,xy))
  rot = as.matrix(p[,1:3])%*%R
  p$theta2 = acos(pmin(1,pmax(-1,rot[,2])))
  p$phi2 = atan2(rot[,1],rot[,3])%%(2*pi)

  # evaluate function
  d = 2/n
  p$out = pmax(0,abs(p$x)-d/2)^2+pmax(0,abs(p$y)-d/2)^2>1
  p$f1 = f(p$theta1,p$phi1,...)
  p$f2 = f(p$theta2,p$phi2,...)

  # determine color range
  if (is.null(clim)) clim = range(c(p$f1,p$f2))
  if (clim[2]==clim[1]) clim=clim+c(-1,1)

  # make color arrays
  ncol = length(col)
  p$img1 = col[pmax(1,pmin(ncol,round(0.5+ncol*(p$f1-clim[1])/(clim[2]-clim[1]))))]
  p$img2 = col[pmax(1,pmin(ncol,round(0.5+ncol*(p$f2-clim[1])/(clim[2]-clim[1]))))]
  p$img1[p$out] = background
  p$img2[p$out] = background

  # plot layers
  if (!add) nplot(center[1]+radius*c(-1,1),center[2]+radius*c(-1,1),pty='s')
  rasterImage(rasterflip(array(p$img1,c(n,n))), interpolate = T,
              center[1]-radius, center[2]-radius, center[1]+radius, center[2]+radius)
  rasterImage(rasterflip(array(p$img2,c(n,n))), interpolate = T,
                center[1]-radius, center[2]-radius, center[1]+radius, center[2]+radius)

  # grid lines
  theta = seq(0,2*pi,length=nv)
  for (phi in grid.phi) {
    xyz = rbind(sin(theta)*sin(phi),cos(theta),sin(theta)*cos(phi))
    xyz = R%*%xyz
    xyz[,xyz[3,]<0] = NA
    xyz = xyz*radius+c(center,0)
    lines(xyz[1,],xyz[2,],lwd=lwd,lty=lty,col=line.col)
  }

  phi = seq(0,2*pi,length=nv)
  for (theta in grid.theta) {
    xyz = rbind(sin(theta)*sin(phi),cos(theta),sin(theta)*cos(phi))
    xyz = R%*%xyz
    xyz[,xyz[3,]<0] = NA
    xyz = xyz*radius+c(center,0)
    lines(xyz[1,],xyz[2,],lwd=lwd,lty=lty,col=line.col)
  }

  # frame
  xpd = par()$xpd
  par(xpd=TRUE)
  a = seq(0,pi/2,length=round(nv/4))
  frame = rbind(c(cos(a),1),c(sin(a),1))
  for (i in seq(4)) {
    frame = rbind(c(0,-1),c(1,0))%*%frame
    polygon(t(radius*frame+center),col=background,border=background)
  }

  # plot border
  if (show.border) plotrix::draw.circle(center[1],center[2],radius=radius,border=line.col,nv=nv,lwd=lwd)
  par(xpd=xpd)
}
