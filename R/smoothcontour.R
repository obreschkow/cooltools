#' Draw smoothed contours
#'
#' @importFrom grDevices contourLines
#' @importFrom stats smooth.spline sd
#' @importFrom pracma meshgrid
#' @importFrom graphics lines image contour
#'
#' @description Draw smoothed iso-countours for a density field. The contours are computed using the \code{\link[grDevices]{contourLines}} routine and smoothed using the \code{\link[stats]{smooth.spline}} function. Both open and closed contour lines are handled correctly.
#'
#' @param x,y vectors containing the locations of grid lines at which the values of z are measured. These must be in ascending order. By default, equally spaced values from 0 to 1 are used.
#' @param z matrix representing the density field on which the contours are plotted.
#' @param levels vector of the iso-contour levels.
#' @param smoothing value between 0 and 1 specifying the degree of smoothing.
#' @param min.radius numerical value. If larger than 0, all contours with a mean radius (in pixels) below \code{min.radius} are removed.
#' @param lwd vector of line widths (see \code{\link[graphics]{par}})
#' @param lty vector of line types (see \code{\link[graphics]{par}})
#' @param col vector of colors (see \code{\link[graphics]{par}})
#' @param ... additional parameters to be passed to the function \code{\link[graphics]{lines}}.
#'
#' @return None
#'
#' @author Danail Obreschkow
#'
#' @examples
#' set.seed(1)
#' f = function(x) cos(2*x[1]-x[2]-1)^2*exp(-x[1]^2-x[2]^2-x[1]*x[2])
#' x = seq(-3,3,length=100)
#' m = pracma::meshgrid(x)
#' z = array(Vectorize(function(x,y) f(c(x,y)))(m$Y,m$X)+rnorm(4e4,sd=0.1),dim(m$X))
#' image(x,x,z,col=terrain.colors(100))
#' contour(x,x,z,levels=c(0.2,0.5),add=TRUE)
#' smoothcontour(x,x,z,levels=c(0.2,0.5),lwd=3,smoothing=0.8,min.radius=2)
#'
#' @seealso \code{\link[grDevices]{contourLines}}, \code{\link[stats]{smooth.spline}}
#'
#' @export

smoothcontour = function(x=seq(0,1,length.out=nrow(z)), y=seq(0,1,length.out=ncol(z)), z, levels, smoothing=0.5, min.radius=1, lwd=1, lty=1, col='black', ...) {

  if (smoothing>1 | smoothing<0) stop('smoothing must lie between 0 and 1.')

  nlevels = length(levels)
  if (length(lwd)<nlevels) lwd=rep(lwd,length(levels))
  if (length(lty)<nlevels) lty=rep(lty,length(levels))
  if (length(col)<nlevels) col=rep(col,length(levels))

  # determine whether x-values are linearly distributed (otherwise exponential is assumed)
  nx = length(x)
  if (nx>2) {
    dx = x[2:nx]-x[1:(nx-1)]
    expx = all(dx[2:(nx-1)]-dx[1:(nx-2)]>0)
  } else {
    expx = FALSE
  }

  # determine whether y-values are linearly distributed (otherwise exponential is assumed)
  ny = length(y)
  if (ny>2) {
    dy = y[2:ny]-y[1:(ny-1)]
    expy = all(dy[2:(ny-1)]-dy[1:(ny-2)]>0)
  } else {
    expy = FALSE
  }

  if (expx) x = log(x)
  if (expy) y = log(y)

  dx = (max(x)-min(x))/length(x)
  dy = (max(y)-min(y))/length(y)

  s = 1-(1-smoothing)^2

  for (i in seq(nlevels)) {

    contours = contourLines(x,y,z,levels=levels[i])

    for (j in seq_along(contours)) {

      n = length(contours[[j]]$x)
      radius = sqrt(stats::sd(contours[[j]]$x)^2/dx^2+stats::sd(contours[[j]]$y)^2/dy^2)

      if (n>4 & radius>min.radius) {

        px = contours[[j]]$x
        py = contours[[j]]$y

        is.closed.curve = abs(px[1]-px[n])<stats::sd(px) & abs(py[1]-py[n])<stats::sd(py)

        interval = max(stats::sd(contours[[j]]$x)/dx,stats::sd(contours[[j]]$y)/dy)

        if (is.closed.curve | interval>min.radius) {

          if (is.closed.curve) {
            df = round(5*((1-s)*n+s*4))
            sx = smooth.spline(seq(5*n),rep(px,5),df=df)$y[c((2*n+1):(3*n),2*n+1)]
            sy = smooth.spline(seq(5*n),rep(py,5),df=df)$y[c((2*n+1):(3*n),2*n+1)]
          } else {
            w = rep(1,n)
            w[1] = w[n] = n
            df = round((1-s)*n+s*4)
            sx = smooth.spline(seq(n),px,w=w,df=df)$y
            sy = smooth.spline(seq(n),py,w=w,df=df)$y
          }

          if (expx) sx = exp(sx)
          if (expy) sy = exp(sy)
          lines(sx,sy,lwd=lwd[i],lty=lty[i],col=col[i],...)

        }
      }
    }
  }

}
