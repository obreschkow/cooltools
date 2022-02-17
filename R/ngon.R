#' Draw a regular n-gon
#'
#' @importFrom graphics par polygon
#'
#' @description Draws a regular polygon with n sides, such as a triangle (n=3) or hexagon (n=6).
#'
#' @param x vector of x-coordinates specifying the centres of the n-gons.
#' @param y vector of y-coordinates specifying the centres of the n-gons.
#' @param s side lengths; either a single number or a vector of the same length as \code{x} and \code{y}. In log-log plots, the value of s is in units of dex.
#' @param n number of sides of the regular n-gons; either a single number or a vector of the same length as \code{x} and \code{y}.
#' @param angle rotation angle in radians; either a single number or a vector of the same length as \code{x} and \code{y}.
#' @param fix.aspect logical flag. If TRUE (default), the aspect ratio of the n-gon on the screen is forced to be unity, even if the this makes it irregular in the coordinates of the plot. If FALSE, the n-gon is regular in plot coordinates, which makes it distorted in screen coordinates if the aspect ratio is not one and/or if logarithmic coordinates are used.
#' @param ... additional arguments used by \code{\link[graphics]{polygon}}.
#'
#' @return None.
#'
#' @author Danail Obreschkow
#'
#' @examples
#'
#' ## Plot random points on the unit sphere in Mollweide projection
#' # hexagon at the center of a plot
#' nplot(bty='o', asp=0.5)
#' ngon(x=0.5, y=0.5, s=0.1, n=6, fix.aspect=FALSE)
#'
#  force correct apparent aspect ratio
#' ngon(x=0.5, y=0.5, s=0.1, n=6, border='red')
#'
#  log-log plot
#' plot(NA,xlim=c(1,1e3),ylim=c(1,1e5),log='xy')
#' ngon(x=10^runif(10,0,3), y=10^runif(10,0,5), s=1, n=6,  border='red',lwd=3)
#'
#' @export

ngon = function(x=0, y=0, s=1, n=6, angle=0, fix.aspect=TRUE, ...) {

  # safe use of par()
  oldpar = par(no.readonly = TRUE)
  on.exit(par(oldpar))

  ntiles = max(length(x),length(y))

  # make vectors of all intput parameters
  if (ntiles>1) {
    if (length(x)<ntiles) x = rep(x[1],ntiles)
    if (length(y)<ntiles) y = rep(y[1],ntiles)
    if (length(s)<ntiles) s = rep(s[1],ntiles)
    if (length(angle)<ntiles) angle = rep(angle[1],ntiles)
  }

  # get current aspect ratio
  if (fix.aspect) {
    w = graphics::par()$pin[1]/diff(graphics::par()$usr[1:2])
    h = graphics::par()$pin[2]/diff(graphics::par()$usr[3:4])
    if (par()$xlog) {
      par(xlog=FALSE)
      x = log10(x)
    }
    if (par()$ylog) {
      par(ylog=FALSE)
      y = log10(y)
    }
    fx = h/sqrt(h*w)
    fy = w/sqrt(h*w)
  }

  # draw all n-gons
  for (i in seq(ntiles)) {
    a = seq(0,2*pi,length=n+1)[-n-1]+angle[i]
    if (fix.aspect) {
      xp = x[i]+s[i]*cos(a)*fx
      yp = y[i]+s[i]*sin(a)*fy
    } else {
      xp = x[i]+s[i]*cos(a)
      yp = y[i]+s[i]*sin(a)
    }
    graphics::polygon(xp, yp, ...)
  }

}
