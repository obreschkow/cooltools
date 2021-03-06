#' Vertical color bar
#'
#' @importFrom graphics rect axis rasterImage
#' @importFrom grDevices gray.colors
#'
#' @description Adds a vertical color bar to a plot with a custom axis.
#'
#' @param xleft left x-coordinate of the bar
#' @param ybottom bottom y-coordinate of the bar
#' @param xright right x-coordinate of the bar
#' @param ytop top y-coordinate of the bar
#' @param col vector of colors
#' @param n number of polygons used to draw the bar
#' @param crange 2-vector specifying the range of values, linearly represented by the full color scale
#' @param clim optional 2-vector specifying a sub-interval of crange to which the color scale is cropped
#' @param show.border logical flag specifying whether to draw a rectangular border around the bar
#' @param text axis label
#' @param line distance between label and color bar
#' @param show.axis logical flag specifying whether to draw an axis
#' @param side character string, which specifies the location of the axis on the bar. This has to be 'left' or 'right' (default).
#' @param lwd linewidth of border and ticks
#' @param nticks number of ticks on the axis
#' @param at vector of values specifying the tick positions on the axis, this overrides nticks.
#' @param ... optional arguments to be passed to the function \code{\link{axis}}
#'
#' @author Danail Obreschkow
#'
#' @examples
#' ## Plot a spherical harmonic function with a color bar
#'
#' nplot(xlim=c(0,1.2), asp=1)
#' f = function(theta,phi) sphericalharmonics(10,10,cbind(theta,phi))
#' sphereplot(f, col=planckcolors(200), phi0=0.1, theta0=pi/3, add=TRUE, clim=c(-0.7,0.7),
#'            center=c(0.5,0.5), radius=0.4)
#' colorbar(1,0.1,1.1,0.9,planckcolors(200),crange=c(-0.7,0.7))
#'
#' @export

colorbar = function(xleft, ybottom, xright, ytop,
                    col = gray.colors(256,0,1), n = length(col),
                    crange = c(0,1), clim = NULL,
                    show.border = TRUE, text = '', line=2,
                    show.axis = TRUE, side = 'right', lwd=1, nticks=5, at=NULL, ...) {

  # avoid clipped margin
  xpd = par()$xpd
  par(xpd=TRUE)

  # set range of values to be displayed
  if (is.null(clim)) {
    clim = crange
  } else {
    if (clim[1]<crange[1] | clim[2]>crange[2]) stop('the interval clim must be contained in crange')
  }

  # plot gradient
  rgb = array(NA,c(n,1,3))
  rgb[,1,] =  t(col2rgb(rev(col))/255)
  rasterImage(rgb, xleft, ybottom, xright, ytop)

  # plot axis
  if (show.axis) {

    # redefine plotting area
    usr = par()$usr
    b = diff(clim)/(ytop-ybottom)
    a = clim[1]-b*ybottom
    par(usr=c(usr[1:2],a+b*usr[3:4]))

    if (is.null(at)) at=seq(clim[1], clim[2], length=nticks)
    if (side=='left') {
      axis(side=2, pos=xleft, at=at, lwd = NA, lwd.ticks = lwd, ...)
    } else if (side=='right') {
      axis(side=4, pos=xright, at=at, lwd = NA, lwd.ticks = lwd, ...)
    } else {
      stop('unknown side')
    }

    # restore plotting area
    par(usr=usr)
  }

  # label axis
  if (side=='left') {
    text(xleft-line*(xright-xleft),(ybottom+ytop)/2,text,srt='90')
  } else if (side=='right') {
    text(xright+line*(xright-xleft),(ybottom+ytop)/2,text,srt='90')
  } else {
    stop('unknown side')
  }

  # plot border
  if (show.border) rect(xleft, ybottom, xright, ytop, lwd=lwd)

  # restore clipped margin
  par(xpd=xpd)

}
