#' Draw a line with uncertainty regions
#'
#' @importFrom graphics lines polygon
#' @importFrom stats smooth.spline predict
#'
#' @param x vector of x-coordinates
#' @param y vector of y-coordinates
#' @param errp vector of y-errors in the positive direction (upwards)
#' @param errn vector of y-errors in the negative direction (downwards)
#' @param col color of the line
#' @param alpha transparency of the uncertainty region
#' @param smooth logical flag indicating whether the line should be smoothed
#' @param df df (=degrees of freedom) parameter of \code{\link[stats]{smooth.spline}} function
#' @param ... additional parameters used by \code{\link[graphics]{lines}}
#'
#' @return None
#'
#' @author Danail Obreschkow
#'
#' @export

errlines = function(x, y, errp, errn=errp, col='black', alpha=0.5, smooth=FALSE, df=NULL, ...) {

  if (length(x)!=length(y)) stop('errlines: x and y must be of the same length.')
  if (length(x)!=length(errp)) stop('errlines: x and errp must be of the same length.')
  if (length(x)!=length(errn)) stop('errlines: x and errp must be of the same length.')

  if (any(is.na(x))) stop('errlines: x contains NA.')
  if (any(is.na(y))) stop('errlines: y contains NA.')
  if (any(is.na(errp))) stop('errlines: errp contains NA.')
  if (any(is.na(errn))) stop('errlines: errn contains NA.')

  if (any(errp<0)) stop('errlines: errp must be non-negative.')
  if (any(errn<0)) stop('errlines: errn must be non-negative.')

  if (smooth) {

    if (!is.null(df)) df = min(length(x),df)

    nsmooth = 500
    if (par()$xlog) {
      xp = log(x)
    } else {
      xp = x
    }
    if (par()$ylog) {
      yp = log(y)
    } else {
      yp = y
    }
    x.smooth = seq(min(xp),max(xp),length=nsmooth)
    y.smooth = predict(smooth.spline(xp,yp,df=df),x.smooth)$y
    errp.smooth = predict(smooth.spline(xp,errp,df=df),x.smooth)$y
    errn.smooth = predict(smooth.spline(xp,errn,df=df),x.smooth)$y
    if (par()$xlog) {
      x = exp(x.smooth)
    } else {
      x = x.smooth
    }
    if (par()$ylog) {
      y = exp(y.smooth)
      errp = exp(errp.smooth)
      errn = exp(errn.smooth)
    } else {
      y = y.smooth
      errp = errp.smooth
      errn = errn.smooth
    }
  }

  xpoly = c(x,rev(x))
  ypoly = c(y+errp,rev(y-errn))
  polygon(xpoly,ypoly,col=transparent(col,alpha),border=NA)
  lines(x,y,col=col,...)

}
