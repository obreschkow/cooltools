#' Re-bin density histograms
#'
#' @importFrom graphics lines
#' @importFrom utils tail
#'
#' @description Transform density histogram data into histogram data with different bins
#'
#' @param x n-vector giving the mid-points of the input histogram bins, must be equally spaced
#' @param y n-vector giving the values of the input histogram values
#' @param xout m-vector giving the mid-points of the output histogram bins, must be equally spaced
#'
#' @return m-vector of y-values associated with the bins specified by xout.
#'
#' @author Danail Obreschkow
#'
#' @examples
#' # original binning
#' x = seq(0.5,4.5)
#' y = seq(5)
#' plot(x,y,xlim=c(-1,6),ylim=c(0,6),pch=16)
#' lines(histcoord(x,y),lwd=3)
#'
#' # rebinning
#' xout = seq(0.125,4.875,0.25)
#' yout = rebindensity(x,y,xout)
#' points(xout,yout,col='red',pch=16)
#' lines(histcoord(xout,yout),col='red')
#'
#' @export

rebindensity = function(x,y,xout) {

  # size of input grid
  n = length(x)
  if (n<=1) stop('x and y must be vectors with more than one element')
  if (length(y)!=n) stop('x and y must have the same length')

  # size of output grid
  nout = length(xout)

  # edges of input grid
  x0 = c(x[1]-(x[2]-x[1])/2,(x[2:n]+x[1:(n-1)])/2)
  x1 = c((x[2:n]+x[1:(n-1)])/2,x[n]+(x[n]-x[n-1])/2)

  # edges of output grid
  xout0 = c(xout[1]-(xout[2]-xout[1])/2,(xout[2:nout]+xout[1:(nout-1)])/2)
  xout1 = c((xout[2:nout]+xout[1:(nout-1)])/2,xout[nout]+(xout[nout]-xout[nout-1])/2)

  # rebinning
  yout = rep(0,nout)
  for (iout in seq(nout)) {
    i0 = which(x1>xout0[iout])[1]
    i1 = utils::tail(which(x0<xout1[iout]),1)
    if (length(i0)==0) i0 = 1
    if (length(i1)==0) i1 = n
    dx = pmin(xout1[iout],x1[i0:i1])-pmax(xout0[iout],x0[i0:i1])
    yout[iout] = yout[iout]+sum(y[i0:i1]*dx)
  }
  yout = yout/(xout1-xout0)

  # return
  return(yout)
}
