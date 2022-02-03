#' Bilinear interpolation function of data on a regular grid
#'
#' @importFrom stats approxfun
#'
#' @description Generates a fast function f(x,y) that interpolates regularly gridded data, based on the equivalent subroutine \code{\link[stats]{approxfun}} in 1D.
#'
#' @param x vector of x-coordinates; must be strictly monotonically increasing, but not necessarily equally spaced
#' @param y vector of y-coordinates; must be strictly monotonically increasing, but not necessarily equally spaced
#' @param z matrix of dimension c(length(x),length(y)) containing the function values to be interpolated
#' @param outside value of the approximation function outside the grid (default is NA)
#'
#' @return Returns a fast an vectorized interpolation function f(x,y)
#'
#' @author Danail Obreschkow
#'
#' @examples
#' x = seq(3)
#' y = seq(4)
#' z = array(c(x+1,x+2,x+3,x+4),c(3,4))
#' f = approxfun2(x,y,z)
#' print(f(1.7,2.4))
#'
#' @seealso \code{\link[stats]{approxfun}}
#'
#' @export

approxfun2 = function(x,y,z,outside=NA) {

  if (is.unsorted(x,strictly=TRUE)) stop('x must be strictly monotonically increasing')
  if (is.unsorted(y,strictly=TRUE)) stop('y must be strictly monotonically increasing')

  nx = length(x)
  ny = length(y)

  if (is.array(z)) {
    if (dim(z)[1]!=nx) stop('z must be an array of dimension (length(x),length(y))')
    if (dim(z)[2]!=ny) stop('z must be an array of dimension (length(x),length(y))')
  } else {
    stop('z must be a 2D array')
  }

  xmin = min(x)
  interval = (max(x)-xmin)*2

  xvect = rep(c(-1e-5,x-xmin,max(x)-xmin+1e-5),ny+2)+rep(seq(ny+2)*interval,each=nx+2)
  zvect = as.vector(cbind(rep(Inf,nx+2),rbind(rep(Inf,ny),z,rep(Inf,ny)),rep(Inf,nx+2)))

  fun = approxfun(xvect,zvect,yleft=outside,yright=outside)

  index = approxfun(y,seq(2,ny+1),yleft=1,yright=ny+2)

  fout = function(x,y) {
    f = index(y)
    i = floor(f)
    j = ceiling(f)
    w = f-i
    x1 = (x-xmin)+i*interval
    x2 = (x-xmin)+j*interval
    out = (1-w)*fun(x1)+w*fun(x2)
    out[is.infinite(out)] = outside
    return(out)
  }

  return(fout)
}
