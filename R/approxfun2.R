#' Bilinear interpolation function of data on a regular grid
#'
#' @importFrom stats approxfun
#'
#' @description Generates a fast function f(x,y) that interpolates gridded data, based on the analogous subroutine \code{\link[stats]{approxfun}} in 1D.
#'
#' @param x n-vector of x-coordinates; must be strictly monotonically increasing, but not necessarily equally spaced
#' @param y m-vector of y-coordinates; must be strictly monotonically increasing, but not necessarily equally spaced
#' @param z n-by-m matrix containing the known function values at the (x,y)-coordinates
#' @param outside value of the approximation function outside the grid (default is NA)
#'
#' @return Returns a fast and vectorized interpolation function f(x,y)
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

  nx = length(x)
  ny = length(y)

  # Check input
  if (!is.numeric(x) || nx<2 || any(!is.finite(x)) ||
      is.unsorted(x,strictly=TRUE))
    stop('x must be finite and strictly monotonically increasing')

  if (!is.numeric(y) || ny<2 || any(!is.finite(y)) ||
      is.unsorted(y,strictly=TRUE))
    stop('y must be finite and strictly monotonically increasing')

  if (!is.numeric(z) || !is.matrix(z) || any(dim(z)!=c(nx,ny)))
    stop('z must be a numeric matrix of dimension (length(x),length(y))')

  if (length(outside)!=1)
    stop('outside must be a scalar')

  # Precompute grid intervals
  dx = diff(x)
  dy = diff(y)

  # Vectorised interpolation function
  fout = function(px,py) {

    if (!length(px) || !length(py)) return(numeric(0))

    n = max(length(px),length(py))
    px = rep_len(px,n)
    py = rep_len(py,n)

    out = rep(as.numeric(outside),n)

    # Select points inside the grid
    k = which(is.finite(px) & is.finite(py) &
                px>=x[1] & px<=x[nx] &
                py>=y[1] & py<=y[ny])

    if (!length(k)) return(out)

    # Find enclosing grid cells
    i = findInterval(px[k],x,rightmost.closed=TRUE)
    j = findInterval(py[k],y,rightmost.closed=TRUE)

    # Fractional coordinates within each cell
    a = (px[k]-x[i])/dx[i]
    b = (py[k]-y[j])/dy[j]

    # Bilinear interpolation
    out[k] = (1-a)*(1-b)*z[cbind(i,j)] +
      a*(1-b)*z[cbind(i+1,j)] +
      (1-a)*b*z[cbind(i,j+1)] +
      a*b*z[cbind(i+1,j+1)]

    out
  }

  return(fout)
}
